use std::{
    cell::Cell,
    cmp::Ordering,
    fmt::{self, Write},
    hash,
};

use crate::js::common::unicode::{
    code_point_from_surrogate_pair, is_ascii, is_high_surrogate_code_unit, is_latin1_char,
    is_latin1_code_point, is_low_surrogate_code_unit, is_whitespace, needs_surrogate_pair,
    try_encode_surrogate_pair, CodePoint, CodeUnit,
};

use super::{
    gc::GcDeref,
    object_descriptor::{ObjectDescriptor, ObjectKind},
    Context, Gc,
};

#[repr(C)]
pub struct StringValue {
    descriptor: Gc<ObjectDescriptor>,
    value: Cell<StringKind>,
}

enum StringKind {
    OneByte(OneByteString),
    TwoByte(TwoByteString),
    Concat(ConcatString),
}

#[derive(Clone, Copy, PartialEq)]
pub enum StringWidth {
    OneByte,
    TwoByte,
}

/// A string where every code unit is represented by a u8. Equivalent to the Latin1 encoding.
/// Guaranteed to not contain any surrogate code units.
struct OneByteString {
    ptr: *const u8,
    len: usize,
}

/// A string where every code unit is represented by a u16. Equivalent to UTF-16, except that
/// surrogate code points may appear outside of surrogate pairs.
///
/// May only contain code units representable in a OneByteString.
struct TwoByteString {
    ptr: *const u16,
    len: usize,
}

/// A string which is the concatenation of a left and right string. Will be lazily flattened when
/// an indexing operation is performed. Tracks the total string length, and the widest width seen
/// in the entire concat tree.
struct ConcatString {
    left: Gc<StringValue>,
    right: Gc<StringValue>,
    len: usize,
    width: StringWidth,
}

impl StringValue {
    fn new(cx: &mut Context, value: StringKind) -> Gc<StringValue> {
        let descriptor = cx.base_descriptors.get(ObjectKind::String);
        cx.heap
            .alloc(StringValue { descriptor, value: Cell::new(value) })
    }

    pub fn from_utf8(cx: &mut Context, str: String) -> Gc<StringValue> {
        // Scan string to find total number of code units and see if a two-byte string must be used
        let mut has_two_byte_chars = false;
        let mut has_non_ascii_one_byte_chars = false;
        let mut length: usize = 0;

        for char in str.chars() {
            if !is_ascii(char) {
                if !is_latin1_char(char) {
                    has_two_byte_chars = true;

                    // Two code units are needed if this is a surrogate pair
                    if needs_surrogate_pair(char as CodePoint) {
                        length += 2;
                    } else {
                        length += 1;
                    }
                } else {
                    has_non_ascii_one_byte_chars = true;
                    length += 1;
                }
            } else {
                length += 1;
            }
        }

        if has_two_byte_chars {
            // Two byte strings are copied into buffer one character at a time, checking if character
            // must be encoded as a surrogate pair.
            let mut buf: Vec<u16> = Vec::with_capacity(length);
            unsafe { buf.set_len(length) };

            let mut index = 0;
            for char in str.chars() {
                match try_encode_surrogate_pair(char as CodePoint) {
                    None => {
                        buf[index] = char as u16;
                        index += 1;
                    }
                    Some((high, low)) => {
                        buf[index] = high;
                        buf[index + 1] = low;
                        index += 2;
                    }
                }
            }

            let two_byte_string = TwoByteString::from_vec(buf);
            StringValue::new(cx, StringKind::TwoByte(two_byte_string))
        } else if !has_non_ascii_one_byte_chars {
            // If this string is pure ASCII then we can directly copy the UTF-8 string.
            let str_copy = str.clone();
            let one_byte_string = OneByteString { ptr: str_copy.as_ptr(), len: length };

            // Memory is managed by heap, and destructor is called when string is garbage collected.
            std::mem::forget(str_copy);

            StringValue::new(cx, StringKind::OneByte(one_byte_string))
        } else {
            // Otherwise we must copy each character into a new buffer, converting from UTF-8 to Latin1
            let mut buf: Vec<u8> = Vec::with_capacity(length);
            unsafe { buf.set_len(length) };

            // Copy each character into the buffer
            let mut index = 0;
            for char in str.chars() {
                buf[index] = char as u8;
                index += 1;
            }

            let one_byte_string = OneByteString::from_vec(buf);
            StringValue::new(cx, StringKind::OneByte(one_byte_string))
        }
    }

    pub fn from_code_unit(cx: &mut Context, code_unit: CodeUnit) -> Gc<StringValue> {
        Self::from_code_point_impl(cx, code_unit as CodePoint)
    }

    pub fn from_code_point(cx: &mut Context, code_point: CodePoint) -> Gc<StringValue> {
        Self::from_code_point_impl(cx, code_point)
    }

    #[inline]
    fn from_code_point_impl(cx: &mut Context, code_point: CodePoint) -> Gc<StringValue> {
        if is_latin1_code_point(code_point) {
            let one_byte_string = OneByteString::from_vec(vec![code_point as u8]);
            StringValue::new(cx, StringKind::OneByte(one_byte_string))
        } else {
            match try_encode_surrogate_pair(code_point) {
                None => {
                    let two_byte_string = TwoByteString::from_vec(vec![code_point as CodeUnit]);
                    StringValue::new(cx, StringKind::TwoByte(two_byte_string))
                }
                Some((high, low)) => {
                    let two_byte_string = TwoByteString::from_vec(vec![high, low]);
                    StringValue::new(cx, StringKind::TwoByte(two_byte_string))
                }
            }
        }
    }

    pub fn concat(
        cx: &mut Context,
        left: Gc<StringValue>,
        right: Gc<StringValue>,
    ) -> Gc<StringValue> {
        let new_len = left.len() + right.len();
        let width = if left.width() == StringWidth::TwoByte || right.width() == StringWidth::TwoByte
        {
            StringWidth::TwoByte
        } else {
            StringWidth::OneByte
        };

        let concat_string = StringKind::Concat(ConcatString { left, right, len: new_len, width });

        StringValue::new(cx, concat_string)
    }

    pub fn concat_all(cx: &mut Context, strings: &[Gc<StringValue>]) -> Gc<StringValue> {
        if strings.is_empty() {
            cx.names.empty_string().as_string()
        } else {
            let mut concat_string = strings[0];
            for string in &strings[1..] {
                concat_string = StringValue::concat(cx, concat_string, *string);
            }

            concat_string
        }
    }
}

impl Gc<StringValue> {
    pub fn is_empty(&self) -> bool {
        match self.value() {
            StringKind::OneByte(str) => str.len == 0,
            StringKind::TwoByte(str) => str.len == 0,
            StringKind::Concat(str) => str.len == 0,
        }
    }

    pub fn len(&self) -> usize {
        match self.value() {
            StringKind::OneByte(str) => str.len,
            StringKind::TwoByte(str) => str.len,
            StringKind::Concat(str) => str.len,
        }
    }

    fn width(&self) -> StringWidth {
        match self.value() {
            StringKind::OneByte(_) => StringWidth::OneByte,
            StringKind::TwoByte(_) => StringWidth::TwoByte,
            StringKind::Concat(str) => str.width,
        }
    }

    #[inline]
    fn value(&self) -> &StringKind {
        unsafe { &*self.value.as_ptr() }
    }

    /// Return the 16-bit code unit at the given index. This may be a surrogate pair that must be
    /// combined with an adjacent code unit to form a full unicode code point.
    ///
    /// Does not bounds check, so only call if index is known to be in range.
    pub fn code_unit_at(&self, index: usize) -> CodeUnit {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.code_unit_at(index)
            }
            StringKind::OneByte(str) => str.as_slice()[index] as CodeUnit,
            StringKind::TwoByte(str) => str.as_slice()[index],
        }
    }

    /// Return the unicode code point that starts with the code unit at the given index. If pointing
    /// to a surrogate code unit that is not the start of a valid code point then return the value
    /// of that surrogate code unit.
    ///
    /// Does not bounds check, so only call if index is known to be in range.
    pub fn code_point_at(&self, index: usize) -> CodePoint {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.code_point_at(index)
            }
            // Every byte in a one byte string is a code point
            StringKind::OneByte(str) => str.as_slice()[index] as CodePoint,
            // Must check if this index is the start of a surrogate pair
            StringKind::TwoByte(str) => {
                let code_unit = str.as_slice()[index];
                if is_high_surrogate_code_unit(code_unit) && index + 1 < str.len {
                    let next_code_unit = str.as_slice()[index + 1];
                    if is_low_surrogate_code_unit(code_unit) {
                        code_point_from_surrogate_pair(code_unit, next_code_unit)
                    } else {
                        code_unit as CodePoint
                    }
                } else {
                    code_unit as CodePoint
                }
            }
        }
    }

    /// Return a substring of this string between the given indices. Indices refer to a half-open
    /// range of code units in the string. This function does not bounds check, so caller must make
    /// sure that 0 <= start <= end < string length.
    pub fn substring(&self, cx: &mut Context, start: usize, end: usize) -> Gc<StringValue> {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.substring(cx, start, end)
            }
            StringKind::OneByte(str) => {
                let one_byte_string = OneByteString::from_slice(&str.as_slice()[start..end]);
                StringValue::new(cx, StringKind::OneByte(one_byte_string))
            }
            StringKind::TwoByte(str) => {
                let two_byte_string = TwoByteString::from_slice(&str.as_slice()[start..end]);
                StringValue::new(cx, StringKind::TwoByte(two_byte_string))
            }
        }
    }

    /// Return the index of the first occurrence of the search string in this string, starting after
    /// a given index (inclusive). This function does not bounds check the after index, so caller
    /// must be make sure to only pass an after index that is less than the length of the string.
    pub fn find(&self, search_string: Gc<StringValue>, after: usize) -> Option<usize> {
        let mut string_code_units = self.iter_slice_code_units(after, self.len());
        let mut search_string_code_units = search_string.iter_code_units();

        // Find the first character in search string, immediately returning if search string is empty
        let first_search_code_unit = match search_string_code_units.next() {
            None => return Some(after),
            Some(code_unit) => code_unit,
        };

        let mut current_index = after;

        while let Some(code_unit) = string_code_units.next() {
            // If we see the first code unit in the search string, clone iterators and check all
            // later code units to see if this is a match.
            if code_unit == first_search_code_unit {
                let mut string_code_units = string_code_units.clone();
                let mut search_string_code_units = search_string_code_units.clone();

                loop {
                    match (string_code_units.next(), search_string_code_units.next()) {
                        // If we reach the end of the search string we must have a match
                        (_, None) => return Some(current_index),
                        (Some(code_unit_1), Some(code_unit_2)) if code_unit_1 == code_unit_2 => {
                            continue;
                        }
                        _ => break,
                    }
                }
            }

            current_index += 1;
        }

        None
    }

    /// Return the index of the last ocurrence of the search string in this string, starting before
    /// a given index (inclusive), which may be equal to the length of the string.
    pub fn rfind(&self, search_string: Gc<StringValue>, before: usize) -> Option<usize> {
        // Find the end index (exclusive) of the string range to check
        let search_string_len = search_string.len();
        let end_index = usize::min(before + search_string_len, self.len());

        let mut string_code_units = self.iter_slice_code_units(0, end_index);
        let mut search_string_code_units = search_string.iter_code_units();

        // Find the last character in search string, immediately returning if search string is empty
        let last_search_code_unit = match search_string_code_units.next_back() {
            None => return Some(before),
            Some(code_unit) => code_unit,
        };

        let mut current_index = end_index - search_string_len;

        while let Some(code_unit) = string_code_units.next_back() {
            // If we see the last code unit in the search string, clone iterators and check all
            // earlier code units to see if this is a match.
            if code_unit == last_search_code_unit {
                let mut string_code_units = string_code_units.clone();
                let mut search_string_code_units = search_string_code_units.clone();

                loop {
                    match (string_code_units.next_back(), search_string_code_units.next_back()) {
                        // If we reach the start of the search string we must have a match
                        (_, None) => return Some(current_index),
                        (Some(code_unit_1), Some(code_unit_2)) if code_unit_1 == code_unit_2 => {
                            continue;
                        }
                        _ => break,
                    }
                }
            }

            if current_index == 0 {
                break;
            }

            current_index -= 1;
        }

        None
    }

    /// If this string is a concat string, flatten it into a single buffer. This modifies the string
    /// value in-place and switches it from a concat string to a sequential string type.
    fn flatten(&self) {
        let concat_string = match self.value() {
            StringKind::Concat(str) => str,
            _ => return,
        };

        let length = concat_string.len;
        let mut index = 0;

        if concat_string.width == StringWidth::OneByte {
            let mut buf: Vec<u8> = Vec::with_capacity(length);
            unsafe { buf.set_len(length) };

            let mut stack = vec![*self];

            while let Some(current_string) = stack.pop() {
                match current_string.value() {
                    StringKind::Concat(str) => {
                        stack.push(str.right);
                        stack.push(str.left);
                    }
                    StringKind::OneByte(str) => {
                        for code_unit in CodeUnitIterator::from_one_byte(str) {
                            buf[index] = code_unit as u8;
                            index += 1;
                        }
                    }
                    StringKind::TwoByte(_) => {
                        unreachable!("Two-byte strings cannot appear in one-byte concat strings",)
                    }
                }
            }

            let one_byte_string = OneByteString::from_vec(buf);
            self.value.set(StringKind::OneByte(one_byte_string))
        } else {
            let mut buf: Vec<u16> = Vec::with_capacity(length);
            unsafe { buf.set_len(length) };

            let mut stack = vec![*self];

            while let Some(current_string) = stack.pop() {
                match current_string.value() {
                    StringKind::Concat(str) => {
                        stack.push(str.right);
                        stack.push(str.left);
                    }
                    StringKind::OneByte(str) => {
                        for code_unit in CodeUnitIterator::from_one_byte(str) {
                            buf[index] = code_unit;
                            index += 1;
                        }
                    }
                    StringKind::TwoByte(str) => {
                        for code_unit in CodeUnitIterator::from_two_byte(str) {
                            buf[index] = code_unit;
                            index += 1;
                        }
                    }
                }
            }

            let two_byte_string = TwoByteString::from_vec(buf);
            self.value.set(StringKind::TwoByte(two_byte_string))
        }
    }

    pub fn trim(&self, cx: &mut Context, trim_start: bool, trim_end: bool) -> Gc<StringValue> {
        let mut code_points_iter = self.iter_code_points();

        let mut start_ptr = code_points_iter.ptr();

        if trim_start {
            while let Some(code_point) = code_points_iter.next() {
                let char = unsafe { char::from_u32_unchecked(code_point) };
                if !is_whitespace(char) {
                    break;
                }

                start_ptr = code_points_iter.ptr();
            }
        }

        let mut end_ptr = code_points_iter.ptr_back();

        if trim_end {
            while let Some(code_point) = code_points_iter.next_back() {
                let char = unsafe { char::from_u32_unchecked(code_point) };
                if !is_whitespace(char) {
                    break;
                }

                end_ptr = code_points_iter.ptr_back();
            }
        }

        if code_points_iter.is_end() {
            return cx.names.empty_string.as_string();
        }

        match code_points_iter.width() {
            StringWidth::OneByte => {
                let slice = unsafe {
                    let length = end_ptr.offset_from(start_ptr);
                    std::slice::from_raw_parts(start_ptr, length as usize)
                };

                let string = OneByteString::from_slice(slice);
                StringValue::new(cx, StringKind::OneByte(string))
            }
            StringWidth::TwoByte => {
                let slice = unsafe {
                    let length = (end_ptr as *const u16).offset_from(start_ptr as *const u16);
                    std::slice::from_raw_parts(start_ptr as *const u16, length as usize)
                };

                let string = TwoByteString::from_slice(slice);
                StringValue::new(cx, StringKind::TwoByte(string))
            }
        }
    }

    pub fn repeat(&self, cx: &mut Context, n: u64) -> Gc<StringValue> {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.repeat(cx, n)
            }
            StringKind::OneByte(str) => {
                let repeated_buf = str.as_slice().repeat(n as usize);
                let string = OneByteString::from_vec(repeated_buf);
                StringValue::new(cx, StringKind::OneByte(string))
            }
            StringKind::TwoByte(str) => {
                let repeated_buf = str.as_slice().repeat(n as usize);
                let string = TwoByteString::from_vec(repeated_buf);
                StringValue::new(cx, StringKind::TwoByte(string))
            }
        }
    }

    pub fn substring_equals(&self, search: Gc<StringValue>, start_index: usize) -> bool {
        let mut slice_code_units =
            self.iter_slice_code_units(start_index, start_index + search.len());
        let mut search_code_units = search.iter_code_units();

        slice_code_units.consume_equals(&mut search_code_units)
    }

    pub fn to_lower_case(&self, cx: &mut Context) -> Gc<StringValue> {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.to_lower_case(cx)
            }
            StringKind::OneByte(string) => {
                // One byte fast path. Know the length of the result string in advance, and can
                // convert to lowercase with bitwise operations.
                let mut lowercased = Vec::with_capacity(string.len);

                for latin1_byte in string.as_slice() {
                    let set_lowercase = if *latin1_byte < 0x80 {
                        // ASCII lowercase
                        *latin1_byte >= ('A' as u8) && *latin1_byte <= ('Z' as u8)
                    } else {
                        // Latin1 lowercase
                        *latin1_byte >= ('À' as u8) && *latin1_byte <= ('Þ' as u8)
                    };

                    if set_lowercase {
                        lowercased.push(*latin1_byte ^ 0x20);
                    } else {
                        lowercased.push(*latin1_byte);
                    }
                }

                let string = OneByteString::from_vec(lowercased);
                StringValue::new(cx, StringKind::OneByte(string))
            }
            StringKind::TwoByte(string) => {
                // Two byte slow path. Must convert each code point to lowercase one by one, each of
                // which can map to multiple code points.
                let mut lowercased = vec![];

                let code_point_iter = CodePointIterator::from_two_byte(string);
                for code_point in code_point_iter {
                    match char::from_u32(code_point) {
                        // Must be an unpaired surrogate, which should be written back to buffer
                        None => lowercased.push(code_point as u16),
                        Some(char) => {
                            // May becomes multiple code points when lowercased
                            for lowercase_char in char.to_lowercase() {
                                match try_encode_surrogate_pair(lowercase_char as CodePoint) {
                                    // Single code unit so write back to buffer
                                    None => lowercased.push(lowercase_char as u16),
                                    // Write both surrogate code units back to buffer
                                    Some((high_surrogate, low_surrogate)) => {
                                        lowercased.push(high_surrogate);
                                        lowercased.push(low_surrogate);
                                    }
                                }
                            }
                        }
                    }
                }

                let string = TwoByteString::from_vec(lowercased);
                StringValue::new(cx, StringKind::TwoByte(string))
            }
        }
    }

    pub fn to_upper_case(&self, cx: &mut Context) -> Gc<StringValue> {
        let code_point_iter = match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                return self.to_upper_case(cx);
            }
            StringKind::OneByte(string) => {
                // Fast path for ASCII-only string, as uppercased Latin1 code points may be out of
                // one byte range or map to multiple code points.
                if string.is_ascii() {
                    let mut uppercased = Vec::with_capacity(string.len);

                    for ascii_byte in string.as_slice() {
                        if *ascii_byte >= ('a' as u8) && *ascii_byte <= ('z' as u8) {
                            uppercased.push(*ascii_byte ^ 0x20);
                        } else {
                            uppercased.push(*ascii_byte);
                        }
                    }

                    let string = OneByteString::from_vec(uppercased);
                    return StringValue::new(cx, StringKind::OneByte(string));
                }

                CodePointIterator::from_one_byte(string)
            }
            StringKind::TwoByte(string) => CodePointIterator::from_two_byte(string),
        };

        // Slow path pessimistically generates two byte strings. Must convert each code point to
        // uppercase one by one, each of which can map to multiple code points.
        let mut uppercased = vec![];

        for code_point in code_point_iter {
            match char::from_u32(code_point) {
                // Must be an unpaired surrogate, which should be written back to buffer
                None => uppercased.push(code_point as u16),
                Some(char) => {
                    // May becomes multiple code points when uppercased
                    for uppercase_char in char.to_uppercase() {
                        match try_encode_surrogate_pair(uppercase_char as CodePoint) {
                            // Single code unit so write back to buffer
                            None => uppercased.push(uppercase_char as u16),
                            // Write both surrogate code units back to buffer
                            Some((high_surrogate, low_surrogate)) => {
                                uppercased.push(high_surrogate);
                                uppercased.push(low_surrogate);
                            }
                        }
                    }
                }
            }
        }

        let string = TwoByteString::from_vec(uppercased);
        StringValue::new(cx, StringKind::TwoByte(string))
    }

    pub fn iter_code_units(&self) -> CodeUnitIterator {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.iter_code_units()
            }
            StringKind::OneByte(string) => CodeUnitIterator::from_one_byte(string),
            StringKind::TwoByte(string) => CodeUnitIterator::from_two_byte(string),
        }
    }

    /// Return an iterator over the code units of this string between the provided start and end
    /// indices (start index is inclusive, end index is exclusive).
    pub fn iter_slice_code_units(&self, start: usize, end: usize) -> CodeUnitIterator {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.iter_code_units()
            }
            StringKind::OneByte(string) => {
                CodeUnitIterator::from_one_byte_slice(string, start, end)
            }
            StringKind::TwoByte(string) => {
                CodeUnitIterator::from_two_byte_slice(string, start, end)
            }
        }
    }

    pub fn iter_code_points(&self) -> CodePointIterator {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.iter_code_points()
            }
            StringKind::OneByte(string) => CodePointIterator::from_one_byte(string),
            StringKind::TwoByte(string) => CodePointIterator::from_two_byte(string),
        }
    }
}

#[derive(Clone)]
pub struct CodeUnitIterator {
    ptr: *const u8,
    end: *const u8,
    width: StringWidth,
}

impl CodeUnitIterator {
    fn from_one_byte(string: &OneByteString) -> Self {
        let ptr = string.ptr;
        let end = unsafe { ptr.add(string.len) };

        CodeUnitIterator { ptr, end, width: StringWidth::OneByte }
    }

    fn from_two_byte(string: &TwoByteString) -> Self {
        let ptr = string.ptr;
        let end = unsafe { ptr.add(string.len) };

        CodeUnitIterator {
            ptr: ptr as *const u8,
            end: end as *const u8,
            width: StringWidth::TwoByte,
        }
    }

    fn from_one_byte_slice(string: &OneByteString, start: usize, end: usize) -> Self {
        let ptr = unsafe { string.ptr.add(start) };
        let end = unsafe { string.ptr.add(end) };

        CodeUnitIterator { ptr, end, width: StringWidth::OneByte }
    }

    fn from_two_byte_slice(string: &TwoByteString, start: usize, end: usize) -> Self {
        let ptr = unsafe { string.ptr.add(start) };
        let end = unsafe { string.ptr.add(end) };

        CodeUnitIterator {
            ptr: ptr as *const u8,
            end: end as *const u8,
            width: StringWidth::TwoByte,
        }
    }

    #[inline]
    pub fn is_end(&self) -> bool {
        std::ptr::eq(self.ptr, self.end)
    }

    pub fn peek(&self) -> Option<CodeUnit> {
        if self.is_end() {
            None
        } else {
            unsafe {
                if self.width == StringWidth::OneByte {
                    Some(self.ptr.read() as u16)
                } else {
                    Some((self.ptr as *const u16).read())
                }
            }
        }
    }

    pub fn peek_back(&self) -> Option<CodeUnit> {
        if self.is_end() {
            None
        } else {
            unsafe {
                if self.width == StringWidth::OneByte {
                    Some(self.end.sub(1).read() as u16)
                } else {
                    Some((self.end.sub(2) as *const u16).read())
                }
            }
        }
    }

    pub fn ptr(&self) -> *const u8 {
        self.ptr
    }

    pub fn ptr_back(&self) -> *const u8 {
        self.end
    }

    pub fn width(&self) -> StringWidth {
        self.width
    }

    #[inline]
    pub fn consume_equals(&mut self, other: &mut CodeUnitIterator) -> bool {
        loop {
            match (self.next(), other.next()) {
                (None, None) => return true,
                (None, Some(_)) | (Some(_), None) => return false,
                (Some(code_unit_1), Some(code_unit_2)) => {
                    if code_unit_1 != code_unit_2 {
                        return false;
                    }
                }
            }
        }
    }
}

impl Iterator for CodeUnitIterator {
    type Item = CodeUnit;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_end() {
            None
        } else {
            unsafe {
                if self.width == StringWidth::OneByte {
                    let item = self.ptr.read() as u16;
                    self.ptr = self.ptr.add(1);
                    Some(item)
                } else {
                    let item = (self.ptr as *const u16).read();
                    self.ptr = self.ptr.add(2);
                    Some(item)
                }
            }
        }
    }
}

impl DoubleEndedIterator for CodeUnitIterator {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.is_end() {
            None
        } else {
            unsafe {
                if self.width == StringWidth::OneByte {
                    self.end = self.end.sub(1);
                    Some(self.end.read() as u16)
                } else {
                    self.end = self.end.sub(2);
                    Some((self.end as *const u16).read())
                }
            }
        }
    }
}

pub struct CodePointIterator {
    iter: CodeUnitIterator,
}

impl CodePointIterator {
    fn from_one_byte(string: &OneByteString) -> Self {
        CodePointIterator { iter: CodeUnitIterator::from_one_byte(string) }
    }

    fn from_two_byte(string: &TwoByteString) -> Self {
        CodePointIterator { iter: CodeUnitIterator::from_two_byte(string) }
    }

    fn is_end(&self) -> bool {
        self.iter.is_end()
    }

    fn ptr(&self) -> *const u8 {
        self.iter.ptr()
    }

    fn ptr_back(&self) -> *const u8 {
        self.iter.ptr_back()
    }

    fn width(&self) -> StringWidth {
        self.iter.width()
    }
}

impl Iterator for CodePointIterator {
    type Item = CodePoint;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(code_unit) => {
                // High surrogate may be followed by a low surrogate, in which case the surrogate
                // pair encodes the full code point.
                if is_high_surrogate_code_unit(code_unit) {
                    match self.iter.peek() {
                        Some(next_code_unit) if is_low_surrogate_code_unit(next_code_unit) => {
                            self.iter.next();
                            Some(code_point_from_surrogate_pair(code_unit, next_code_unit))
                        }
                        // High surrogate was not the start of a surrogate pair so return it directly
                        _ => Some(code_unit as u32),
                    }
                } else {
                    // Both non-surrogate and high surrogate code points are returned directly as
                    // they are not the start of a surrogate pair.
                    Some(code_unit as u32)
                }
            }
        }
    }
}

impl DoubleEndedIterator for CodePointIterator {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.iter.next_back() {
            None => None,
            Some(code_unit) => {
                // Low surrogate may followa high surrogate, in which case the surrogate pair
                // encodes the full code point.
                if is_low_surrogate_code_unit(code_unit) {
                    match self.iter.peek_back() {
                        Some(prev_code_unit) if is_high_surrogate_code_unit(prev_code_unit) => {
                            self.iter.next();
                            Some(code_point_from_surrogate_pair(prev_code_unit, code_unit))
                        }
                        // Low surrogate was not the start of a surrogate pair so return it directly
                        _ => Some(code_unit as u32),
                    }
                } else {
                    // Both non-surrogate and low surrogate code points are returned directly as
                    // they are not the start of a surrogate pair.
                    Some(code_unit as u32)
                }
            }
        }
    }
}

impl GcDeref for StringValue {}

impl fmt::Display for Gc<StringValue> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let iter = self.iter_code_points();

        for code_point in iter {
            let char = char::from_u32(code_point).unwrap_or(char::REPLACEMENT_CHARACTER);
            f.write_char(char)?;
        }

        Ok(())
    }
}

impl PartialEq for Gc<StringValue> {
    fn eq(&self, other: &Self) -> bool {
        // Fast pass if lengths differ
        if self.len() != other.len() {
            return false;
        }

        let mut iter1 = self.iter_code_units();
        let mut iter2 = other.iter_code_units();

        iter1.consume_equals(&mut iter2)
    }
}

impl Eq for Gc<StringValue> {}

impl PartialOrd for Gc<StringValue> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let mut iter1 = self.iter_code_units();
        let mut iter2 = other.iter_code_units();

        loop {
            match (iter1.next(), iter2.next()) {
                (None, None) => return Some(Ordering::Equal),
                (None, Some(_)) => return Some(Ordering::Less),
                (Some(_), None) => return Some(Ordering::Greater),
                (Some(code_unit_1), Some(code_unit_2)) => {
                    if code_unit_1 < code_unit_2 {
                        return Some(Ordering::Less);
                    } else if code_unit_1 > code_unit_2 {
                        return Some(Ordering::Greater);
                    }
                }
            }
        }
    }
}

impl hash::Hash for Gc<StringValue> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        let iter = self.iter_code_units();

        for code_unit in iter {
            code_unit.hash(state)
        }
    }
}

impl OneByteString {
    fn from_vec(buf: Vec<u8>) -> Self {
        let string = OneByteString { ptr: buf.as_ptr(), len: buf.len() };

        // Memory is managed by heap, and destructor is called when string is garbage collected
        std::mem::forget(buf);

        string
    }

    fn from_slice(slice: &[u8]) -> Self {
        // Copy slice to buffer
        let mut buf = Vec::with_capacity(slice.len());
        unsafe { buf.set_len(slice.len()) }
        buf.copy_from_slice(slice);

        Self::from_vec(buf)
    }

    #[inline]
    pub const fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }

    fn is_ascii(&self) -> bool {
        // TODO: Optimize by checking multiple bytes at a time using multi-byte mask
        for byte in self.as_slice() {
            if *byte >= 0x80 {
                return false;
            }
        }

        true
    }
}

impl TwoByteString {
    fn from_vec(buf: Vec<u16>) -> Self {
        let string = TwoByteString { ptr: buf.as_ptr(), len: buf.len() };

        // Memory is managed by heap, and destructor is called when string is garbage collected
        std::mem::forget(buf);

        string
    }

    fn from_slice(slice: &[u16]) -> Self {
        // Copy slice to buffer
        let mut buf = Vec::with_capacity(slice.len());
        unsafe { buf.set_len(slice.len()) }
        buf.copy_from_slice(slice);

        Self::from_vec(buf)
    }

    #[inline]
    pub const fn as_slice(&self) -> &[u16] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }
}
