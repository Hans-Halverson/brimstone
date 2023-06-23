use std::{
    cell::Cell,
    cmp::Ordering,
    collections::hash_map::DefaultHasher,
    convert::TryFrom,
    fmt::{self, Write},
    hash::{self, Hash, Hasher},
    mem::{align_of, size_of},
    num::NonZeroU32,
    ptr::copy_nonoverlapping,
};

use crate::{
    field_offset,
    js::common::unicode::{
        code_point_from_surrogate_pair, is_ascii, is_high_surrogate_code_unit, is_latin1_char,
        is_latin1_code_point, is_low_surrogate_code_unit, is_whitespace, needs_surrogate_pair,
        try_encode_surrogate_pair, CodePoint, CodeUnit,
    },
    set_uninit, static_assert,
};

use super::{
    gc::{Handle, HeapInfo, HeapPtr, IsHeapObject},
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    Context,
};

#[derive(PartialEq)]
enum StringKind {
    /// A string which is the concatenation of a left and right string.
    Concat,
    /// A string where every code unit is represented by a u8. Equivalent to the Latin1 encoding.
    /// Guaranteed to not contain any surrogate code units.
    OneByte,
    /// A string where every code unit is represented by a u16. Equivalent to UTF-16, except that
    /// surrogate code points may appear outside of surrogate pairs.
    ///
    /// May only contain code units representable in a OneByte string.
    TwoByte,
}

#[derive(Clone, Copy, PartialEq)]
pub enum StringWidth {
    OneByte,
    TwoByte,
}

/// Fields common to all strings.
#[repr(C)]
pub struct StringValue {
    descriptor: HeapPtr<ObjectDescriptor>,
    // Number of code units in the string
    len: usize,
    // Whether this string is a flat string or a concat string
    kind: StringKind,
}

impl IsHeapObject for StringValue {}

impl StringValue {
    pub fn concat(
        cx: &mut Context,
        left: Handle<StringValue>,
        right: Handle<StringValue>,
    ) -> Handle<StringValue> {
        let new_len = left.len() + right.len();
        let width = if left.width() == StringWidth::TwoByte || right.width() == StringWidth::TwoByte
        {
            StringWidth::TwoByte
        } else {
            StringWidth::OneByte
        };

        ConcatString::new(cx, left, right, new_len, width)
    }

    pub fn concat_all(cx: &mut Context, strings: &[Handle<StringValue>]) -> Handle<StringValue> {
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

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_flat(&self) -> bool {
        self.kind != StringKind::Concat
    }

    fn cx(&self) -> &mut Context {
        HeapInfo::from_raw_heap_ptr(self as *const _).cx()
    }
}

impl HeapPtr<StringValue> {
    #[inline]
    pub fn as_flat(&self) -> HeapPtr<FlatString> {
        self.cast()
    }

    #[inline]
    pub fn as_concat_opt(&self) -> Option<HeapPtr<ConcatString>> {
        if self.kind == StringKind::Concat {
            Some(self.cast())
        } else {
            None
        }
    }

    #[inline]
    pub fn width(&self) -> StringWidth {
        if let Some(concat_string) = self.as_concat_opt() {
            concat_string.width()
        } else {
            self.as_flat().width()
        }
    }
}

impl Handle<StringValue> {
    #[inline]
    pub fn as_concat_opt(&self) -> Option<Handle<ConcatString>> {
        if self.kind == StringKind::Concat {
            Some(self.cast())
        } else {
            None
        }
    }

    #[inline]
    pub fn as_flat(&self) -> Handle<FlatString> {
        self.cast()
    }

    pub fn eq(&self, other: &Self) -> bool {
        // Fast path if lengths differ
        if self.len() != other.len() {
            return false;
        }

        // First flatten so that we do not allocate while iterating
        let flat_string_1 = self.flatten();
        let flat_string_2 = other.flatten();

        flat_string_1 == flat_string_2
    }

    pub fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // First flatten so that we do not allocate while iterating
        let flat_string_1 = self.flatten();
        let flat_string_2 = other.flatten();

        // Cannot allocate while iterating
        let mut iter1 = flat_string_1.iter_code_units();
        let mut iter2 = flat_string_2.iter_code_units();

        loop {
            match (iter1.next(), iter2.next()) {
                (None, None) => return Ordering::Equal,
                (None, Some(_)) => return Ordering::Less,
                (Some(_), None) => return Ordering::Greater,
                (Some(code_unit_1), Some(code_unit_2)) => {
                    if code_unit_1 < code_unit_2 {
                        return Ordering::Less;
                    } else if code_unit_1 > code_unit_2 {
                        return Ordering::Greater;
                    }
                }
            }
        }
    }

    /// Return the 16-bit code unit at the given index. This may be a surrogate pair that must be
    /// combined with an adjacent code unit to form a full unicode code point.
    ///
    /// Does not bounds check, so only call if index is known to be in range.
    pub fn code_unit_at(&self, index: usize) -> CodeUnit {
        let flat_string = self.flatten();
        flat_string.code_unit_at(index)
    }

    /// Return the unicode code point that starts with the code unit at the given index. If pointing
    /// to a surrogate code unit that is not the start of a valid code point then return the value
    /// of that surrogate code unit.
    ///
    /// Does not bounds check, so only call if index is known to be in range.
    pub fn code_point_at(&self, index: usize) -> CodePoint {
        let flat_string = self.flatten();
        flat_string.code_point_at(index)
    }

    /// Return a substring of this string between the given indices. Indices refer to a half-open
    /// range of code units in the string. This function does not bounds check, so caller must make
    /// sure that 0 <= start <= end < string length.
    pub fn substring(&self, cx: &mut Context, start: usize, end: usize) -> Handle<FlatString> {
        let flat_string = self.flatten();

        match flat_string.width() {
            StringWidth::OneByte => {
                let string_slice = &flat_string.as_one_byte_slice()[start..end];
                FlatString::new_one_byte(cx, string_slice).to_handle()
            }
            StringWidth::TwoByte => {
                let string_slice = &flat_string.as_two_byte_slice()[start..end];
                FlatString::new_two_byte(cx, string_slice).to_handle()
            }
        }
    }

    /// Return the index of the first occurrence of the search string in this string, starting after
    /// a given index (inclusive). This function does not bounds check the after index, so caller
    /// must be make sure to only pass an after index that is less than the length of the string.
    pub fn find(&self, search_string: Handle<StringValue>, after: usize) -> Option<usize> {
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
    pub fn rfind(&self, search_string: Handle<StringValue>, before: usize) -> Option<usize> {
        // Find the end index (exclusive) of the string range to check
        let search_string_len = search_string.len();
        let end_index = usize::min(before + search_string_len, self.len());

        // Search string does not fit before the given index
        if search_string_len > end_index {
            return None;
        }

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
    pub fn flatten(&self) -> Handle<FlatString> {
        let mut concat_string = if let Some(concat_string) = self.as_concat_opt() {
            // Is a forwarding concat string when right is None
            if concat_string.right.is_none() {
                return concat_string.left.as_flat().to_handle();
            } else {
                // Other concat strings need to be flattened
                concat_string
            }
        } else {
            // Otherwise is already flat
            return self.as_flat();
        };

        let length = concat_string.len;
        let mut index = 0;

        if concat_string.width == StringWidth::OneByte {
            let mut buf: Vec<u8> = Vec::with_capacity(length);
            unsafe { buf.set_len(length) };

            let mut stack = vec![concat_string.as_string()];

            while let Some(current_string) = stack.pop() {
                if let Some(str) = current_string.as_concat_opt() {
                    if let Some(right) = str.right {
                        stack.push(right);
                    }

                    stack.push(str.left);
                } else {
                    let str = current_string.as_flat();
                    match str.width() {
                        StringWidth::OneByte => {
                            for code_unit in CodeUnitIterator::from_one_byte(str) {
                                buf[index] = code_unit as u8;
                                index += 1;
                            }
                        }
                        StringWidth::TwoByte => {
                            unreachable!(
                                "Two-byte strings cannot appear in one-byte concat strings",
                            )
                        }
                    }
                }
            }

            let cx = self.cx();
            let flat_string = FlatString::new_one_byte(cx, &buf);

            concat_string.left = flat_string.as_string();
            concat_string.right = None;

            flat_string.to_handle()
        } else {
            let mut buf: Vec<u16> = Vec::with_capacity(length);
            unsafe { buf.set_len(length) };

            let mut stack = vec![concat_string.as_string()];

            while let Some(current_string) = stack.pop() {
                if let Some(str) = current_string.as_concat_opt() {
                    if let Some(right) = str.right {
                        stack.push(right);
                    }

                    stack.push(str.left);
                } else {
                    let str = current_string.as_flat();
                    match str.width() {
                        StringWidth::OneByte => {
                            for code_unit in CodeUnitIterator::from_one_byte(str) {
                                buf[index] = code_unit;
                                index += 1;
                            }
                        }
                        StringWidth::TwoByte => {
                            for code_unit in CodeUnitIterator::from_two_byte(str) {
                                buf[index] = code_unit;
                                index += 1;
                            }
                        }
                    }
                }
            }

            let cx = self.cx();
            let flat_string = FlatString::new_two_byte(cx, &buf);

            concat_string.left = flat_string.as_string();
            concat_string.right = None;

            flat_string.to_handle()
        }
    }

    pub fn trim(&self, cx: &mut Context, trim_start: bool, trim_end: bool) -> Handle<StringValue> {
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
            return cx.names.empty_string().as_string();
        }

        match code_points_iter.width() {
            StringWidth::OneByte => {
                let slice = unsafe {
                    let length = end_ptr.offset_from(start_ptr);
                    std::slice::from_raw_parts(start_ptr, length as usize)
                };

                FlatString::new_one_byte(cx, slice).as_string().to_handle()
            }
            StringWidth::TwoByte => {
                let slice = unsafe {
                    let length = (end_ptr as *const u16).offset_from(start_ptr as *const u16);
                    std::slice::from_raw_parts(start_ptr as *const u16, length as usize)
                };

                FlatString::new_two_byte(cx, slice).as_string().to_handle()
            }
        }
    }

    pub fn repeat(&self, cx: &mut Context, n: u64) -> Handle<FlatString> {
        let flat_string = self.flatten();

        match flat_string.width() {
            StringWidth::OneByte => {
                let repeated_buf = flat_string.as_one_byte_slice().repeat(n as usize);
                FlatString::new_one_byte(cx, &repeated_buf).to_handle()
            }
            StringWidth::TwoByte => {
                let repeated_buf = flat_string.as_two_byte_slice().repeat(n as usize);
                FlatString::new_two_byte(cx, &repeated_buf).to_handle()
            }
        }
    }

    pub fn substring_equals(&self, search: Handle<StringValue>, start_index: usize) -> bool {
        let mut slice_code_units =
            self.iter_slice_code_units(start_index, start_index + search.len());
        let mut search_code_units = search.iter_code_units();

        slice_code_units.consume_equals(&mut search_code_units)
    }

    pub fn to_lower_case(&self, cx: &mut Context) -> Handle<FlatString> {
        let flat_string = self.flatten();

        match flat_string.width() {
            StringWidth::OneByte => {
                // One byte fast path. Know the length of the result string in advance, and can
                // convert to lowercase with bitwise operations.
                let mut lowercased = Vec::with_capacity(flat_string.len);

                for latin1_byte in flat_string.as_one_byte_slice() {
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

                FlatString::new_one_byte(cx, &lowercased).to_handle()
            }
            StringWidth::TwoByte => {
                // Two byte slow path. Must convert each code point to lowercase one by one, each of
                // which can map to multiple code points.
                let mut lowercased = vec![];

                let code_point_iter = CodePointIterator::from_two_byte(flat_string.get_());
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

                FlatString::new_two_byte(cx, &lowercased).to_handle()
            }
        }
    }

    pub fn to_upper_case(&self, cx: &mut Context) -> Handle<FlatString> {
        let flat_string = self.flatten();

        let code_point_iter = match flat_string.width() {
            StringWidth::OneByte => {
                // Fast path for ASCII-only string, as uppercased Latin1 code points may be out of
                // one byte range or map to multiple code points.
                if flat_string.is_one_byte_ascii() {
                    let mut uppercased = Vec::with_capacity(flat_string.len);

                    for ascii_byte in flat_string.as_one_byte_slice() {
                        if *ascii_byte >= ('a' as u8) && *ascii_byte <= ('z' as u8) {
                            uppercased.push(*ascii_byte ^ 0x20);
                        } else {
                            uppercased.push(*ascii_byte);
                        }
                    }

                    return FlatString::new_one_byte(cx, &uppercased).to_handle();
                }

                CodePointIterator::from_one_byte(flat_string.get_())
            }
            StringWidth::TwoByte => CodePointIterator::from_two_byte(flat_string.get_()),
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

        FlatString::new_two_byte(cx, &uppercased).to_handle()
    }

    /// Return an iterator over the code units of this string between the provided start and end
    /// indices (start index is inclusive, end index is exclusive).
    pub fn iter_slice_code_units(&self, start: usize, end: usize) -> CodeUnitIterator {
        let flat_string = self.flatten();

        match flat_string.width() {
            StringWidth::OneByte => {
                CodeUnitIterator::from_one_byte_slice(flat_string.get_(), start, end)
            }
            StringWidth::TwoByte => {
                CodeUnitIterator::from_two_byte_slice(flat_string.get_(), start, end)
            }
        }
    }

    pub fn iter_code_units(&self) -> CodeUnitIterator {
        let flat_string = self.flatten();
        flat_string.iter_code_units()
    }

    pub fn iter_code_points(&self) -> CodePointIterator {
        let flat_string = self.flatten();
        flat_string.iter_code_points()
    }

    pub fn iter_code_points_safe(&self) -> SafeCodePointIterator {
        let flat_string = self.flatten();
        SafeCodePointIterator::from_string(flat_string)
    }
}

impl From<Handle<StringValue>> for Handle<ObjectValue> {
    fn from(value: Handle<StringValue>) -> Self {
        value.cast()
    }
}

impl fmt::Display for HeapPtr<StringValue> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_handle().fmt(f)
    }
}

impl fmt::Display for Handle<StringValue> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let iter = self.iter_code_points();

        for code_point in iter {
            let char = char::from_u32(code_point).unwrap_or(char::REPLACEMENT_CHARACTER);
            f.write_char(char)?;
        }

        Ok(())
    }
}

#[repr(C)]
pub struct FlatString {
    // Fields inherited from StringValue
    descriptor: HeapPtr<ObjectDescriptor>,
    len: usize,
    kind: StringKind,
    // Whether this is the canonical interned string for its code unit sequence
    is_interned: bool,
    // Cached hash code for this string's value, computed lazily
    hash_code: Cell<Option<NonZeroU32>>,
    // Start of the string's array of code points. Variable sized but struct has a single item
    // for alignment.
    data: [u8; 1],
}

// FlatString with no interior mutability. Keep in sync with FlatString. Needed to inspect layout
// of FlatString in const context.
//
// Can remove once https://github.com/rust-lang/rust/issues/69908 is resolved.
#[repr(C)]
struct FlatStringNoInteriorMutability {
    descriptor: HeapPtr<ObjectDescriptor>,
    len: usize,
    kind: StringKind,
    is_interned: bool,
    hash_code: Option<NonZeroU32>,
    data: [u8; 1],
}

impl IsHeapObject for FlatString {}

impl FlatString {
    const DATA_OFFSET: usize = field_offset!(FlatStringNoInteriorMutability, data);

    fn new_one_byte(cx: &mut Context, one_byte_slice: &[u8]) -> HeapPtr<FlatString> {
        let len = one_byte_slice.len();
        let size = Self::DATA_OFFSET + len * size_of::<u8>();

        let mut string = cx
            .heap
            .alloc_uninit_with_size::<FlatString>(size, align_of::<FlatString>());

        set_uninit!(string.descriptor, cx.base_descriptors.get(ObjectKind::String));
        set_uninit!(string.len, len);
        set_uninit!(string.kind, StringKind::OneByte);
        set_uninit!(string.is_interned, false);
        set_uninit!(string.hash_code, Cell::new(None));

        unsafe {
            copy_nonoverlapping(one_byte_slice.as_ptr(), string.one_byte_data().cast_mut(), len);
        }

        string
    }

    fn new_two_byte(cx: &mut Context, two_byte_slice: &[u16]) -> HeapPtr<FlatString> {
        let len = two_byte_slice.len();
        let size = Self::DATA_OFFSET + len * size_of::<u16>();

        let mut string = cx
            .heap
            .alloc_uninit_with_size::<FlatString>(size, align_of::<FlatString>());

        set_uninit!(string.descriptor, cx.base_descriptors.get(ObjectKind::String));
        set_uninit!(string.len, len);
        set_uninit!(string.kind, StringKind::TwoByte);
        set_uninit!(string.is_interned, false);
        set_uninit!(string.hash_code, Cell::new(None));

        unsafe {
            copy_nonoverlapping(two_byte_slice.as_ptr(), string.two_byte_data().cast_mut(), len);
        }

        string
    }

    pub fn from_utf8(cx: &mut Context, str: String) -> HeapPtr<FlatString> {
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

            FlatString::new_two_byte(cx, &buf)
        } else if !has_non_ascii_one_byte_chars {
            // If this string is pure ASCII then we can directly copy the UTF-8 string.
            FlatString::new_one_byte(cx, str.as_bytes())
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

            FlatString::new_one_byte(cx, &buf)
        }
    }

    pub fn from_code_unit(cx: &mut Context, code_unit: CodeUnit) -> Handle<FlatString> {
        Self::from_code_point_impl(cx, code_unit as CodePoint)
    }

    pub fn from_code_point(cx: &mut Context, code_point: CodePoint) -> Handle<FlatString> {
        Self::from_code_point_impl(cx, code_point)
    }

    #[inline]
    fn from_code_point_impl(cx: &mut Context, code_point: CodePoint) -> Handle<FlatString> {
        if is_latin1_code_point(code_point) {
            FlatString::new_one_byte(cx, &[code_point as u8]).to_handle()
        } else {
            match try_encode_surrogate_pair(code_point) {
                None => FlatString::new_two_byte(cx, &[code_point as CodeUnit]).to_handle(),
                Some((high, low)) => FlatString::new_two_byte(cx, &[high, low]).to_handle(),
            }
        }
    }

    #[inline]
    pub fn width(&self) -> StringWidth {
        if self.kind == StringKind::OneByte {
            StringWidth::OneByte
        } else {
            StringWidth::TwoByte
        }
    }

    pub fn is_interned(&self) -> bool {
        self.is_interned
    }

    pub fn intern(&mut self) {
        self.is_interned = true
    }

    #[inline]
    pub const fn one_byte_data(&self) -> *const u8 {
        self.data.as_ptr()
    }

    #[inline]
    pub const fn two_byte_data(&self) -> *const u16 {
        self.data.as_ptr() as *const u16
    }

    #[inline]
    pub const fn as_one_byte_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.one_byte_data(), self.len) }
    }

    #[inline]
    pub const fn as_two_byte_slice(&self) -> &[u16] {
        unsafe { std::slice::from_raw_parts(self.two_byte_data(), self.len) }
    }

    /// Whether this is a one byte string containing all ASCII characters
    fn is_one_byte_ascii(&self) -> bool {
        // TODO: Optimize by checking multiple bytes at a time using multi-byte mask
        for byte in self.as_one_byte_slice() {
            if *byte >= 0x80 {
                return false;
            }
        }

        true
    }

    #[inline]
    fn code_unit_at(&self, index: usize) -> CodeUnit {
        match self.width() {
            StringWidth::OneByte => self.as_one_byte_slice()[index] as CodeUnit,
            StringWidth::TwoByte => self.as_two_byte_slice()[index],
        }
    }

    #[inline]
    fn code_point_at(&self, index: usize) -> CodePoint {
        match self.width() {
            // Every byte in a one byte string is a code point
            StringWidth::OneByte => self.as_one_byte_slice()[index] as CodePoint,
            // Must check if this index is the start of a surrogate pair
            StringWidth::TwoByte => {
                let code_unit = self.as_two_byte_slice()[index];
                if is_high_surrogate_code_unit(code_unit) && index + 1 < self.len {
                    let next_code_unit = self.as_two_byte_slice()[index + 1];
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
}

impl HeapPtr<FlatString> {
    #[inline]
    pub fn as_string(&self) -> HeapPtr<StringValue> {
        self.cast()
    }

    pub fn hash_code(&self) -> NonZeroU32 {
        match self.hash_code.get() {
            Some(hash_code) => hash_code,
            // Lazily compute and cache hash code
            None => {
                let mut hasher = DefaultHasher::new();

                for code_unit in self.iter_code_units() {
                    code_unit.hash(&mut hasher);
                }

                // Truncate hash code, making sure it is nonzero
                let hash_code = hasher.finish() as u32;
                let hash_code =
                    NonZeroU32::try_from(hash_code).unwrap_or(NonZeroU32::new(1).unwrap());

                self.hash_code.set(Some(hash_code));

                hash_code
            }
        }
    }

    pub fn iter_code_units(&self) -> CodeUnitIterator {
        match self.width() {
            StringWidth::OneByte => CodeUnitIterator::from_one_byte(*self),
            StringWidth::TwoByte => CodeUnitIterator::from_two_byte(*self),
        }
    }

    pub fn iter_code_points(&self) -> CodePointIterator {
        match self.width() {
            StringWidth::OneByte => CodePointIterator::from_one_byte(*self),
            StringWidth::TwoByte => CodePointIterator::from_two_byte(*self),
        }
    }
}

impl Handle<FlatString> {
    #[inline]
    pub fn as_string(&self) -> Handle<StringValue> {
        self.cast()
    }
}

impl PartialEq for HeapPtr<FlatString> {
    fn eq(&self, other: &Self) -> bool {
        // Fast path if lengths differ
        if self.len != other.len {
            return false;
        }

        let mut iter1 = self.iter_code_units();
        let mut iter2 = other.iter_code_units();

        iter1.consume_equals(&mut iter2)
    }
}

impl PartialEq for Handle<FlatString> {
    fn eq(&self, other: &Self) -> bool {
        self.get_().eq(&other.get_())
    }
}

impl Eq for HeapPtr<FlatString> {}

impl Eq for Handle<FlatString> {}

impl hash::Hash for HeapPtr<FlatString> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.hash_code().hash(state)
    }
}

impl hash::Hash for Handle<FlatString> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.get_().hash(state)
    }
}

/// A string which is the concatenation of a left and right string. Will be lazily flattened when
/// an indexing operation is performed. Tracks the total string length, and the widest width seen
/// in the entire concat tree.
///
/// When a string is flattened the left side points to the new flat string, while the right is None.
#[repr(C)]
pub struct ConcatString {
    // Fields inherited from StringValue
    descriptor: HeapPtr<ObjectDescriptor>,
    len: usize,
    kind: StringKind,
    // Width of this concat string
    width: StringWidth,
    left: HeapPtr<StringValue>,
    right: Option<HeapPtr<StringValue>>,
}

impl IsHeapObject for ConcatString {}

impl ConcatString {
    fn new(
        cx: &mut Context,
        left: Handle<StringValue>,
        right: Handle<StringValue>,
        len: usize,
        width: StringWidth,
    ) -> Handle<StringValue> {
        let mut string = cx.heap.alloc_uninit::<ConcatString>();

        set_uninit!(string.descriptor, cx.base_descriptors.get(ObjectKind::String));
        set_uninit!(string.len, len);
        set_uninit!(string.kind, StringKind::Concat);
        set_uninit!(string.width, width);
        set_uninit!(string.left, left.get_());
        set_uninit!(string.right, Some(right.get_()));

        string.as_string().to_handle()
    }

    fn width(&self) -> StringWidth {
        self.width
    }
}

impl HeapPtr<ConcatString> {
    #[inline]
    pub fn as_string(&self) -> HeapPtr<StringValue> {
        self.cast()
    }
}

// Ensure that data will be aligned if placed after the rest of the fields
static_assert!(FlatString::DATA_OFFSET % align_of::<u16>() == 0);

/// Methods necessary to treat as a peekable stream of code units.
pub trait GenericCodeUnitIterator: Iterator<Item = CodeUnit> {
    fn peek(&self) -> Option<Self::Item>;
}

/// An iterator over the code units of a string. Is not GC-safe.
#[derive(Clone)]
pub struct CodeUnitIterator {
    ptr: *const u8,
    end: *const u8,
    width: StringWidth,
}

impl CodeUnitIterator {
    fn from_one_byte(string: HeapPtr<FlatString>) -> Self {
        let ptr = string.one_byte_data();
        let end = unsafe { ptr.add(string.len) };

        CodeUnitIterator { ptr, end, width: StringWidth::OneByte }
    }

    fn from_two_byte(string: HeapPtr<FlatString>) -> Self {
        let ptr = string.two_byte_data();
        let end = unsafe { ptr.add(string.len) };

        CodeUnitIterator {
            ptr: ptr as *const u8,
            end: end as *const u8,
            width: StringWidth::TwoByte,
        }
    }

    fn from_one_byte_slice(string: HeapPtr<FlatString>, start: usize, end: usize) -> Self {
        let ptr = unsafe { string.one_byte_data().add(start) };
        let end = unsafe { string.one_byte_data().add(end) };

        CodeUnitIterator { ptr, end, width: StringWidth::OneByte }
    }

    fn from_two_byte_slice(string: HeapPtr<FlatString>, start: usize, end: usize) -> Self {
        let ptr = unsafe { string.two_byte_data().add(start) };
        let end = unsafe { string.two_byte_data().add(end) };

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

    fn peek_back(&self) -> Option<CodeUnit> {
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
                    self.ptr = (self.ptr as *const u16).add(1) as *const u8;
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

impl GenericCodeUnitIterator for CodeUnitIterator {
    fn peek(&self) -> Option<CodeUnit> {
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
}

/// An iterator over the code units of a string. Is GC-safe.
#[derive(Clone)]
pub struct SafeCodeUnitIterator {
    // String that is being iterated over
    string: HeapPtr<FlatString>,
    // Index of the next code unit to return in the string
    index: usize,
}

impl SafeCodeUnitIterator {
    fn from_string(string: Handle<FlatString>) -> Self {
        SafeCodeUnitIterator { string: string.get_(), index: 0 }
    }
}

impl Iterator for SafeCodeUnitIterator {
    type Item = CodeUnit;

    fn next(&mut self) -> Option<Self::Item> {
        let current_index = self.index;
        if current_index >= self.string.len {
            None
        } else {
            self.index += 1;
            Some(self.string.code_unit_at(current_index))
        }
    }
}

impl GenericCodeUnitIterator for SafeCodeUnitIterator {
    fn peek(&self) -> Option<CodeUnit> {
        let current_index = self.index;
        if current_index >= self.string.len {
            None
        } else {
            Some(self.string.code_unit_at(current_index))
        }
    }
}

pub struct GenericCodePointIterator<I: GenericCodeUnitIterator> {
    iter: I,
}

impl<T: GenericCodeUnitIterator> Iterator for GenericCodePointIterator<T> {
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

/// An iterator over the code points of a string. Is not GC-safe.
pub type CodePointIterator = GenericCodePointIterator<CodeUnitIterator>;

impl CodePointIterator {
    fn from_one_byte(string: HeapPtr<FlatString>) -> Self {
        CodePointIterator { iter: CodeUnitIterator::from_one_byte(string) }
    }

    fn from_two_byte(string: HeapPtr<FlatString>) -> Self {
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

/// An iterator over the code points of a string. Is GC-safe.
pub type SafeCodePointIterator = GenericCodePointIterator<SafeCodeUnitIterator>;

impl SafeCodePointIterator {
    fn from_string(string: Handle<FlatString>) -> Self {
        SafeCodePointIterator { iter: SafeCodeUnitIterator::from_string(string) }
    }
}
