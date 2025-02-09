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
    js::common::{
        unicode::{
            code_point_from_surrogate_pair, is_ascii, is_high_surrogate_code_unit, is_latin1,
            is_low_surrogate_code_unit, is_newline, is_surrogate_code_point, is_whitespace,
            needs_surrogate_pair, try_encode_surrogate_pair, CodePoint, CodeUnit,
        },
        wtf_8::{Wtf8CodePointsIterator, Wtf8String},
    },
    set_uninit, static_assert,
};

use super::{
    debug_print::{DebugPrint, DebugPrinter},
    gc::{Handle, HeapInfo, HeapObject, HeapPtr, HeapVisitor},
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    Context, Value,
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

const MAX_STRING_LENGTH: u32 = u32::MAX - 2;

/// Fields common to all strings.
#[repr(C)]
pub struct StringValue {
    descriptor: HeapPtr<ObjectDescriptor>,
    // Number of code units in the string
    len: u32,
    // Whether this string is a flat string or a concat string
    kind: StringKind,
}

impl StringValue {
    pub fn concat(
        cx: Context,
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

    pub fn concat_all(cx: Context, strings: &[Handle<StringValue>]) -> Handle<StringValue> {
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

    pub fn len(&self) -> u32 {
        self.len
    }

    #[inline]
    pub fn is_flat(&self) -> bool {
        self.kind != StringKind::Concat
    }

    fn cx(&self) -> Context {
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

impl DebugPrint for HeapPtr<StringValue> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        if let Some(concat_string) = self.as_concat_opt() {
            concat_string.debug_format(printer)
        } else {
            self.as_flat().debug_format(printer)
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

    #[inline]
    pub fn as_value(&self) -> Handle<Value> {
        self.cast()
    }

    /// Return the 16-bit code unit at the given index. This may be a surrogate pair that must be
    /// combined with an adjacent code unit to form a full unicode code point.
    ///
    /// Does not bounds check, so only call if index is known to be in range.
    pub fn code_unit_at(&self, index: u32) -> CodeUnit {
        let flat_string = self.flatten();
        flat_string.code_unit_at(index)
    }

    /// Return the unicode code point that starts with the code unit at the given index. If pointing
    /// to a surrogate code unit that is not the start of a valid code point then return the value
    /// of that surrogate code unit.
    ///
    /// Does not bounds check, so only call if index is known to be in range.
    pub fn code_point_at(&self, index: u32) -> CodePoint {
        let flat_string = self.flatten();
        flat_string.code_point_at(index)
    }

    /// Return a substring of this string between the given indices. Indices refer to a half-open
    /// range of code units in the string. This function does not bounds check, so caller must make
    /// sure that 0 <= start <= end < string length.
    pub fn substring(&self, cx: Context, start: u32, end: u32) -> Handle<FlatString> {
        let flat_string = self.flatten();
        flat_string.substring(cx, start, end)
    }

    /// Return the index of the first occurrence of the search string in this string, starting after
    /// a given index (inclusive). This function does not bounds check the after index, so caller
    /// must be make sure to only pass an after index that is less than or equal to the length of
    /// the string.
    pub fn find(&self, search_string: Handle<StringValue>, after: u32) -> Option<u32> {
        // First flatten so that we do not allocate while iterating
        search_string.flatten();

        let mut string_code_units = self.iter_slice_code_units(after, self.len());
        let mut search_string_code_units = search_string.iter_code_units();

        // Find the first character in search string, immediately returning if search string is empty
        let first_search_code_unit = match search_string_code_units.next() {
            None => {
                if after <= self.len() {
                    return Some(after);
                } else {
                    return None;
                }
            }
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
    pub fn rfind(&self, search_string: Handle<StringValue>, before: u32) -> Option<u32> {
        // Find the end index (exclusive) of the string range to check
        let search_string_len = search_string.len();
        let end_index = u32::min(before + search_string_len, self.len());

        // Search string does not fit before the given index
        if search_string_len > end_index {
            return None;
        }

        // First flatten so that we do not allocate while iterating
        search_string.flatten();

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
            let mut buf: Vec<u8> = Vec::with_capacity(string_index_to_usize(length));
            let uninit_buf = buf.spare_capacity_mut();

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
                                uninit_buf[index].write(code_unit as u8);
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

            unsafe { buf.set_len(string_index_to_usize(length)) };

            let cx = self.cx();
            let flat_string = FlatString::new_one_byte(cx, &buf);

            concat_string.left = flat_string.as_string();
            concat_string.right = None;

            flat_string.to_handle()
        } else {
            let mut buf: Vec<u16> = Vec::with_capacity(string_index_to_usize(length));
            let uninit_buf = buf.spare_capacity_mut();

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
                                uninit_buf[index].write(code_unit);
                                index += 1;
                            }
                        }
                        StringWidth::TwoByte => {
                            for code_unit in CodeUnitIterator::from_two_byte(str) {
                                uninit_buf[index].write(code_unit);
                                index += 1;
                            }
                        }
                    }
                }
            }

            unsafe { buf.set_len(string_index_to_usize(length)) };

            let cx = self.cx();
            let flat_string = FlatString::new_two_byte(cx, &buf);

            concat_string.left = flat_string.as_string();
            concat_string.right = None;

            flat_string.to_handle()
        }
    }

    pub fn trim(&self, cx: Context, trim_start: bool, trim_end: bool) -> Handle<StringValue> {
        let mut code_points_iter = self.iter_code_points();

        let mut start_ptr = code_points_iter.ptr();

        if trim_start {
            while let Some(code_point) = code_points_iter.next() {
                if !is_whitespace(code_point) && !is_newline(code_point) {
                    break;
                }

                start_ptr = code_points_iter.ptr();
            }
        }

        let mut end_ptr = code_points_iter.ptr_back();

        if trim_end {
            while let Some(code_point) = code_points_iter.next_back() {
                if !is_whitespace(code_point) && !is_newline(code_point) {
                    break;
                }

                end_ptr = code_points_iter.ptr_back();
            }
        }

        if start_ptr == end_ptr {
            return cx.names.empty_string().as_string();
        }

        match code_points_iter.width() {
            StringWidth::OneByte => {
                // Must copy into a temporary buffer as GC may occur
                let buf = unsafe {
                    let length = end_ptr.offset_from(start_ptr);
                    std::slice::from_raw_parts(start_ptr, length as usize).to_owned()
                };

                FlatString::new_one_byte(cx, &buf).as_string().to_handle()
            }
            StringWidth::TwoByte => {
                // Must copy into a temporary buffer as GC may occur
                let buf = unsafe {
                    let length = (end_ptr as *const u16).offset_from(start_ptr as *const u16);
                    std::slice::from_raw_parts(start_ptr as *const u16, length as usize).to_owned()
                };

                FlatString::new_two_byte(cx, &buf).as_string().to_handle()
            }
        }
    }

    pub fn repeat(&self, cx: Context, n: u32) -> Handle<FlatString> {
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

    pub fn substring_equals(&self, search: Handle<StringValue>, start_index: u32) -> bool {
        // First flatten so that we do not allocate while iterating
        search.flatten();

        let mut slice_code_units =
            self.iter_slice_code_units(start_index, start_index + search.len());
        let mut search_code_units = search.iter_code_units();

        slice_code_units.consume_equals(&mut search_code_units)
    }

    pub fn to_lower_case(self, cx: Context) -> Handle<FlatString> {
        let flat_string = self.flatten();

        match flat_string.width() {
            StringWidth::OneByte => {
                // One byte fast path. Know the length of the result string in advance, and can
                // convert to lowercase with bitwise operations.
                let mut lowercased = Vec::with_capacity(string_index_to_usize(flat_string.len));

                for latin1_byte in flat_string.as_one_byte_slice() {
                    let set_lowercase = if *latin1_byte < 0x80 {
                        // ASCII lowercase
                        *latin1_byte >= b'A' && *latin1_byte <= b'Z'
                    } else {
                        // Latin1 lowercase
                        *latin1_byte >= b'\xC0' && *latin1_byte <= b'\xDE'
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
                // Two byte slow path - must convert to valid str ranges for to_lowercase function
                let iter = CodePointIterator::from_two_byte(*flat_string);
                let lowercased = map_valid_substrings(iter, |str| str.to_lowercase());

                FlatString::from_wtf8(cx, lowercased.as_bytes()).to_handle()
            }
        }
    }

    pub fn to_upper_case(self, cx: Context) -> Handle<FlatString> {
        let flat_string = self.flatten();

        let code_point_iter = match flat_string.width() {
            StringWidth::OneByte => {
                // Fast path for ASCII-only string, as uppercased Latin1 code points may be out of
                // one byte range or map to multiple code points.
                if flat_string.is_one_byte_ascii() {
                    let mut uppercased = Vec::with_capacity(string_index_to_usize(flat_string.len));

                    for ascii_byte in flat_string.as_one_byte_slice() {
                        if *ascii_byte >= b'a' && *ascii_byte <= b'z' {
                            uppercased.push(*ascii_byte ^ 0x20);
                        } else {
                            uppercased.push(*ascii_byte);
                        }
                    }

                    return FlatString::new_one_byte(cx, &uppercased).to_handle();
                }

                CodePointIterator::from_one_byte(*flat_string)
            }
            StringWidth::TwoByte => CodePointIterator::from_two_byte(*flat_string),
        };

        // Slow path - must convert to valid str ranges for to_uppercase function
        let uppercased = map_valid_substrings(code_point_iter, |str| str.to_uppercase());

        FlatString::from_wtf8(cx, uppercased.as_bytes()).to_handle()
    }

    pub fn to_wtf8_string(self) -> Wtf8String {
        let flat_string = self.flatten();
        flat_string.to_wtf8_string()
    }

    pub fn is_well_formed(&self) -> bool {
        let flat_string = self.flatten();
        flat_string.is_well_formed()
    }

    pub fn to_well_formed(self, cx: Context) -> HeapPtr<FlatString> {
        let flat_string = self.flatten();
        flat_string.to_well_formed(cx)
    }

    /// Return an iterator over the code units of this string between the provided start and end
    /// indices (start index is inclusive, end index is exclusive).
    pub fn iter_slice_code_units(&self, start: u32, end: u32) -> CodeUnitIterator {
        let flat_string = self.flatten();

        match flat_string.width() {
            StringWidth::OneByte => CodeUnitIterator::from_one_byte_slice(*flat_string, start, end),
            StringWidth::TwoByte => CodeUnitIterator::from_two_byte_slice(*flat_string, start, end),
        }
    }

    /// Return an iterator over the code points of this string between the provided start and end
    /// indices (start index is inclusive, end index is exclusive).
    pub fn iter_slice_code_points(&self, start: u32, end: u32) -> CodePointIterator {
        let flat_string = self.flatten();

        match flat_string.width() {
            StringWidth::OneByte => {
                CodePointIterator::from_one_byte_slice(*flat_string, start, end)
            }
            StringWidth::TwoByte => {
                CodePointIterator::from_two_byte_slice(*flat_string, start, end)
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
}

impl PartialEq for Handle<StringValue> {
    fn eq(&self, other: &Self) -> bool {
        // Fast path if lengths differ
        if self.len() != other.len() {
            return false;
        }

        // First flatten so that we do not allocate while iterating
        let flat_string_1 = self.flatten();
        let flat_string_2 = other.flatten();

        flat_string_1 == flat_string_2
    }
}

impl Eq for Handle<StringValue> {}

impl PartialOrd for Handle<StringValue> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Handle<StringValue> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // First flatten so that we do not allocate while iterating
        let flat_string_1 = self.flatten();
        let flat_string_2 = other.flatten();

        flat_string_1.cmp(&flat_string_2)
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

impl fmt::Display for Handle<FlatString> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

fn format_code_point_iter(
    f: &mut fmt::Formatter,
    iter: impl Iterator<Item = CodePoint>,
) -> fmt::Result {
    for code_point in iter {
        let char = char::from_u32(code_point).unwrap_or(char::REPLACEMENT_CHARACTER);
        f.write_char(char)?;
    }

    Ok(())
}

impl fmt::Display for Handle<StringValue> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_code_point_iter(f, self.iter_code_points())
    }
}

impl fmt::Display for HeapPtr<FlatString> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_code_point_iter(f, self.iter_code_points())
    }
}

#[repr(C)]
pub struct FlatString {
    // Fields inherited from StringValue
    descriptor: HeapPtr<ObjectDescriptor>,
    len: u32,
    kind: StringKind,
    // Whether this is the canonical interned string for its code unit sequence
    is_interned: bool,
    // Cached hash code for this string's value, computed lazily
    hash_code: Cell<Option<NonZeroU32>>,
    // Start of the string's array of code points. Variable sized but array has a single item
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
    len: u32,
    kind: StringKind,
    is_interned: bool,
    hash_code: Option<NonZeroU32>,
    data: [u8; 1],
}

impl FlatString {
    const DATA_OFFSET: usize = field_offset!(FlatStringNoInteriorMutability, data);

    fn new_one_byte(cx: Context, one_byte_slice: &[u8]) -> HeapPtr<FlatString> {
        let len = Self::check_string_length(one_byte_slice.len());

        let size = Self::calculate_size_in_bytes(len, StringWidth::OneByte);
        let mut string = cx.alloc_uninit_with_size::<FlatString>(size);

        set_uninit!(string.descriptor, cx.base_descriptors.get(ObjectKind::String));
        set_uninit!(string.len, len);
        set_uninit!(string.kind, StringKind::OneByte);
        set_uninit!(string.is_interned, false);
        set_uninit!(string.hash_code, Cell::new(None));

        unsafe {
            copy_nonoverlapping(
                one_byte_slice.as_ptr(),
                string.one_byte_data().cast_mut(),
                string_index_to_usize(len),
            );
        }

        string
    }

    fn new_two_byte(cx: Context, two_byte_slice: &[u16]) -> HeapPtr<FlatString> {
        let len = Self::check_string_length(two_byte_slice.len());

        let size = Self::calculate_size_in_bytes(len, StringWidth::TwoByte);
        let mut string = cx.alloc_uninit_with_size::<FlatString>(size);

        set_uninit!(string.descriptor, cx.base_descriptors.get(ObjectKind::String));
        set_uninit!(string.len, len);
        set_uninit!(string.kind, StringKind::TwoByte);
        set_uninit!(string.is_interned, false);
        set_uninit!(string.hash_code, Cell::new(None));

        unsafe {
            copy_nonoverlapping(
                two_byte_slice.as_ptr(),
                string.two_byte_data().cast_mut(),
                string_index_to_usize(len),
            );
        }

        string
    }

    pub fn from_wtf8(cx: Context, bytes: &[u8]) -> HeapPtr<FlatString> {
        // Scan string to find total number of code units and see if a two-byte string must be used
        let mut has_two_byte_chars = false;
        let mut has_non_ascii_one_byte_chars = false;
        let mut length: usize = 0;

        for code_point in Wtf8CodePointsIterator::new(bytes) {
            if !is_ascii(code_point) {
                if !is_latin1(code_point) {
                    has_two_byte_chars = true;

                    // Two code units are needed if this is a surrogate pair
                    if needs_surrogate_pair(code_point) {
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
            let uninit_buf = buf.spare_capacity_mut();

            let mut index = 0;
            for code_point in Wtf8CodePointsIterator::new(bytes) {
                match try_encode_surrogate_pair(code_point) {
                    None => {
                        uninit_buf[index].write(code_point as u16);
                        index += 1;
                    }
                    Some((high, low)) => {
                        uninit_buf[index].write(high);
                        uninit_buf[index + 1].write(low);
                        index += 2;
                    }
                }
            }

            unsafe { buf.set_len(length) };

            FlatString::new_two_byte(cx, &buf)
        } else if !has_non_ascii_one_byte_chars {
            // If this string is pure ASCII then we can directly copy the UTF-8 string.
            FlatString::new_one_byte(cx, bytes)
        } else {
            // Otherwise we must copy each character into a new buffer, converting from UTF-8 to Latin1
            let mut buf: Vec<u8> = Vec::with_capacity(length);
            let uninit_buf = buf.spare_capacity_mut();

            // Copy each character into the buffer
            for (index, code_point) in Wtf8CodePointsIterator::new(bytes).enumerate() {
                uninit_buf[index].write(code_point as u8);
            }

            unsafe { buf.set_len(length) };

            FlatString::new_one_byte(cx, &buf)
        }
    }

    /// Create a new one byte string from a slice of Latin1 bytes
    pub fn from_one_byte_slice(cx: Context, one_byte_slice: &[u8]) -> HeapPtr<FlatString> {
        FlatString::new_one_byte(cx, one_byte_slice)
    }

    pub fn from_code_unit(cx: Context, code_unit: CodeUnit) -> Handle<FlatString> {
        Self::from_code_points(cx, &[code_unit as CodePoint])
    }

    pub fn from_code_point(cx: Context, code_point: CodePoint) -> Handle<FlatString> {
        Self::from_code_points(cx, &[code_point])
    }

    #[inline]
    pub fn from_code_points(cx: Context, code_points: &[CodePoint]) -> Handle<FlatString> {
        let is_one_byte = code_points.iter().all(|code_point| is_latin1(*code_point));

        if is_one_byte {
            let one_byte_buf = code_points
                .iter()
                .map(|code_point| *code_point as u8)
                .collect::<Vec<_>>();
            FlatString::new_one_byte(cx, &one_byte_buf).to_handle()
        } else {
            let mut two_byte_buf = vec![];
            for code_point in code_points {
                match try_encode_surrogate_pair(*code_point) {
                    None => two_byte_buf.push(*code_point as CodeUnit),
                    Some((high, low)) => {
                        two_byte_buf.push(high);
                        two_byte_buf.push(low);
                    }
                }
            }

            FlatString::new_two_byte(cx, &two_byte_buf).to_handle()
        }
    }

    #[inline]
    fn calculate_size_in_bytes(length: u32, width: StringWidth) -> usize {
        match width {
            StringWidth::OneByte => {
                Self::DATA_OFFSET + string_index_to_usize(length) * size_of::<u8>()
            }
            StringWidth::TwoByte => {
                Self::DATA_OFFSET + string_index_to_usize(length) * size_of::<u16>()
            }
        }
    }

    fn check_string_length(length: usize) -> u32 {
        // TODO: Throw error when attempting to allocate string that is too large
        if length > MAX_STRING_LENGTH as usize {
            panic!("String length exceeds maximum string length");
        }

        length as u32
    }

    #[inline]
    pub fn len(&self) -> u32 {
        self.len
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
    pub const fn descriptor(&self) -> HeapPtr<ObjectDescriptor> {
        self.descriptor
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
        unsafe { std::slice::from_raw_parts(self.one_byte_data(), string_index_to_usize(self.len)) }
    }

    #[inline]
    pub const fn as_two_byte_slice(&self) -> &[u16] {
        unsafe { std::slice::from_raw_parts(self.two_byte_data(), string_index_to_usize(self.len)) }
    }

    #[inline]
    pub const fn one_byte_code_unit_at(&self, index: u32) -> u8 {
        self.as_one_byte_slice()[string_index_to_usize(index)]
    }

    #[inline]
    pub const fn two_byte_code_unit_at(&self, index: u32) -> u16 {
        self.as_two_byte_slice()[string_index_to_usize(index)]
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
    pub fn code_unit_at(&self, index: u32) -> CodeUnit {
        match self.width() {
            StringWidth::OneByte => self.one_byte_code_unit_at(index) as CodeUnit,
            StringWidth::TwoByte => self.two_byte_code_unit_at(index),
        }
    }

    #[inline]
    fn code_point_at(&self, index: u32) -> CodePoint {
        match self.width() {
            // Every byte in a one byte string is a code point
            StringWidth::OneByte => self.one_byte_code_unit_at(index) as CodePoint,
            // Must check if this index is the start of a surrogate pair
            StringWidth::TwoByte => {
                let code_unit = self.two_byte_code_unit_at(index);
                if is_high_surrogate_code_unit(code_unit)
                    && string_index_to_usize(index) + 1 < string_index_to_usize(self.len)
                {
                    let next_code_unit = self.two_byte_code_unit_at(index + 1);
                    if is_low_surrogate_code_unit(next_code_unit) {
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

    #[inline]
    pub fn substring(&self, cx: Context, start: u32, end: u32) -> Handle<FlatString> {
        let start = string_index_to_usize(start);
        let end = string_index_to_usize(end);

        match self.width() {
            StringWidth::OneByte => {
                // Copy substring to new buffer as allocation may trigger a GC
                let substring_buf = &self.as_one_byte_slice()[start..end].to_owned();
                FlatString::new_one_byte(cx, substring_buf).to_handle()
            }
            StringWidth::TwoByte => {
                // Copy substring to new buffer as allocation may trigger a GC
                let substring_buf = &self.as_two_byte_slice()[start..end].to_owned();
                FlatString::new_two_byte(cx, substring_buf).to_handle()
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

    pub fn to_wtf8_string(self) -> Wtf8String {
        let mut wtf8_string = Wtf8String::new();
        for code_point in self.iter_code_points() {
            wtf8_string.push(code_point);
        }

        wtf8_string
    }

    pub fn is_well_formed(&self) -> bool {
        match self.width() {
            // One byte strings are always well-formed
            StringWidth::OneByte => true,
            // Two byte strings are well-formed if they do not contain any unpaired surrogates
            StringWidth::TwoByte => {
                for code_point in self.iter_code_points() {
                    if is_surrogate_code_point(code_point) {
                        return false;
                    }
                }

                true
            }
        }
    }

    pub fn to_well_formed(self, cx: Context) -> HeapPtr<FlatString> {
        match self.width() {
            // One byte strings are always well-formed so never allocate new string
            StringWidth::OneByte => self,
            // Two byte strings are well-formed if they do not contain any unpaired surrogates
            StringWidth::TwoByte => {
                if self.is_well_formed() {
                    return self;
                }

                // Copy all code points to new string, replacing unpaired surrogates with the
                // replacement character.
                let mut wtf8_string = Wtf8String::new();

                for code_point in self.iter_code_points() {
                    if is_surrogate_code_point(code_point) {
                        wtf8_string.push_char(char::REPLACEMENT_CHARACTER);
                    } else {
                        wtf8_string.push(code_point);
                    }
                }

                FlatString::from_wtf8(cx, wtf8_string.as_bytes())
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

impl DebugPrint for HeapPtr<FlatString> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        printer.write_default_with_context("String", &self.to_string());
    }
}

impl Handle<FlatString> {
    #[inline]
    pub fn as_string(&self) -> Handle<StringValue> {
        self.cast()
    }

    #[inline]
    pub fn as_value(&self) -> Handle<Value> {
        self.cast()
    }

    pub fn iter_code_points_safe(&self) -> SafeCodePointIterator {
        SafeCodePointIterator::from_string(*self)
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
        (**self).eq(&**other)
    }
}

impl HeapPtr<FlatString> {
    /// Compare a flat string against a Rust `&str`.
    pub fn eq_str(&self, other: &str) -> bool {
        let mut iter1 = self.iter_code_points();
        let mut iter2 = other.chars();

        while let (Some(code_point_1), Some(code_point_2)) = (iter1.next(), iter2.next()) {
            if code_point_1 != code_point_2 as CodePoint {
                return false;
            }
        }

        iter1.next().is_none() && iter2.next().is_none()
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
        (**self).hash(state)
    }
}

impl PartialOrd for HeapPtr<FlatString> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for Handle<FlatString> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HeapPtr<FlatString> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Cannot allocate while iterating
        let mut iter1 = self.iter_code_units();
        let mut iter2 = other.iter_code_units();

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
}

impl Ord for Handle<FlatString> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

#[inline]
pub const fn string_index_to_usize(index: u32) -> usize {
    index as usize
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
    len: u32,
    kind: StringKind,
    // Width of this concat string
    width: StringWidth,
    left: HeapPtr<StringValue>,
    right: Option<HeapPtr<StringValue>>,
}

impl ConcatString {
    fn new(
        cx: Context,
        left: Handle<StringValue>,
        right: Handle<StringValue>,
        len: u32,
        width: StringWidth,
    ) -> Handle<StringValue> {
        let mut string = cx.alloc_uninit::<ConcatString>();

        set_uninit!(string.descriptor, cx.base_descriptors.get(ObjectKind::String));
        set_uninit!(string.len, len);
        set_uninit!(string.kind, StringKind::Concat);
        set_uninit!(string.width, width);
        set_uninit!(string.left, *left);
        set_uninit!(string.right, Some(*right));

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

impl DebugPrint for HeapPtr<ConcatString> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        printer.write_default("ConcatString");
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
        let end = unsafe { ptr.add(string_index_to_usize(string.len)) };

        CodeUnitIterator { ptr, end, width: StringWidth::OneByte }
    }

    fn from_two_byte(string: HeapPtr<FlatString>) -> Self {
        let ptr = string.two_byte_data();
        let end = unsafe { ptr.add(string_index_to_usize(string.len)) };

        CodeUnitIterator {
            ptr: ptr as *const u8,
            end: end as *const u8,
            width: StringWidth::TwoByte,
        }
    }

    fn from_one_byte_slice(string: HeapPtr<FlatString>, start: u32, end: u32) -> Self {
        let ptr = unsafe { string.one_byte_data().add(string_index_to_usize(start)) };
        let end = unsafe { string.one_byte_data().add(string_index_to_usize(end)) };

        CodeUnitIterator { ptr, end, width: StringWidth::OneByte }
    }

    fn from_two_byte_slice(string: HeapPtr<FlatString>, start: u32, end: u32) -> Self {
        let ptr = unsafe { string.two_byte_data().add(string_index_to_usize(start)) };
        let end = unsafe { string.two_byte_data().add(string_index_to_usize(end)) };

        CodeUnitIterator {
            ptr: ptr as *const u8,
            end: end as *const u8,
            width: StringWidth::TwoByte,
        }
    }

    pub fn from_raw_one_byte_slice(slice: &[u8]) -> Self {
        let range = slice.as_ptr_range();
        CodeUnitIterator {
            ptr: range.start,
            end: range.end,
            width: StringWidth::OneByte,
        }
    }

    pub fn from_raw_two_byte_slice(slice: &[CodeUnit]) -> Self {
        let range = slice.as_ptr_range();
        CodeUnitIterator {
            ptr: range.start as *const u8,
            end: range.end as *const u8,
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
    index: u32,
}

impl SafeCodeUnitIterator {
    fn from_string(string: Handle<FlatString>) -> Self {
        SafeCodeUnitIterator { string: *string, index: 0 }
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

#[derive(Clone)]
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
                        _ => Some(code_unit as CodePoint),
                    }
                } else {
                    // Both non-surrogate and high surrogate code points are returned directly as
                    // they are not the start of a surrogate pair.
                    Some(code_unit as CodePoint)
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

    fn from_one_byte_slice(string: HeapPtr<FlatString>, start: u32, end: u32) -> Self {
        CodePointIterator {
            iter: CodeUnitIterator::from_one_byte_slice(string, start, end),
        }
    }

    fn from_two_byte_slice(string: HeapPtr<FlatString>, start: u32, end: u32) -> Self {
        CodePointIterator {
            iter: CodeUnitIterator::from_two_byte_slice(string, start, end),
        }
    }

    pub fn from_raw_one_byte_slice(slice: &[u8]) -> Self {
        CodePointIterator { iter: CodeUnitIterator::from_raw_one_byte_slice(slice) }
    }

    pub fn from_raw_two_byte_slice(slice: &[CodeUnit]) -> Self {
        CodePointIterator { iter: CodeUnitIterator::from_raw_two_byte_slice(slice) }
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
                // Low surrogate may follow a high surrogate, in which case the surrogate pair
                // encodes the full code point.
                if is_low_surrogate_code_unit(code_unit) {
                    match self.iter.peek_back() {
                        Some(prev_code_unit) if is_high_surrogate_code_unit(prev_code_unit) => {
                            self.iter.next();
                            Some(code_point_from_surrogate_pair(prev_code_unit, code_unit))
                        }
                        // Low surrogate was not the start of a surrogate pair so return it directly
                        _ => Some(code_unit as CodePoint),
                    }
                } else {
                    // Both non-surrogate and low surrogate code points are returned directly as
                    // they are not the start of a surrogate pair.
                    Some(code_unit as CodePoint)
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

    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.iter.string);
    }
}

/// Break up string into sequences of valid code points and individual unpaired surrogates, then
/// apply a mapping function over valid strs leaving unpaired surrogates unchanged.
fn map_valid_substrings(iter: CodePointIterator, f: impl Fn(&str) -> String) -> Wtf8String {
    let mut result = Wtf8String::new();
    let mut current_valid_substring = String::new();

    for code_point in iter {
        if is_surrogate_code_point(code_point) {
            // First flush the valid range up until this unpaired surrogate, if there was a range
            if !current_valid_substring.is_empty() {
                let mapped_string = f(&current_valid_substring);
                result.push_str(&mapped_string);
            }

            // Add the unpaired surrogate without modification
            result.push(code_point);

            current_valid_substring.clear();
        } else {
            // Add this code point to the current valid range
            current_valid_substring.push(unsafe { char::from_u32_unchecked(code_point) });
        }
    }

    // Flush the final valid range if one exists
    if !current_valid_substring.is_empty() {
        let mapped_string = f(&current_valid_substring);
        result.push_str(&mapped_string);
    }

    result
}

impl HeapObject for HeapPtr<StringValue> {
    fn byte_size(&self) -> usize {
        if let Some(concat_string) = self.as_concat_opt() {
            concat_string.byte_size()
        } else {
            self.as_flat().byte_size()
        }
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        if let Some(mut concat_string) = self.as_concat_opt() {
            concat_string.visit_pointers(visitor)
        } else {
            self.as_flat().visit_pointers(visitor)
        }
    }
}

impl HeapObject for HeapPtr<ConcatString> {
    fn byte_size(&self) -> usize {
        size_of::<ConcatString>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.left);
        visitor.visit_pointer_opt(&mut self.right);
    }
}

impl HeapObject for HeapPtr<FlatString> {
    fn byte_size(&self) -> usize {
        FlatString::calculate_size_in_bytes(self.len, self.width())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}
