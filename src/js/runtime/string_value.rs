use std::{cell::Cell, cmp::Ordering, fmt, hash};

use crate::js::common::unicode::{
    code_point_from_surrogate_pair, is_high_surrogate_code_unit, is_low_surrogate_code_unit,
    CodePoint, CodeUnit,
};

use super::{gc::GcDeref, Context, Gc};

pub struct StringValue {
    value: Cell<StringKind>,
}

enum StringKind {
    Utf8(String),
    Concat(ConcatString),
}

struct ConcatString {
    left: Gc<StringValue>,
    right: Gc<StringValue>,
    len: usize,
}

impl StringValue {
    fn new(value: StringKind) -> StringValue {
        StringValue { value: Cell::new(value) }
    }

    pub fn from_utf8(str: String) -> StringValue {
        StringValue::new(StringKind::Utf8(str))
    }

    pub fn from_code_unit(cx: &mut Context, code_unit: CodeUnit) -> Gc<StringValue> {
        // TODO: Create two-byte strings
        if code_unit >= 256 {
            unimplemented!("Two-byte strings")
        }

        cx.heap.alloc_string(String::from(code_unit as u8 as char))
    }

    pub fn from_code_point(cx: &mut Context, code_point: CodePoint) -> Gc<StringValue> {
        // TODO: Create two-byte strings
        if code_point >= 256 {
            unimplemented!("Two-byte strings")
        }

        cx.heap.alloc_string(String::from(code_point as u8 as char))
    }

    pub fn concat(
        cx: &mut Context,
        left: Gc<StringValue>,
        right: Gc<StringValue>,
    ) -> Gc<StringValue> {
        let new_len = left.len() + right.len();
        let concat_string = StringKind::Concat(ConcatString { left, right, len: new_len });

        cx.heap.alloc_string_value(StringValue::new(concat_string))
    }
}

impl Gc<StringValue> {
    pub fn is_empty(&self) -> bool {
        match self.value() {
            StringKind::Utf8(str) => str.is_empty(),
            StringKind::Concat(str) => str.len == 0,
        }
    }

    pub fn len(&self) -> usize {
        match self.value() {
            StringKind::Utf8(str) => str.len(),
            StringKind::Concat(str) => str.len,
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
            StringKind::Utf8(str) => str.as_bytes()[index] as u16,
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
            StringKind::Utf8(str) => str.as_bytes()[index] as u32,
        }
    }

    /// Return the index of the first occurrence of the search string in this string, starting after
    /// a given index (inclusive).
    pub fn find(&self, search_string: Gc<StringValue>, after: usize) -> Option<usize> {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.find(search_string, after)
            }
            StringKind::Utf8(str) => {
                search_string.flatten();

                // TODO: Handle non-utf8 search strings
                let search_string = if let StringKind::Utf8(search_string) = search_string.value() {
                    search_string
                } else {
                    unreachable!("String must be Utf8 after flattening")
                };

                str[after..].find(search_string)
            }
        }
    }

    /// Return the index of the last ocurrence of the search string in this string, starting before
    /// a given index (exclusive).
    pub fn rfind(&self, search_string: Gc<StringValue>, before: usize) -> Option<usize> {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.rfind(search_string, before)
            }
            StringKind::Utf8(str) => {
                search_string.flatten();

                // TODO: Handle non-utf8 search strings
                let search_string = if let StringKind::Utf8(search_string) = search_string.value() {
                    search_string
                } else {
                    unreachable!("String must be Utf8 after flattening")
                };

                str[..before].rfind(search_string)
            }
        }
    }

    /// If this string is a concat string, flatten it into a single buffer. This modifies the string
    /// value in-place and switches it from a concat string to a sequential string type.
    fn flatten(&self) {
        let concat_string = match self.value() {
            StringKind::Concat(str) => str,
            _ => return,
        };

        let mut flattened_string = String::with_capacity(concat_string.len);
        let mut stack = vec![*self];

        while let Some(current_string) = stack.pop() {
            match current_string.value() {
                StringKind::Utf8(str) => flattened_string.push_str(str.as_str()),
                StringKind::Concat(str) => {
                    stack.push(str.right);
                    stack.push(str.left);
                }
            }
        }

        self.value.set(StringKind::Utf8(flattened_string))
    }

    pub fn iter_code_units(&self) -> CodeUnitIterator {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.iter_code_units()
            }
            StringKind::Utf8(str) => CodeUnitIterator::new(&str),
        }
    }

    pub fn iter_code_points(&self) -> CodePointIterator {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.iter_code_points()
            }
            StringKind::Utf8(str) => CodePointIterator::new(&str),
        }
    }
}

pub struct CodeUnitIterator {
    ptr: *const u8,
    end: *const u8,
}

impl CodeUnitIterator {
    fn new(str: &str) -> Self {
        let ptr = str.as_bytes().as_ptr();
        let end = unsafe { ptr.add(str.len()) };

        CodeUnitIterator { ptr, end }
    }

    #[inline]
    pub fn is_end(&self) -> bool {
        std::ptr::eq(self.ptr, self.end)
    }

    pub fn peek(&self) -> Option<CodeUnit> {
        if self.is_end() {
            None
        } else {
            unsafe { Some(self.ptr.read() as u16) }
        }
    }

    pub fn ptr(&self) -> *const u8 {
        self.ptr
    }
}

impl Iterator for CodeUnitIterator {
    type Item = CodeUnit;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_end() {
            None
        } else {
            unsafe {
                let item = self.ptr.read();
                self.ptr = self.ptr.add(1);
                Some(item as u16)
            }
        }
    }
}

pub struct CodePointIterator {
    iter: CodeUnitIterator,
}

impl CodePointIterator {
    fn new(str: &str) -> Self {
        CodePointIterator { iter: CodeUnitIterator::new(str) }
    }
}

impl Iterator for CodePointIterator {
    type Item = CodePoint;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(code_unit) => {
                // Low surrogate may be followed by a high surrogate, in which case the surrogate
                // pair encodes the full code point.
                if is_low_surrogate_code_unit(code_unit) {
                    match self.iter.peek() {
                        Some(next_code_unit) if is_high_surrogate_code_unit(next_code_unit) => {
                            self.iter.next();
                            Some(code_point_from_surrogate_pair(code_unit, next_code_unit))
                        }
                        // Low surrogate was not the start of a surrogate pair so return it directly
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

impl GcDeref for StringValue {}

impl fmt::Display for Gc<StringValue> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.fmt(f)
            }
            StringKind::Utf8(str) => write!(f, "{}", str),
        }
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

        loop {
            match (iter1.next(), iter2.next()) {
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
        match self.value() {
            StringKind::Concat(_) => {
                self.flatten();
                self.hash(state)
            }
            StringKind::Utf8(str) => str.hash(state),
        }
    }
}
