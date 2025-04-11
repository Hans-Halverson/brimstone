use super::{
    string::StringWidth,
    unicode::{
        code_point_from_surrogate_pair, is_high_surrogate_code_unit, is_low_surrogate_code_unit,
        CodePoint, CodeUnit,
    },
};

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
    pub fn new_one_byte(ptr: *const u8, end: *const u8) -> Self {
        CodeUnitIterator { ptr, end, width: StringWidth::OneByte }
    }

    pub fn new_two_byte(ptr: *const u16, end: *const u16) -> Self {
        CodeUnitIterator {
            ptr: ptr as *const u8,
            end: end as *const u8,
            width: StringWidth::TwoByte,
        }
    }

    pub fn from_raw_one_byte_slice(slice: &[u8]) -> Self {
        let range = slice.as_ptr_range();
        Self::new_one_byte(range.start, range.end)
    }

    pub fn from_raw_two_byte_slice(slice: &[CodeUnit]) -> Self {
        let range = slice.as_ptr_range();
        Self::new_two_byte(range.start, range.end)
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

#[derive(Clone)]
pub struct GenericCodePointIterator<I: GenericCodeUnitIterator> {
    iter: I,
}

impl<I: GenericCodeUnitIterator> GenericCodePointIterator<I> {
    pub fn new(iter: I) -> Self {
        Self { iter }
    }

    #[inline]
    pub fn inner(&self) -> &I {
        &self.iter
    }

    #[inline]
    pub fn inner_mut(&mut self) -> &mut I {
        &mut self.iter
    }
}

impl<I: GenericCodeUnitIterator> Iterator for GenericCodePointIterator<I> {
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
    pub fn new_one_byte(ptr: *const u8, end: *const u8) -> Self {
        Self::new(CodeUnitIterator::new_one_byte(ptr, end))
    }

    pub fn new_two_byte(ptr: *const u16, end: *const u16) -> Self {
        Self::new(CodeUnitIterator::new_two_byte(ptr, end))
    }

    pub fn from_raw_one_byte_slice(slice: &[u8]) -> Self {
        Self::new(CodeUnitIterator::from_raw_one_byte_slice(slice))
    }

    pub fn from_raw_two_byte_slice(slice: &[CodeUnit]) -> Self {
        Self::new(CodeUnitIterator::from_raw_two_byte_slice(slice))
    }

    pub fn ptr(&self) -> *const u8 {
        self.iter.ptr()
    }

    pub fn ptr_back(&self) -> *const u8 {
        self.iter.ptr_back()
    }

    pub fn width(&self) -> StringWidth {
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
