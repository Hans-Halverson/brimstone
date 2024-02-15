use std::fmt;

use super::{
    stack_frame::{FIRST_ARGUMENT_SLOT_INDEX, RECEIVER_SLOT_INDEX, SCOPE_SLOT_INDEX},
    width::{ExtraWide, Narrow, SignedWidthRepr, UnsignedWidthRepr, Wide, Width, WidthEnum},
};

pub trait Operand<W: Width>: fmt::Display {
    /// Construct an operand from a value of the appropriate width.
    fn from_unsigned(value: W::UInt) -> Self;
    fn from_signed(value: W::SInt) -> Self;

    /// Return the inner value.
    fn unsigned(&self) -> W::UInt;
    fn signed(&self) -> W::SInt;

    /// Construct an operand if the value fits in the operand's width, otherwise return None.
    fn try_from_unsigned(value: usize) -> Option<Self>
    where
        Self: Sized;
    fn try_from_signed(value: isize) -> Option<Self>
    where
        Self: Sized;

    /// Return the minimum width that would fit the value of this operand.
    fn min_width(&self) -> WidthEnum;
}

macro_rules! operand_type {
    ($name:ident, $sign:ident) => {
        pub struct $name<W: Width>(W::UInt);

        impl<W: Width> Operand<W> for $name<W> {
            #[inline]
            fn from_unsigned(value: W::UInt) -> Self {
                $name(value)
            }

            #[inline]
            fn from_signed(value: W::SInt) -> Self {
                $name(value.as_unsigned())
            }

            #[inline]
            fn unsigned(&self) -> W::UInt {
                self.0
            }

            #[inline]
            fn signed(&self) -> W::SInt {
                self.0.as_signed()
            }

            fn try_from_unsigned(value: usize) -> Option<Self> {
                if value <= W::UNSIGNED_MAX {
                    Some(Self::from_unsigned(W::UInt::from_usize(value)))
                } else {
                    None
                }
            }

            fn try_from_signed(value: isize) -> Option<Self> {
                if W::SIGNED_MIN <= value && value <= W::SIGNED_MAX {
                    Some(Self::from_signed(W::SInt::from_isize(value)))
                } else {
                    None
                }
            }

            fn min_width(&self) -> WidthEnum {
                if $sign == SIGNED {
                    min_width_for_signed(self.signed().to_isize())
                } else {
                    min_width_for_unsigned(self.unsigned().to_usize())
                }
            }
        }

        impl $name<ExtraWide> {
            #[inline]
            pub fn to_narrow(&self) -> $name<Narrow> {
                $name::from_unsigned(self.unsigned() as <Narrow as Width>::UInt)
            }

            #[inline]
            pub fn to_wide(&self) -> $name<Wide> {
                $name::from_unsigned(self.unsigned() as <Wide as Width>::UInt)
            }
        }

        impl<W: Width> Clone for $name<W> {
            #[inline]
            fn clone(&self) -> Self {
                Self(self.0)
            }
        }

        impl<W: Width> Copy for $name<W> {}

        impl<W: Width> PartialEq for $name<W> {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                self.unsigned() == other.unsigned()
            }
        }
    };
}

const SIGNED: bool = true;
const UNSIGNED: bool = false;

// Registers are either arguments or locals (including temporaries) within a function
operand_type!(Register, SIGNED);

// Unsigned integers
operand_type!(UInt, UNSIGNED);

// Signed integers
operand_type!(SInt, SIGNED);

// An index into the constant table
operand_type!(ConstantIndex, UNSIGNED);

pub enum OperandType {
    Register,
    UInt,
    SInt,
    ConstantIndex,
}

/// Registers may be either registers local to a function or arguments to that function. Registers
/// are encoded as an isize relative stack slot index so that the register or argument is present at
/// fp + 8 * stack_slot_index.
impl<W: Width> Register<W> {
    pub const MAX_ARGUMENT_INDEX: usize = decode_argument_register(W::SInt::MAX);

    pub const MAX_LOCAL_INDEX: usize = decode_local_register(W::SInt::MIN);

    /// Construct a register for the argument with the given index.
    ///
    /// Must only be called for indices less than or equal to Self::MAX_ARGUMENT_INDEX.
    #[inline]
    pub fn argument(index: usize) -> Self {
        debug_assert!(index <= Self::MAX_ARGUMENT_INDEX);

        let encoded_value = encode_argument_register(index);
        Self::from_signed(W::SInt::from_isize(encoded_value))
    }

    /// Construct a local register with the given index.
    ///
    /// Must only be called for indices less than or equal to Self::MAX_LOCAL_INDEX.
    #[inline]
    pub fn local(index: usize) -> Self {
        debug_assert!(index <= Self::MAX_LOCAL_INDEX);

        let encoded_value = encode_local_register(index);
        Self::from_signed(W::SInt::from_isize(encoded_value))
    }

    /// Construct a register referencing the current `this` value.
    #[inline]
    pub fn this() -> Self {
        Self::from_unsigned(W::UInt::from_usize(RECEIVER_SLOT_INDEX))
    }

    /// Construct a register referencing the current scope in the scope chain.
    #[inline]
    pub fn scope() -> Self {
        Self::from_unsigned(W::UInt::from_usize(SCOPE_SLOT_INDEX))
    }

    #[inline]
    pub fn is_argument(&self) -> bool {
        self.signed().to_isize() >= FIRST_ARGUMENT_SLOT_INDEX as isize
    }

    #[inline]
    pub fn is_local(&self) -> bool {
        self.signed().to_isize() < 0
    }

    #[inline]
    pub fn is_this(&self) -> bool {
        self.unsigned().to_usize() == RECEIVER_SLOT_INDEX
    }

    #[inline]
    pub fn is_scope(&self) -> bool {
        self.unsigned().to_usize() == SCOPE_SLOT_INDEX
    }

    #[inline]
    pub fn value(&self) -> W::SInt {
        self.signed()
    }

    #[inline]
    pub fn argument_index(&self) -> usize {
        decode_argument_register(self.value().to_isize())
    }

    #[inline]
    pub fn local_index(&self) -> usize {
        decode_local_register(self.value().to_isize())
    }
}

#[inline]
const fn encode_argument_register(argument_index: usize) -> isize {
    (argument_index + FIRST_ARGUMENT_SLOT_INDEX) as isize
}

#[inline]
const fn decode_argument_register(encoded_value: isize) -> usize {
    encoded_value as usize - FIRST_ARGUMENT_SLOT_INDEX
}

#[inline]
const fn encode_local_register(register_index: usize) -> isize {
    -1 - (register_index as isize)
}

#[inline]
const fn decode_local_register(encoded_value: isize) -> usize {
    (-1 - encoded_value) as usize
}

impl<W: Width> fmt::Display for Register<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_local() {
            write!(f, "r{}", self.local_index())
        } else if self.is_argument() {
            write!(f, "a{}", self.argument_index())
        } else if self.is_this() {
            write!(f, "<this>")
        } else if self.is_scope() {
            write!(f, "<scope>")
        } else {
            unreachable!("Unknown register type")
        }
    }
}

impl<W: Width> UInt<W> {
    #[inline]
    pub fn new(value: W::UInt) -> Self {
        Self::from_unsigned(value)
    }

    #[inline]
    pub fn value(&self) -> W::UInt {
        self.unsigned()
    }
}

impl<W: Width> fmt::Display for UInt<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl<W: Width> SInt<W> {
    #[inline]
    pub fn new(value: W::SInt) -> Self {
        Self::from_signed(value)
    }

    #[inline]
    pub fn value(&self) -> W::SInt {
        self.signed()
    }
}

impl<W: Width> fmt::Display for SInt<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl<W: Width> ConstantIndex<W> {
    #[inline]
    pub fn new(value: W::UInt) -> Self {
        Self::from_unsigned(value)
    }

    #[inline]
    pub fn value(&self) -> W::UInt {
        self.unsigned()
    }
}

impl<W: Width> fmt::Display for ConstantIndex<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "c{}", self.value())
    }
}

/// Return the minimum width needed to fit the given signed value.
pub fn min_width_for_signed(value: isize) -> WidthEnum {
    if value <= Narrow::SIGNED_MAX && value >= Narrow::SIGNED_MIN {
        WidthEnum::Narrow
    } else if value <= Wide::SIGNED_MAX && value >= Wide::SIGNED_MIN {
        WidthEnum::Wide
    } else {
        WidthEnum::ExtraWide
    }
}

/// Return the minimum width needed to fit the given unsigned value.
pub fn min_width_for_unsigned(value: usize) -> WidthEnum {
    if value <= Narrow::UNSIGNED_MAX {
        WidthEnum::Narrow
    } else if value <= Wide::UNSIGNED_MAX {
        WidthEnum::Wide
    } else {
        WidthEnum::ExtraWide
    }
}
