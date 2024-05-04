use std::{collections::HashMap, hash};

use crate::js::runtime::{gc::HeapItem, string_value::FlatString, Context, Handle, Value};

use super::{
    constant_table::ConstantTable,
    generator::{EmitError, EmitResult},
    operand::min_width_for_unsigned,
    width::{ExtraWide, Narrow, Wide, Width, WidthEnum},
};

#[derive(Clone, Copy)]
enum ConstantTableEntry {
    /// Interned string such as an identifier or string literal.
    String(Handle<FlatString>),
    /// Generic heap object - not deduplicated, so keep a unique incremented index as a unique id.
    HeapObject { object: Handle<HeapItem>, key: ConstantTableIndex },
    /// Double encoded as a value.
    Double(Value),
    /// Jump offset in bytes, not encoded as a value.
    BytecodeOffset(isize),
}

impl ConstantTableEntry {
    /// Convert this entry to a value that is stored in the constant table. Return the value that
    /// is stored, along with whether the entry represents an actual value (e.g. vs raw offset).
    fn to_value(&self, cx: Context) -> ToValueResult {
        match self {
            ConstantTableEntry::String(string) => ToValueResult::Value(string.cast()),
            ConstantTableEntry::HeapObject { object, .. } => ToValueResult::Value(object.cast()),
            ConstantTableEntry::Double(double) => ToValueResult::Value(double.to_handle(cx)),
            // Bytecode offsets are stored directly, not encoded as a value
            ConstantTableEntry::BytecodeOffset(offset) => {
                ToValueResult::Raw(Value::from_raw_bits(*offset as u64))
            }
        }
    }
}

enum ToValueResult {
    Value(Handle<Value>),
    Raw(Value),
}

impl ToValueResult {
    fn is_value(&self) -> bool {
        matches!(self, ToValueResult::Value(_))
    }
}

impl PartialEq for ConstantTableEntry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Strings must be interned, so we can check pointer equality
            (ConstantTableEntry::String(lhs), ConstantTableEntry::String(rhs)) => {
                lhs.get_().ptr_eq(&rhs.get_())
            }
            // Heap objects are not deduplicated, so compare the index
            (
                ConstantTableEntry::HeapObject { key: lhs, .. },
                ConstantTableEntry::HeapObject { key: rhs, .. },
            ) => lhs == rhs,
            (ConstantTableEntry::Double(lhs), ConstantTableEntry::Double(rhs)) => {
                lhs.as_raw_bits() == rhs.as_raw_bits()
            }
            (ConstantTableEntry::BytecodeOffset(lhs), ConstantTableEntry::BytecodeOffset(rhs)) => {
                lhs == rhs
            }
            _ => false,
        }
    }
}

impl Eq for ConstantTableEntry {}

impl hash::Hash for ConstantTableEntry {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            ConstantTableEntry::String(string) => string.hash(state),
            // Objects are not deduplicated, so use the unique key
            ConstantTableEntry::HeapObject { key, .. } => key.hash(state),
            // Use raw bits as hash for doubles. Note that this differentiates between +0 and -0,
            // unlike values used as ValueCollectionKeys.
            ConstantTableEntry::Double(double) => double.as_raw_bits().hash(state),
            ConstantTableEntry::BytecodeOffset(offset) => offset.hash(state),
        }
    }
}

pub type ConstantTableIndex = u32;

pub struct ConstantTableBuilder {
    /// Cache of constants that have already been added to the constant table, mapped to their index
    /// in the resulting array.
    constants: HashMap<ConstantTableEntry, ConstantTableIndex>,

    narrow_allocated: ConstantTableIndex,
    narrow_reserved: ConstantTableIndex,

    wide_allocated: ConstantTableIndex,
    wide_reserved: ConstantTableIndex,

    extra_wide_allocated: ConstantTableIndex,
    extra_wide_reserved: ConstantTableIndex,

    /// Duplicates of constants that already exist in the constants cache. These constants have a
    /// greater width than their corresponding entry in the constants table, and must still be added
    /// to the constant table at the end.
    duplicates: Vec<(ConstantTableEntry, ConstantTableIndex)>,

    /// Number of heap objects that have been added to the constant table. This is used to generate
    /// unique keys for heap objects.
    num_heap_objects: u32,
}

impl ConstantTableBuilder {
    /// Total number of entries in the narrow range of the constant table.
    const NUM_NARROW_ENTRIES: ConstantTableIndex = Narrow::UNSIGNED_MAX as ConstantTableIndex + 1;

    /// Total number of entries in the wide range of the constant table.
    const FIRST_WIDE_INDEX: ConstantTableIndex = Self::NUM_NARROW_ENTRIES;
    const NUM_WIDE_ENTRIES: ConstantTableIndex =
        Wide::UNSIGNED_MAX as ConstantTableIndex - Self::FIRST_WIDE_INDEX + 1;

    /// Total number of entries in the extra wide range of the constant table.
    const FIRST_EXTRA_WIDE_INDEX: ConstantTableIndex =
        Self::NUM_NARROW_ENTRIES + Self::NUM_WIDE_ENTRIES;
    const NUM_EXTRA_WIDE_ENTRIES: ConstantTableIndex =
        ExtraWide::UNSIGNED_MAX as ConstantTableIndex - Self::FIRST_EXTRA_WIDE_INDEX + 1;

    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),

            narrow_allocated: 0,
            narrow_reserved: 0,

            wide_allocated: 0,
            wide_reserved: 0,

            extra_wide_allocated: 0,
            extra_wide_reserved: 0,

            duplicates: vec![],

            num_heap_objects: 0,
        }
    }

    fn insert_if_missing(&mut self, key: ConstantTableEntry) -> EmitResult<ConstantTableIndex> {
        if let Some(value) = self.constants.get(&key) {
            return Ok(*value);
        }

        // Try allocating the constant index in the smallest width range possible
        let next_index = match self.next_index_width()? {
            WidthEnum::Narrow => {
                let next_index = self.narrow_allocated;
                self.narrow_allocated += 1;

                next_index
            }
            WidthEnum::Wide => {
                let next_index = self.wide_allocated + Self::FIRST_WIDE_INDEX;
                self.wide_allocated += 1;

                next_index
            }
            WidthEnum::ExtraWide => {
                let next_index = self.extra_wide_allocated + Self::FIRST_EXTRA_WIDE_INDEX;
                self.extra_wide_allocated += 1;

                next_index
            }
        };

        self.constants.insert(key, next_index);

        Ok(next_index)
    }

    /// Return the smallest available width to be used for the next constant index, or error if
    /// there is no space left in the constant table.
    fn next_index_width(&self) -> EmitResult<WidthEnum> {
        if self.narrow_allocated + self.narrow_reserved < Self::NUM_NARROW_ENTRIES {
            Ok(WidthEnum::Narrow)
        } else if self.wide_allocated + self.wide_reserved < Self::NUM_WIDE_ENTRIES {
            Ok(WidthEnum::Wide)
        } else if self.extra_wide_allocated + self.extra_wide_reserved
            < Self::NUM_EXTRA_WIDE_ENTRIES
        {
            Ok(WidthEnum::ExtraWide)
        } else {
            Err(EmitError::ConstantTableTooLarge)
        }
    }

    /// Reserve space for an entry (e.g. a forward jump offset) in the constant table. Return the
    /// width needed to encode the constant index.
    pub fn reserve_entry(&mut self) -> EmitResult<WidthEnum> {
        // Try reserving the constant index in the smallest width range possible
        let next_index_width = self.next_index_width()?;
        match next_index_width {
            WidthEnum::Narrow => {
                self.narrow_reserved += 1;
            }
            WidthEnum::Wide => {
                self.wide_reserved += 1;
            }
            WidthEnum::ExtraWide => {
                self.extra_wide_reserved += 1;
            }
        }

        Ok(next_index_width)
    }

    /// Remove a reserved entry of the provided width, allowing it to be allocated or reserved again.
    pub fn remove_reserved(&mut self, width: WidthEnum) {
        match width {
            WidthEnum::Narrow => self.narrow_reserved -= 1,
            WidthEnum::Wide => self.wide_reserved -= 1,
            WidthEnum::ExtraWide => self.extra_wide_reserved -= 1,
        }
    }

    /// Commit a reserved bytecode offset of the provided width, returning a constant index that
    /// points to that bytecode offset. The returned constant index fits into the provided width.
    ///
    /// If the offset was already added to the constant table, the existing index is returned. Care
    /// is taken in the case of a duplicate offset that was already added to the constant table with
    /// a greater width.
    pub fn commit_reserved_bytecode_offset(
        &mut self,
        width: WidthEnum,
        offset: isize,
    ) -> ConstantTableIndex {
        let entry_key = ConstantTableEntry::BytecodeOffset(offset);

        match width {
            WidthEnum::Narrow => {
                self.narrow_reserved -= 1;

                // Check if this offset was already added and can potentially be reused
                if let Some(index) = self.constants.get(&entry_key) {
                    if min_width_for_unsigned(*index as usize) <= WidthEnum::Narrow {
                        return *index;
                    }

                    // Offset was already added, but at an index with a greater width. Add the
                    // existing entry to the duplicates array since it will be overwritten by the
                    // narrow entry.
                    self.duplicates.push((entry_key, *index));
                }

                let next_index = self.narrow_allocated;
                self.constants.insert(entry_key, next_index);
                self.narrow_allocated += 1;

                next_index
            }
            WidthEnum::Wide => {
                self.wide_reserved -= 1;

                // Check if this offset was already added and can potentially be reused
                if let Some(index) = self.constants.get(&entry_key) {
                    if min_width_for_unsigned(*index as usize) <= WidthEnum::Wide {
                        return *index;
                    }

                    // Offset was already added, but at an index with a greater width. Add the
                    // existing entry to the duplicates array since it will be overwritten by the
                    // wide entry.
                    self.duplicates.push((entry_key, *index));
                }

                let next_index = self.wide_allocated + Self::FIRST_WIDE_INDEX;
                self.constants.insert(entry_key, next_index);
                self.wide_allocated += 1;

                next_index
            }
            WidthEnum::ExtraWide => {
                // If this offset was already added it can always be reused
                if let Some(index) = self.constants.get(&entry_key) {
                    return *index;
                }

                let next_index = self.extra_wide_allocated + Self::FIRST_EXTRA_WIDE_INDEX;
                self.extra_wide_reserved -= 1;
                self.extra_wide_allocated += 1;

                next_index
            }
        }
    }

    /// Add a string to the constant table. Note that the string must already be interned.
    pub fn add_string(&mut self, string: Handle<FlatString>) -> EmitResult<ConstantTableIndex> {
        debug_assert!(string.is_interned());
        self.insert_if_missing(ConstantTableEntry::String(string))
    }

    pub fn add_heap_object(&mut self, object: Handle<HeapItem>) -> EmitResult<ConstantTableIndex> {
        let key = self.num_heap_objects;
        self.num_heap_objects += 1;
        self.insert_if_missing(ConstantTableEntry::HeapObject { object, key })
    }

    pub fn add_double(&mut self, double: f64) -> EmitResult<ConstantTableIndex> {
        let double = ConstantTableEntry::Double(Value::from(double));
        self.insert_if_missing(double)
    }

    pub fn add_bytecode_offset(&mut self, offset: isize) -> EmitResult<ConstantTableIndex> {
        let offset = ConstantTableEntry::BytecodeOffset(offset);
        self.insert_if_missing(offset)
    }

    pub fn finish(&self, cx: Context) -> Option<Handle<ConstantTable>> {
        // All reservations must be released once finished generating bytecode
        debug_assert!(
            self.narrow_reserved == 0 && self.wide_reserved == 0 && self.extra_wide_reserved == 0
        );

        // Find the final length of the constant table
        let num_constants = if self.extra_wide_allocated > 0 {
            Self::FIRST_EXTRA_WIDE_INDEX + self.extra_wide_allocated
        } else if self.wide_allocated > 0 {
            Self::FIRST_WIDE_INDEX + self.wide_allocated
        } else if self.narrow_allocated > 0 {
            self.narrow_allocated
        } else {
            // No constants were added, no constant table is needed
            return None;
        };

        // Start uninitialized and fill in constants that we have allocated
        let mut constants = vec![Handle::dangling(); num_constants as usize];
        let mut metadata = vec![0; ConstantTable::calculate_metadata_size(num_constants as usize)];
        let mut raw_values = vec![Value::undefined(); num_constants as usize];

        macro_rules! iter_constants {
            ($iter:expr) => {{
                for (constant, index) in $iter {
                    let value_or_raw = constant.to_value(cx);
                    let value = match value_or_raw {
                        ToValueResult::Value(value) => value,
                        // Raw values cannot be placed behind handles in heap blocks since the GC
                        // expects valid values. Instead place in temporary vec and use a non-heap
                        // handle.
                        ToValueResult::Raw(raw_value) => {
                            raw_values[*index as usize] = raw_value;
                            Handle::<Value>::from_fixed_non_heap_ptr(&raw_values[*index as usize])
                        }
                    };

                    constants[*index as usize] = value;
                    Self::set_metadata(&mut metadata, *index as usize, value_or_raw.is_value());
                }
            }};
        }

        iter_constants!(&self.constants);
        iter_constants!(&self.duplicates);

        // There may be holes in the array due to removed reservations or placeholders that will be
        // patched later. Fill them in with undefined.
        for constant in &mut constants {
            if constant.is_dangling() {
                *constant = Value::undefined().to_handle(cx);
            }
        }

        Some(ConstantTable::new(cx, constants, metadata))
    }

    fn set_metadata(metadata: &mut Vec<u8>, index: usize, is_value: bool) {
        if is_value {
            return;
        }

        // Determine the containing byte and bit
        let byte_index = index / 8;
        let bit_mask = 1 << (index % 8);

        // Set the bit in the corresponding byte
        metadata[byte_index] |= bit_mask;
    }
}
