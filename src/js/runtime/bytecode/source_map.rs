use std::ops::Range;

use crate::{
    common::varint::decode_varint,
    parser::loc::Pos,
    runtime::{
        collections::array::ByteArray, object_descriptor::ObjectKind, Context, Handle, HeapPtr,
    },
};

/// A map from bytecode instruction offsets to the corresponding offset in the source code.
///
/// Represented as a sequence of varint pairs, where the first varint is the bytecode offset and the
/// second varint is the source offset.
///
/// Not every bytecode instruction has a corresponding source offset (e.g. it may not throw). We
/// also don't store the same source offset multiple times in a row. Each entry in the source map
/// covers the bytecode range until the next entry.
///
/// The bytecode offset stored for an instruction is the offset directly following the instruction
/// itself. This is the state of the pc (or saved pc) when the instruction is actually executing.
pub struct BytecodeSourceMap;

impl BytecodeSourceMap {
    pub fn new(cx: Context, source_positions: &[u8]) -> Handle<ByteArray> {
        ByteArray::new_from_slice(cx, ObjectKind::ByteArray, source_positions).to_handle()
    }

    /// The first two varints are the full source range of the function. Return them as a range.
    pub fn get_function_range(source_map: HeapPtr<ByteArray>) -> Range<Pos> {
        let source_map = source_map.as_slice();

        let (start_pos, num_bytes) = decode_varint(source_map);
        let (end_pos, _) = decode_varint(&source_map[num_bytes..]);

        start_pos..end_pos
    }

    /// Look up the source position for the given bytecode offset.
    ///
    /// Return if the bytecode offset could not be mapped to any values (i.e. the map is empty)
    pub fn get_source_position(
        source_map: HeapPtr<ByteArray>,
        bytecode_offset: usize,
    ) -> Option<usize> {
        let mut iter = Self::iter(&source_map);

        // Keep track of the last source position seen. Initially it is the first source position.
        let map_entry = iter.next()?;
        let mut current_source_position = map_entry.source_position;

        // Then find the last entry is that is less than or equal to the given bytecode offset.
        for map_entry in iter {
            if bytecode_offset < map_entry.bytecode_offset {
                break;
            } else {
                current_source_position = map_entry.source_position;
            }
        }

        Some(current_source_position)
    }

    fn iter(source_map: &HeapPtr<ByteArray>) -> SourceMapIter {
        SourceMapIter::new(source_map.as_slice())
    }
}

struct SourceMapIter<'a> {
    source_map: &'a [u8],
    offset: usize,
}

struct SourceMapEntry {
    bytecode_offset: usize,
    source_position: usize,
}

impl<'a> SourceMapIter<'a> {
    fn new(source_map: &'a [u8]) -> Self {
        let mut iter = Self { source_map, offset: 0 };

        // Skip the first entry, which is the source range of the entire function
        iter.next();

        iter
    }
}

impl Iterator for SourceMapIter<'_> {
    type Item = SourceMapEntry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.source_map.len() {
            return None;
        }

        let (bytecode_offset, num_bytes) = decode_varint(&self.source_map[self.offset..]);
        self.offset += num_bytes;

        let (source_position, num_bytes) = decode_varint(&self.source_map[self.offset..]);
        self.offset += num_bytes;

        Some(SourceMapEntry { bytecode_offset, source_position })
    }
}
