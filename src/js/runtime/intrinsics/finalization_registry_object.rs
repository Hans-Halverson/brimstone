use std::{mem::size_of, slice};

use crate::{
    extend_object, field_offset,
    runtime::{
        alloc_error::AllocResult,
        collections::InlineArray,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        type_utilities::same_value_non_numeric_non_allocating,
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// FinalizationRegistry Objects (https://tc39.es/ecma262/#sec-finalization-registry-objects)
extend_object! {
    pub struct FinalizationRegistryObject {
        // Cells in the finalization registry
        cells: HeapPtr<FinalizationRegistryCells>,
        // Function to be called when values are garbage collected
        cleanup_callback: HeapPtr<ObjectValue>,
        // Holds the address of the next finalization registry that has been visited during garbage
        // collection. Unused outside of garbage collection.
        next_finalization_registry: Option<HeapPtr<FinalizationRegistryObject>>,
    }
}

impl FinalizationRegistryObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        cleanup_callback: Handle<ObjectValue>,
    ) -> EvalResult<Handle<FinalizationRegistryObject>> {
        let cells = FinalizationRegistryCells::new(cx, FinalizationRegistryCells::MIN_CAPACITY)?
            .to_handle();
        let mut object = object_create_from_constructor::<FinalizationRegistryObject>(
            cx,
            constructor,
            HeapItemKind::FinalizationRegistryObject,
            Intrinsic::FinalizationRegistryPrototype,
        )?;

        set_uninit!(object.cells, *cells);
        set_uninit!(object.cleanup_callback, *cleanup_callback);

        Ok(object.to_handle())
    }

    pub fn cells(&self) -> HeapPtr<FinalizationRegistryCells> {
        self.cells
    }

    pub fn cleanup_callback(&self) -> HeapPtr<ObjectValue> {
        self.cleanup_callback
    }

    pub fn next_finalization_registry(&self) -> Option<HeapPtr<FinalizationRegistryObject>> {
        self.next_finalization_registry
    }

    pub fn set_next_finalization_registry(
        &mut self,
        next_finalization_registry: Option<HeapPtr<FinalizationRegistryObject>>,
    ) {
        self.next_finalization_registry = next_finalization_registry;
    }
}

// A single cell in the FinalizationRegistry.
#[derive(Clone)]
pub struct FinalizationRegistryCell {
    pub target: Value,
    pub held_value: Value,
    pub unregister_token: Option<Value>,
}

#[repr(C)]
pub struct FinalizationRegistryCells {
    descriptor: HeapPtr<HeapItemDescriptor>,
    // Number of cells currently inserted, excluding deleted cells
    num_occupied: usize,
    // Number of deleted cells
    num_deleted: usize,
    // Array of cells up to num_occupied + num_deleted. Deleted cells are set to None.
    cells: InlineArray<Option<FinalizationRegistryCell>>,
}

const CELLS_BYTE_OFFSET: usize = field_offset!(FinalizationRegistryCells, cells);

impl FinalizationRegistryCells {
    const MIN_CAPACITY: usize = 4;

    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<FinalizationRegistryCells>> {
        let size = Self::calculate_size_in_bytes(capacity);
        let mut cells = cx.alloc_uninit_with_size::<FinalizationRegistryCells>(size)?;

        set_uninit!(
            cells.descriptor,
            cx.base_descriptors
                .get(HeapItemKind::FinalizationRegistryCells)
        );
        set_uninit!(cells.num_occupied, 0);
        set_uninit!(cells.num_deleted, 0);

        // Leave cells array uninitialized
        cells.cells.init_with_uninit(capacity);

        // Leave entries uninitialized

        Ok(cells)
    }

    #[inline]
    fn calculate_size_in_bytes(capacity: usize) -> usize {
        CELLS_BYTE_OFFSET
            + InlineArray::<FinalizationRegistryCell>::calculate_size_in_bytes(capacity)
    }

    #[inline]
    fn capacity(&self) -> usize {
        self.cells.len()
    }

    /// Total number of cells that have been inserted, including those that have been deleted.
    #[inline]
    pub fn num_cells_used(&self) -> usize {
        self.num_occupied + self.num_deleted
    }

    /// Prepare array for insertion of a single cell. This will grow the array and update container to
    /// point to new array if there is no room to insert another cell in the array.
    pub fn maybe_grow_for_insertion(
        cx: Context,
        mut registry: Handle<FinalizationRegistryObject>,
    ) -> AllocResult<HeapPtr<FinalizationRegistryCells>> {
        let old_cells = registry.cells;
        let capacity = old_cells.capacity();
        let num_cells_used = old_cells.num_cells_used();

        // Check if we already have enough room for an insertion
        if num_cells_used < capacity {
            return Ok(old_cells);
        }

        let new_num_occupied = old_cells.num_occupied + 1;

        // Find next power of two that is at least two times the number of occupied cells
        let mut new_capacity = usize::next_power_of_two(new_num_occupied);
        if new_num_occupied > new_capacity / 2 {
            new_capacity *= 2;
        }

        // Save old cells pointer behind handle across allcation
        let old_cells = old_cells.to_handle();
        let mut new_cells = FinalizationRegistryCells::new(cx, new_capacity)?;
        let old_cells = *old_cells;

        // Copy over occupied cells to new array
        let mut new_cells_index = 0;
        for i in 0..num_cells_used {
            if let Some(cell) = old_cells.cells.get_unchecked(i) {
                new_cells
                    .cells
                    .set_unchecked(new_cells_index, Some(cell.clone()));
                new_cells_index += 1;
            }
        }

        registry.cells = new_cells;

        Ok(new_cells)
    }

    pub fn insert_without_growing(&mut self, cell: FinalizationRegistryCell) {
        self.cells.set_unchecked(self.num_cells_used(), Some(cell));
        self.num_occupied += 1;
    }

    pub fn remove(&mut self, token: Value) -> bool {
        let mut has_removed = false;

        for i in 0..self.num_cells_used() {
            // Delete all cells whose unregister token matches the given token
            if let Some(FinalizationRegistryCell {
                unregister_token: Some(unregister_token), ..
            }) = self.cells.get_unchecked(i)
            {
                if same_value_non_numeric_non_allocating(*unregister_token, token) {
                    self.cells.set_unchecked(i, None);
                    self.num_occupied -= 1;
                    self.num_deleted += 1;
                    has_removed = true;
                }
            }
        }

        has_removed
    }

    pub fn remove_cell(&mut self, cell_ref: &mut Option<FinalizationRegistryCell>) {
        *cell_ref = None;
        self.num_occupied -= 1;
        self.num_deleted += 1;
    }

    pub fn iter_mut_gc_unsafe(&mut self) -> slice::IterMut<'_, Option<FinalizationRegistryCell>> {
        let num_cells_used = self.num_cells_used();
        let used_slice = &mut self.cells.as_mut_slice()[0..num_cells_used];
        used_slice.iter_mut()
    }
}

impl HeapItem for HeapPtr<FinalizationRegistryObject> {
    fn byte_size(&self) -> usize {
        size_of::<FinalizationRegistryObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.cells);
        visitor.visit_pointer(&mut self.cleanup_callback);

        // Intentionally do not visit next_finalization_registry
    }
}

impl HeapItem for HeapPtr<FinalizationRegistryCells> {
    fn byte_size(&self) -> usize {
        FinalizationRegistryCells::calculate_size_in_bytes(self.capacity())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        for i in 0..self.num_cells_used() {
            if let Some(cell) = self.cells.get_unchecked_mut(i) {
                visitor.visit_value(&mut cell.held_value);

                visitor.visit_weak_value(&mut cell.target);

                if let Some(unregister_token) = cell.unregister_token.as_mut() {
                    visitor.visit_weak_value(unregister_token);
                }
            }
        }
    }
}
