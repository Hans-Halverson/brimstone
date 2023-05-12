use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};

use crate::js::runtime::{
    object_value::ObjectValue,
    string_value::StringValue,
    value::{AccessorValue, BigIntValue, SymbolValue},
    Context, PropertyKey, Value,
};

use super::{HeapInfo, HeapPtr, IsHeapObject};

/// Handles store a pointer-sized unit of data. This may be either a value or a heap pointer.
pub type HandleContents = usize;

/// Handles hold a value or heap pointer behind a pointer. Handles are safe to store on the stack
/// during a GC, since the handle's pointer does not change but the address of the heap object
/// behind the pointer may be updated.
pub struct Handle<T> {
    ptr: NonNull<HandleContents>,
    phantom_data: PhantomData<T>,
}

impl<T> Handle<T> {
    #[inline]
    pub fn new(handle_context: &mut HandleContext, contents: HandleContents) -> Handle<T> {
        // Handle scope block is full, so push a new handle scope block onto stack
        if handle_context.next_ptr == handle_context.end_ptr {
            handle_context.push_block();
        }

        // Write pointer into handle's address
        let handle = handle_context.next_ptr;
        unsafe { handle.write(contents) };

        handle_context.next_ptr = unsafe { handle.add(1) };

        Handle {
            ptr: unsafe { NonNull::new_unchecked(handle.cast()) },
            phantom_data: PhantomData,
        }
    }

    #[inline]
    pub fn empty(cx: &mut Context) -> Handle<T> {
        let handle_context = cx.heap.info().handle_context();
        Handle::new(handle_context, Value::empty().to_handle_contents())
    }

    #[inline]
    pub const fn dangling() -> Handle<T> {
        Handle { ptr: NonNull::dangling(), phantom_data: PhantomData }
    }

    #[inline]
    pub fn cast<U>(&self) -> Handle<U> {
        Handle { ptr: self.ptr, phantom_data: PhantomData }
    }
}

impl<T> Clone for Handle<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Handle<T> {}

/// A saved next_ptr, end_ptr pair that restores the handle stack to the saved position when dropped.
/// Must only be created on the stack.
pub struct HandleScope;

/// Saved handle state that allows restoring to the state right before a handle scope was entered.
struct SavedHandleState {
    next_ptr: *mut HandleContents,
    end_ptr: *mut HandleContents,
}

impl HandleScope {
    #[inline]
    pub fn enter<F: FnMut(&mut Context) -> R, R: Escapable>(cx: &mut Context, mut f: F) -> R {
        let saved_state = Self::save_handle_state(cx);
        let result = f(cx);
        Self::restore_handle_state(cx, saved_state);

        result.escape(cx)
    }

    #[inline]
    fn save_handle_state(cx: &mut Context) -> SavedHandleState {
        let handle_context = cx.heap.info().handle_context();
        SavedHandleState {
            next_ptr: handle_context.next_ptr,
            end_ptr: handle_context.end_ptr,
        }
    }

    #[inline]
    fn restore_handle_state(cx: &mut Context, saved_state: SavedHandleState) {
        let handle_context = cx.heap.info().handle_context();

        // The saved handle scope was in a previous block. Pop blocks until the current block
        // matches that of the saved handle scope.
        if saved_state.end_ptr != handle_context.end_ptr {
            while saved_state.end_ptr != handle_context.pop_block() {}
        }

        handle_context.next_ptr = saved_state.next_ptr;
        handle_context.end_ptr = saved_state.end_ptr;
    }
}

/// Number of handles contained in a single handle block
const HANDLE_BLOCK_SIZE: usize = 128;

pub struct HandleBlock {
    ptrs: [HandleContents; HANDLE_BLOCK_SIZE],
    // Pointer to the start of the handles array
    start_ptr: *mut HandleContents,
    // Pointer to the end of the handles array. Used to uniquely identify this block.
    end_ptr: *mut HandleContents,
    prev_block: Option<Pin<Box<HandleBlock>>>,
}

impl HandleBlock {
    fn new(prev_block: Option<Pin<Box<HandleBlock>>>) -> Pin<Box<HandleBlock>> {
        // Block must first be allocated on heap before start and end ptrs can be calculated.
        let mut block = Pin::new(Box::new(HandleBlock {
            ptrs: [0; HANDLE_BLOCK_SIZE],
            start_ptr: std::ptr::null_mut(),
            end_ptr: std::ptr::null_mut(),
            prev_block,
        }));

        let range = block.ptrs.as_mut_ptr_range();
        block.start_ptr = range.start;
        block.end_ptr = range.end;

        block
    }
}

pub struct HandleContext {
    // Pointer to within a handle block, pointing to address of the next handle to allocate
    next_ptr: *mut HandleContents,

    // Pointer one beyond the end of the current handle scope block, marking the limit for this
    // handle scope. Used to uniquely identify the current handle block.
    end_ptr: *mut HandleContents,

    // Current block for the handle scope stack. Contains chain of other blocks in use.
    current_block: Pin<Box<HandleBlock>>,

    // Chain of free blocks
    free_blocks: Option<Pin<Box<HandleBlock>>>,
}

impl HandleContext {
    pub fn init(&mut self) {
        let first_block = HandleBlock::new(None);

        let handle_context = HandleContext {
            next_ptr: first_block.start_ptr,
            end_ptr: first_block.end_ptr,
            current_block: first_block,
            free_blocks: None,
        };

        // Initial value was uninitialized, so replace without dropping uninitialized value
        std::mem::forget(std::mem::replace(self, handle_context));
    }

    fn push_block(&mut self) {
        match &mut self.free_blocks {
            None => {
                // Allocate a new block and push it as the current block
                let new_block = HandleBlock::new(None);
                let old_current_block = std::mem::replace(&mut self.current_block, new_block);
                self.current_block.prev_block = Some(old_current_block);
            }
            Some(free_blocks) => {
                // Pull the top free block off of the free list
                let rest_free_blocks = std::mem::replace(&mut free_blocks.prev_block, None);
                let free_block = std::mem::replace(&mut self.free_blocks, rest_free_blocks);

                // Push free block as the current block
                let old_current_block =
                    std::mem::replace(&mut self.current_block, free_block.unwrap());
                self.current_block.prev_block = Some(old_current_block);
            }
        }

        self.next_ptr = self.current_block.start_ptr;
        self.end_ptr = self.current_block.end_ptr;
    }

    fn pop_block(&mut self) -> *mut HandleContents {
        // Current block is replaced by its previous block
        let old_prev_block = std::mem::replace(&mut self.current_block.prev_block, None);

        let new_current_block = old_prev_block.unwrap();
        let new_end_ptr = new_current_block.end_ptr;
        let old_current_block = std::mem::replace(&mut self.current_block, new_current_block);

        // Current block is moved to start of free list
        let old_free_blocks = std::mem::replace(&mut self.free_blocks, Some(old_current_block));
        if let Some(new_first_free_block) = &mut self.free_blocks {
            new_first_free_block.prev_block = old_free_blocks;
        }

        // Return the end pointer for the new current block, uniquely identifying the new current block
        new_end_ptr
    }

    pub fn num_handles(&self) -> usize {
        // Number of handles used in the current block
        let mut total =
            unsafe { HANDLE_BLOCK_SIZE - (self.end_ptr.offset_from(self.next_ptr) as usize) };

        // Add handles used in previous handle blocks
        let mut current_block = &self.current_block;
        while let Some(next_block) = &current_block.prev_block {
            current_block = next_block;
            total += HANDLE_BLOCK_SIZE;
        }

        total
    }
}

impl Handle<Value> {
    /// Get the value stored behind the handle.
    #[inline]
    pub fn get(&self) -> Value {
        unsafe { self.ptr.as_ptr().cast::<Value>().read() }
    }

    /// Replace the value stored behind this handle with a new value. Note that all copies of this
    /// handle will also be changed.
    #[inline]
    pub fn replace(&mut self, value: Value) {
        unsafe { self.ptr.as_ptr().write(value.to_handle_contents()) }
    }

    #[inline]
    pub fn from_fixed_non_heap_ptr(value_ref: &Value) -> Handle<Value> {
        let ptr = unsafe { NonNull::new_unchecked(value_ref as *const Value as *mut Value) };
        Handle { ptr: ptr.cast(), phantom_data: PhantomData }
    }

    #[inline]
    pub fn as_object(&self) -> Handle<ObjectValue> {
        self.cast()
    }

    #[inline]
    pub fn as_string(&self) -> Handle<StringValue> {
        self.cast()
    }

    #[inline]
    pub fn as_symbol(&self) -> Handle<SymbolValue> {
        self.cast()
    }

    #[inline]
    pub fn as_bigint(&self) -> Handle<BigIntValue> {
        self.cast()
    }

    #[inline]
    pub fn as_accessor(&self) -> Handle<AccessorValue> {
        self.cast()
    }
}

impl<T: IsHeapObject> Handle<T> {
    /// Get the heap pointer stored behind the handle.
    #[inline]
    pub fn get_(&self) -> HeapPtr<T> {
        unsafe { self.ptr.as_ptr().cast::<HeapPtr<T>>().read() }
    }
}

impl Value {
    #[inline]
    pub fn to_handle(&self, cx: &mut Context) -> Handle<Value> {
        let handle_context = cx.heap.info().handle_context();
        Handle::new(handle_context, self.to_handle_contents())
    }
}

impl<T> HeapPtr<T> {
    #[inline]
    pub fn to_handle(&self) -> Handle<T> {
        let handle_context = HeapInfo::from_heap_ptr(*self).handle_context();
        Handle::new(handle_context, self.to_handle_contents())
    }
}

impl Deref for Handle<Value> {
    type Target = Value;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.cast::<Value>().as_ref() }
    }
}

impl DerefMut for Handle<Value> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.cast::<Value>().as_mut() }
    }
}

impl<T: IsHeapObject> Deref for Handle<T> {
    type Target = HeapPtr<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.cast::<HeapPtr<T>>().as_ref() }
    }
}

impl<T: IsHeapObject> DerefMut for Handle<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.cast::<HeapPtr<T>>().as_mut() }
    }
}

impl Deref for Handle<PropertyKey> {
    type Target = PropertyKey;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.cast::<PropertyKey>().as_ref() }
    }
}

impl DerefMut for Handle<PropertyKey> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.cast::<PropertyKey>().as_mut() }
    }
}

impl<T: IsHeapObject> From<Handle<T>> for Handle<Value> {
    #[inline]
    fn from(value: Handle<T>) -> Self {
        value.cast()
    }
}

/// Trait for items that can escape (be returned from) a handle scope. The item must be copied into
/// the parent handle scope.
pub trait Escapable {
    /// Copy this item into the current handle scope. Called from the parent's handle scope so that
    /// this item can escape the destroyed child handle scope.
    ///
    /// This is called after the handle scope containing the escaped item has been destroyed. This
    /// means that allocating a handle may overwrite the handles in this item. If multiple handles
    /// must be moved to the parent scope then be sure to copy out all the values before allocating
    /// any new handles, to avoid overwriting the old handles.
    fn escape(&self, cx: &mut Context) -> Self;
}

impl Escapable for () {
    #[inline]
    fn escape(&self, _: &mut Context) -> Self {
        ()
    }
}

impl Escapable for Handle<Value> {
    #[inline]
    fn escape(&self, cx: &mut Context) -> Self {
        self.get().to_handle(cx)
    }
}

impl<T: IsHeapObject> Escapable for Handle<T> {
    #[inline]
    fn escape(&self, _: &mut Context) -> Self {
        self.get_().to_handle()
    }
}

impl<T: Escapable> Escapable for Option<T> {
    #[inline]
    fn escape(&self, cx: &mut Context) -> Self {
        self.as_ref().map(|some| some.escape(cx))
    }
}
