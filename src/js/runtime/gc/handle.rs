use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};

use crate::runtime::{
    object_value::ObjectValue,
    string_value::StringValue,
    value::{BigIntValue, SymbolValue},
    Context, Value,
};

use super::{Heap, HeapInfo, HeapPtr, HeapVisitor, IsHeapObject};

/// Handles store a pointer-sized unit of data. This may be either a value or a heap pointer.
pub type HandleContents = usize;

pub trait ToHandleContents {
    type Impl;

    fn to_handle_contents(value: Self::Impl) -> HandleContents;
}

/// Handles hold a value or heap pointer behind a pointer. Handles are safe to store on the stack
/// during a GC, since the handle's pointer does not change but the address of the heap object
/// behind the pointer may be updated.
pub struct Handle<T> {
    ptr: NonNull<HandleContents>,
    phantom_data: PhantomData<T>,
}

impl<T: ToHandleContents> Handle<T> {
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

        // Increment handle count if tracking handles
        #[cfg(feature = "handle_stats")]
        {
            handle_context.num_handles += 1;
            handle_context.max_handles = handle_context.max_handles.max(handle_context.num_handles);
        }

        Handle {
            ptr: unsafe { NonNull::new_unchecked(handle.cast()) },
            phantom_data: PhantomData,
        }
    }

    #[inline]
    pub fn empty(cx: Context) -> Handle<T> {
        let handle_context = cx.heap.info().handle_context();
        Handle::new(handle_context, Value::to_handle_contents(Value::empty()))
    }

    #[inline]
    pub const fn dangling() -> Handle<T> {
        Handle { ptr: NonNull::dangling(), phantom_data: PhantomData }
    }

    #[inline]
    pub fn is_dangling(&self) -> bool {
        self.ptr == NonNull::dangling()
    }

    /// Replace the value stored behind this handle with a new value. Note that all copies of this
    /// handle will also be changed.
    #[inline]
    pub fn replace(&mut self, new_contents: T::Impl) {
        unsafe { self.ptr.as_ptr().write(T::to_handle_contents(new_contents)) }
    }

    pub fn replace_into<U: ToHandleContents>(self, new_contents: U::Impl) -> Handle<U> {
        let mut handle = self.cast::<U>();
        handle.replace(new_contents);
        handle
    }
}

impl<T> Handle<T> {
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

impl<T: ToHandleContents> Deref for Handle<T> {
    type Target = T::Impl;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.cast::<Self::Target>().as_ref() }
    }
}

impl<T: ToHandleContents> DerefMut for Handle<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.cast::<Self::Target>().as_mut() }
    }
}

/// Saved handle state that allows restoring to the state right before a handle scope was entered.
/// Must only be created on the stack.
#[must_use = "HandleScopes must be explicitly exited with a call to exit"]
pub struct HandleScope {
    heap_ptr: *mut Heap,
    next_ptr: *mut HandleContents,
    end_ptr: *mut HandleContents,
}

impl HandleScope {
    #[inline]
    pub fn new<F: FnOnce(Context) -> R, R: Escapable>(cx: Context, f: F) -> R {
        let handle_scope = Self::enter(cx);
        let result = f(cx);
        handle_scope.escape(cx, result)
    }

    #[inline]
    pub fn enter(mut cx: Context) -> HandleScope {
        let heap = &mut cx.heap;
        let handle_context = heap.info().handle_context();

        HandleScope {
            heap_ptr: heap as *mut Heap,
            next_ptr: handle_context.next_ptr,
            end_ptr: handle_context.end_ptr,
        }
    }

    /// Exit a handle scope and return an item escaped into the parent's handle scope.
    #[inline]
    pub fn escape<R: Escapable>(self, cx: Context, result: R) -> R {
        self.exit();
        result.escape(cx)
    }

    /// Exit a handle scope without returning an escaped item.
    #[inline]
    pub fn exit(self) {
        self.exit_non_consuming();
    }

    #[inline]
    fn exit_non_consuming(&self) {
        let heap = unsafe { &mut *self.heap_ptr };
        let handle_context = heap.info().handle_context();

        // The saved handle scope was in a previous block. Pop blocks until the current block
        // matches that of the saved handle scope.
        if self.end_ptr != handle_context.end_ptr {
            // If tracking handles then decrement the handle count for the first popped block. This
            // removes the handle range from the start of the block to the next pointer.
            #[cfg(feature = "handle_stats")]
            {
                let unallocated_in_block =
                    unsafe { handle_context.end_ptr.offset_from(handle_context.next_ptr) as usize };
                handle_context.num_handles -= HANDLE_BLOCK_SIZE - unallocated_in_block;
            }

            while self.end_ptr != handle_context.pop_block() {
                // All later blocks were fully allocated
                #[cfg(feature = "handle_stats")]
                {
                    handle_context.num_handles -= HANDLE_BLOCK_SIZE;
                }
            }

            // Decrement the handle count for newly deallocated handles in the new current block.
            // These handles are from the next pointer to the end of the block.
            #[cfg(feature = "handle_stats")]
            {
                handle_context.num_handles -=
                    unsafe { self.end_ptr.offset_from(self.next_ptr) } as usize;
            }
        } else {
            // If tracking handles then remove the handle range in this block that was deallocated.
            #[cfg(feature = "handle_stats")]
            {
                handle_context.num_handles -=
                    unsafe { handle_context.next_ptr.offset_from(self.next_ptr) } as usize;
            }
        }

        handle_context.next_ptr = self.next_ptr;
        handle_context.end_ptr = self.end_ptr;
    }
}

/// A guard which enters a handle scope and exits it when dropped. Does not escape any values.
pub struct HandleScopeGuard {
    handle_scope: HandleScope,
}

impl HandleScopeGuard {
    #[inline]
    pub fn new(cx: Context) -> HandleScopeGuard {
        HandleScopeGuard { handle_scope: HandleScope::enter(cx) }
    }
}

impl Drop for HandleScopeGuard {
    #[inline]
    fn drop(&mut self) {
        self.handle_scope.exit_non_consuming();
    }
}

/// A guard which enters a handle scope and exits it when dropped. Does not escape any values.
#[macro_export]
macro_rules! handle_scope_guard {
    ($cx:expr) => {
        let _guard = $crate::runtime::gc::HandleScopeGuard::new($cx);
    };
}

/// Enter a handle scope and execute the given statement. Returns and escapes the result of
/// executing the statement.
#[macro_export]
macro_rules! handle_scope {
    ($cx:expr, $body:stmt) => {
        $crate::runtime::gc::HandleScope::new($cx, |_| {
            let result = { $body };
            result
        })
    };
}

/// Number of handles contained in a single handle block. Default to 4KB handle blocks.
const HANDLE_BLOCK_SIZE: usize = 512;

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
    /// Pointer to within a handle block, pointing to address of the next handle to allocate
    next_ptr: *mut HandleContents,

    /// Pointer one beyond the end of the current handle scope block, marking the limit for this
    /// handle scope. Used to uniquely identify the current handle block.
    end_ptr: *mut HandleContents,

    /// Current block for the handle scope stack. Contains chain of other blocks in use.
    current_block: Pin<Box<HandleBlock>>,

    /// Chain of free blocks
    free_blocks: Option<Pin<Box<HandleBlock>>>,

    /// Total number of handles currently allocated
    #[cfg(feature = "handle_stats")]
    num_handles: usize,

    /// Max number of handles allocated at once observed so far
    #[cfg(feature = "handle_stats")]
    max_handles: usize,
}

#[cfg(feature = "handle_stats")]
#[derive(Debug)]
pub struct HandleStats {
    pub num_handles: usize,
    pub max_handles: usize,
}

impl HandleContext {
    pub fn init(&mut self) {
        let first_block = HandleBlock::new(None);

        let handle_context = HandleContext {
            next_ptr: first_block.start_ptr,
            end_ptr: first_block.end_ptr,
            current_block: first_block,
            free_blocks: None,
            #[cfg(feature = "handle_stats")]
            num_handles: 0,
            #[cfg(feature = "handle_stats")]
            max_handles: 0,
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
                let rest_free_blocks = free_blocks.prev_block.take();
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
        let old_prev_block = self.current_block.prev_block.take();

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

    /// Return the number of handles that are currently being used.
    ///
    /// Currently only used for debugging.
    #[allow(dead_code)]
    pub fn handle_count(&self) -> usize {
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

    /// Return the number of free handle blocks in the free list.
    ///
    /// Currently only used for debugging.
    #[allow(dead_code)]
    pub fn free_handle_block_count(&self) -> usize {
        let mut total = 0;

        let mut current_block = &self.free_blocks;
        while let Some(next_block) = current_block {
            current_block = &next_block.prev_block;
            total += 1;
        }

        total
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        // Only visit values that have been used (aka before the next pointer) in the current block
        let mut current_block = &self.current_block;
        Self::visit_roots_between_pointers(current_block.start_ptr, self.next_ptr, visitor);

        // Visit all values in earlier blocks
        while let Some(prev_block) = &current_block.prev_block {
            current_block = prev_block;
            Self::visit_roots_between_pointers(
                current_block.start_ptr,
                current_block.end_ptr,
                visitor,
            );
        }
    }

    fn visit_roots_between_pointers(
        start_ptr: *const HandleContents,
        end_ptr: *const HandleContents,
        visitor: &mut impl HeapVisitor,
    ) {
        unsafe {
            let mut current_ptr = start_ptr;
            while current_ptr != end_ptr {
                let value_ref = &mut *(current_ptr.cast_mut() as *mut Value);
                visitor.visit_value(value_ref);

                current_ptr = current_ptr.add(1)
            }
        }
    }

    #[cfg(feature = "handle_stats")]
    pub fn handle_stats(&self) -> HandleStats {
        HandleStats { num_handles: self.num_handles, max_handles: self.max_handles }
    }
}

impl Handle<Value> {
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
}

impl Value {
    #[inline]
    pub fn to_handle(self, cx: Context) -> Handle<Value> {
        let handle_context = cx.heap.info().handle_context();
        Handle::new(handle_context, Value::to_handle_contents(self))
    }
}

impl<T: IsHeapObject> HeapPtr<T> {
    #[inline]
    pub fn to_handle(self) -> Handle<T> {
        let handle_context = HeapInfo::from_heap_ptr(self).handle_context();
        Handle::new(handle_context, T::to_handle_contents(self))
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
    fn escape(&self, cx: Context) -> Self;
}

impl Escapable for () {
    #[inline]
    fn escape(&self, _: Context) -> Self {}
}

impl Escapable for u32 {
    #[inline]
    fn escape(&self, _: Context) -> Self {
        *self
    }
}

impl Escapable for Value {
    #[inline]
    fn escape(&self, _: Context) -> Self {
        *self
    }
}

impl<T> Escapable for HeapPtr<T> {
    #[inline]
    fn escape(&self, _: Context) -> Self {
        *self
    }
}

impl Escapable for Handle<Value> {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        (**self).to_handle(cx)
    }
}

impl<T: IsHeapObject> Escapable for Handle<T> {
    #[inline]
    fn escape(&self, _: Context) -> Self {
        (**self).to_handle()
    }
}

impl<T: Escapable> Escapable for Option<T> {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        self.as_ref().map(|some| some.escape(cx))
    }
}

impl<T: Escapable, E: Escapable> Escapable for Result<T, E> {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        match self {
            Ok(ok) => Ok(ok.escape(cx)),
            Err(err) => Err(err.escape(cx)),
        }
    }
}

impl<T: Escapable, U: Escapable> Escapable for (T, U) {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        (self.0.escape(cx), self.1.escape(cx))
    }
}
