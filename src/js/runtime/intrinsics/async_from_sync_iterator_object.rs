use crate::{
    extend_object,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        iterator::Iterator,
        object_value::ObjectValue,
        ordinary_object::object_create,
    },
    set_uninit,
};

// Async-from-Sync Iterator Objects (https://tc39.es/ecma262/#sec-async-from-sync-iterator-objects)
extend_object! {
    pub struct AsyncFromSyncIteratorObject {
        iterator: HeapPtr<ObjectValue>,
        next_method: Value,
    }
}

impl AsyncFromSyncIteratorObject {
    pub fn new(
        cx: Context,
        iterator: Iterator,
    ) -> AllocResult<Handle<AsyncFromSyncIteratorObject>> {
        let mut object = object_create::<AsyncFromSyncIteratorObject>(
            cx,
            HeapItemKind::AsyncFromSyncIteratorObject,
            Intrinsic::AsyncFromSyncIteratorPrototype,
        )?;

        set_uninit!(
            object.descriptor,
            cx.descriptors
                .get(HeapItemKind::AsyncFromSyncIteratorObject)
        );
        set_uninit!(object.iterator, *iterator.iterator);
        set_uninit!(object.next_method, *iterator.next_method);

        Ok(object.to_handle())
    }

    pub fn iterator(&self) -> Handle<ObjectValue> {
        self.iterator.to_handle()
    }

    pub fn next_method(&self, cx: Context) -> Handle<Value> {
        self.next_method.to_handle(cx)
    }
}

impl HeapItem for AsyncFromSyncIteratorObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        std::mem::size_of::<AsyncFromSyncIteratorObject>()
    }

    fn visit_pointers(mut async_from_sync_iterator: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        async_from_sync_iterator.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut async_from_sync_iterator.iterator);
        visitor.visit_value(&mut async_from_sync_iterator.next_method);
    }
}
