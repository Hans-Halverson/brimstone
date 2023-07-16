use crate::{
    extend_object,
    js::runtime::{
        collections::{BsIndexSet, BsIndexSetField},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::ValueCollectionKey,
        Context, EvalResult, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

// 24.2 Set Objects
extend_object! {
    pub struct SetObject {
        set_data: HeapPtr<ValueSet>,
    }
}

type ValueSet = BsIndexSet<ValueCollectionKey>;

impl SetObject {
    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<SetObject>> {
        // Allocate and place behind handle before allocating object
        let set_data = ValueSet::new(cx, ObjectKind::SetObjectValueSet, ValueSet::MIN_CAPACITY);

        let mut object = maybe!(object_create_from_constructor::<SetObject>(
            cx,
            constructor,
            ObjectKind::SetObject,
            Intrinsic::SetPrototype
        ));

        set_uninit!(object.set_data, set_data);

        object.to_handle().into()
    }

    pub fn set_data(&mut self) -> HeapPtr<ValueSet> {
        self.set_data
    }
}

impl Handle<SetObject> {
    pub fn clear_set_data(&mut self, cx: &mut Context) {
        let new_set = ValueSet::new(cx, ObjectKind::SetObjectValueSet, ValueSet::MIN_CAPACITY);
        self.set_data = new_set;
    }

    pub fn set_data_field(&self) -> SetObjectSetField {
        SetObjectSetField(*self)
    }
}

#[derive(Clone)]
pub struct SetObjectSetField(Handle<SetObject>);

impl BsIndexSetField<ValueCollectionKey> for SetObjectSetField {
    fn new(cx: &mut Context, capacity: usize) -> HeapPtr<ValueSet> {
        ValueSet::new(cx, ObjectKind::SetObjectValueSet, capacity)
    }

    fn get(&self) -> HeapPtr<ValueSet> {
        self.0.set_data
    }

    fn set(&mut self, set: HeapPtr<ValueSet>) {
        self.0.set_data = set;
    }
}