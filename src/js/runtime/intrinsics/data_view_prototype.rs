use crate::{
    js::runtime::{
        error::type_error_,
        gc::Gc,
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        realm::Realm,
        Context, EvalResult, PropertyKey, Value,
    },
    maybe,
};

use super::{data_view_constructor::DataViewObject, intrinsics::Intrinsic};

pub struct DataViewPrototype;

impl DataViewPrototype {
    // 25.3.4 Properties of the DataView Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once DataViewConstructor has been created
        object.intrinsic_getter(cx, &cx.names.buffer(), Self::get_buffer, realm);
        object.intrinsic_getter(cx, &cx.names.byte_length(), Self::get_byte_length, realm);
        object.intrinsic_getter(cx, &cx.names.byte_offset(), Self::get_byte_offset, realm);

        // 25.3.4.25 DataView.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.set_property(
            &to_string_tag_key,
            Property::data(cx.names.data_view().as_string().into(), false, false, true),
        );

        cx.heap.alloc(object).into()
    }

    // 25.3.4.1 get DataView.prototype.buffer
    fn get_buffer(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let data_view = maybe!(require_is_data_view(cx, this_value));
        data_view.viewed_array_buffer().into()
    }

    // 25.3.4.2 get DataView.prototype.byteLength
    fn get_byte_length(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let data_view = maybe!(require_is_data_view(cx, this_value));

        if data_view.viewed_array_buffer().is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        Value::from(data_view.byte_length()).into()
    }

    // 25.3.4.3 get DataView.prototype.byteOffset
    fn get_byte_offset(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let data_view = maybe!(require_is_data_view(cx, this_value));

        if data_view.viewed_array_buffer().is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        Value::from(data_view.byte_offset()).into()
    }
}

#[inline]
fn require_is_data_view(cx: &mut Context, value: Value) -> EvalResult<Gc<DataViewObject>> {
    if !value.is_object() {
        return type_error_(cx, "expected data view");
    }

    let object = value.as_object();
    if !object.is_data_view() {
        return type_error_(cx, "expected data view");
    }

    object.cast::<DataViewObject>().into()
}
