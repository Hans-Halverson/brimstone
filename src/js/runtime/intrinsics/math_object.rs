use crate::{
    js::runtime::{
        completion::EvalResult,
        function::get_argument,
        gc::Gc,
        numeric_operations::number_exponentiate,
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::to_number,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 21.3 The Math Object
pub struct MathObject;

impl MathObject {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // 21.3.1 Value Properties of the Math Object
        object.intrinsic_frozen_property(&cx.names.e(), Value::number(std::f64::consts::E));
        object.intrinsic_frozen_property(&cx.names.ln10(), Value::number(std::f64::consts::LN_10));
        object.intrinsic_frozen_property(&cx.names.ln2(), Value::number(std::f64::consts::LN_2));
        object.intrinsic_frozen_property(
            &cx.names.log10e(),
            Value::number(std::f64::consts::LOG10_E),
        );
        object
            .intrinsic_frozen_property(&cx.names.log2e(), Value::number(std::f64::consts::LOG2_E));
        object.intrinsic_frozen_property(&cx.names.pi(), Value::number(std::f64::consts::PI));
        object.intrinsic_frozen_property(
            &cx.names.sqrt1_2(),
            Value::number(std::f64::consts::FRAC_1_SQRT_2),
        );
        object
            .intrinsic_frozen_property(&cx.names.sqrt2(), Value::number(std::f64::consts::SQRT_2));

        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let math_name_value = cx.names.math().as_string().into();
        object
            .set_property(&to_string_tag_key, Property::data(math_name_value, false, false, true));

        object.intrinsic_func(cx, &cx.names.pow(), Self::pow, 2, realm);

        cx.heap.alloc(object).into()
    }

    // 21.3.2.26 Math.pow
    fn pow(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let base = maybe!(to_number(cx, get_argument(arguments, 0)));
        let exponent = maybe!(to_number(cx, get_argument(arguments, 1)));

        number_exponentiate(base.as_number(), exponent.as_number()).into()
    }
}
