use std::{cell::RefCell, rc::Rc};

use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::AbstractResult,
        error::type_error_,
        gc::Gc,
        intrinsics::{function_prototype::FunctionPrototype, object_prototype::ObjectPrototype},
        object_value::{Object, ObjectValue},
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        value::Value,
        Context,
    },
    must_,
};

#[repr(u8)]
pub enum Intrinsic {
    ObjectPrototype = 0,
    FunctionPrototype,
    ThrowTypeError,
    Last,
}

impl Intrinsic {
    const fn num_intrinsics() -> usize {
        Intrinsic::Last as usize
    }
}

pub struct Intrinsics {
    intrinsics: Vec<Gc<ObjectValue>>,
}

impl Intrinsics {
    pub fn new_uninit() -> Intrinsics {
        Intrinsics {
            intrinsics: Vec::new(),
        }
    }

    // 8.2.2 CreateIntrinsics
    pub fn initialize(&mut self, cx: &mut Context, realm: Rc<RefCell<Realm>>) {
        let intrinsics = &mut self.intrinsics;
        intrinsics.reserve_exact(Intrinsic::num_intrinsics());

        macro_rules! register_existing_intrinsic {
            ($intrinsic_name:ident, $expr:expr) => {
                intrinsics[Intrinsic::$intrinsic_name as usize] = ($expr);
            };
        }

        macro_rules! register_intrinsic {
            ($intrinsic_name:ident, $struct_name:ident) => {
                register_existing_intrinsic!($intrinsic_name, $struct_name::new(cx, realm.clone()))
            };
        }

        register_intrinsic!(ObjectPrototype, ObjectPrototype);
        register_intrinsic!(FunctionPrototype, FunctionPrototype);

        let throw_type_error_intrinsic = create_throw_type_error_intrinsic(cx, realm.clone());
        register_existing_intrinsic!(ThrowTypeError, throw_type_error_intrinsic.into());

        add_restricted_function_properties(cx, self.get(Intrinsic::FunctionPrototype), realm)
    }

    pub fn get(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.intrinsics[intrinsic as usize]
    }
}

fn throw_type_error(
    cx: &mut Context,
    _: Value,
    _: Vec<Value>,
    _: Option<Gc<ObjectValue>>,
) -> AbstractResult<Value> {
    type_error_(cx, "'caller', 'callee', and 'arguments' properties may not be accessed on strict mode functions or the arguments objects for calls to them")
}

// 9.2.4.1 %ThrowTypeError%
fn create_throw_type_error_intrinsic(
    cx: &mut Context,
    realm: Rc<RefCell<Realm>>,
) -> Gc<BuiltinFunction> {
    let mut throw_type_error_func =
        BuiltinFunction::create(cx, throw_type_error, Some(realm), None);
    throw_type_error_func.prevent_extensions();

    let length_desc = PropertyDescriptor::data(0.into(), false, false, false);
    must_!(define_property_or_throw(
        cx,
        throw_type_error_func.into(),
        "length",
        length_desc
    ));

    throw_type_error_func
}

// 9.2.4 AddRestrictedFunctionProperties
fn add_restricted_function_properties(
    cx: &mut Context,
    func: Gc<ObjectValue>,
    realm: Rc<RefCell<Realm>>,
) {
    let thrower_func = realm.borrow().get_intrinsic(Intrinsic::ThrowTypeError);

    let caller_desc =
        PropertyDescriptor::accessor(Some(thrower_func), Some(thrower_func), false, true);
    must_!(define_property_or_throw(cx, func, "caller", caller_desc));

    let arguments_desc =
        PropertyDescriptor::accessor(Some(thrower_func), Some(thrower_func), false, true);
    must_!(define_property_or_throw(
        cx,
        func,
        "arguments",
        arguments_desc
    ));
}
