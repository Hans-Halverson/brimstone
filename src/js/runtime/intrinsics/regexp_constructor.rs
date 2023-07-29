use std::mem::size_of;

use crate::{
    extend_object,
    js::{
        parser::regexp::RegExpFlags,
        runtime::{
            builtin_function::BuiltinFunction,
            completion::EvalResult,
            function::get_argument,
            gc::{Handle, HeapObject, HeapVisitor},
            get,
            object_descriptor::ObjectKind,
            object_value::ObjectValue,
            ordinary_object::{object_create, object_create_from_constructor},
            property::Property,
            realm::Realm,
            string_value::StringValue,
            type_utilities::{is_regexp, same_value},
            Context, HeapPtr, Value,
        },
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 22.2 RegExp (Regular Expression) Objects
extend_object! {
    pub struct RegExpObject {
        // Flags of the regexp object
        flags: RegExpFlags,
        // The pattern component of the original regexp as a string. Escaped so that it can be
        // parsed into exactly the same pattern again.
        escaped_pattern_source: HeapPtr<StringValue>,
    }
}

impl RegExpObject {
    pub fn new(cx: &mut Context) -> Handle<RegExpObject> {
        let object =
            object_create::<RegExpObject>(cx, ObjectKind::RegExpObject, Intrinsic::RegExpPrototype);

        object.to_handle()
    }

    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<RegExpObject>> {
        let object = maybe!(object_create_from_constructor::<RegExpObject>(
            cx,
            constructor,
            ObjectKind::RegExpObject,
            Intrinsic::RegExpPrototype
        ));

        object.to_handle().into()
    }

    pub fn flags(&self) -> RegExpFlags {
        self.flags
    }

    pub fn escaped_pattern_source(&self) -> Handle<StringValue> {
        self.escaped_pattern_source.to_handle()
    }
}

pub struct RegExpConstructor;

impl RegExpConstructor {
    // 22.2.5 Properties of the RegExp Constructor
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            2,
            cx.names.regexp(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::RegExpPrototype).into(),
                false,
                false,
                false,
            ),
        );

        // 22.2.5.2 get RegExp [ @@species ]
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 22.2.4.1 RegExp
    fn construct(
        cx: &mut Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let pattern = get_argument(cx, arguments, 0);
        let flags = get_argument(cx, arguments, 1);

        let pattern_is_regexp = maybe!(is_regexp(cx, pattern));

        let new_target = match new_target {
            None => {
                let new_target = cx.current_execution_context().function();

                if pattern_is_regexp && flags.is_undefined() {
                    let pattern_constructor =
                        maybe!(get(cx, pattern.as_object(), cx.names.constructor()));

                    if same_value(new_target.into(), pattern_constructor) {
                        return pattern.into();
                    }
                }

                new_target
            }
            Some(new_target) => new_target,
        };

        unimplemented!()
    }

    // 22.2.5.2 get RegExp [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}

impl HeapObject for HeapPtr<RegExpObject> {
    fn byte_size(&self) -> usize {
        size_of::<RegExpObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
