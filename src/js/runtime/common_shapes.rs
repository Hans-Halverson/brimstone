use crate::runtime::{
    Context, Handle, HeapItemKind, HeapPtr, PropertyFlags, PropertyKey, Realm,
    alloc_error::AllocResult, gc::HeapVisitor, intrinsics::intrinsics::Intrinsic,
    property::DEFAULT_DATA_PROPERTY_FLAGS, shape::Shape, shape_registry::ShapeRegistry,
};

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum CommonShape {
    /// A closure object (not a constructor): { length, name }.
    Closure,
    /// A closure object for a constructor function: { length, name, prototype }.
    ConstructorClosure,
    /// A closure object for an async function: { length, name }.
    AsyncClosure,
    /// A closure object for a generator function: { length, name, prototype }.
    GeneratorClosure,
    /// A closure object for an async generator function: { length, name, prototype }.
    AsyncGeneratorClosure,
    /// The `prototype` property of a constructor: { constructor }.
    ConstructorPrototype,
    /// A complete data property descriptor: { value, writable, enumerable, configurable }.
    DataPropertyDescriptor,
    /// A complete accessor property descriptor: { get, set, enumerable, configurable }.
    AccessorPropertyDescriptor,
    /// A string object: { length }.
    StringObject,
    /// A RegExp object: { lastIndex }.
    RegExp,
    /// A RegExp match result array, with named properties: { index, input, groups }.
    RegExpMatch,
    /// An iterator result object: { value, done }.
    IteratorResult,
    /// A native error object: { message }.
    EvalError,
    RangeError,
    ReferenceError,
    SyntaxError,
    TypeError,
    URIError,
    AggregateError,
    /// An unmapped arguments object: { length, @@iterator, callee }.
    UnmappedArguments,
    /// A mapped arguments object: { length, @@iterator, callee }.
    MappedArguments,
}

impl CommonShape {
    const COUNT: u8 = CommonShape::MappedArguments as u8 + 1;
}

pub struct CommonShapes {
    /// Common shapes indexed by CommonShape. Lazily created during realm initialization, but
    /// guaranteed to be initialized after realm initialization is complete.
    shapes: [Option<HeapPtr<Shape>>; CommonShape::COUNT as usize],
}

impl CommonShapes {
    pub fn new_uninit() -> Self {
        Self { shapes: [None; CommonShape::COUNT as usize] }
    }

    /// Initialize all common shapes. Some shapes
    pub fn initialize(cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
        for i in 0..CommonShape::COUNT {
            // Some shapes are already initialized during realm initialization
            if realm.common_shapes.shapes[i as usize].is_some() {
                continue;
            }

            let common_shape = unsafe { std::mem::transmute::<u8, CommonShape>(i) };
            Self::get(cx, realm, common_shape)?;
        }

        Ok(())
    }

    /// Return a common shape, building and caching it on first request.
    pub fn get(
        cx: Context,
        mut realm: Handle<Realm>,
        common_shape: CommonShape,
    ) -> AllocResult<Handle<Shape>> {
        if let Some(shape) = realm.common_shapes.shapes[common_shape as usize] {
            return Ok(shape.to_handle());
        }

        let shape = Self::build(cx, realm, common_shape)?;
        realm.common_shapes.shapes[common_shape as usize] = Some(*shape);

        Ok(shape)
    }

    fn build(
        cx: Context,
        realm: Handle<Realm>,
        common_shape: CommonShape,
    ) -> AllocResult<Handle<Shape>> {
        // Common attributes
        let data = DEFAULT_DATA_PROPERTY_FLAGS;
        let non_enum = PropertyFlags::empty().writable().configurable();
        let writable = PropertyFlags::empty().writable();
        let config = PropertyFlags::empty().configurable();
        let accessor_none = PropertyFlags::empty().accessor();
        let none = PropertyFlags::empty();

        let (kind, intrinsic_proto, properties): (_, _, &[(Handle<PropertyKey>, PropertyFlags)]) =
            match common_shape {
                CommonShape::Closure => (
                    HeapItemKind::ClosureObject,
                    Intrinsic::FunctionPrototype,
                    &[(cx.names.length(), config), (cx.names.name(), config)],
                ),
                CommonShape::ConstructorClosure => (
                    HeapItemKind::ClosureObject,
                    Intrinsic::FunctionPrototype,
                    &[
                        (cx.names.length(), config),
                        (cx.names.name(), config),
                        (cx.names.prototype(), writable),
                    ],
                ),
                CommonShape::AsyncClosure => (
                    HeapItemKind::ClosureObject,
                    Intrinsic::AsyncFunctionPrototype,
                    &[(cx.names.length(), config), (cx.names.name(), config)],
                ),
                CommonShape::GeneratorClosure => (
                    HeapItemKind::ClosureObject,
                    Intrinsic::GeneratorFunctionPrototype,
                    &[
                        (cx.names.length(), config),
                        (cx.names.name(), config),
                        (cx.names.prototype(), writable),
                    ],
                ),
                CommonShape::AsyncGeneratorClosure => (
                    HeapItemKind::ClosureObject,
                    Intrinsic::AsyncGeneratorFunctionPrototype,
                    &[
                        (cx.names.length(), config),
                        (cx.names.name(), config),
                        (cx.names.prototype(), writable),
                    ],
                ),
                CommonShape::ConstructorPrototype => (
                    HeapItemKind::OrdinaryObject,
                    Intrinsic::ObjectPrototype,
                    &[(cx.names.constructor(), non_enum)],
                ),
                CommonShape::DataPropertyDescriptor => (
                    HeapItemKind::OrdinaryObject,
                    Intrinsic::ObjectPrototype,
                    &[
                        (cx.names.value(), data),
                        (cx.names.writable(), data),
                        (cx.names.enumerable(), data),
                        (cx.names.configurable(), data),
                    ],
                ),
                CommonShape::AccessorPropertyDescriptor => (
                    HeapItemKind::OrdinaryObject,
                    Intrinsic::ObjectPrototype,
                    &[
                        (cx.names.get(), data),
                        (cx.names.set_(), data),
                        (cx.names.enumerable(), data),
                        (cx.names.configurable(), data),
                    ],
                ),
                CommonShape::StringObject => (
                    HeapItemKind::StringObject,
                    Intrinsic::StringPrototype,
                    &[(cx.names.length(), none)],
                ),
                CommonShape::RegExp => (
                    HeapItemKind::RegExpObject,
                    Intrinsic::RegExpPrototype,
                    &[(cx.names.last_index(), writable)],
                ),
                CommonShape::RegExpMatch => (
                    HeapItemKind::ArrayObject,
                    Intrinsic::ArrayPrototype,
                    &[
                        (cx.names.index(), data),
                        (cx.names.input(), data),
                        (cx.names.groups(), data),
                    ],
                ),
                CommonShape::IteratorResult => (
                    HeapItemKind::OrdinaryObject,
                    Intrinsic::ObjectPrototype,
                    &[(cx.names.value(), data), (cx.names.done(), data)],
                ),
                CommonShape::EvalError => (
                    HeapItemKind::ErrorObject,
                    Intrinsic::EvalErrorPrototype,
                    &[(cx.names.message(), non_enum)],
                ),
                CommonShape::RangeError => (
                    HeapItemKind::ErrorObject,
                    Intrinsic::RangeErrorPrototype,
                    &[(cx.names.message(), non_enum)],
                ),
                CommonShape::ReferenceError => (
                    HeapItemKind::ErrorObject,
                    Intrinsic::ReferenceErrorPrototype,
                    &[(cx.names.message(), non_enum)],
                ),
                CommonShape::SyntaxError => (
                    HeapItemKind::ErrorObject,
                    Intrinsic::SyntaxErrorPrototype,
                    &[(cx.names.message(), non_enum)],
                ),
                CommonShape::TypeError => (
                    HeapItemKind::ErrorObject,
                    Intrinsic::TypeErrorPrototype,
                    &[(cx.names.message(), non_enum)],
                ),
                CommonShape::URIError => (
                    HeapItemKind::ErrorObject,
                    Intrinsic::URIErrorPrototype,
                    &[(cx.names.message(), non_enum)],
                ),
                CommonShape::AggregateError => (
                    HeapItemKind::ErrorObject,
                    Intrinsic::AggregateErrorPrototype,
                    &[(cx.names.errors(), non_enum)],
                ),
                CommonShape::UnmappedArguments => (
                    HeapItemKind::UnmappedArgumentsObject,
                    Intrinsic::ObjectPrototype,
                    &[
                        (cx.names.length(), non_enum),
                        (cx.symbols.iterator(), non_enum),
                        (cx.names.callee(), accessor_none),
                    ],
                ),
                CommonShape::MappedArguments => (
                    HeapItemKind::MappedArgumentsObject,
                    Intrinsic::ObjectPrototype,
                    &[
                        (cx.names.length(), non_enum),
                        (cx.symbols.iterator(), non_enum),
                        (cx.names.callee(), non_enum),
                    ],
                ),
            };

        let proto = realm.get_intrinsic(intrinsic_proto);
        let inline_properties_capacity = properties.len() as u8;

        let mut shape = ShapeRegistry::get_root_object_shape(
            cx,
            kind,
            Some(proto),
            inline_properties_capacity,
        )?;

        for (key, attributes) in properties {
            let (next_shape, _) = shape.define_own_property(cx, *key, *attributes)?;
            shape = next_shape.to_handle();
        }

        debug_assert!(shape.num_properties() as usize == properties.len());

        Ok(shape)
    }

    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        for shape in self.shapes.iter_mut() {
            visitor.visit_pointer_opt(shape);
        }
    }
}

impl Realm {
    #[inline]
    pub fn is_common_shape(&self, shape: HeapPtr<Shape>, common_shape: CommonShape) -> bool {
        self.common_shapes.shapes[common_shape as usize].is_some_and(|s| s.ptr_eq(&shape))
    }
}
