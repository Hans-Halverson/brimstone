use crate::{
    field_offset,
    js::{
        parser::ast::{AstPtr, TemplateLiteral},
        runtime::gc::HandleScope,
    },
    set_uninit,
};

use super::{
    collections::{BsHashMap, BsHashMapField},
    environment::global_environment::GlobalEnvironment,
    execution_context::ExecutionContext,
    gc::{Handle, HeapObject, HeapPtr, HeapVisitor},
    intrinsics::{
        global_object::set_default_global_bindings,
        intrinsics::{Intrinsic, Intrinsics},
    },
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    Context,
};

// 9.3 Realm Record
#[repr(C)]
pub struct Realm {
    descriptor: HeapPtr<ObjectDescriptor>,
    global_env: HeapPtr<GlobalEnvironment>,
    global_object: HeapPtr<ObjectValue>,
    template_map: HeapPtr<TemplateMap>,
    pub intrinsics: Intrinsics,
}

type TemplateMap = BsHashMap<AstPtr<TemplateLiteral>, HeapPtr<ObjectValue>>;

const INTRINSICS_BYTE_OFFSET: usize = field_offset!(Realm, intrinsics);

impl Realm {
    // 9.3.1 CreateRealm. Realm initializes intrinsics but leaves other properties uninitialized.
    // Must call `initialize` before using.
    pub fn new_uninit(cx: Context) -> Handle<Realm> {
        HandleScope::new(cx, |cx| {
            // Realm record must be created before setting up intrinsics, as realm must be referenced
            // during intrinsic creation.
            let size = Self::calculate_size_in_bytes();
            let mut realm = cx.alloc_uninit_with_size::<Realm>(size);

            set_uninit!(realm.descriptor, cx.base_descriptors.get(ObjectKind::Realm));
            set_uninit!(realm.global_env, HeapPtr::uninit());
            set_uninit!(realm.global_object, HeapPtr::uninit());
            set_uninit!(realm.template_map, HeapPtr::uninit());

            let realm = realm.to_handle();

            Intrinsics::initialize(cx, realm);

            realm
        })
    }

    #[inline]
    pub fn calculate_size_in_bytes() -> usize {
        INTRINSICS_BYTE_OFFSET + Intrinsics::calculate_size_in_bytes()
    }

    #[inline]
    pub fn global_object(&self) -> Handle<ObjectValue> {
        self.global_object.to_handle()
    }

    #[inline]
    pub fn global_env(&self) -> Handle<GlobalEnvironment> {
        self.global_env.to_handle()
    }

    pub fn global_this_value(&self) -> Handle<ObjectValue> {
        self.global_env.global_this_value()
    }

    pub fn get_intrinsic_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.intrinsics.get_ptr(intrinsic)
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.intrinsics.get(intrinsic)
    }

    pub fn get_template_object(
        &self,
        template_node: AstPtr<TemplateLiteral>,
    ) -> Option<Handle<ObjectValue>> {
        self.template_map
            .get(&template_node)
            .map(|template_object| template_object.to_handle())
    }
}

impl Handle<Realm> {
    fn template_map_field(&self) -> RealmTemplateMapField {
        RealmTemplateMapField(*self)
    }

    pub fn add_template_object(
        &mut self,
        cx: Context,
        template_node: AstPtr<TemplateLiteral>,
        template_object: Handle<ObjectValue>,
    ) {
        self.template_map_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(template_node, template_object.get_());
    }

    // 9.3.3 SetRealmGlobalObject
    // Initializes remaining properties of realm that were not initialized in `new_uninit`.
    pub fn initialize(
        &mut self,
        cx: Context,
        global_object: Option<Handle<ObjectValue>>,
        this_value: Option<Handle<ObjectValue>>,
    ) {
        let global_object = global_object.unwrap_or_else(|| ordinary_object_create(cx));

        let this_value = this_value.unwrap_or(global_object);

        self.global_object = global_object.get_();
        self.global_env = GlobalEnvironment::new(cx, global_object, this_value).get_();
        self.template_map = TemplateMap::new_initial(cx, ObjectKind::RealmTemplateMap);
    }
}

// 9.6 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: Context, expose_gc: bool) -> Handle<Realm> {
    HandleScope::new(cx, |mut cx| {
        let mut realm = Realm::new_uninit(cx);
        let exec_ctx = ExecutionContext::new(
            cx, /* function */ None, realm, /* script_or_module */ None,
            /* lexical_env */ None, /* variable_env */ None, /* private_env */ None,
            /* is_strict_mode */ false,
        );

        cx.push_execution_context(exec_ctx);

        realm.initialize(cx, None, None);
        set_default_global_bindings(cx, realm, expose_gc);

        realm
    })
}

pub struct RealmTemplateMapField(Handle<Realm>);

impl BsHashMapField<AstPtr<TemplateLiteral>, HeapPtr<ObjectValue>> for RealmTemplateMapField {
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<TemplateMap> {
        TemplateMap::new(cx, ObjectKind::RealmTemplateMap, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<TemplateMap> {
        self.0.template_map
    }

    fn set(&mut self, _: Context, map: HeapPtr<TemplateMap>) {
        self.0.template_map = map;
    }
}

impl HeapObject for HeapPtr<Realm> {
    fn byte_size(&self) -> usize {
        Realm::calculate_size_in_bytes()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.global_env);
        visitor.visit_pointer(&mut self.global_object);
        visitor.visit_pointer(&mut self.template_map);
        self.intrinsics.visit_pointers(visitor);
    }
}

impl RealmTemplateMapField {
    pub fn byte_size(map: &HeapPtr<TemplateMap>) -> usize {
        TemplateMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<TemplateMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (_, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(value);
        }
    }
}
