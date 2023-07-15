use crate::{
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
    gc::{Handle, HeapPtr, IsHeapObject},
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
    intrinsics: Intrinsics,
}

type TemplateMap = BsHashMap<AstPtr<TemplateLiteral>, HeapPtr<ObjectValue>>;

impl IsHeapObject for Realm {}

impl Realm {
    // 9.3.1 CreateRealm. Realm initializes intrinsics but leaves other properties uninitialized.
    // Must call `initialize` before using.
    pub fn new_uninit(cx: &mut Context) -> Handle<Realm> {
        HandleScope::new(cx, |cx| {
            // Realm record must be created before setting up intrinsics, as realm must be referenced
            // during intrinsic creation.
            let mut realm = cx.heap.alloc_uninit::<Realm>();

            set_uninit!(realm.descriptor, cx.base_descriptors.get(ObjectKind::Realm));
            set_uninit!(realm.global_env, HeapPtr::uninit());
            set_uninit!(realm.global_object, HeapPtr::uninit());
            set_uninit!(realm.intrinsics, Intrinsics::new_uninit());
            set_uninit!(realm.template_map, HeapPtr::uninit());

            let realm = realm.to_handle();

            realm.clone().intrinsics.initialize(cx, realm);

            realm
        })
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
    fn template_map_field(&self) -> TemplateMapField {
        TemplateMapField(*self)
    }

    pub fn add_template_object(
        &mut self,
        cx: &mut Context,
        template_node: AstPtr<TemplateLiteral>,
        template_object: Handle<ObjectValue>,
    ) {
        self.template_map_field()
            .insert(cx, template_node, template_object.get_());
    }

    // 9.3.3 SetRealmGlobalObject
    // Initializes remaining properties of realm that were not initialized in `new_uninit`.
    pub fn initialize(
        &mut self,
        cx: &mut Context,
        global_object: Option<Handle<ObjectValue>>,
        this_value: Option<Handle<ObjectValue>>,
    ) {
        let global_object = global_object.unwrap_or_else(|| ordinary_object_create(cx));

        let this_value = this_value.unwrap_or(global_object);

        self.global_object = global_object.get_();
        self.global_env = GlobalEnvironment::new(cx, global_object, this_value).get_();
        self.template_map = TemplateMapField::new(cx, TemplateMap::MIN_CAPACITY);
    }
}

// 9.6 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: &mut Context) -> Handle<Realm> {
    HandleScope::new(cx, |cx| {
        let mut realm = Realm::new_uninit(cx);
        let exec_ctx = ExecutionContext::new(
            cx,
            /* function */ None,
            realm,
            /* script_or_module */ None,
            /* lexical_env */ cx.uninit_environment,
            /* variable_env */ cx.uninit_environment,
            /* private_env */ None,
            /* is_strict_mode */ false,
        );

        cx.push_execution_context(exec_ctx);

        realm.initialize(cx, None, None);
        set_default_global_bindings(cx, realm);

        realm
    })
}

struct TemplateMapField(Handle<Realm>);

impl BsHashMapField<AstPtr<TemplateLiteral>, HeapPtr<ObjectValue>> for TemplateMapField {
    fn new(cx: &mut Context, capacity: usize) -> HeapPtr<TemplateMap> {
        TemplateMap::new(cx, ObjectKind::RealmTemplateMap, capacity)
    }

    fn get(&self) -> HeapPtr<TemplateMap> {
        self.0.template_map
    }

    fn set(&mut self, map: HeapPtr<TemplateMap>) {
        self.0.template_map = map;
    }
}
