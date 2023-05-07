use std::collections::HashMap;

use crate::{
    js::parser::ast::{AstPtr, TemplateLiteral},
    set_uninit,
};

use super::{
    environment::global_environment::GlobalEnvironment,
    execution_context::ExecutionContext,
    gc::{GcDeref, Handle, HeapPtr},
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
    template_map: HashMap<AstPtr<TemplateLiteral>, HeapPtr<ObjectValue>>,
    intrinsics: Intrinsics,
}

impl GcDeref for Realm {}

impl Realm {
    // 9.3.1 CreateRealm
    pub fn new(cx: &mut Context) -> Handle<Realm> {
        // Realm record must be created before setting up intrinsics, as realm must be referenced
        // during intrinsic creation.
        let mut realm = cx.heap.alloc_uninit::<Realm>();

        set_uninit!(realm.descriptor, cx.base_descriptors.get(ObjectKind::Realm));
        set_uninit!(realm.global_env, HeapPtr::uninit());
        set_uninit!(realm.global_object, HeapPtr::uninit());
        set_uninit!(realm.intrinsics, Intrinsics::new_uninit());
        set_uninit!(realm.template_map, HashMap::new());

        let this_realm = realm.clone();
        realm.intrinsics.initialize(cx, this_realm);

        realm
    }

    #[inline]
    pub fn global_object(&self) -> Handle<ObjectValue> {
        self.global_object
    }

    #[inline]
    pub fn global_env(&self) -> Handle<GlobalEnvironment> {
        self.global_env
    }

    pub fn global_this_value(&self) -> Handle<ObjectValue> {
        self.global_env.global_this_value()
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
            .map(|template_object| *template_object)
    }

    pub fn add_template_object(
        &mut self,
        template_node: AstPtr<TemplateLiteral>,
        template_object: Handle<ObjectValue>,
    ) {
        self.template_map
            .insert(template_node, template_object.get_());
    }
}

impl Handle<Realm> {
    // 9.3.3 SetRealmGlobalObject
    pub fn set_global_object(
        &mut self,
        cx: &mut Context,
        global_object: Option<Handle<ObjectValue>>,
        this_value: Option<Handle<ObjectValue>>,
    ) {
        let global_object = global_object.unwrap_or_else(|| ordinary_object_create(cx));

        let this_value = this_value.unwrap_or(global_object);

        self.global_object = global_object;
        self.global_env = GlobalEnvironment::new(cx, global_object, this_value);
    }
}

// 9.6 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: &mut Context) -> Handle<Realm> {
    let mut realm = Realm::new(cx);
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

    realm.set_global_object(cx, None, None);
    set_default_global_bindings(cx, realm);

    realm
}
