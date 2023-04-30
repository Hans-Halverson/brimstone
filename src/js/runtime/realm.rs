use std::collections::HashMap;

use crate::js::parser::ast::{AstPtr, TemplateLiteral};

use super::{
    environment::global_environment::GlobalEnvironment,
    execution_context::ExecutionContext,
    gc::{Gc, GcDeref},
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
    descriptor: Gc<ObjectDescriptor>,
    global_env: Gc<GlobalEnvironment>,
    global_object: Gc<ObjectValue>,
    template_map: HashMap<AstPtr<TemplateLiteral>, Gc<ObjectValue>>,
    intrinsics: Intrinsics,
}

impl GcDeref for Realm {}

impl Realm {
    // 9.3.1 CreateRealm
    pub fn new(cx: &mut Context) -> Gc<Realm> {
        // Realm record must be created before setting up intrinsics, as realm must be referenced
        // during intrinsic creation.
        let mut realm = cx.heap.alloc(Realm {
            // Initialized in set_global_object
            descriptor: cx.base_descriptors.get(ObjectKind::Realm),
            global_env: Gc::uninit(),
            global_object: Gc::uninit(),
            intrinsics: Intrinsics::new_uninit(),
            template_map: HashMap::new(),
        });

        let this_realm = realm.clone();
        realm.intrinsics.initialize(cx, this_realm);

        realm
    }

    #[inline]
    pub fn global_object(&self) -> Gc<ObjectValue> {
        self.global_object
    }

    #[inline]
    pub fn global_env(&self) -> Gc<GlobalEnvironment> {
        self.global_env
    }

    pub fn global_this_value(&self) -> Gc<ObjectValue> {
        self.global_env.global_this_value()
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.intrinsics.get(intrinsic)
    }

    pub fn get_template_object(
        &self,
        template_node: AstPtr<TemplateLiteral>,
    ) -> Option<Gc<ObjectValue>> {
        self.template_map
            .get(&template_node)
            .map(|template_object| *template_object)
    }

    pub fn add_template_object(
        &mut self,
        template_node: AstPtr<TemplateLiteral>,
        template_object: Gc<ObjectValue>,
    ) {
        self.template_map.insert(template_node, template_object);
    }
}

impl Gc<Realm> {
    // 9.3.3 SetRealmGlobalObject
    pub fn set_global_object(
        &mut self,
        cx: &mut Context,
        global_object: Option<Gc<ObjectValue>>,
        this_value: Option<Gc<ObjectValue>>,
    ) {
        let global_object = global_object.unwrap_or_else(|| {
            ordinary_object_create(cx, self.get_intrinsic(Intrinsic::ObjectPrototype)).into()
        });

        let this_value = this_value.unwrap_or(global_object);

        self.global_object = global_object;
        self.global_env = GlobalEnvironment::new(cx, global_object, this_value);
    }
}

// 9.6 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: &mut Context) -> Gc<Realm> {
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
