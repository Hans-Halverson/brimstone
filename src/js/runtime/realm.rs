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
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    Context,
};

// 9.3 Realm Record
pub struct Realm {
    pub global_env: Gc<GlobalEnvironment>,
    pub global_object: Gc<ObjectValue>,
    pub intrinsics: Intrinsics,
    pub template_map: HashMap<AstPtr<TemplateLiteral>, Gc<ObjectValue>>,
}

impl GcDeref for Realm {}

impl Realm {
    // 9.3.1 CreateRealm
    pub fn new(cx: &mut Context) -> Gc<Realm> {
        // Realm record must be created before setting up intrinsics, as realm must be referenced
        // during intrinsic creation.
        let realm = cx.heap.alloc(Realm {
            // Initialized in set_global_object
            global_env: Gc::uninit(),
            global_object: Gc::uninit(),
            intrinsics: Intrinsics::new_uninit(),
            template_map: HashMap::new(),
        });

        realm.clone().intrinsics.initialize(cx, realm);

        realm
    }

    // 9.3.3 SetRealmGlobalObject
    fn set_global_object(
        &mut self,
        cx: &mut Context,
        global_object: Option<Gc<ObjectValue>>,
        this_value: Option<Gc<ObjectValue>>,
    ) {
        let global_object = global_object.unwrap_or_else(|| {
            let ordinary_object =
                ordinary_object_create(self.get_intrinsic(Intrinsic::ObjectPrototype));
            cx.heap.alloc(ordinary_object).into()
        });

        let this_value = this_value.unwrap_or(global_object);

        self.global_object = global_object;
        self.global_env = GlobalEnvironment::new(cx, global_object, this_value);
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.intrinsics.get(intrinsic)
    }
}

// 9.6 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: &mut Context) -> Gc<Realm> {
    let mut realm = Realm::new(cx);
    let exec_ctx = cx.heap.alloc(ExecutionContext {
        script_or_module: None,
        realm,
        function: None,
        lexical_env: cx.uninit_environment,
        variable_env: cx.uninit_environment,
        private_env: None,
        is_strict_mode: false,
    });

    cx.push_execution_context(exec_ctx);

    realm.set_global_object(cx, None, None);
    set_default_global_bindings(cx, realm);

    realm
}
