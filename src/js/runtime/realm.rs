use std::{cell::RefCell, rc::Rc};

use super::{
    environment::global_environment::GlobalEnvironment,
    execution_context::ExecutionContext,
    gc::Gc,
    intrinsics::intrinsics::{Intrinsic, Intrinsics},
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    Context,
};

// 8.2 Realm Record
pub struct Realm {
    pub global_env: Gc<GlobalEnvironment>,
    pub global_object: Gc<ObjectValue>,
    pub intrinsics: Intrinsics,
}

impl Realm {
    // 8.2.1 CreateRealm
    pub fn new(cx: &mut Context) -> Rc<RefCell<Realm>> {
        // Realm record must be created before setting up intrinsics, as realm must be referenced
        // during intrinsic creation.
        let realm = Rc::new(RefCell::new(Realm {
            // Initialized in set_global_object
            global_env: Gc::uninit(),
            global_object: Gc::uninit(),
            intrinsics: Intrinsics::new_uninit(),
        }));

        realm.borrow_mut().intrinsics.initialize(cx, realm.clone());

        realm
    }

    // 8.2.3 SetRealmGlobalObject
    fn set_global_object(&mut self, cx: &mut Context) {
        let ordinary_object =
            ordinary_object_create(cx, self.get_intrinsic(Intrinsic::ObjectPrototype));
        let global_object: Gc<ObjectValue> = cx.heap.alloc(ordinary_object).into();
        let this_val = global_object;

        self.global_object = global_object;
        self.global_env = GlobalEnvironment::new(cx, global_object, this_val);
    }

    // 8.2.4 SetDefaultGlobalBindings
    fn set_default_global_bindings(&mut self) {
        // TODO: Create default global bindings in realm
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.intrinsics.get(intrinsic)
    }
}

// 8.5 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: &mut Context) -> Rc<RefCell<Realm>> {
    let realm = Realm::new(cx);
    let exec_ctx = cx.heap.alloc(ExecutionContext {
        script_or_module: None,
        realm: realm.clone(),
        function: None,
        lexical_env: cx.uninit_environment,
        variable_env: cx.uninit_environment,
    });

    cx.push_execution_context(exec_ctx);

    realm.borrow_mut().set_global_object(cx);
    realm.borrow_mut().set_default_global_bindings();

    realm
}
