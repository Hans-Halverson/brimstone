use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    environment::{environment::placeholder_environment, global_environment::GlobalEnvironment},
    execution_context::ExecutionContext,
    gc::Gc,
    object::OrdinaryObject,
    object_value::ObjectValue,
    Context,
};

// 8.2 Realm Record
pub struct Realm {
    pub global_env: Gc<GlobalEnvironment>,
    pub global_object: Gc<ObjectValue>,
    instrinsics: Intrinsics,
}

type Intrinsics = HashMap<String, ObjectValue>;

impl Realm {
    // 8.2.1 CreateRealm
    pub fn new(cx: &mut Context) -> Realm {
        // Realm record must be created before setting up instrinsics, as realm must be referenced
        // during instrinsic creation.
        let mut realm = Realm {
            // Initialized in set_global_object
            global_env: Gc::uninit(),
            global_object: Gc::uninit(),
            instrinsics: HashMap::new(),
        };

        realm.create_intrinsics();

        realm
    }

    // 8.2.2 CreateIntrinsics
    fn create_intrinsics(&mut self) {
        // TODO: Create intrinsics for realm
    }

    // 8.2.3 SetRealmGlobalObject
    fn set_global_object(&mut self, cx: &mut Context) {
        // TODO: Create global object from OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]])
        let global_object: Gc<ObjectValue> = cx.heap.alloc(OrdinaryObject::new()).into();
        let this_val = global_object;

        self.global_object = global_object;
        self.global_env = GlobalEnvironment::new(cx, global_object, this_val);
    }

    // 8.2.4 SetDefaultGlobalBindings
    fn set_default_global_bindings(&mut self) {
        // TODO: Create default global bindings in realm
    }
}

// 8.5 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: &mut Context) -> Rc<RefCell<Realm>> {
    let realm = Rc::new(RefCell::new(Realm::new(cx)));
    let placeholder_env = placeholder_environment(cx);
    let exec_ctx = cx.heap.alloc(ExecutionContext {
        script_or_module: None,
        realm: realm.clone(),
        function: None,
        lexical_env: placeholder_env,
        variable_env: placeholder_env,
    });

    cx.push_execution_context(exec_ctx);

    realm.borrow_mut().set_global_object(cx);
    realm.borrow_mut().set_default_global_bindings();

    realm
}
