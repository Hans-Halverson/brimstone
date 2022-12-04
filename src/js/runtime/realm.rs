use std::{collections::HashMap, rc::Rc};

use super::{
    runtime::{Completion, ExecutionContext, LexicalEnvironment, ObjectValue},
    Agent,
};

// 8.2 Realm Record
pub struct Realm {
    pub global_env: Rc<LexicalEnvironment>,
    global_obj: Rc<ObjectValue>,
    instrinsics: Intrinsics,
}

type Intrinsics = HashMap<String, ObjectValue>;

impl Realm {
    // 8.2.1 CreateRealm
    pub fn new() -> Realm {
        // Realm record must be created before setting up instrinsics, as realm must be referenced
        // during instrinsic creation.
        let mut realm = Realm {
            global_env: Rc::new(LexicalEnvironment::EMPTY),
            global_obj: Rc::new(ObjectValue::EMPTY),
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
    fn set_global_object(&mut self) {
        // TODO: Create global object from OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]])
        let global_obj = Rc::new(ObjectValue::EMPTY);
        let this_val = global_obj.clone();

        self.global_obj = global_obj.clone();
        self.global_env = Rc::new(LexicalEnvironment::new_global_environment(
            global_obj, this_val,
        ));
    }

    // 8.2.4 SetDefaultGlobalBindings
    fn set_default_global_bindings(&mut self) {
        // TODO: Add global bindings to global object
    }
}

// 8.5 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(agent: &mut Agent) -> Completion {
    let exec_ctx = ExecutionContext {
        program: None,
        realm: Realm::new(),
        function: None,
        lexical_env: Rc::new(LexicalEnvironment::EMPTY),
        variable_env: Rc::new(LexicalEnvironment::EMPTY),
    };

    agent.push_execution_context(exec_ctx);

    let realm = &mut agent.current_execution_context().realm;
    realm.set_global_object();
    realm.set_default_global_bindings();

    Completion::Normal(None)
}
