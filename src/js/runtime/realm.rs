use std::{collections::HashMap, rc::Rc};

use super::{
    completion::Completion, environment::environment::LexicalEnvironment, gc::Gc,
    runtime::ExecutionContext, value::ObjectValue, Context,
};

// 8.2 Realm Record
pub struct Realm {
    pub global_env: Rc<LexicalEnvironment>,
    global_obj: Gc<ObjectValue>,
    instrinsics: Intrinsics,
}

type Intrinsics = HashMap<String, ObjectValue>;

impl Realm {
    // 8.2.1 CreateRealm
    pub fn new(cx: &mut Context) -> Realm {
        // Realm record must be created before setting up instrinsics, as realm must be referenced
        // during instrinsic creation.
        let mut realm = Realm {
            global_env: Rc::new(LexicalEnvironment::placeholder()),
            // TODO: Set to empty object instead of allocating
            global_obj: cx.heap.alloc_object(),
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
        let global_obj = cx.heap.alloc_object();
        let this_val = global_obj;

        self.global_obj = global_obj;
        self.global_env = Rc::new(LexicalEnvironment::new_global_environment(
            global_obj, this_val,
        ));
    }

    // 8.2.4 SetDefaultGlobalBindings
    fn set_default_global_bindings(&mut self) {
        unimplemented!()
    }
}

// 8.5 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(cx: &mut Context) -> Completion {
    let realm = Realm::new(cx);
    let mut exec_ctx = cx.heap.alloc(ExecutionContext {
        program: None,
        realm,
        function: None,
        lexical_env: Rc::new(LexicalEnvironment::placeholder()),
        variable_env: Rc::new(LexicalEnvironment::placeholder()),
    });

    cx.push_execution_context(exec_ctx);

    let realm = &mut exec_ctx.as_mut().realm;
    realm.set_global_object(cx);
    realm.set_default_global_bindings();

    Completion::empty()
}
