use crate::{js::runtime::value::Value, maybe_};

use super::{
    abstract_operations::define_property_or_throw,
    completion::AbstractResult,
    environment::global_environment::GlobalEnvironment,
    execution_context::ExecutionContext,
    gc::{Gc, GcDeref},
    intrinsics::intrinsics::{Intrinsic, Intrinsics},
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    property_descriptor::PropertyDescriptor,
    Context,
};

// 9.3 Realm Record
pub struct Realm {
    pub global_env: Gc<GlobalEnvironment>,
    pub global_object: Gc<ObjectValue>,
    pub intrinsics: Intrinsics,
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

    // 9.3.4 SetDefaultGlobalBindings
    fn set_default_global_bindings(&mut self, cx: &mut Context) -> AbstractResult<()> {
        macro_rules! value_prop {
            ($name:expr, $value:expr, $is_writable:expr, $is_enumerable:expr, $is_configurable:expr) => {
                maybe_!(define_property_or_throw(
                    cx,
                    self.global_object,
                    $name,
                    PropertyDescriptor::data(
                        $value,
                        $is_writable,
                        $is_enumerable,
                        $is_configurable
                    )
                ));
            };
        }

        macro_rules! intrinsic_prop {
            ($name:expr, $intrinsic:ident) => {
                let value = self.get_intrinsic(Intrinsic::$intrinsic);
                maybe_!(define_property_or_throw(
                    cx,
                    self.global_object,
                    $name,
                    PropertyDescriptor::data(value.into(), true, false, true)
                ));
            };
        }

        // 19.1 Value Properties of the Global Object
        value_prop!(
            "globalThis",
            self.global_env.global_this_value.into(),
            true,
            false,
            true
        );
        value_prop!(
            "Infinity",
            Value::number(f64::INFINITY),
            false,
            false,
            false
        );
        value_prop!("NaN", Value::nan(), false, false, false);
        value_prop!("undefined", Value::undefined(), false, false, false);

        // 19.3 Constructor Properties of the Global Object
        intrinsic_prop!("EvalError", EvalErrorConstructor);
        intrinsic_prop!("Object", ObjectConstructor);
        intrinsic_prop!("RangeError", RangeErrorConstructor);
        intrinsic_prop!("ReferenceError", ReferenceErrorConstructor);
        intrinsic_prop!("SyntaxError", SyntaxErrorConstructor);
        intrinsic_prop!("TypeError", TypeErrorConstructor);
        intrinsic_prop!("URIError", URIErrorConstructor);

        ().into()
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
    });

    cx.push_execution_context(exec_ctx);

    realm.set_global_object(cx, None, None);
    realm.set_default_global_bindings(cx);

    realm
}
