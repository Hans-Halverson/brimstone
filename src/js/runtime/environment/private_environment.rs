use std::collections::HashMap;

use crate::js::runtime::{
    gc::{Gc, GcDeref},
    object_descriptor::{ObjectDescriptor, ObjectKind},
    value::SymbolValue,
    Context,
};

// 6.2.11 Private Name
// Private names have a globally unique id and string description. String description must currently
// be known from context so it is elided.
pub type PrivateName = Gc<SymbolValue>;

// 9.2 Private Environment Record
#[repr(C)]
pub struct PrivateEnvironment {
    descriptor: Gc<ObjectDescriptor>,
    pub names: HashMap<String, PrivateName>,
    pub outer: Option<Gc<PrivateEnvironment>>,
}

impl GcDeref for PrivateEnvironment {}

impl PrivateEnvironment {
    // 9.2.1.1 NewPrivateEnvironment
    pub fn new(cx: &mut Context, outer: Option<Gc<PrivateEnvironment>>) -> Gc<PrivateEnvironment> {
        let descriptor = cx.base_descriptors.get(ObjectKind::PrivateEnvironment);
        cx.heap
            .alloc(PrivateEnvironment { descriptor, names: HashMap::new(), outer })
    }

    // 9.2.1.2 ResolvePrivateIdentifier
    pub fn resolve_private_identifier<'a>(&self, name: &str) -> PrivateName {
        match self.names.get(name) {
            Some(private_name_id) => *private_name_id,
            None => self.outer.unwrap().resolve_private_identifier(name),
        }
    }

    pub fn add_private_name(&mut self, cx: &mut Context, description: String) {
        let symbol_name = SymbolValue::new(cx, None);
        self.names.insert(description, symbol_name);
    }
}
