use std::{collections::HashMap, num::NonZeroU64};

use crate::js::runtime::{
    gc::{Gc, GcDeref},
    Context,
};

// 6.2.11 Private Name
// Private names have a globally unique id and string description. String description must currently
// be known from context so it is elided.
pub type PrivateNameId = NonZeroU64;

// 9.2 Private Environment Record
pub struct PrivateEnvironment {
    pub names: HashMap<String, PrivateNameId>,
    pub outer: Option<Gc<PrivateEnvironment>>,
    next_id: NonZeroU64,
}

impl GcDeref for PrivateEnvironment {}

impl PrivateEnvironment {
    // 9.2.1.1 NewPrivateEnvironment
    pub fn new(cx: &mut Context, outer: Option<Gc<PrivateEnvironment>>) -> Gc<PrivateEnvironment> {
        cx.heap.alloc(PrivateEnvironment {
            names: HashMap::new(),
            outer,
            next_id: NonZeroU64::new(1).unwrap(),
        })
    }

    // 9.2.1.2 ResolvePrivateIdentifier
    pub fn resolve_private_identifier<'a>(&self, name: &str) -> PrivateNameId {
        match self.names.get(name) {
            Some(private_name_id) => *private_name_id,
            None => self.outer.unwrap().resolve_private_identifier(name),
        }
    }

    pub fn add_private_name(&mut self, description: String) {
        self.names.insert(description, self.next_id);
        self.next_id = self.next_id.checked_add(1).unwrap();
    }
}
