use std::{cell::RefCell, rc::Rc};

use crate::js::runtime::{
    gc::Gc,
    intrinsics::{function_prototype::FunctionPrototype, object_prototype::ObjectPrototype},
    object_value::ObjectValue,
    realm::Realm,
    Context,
};

#[repr(u8)]
pub enum Intrinsic {
    ObjectPrototype = 0,
    FunctionPrototype,
    Last,
}

impl Intrinsic {
    const fn num_intrinsics() -> usize {
        Intrinsic::Last as usize
    }
}

pub struct Intrinsics {
    intrinsics: Vec<Gc<ObjectValue>>,
}

impl Intrinsics {
    pub fn new_uninit() -> Intrinsics {
        Intrinsics {
            intrinsics: Vec::new(),
        }
    }

    // 8.2.2 CreateIntrinsics
    pub fn initialize(&mut self, cx: &mut Context, realm: Rc<RefCell<Realm>>) {
        let intrinsics = &mut self.intrinsics;
        intrinsics.reserve_exact(Intrinsic::num_intrinsics());

        macro_rules! register_intrinsic {
            ($intrinsic_name:ident, $struct_name:ident) => {
                intrinsics[Intrinsic::$intrinsic_name as usize] =
                    ($struct_name::new(cx, realm.clone()));
            };
        }

        register_intrinsic!(ObjectPrototype, ObjectPrototype);
        register_intrinsic!(FunctionPrototype, FunctionPrototype);
    }

    pub fn get(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.intrinsics[intrinsic as usize]
    }
}
