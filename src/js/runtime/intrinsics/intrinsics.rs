use crate::js::runtime::{
    gc::Gc, intrinsics::object_prototype::ObjectPrototype, object_value::ObjectValue, Context,
};

#[repr(u8)]
pub enum Intrinsic {
    ObjectPrototype = 0,
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
    pub fn initialize(&mut self, cx: &mut Context) {
        let intrinsics = &mut self.intrinsics;
        intrinsics.reserve_exact(Intrinsic::num_intrinsics());

        macro_rules! register_intrinsic {
            ($intrinsic_name:ident, $struct_name:ident) => {
                intrinsics[Intrinsic::$intrinsic_name as usize] = ($struct_name::new(cx));
            };
        }

        register_intrinsic!(ObjectPrototype, ObjectPrototype);
    }

    pub fn get(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.intrinsics[intrinsic as usize]
    }
}
