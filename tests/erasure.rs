use std::any::{Any, TypeId};

struct Registers {
    slots: Vec<Types>,
}

#[derive(Copy, Clone)]
union Types {
    user: *mut Box<dyn Any>,
    f: f64,
}

impl Registers {
    fn new() -> Self {
        Self {
            slots: vec![],
        }
    }

    fn push<T>(&mut self, t: T) -> usize
    where
        T: Any + Clone,
    {
        let wrapped = if TypeId::of::<T>() == TypeId::of::<f64>() {
            let f = unsafe { std::mem::transmute_copy(&t) };
            Types { f }
        } else {
            Types { user: Box::into_raw(Box::new(Box::new(t))) }
        };

        self.slots.push(wrapped);
        self.slots.len() - 1
    }

    fn access<T>(&mut self, idx: usize) -> &mut T
    where
        T: Any + Clone,
    {
        let val = &mut self.slots[idx];
        if TypeId::of::<T>() == TypeId::of::<f64>() {
            unsafe { std::mem::transmute(&mut val.f) }
        } else {
            unsafe { Box::leak(Box::from_raw(val.user)).downcast_mut().unwrap() }
        }
    }
}

#[derive(Debug, Clone)]
struct ArbitraryUserType {
    junk: Vec<usize>,
}

#[test]
fn test_registers() {
    let mut registers = Registers::new();
    let idx = registers.push::<f64>(10.0);
    {
        let float_ref = registers.access::<f64>(idx);
        dbg!(&float_ref);
        *float_ref = *float_ref + 1.0;
    }
    {
        let float_ref = registers.access::<f64>(idx);
        dbg!(&float_ref);
    }
    let a = ArbitraryUserType { junk: vec![] };
    let idx = registers.push(a);
    {
        let a_ref = registers.access::<ArbitraryUserType>(idx);
        dbg!(&a_ref);
        a_ref.junk.push(100);
        dbg!(a_ref);
    }
    {
        let a_ref = registers.access::<ArbitraryUserType>(idx);
        dbg!(a_ref);
    }
    panic!()
}