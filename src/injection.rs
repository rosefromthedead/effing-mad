use core::marker::PhantomData;

use frunk::{coproduct::CNil, Coproduct};

use crate::Effect;

pub struct Begin;

/// Tagging a value with `PhantomData` of another type allows it to be distinguished from other
/// occurrences of the same type in a coproduct. If two effects' injections were both i32, it would
/// be impossible to tell the injections apart without tagging them with the effect that they come
/// from.
pub struct Tagged<T, Tag>(T, PhantomData<Tag>);

impl<T, Tag> Tagged<T, Tag> {
    pub fn new(v: T) -> Self {
        Tagged(v, PhantomData)
    }

    pub fn untag(self) -> T {
        self.0
    }
}

pub trait EffectList {
    type Injections;
}

impl EffectList for CNil {
    type Injections = Coproduct<Begin, CNil>;
}

impl<E: Effect, Es: EffectList> EffectList for Coproduct<E, Es> {
    type Injections = Coproduct<Tagged<E::Injection, E>, Es::Injections>;
}
