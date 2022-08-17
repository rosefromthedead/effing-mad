use core::marker::PhantomData;

use frunk::{
    coproduct::{CNil, CoprodInjector},
    indices::{Here, There},
    Coproduct,
};

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

pub trait InjectionList {
    type Inj;
    type BeginIndex;
    type List: CoprodInjector<Self::Inj, Here> + CoprodInjector<Begin, Self::BeginIndex>;
}

impl InjectionList for CNil {
    type Inj = Begin;
    type BeginIndex = Here;
    type List = Coproduct<Begin, CNil>;
}

impl<E: Effect, Is: InjectionList> InjectionList for Coproduct<E, Is> {
    type Inj = Tagged<E::Injection, E>;
    type BeginIndex = There<Is::BeginIndex>;
    type List = Coproduct<Tagged<E::Injection, E>, Is::List>;
}
