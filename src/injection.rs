//! Implementation details of injections - values that come from running effects.

use core::marker::PhantomData;

use frunk::{coproduct::CNil, Coproduct};

use crate::Effect;

/// Before an effectful computation has started, there is no injection to pass in, because no
/// effects have been run yet. However, due to the signature of
/// [`Generator::resume`](core::ops::Generator::resume), it is necessary to pass one in anyway.
/// This type is used as a first injection for all effectful computations.
#[derive(Clone, Copy)]
pub struct Begin;

/// Tagging a value with `PhantomData` of another type allows it to be distinguished from other
/// occurrences of the same type in a coproduct. If two effects' injections were both `i32`, it
/// would be impossible to tell the injections apart without tagging them with the effect that they
/// come from.
pub struct Tagged<T, Tag>(T, PhantomData<Tag>);

impl<T, Tag> Tagged<T, Tag> {
    pub fn new(v: T) -> Self {
        Tagged(v, PhantomData)
    }

    pub fn untag(self) -> T {
        self.0
    }
}

impl<T: Clone, Tag> Clone for Tagged<T, Tag> {
    fn clone(&self) -> Self {
        Tagged::new(self.0.clone())
    }
}

/// Used for determining the list of injection types for a certain list of effect types.
///
/// All effect lists must have [`Begin`] in their injection lists, and injections in the list must
/// be [`Tagged`] with their effect type. This trait makes it easy to name the correct injection
/// list according to these rules.
pub trait EffectList {
    type Injections;
}

impl EffectList for CNil {
    type Injections = Coproduct<Begin, CNil>;
}

impl<E: Effect, Es: EffectList> EffectList for Coproduct<E, Es> {
    type Injections = Coproduct<Tagged<E::Injection, E>, Es::Injections>;
}
