//! Implementation details of the macros exported by `effing_mad`.

use coproduct::{self, Coproduct, EmptyUnion, IndexedDrop, Union};
use core::marker::PhantomData;

use crate::{injection::Tagged, Effect, EffectGroup};

#[must_use]
pub fn mark<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

pub fn get_inj<E, Injs, Index>(injs: Injs, _marker: PhantomData<E>) -> Option<E::Injection>
where
    E: Effect,
    Injs: coproduct::At<Index, Tagged<E::Injection, E>>,
{
    injs.uninject().ok().map(Tagged::untag)
}

pub trait FlattenEffects {
    type Out;
}

impl<T: IndexedDrop, O> FlattenEffects for Coproduct<T>
where
    T: FlattenEffects<Out = O>,
    O: IndexedDrop,
{
    type Out = Coproduct<<T as FlattenEffects>::Out>;
}

impl<G, Tail> FlattenEffects for Union<G, Tail>
where
    G: EffectGroup,
    <G as EffectGroup>::Effects: Prepend<Tail>,
{
    type Out = <<G as EffectGroup>::Effects as Prepend<Tail>>::Out;
}

pub trait Prepend<Tail> {
    type Out;
}

impl<Tail> Prepend<Tail> for EmptyUnion {
    type Out = Tail;
}

impl<Head, Tail1: Prepend<Tail2>, Tail2> Prepend<Tail2> for Union<Head, Tail1> {
    type Out = Union<Head, Tail1::Out>;
}
