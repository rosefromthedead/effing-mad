//! Implementation details of the macros exported by `effing_mad`.

use core::marker::PhantomData;

use frunk::{
    coproduct::{CNil, CoprodUninjector},
    Coproduct,
};

use crate::{injection::Tagged, Effect, IntoEffect};

#[must_use]
pub fn mark<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

pub fn get_inj<E, Injs, Index>(injs: Injs, _marker: PhantomData<E>) -> Option<E::Injection>
where
    E: Effect,
    Injs: CoprodUninjector<Tagged<E::Injection, E>, Index>,
{
    injs.uninject().ok().map(Tagged::untag)
}

pub fn get_inj2<E, I, Injs>(injs: Injs, _marker: PhantomData<I>) -> Option<I::Injection>
where
    I: IntoEffect<Effect = E>,
    E: Effect<Injection = Injs>,
{
    I::uninject(injs)
}

pub trait EffectSet<Tail> {
    type Out;
}

impl<E: Effect, Tail> EffectSet<Tail> for E {
    type Out = Coproduct<E, Tail>;
}

pub trait Prepend<Tail> {
    type Out;
}

impl<Tail> Prepend<Tail> for CNil {
    type Out = Tail;
}

impl<Head, Tail1: Prepend<Tail2>, Tail2> Prepend<Tail2> for Coproduct<Head, Tail1> {
    type Out = Coproduct<Head, Tail1::Out>;
}
