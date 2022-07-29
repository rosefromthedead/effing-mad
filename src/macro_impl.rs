use core::marker::PhantomData;

use frunk::coproduct::CoprodUninjector;

use crate::{injection::Tagged, Effect};

/// Construct a PhantomData with the type of an expression
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
