use core::marker::PhantomData;

use frunk::{coproduct::{CoprodUninjector, CNil}, Coproduct};

use crate::{injection::Tagged, Effect};

// I would have liked to put this in the effing_macros crate, but it turns out you can't do that.
#[macro_export]
macro_rules! effect_set {
    ($effects:ident {
        $(
        fn $effect:ident($($arg:ident: $arg_ty:ty),*) -> $ret:ty;
        )*
    }) => {
        struct $effects;

        impl $effects {
            $(
            fn $effect($($arg: $arg_ty),*) -> $effect {
                $effect($($arg),*)
            }
            )*
        }

        impl<Tail> ::effing_mad::macro_impl::EffectSet<Tail> for $effects {
            type Out =
                <::frunk::Coprod!($($effect),*) as ::effing_mad::macro_impl::Prepend<Tail>>::Out;
        }

        $(
        #[allow(non_camel_case_types)]
        struct $effect($($arg_ty),*);

        impl ::effing_mad::Effect for $effect {
            type Injection = $ret;
        }
        )*
    };
}

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
