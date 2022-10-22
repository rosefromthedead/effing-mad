//! Effectful versions of standard higher-order functions.

use core::{
    marker::PhantomData,
    ops::{Generator, GeneratorState},
    pin::Pin,
};

use crate::injection::EffectList;

pub struct OptionMapEff<G, Effs, U> {
    g: Option<G>,
    _effs: PhantomData<*mut Effs>,
    _u: PhantomData<fn() -> U>,
}

impl<G, Effs, U> Generator<Effs::Injections> for OptionMapEff<G, Effs, U>
where
    Effs: EffectList,
    G: Generator<Effs::Injections, Yield = Effs, Return = U>,
{
    type Yield = Effs;
    type Return = Option<U>;

    fn resume(
        self: Pin<&mut Self>,
        injs: Effs::Injections,
    ) -> GeneratorState<Self::Yield, Self::Return> {
        unsafe {
            match &mut self.get_unchecked_mut().g {
                Some(g) => match Pin::new_unchecked(g).resume(injs) {
                    GeneratorState::Yielded(effs) => GeneratorState::Yielded(effs),
                    GeneratorState::Complete(ret) => GeneratorState::Complete(Some(ret)),
                },
                None => GeneratorState::Complete(None),
            }
        }
    }
}

pub trait OptionExt<T>: Sized {
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> OptionMapEff<G, Effs, G::Return>
    where
        Effs: EffectList,
        G: Generator<Effs::Injections, Yield = Effs>;
}

impl<T> OptionExt<T> for Option<T> {
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> OptionMapEff<G, Effs, G::Return>
    where
        Effs: EffectList,
        G: Generator<Effs::Injections, Yield = Effs>,
    {
        OptionMapEff {
            g: self.map(g),
            _effs: PhantomData,
            _u: PhantomData,
        }
    }
}
