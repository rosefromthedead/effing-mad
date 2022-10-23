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

pub struct ResultMapEff<G, E, Effs, U> {
    g: Option<Result<G, E>>,
    _effs: PhantomData<*mut Effs>,
    _u: PhantomData<fn() -> U>,
}

impl<G, E, Effs, U> Generator<Effs::Injections> for ResultMapEff<G, E, Effs, U>
where
    E: Unpin,
    Effs: EffectList,
    G: Generator<Effs::Injections, Yield = Effs, Return = U>,
{
    type Yield = Effs;
    type Return = Result<U, E>;

    fn resume(
        self: Pin<&mut Self>,
        injs: Effs::Injections,
    ) -> GeneratorState<Self::Yield, Self::Return> {
        unsafe {
            let g = &mut self.get_unchecked_mut().g;
            match g {
                Some(Ok(g)) => match Pin::new_unchecked(g).resume(injs) {
                    GeneratorState::Yielded(effs) => GeneratorState::Yielded(effs),
                    GeneratorState::Complete(ret) => GeneratorState::Complete(Ok(ret)),
                },
                Some(Err(_)) => {
                    let Some(Err(e)) = core::mem::take(g) else { unreachable!() };
                    GeneratorState::Complete(Err(e))
                },
                None => panic!("resumed after completed"),
            }
        }
    }
}

pub struct ResultMapErrEff<G, T, Effs, U> {
    g: Option<Result<T, G>>,
    _effs: PhantomData<*mut Effs>,
    _u: PhantomData<fn() -> U>,
}

impl<G, T, Effs, U> Generator<Effs::Injections> for ResultMapErrEff<G, T, Effs, U>
where
    T: Unpin,
    Effs: EffectList,
    G: Generator<Effs::Injections, Yield = Effs, Return = U>,
{
    type Yield = Effs;
    type Return = Result<T, U>;

    fn resume(
        self: Pin<&mut Self>,
        injs: Effs::Injections,
    ) -> GeneratorState<Self::Yield, Self::Return> {
        unsafe {
            let g = &mut self.get_unchecked_mut().g;
            match g {
                Some(Err(g)) => match Pin::new_unchecked(g).resume(injs) {
                    GeneratorState::Yielded(effs) => GeneratorState::Yielded(effs),
                    GeneratorState::Complete(ret) => GeneratorState::Complete(Err(ret)),
                },
                Some(Ok(_)) => {
                    let Some(Ok(e)) = core::mem::take(g) else { unreachable!() };
                    GeneratorState::Complete(Ok(e))
                },
                None => panic!("resumed after completed"),
            }
        }
    }
}

pub trait ResultExt<T, E>: Sized {
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> ResultMapEff<G, E, Effs, G::Return>
    where
        Effs: EffectList,
        G: Generator<Effs::Injections, Yield = Effs>;

    fn map_err_eff<Effs, G>(self, g: impl FnOnce(E) -> G) -> ResultMapErrEff<G, T, Effs, G::Return>
    where
        Effs: EffectList,
        G: Generator<Effs::Injections, Yield = Effs>;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> ResultMapEff<G, E, Effs, G::Return>
    where
        Effs: EffectList,
        G: Generator<Effs::Injections, Yield = Effs>,
    {
        ResultMapEff {
            g: Some(self.map(g)),
            _effs: PhantomData,
            _u: PhantomData,
        }
    }

    fn map_err_eff<Effs, G>(self, g: impl FnOnce(E) -> G) -> ResultMapErrEff<G, T, Effs, G::Return>
    where
        Effs: EffectList,
        G: Generator<Effs::Injections, Yield = Effs>,
    {
        ResultMapErrEff {
            g: Some(self.map_err(g)),
            _effs: PhantomData,
            _u: PhantomData,
        }
    }
}
