//! Effectful versions of standard higher-order functions.

use core::{
    marker::PhantomData,
    ops::{Coroutine, CoroutineState},
    pin::Pin,
};

use crate::injection::EffectList;

/// An implementation detail of [`OptionExt::map_eff`].
pub struct OptionMapEff<G, Effs, U> {
    g: Option<G>,
    _effs: PhantomData<*mut Effs>,
    _u: PhantomData<fn() -> U>,
}

impl<G, Effs, U> Coroutine<Effs::Injections> for OptionMapEff<G, Effs, U>
where
    Effs: EffectList,
    G: Coroutine<Effs::Injections, Yield = Effs, Return = U>,
{
    type Yield = Effs;
    type Return = Option<U>;

    fn resume(
        self: Pin<&mut Self>,
        injs: Effs::Injections,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        unsafe {
            match &mut self.get_unchecked_mut().g {
                Some(g) => match Pin::new_unchecked(g).resume(injs) {
                    CoroutineState::Yielded(effs) => CoroutineState::Yielded(effs),
                    CoroutineState::Complete(ret) => CoroutineState::Complete(Some(ret)),
                },
                None => CoroutineState::Complete(None),
            }
        }
    }
}

/// Higher-order effectful functions on [`Option`].
pub trait OptionExt<T>: Sized {
    /// Transforms the value inside an [`Option::Some`] with some effects.
    ///
    /// This function is analogous to [`Option::map`] except it allows its argument to have effects
    /// which must then be handled by its caller. This means that the mapper function can, for
    /// example, await a `Future`. Other control flow constructs are of course possible here, and
    /// impossible with vanilla [`Option::map`].
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> OptionMapEff<G, Effs, G::Return>
    where
        Effs: EffectList,
        G: Coroutine<Effs::Injections, Yield = Effs>;
}

impl<T> OptionExt<T> for Option<T> {
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> OptionMapEff<G, Effs, G::Return>
    where
        Effs: EffectList,
        G: Coroutine<Effs::Injections, Yield = Effs>,
    {
        OptionMapEff {
            g: self.map(g),
            _effs: PhantomData,
            _u: PhantomData,
        }
    }
}

/// An implementation detail of [`ResultExt::map_eff`].
pub struct ResultMapEff<G, E, Effs, U> {
    g: Option<Result<G, E>>,
    _effs: PhantomData<*mut Effs>,
    _u: PhantomData<fn() -> U>,
}

impl<G, E, Effs, U> Coroutine<Effs::Injections> for ResultMapEff<G, E, Effs, U>
where
    E: Unpin,
    Effs: EffectList,
    G: Coroutine<Effs::Injections, Yield = Effs, Return = U>,
{
    type Yield = Effs;
    type Return = Result<U, E>;

    fn resume(
        self: Pin<&mut Self>,
        injs: Effs::Injections,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        unsafe {
            let g = &mut self.get_unchecked_mut().g;
            match g {
                Some(Ok(g)) => match Pin::new_unchecked(g).resume(injs) {
                    CoroutineState::Yielded(effs) => CoroutineState::Yielded(effs),
                    CoroutineState::Complete(ret) => CoroutineState::Complete(Ok(ret)),
                },
                Some(Err(_)) => {
                    let Some(Err(e)) = core::mem::take(g) else {
                        unreachable!()
                    };
                    CoroutineState::Complete(Err(e))
                },
                None => panic!("resumed after completed"),
            }
        }
    }
}

/// An implementation detail of [`ResultExt::map_err_eff`].
pub struct ResultMapErrEff<G, T, Effs, U> {
    g: Option<Result<T, G>>,
    _effs: PhantomData<*mut Effs>,
    _u: PhantomData<fn() -> U>,
}

impl<G, T, Effs, U> Coroutine<Effs::Injections> for ResultMapErrEff<G, T, Effs, U>
where
    T: Unpin,
    Effs: EffectList,
    G: Coroutine<Effs::Injections, Yield = Effs, Return = U>,
{
    type Yield = Effs;
    type Return = Result<T, U>;

    fn resume(
        self: Pin<&mut Self>,
        injs: Effs::Injections,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        unsafe {
            let g = &mut self.get_unchecked_mut().g;
            match g {
                Some(Err(g)) => match Pin::new_unchecked(g).resume(injs) {
                    CoroutineState::Yielded(effs) => CoroutineState::Yielded(effs),
                    CoroutineState::Complete(ret) => CoroutineState::Complete(Err(ret)),
                },
                Some(Ok(_)) => {
                    let Some(Ok(e)) = core::mem::take(g) else {
                        unreachable!()
                    };
                    CoroutineState::Complete(Ok(e))
                },
                None => panic!("resumed after completed"),
            }
        }
    }
}

/// Higher-order effectful functions on [`Result`].
pub trait ResultExt<T, E>: Sized {
    /// Transforms the value inside a [`Result::Ok`] with some effects.
    ///
    /// For more details, see [`OptionExt::map_eff`].
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> ResultMapEff<G, E, Effs, G::Return>
    where
        Effs: EffectList,
        G: Coroutine<Effs::Injections, Yield = Effs>;

    /// Transforms the value inside a [`Result::Err`] with some effects.
    ///
    /// For more details, see [`OptionExt::map_eff`].
    fn map_err_eff<Effs, G>(self, g: impl FnOnce(E) -> G) -> ResultMapErrEff<G, T, Effs, G::Return>
    where
        Effs: EffectList,
        G: Coroutine<Effs::Injections, Yield = Effs>;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn map_eff<Effs, G>(self, g: impl FnOnce(T) -> G) -> ResultMapEff<G, E, Effs, G::Return>
    where
        Effs: EffectList,
        G: Coroutine<Effs::Injections, Yield = Effs>,
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
        G: Coroutine<Effs::Injections, Yield = Effs>,
    {
        ResultMapErrEff {
            g: Some(self.map_err(g)),
            _effs: PhantomData,
            _u: PhantomData,
        }
    }
}
