//! Effectful operations on common functor types.

use core::{
    ops::{Generator, GeneratorState},
    pin::Pin,
};

use crate::injection::EffectList;

pub trait IntoEffectful: Sized {
    fn into_effectful(self) -> Effectful<Self> {
        Effectful(self)
    }
}

pub struct Effectful<T>(T);

impl<T> IntoEffectful for Option<T> {}

impl<T> Effectful<Option<T>> {
    pub fn map<F, G, Effs, U>(
        self,
        f: F,
    ) -> impl Generator<Effs::Injections, Yield = Effs, Return = Option<U>>
    where
        F: FnOnce(T) -> G,
        G: Generator<Effs::Injections, Yield = Effs, Return = U>,
        Effs: EffectList,
    {
        move |mut injs| {
            match self.0 {
                Some(v) => {
                    let mut g = f(v);
                    loop {
                        // inside a generator, locals are pinned
                        let pinned = unsafe { Pin::new_unchecked(&mut g) };
                        match pinned.resume(injs) {
                            GeneratorState::Yielded(effs) => injs = yield effs,
                            GeneratorState::Complete(v) => return Some(v),
                        }
                    }
                }
                None => return None,
            }
        }
    }
}

impl<T, E> Effectful<Result<T, E>> {
    pub fn map<F, G, Effs, U>(
        self,
        f: F,
    ) -> impl Generator<Effs::Injections, Yield = Effs, Return = Result<U, E>>
    where
        F: FnOnce(T) -> G,
        G: Generator<Effs::Injections, Yield = Effs, Return = U>,
        Effs: EffectList,
    {
        move |mut injs| {
            match self.0 {
                Ok(v) => {
                    let mut g = f(v);
                    loop {
                        // inside a generator, locals are pinned
                        let pinned = unsafe { Pin::new_unchecked(&mut g) };
                        match pinned.resume(injs) {
                            GeneratorState::Yielded(effs) => injs = yield effs,
                            GeneratorState::Complete(v) => return Ok(v),
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }

    pub fn map_err<O, G, Effs, F>(
        self,
        op: O,
    ) -> impl Generator<Effs::Injections, Yield = Effs, Return = Result<T, F>>
    where
        O: FnOnce(E) -> G,
        G: Generator<Effs::Injections, Yield = Effs, Return = F>,
        Effs: EffectList,
    {
        move |mut injs| {
            match self.0 {
                Ok(v) => return Ok(v),
                Err(e) => {
                    let mut g = op(e);
                    loop {
                        // inside a generator, locals are pinned
                        let pinned = unsafe { Pin::new_unchecked(&mut g) };
                        match pinned.resume(injs) {
                            GeneratorState::Yielded(effs) => injs = yield effs,
                            GeneratorState::Complete(e) => return Err(e),
                        }
                    }
                }
            }
        }
    }
}
