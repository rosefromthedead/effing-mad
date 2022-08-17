#![feature(generators)]
#![feature(generator_trait)]
#![feature(pin_macro)]
#![no_std]

pub mod injection;
pub mod macro_impl;

use core::{
    future::Future,
    ops::{ControlFlow, Generator, GeneratorState},
    pin::Pin,
};
use frunk::{
    coproduct::{CNil, CoprodInjector, CoprodUninjector, CoproductEmbedder},
    Coprod, Coproduct,
};

pub use effing_macros::{effectful, effects, handler};
use injection::{Begin, InjectionList, Tagged};

pub enum Never {}

pub fn run<F, R>(mut f: F) -> R
where
    F: Generator<Coproduct<Begin, CNil>, Yield = CNil, Return = R>,
{
    let pinned = core::pin::pin!(f);
    match pinned.resume(Coproduct::Inl(Begin)) {
        GeneratorState::Yielded(_) => unreachable!(),
        GeneratorState::Complete(ret) => ret,
    }
}

pub trait Effect {
    type Injection;
}

pub trait IntoEffect {
    type Effect: Effect;
    type Injection;

    fn into_effect(self) -> Self::Effect;
    fn inject(inj: Self::Injection) -> <Self::Effect as Effect>::Injection;
    fn uninject(injs: <Self::Effect as Effect>::Injection) -> Option<Self::Injection>;
}

impl<E: Effect> IntoEffect for E {
    type Effect = Self;
    type Injection = <Self as Effect>::Injection;

    fn into_effect(self) -> Self {
        self
    }
    fn inject(inj: Self::Injection) -> Self::Injection {
        inj
    }
    fn uninject(injs: Self::Injection) -> Option<Self::Injection> {
        Some(injs)
    }
}

pub fn map<E, I, T, U>(
    mut g: impl Generator<I, Yield = E, Return = T>,
    f: impl FnOnce(T) -> U,
) -> impl Generator<I, Yield = E, Return = U> {
    move |mut injs: I| {
        loop {
            // safety: see handle()
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injs) {
                GeneratorState::Yielded(effs) => injs = yield effs,
                GeneratorState::Complete(ret) => return f(ret),
            }
        }
    }
}

pub fn handle<G, R, E, PreEs, PostEs, EffIndex, PreIs, PostIs, BeginIndex, InjIndex, EmbedIndices>(
    mut g: G,
    mut handler: impl FnMut(E) -> ControlFlow<R, E::Injection>,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    PreEs: InjectionList<List = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PostEs>,
    PostEs: InjectionList<List = PostIs>,
    PreIs: CoprodInjector<Begin, BeginIndex> + CoprodInjector<Tagged<E::Injection, E>, InjIndex>,
    PostIs: CoproductEmbedder<PreIs, EmbedIndices>,
    G: Generator<PreIs, Yield = PreEs, Return = R>,
{
    move |_begin: PostIs| {
        let mut injection = PreIs::inject(Begin);
        loop {
            // safety: im 90% sure that since we are inside Generator::resume, which takes
            // Pin<&mut self>, all locals in this function are effectively pinned and this call is
            // simply projecting that
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                GeneratorState::Yielded(effs) => match effs.uninject() {
                    Ok(eff) => match handler(eff) {
                        ControlFlow::Continue(inj) => injection = PreIs::inject(Tagged::new(inj)),
                        ControlFlow::Break(ret) => return ret,
                    },
                    Err(effs) => {
                        let effs: PostEs = effs;
                        let inj = yield effs;
                        injection = inj.embed();
                    }
                },
                GeneratorState::Complete(ret) => return ret,
            }
        }
    }
}

pub async fn run_async<Eff, G, R, H, Fut>(mut g: G, mut handler: H) -> G::Return
where
    Eff: Effect,
    G: Generator<Coprod!(Tagged<Eff::Injection, Eff>, Begin), Yield = Coprod!(Eff), Return = R>,
    H: FnMut(Eff) -> Fut,
    Fut: Future<Output = ControlFlow<R, Eff::Injection>>,
{
    let mut inj = Coproduct::inject(Begin);
    loop {
        // safety: see handle() - remember that futures are pinned in the same way as generators
        let pinned = unsafe { Pin::new_unchecked(&mut g) };
        match pinned.resume(inj) {
            GeneratorState::Yielded(eff) => match handler(eff.take().unwrap()).await {
                ControlFlow::Continue(new_inj) => inj = Coproduct::inject(Tagged::new(new_inj)),
                ControlFlow::Break(ret) => return ret,
            },
            GeneratorState::Complete(ret) => return ret,
        }
    }
}
