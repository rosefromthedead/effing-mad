#![feature(generators)]
#![feature(generator_trait)]
#![feature(pin_macro)]
#![no_std]

pub use frunk;

pub mod functor_eff;
pub mod injection;
pub mod macro_impl;

use core::{
    future::Future,
    ops::{ControlFlow, Generator, GeneratorState},
    pin::Pin,
};
use frunk::{
    coproduct::{CNil, CoprodInjector, CoprodUninjector, CoproductEmbedder, CoproductSubsetter},
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
        GeneratorState::Yielded(nil) => match nil {},
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
                    // the effect we are handling
                    Ok(eff) => match handler(eff) {
                        ControlFlow::Continue(inj) => injection = PreIs::inject(Tagged::new(inj)),
                        ControlFlow::Break(ret) => return ret,
                    },
                    // any other effect
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

pub fn transform<
    G1,
    R,
    E,
    H,
    PreEs,
    PreHandleEs,
    HandlerEs,
    PostEs,
    EffIndex,
    PreIs,
    PreHandleIs,
    HandlerIs,
    PostIs,
    BeginIndex1,
    BeginIndex2,
    BeginIndex3,
    InjIndex,
    SubsetIndices1,
    SubsetIndices2,
    EmbedIndices1,
    EmbedIndices2,
    EmbedIndices3,
>(
    mut g: G1,
    mut handler: impl FnMut(E) -> H,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    H: Generator<HandlerIs, Yield = HandlerEs, Return = E::Injection>,
    PreEs: InjectionList<List = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PreHandleEs>,
    PreHandleEs: InjectionList<List = PreHandleIs> + CoproductEmbedder<PostEs, EmbedIndices1>,
    HandlerEs: InjectionList<List = HandlerIs> + CoproductEmbedder<PostEs, EmbedIndices2>,
    PostEs: InjectionList<List = PostIs>,
    PreIs: CoprodInjector<Begin, BeginIndex1>
        + CoprodUninjector<Tagged<E::Injection, E>, InjIndex, Remainder = PreHandleIs>,
    PreHandleIs: CoproductEmbedder<PreIs, EmbedIndices3>,
    HandlerIs: CoprodInjector<Begin, BeginIndex2>,
    PostIs: CoprodInjector<Begin, BeginIndex3>
        + CoproductSubsetter<
            <PreIs as CoprodUninjector<Tagged<E::Injection, E>, InjIndex>>::Remainder,
            SubsetIndices1,
        > + CoproductSubsetter<HandlerIs, SubsetIndices2>,
    G1: Generator<PreIs, Yield = PreEs, Return = R>,
{
    move |_begin: PostIs| {
        let mut injection = PreIs::inject(Begin);
        loop {
            // safety: see handle()
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                GeneratorState::Yielded(effs) => match effs.uninject() {
                    // the effect we are handling
                    Ok(eff) => {
                        let mut handling = handler(eff);
                        let mut handler_inj = HandlerIs::inject(Begin);
                        'run_handler: loop {
                            // safety: same again
                            let pinned = unsafe { Pin::new_unchecked(&mut handling) };
                            match pinned.resume(handler_inj) {
                                GeneratorState::Yielded(effs) => {
                                    handler_inj = PostIs::subset(yield effs.embed()).ok().unwrap()
                                }
                                GeneratorState::Complete(inj) => {
                                    injection = PreIs::inject(Tagged::new(inj));
                                    break 'run_handler;
                                }
                            }
                        }
                    }
                    // any other effect
                    Err(effs) => {
                        injection =
                            PreHandleIs::embed(PostIs::subset(yield effs.embed()).ok().unwrap())
                    }
                },
                GeneratorState::Complete(ret) => return ret,
            }
        }
    }
}

pub fn transform0<
    G1,
    R,
    E,
    H,
    PreEs,
    HandlerEs,
    PostEs,
    EffIndex,
    PreIs,
    HandlerIs,
    PostIs,
    I1Index,
    BeginIndex1,
    BeginIndex2,
    BeginIndex3,
    SubsetIndices1,
    SubsetIndices2,
    EmbedIndices1,
    EmbedIndices2,
    EmbedIndices3,
>(
    g: G1,
    handler: impl FnMut(E) -> H,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    H: Generator<HandlerIs, Yield = HandlerEs, Return = E::Injection>,
    PreEs: InjectionList<List = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PostEs>,
    HandlerEs: InjectionList<List = HandlerIs> + CoproductEmbedder<PostEs, EmbedIndices1>,
    PostEs: InjectionList<List = PostIs> + CoproductEmbedder<PostEs, EmbedIndices2>,
    PreIs: CoprodInjector<Begin, BeginIndex1>
        + CoprodUninjector<Tagged<E::Injection, E>, I1Index, Remainder = PostIs>,
    HandlerIs: CoprodInjector<Begin, BeginIndex2>,
    PostIs: CoprodInjector<Begin, BeginIndex3>
        + CoproductSubsetter<HandlerIs, SubsetIndices1>
        + CoproductSubsetter<PostIs, SubsetIndices2>
        + CoproductEmbedder<PreIs, EmbedIndices3>,
    G1: Generator<PreIs, Yield = PreEs, Return = R>,
{
    transform(g, handler)
}

pub fn transform1<
    G1,
    R,
    E1,
    E2,
    H,
    PreEs,
    PreHandleEs,
    HandlerEs,
    E1Index,
    PreIs,
    PreHandleIs,
    HandlerIs,
    I1Index,
    BeginIndex1,
    BeginIndex2,
    BeginIndex3,
    SubsetIndices1,
    SubsetIndices2,
    EmbedIndices1,
    EmbedIndices2,
    EmbedIndices3,
>(
    g: G1,
    handler: impl FnMut(E1) -> H,
) -> impl Generator<
    Coproduct<Tagged<E2::Injection, E2>, PreHandleIs>,
    Yield = Coproduct<E2, PreHandleEs>,
    Return = R,
>
where
    E1: Effect,
    E2: Effect,
    H: Generator<HandlerIs, Yield = HandlerEs, Return = E1::Injection>,
    PreEs: InjectionList<List = PreIs> + CoprodUninjector<E1, E1Index, Remainder = PreHandleEs>,
    PreHandleEs: InjectionList<List = PreHandleIs>
        + CoproductEmbedder<Coproduct<E2, PreHandleEs>, EmbedIndices1>,
    HandlerEs: InjectionList<List = HandlerIs>
        + CoproductEmbedder<Coproduct<E2, PreHandleEs>, EmbedIndices2>,
    PreIs: CoprodInjector<Begin, BeginIndex1>
        + CoprodUninjector<Tagged<E1::Injection, E1>, I1Index, Remainder = PreHandleIs>,
    PreHandleIs: CoproductEmbedder<PreIs, EmbedIndices3>,
    HandlerIs: CoprodInjector<Begin, BeginIndex2>,
    Coproduct<Tagged<E2::Injection, E2>, PreHandleIs>: CoprodInjector<Begin, BeginIndex3>
        + CoproductSubsetter<HandlerIs, SubsetIndices1>
        + CoproductSubsetter<PreHandleIs, SubsetIndices2>,
    G1: Generator<PreIs, Yield = PreEs, Return = R>,
{
    transform(g, handler)
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
            GeneratorState::Yielded(eff) => {
                let eff: Eff = match eff.uninject() {
                    Ok(eff) => eff,
                    Err(other) => match other {},
                };
                match handler(eff).await {
                    ControlFlow::Continue(new_inj) => inj = Coproduct::inject(Tagged::new(new_inj)),
                    ControlFlow::Break(ret) => return ret,
                }
            }
            GeneratorState::Complete(ret) => return ret,
        }
    }
}
