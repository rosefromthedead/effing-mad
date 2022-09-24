#![feature(generators)]
#![feature(generator_trait)]
#![feature(pin_macro)]
#![no_std]

pub use frunk;

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
use injection::{Begin, EffectList, Tagged};

/// An uninhabited type that can never be constructed.
///
/// Substitutes for `!` until that is stabilised.
pub enum Never {}

/// Run an effectful computation that has no effects.
///
/// Effectful computations are generators, but if they have no effects, it is guaranteed that they
/// will never yield. Therefore they can be run by resuming them once. This function does that.
pub fn run<F, R>(mut f: F) -> R
where
    F: Generator<Coproduct<Begin, CNil>, Yield = CNil, Return = R>,
{
    let pinned = core::pin::pin!(f);
    match pinned.resume(Coproduct::Inl(Begin)) {
        GeneratorState::Yielded(never) => match never {},
        GeneratorState::Complete(ret) => ret,
    }
}

/// A side effect that must be handled by the caller of an effectful computation, or propagated up
/// the call stack.
pub trait Effect {
    /// The type of value that running this effect gives.
    type Injection;
}

pub trait EffectGroup {
    type Effects;
}

impl<E: Effect> EffectGroup for E {
    type Effects = Coproduct<E, CNil>;
}

/// Create a new effectful computation by applying a pure function to the return value of an
/// existing computation.
pub fn map<E, I, T, U>(
    mut g: impl Generator<I, Yield = E, Return = T>,
    f: impl FnOnce(T) -> U,
) -> impl Generator<I, Yield = E, Return = U> {
    move |mut injs: I| {
        loop {
            // safety: see handle_group()
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injs) {
                GeneratorState::Yielded(effs) => injs = yield effs,
                GeneratorState::Complete(ret) => return f(ret),
            }
        }
    }
}

/// Apply a pure handler to an effectful computation, handling one effect.
///
/// When given an effectful computation with effects (A, B, C) and a handler for effect C, this
/// returns a new effectful computation with effects (A, B). Handlers can choose for each instance
/// of their effect whether to resume the computation, passing in a value (injection) or to force a
/// return from the computation. This is done using
/// [`ControlFlow::Continue`](core::ops::ControlFlow::Continue) and
/// [`ControlFlow::Break`](core::ops::ControlFlow::Break) respectively.
///
/// For handling multiple effects with one closure, see [`handle_group`].
pub fn handle<
    G,
    R,
    E,
    PreEs,
    PostEs,
    EffIndex,
    PreIs,
    PostIs,
    BeginIndex,
    InjIndex,
    InjsIndices,
    EmbedIndices,
>(
    g: G,
    mut handler: impl FnMut(E) -> ControlFlow<R, E::Injection>,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    Coprod!(Tagged<E::Injection, E>, Begin): CoproductEmbedder<PreIs, InjsIndices>,
    PreEs: EffectList<Injections = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PostEs>,
    PostEs: EffectList<Injections = PostIs>,
    PreIs: CoprodInjector<Begin, BeginIndex> + CoprodInjector<Tagged<E::Injection, E>, InjIndex>,
    PostIs: CoproductEmbedder<PreIs, EmbedIndices>,
    G: Generator<PreIs, Yield = PreEs, Return = R>,
{
    handle_group(g, move |effs| match effs {
        Coproduct::Inl(eff) => match handler(eff) {
            ControlFlow::Continue(inj) => ControlFlow::Continue(Coproduct::Inl(Tagged::new(inj))),
            ControlFlow::Break(ret) => ControlFlow::Break(ret),
        },
        Coproduct::Inr(never) => match never {},
    })
}

/// Apply a pure handler to an effectful computation, handling any number of effects.
///
/// When given an effectful computation with effects (A, B, C, D) and a handler for effects (A, B),
/// this function returns a new effectful computation with effects (C, D). Handlers can choose for
/// each instance of their effects whether to resume the computation, passing in a value (injection)
/// or to force a return from the computation. This is done using
/// [`ControlFlow::Continue`](core::ops::ControlFlow::Continue) and
/// [`ControlFlow::Break`](core::ops::ControlFlow::Break) respectively.
///
/// `Es` must be a [`Coproduct`](frunk::Coproduct) of effects.
///
/// Care should be taken to only produce an injection type when handling the corresponding effect.
/// If the injection type does not match the effect that is being handled, the computation will
/// most likely panic.
pub fn handle_group<
    G,
    R,
    Es,
    Is,
    PreEs,
    PostEs,
    PreIs,
    PostIs,
    EffsIndices,
    InjsIndices,
    BeginIndex,
    EmbedIndices,
>(
    mut g: G,
    mut handler: impl FnMut(Es) -> ControlFlow<R, Is>,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    Es: EffectList<Injections = Is>,
    Is: CoproductEmbedder<PreIs, InjsIndices>,
    PreEs: EffectList<Injections = PreIs> + CoproductSubsetter<Es, EffsIndices, Remainder = PostEs>,
    PostEs: EffectList<Injections = PostIs>,
    PreIs: CoprodInjector<Begin, BeginIndex>,
    PostIs: CoproductEmbedder<PreIs, EmbedIndices>,
    G: Generator<PreIs, Yield = PreEs, Return = R>,
{
    move |_begin: PostIs| {
        let mut injection = PreIs::inject(Begin);
        loop {
            // safety: im 90% sure that since we are inside Generator::resume which takes
            // Pin<&mut self>, all locals in this function are effectively pinned and this call is
            // simply projecting that
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                GeneratorState::Yielded(effs) => match effs.subset() {
                    Ok(effs) => match handler(effs) {
                        ControlFlow::Continue(injs) => injection = injs.embed(),
                        ControlFlow::Break(ret) => return ret,
                    },
                    Err(effs) => injection = (yield effs).embed(),
                },
                GeneratorState::Complete(ret) => return ret,
            }
        }
    }
}

/// Handle the last effect in a computation using an async handler.
///
/// For handling multiple effects asynchronously, see [`handle_group_async`]. For details on
/// handling effects, see [`handle`].
///
/// When an async handler is used, it must handle all of the remaining effects in a computation,
/// because it is impossible to construct a computation that is both asynchronous and effectful.
pub async fn handle_async<Eff, G, Fut>(mut g: G, mut handler: impl FnMut(Eff) -> Fut) -> G::Return
where
    Eff: Effect,
    G: Generator<Coprod!(Tagged<Eff::Injection, Eff>, Begin), Yield = Coprod!(Eff)>,
    Fut: Future<Output = ControlFlow<G::Return, Eff::Injection>>,
{
    let mut injs = Coproduct::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // generators
        let pinned = unsafe { Pin::new_unchecked(&mut g) };
        match pinned.resume(injs) {
            GeneratorState::Yielded(effs) => {
                let eff = match effs {
                    Coproduct::Inl(v) => v,
                    Coproduct::Inr(never) => match never {},
                };
                match handler(eff).await {
                    ControlFlow::Continue(new_injs) => injs = Coproduct::Inl(Tagged::new(new_injs)),
                    ControlFlow::Break(ret) => return ret,
                }
            }
            GeneratorState::Complete(ret) => return ret,
        }
    }
}

/// Handle all of the remaining effects in a computation using an async handler.
///
/// For handling one effect asynchronously, see [`handle_group_async`]. For details on handling
/// effects in groups, see [`handle_group`].
///
/// When an async handler is used, it must handle all of the remaining effects in a computation,
/// because it is impossible to construct a computation that is both asynchronous and effectful.
pub async fn handle_group_async<G, Fut, Es, Is, BeginIndex>(
    mut g: G,
    mut handler: impl FnMut(Es) -> Fut,
) -> G::Return
where
    Es: EffectList<Injections = Is>,
    Is: CoprodInjector<Begin, BeginIndex>,
    G: Generator<Is, Yield = Es>,
    Fut: Future<Output = ControlFlow<G::Return, Is>>,
{
    let mut injs = Is::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // generators
        let pinned = unsafe { Pin::new_unchecked(&mut g) };
        match pinned.resume(injs) {
            GeneratorState::Yielded(effs) => match handler(effs).await {
                ControlFlow::Continue(new_injs) => injs = new_injs,
                ControlFlow::Break(ret) => return ret,
            },
            GeneratorState::Complete(ret) => return ret,
        }
    }
}

/// Handle one effect in the computation `g` by running other effects.
///
/// It is not possible to get rustc to infer the type of `PostEs`, so calling this function almost
/// always requires annotating that - which means you also have to annotate 21 underscores.
/// For this reason, prefer to use [`transform0`] or [`transform1`] instead, which should not
/// require annotation.
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
    PreEs: EffectList<Injections = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PreHandleEs>,
    PreHandleEs: EffectList<Injections = PreHandleIs> + CoproductEmbedder<PostEs, EmbedIndices1>,
    HandlerEs: EffectList<Injections = HandlerIs> + CoproductEmbedder<PostEs, EmbedIndices2>,
    PostEs: EffectList<Injections = PostIs>,
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
            // safety: see handle_group()
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
                                    handler_inj = PostIs::subset(yield effs.embed()).ok().unwrap();
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
                            PreHandleIs::embed(PostIs::subset(yield effs.embed()).ok().unwrap());
                    }
                },
                GeneratorState::Complete(ret) => return ret,
            }
        }
    }
}

/// Handle one effect of `g` by running other effects that it already uses.
///
/// This function is a special case of [`transform`] for when the handler does not introduce any
/// effects on top of the ones from `g` that it's not handling.
///
/// For introducing a new effect, see [`transform1`].
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
    PreEs: EffectList<Injections = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PostEs>,
    HandlerEs: EffectList<Injections = HandlerIs> + CoproductEmbedder<PostEs, EmbedIndices1>,
    PostEs: EffectList<Injections = PostIs> + CoproductEmbedder<PostEs, EmbedIndices2>,
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

/// Handle one effect of `g` by running a new effect.
///
/// This function is a special case of [`transform`] for when the handler introduces one effect on
/// top of the ones from `g` that it's not handling.
///
/// It is possible for the handler to run effects from `g` as well as the effect that it introduces.
///
/// To transform without introducing any effects, see [`transform0`].
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
    PreEs: EffectList<Injections = PreIs> + CoprodUninjector<E1, E1Index, Remainder = PreHandleEs>,
    PreHandleEs: EffectList<Injections = PreHandleIs>
        + CoproductEmbedder<Coproduct<E2, PreHandleEs>, EmbedIndices1>,
    HandlerEs: EffectList<Injections = HandlerIs>
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
