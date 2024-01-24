//! This library brings typed effects to Rust in a flexible and composable way. By building on
//! [`Coroutine`]s, effectful computations can be expressed in a way that allows arbitrary and
//! swappable behaviour - any handler of the correct type can be applied to a computation, meaning
//! different semantics of effects can be selected at each call site of an effectful function.
//!
//! ## Glossary
//! - effectful computation: an in-progress computation that uses effects. analogous to `Future`.
//! - effectful function: a function that returns an effectful computation. analogous to `async fn`.
//! - effect: you know, I'm actually not sure. I should ask one of my PL teachers. In this library
//! though, an effect is a value that can be passed out of an effectful computation and into an
//! effect handler, which produces another value to pass back in.
//! - injection: an `effing_mad` term referring to the value passed into a computation as a result of
//! it running an effect.
//! - "pure" function: a Rust function that does not use `effing_mad` effects. Rust is not a pure
//! language (crudely, Rust code can `println!()` whenever it wants) so these docs use quotes to
//! indicate this meaning as opposed to the real meaning of pure, where functions do not use side
//! effects.
//!
//! ## Getting started
//! Define an [`Effect`]. Now, you can define an [`#[effectful(…)]`](effing_macros::effectful)
//! function that uses it. Once you call this function, its effects can be handled one by one with
//! [`handle`]. Handlers are "pure" Rust functions, but it's easiest to construct them using
//! [`handler!`](effing_macros::handler). Once all the effects have been handled away, a computation
//! can be driven with [`run`].
//!
//! ## Interaction with `async`
//! There are two ways to bring together the async world and the effectful world. The first, and
//! simplest, is [`handle_async`]. This allows you to handle the last effect in a computation using
//! a handler that is an `async fn`.
//!
//! The second, more freaky way is with the contents of [`effects::future`]. These allow you to
//! convert between futures and effectful computations freely - namely the `effectfulise` function
//! and the `futurise` function will take your futures and your computations and abstract away all
//! the other nonsense in that module to get you the respective constructs.

#![feature(doc_auto_cfg)]
#![feature(doc_notable_trait)]
#![feature(coroutines)]
#![feature(coroutine_trait)]
#![no_std]
#![warn(missing_docs)]

#[cfg(feature = "alloc")]
extern crate alloc;

pub use frunk;

pub mod effects;
pub mod higher_order;
pub mod injection;
pub mod macro_impl;

use core::{
    future::Future,
    ops::{ControlFlow, Coroutine, CoroutineState},
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
/// Effectful computations are coroutines, but if they have no effects, it is guaranteed that they
/// will never yield. Therefore they can be run by resuming them once. This function does that.
pub fn run<F, R>(mut f: F) -> R
where
    F: Coroutine<Coproduct<Begin, CNil>, Yield = CNil, Return = R>,
{
    let pinned = core::pin::pin!(f);
    match pinned.resume(Coproduct::Inl(Begin)) {
        CoroutineState::Yielded(never) => match never {},
        CoroutineState::Complete(ret) => ret,
    }
}

/// An effect that must be handled by the caller of an effectful computation, or propagated up the
/// call stack.
pub trait Effect {
    /// The type of value that running this effect gives.
    type Injection;
}

/// Types which represent multiple effects.
///
/// Effects that are commonly used together can be grouped using a type that implements this trait.
/// Defining an [`#[effectful]`](effing_macros::effectful) function that uses all of the effects in
/// a group can then be done by naming the group instead of each effect.
#[doc(notable_trait)]
pub trait EffectGroup {
    /// A [`Coproduct`](frunk::coproduct::Coproduct) of effects in this group.
    type Effects;
}

impl<E: Effect> EffectGroup for E {
    type Effects = Coproduct<E, CNil>;
}

/// Create a new effectful computation by applying a "pure" function to the return value of an
/// existing computation.
pub fn map<E, I, T, U>(
    mut g: impl Coroutine<I, Yield = E, Return = T>,
    f: impl FnOnce(T) -> U,
) -> impl Coroutine<I, Yield = E, Return = U> {
    move |mut injs: I| {
        loop {
            // safety: see handle_group()
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injs) {
                CoroutineState::Yielded(effs) => injs = yield effs,
                CoroutineState::Complete(ret) => return f(ret),
            }
        }
    }
}

/// Apply a "pure" handler to an effectful computation, handling one effect.
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
) -> impl Coroutine<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    Coprod!(Tagged<E::Injection, E>, Begin): CoproductEmbedder<PreIs, InjsIndices>,
    PreEs: EffectList<Injections = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PostEs>,
    PostEs: EffectList<Injections = PostIs>,
    PreIs: CoprodInjector<Begin, BeginIndex> + CoprodInjector<Tagged<E::Injection, E>, InjIndex>,
    PostIs: CoproductEmbedder<PreIs, EmbedIndices>,
    G: Coroutine<PreIs, Yield = PreEs, Return = R>,
{
    handle_group(g, move |effs| match effs {
        Coproduct::Inl(eff) => match handler(eff) {
            ControlFlow::Continue(inj) => ControlFlow::Continue(Coproduct::Inl(Tagged::new(inj))),
            ControlFlow::Break(ret) => ControlFlow::Break(ret),
        },
        Coproduct::Inr(never) => match never {},
    })
}

/// Apply a "pure" handler to an effectful computation, handling any number of effects.
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
) -> impl Coroutine<PostIs, Yield = PostEs, Return = R>
where
    Es: EffectList<Injections = Is>,
    Is: CoproductEmbedder<PreIs, InjsIndices>,
    PreEs: EffectList<Injections = PreIs> + CoproductSubsetter<Es, EffsIndices, Remainder = PostEs>,
    PostEs: EffectList<Injections = PostIs>,
    PreIs: CoprodInjector<Begin, BeginIndex>,
    PostIs: CoproductEmbedder<PreIs, EmbedIndices>,
    G: Coroutine<PreIs, Yield = PreEs, Return = R>,
{
    move |_begin: PostIs| {
        let mut injection = PreIs::inject(Begin);
        loop {
            // safety: im 90% sure that since we are inside Coroutine::resume which takes
            // Pin<&mut self>, all locals in this function are effectively pinned and this call is
            // simply projecting that
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                CoroutineState::Yielded(effs) => match effs.subset() {
                    Ok(effs) => match handler(effs) {
                        ControlFlow::Continue(injs) => injection = injs.embed(),
                        ControlFlow::Break(ret) => return ret,
                    },
                    Err(effs) => injection = (yield effs).embed(),
                },
                CoroutineState::Complete(ret) => return ret,
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
///
/// For more flexible interactions with Futures, see [`effects::future`].
pub async fn handle_async<Eff, G, Fut>(mut g: G, mut handler: impl FnMut(Eff) -> Fut) -> G::Return
where
    Eff: Effect,
    G: Coroutine<Coprod!(Tagged<Eff::Injection, Eff>, Begin), Yield = Coprod!(Eff)>,
    Fut: Future<Output = ControlFlow<G::Return, Eff::Injection>>,
{
    let mut injs = Coproduct::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // coroutines
        let pinned = unsafe { Pin::new_unchecked(&mut g) };
        match pinned.resume(injs) {
            CoroutineState::Yielded(effs) => {
                let eff = match effs {
                    Coproduct::Inl(v) => v,
                    Coproduct::Inr(never) => match never {},
                };
                match handler(eff).await {
                    ControlFlow::Continue(new_injs) => injs = Coproduct::Inl(Tagged::new(new_injs)),
                    ControlFlow::Break(ret) => return ret,
                }
            },
            CoroutineState::Complete(ret) => return ret,
        }
    }
}

/// Handle all of the remaining effects in a computation using an async handler.
///
/// For handling one effect asynchronously, see [`handle_async`]. For details on handling effects in
/// groups, see [`handle_group`].
///
/// When an async handler is used, it must handle all of the remaining effects in a computation,
/// because it is impossible to construct a computation that is both asynchronous and effectful.
///
/// For more flexible interactions with Futures, see [`effects::future`].
pub async fn handle_group_async<G, Fut, Es, Is, BeginIndex>(
    mut g: G,
    mut handler: impl FnMut(Es) -> Fut,
) -> G::Return
where
    Es: EffectList<Injections = Is>,
    Is: CoprodInjector<Begin, BeginIndex>,
    G: Coroutine<Is, Yield = Es>,
    Fut: Future<Output = ControlFlow<G::Return, Is>>,
{
    let mut injs = Is::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // coroutines
        let pinned = unsafe { Pin::new_unchecked(&mut g) };
        match pinned.resume(injs) {
            CoroutineState::Yielded(effs) => match handler(effs).await {
                ControlFlow::Continue(new_injs) => injs = new_injs,
                ControlFlow::Break(ret) => return ret,
            },
            CoroutineState::Complete(ret) => return ret,
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
) -> impl Coroutine<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    H: Coroutine<HandlerIs, Yield = HandlerEs, Return = E::Injection>,
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
    G1: Coroutine<PreIs, Yield = PreEs, Return = R>,
{
    move |_begin: PostIs| {
        let mut injection = PreIs::inject(Begin);
        loop {
            // safety: see handle_group()
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                CoroutineState::Yielded(effs) => match effs.uninject() {
                    // the effect we are handling
                    Ok(eff) => {
                        let mut handling = handler(eff);
                        let mut handler_inj = HandlerIs::inject(Begin);
                        'run_handler: loop {
                            // safety: same again
                            let pinned = unsafe { Pin::new_unchecked(&mut handling) };
                            match pinned.resume(handler_inj) {
                                CoroutineState::Yielded(effs) => {
                                    handler_inj = PostIs::subset(yield effs.embed()).ok().unwrap();
                                },
                                CoroutineState::Complete(inj) => {
                                    injection = PreIs::inject(Tagged::new(inj));
                                    break 'run_handler;
                                },
                            }
                        }
                    },
                    // any other effect
                    Err(effs) => {
                        injection =
                            PreHandleIs::embed(PostIs::subset(yield effs.embed()).ok().unwrap());
                    },
                },
                CoroutineState::Complete(ret) => return ret,
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
) -> impl Coroutine<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    H: Coroutine<HandlerIs, Yield = HandlerEs, Return = E::Injection>,
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
    G1: Coroutine<PreIs, Yield = PreEs, Return = R>,
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
) -> impl Coroutine<
    Coproduct<Tagged<E2::Injection, E2>, PreHandleIs>,
    Yield = Coproduct<E2, PreHandleEs>,
    Return = R,
>
where
    E1: Effect,
    E2: Effect,
    H: Coroutine<HandlerIs, Yield = HandlerEs, Return = E1::Injection>,
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
    G1: Coroutine<PreIs, Yield = PreEs, Return = R>,
{
    transform(g, handler)
}
