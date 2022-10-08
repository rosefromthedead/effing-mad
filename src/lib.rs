#![feature(generators)]
#![feature(generator_trait)]
#![feature(pin_macro)]
#![no_std]

pub mod injection;
pub mod macro_impl;

pub use coproduct;
use coproduct::{Coproduct, Count, Embed, EmptyUnion, Merge, Split, Union};
use core::{
    future::Future,
    ops::{ControlFlow, Generator, GeneratorState},
    pin::Pin,
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
    F: Generator<Coproduct<Union<Begin, EmptyUnion>>, Yield = Coproduct<EmptyUnion>, Return = R>,
{
    let pinned = core::pin::pin!(f);
    match pinned.resume(coproduct::inject(Begin)) {
        GeneratorState::Yielded(absurd) => absurd.ex_falso(),
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
    type Effects = Union<E, EmptyUnion>;
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
    All: EffectList,
    Handled: Effect,
    Unhandled: EffectList,
    BeginIndex: Count,
    InjIndex,
    InjsIndices,
    EmbedIndices,
    SplitIndices,
>(
    g: G,
    mut handler: impl FnMut(Handled) -> ControlFlow<R, Handled::Injection>,
) -> impl Generator<Unhandled::Injections, Yield = Unhandled, Return = R>
where
    All::Injections: coproduct::At<BeginIndex, Begin>
        + coproduct::At<InjIndex, Tagged<Handled::Injection, Handled>>,
    All: coproduct::Split<Coproduct!(Handled), SplitIndices, Remainder = Unhandled>,
    Coproduct!(Tagged<Handled::Injection, Handled>, Begin): Embed<All::Injections, InjsIndices>,
    Unhandled::Injections: Embed<All::Injections, EmbedIndices>,
    G: Generator<All::Injections, Yield = All, Return = R>,
{
    handle_group(g, move |effs: Coproduct!(Handled)| match effs.take_head() {
        Ok(eff) => match handler(eff) {
            ControlFlow::Continue(inj) => {
                ControlFlow::Continue(coproduct::inject(Tagged::new(inj)))
            }
            ControlFlow::Break(ret) => ControlFlow::Break(ret),
        },
        Err(absurd) => absurd.ex_falso(),
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
/// `Handled` must be a [`Coproduct`](coproduct::Coproduct) of effects.
///
/// Care should be taken to only produce an injection type when handling the corresponding effect.
/// If the injection type does not match the effect that is being handled, the computation will
/// most likely panic.
pub fn handle_group<
    G,
    R,
    All: EffectList,
    Handled: EffectList,
    Unhandled: EffectList,
    EffsIndices,
    InjsIndices,
    BeginIndex: Count,
    EmbedIndices,
>(
    mut g: G,
    mut handler: impl FnMut(Handled) -> ControlFlow<R, Handled::Injections>,
) -> impl Generator<Unhandled::Injections, Yield = Unhandled, Return = R>
where
    All::Injections: coproduct::At<BeginIndex, Begin>,
    All: coproduct::Split<Handled, EffsIndices, Remainder = Unhandled>,
    Handled::Injections: Embed<All::Injections, InjsIndices>,
    Unhandled::Injections: Embed<All::Injections, EmbedIndices>,
    G: Generator<All::Injections, Yield = All, Return = R>,
{
    move |_begin: Unhandled::Injections| {
        let mut injection = coproduct::inject(Begin);
        loop {
            // safety: im 90% sure that since we are inside Generator::resume which takes
            // Pin<&mut self>, all locals in this function are effectively pinned and this call is
            // simply projecting that
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                GeneratorState::Yielded(effs) => match effs.split() {
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
    G: Generator<Coproduct!(Tagged<Eff::Injection, Eff>, Begin), Yield = Coproduct!(Eff)>,
    Fut: Future<Output = ControlFlow<G::Return, Eff::Injection>>,
{
    let mut injs = coproduct::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // generators
        let pinned = unsafe { Pin::new_unchecked(&mut g) };
        match pinned.resume(injs) {
            GeneratorState::Yielded(effs) => {
                let eff = match effs.take_head() {
                    Ok(v) => v,
                    Err(never) => never.ex_falso(),
                };
                match handler(eff).await {
                    ControlFlow::Continue(new_injs) => {
                        injs = coproduct::inject(Tagged::new(new_injs))
                    }
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
    Is: coproduct::At<BeginIndex, Begin>,
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
    E: Effect,
    H,
    Pre: EffectList,
    Unhandled: EffectList,
    Handler: EffectList,
    Post: EffectList,
    EffIndex,
    BeginIndex1,
    BeginIndex2,
    InjIndex,
    SubsetIndices1,
    SubsetIndices2,
    EmbedIndices1,
    EmbedIndices2,
    EmbedIndices3,
    MergeIndices,
>(
    mut g: G1,
    mut handler: impl FnMut(E) -> H,
) -> impl Generator<Post::Injections, Yield = Post, Return = R>
where
    H: Generator<Handler::Injections, Yield = Handler, Return = E::Injection>,
    Pre: coproduct::At<EffIndex, E, Pruned = Unhandled>,
    Unhandled: Embed<Post, EmbedIndices1>,
    Handler: Embed<Post, EmbedIndices2>,
    Pre::Injections: coproduct::At<BeginIndex1, Begin>
        + coproduct::At<InjIndex, Tagged<E::Injection, E>, Pruned = Unhandled::Injections>,
    Unhandled::Injections: Embed<Pre::Injections, EmbedIndices3>,
    Handler::Injections: coproduct::At<BeginIndex2, Begin>,
    Post::Injections: coproduct::Split<Unhandled::Injections, SubsetIndices1>
        + coproduct::Split<Handler::Injections, SubsetIndices2>,
    G1: Generator<Pre::Injections, Yield = Pre, Return = R>,
    // for restricting the the parameters to just one impl
    Unhandled: Merge<Handler, MergeIndices, Merged = Post>,
{
    move |_begin: Post::Injections| {
        let mut injection = coproduct::inject(Begin);
        loop {
            // safety: see handle_group()
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                GeneratorState::Yielded(effs) => match effs.uninject() {
                    // the effect we are handling
                    Ok(eff) => {
                        let mut handling = handler(eff);
                        let mut handler_inj = coproduct::inject(Begin);
                        'run_handler: loop {
                            // safety: same again
                            let pinned = unsafe { Pin::new_unchecked(&mut handling) };
                            match pinned.resume(handler_inj) {
                                GeneratorState::Yielded(effs) => {
                                    let effs: Post::Injections = yield effs.embed();
                                    handler_inj = effs.split().ok().unwrap();
                                }
                                GeneratorState::Complete(inj) => {
                                    injection = coproduct::inject(Tagged::new(inj));
                                    break 'run_handler;
                                }
                            }
                        }
                    }
                    // any other effect
                    Err(effs) => {
                        let effs: Post::Injections = yield effs.embed();
                        let inj: Unhandled::Injections = effs.split().ok().unwrap();
                        injection = inj.embed();
                    }
                },
                GeneratorState::Complete(ret) => return ret,
            }
        }
    }
}
