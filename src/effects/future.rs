//! Functions and types for converting between Futures and effectful computations.

use core::{
    future::Future,
    ops::{Generator, GeneratorState},
    pin::Pin,
    task::{Context, Poll, Waker},
};

use frunk::{Coprod, Coproduct};

use crate::{
    injection::{Begin, EffectList, Tagged},
    Effect, EffectGroup,
};

/// An effect equivalent to `future.await`.
pub struct Await;
/// A request that the handler provide a [Context](core::task::Context) so that futures can be
/// polled.
pub struct GetContext;

impl Effect for Await {
    type Injection = ();
}
impl Effect for GetContext {
    // no lifetimes issues now lmao
    type Injection = *const Waker;
}

type FEffs = <FutureEffs as EffectGroup>::Effects;
type FInjs = <Coprod!(Await, GetContext) as EffectList>::Injections;

/// The effects that allow effectful computations to emulate Futures.
pub struct FutureEffs;

impl EffectGroup for FutureEffs {
    type Effects = Coprod!(Await, GetContext);
}

/// Brings a computation from effects land into futures land.
///
/// If a computation has effects [`GetContext`] and [`Await`] then it behaves like a Future and so
/// can be used as one through this wrapper. See [`EffExt::futurise`] for a way to construct this.
pub struct Futurise<G> {
    g: G,
    // TODO: if we specify that the first inj doesn't *have* to be `Begin`, we can get rid of this
    // field. would that work?
    has_begun: bool,
}

impl<G> Future for Futurise<G>
where
    G: Generator<FInjs, Yield = FEffs>,
{
    type Output = <G as Generator<FInjs>>::Return;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // see the todo on has_begun
        let mut inj = if self.has_begun {
            Coproduct::inject(Tagged::new(()))
        } else {
            Coproduct::inject(Begin)
        };
        // project
        let g = unsafe { Pin::new_unchecked(&mut self.as_mut().get_unchecked_mut().g) };
        let res = g.resume(inj);
        match res {
            GeneratorState::Yielded(eff) => match eff.uninject() {
                Ok(GetContext) => inj = Coproduct::inject(Tagged::new(cx.waker() as *const Waker)),
                Err(Coproduct::Inl(Await)) => panic!("can't await without context"),
                Err(Coproduct::Inr(never)) => match never {},
            },
            GeneratorState::Complete(ret) => return Poll::Ready(ret),
        }
        let g = unsafe { Pin::new_unchecked(&mut self.get_unchecked_mut().g) };
        let res = g.resume(inj);
        match res {
            GeneratorState::Yielded(eff) => match eff.uninject() {
                Ok(Await) => return Poll::Pending,
                Err(Coproduct::Inl(GetContext)) => panic!("no need to GetContext twice"),
                Err(Coproduct::Inr(never)) => match never {},
            },
            GeneratorState::Complete(ret) => return Poll::Ready(ret),
        }
    }
}

/// Brings a future into effects land.
///
/// Futures can be expressed in terms of effectful computations with effects [`GetContext`] and
/// [`Await`]. See [`FutureExt::effectfulise`] for a way to construct this.
pub struct Effectfulise<F> {
    f: F,
}

impl<F> Generator<FInjs> for Effectfulise<F>
where
    F: Future,
{
    type Yield = FEffs;
    type Return = F::Output;

    fn resume(self: Pin<&mut Self>, injs: FInjs) -> GeneratorState<Self::Yield, Self::Return> {
        // safety: project
        let f = unsafe { Pin::new_unchecked(&mut self.get_unchecked_mut().f) };
        match injs.uninject::<Tagged<*const Waker, _>, _>() {
            // safety: it's a pointer to something that the caller has a reference to.
            Ok(waker) => match f.poll(&mut Context::from_waker(unsafe { &*waker.untag() })) {
                Poll::Ready(ret) => GeneratorState::Complete(ret),
                Poll::Pending => GeneratorState::Yielded(Coproduct::inject(GetContext)),
            },
            Err(_) => GeneratorState::Yielded(Coproduct::inject(GetContext)),
        }
    }
}

/// Effect-related functions on [Future](core::future::Future)s.
pub trait FutureExt: Sized {
    /// Wraps this `Future` in a type that allows it to be used as an effectful computation.
    fn effectfulise(self) -> Effectfulise<Self>;
}
/// Future-related functions on effectful computations.
pub trait EffExt: Sized {
    /// Wraps this computation in a type that allows it to be used as a `Future`.
    fn futurise(self) -> Futurise<Self>;
}

impl<F: Future> FutureExt for F {
    fn effectfulise(self) -> Effectfulise<Self> {
        Effectfulise { f: self }
    }
}
impl<G> EffExt for G
where
    G: Generator<FInjs, Yield = FEffs>,
{
    fn futurise(self) -> Futurise<Self> {
        Futurise {
            g: self,
            has_begun: false,
        }
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use std::{
        future::Future,
        sync::Arc,
        task::{Context, Poll, Wake},
    };

    use super::{EffExt, FutureExt};

    struct NoopWake;
    impl Wake for NoopWake {
        fn wake(self: Arc<Self>) {}
    }

    #[test]
    fn back_and_forth() {
        // lol
        let f = core::future::ready(5);
        let eff_f = f.effectfulise();
        let fut_eff_f = eff_f.futurise();
        let eff_fut_eff_f = fut_eff_f.effectfulise();
        let fut_eff_fut_eff_f = eff_fut_eff_f.futurise();
        let pin_fut_eff_fut_eff_f = core::pin::pin!(fut_eff_fut_eff_f);

        let waker = Arc::new(NoopWake).into();
        let mut context = Context::from_waker(&waker);
        assert_eq!(pin_fut_eff_fut_eff_f.poll(&mut context), Poll::Ready(5))
    }
}
