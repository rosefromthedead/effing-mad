#![feature(generators)]
#![feature(generator_trait)]
#![feature(pin_macro)]
#![no_std]

use core::{
    ops::{ControlFlow, Generator, GeneratorState},
    pin::Pin,
};
use frunk::coproduct::{CNil, CoprodUninjector};

pub use effing_macros::effectful;

pub enum Never {}

pub struct Pure<T: Unpin>(Option<T>);

impl<T: Unpin> Generator<()> for Pure<T> {
    type Yield = Never;
    type Return = T;

    fn resume(mut self: Pin<&mut Self>, _arg: ()) -> GeneratorState<Never, T> {
        GeneratorState::Complete(self.0.take().unwrap())
    }
}

pub fn unlift<F, R>(mut f: F) -> R
where
    F: Generator<Yield = CNil, Return = R>,
{
    let pinned = core::pin::pin!(f);
    match pinned.resume(()) {
        GeneratorState::Yielded(_) => unreachable!(),
        GeneratorState::Complete(ret) => ret,
    }
}

pub trait Effect {
    type Injection;
}

pub fn handle<F, Es, R, E, Index>(
    mut f: F,
    mut handler: impl FnMut(E) -> ControlFlow<R, E::Injection>,
) -> impl Generator<Yield = <Es as CoprodUninjector<E, Index>>::Remainder, Return = R>
where
    E: Effect,
    F: Generator<Yield = Es, Return = R>,
    <F as Generator>::Yield: CoprodUninjector<E, Index>,
{
    move || {
        loop {
            // safety: i genuinely don't know
            let pinned = unsafe { Pin::new_unchecked(&mut f) };
            match pinned.resume(()) {
                GeneratorState::Yielded(effs) => match effs.uninject() {
                    Ok(eff) => match handler(eff) {
                        ControlFlow::Continue(_inj) => todo!(),
                        ControlFlow::Break(ret) => return ret,
                    },
                    Err(effs) => yield effs,
                },
                GeneratorState::Complete(ret) => return ret,
            }
        }
    }
}
