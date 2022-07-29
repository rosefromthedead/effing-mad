#![feature(generators)]
#![feature(generator_trait)]
#![feature(pin_macro)]
#![no_std]

pub mod injection;
pub mod macro_impl;

use core::{
    ops::{ControlFlow, Generator, GeneratorState},
    pin::Pin,
};
use frunk::{
    coproduct::{CNil, CoprodInjector, CoprodUninjector, CoproductEmbedder},
    Coproduct,
};

pub use effing_macros::effectful;
use injection::{Begin, InjectionList, Tagged};

pub enum Never {}

pub struct Pure<T: Unpin>(Option<T>);

impl<T: Unpin> Generator<()> for Pure<T> {
    type Yield = Never;
    type Return = T;

    fn resume(mut self: Pin<&mut Self>, _arg: ()) -> GeneratorState<Never, T> {
        GeneratorState::Complete(self.0.take().unwrap())
    }
}

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

pub fn handle<
    F,
    R,
    E,
    PreEs,
    PostEs,
    EffIndex,
    PreIs,
    PostIs,
    BeginIndex1,
    InjIndex,
    EmbedIndices,
>(
    mut f: F,
    mut handler: impl FnMut(E) -> ControlFlow<R, E::Injection>,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    PreEs: InjectionList<List = PreIs> + CoprodUninjector<E, EffIndex, Remainder = PostEs>,
    PostEs: InjectionList<List = PostIs>,
    PreIs: CoprodInjector<Begin, BeginIndex1> + CoprodInjector<Tagged<E::Injection, E>, InjIndex>,
    PostIs: CoproductEmbedder<PreIs, EmbedIndices>,
    F: Generator<PreIs, Yield = PreEs, Return = R>,
{
    move |_begin: PostIs| {
        let mut injection = PreIs::inject(Begin);
        loop {
            // safety: i genuinely don't know
            let pinned = unsafe { Pin::new_unchecked(&mut f) };
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
