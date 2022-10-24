use frunk::{Coprod, Coproduct};

use std::ops::{Generator, GeneratorState};

use crate::{
    injection::{Begin, Tagged},
    Effect,
};

#[derive(Clone)]
pub struct Nondet<T>(pub Vec<T>);

impl<T> Effect for Nondet<T> {
    type Injection = T;
}

pub fn run_nondet<G, T>(g: G) -> Vec<G::Return>
where
    G: Generator<Coprod!(Tagged<T, Nondet<T>>, Begin), Yield = Coprod!(Nondet<T>)> + Clone,
{
    let mut rets = Vec::new();
    run_nondet_inner(g, Coproduct::inject(Begin), &mut rets);
    rets
}

fn run_nondet_inner<G, T>(
    mut g: G,
    injs: Coprod!(Tagged<T, Nondet<T>>, Begin),
    rets: &mut Vec<G::Return>,
) where
    G: Generator<Coprod!(Tagged<T, Nondet<T>>, Begin), Yield = Coprod!(Nondet<T>)> + Clone,
{
    let mut pinned = core::pin::pin!(g);
    match pinned.as_mut().resume(injs) {
        GeneratorState::Yielded(effs) => {
            let Nondet(xs) = match effs {
                Coproduct::Inl(v) => v,
                Coproduct::Inr(never) => match never {},
            };
            for x in xs {
                let g2 = pinned.clone();
                run_nondet_inner(g2, Coproduct::inject(Tagged::new(x)), rets);
            }
        }
        GeneratorState::Complete(ret) => rets.push(ret),
    }
}
