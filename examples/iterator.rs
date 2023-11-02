//! A demonstration of how to tap into the internals of effing-mad a little bit. Here we define the
//! classic `Iterator` effect, then turn it into a standard rust `Iterator`.

#![feature(generators)]
#![feature(generator_trait)]

use std::{
    marker::PhantomData,
    ops::{Generator, GeneratorState},
    pin::Pin,
};

use effing_mad::{
    effectful, handle_group, handler,
    injection::{EffectList, Tagged},
    run,
};
use frunk::{Coprod, Coproduct};

effing_mad::effects! {
    Iterator<T> {
        fn yield_next(v: T) -> ();
    }
}

#[effectful(Iterator<u32>)]
fn range_iterator(n: u32) {
    for i in 0..n {
        yield Iterator::yield_next(i);
    }
}

#[effectful(Iterator<u32>)]
fn triangle_iterator(n: u32) {
    for i in 0..n {
        range_iterator(i).do_
    }
}

fn main() {
    let mut vec = Vec::new();
    let iterator_collector = handler!(Iterator<u32> {
        yield_next(v) => vec.push(v),
    });
    let handled = handle_group(triangle_iterator(7), iterator_collector);
    run(handled);
    println!("collected values: {:?}", vec);

    let vec: Vec<_> = Iterator::into_rust_iterator(triangle_iterator(7)).collect();
    println!("collected values: {:?}", vec);
}

impl<T> Iterator<T> {
    fn into_rust_iterator<G>(g: G) -> impl std::iter::Iterator<Item = T>
    where
        G: Generator<
            <Coprod!(yield_next<T>) as EffectList>::Injections,
            Yield = Coprod!(yield_next<T>),
            Return = (),
        >,
    {
        struct RealIterator<T, G> {
            gen: Pin<Box<G>>,
            ty: PhantomData<Vec<T>>,
        }

        impl<T, G> std::iter::Iterator for RealIterator<T, G>
        where
            G: Generator<
                <Coprod!(yield_next<T>) as EffectList>::Injections,
                Yield = Coprod!(yield_next<T>),
                Return = (),
            >,
        {
            type Item = T;

            fn next(&mut self) -> Option<Self::Item> {
                match self.gen.as_mut().resume(Coproduct::Inl(Tagged::new(()))) {
                    GeneratorState::Yielded(Coproduct::Inl(yield_next(next, ..))) => Some(next),
                    GeneratorState::Yielded(Coproduct::Inr(never)) => match never {},
                    GeneratorState::Complete(()) => None,
                }
            }
        }

        RealIterator {
            gen: Box::pin(g),
            ty: PhantomData,
        }
    }
}
