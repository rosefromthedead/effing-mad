#![feature(generators)]
#![feature(generator_trait)]

use core::ops::ControlFlow;
use core::cell::Cell;
use effing_mad::{effectful, handle, run};

fn main() {
    let state = Cell::new(34);
    let handled1 = handle(use_state(), |get()| ControlFlow::Continue(state.get()));
    let handled2 = handle(handled1, |put(v)| {
        state.set(v);
        ControlFlow::Continue(())
    });
    run(handled2);
    println!("final value: {}", state.get());
}

effing_mad::effect_set! {
    // No generics support yet, but everything is made of integers anyway...
    StateI32 {
        fn get() -> i32;
        fn put(v: i32) -> ();
    }
}

// Rust encourages immutability!
#[effectful(StateI32)]
fn use_state() {
    let initial = yield StateI32::get();
    println!("initial value: {}", initial);
    yield StateI32::put(initial + 5);
}
