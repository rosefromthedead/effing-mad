#![feature(generators)]
#![feature(generator_trait)]

use core::ops::ControlFlow;
use effing_mad::{effectful, handle, handler, run};

fn main() {
    let mut state = 34;
    let handled = handle(
        use_state(),
        handler! {
            StateI32,
            get() => ControlFlow::Continue(state),
            put(v) => {
                state = v;
                ControlFlow::Continue(())
            },
        },
    );
    run(handled);
    println!("final value: {}", state);
}

effing_mad::effects! {
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
