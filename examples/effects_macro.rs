#![feature(generators)]
#![feature(generator_trait)]

use core::ops::ControlFlow;
use effing_mad::{effectful, handle, handler, run};

fn main() {
    let mut state = 34;
    let handled = handle(
        use_state(),
        handler! {
            state::State<i32>,
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
    state::State<T> {
        fn get() -> T;
        fn put(v: T) -> ();
    }
}

use state::State;
// Rust encourages immutability!
#[effectful(State<i32>)]
fn use_state() {
    let initial = yield State::get();
    println!("initial value: {}", initial);
    yield State::put(initial + 5);
}
