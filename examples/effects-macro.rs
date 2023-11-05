//! A demonstration of the effects! and handler! macros, which allow you to group effects into one
//! type, and use one handler for all of them. The primary benefit of this is that this handler can
//! own or mutably borrow some data, and that data can be accessed by all of the arms of the
//! handler.
//! Here we use State as a classic (and generic!) example of an effect. The same program could just
//! be written using a mutable variable, but that's no fun.

#![feature(coroutines)]
#![feature(coroutine_trait)]

use effing_mad::{effectful, handle_group, handler, run};

fn main() {
    let mut state = 34;
    let state_handler = handler!(State<i32> {
        get() => state,
        put(v) => state = v,
    });
    let handled = handle_group(use_state(), state_handler);
    run(handled);
    println!("final value: {}", state);
}

effing_mad::effects! {
    State<T> {
        fn get() -> T;
        fn put(v: T) -> ();
    }
}

// Rust encourages immutability!
#[effectful(State<i32>)]
fn use_state() {
    let initial = yield State::get();
    println!("initial value: {}", initial);
    yield State::put(initial + 5);
}
