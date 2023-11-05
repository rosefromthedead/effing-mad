//! The `effing_mad::higher_order` module contains variants of standard higher-order functions (such
//! as `Option::map`) that take effectful functions instead of "pure" ones. This allows some pretty
//! freaky control flow, like what we see here where the mapper function fails.

#![feature(coroutines)]
#![feature(coroutine_trait)]

use effing_mad::{effectful, handle, handler, higher_order::OptionExt, run, Effect, Never};

fn main() {
    let handler = handler!(Fail(msg) => {
        println!("Error: {msg}");
        break;
    });
    run(handle(precarious(), handler));
}

struct Fail(&'static str);
impl Effect for Fail {
    type Injection = Never;
}

#[effectful(Fail)]
fn precarious() {
    let null_plus_five = None.map_eff(add_5).do_;
    dbg!(null_plus_five);
    let four_plus_five = Some(4).map_eff(add_5).do_;
    dbg!(four_plus_five);
    let forty_five_plus_five = Some(45).map_eff(add_5).do_;
    dbg!(forty_five_plus_five); // psst - unreachable!
}

#[effectful(Fail)]
fn add_5(n: i32) -> i32 {
    if n == 45 {
        yield Fail("I'm scared of 45 :(");
        unreachable!()
    } else {
        n + 5
    }
}
