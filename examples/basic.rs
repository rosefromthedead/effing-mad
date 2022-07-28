#![feature(generators)]
#![feature(generator_trait)]

use core::ops::ControlFlow;

use effing_mad::{effectful, handle};

fn main() {
    let task = handle(foo(), |Cancel| ControlFlow::Break(()));
    effing_mad::unlift(task);
}

struct Cancel;

impl effing_mad::Effect for Cancel {
    type Injection = effing_mad::Never;
}

#[effectful(Cancel)]
fn foo() {
    println!("starting...");
    println!("something went wrong! aah!");
    yield Cancel;
    println!("no, sorry. i have gone home.");
}
