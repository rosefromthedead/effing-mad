#![feature(generators)]
#![feature(generator_trait)]

use core::ops::ControlFlow;

use effing_mad::{effectful, handle, run, transform, Effect};
use frunk::Coprod;

fn main() {
    let work = take_over_the_world();
    // At this point we are starting to hit the limits of what is reasonably possible without
    // compiler support. The compiler has to infer a set of effects that is a superset of both the
    // original effect set and the effect set of the handler, but since there are infinite
    // possibilities when our only bounds are "is a superset of X", it can't infer it. So we have
    // to annotate it, which sadly means we also have to annotate the 21 other type arguments...
    // :(
    #[rustfmt::skip]
    let transformed = transform::<_, _, _, _, _, _, _, Coprod!(Print, Lunchtime), _, _, _, _, _, _,
                                  _, _, _, _, _, _, _, _>(work, print_log);
    let handled = handle(transformed, |Print(message)| {
        println!("{message}");
        ControlFlow::Continue(())
    });
    let handled = handle(handled, |Lunchtime| {
        std::thread::sleep(std::time::Duration::from_secs(3));
        ControlFlow::Continue(())
    });
    run(handled);
}

struct Print(String);
impl Effect for Print {
    type Injection = ();
}

struct Log(String, i32);
impl Effect for Log {
    type Injection = ();
}

struct Lunchtime;
impl Effect for Lunchtime {
    type Injection = ();
}

// I am running out of inspiration for the functions in these examples
#[effectful(Log, Lunchtime)]
fn take_over_the_world() {
    yield Log("I intend to take over the world".into(), 5);
    yield Log("I'm off to get lunch first though".into(), 0);
    yield Lunchtime;
    yield Log("They're out of sausage rolls at the bakery!".into(), 100);
}

#[effectful(Print)]
fn print_log(Log(message, importance): Log) {
    yield Print(format!("log (importance {importance}): {message}"));
}
