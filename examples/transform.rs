#![feature(generators)]
#![feature(generator_trait)]

use core::ops::ControlFlow;

use effing_mad::{effectful, handle, run, transform, Effect};

fn main() {
    let work = take_over_the_world();
    // Log, Lunchtime -> Print, Lunchtime
    let transformed = transform(work, print_log);
    // Print, Lunchtime -> Print
    let transformed = transform(transformed, print_lunchtime);
    let handled = handle(transformed, |Print(message)| {
        println!("{message}");
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
    yield Log("I intend to take over the world!".into(), 5);
    yield Log("I'm off to get lunch first though".into(), 0);
    yield Lunchtime;
    yield Log("They're out of sausage rolls at the bakery!".into(), 100);
}

#[effectful(Print)]
fn print_log(Log(message, importance): Log) {
    yield Print(format!("log (importance {importance}): {message}"));
}

#[effectful(Print)]
fn print_lunchtime(Lunchtime: Lunchtime) {
    yield Print("lunchtime: in progress...".into());
    std::thread::sleep(std::time::Duration::from_secs(3));
    yield Print("lunchtime: failed!".into());
}
