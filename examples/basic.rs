#![feature(generators)]
#![feature(generator_trait)]

use core::ops::ControlFlow;

use effing_mad::{effectful, handle, Effect};

fn main() {
    let cancelled = handle(combined(), |Cancel| ControlFlow::Break(()));

    let logged = handle(cancelled, |Log(msg)| {
        println!("log: {msg}");
        ControlFlow::Continue(())
    });

    let filesystemed = handle(logged, |FileRead(path)| {
        assert_eq!(path, "~/my passwords.txt");
        ControlFlow::Continue("monadtransformerssuck".into())
    });

    effing_mad::run(filesystemed);
}

struct Cancel;

impl Effect for Cancel {
    /// Resuming an effectful function after it has cancelled is impossible.
    type Injection = effing_mad::Never;
}

struct Log(String);

impl Effect for Log {
    /// The logging handler does not provide any information back to the effectful function.
    type Injection = ();
}

struct FileRead(String);

impl Effect for FileRead {
    /// For this example, we pretend files are just strings, and the whole file is read at once.
    type Injection = String;
}

// This function demonstrates combining effects to represent both control flow and I/O, where the
// behaviour of the I/O is specified outside the function. In this case the behaviour comes from
// the closure passed to `handle()` in `main`.
// Allow unreachable code because the compiler knows that after cancelling, there is no more
// execution, so this function would otherwise cause a warning. However, this warning only comes up
// if there is a `yield` after cancelling, not if there are only normal statements and expressions.
#[allow(unreachable_code)]
#[effectful(Cancel, Log)]
fn simple() {
    yield Log("starting...".into());
    yield Log("something went wrong! aah!".into());
    yield Cancel;
    yield Log("no, sorry. i have gone home.".into());
}

// This function demonstrates how effect handlers can pass values back into the effectful function,
// and how the `do_` operator can be used to call effectful functions, as long as the callee has a
// subset of the caller's effects.
#[effectful(Cancel, Log, FileRead)]
fn combined() {
    let mischief = yield FileRead("~/my passwords.txt".into());
    yield Log(format!("I know your password! It's {mischief}"));
    yield Log("I'm going to do evil things and you can't stop me!".into());
    simple().do_;
}
