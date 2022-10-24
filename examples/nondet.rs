#![feature(generators)]
#![feature(generator_trait)]
#![feature(generator_clone)]

fn main() {
    #[cfg(feature = "nondet")]
    example::main();

    #[cfg(not(feature = "nondet"))]
    eprintln!("this example requires the `nondet` feature to be enabled.");
    #[cfg(not(feature = "nondet"))]
    eprintln!("you can run it using `cargo run --example nondet --features nondet`");
}

#[cfg(feature = "nondet")]
mod example {
    use effing_mad::effectful;
    use effing_mad::effects::{run_nondet, Nondet};

    pub fn main() {
        println!("{:?}", run_nondet(send_more_money()));
    }

    // Halt this walk of the computation if pred is false
    #[effectful(Nondet<u8>)]
    #[effectful::cloneable]
    fn guard(pred: bool) {
        if !pred {
            yield Nondet(Vec::new());
        }
    }

    #[effectful(Nondet<u8>)]
    #[effectful::cloneable]
    fn send_more_money() -> [u8; 8] {
        let mut digits: Nondet<u8> = Nondet((0..=9).collect());
        let s = yield digits.clone();
        digits.0.retain(|&digit| digit != s);
        let e = yield digits.clone();
        digits.0.retain(|&digit| digit != e);
        let n = yield digits.clone();
        digits.0.retain(|&digit| digit != n);
        let d = yield digits.clone();
        digits.0.retain(|&digit| digit != d);
        let m = yield digits.clone();
        digits.0.retain(|&digit| digit != m);
        let o = yield digits.clone();
        digits.0.retain(|&digit| digit != o);
        let r = yield digits.clone();
        digits.0.retain(|&digit| digit != r);
        let y = yield digits.clone();

        guard(y == (d + e) % 10).do_;
        let c1 = (d + e) / 10;
        guard(e == (n + r + c1) % 10).do_;
        let c2 = (n + r + c1) / 10;
        guard(n == (e + o + c2) % 10).do_;
        let c3 = (e + o + c2) / 10;
        guard(o == (s + m + c3) % 10).do_;
        guard(m == (s + m + c3) / 10).do_;

        [s, e, n, d, m, o, r, y]
    }
}
