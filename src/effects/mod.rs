//! Standard effects and functions to make them useful.

pub use future::{EffExt, FutureExt};
#[cfg(feature = "alloc")]
pub use nondet::{run_nondet, Nondet};

pub mod future;
#[cfg(feature = "alloc")]
pub mod nondet;
