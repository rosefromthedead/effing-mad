//! Standard effects and functions to make them useful.

pub mod future;
#[cfg(feature = "nondet")]
pub mod nondet;

#[cfg(feature = "nondet")]
pub use nondet::{run_nondet, Nondet};
