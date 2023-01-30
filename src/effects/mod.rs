//! Standard effects and functions to make them useful.

pub use future::{EffExt, FutureExt};

pub mod future;
#[cfg(feature = "nondet")]
pub mod nondet;
