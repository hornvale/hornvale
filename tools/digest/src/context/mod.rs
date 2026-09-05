//! Validation and deterministic rendering for freshly collected context.

mod compose;

pub use compose::{CheckoutContext, ContextReport, compose};

mod collect;
mod discovery;
mod process;

pub use collect::context_from_current_checkout;
