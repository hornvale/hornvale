use thiserror::Error as ThisError;

/// An error type for the astronomy crate.
#[derive(ThisError, Debug)]
pub enum AstronomyError {}
