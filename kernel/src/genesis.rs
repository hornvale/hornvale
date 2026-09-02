//! Shared genesis vocabulary (decision 0517 clause (a)): the pin-refusal
//! error and the outcome envelope that every pin-driven genesis speaks.
//! `GenesisError` was defined character-for-character identically in
//! `domains/astronomy` and `domains/terrain` — a forced duplicate, since
//! neither may import the other (decision 0002). The kernel holds the type;
//! each domain keeps its own pins, validation, and generation (the
//! roster/meaning split, decision 0216).
//!
//! Genesis itself is deliberately NOT a `Domain` trait member — its inputs
//! are domain-specific composition work (Constitution §2.6; see
//! `kernel/src/domain.rs`). These are shared *types*, not a shared
//! mechanism.

/// Why genesis refused to produce its artifact.
/// type-audit: bare-ok(identifier-text: InvalidPin.pin), bare-ok(prose: InvalidPin.reason), bare-ok(identifier-text: UnsatisfiablePin.pin), bare-ok(prose: UnsatisfiablePin.reason)
#[derive(Debug, Clone, PartialEq)]
pub enum GenesisError {
    /// A pin's value is outside its legal range.
    InvalidPin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The rule it violates.
        reason: String,
    },
    /// A legal pin has no physically consistent solution under the model.
    UnsatisfiablePin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The physical conflict.
        reason: String,
    },
}

impl std::fmt::Display for GenesisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenesisError::InvalidPin { pin, reason } => {
                write!(f, "invalid pin '{pin}': {reason}")
            }
            GenesisError::UnsatisfiablePin { pin, reason } => {
                write!(f, "unsatisfiable pin '{pin}': {reason}")
            }
        }
    }
}

impl std::error::Error for GenesisError {}

/// What genesis produced: the artifact plus the degradation notes it
/// recorded along the way (empty when genesis was untroubled). The notes
/// become genesis-note facts at the composition root.
/// type-audit: bare-ok(prose: notes)
#[derive(Debug, Clone, PartialEq)]
pub struct GenesisOutcome<T> {
    /// The generated artifact (a star system, a tectonic globe, ...).
    pub value: T,
    /// Human-readable degradation records.
    pub notes: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The Display strings are a CLI-facing spelling contract — both domains
    /// printed exactly these before the move.
    #[test]
    fn display_spellings_are_preserved() {
        let invalid = GenesisError::InvalidPin {
            pin: "moons".to_string(),
            reason: "must be small".to_string(),
        };
        assert_eq!(invalid.to_string(), "invalid pin 'moons': must be small");
        let unsat = GenesisError::UnsatisfiablePin {
            pin: "sky".to_string(),
            reason: "no such star".to_string(),
        };
        assert_eq!(unsat.to_string(), "unsatisfiable pin 'sky': no such star");
    }
}
