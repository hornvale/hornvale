//! Simulation phase tracking.
//!
//! The phase determines what operations are allowed on each layer.
//!
//! ## Phase Lifecycle
//!
//! ```text
//! Boot --> Idle <--> Tick
//!          ^          |
//!          +----------+
//! ```
//!
//! - **Boot**: Initial setup. Schema entities can be created and modified.
//!   Transitions to Idle via `end_boot()`.
//! - **Idle**: Between ticks. Meta entities can be modified.
//!   Transitions to Tick via `begin_tick()`.
//! - **Tick**: During simulation. Only World and Execution entities mutable.
//!   Transitions to Idle via `end_tick()`.

use std::fmt;

/// The current phase of the simulation.
///
/// Phase determines which layer mutations are allowed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Phase {
    /// Initial setup phase. Schema entities can be created.
    #[default]
    Boot,
    /// Between ticks. Meta entities can be modified.
    Idle,
    /// During tick execution. Meta is frozen.
    Tick,
}

impl Phase {
    /// Get the name of this phase as a string.
    pub fn name(&self) -> &'static str {
        match self {
            Phase::Boot => "Boot",
            Phase::Idle => "Idle",
            Phase::Tick => "Tick",
        }
    }

    /// Check if this is the boot phase.
    pub fn is_boot(&self) -> bool {
        matches!(self, Phase::Boot)
    }

    /// Check if this is the idle phase.
    pub fn is_idle(&self) -> bool {
        matches!(self, Phase::Idle)
    }

    /// Check if this is the tick phase.
    pub fn is_tick(&self) -> bool {
        matches!(self, Phase::Tick)
    }
}

impl fmt::Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_phase() {
        assert_eq!(Phase::default(), Phase::Boot);
    }

    #[test]
    fn test_phase_predicates() {
        assert!(Phase::Boot.is_boot());
        assert!(!Phase::Boot.is_idle());
        assert!(!Phase::Boot.is_tick());

        assert!(!Phase::Idle.is_boot());
        assert!(Phase::Idle.is_idle());
        assert!(!Phase::Idle.is_tick());

        assert!(!Phase::Tick.is_boot());
        assert!(!Phase::Tick.is_idle());
        assert!(Phase::Tick.is_tick());
    }
}
