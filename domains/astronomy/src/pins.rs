//! Scenario pins: parameters supplied instead of drawn (spec §2.2).
//! Downstream generation conditions on pinned values identically to drawn
//! ones; unsatisfiable pins fail loudly.

/// The scenario pins for sky genesis. Every field: `None` = drawn from the
/// seed; `Some` = supplied by the experimenter and conditioned on.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SkyPins {
    /// Number of moons (0–3).
    pub moons: Option<u32>,
    /// Rotation regime of the anchor world.
    pub rotation: Option<RotationPin>,
    /// Axial tilt in degrees (0–35); 0 = no seasons.
    pub obliquity_deg: Option<f64>,
    /// Year length in local days (requires a spinning world).
    pub year_local_days: Option<f64>,
    /// Force one showpiece neighbor of this class.
    pub neighbor: Option<NeighborClass>,
}

/// Pinnable rotation regimes.
#[derive(Debug, Clone, PartialEq)]
pub enum RotationPin {
    /// Drawn-length ordinary spin.
    Normal,
    /// Tidally locked: no local solar day.
    Locked,
    /// A specific day length in standard hours (4–100).
    PeriodHours(f64),
}

/// Spectral classes for notable neighbor stars.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NeighborClass {
    /// Dim red dwarf.
    RedDwarf,
    /// Sun-like yellow star.
    SunLike,
    /// Dense white remnant.
    WhiteDwarf,
    /// Swollen orange giant.
    OrangeGiant,
    /// Vast red giant.
    RedGiant,
    /// Blazing blue giant.
    BlueGiant,
}

/// Why sky genesis refused to produce a system.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_pins_pin_nothing() {
        let pins = SkyPins::default();
        assert!(pins.moons.is_none());
        assert!(pins.rotation.is_none());
        assert!(pins.obliquity_deg.is_none());
        assert!(pins.year_local_days.is_none());
        assert!(pins.neighbor.is_none());
    }

    #[test]
    fn genesis_errors_carry_pin_and_reason() {
        let e = GenesisError::UnsatisfiablePin {
            pin: "year-days".to_string(),
            reason: "orbit outside habitable zone".to_string(),
        };
        let text = e.to_string();
        assert!(text.contains("year-days"));
        assert!(text.contains("habitable zone"));
    }
}
