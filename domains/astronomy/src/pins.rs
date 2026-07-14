//! Scenario pins: parameters supplied instead of drawn (spec §2.2).
//! Downstream generation conditions on pinned values identically to drawn
//! ones; unsatisfiable pins fail loudly — the seed is the world's identity,
//! never retried (decision 0007).

use crate::units::{Degrees, LocalDays};

/// A graded moon request: `min` essential, `want` desired (spec §4).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MoonsPin {
    min: u32,
    want: u32,
}

impl MoonsPin {
    /// Exactly `n` moons: loud failure below n.
    /// type-audit: bare-ok(count)
    pub fn exact(n: u32) -> Result<MoonsPin, GenesisError> {
        MoonsPin::graded(n, 0)
    }
    /// `min` essential plus up to `extra` desired.
    /// type-audit: bare-ok(count)
    pub fn graded(min: u32, extra: u32) -> Result<MoonsPin, GenesisError> {
        let Some(want) = min.checked_add(extra) else {
            return Err(GenesisError::InvalidPin {
                pin: "moons".to_string(),
                reason: format!("{min}+{extra} moons requested; the legal maximum is 3"),
            });
        };
        if want > 3 {
            return Err(GenesisError::InvalidPin {
                pin: "moons".to_string(),
                reason: format!("{min}+{extra} moons requested; the legal maximum is 3"),
            });
        }
        Ok(MoonsPin { min, want })
    }
    /// The essential count.
    /// type-audit: bare-ok(count)
    pub fn min(&self) -> u32 {
        self.min
    }
    /// The desired count.
    /// type-audit: bare-ok(count)
    pub fn want(&self) -> u32 {
        self.want
    }
}

/// The scenario pins for sky genesis. Every field: `None` = drawn from the
/// seed; `Some` = supplied by the experimenter and conditioned on.
/// type-audit: bare-ok(count: wanderers)
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SkyPins {
    /// Moon request: exact or graded; None = drawn.
    pub moons: Option<MoonsPin>,
    /// Rotation regime of the anchor world.
    pub rotation: Option<RotationPin>,
    /// Axial tilt in degrees (0–35); 0 = no seasons.
    pub obliquity: Option<Degrees>,
    /// Year length in local days (requires a spinning world).
    pub year_local_days: Option<LocalDays>,
    /// Force one showpiece neighbor of this class.
    pub neighbor: Option<NeighborClass>,
    /// Deep-time orbital forcing; None = drawn.
    pub forcing: Option<ForcingPin>,
    /// Spin direction of a spinning world (SKY-22); None = drawn.
    /// Conflicts with a locked rotation, which has no sunrise to direct.
    pub spin: Option<SpinPin>,
    /// Wandering-planet count (0–4); None = drawn.
    pub wanderers: Option<u32>,
}

/// Pinnable rotation regimes.
/// type-audit: pending(wave-1)
#[derive(Debug, Clone, PartialEq)]
pub enum RotationPin {
    /// Drawn-length ordinary spin.
    Normal,
    /// Tidally locked: no local solar day.
    Locked,
    /// A specific day length in standard hours (4–100).
    PeriodHours(f64),
}

/// Pinnable spin directions for a spinning world (SKY-22).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpinPin {
    /// Ordinary spin: the sun rises in the east.
    Prograde,
    /// Backward spin: the sun rises in the west.
    Retrograde,
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

/// Deep-time orbital-forcing setting. `Zero` is the null control: a circular
/// orbit with no obliquity drift (all oscillation amplitudes zero).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ForcingPin {
    /// No forcing: circular orbit, fixed obliquity (the Year-3 null control).
    Zero,
}

/// Why sky genesis refused to produce a system.
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

/// Render every pinned field of `pins` as a round-trippable `key=value`
/// string (spec §8). Unpinned (`None`) fields emit nothing.
/// type-audit: bare-ok(identifier-text)
pub fn pin_strings(pins: &SkyPins) -> Vec<String> {
    let mut out = Vec::new();

    if let Some(moons) = pins.moons {
        if moons.min() == moons.want() {
            out.push(format!("moons={}", moons.want()));
        } else {
            out.push(format!(
                "moons={}+{}",
                moons.min(),
                moons.want() - moons.min()
            ));
        }
    }

    match &pins.rotation {
        Some(RotationPin::Normal) => out.push("rotation=normal".to_string()),
        Some(RotationPin::Locked) => out.push("rotation=locked".to_string()),
        Some(RotationPin::PeriodHours(hours)) => out.push(format!("day-hours={hours}")),
        None => {}
    }

    if let Some(degrees) = pins.obliquity {
        if degrees.get() == 0.0 {
            out.push("obliquity=none".to_string());
        } else {
            out.push(format!("obliquity={}", degrees.get()));
        }
    }

    if let Some(local_days) = pins.year_local_days {
        out.push(format!("year-days={}", local_days.get()));
    }

    if let Some(neighbor) = pins.neighbor {
        out.push(format!("neighbor={}", neighbor_class_name(neighbor)));
    }

    if let Some(forcing) = pins.forcing {
        match forcing {
            ForcingPin::Zero => out.push("forcing=zero".to_string()),
        }
    }

    match pins.spin {
        Some(SpinPin::Prograde) => out.push("spin=prograde".to_string()),
        Some(SpinPin::Retrograde) => out.push("spin=retrograde".to_string()),
        None => {}
    }

    if let Some(wanderers) = pins.wanderers {
        out.push(format!("wanderers={wanderers}"));
    }

    out
}

/// Parse one `key=value` pin string (as produced by `pin_strings`) into
/// `pins`, overwriting whichever field it names. Unknown keys or malformed
/// values are user-facing errors naming the offending key/value.
/// type-audit: bare-ok(identifier-text: s), bare-ok(prose: return)
pub fn parse_pin(s: &str, pins: &mut SkyPins) -> Result<(), String> {
    let (key, value) = s
        .split_once('=')
        .ok_or_else(|| format!("malformed pin '{s}': expected key=value"))?;
    match key {
        "moons" => {
            let moons = match value.split_once('+') {
                Some((min_s, extra_s)) => {
                    let min: u32 = min_s
                        .parse()
                        .map_err(|_| format!("moons: invalid min '{min_s}'"))?;
                    let extra: u32 = extra_s
                        .parse()
                        .map_err(|_| format!("moons: invalid extra '{extra_s}'"))?;
                    MoonsPin::graded(min, extra)
                }
                None => {
                    let n: u32 = value
                        .parse()
                        .map_err(|_| format!("moons: invalid count '{value}'"))?;
                    MoonsPin::exact(n)
                }
            }
            .map_err(|e| e.to_string())?;
            pins.moons = Some(moons);
        }
        "rotation" => {
            pins.rotation = Some(match value {
                "normal" => RotationPin::Normal,
                "locked" => RotationPin::Locked,
                other => return Err(format!("rotation: unknown value '{other}'")),
            });
        }
        "spin" => {
            pins.spin = Some(match value {
                "prograde" => SpinPin::Prograde,
                "retrograde" => SpinPin::Retrograde,
                other => return Err(format!("spin: unknown value '{other}'")),
            });
        }
        "day-hours" => {
            let hours: f64 = value
                .parse()
                .map_err(|_| format!("day-hours: invalid number '{value}'"))?;
            pins.rotation = Some(RotationPin::PeriodHours(hours));
        }
        "obliquity" => {
            let degrees = if value == "none" {
                0.0
            } else {
                value
                    .parse()
                    .map_err(|_| format!("obliquity: invalid number '{value}'"))?
            };
            pins.obliquity = Some(Degrees::new(degrees).map_err(|e| e.to_string())?);
        }
        "year-days" => {
            let days: f64 = value
                .parse()
                .map_err(|_| format!("year-days: invalid number '{value}'"))?;
            pins.year_local_days = Some(LocalDays::new(days).map_err(|e| e.to_string())?);
        }
        "neighbor" => {
            pins.neighbor = Some(neighbor_class_from_name(value)?);
        }
        "forcing" => {
            pins.forcing = Some(match value {
                "zero" => ForcingPin::Zero,
                other => return Err(format!("forcing: unknown value '{other}'")),
            });
        }
        "wanderers" => {
            let n: u32 = value
                .parse()
                .map_err(|_| format!("wanderers: invalid count '{value}'"))?;
            if n > 4 {
                return Err(GenesisError::InvalidPin {
                    pin: "wanderers".to_string(),
                    reason: format!("{n} wanderers requested; the legal maximum is 4"),
                }
                .to_string());
            }
            pins.wanderers = Some(n);
        }
        other => return Err(format!("unknown pin key '{other}'")),
    }
    Ok(())
}

fn neighbor_class_name(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "red-dwarf",
        NeighborClass::SunLike => "sun-like",
        NeighborClass::WhiteDwarf => "white-dwarf",
        NeighborClass::OrangeGiant => "orange-giant",
        NeighborClass::RedGiant => "red-giant",
        NeighborClass::BlueGiant => "blue-giant",
    }
}

fn neighbor_class_from_name(name: &str) -> Result<NeighborClass, String> {
    match name {
        "red-dwarf" => Ok(NeighborClass::RedDwarf),
        "sun-like" => Ok(NeighborClass::SunLike),
        "white-dwarf" => Ok(NeighborClass::WhiteDwarf),
        "orange-giant" => Ok(NeighborClass::OrangeGiant),
        "red-giant" => Ok(NeighborClass::RedGiant),
        "blue-giant" => Ok(NeighborClass::BlueGiant),
        other => Err(format!("neighbor: unknown class '{other}'")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_pins_pin_nothing() {
        let pins = SkyPins::default();
        assert!(pins.moons.is_none());
        assert!(pins.rotation.is_none());
        assert!(pins.obliquity.is_none());
        assert!(pins.year_local_days.is_none());
        assert!(pins.neighbor.is_none());
        assert!(pins.forcing.is_none());
        assert!(pins.wanderers.is_none());
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

    #[test]
    fn pin_strings_round_trip_through_parse() {
        let pins = SkyPins {
            moons: Some(MoonsPin::graded(1, 2).unwrap()),
            rotation: Some(RotationPin::Locked),
            obliquity: Some(Degrees::new(12.5).unwrap()),
            year_local_days: None,
            neighbor: Some(NeighborClass::BlueGiant),
            forcing: None,
            spin: Some(SpinPin::Retrograde),
            wanderers: Some(3),
        };
        let mut rebuilt = SkyPins::default();
        for s in pin_strings(&pins) {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
        assert!(pin_strings(&SkyPins::default()).is_empty());
    }

    #[test]
    fn pin_strings_emits_one_string_per_pinned_field() {
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(2).unwrap()),
            rotation: Some(RotationPin::Normal),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            year_local_days: Some(LocalDays::new(300.0).unwrap()),
            neighbor: Some(NeighborClass::RedGiant),
            forcing: None,
            spin: Some(SpinPin::Prograde),
            wanderers: Some(2),
        };
        let strings = pin_strings(&pins);
        assert_eq!(strings.len(), 7);
        assert!(strings.contains(&"moons=2".to_string()));
        assert!(strings.contains(&"rotation=normal".to_string()));
        assert!(strings.contains(&"obliquity=none".to_string()));
        assert!(strings.contains(&"year-days=300".to_string()));
        assert!(strings.contains(&"neighbor=red-giant".to_string()));
        assert!(strings.contains(&"spin=prograde".to_string()));
        assert!(strings.contains(&"wanderers=2".to_string()));
    }

    #[test]
    fn graded_moons_pin_round_trips() {
        let pins = SkyPins {
            moons: Some(MoonsPin::graded(1, 2).unwrap()),
            ..SkyPins::default()
        };
        assert_eq!(pin_strings(&pins), vec!["moons=1+2".to_string()]);
        let mut rebuilt = SkyPins::default();
        parse_pin("moons=1+2", &mut rebuilt).unwrap();
        assert_eq!(rebuilt.moons, pins.moons);
    }

    #[test]
    fn spin_pin_parses_and_round_trips() {
        // SKY-22: the spin direction is its own orthogonal pin.
        let mut pins = SkyPins::default();
        parse_pin("spin=retrograde", &mut pins).unwrap();
        assert_eq!(pins.spin, Some(SpinPin::Retrograde));
        parse_pin("spin=prograde", &mut pins).unwrap();
        assert_eq!(pins.spin, Some(SpinPin::Prograde));
        assert!(parse_pin("spin=widdershins", &mut pins).is_err());

        let pins = SkyPins {
            spin: Some(SpinPin::Retrograde),
            ..SkyPins::default()
        };
        let strings = pin_strings(&pins);
        assert!(strings.contains(&"spin=retrograde".to_string()));
        let mut reparsed = SkyPins::default();
        for s in &strings {
            parse_pin(s, &mut reparsed).unwrap();
        }
        assert_eq!(reparsed, pins);
    }

    #[test]
    fn day_hours_pin_sets_a_period_hours_rotation() {
        let mut pins = SkyPins::default();
        parse_pin("day-hours=30", &mut pins).unwrap();
        assert_eq!(pins.rotation, Some(RotationPin::PeriodHours(30.0)));
        assert_eq!(pin_strings(&pins), vec!["day-hours=30".to_string()]);
    }

    #[test]
    fn obliquity_numeric_value_round_trips() {
        let mut pins = SkyPins::default();
        parse_pin("obliquity=7.5", &mut pins).unwrap();
        assert_eq!(pins.obliquity, Some(Degrees::new(7.5).unwrap()));
        assert_eq!(pin_strings(&pins), vec!["obliquity=7.5".to_string()]);
    }

    #[test]
    fn every_neighbor_class_round_trips() {
        for class in [
            NeighborClass::RedDwarf,
            NeighborClass::SunLike,
            NeighborClass::WhiteDwarf,
            NeighborClass::OrangeGiant,
            NeighborClass::RedGiant,
            NeighborClass::BlueGiant,
        ] {
            let pins = SkyPins {
                neighbor: Some(class),
                ..SkyPins::default()
            };
            let mut rebuilt = SkyPins::default();
            for s in pin_strings(&pins) {
                parse_pin(&s, &mut rebuilt).unwrap();
            }
            assert_eq!(rebuilt.neighbor, Some(class));
        }
    }

    #[test]
    fn parse_pin_rejects_malformed_and_unknown_input() {
        let mut pins = SkyPins::default();
        assert!(parse_pin("no-equals-sign", &mut pins).is_err());
        assert!(parse_pin("bogus=whatever", &mut pins).is_err());
        assert!(parse_pin("rotation=sideways", &mut pins).is_err());
        assert!(parse_pin("neighbor=green-dwarf", &mut pins).is_err());
        assert!(parse_pin("moons=abc", &mut pins).is_err());
        assert!(parse_pin("day-hours=abc", &mut pins).is_err());
        assert!(parse_pin("obliquity=abc", &mut pins).is_err());
        assert!(parse_pin("year-days=abc", &mut pins).is_err());
    }

    #[test]
    fn parse_pin_propagates_constructor_validation() {
        let mut pins = SkyPins::default();
        // moons=4 exceeds the legal maximum of 3.
        assert!(parse_pin("moons=4", &mut pins).is_err());
        // Degrees::new rejects negative values.
        assert!(parse_pin("obliquity=-1", &mut pins).is_err());
        // year-days must be a non-negative finite LocalDays value.
        assert!(parse_pin("year-days=-5", &mut pins).is_err());
    }

    #[test]
    fn graded_moons_pin_rejects_overflowing_sum_instead_of_wrapping() {
        let err = MoonsPin::graded(u32::MAX, 1).unwrap_err();
        match err {
            GenesisError::InvalidPin { pin, reason } => {
                assert_eq!(pin, "moons");
                assert!(reason.contains("legal maximum"));
            }
            other => panic!("expected InvalidPin, got {other:?}"),
        }
    }

    #[test]
    fn parse_pin_rejects_moons_sum_that_overflows_u32() {
        let mut pins = SkyPins::default();
        let err = parse_pin("moons=4294967295+1", &mut pins).unwrap_err();
        assert!(err.contains("moons"), "unexpected error text: {err}");
    }

    #[test]
    fn forcing_pin_round_trips() {
        let pins = SkyPins {
            forcing: Some(ForcingPin::Zero),
            ..SkyPins::default()
        };
        let strings = pin_strings(&pins);
        assert!(strings.contains(&"forcing=zero".to_string()));
        let mut rebuilt = SkyPins::default();
        parse_pin("forcing=zero", &mut rebuilt).unwrap();
        assert_eq!(rebuilt.forcing, Some(ForcingPin::Zero));
    }

    #[test]
    fn wanderers_pin_round_trips() {
        let pins = SkyPins {
            wanderers: Some(3),
            ..SkyPins::default()
        };
        let strings = pin_strings(&pins);
        assert_eq!(strings, vec!["wanderers=3".to_string()]);
        let mut rebuilt = SkyPins::default();
        for s in &strings {
            parse_pin(s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt.wanderers, Some(3));
    }

    #[test]
    fn wanderers_pin_rejects_values_above_the_legal_maximum() {
        let mut pins = SkyPins::default();
        let err = parse_pin("wanderers=5", &mut pins).unwrap_err();
        assert!(
            err.contains("the legal maximum is 4"),
            "unexpected error text: {err}"
        );
    }
}
