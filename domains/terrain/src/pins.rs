//! Tectonic scenario pins: parameters supplied instead of drawn. Downstream
//! generation conditions on pinned values identically to drawn ones;
//! out-of-range or unsatisfiable pins fail loudly.

/// The scenario pins for tectonic genesis. Every field: `None` = drawn from
/// the seed; `Some` = supplied by the experimenter and conditioned on.
/// type-audit: bare-ok(count: plates), bare-ok(ratio: ocean_fraction), bare-ok(flag: supercontinent), bare-ok(count: globe_level), bare-ok(count: continents)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct TerrainPins {
    /// Plate count (legal 2–64); drawn 8–40 when `None`.
    pub plates: Option<u32>,
    /// Target ocean fraction (legal 0.05–0.95); drawn 0.5–0.75 when `None`.
    pub ocean_fraction: Option<f64>,
    /// `Some(true)` clusters the drawn cratons (see `continents`) toward
    /// craton 0 into one landmass; `Some(false)` explicitly re-affirms the
    /// drawn (scattered) layout. Structural — no drawn counterpart, so it
    /// is never metered in genesis notes.
    pub supercontinent: Option<bool>,
    /// Canonical geodesic grid level (legal 4–7); the crate default
    /// `GLOBE_LEVEL` when `None`. Part of world identity: the same seed at
    /// a different level is deliberately a different world (spec §5).
    pub globe_level: Option<u32>,
    /// Craton count (legal 1–16); drawn 6–11 (budget-scaled) when `None`.
    pub continents: Option<u32>,
}

/// Why tectonic genesis refused to produce a globe.
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

/// Check every pinned value against its legal range. Called once at the top
/// of `generate`; pins constructed directly (not via `parse_pin`) are still
/// validated here.
pub(crate) fn validate(pins: &TerrainPins) -> Result<(), GenesisError> {
    if let Some(n) = pins.plates
        && !(2..=64).contains(&n)
    {
        return Err(GenesisError::InvalidPin {
            pin: "plates".to_string(),
            reason: format!("{n} plates requested; the legal range is 2-64"),
        });
    }
    if let Some(f) = pins.ocean_fraction
        && (!f.is_finite() || !(0.05..=0.95).contains(&f))
    {
        return Err(GenesisError::InvalidPin {
            pin: "ocean-fraction".to_string(),
            reason: format!("{f} requested; the legal range is 0.05-0.95"),
        });
    }
    if let Some(level) = pins.globe_level
        && !(4..=7).contains(&level)
    {
        return Err(GenesisError::InvalidPin {
            pin: "globe-level".to_string(),
            reason: format!("globe level {level} outside legal 4-7"),
        });
    }
    if let Some(n) = pins.continents
        && !(1..=16).contains(&n)
    {
        return Err(GenesisError::InvalidPin {
            pin: "continents".to_string(),
            reason: format!("craton count {n} outside legal 1-16"),
        });
    }
    Ok(())
}

/// Render every pinned field as a round-trippable `key=value` string.
/// Unpinned (`None`) fields emit nothing.
/// type-audit: bare-ok(identifier-text)
pub fn pin_strings(pins: &TerrainPins) -> Vec<String> {
    let mut out = Vec::new();
    if let Some(n) = pins.plates {
        out.push(format!("plates={n}"));
    }
    if let Some(f) = pins.ocean_fraction {
        out.push(format!("ocean-fraction={f}"));
    }
    if let Some(s) = pins.supercontinent {
        out.push(format!("supercontinent={s}"));
    }
    if let Some(level) = pins.globe_level {
        out.push(format!("globe-level={level}"));
    }
    if let Some(n) = pins.continents {
        out.push(format!("continents={n}"));
    }
    out
}

/// Parse one `key=value` pin string (as produced by `pin_strings`) into
/// `pins`, overwriting whichever field it names. Unknown keys or malformed
/// values are user-facing errors naming the offending key/value.
/// type-audit: bare-ok(identifier-text: s), bare-ok(prose: return)
pub fn parse_pin(s: &str, pins: &mut TerrainPins) -> Result<(), String> {
    let (key, value) = s
        .split_once('=')
        .ok_or_else(|| format!("malformed pin '{s}': expected key=value"))?;
    match key {
        "plates" => {
            let n: u32 = value
                .parse()
                .map_err(|_| format!("plates: invalid count '{value}'"))?;
            pins.plates = Some(n);
        }
        "ocean-fraction" => {
            let f: f64 = value
                .parse()
                .map_err(|_| format!("ocean-fraction: invalid number '{value}'"))?;
            pins.ocean_fraction = Some(f);
        }
        "supercontinent" => {
            pins.supercontinent = Some(match value {
                "true" => true,
                "false" => false,
                other => {
                    return Err(format!(
                        "supercontinent: expected true or false, got '{other}'"
                    ));
                }
            });
        }
        "globe-level" => {
            let n: u32 = value
                .parse()
                .map_err(|_| format!("globe-level: invalid level '{value}'"))?;
            pins.globe_level = Some(n);
        }
        "continents" => {
            let n: u32 = value
                .parse()
                .map_err(|_| format!("continents: invalid count '{value}'"))?;
            pins.continents = Some(n);
        }
        other => return Err(format!("unknown terrain pin key '{other}'")),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_pins_pin_nothing() {
        assert!(pin_strings(&TerrainPins::default()).is_empty());
    }

    #[test]
    fn pin_strings_round_trip_through_parse_pin() {
        let pins = TerrainPins {
            plates: Some(12),
            ocean_fraction: Some(0.65),
            supercontinent: Some(true),
            globe_level: Some(6),
            continents: Some(5),
        };
        let mut rebuilt = TerrainPins::default();
        for s in pin_strings(&pins) {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
    }

    #[test]
    fn unknown_keys_and_bad_values_are_user_facing_errors() {
        let mut pins = TerrainPins::default();
        assert!(
            parse_pin("plates", &mut pins)
                .unwrap_err()
                .contains("key=value")
        );
        assert!(
            parse_pin("volcanoes=9", &mut pins)
                .unwrap_err()
                .contains("unknown")
        );
        assert!(
            parse_pin("plates=many", &mut pins)
                .unwrap_err()
                .contains("invalid")
        );
        assert!(
            parse_pin("supercontinent=maybe", &mut pins)
                .unwrap_err()
                .contains("true or false")
        );
    }

    #[test]
    fn globe_level_pin_validates_its_range() {
        for bad in [0u32, 3, 8, 99] {
            let pins = TerrainPins {
                globe_level: Some(bad),
                ..TerrainPins::default()
            };
            assert!(matches!(
                validate(&pins),
                Err(GenesisError::InvalidPin { pin, .. }) if pin == "globe-level"
            ));
        }
        for good in [4u32, 5, 6, 7] {
            let pins = TerrainPins {
                globe_level: Some(good),
                ..TerrainPins::default()
            };
            assert!(validate(&pins).is_ok(), "level {good} should be legal");
        }
    }

    #[test]
    fn continents_pin_validates_its_range() {
        for bad in [0u32, 17, 99] {
            let pins = TerrainPins {
                continents: Some(bad),
                ..TerrainPins::default()
            };
            assert!(matches!(
                validate(&pins),
                Err(GenesisError::InvalidPin { pin, .. }) if pin == "continents"
            ));
        }
        for good in [1u32, 5, 11, 16] {
            let pins = TerrainPins {
                continents: Some(good),
                ..TerrainPins::default()
            };
            assert!(validate(&pins).is_ok(), "count {good} should be legal");
        }
    }

    #[test]
    fn globe_level_pin_parses() {
        let mut pins = TerrainPins::default();
        parse_pin("globe-level=6", &mut pins).expect("parses");
        assert_eq!(pins.globe_level, Some(6));
        assert!(parse_pin("globe-level=9", &mut pins).is_err() || validate(&pins).is_err());
    }

    #[test]
    fn validate_rejects_out_of_range_pins_with_the_physical_reason() {
        let bad_plates = TerrainPins {
            plates: Some(1),
            ..TerrainPins::default()
        };
        assert!(matches!(
            validate(&bad_plates),
            Err(GenesisError::InvalidPin { .. })
        ));
        let bad_ocean = TerrainPins {
            ocean_fraction: Some(1.5),
            ..TerrainPins::default()
        };
        assert!(matches!(
            validate(&bad_ocean),
            Err(GenesisError::InvalidPin { .. })
        ));
        assert!(validate(&TerrainPins::default()).is_ok());
    }
}
