//! Tectonic scenario pins: parameters supplied instead of drawn. Downstream
//! generation conditions on pinned values identically to drawn ones;
//! out-of-range or unsatisfiable pins fail loudly.

/// A world's metaphysical tier — the gate every reserved fantasy overlay on
/// the terrain substrate sits behind (The Ground, spec §8; `UNI-2`).
///
/// Named for the **gate**, not for the first axis it admits: `thaumic`
/// saturation is one overlay, and `domains/terrain/src/features.rs` already
/// reserves a sibling behind the same gate ("magical ores are
/// metaphysics-gated and stay reserved").
///
/// Coarse constrains fine: the mundane substrate Hornvale ships *is* the
/// charged tier's floor, so a charged world refines the inert one and never
/// contradicts it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Metaphysics {
    /// No magic in the physics. Every metaphysics-gated axis reads its
    /// reserved floor — `thaumic` is identically zero. The default, and the
    /// only tier an unpinned world has ever had.
    Inert,
    /// Thaumic saturation is a real field: charged ground exists, derived
    /// from the faults, hotspots and deep time terrain already owns (see
    /// [`crate::lithology::thaumic_at`]).
    Thaumic,
}

impl Metaphysics {
    /// Whether this tier admits the metaphysics-gated overlays at all.
    /// False for [`Metaphysics::Inert`], which is the reserved floor.
    /// type-audit: bare-ok(flag: return)
    pub fn is_charged(self) -> bool {
        matches!(self, Metaphysics::Thaumic)
    }
}

/// The scenario pins for tectonic genesis. Every field: `None` = drawn from
/// the seed; `Some` = supplied by the experimenter and conditioned on.
/// type-audit: bare-ok(count: plates), bare-ok(ratio: ocean_fraction), bare-ok(flag: supercontinent), bare-ok(count: globe_level), bare-ok(count: continents)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct TerrainPins {
    /// Plate count (legal 2–64); drawn 8–40 when `None`.
    pub plates: Option<u32>,
    /// Target ocean fraction (legal 0.05–0.95); drawn 0.5–0.75 when `None`.
    pub ocean_fraction: Option<f64>,
    /// `Some(true)` holds the world at its pre-breakup assembly — a sutured
    /// supercontinent whose rift seams are drawn but never opened (each
    /// major craton's center is replaced by its `rift.assembly` position at
    /// genesis, rift-and-fit spec §4); `Some(false)` re-affirms the drawn
    /// (displaced) layout. Structural — no drawn counterpart, so it is never
    /// metered in genesis notes.
    pub supercontinent: Option<bool>,
    /// Canonical geodesic grid level (legal 4–7); the crate default
    /// `GLOBE_LEVEL` when `None`. Part of world identity: the same seed at
    /// a different level is deliberately a different world (spec §5).
    pub globe_level: Option<u32>,
    /// Craton count (legal 1–16); drawn 8–14 (ocean-fraction-budget-scaled,
    /// Task 9 iteration 3') when `None`.
    pub continents: Option<u32>,
    /// The world's metaphysical tier (The Ground, spec §8). **Structural,
    /// exactly like `supercontinent`: it has no drawn counterpart**, so
    /// `None` means [`Metaphysics::Inert`] rather than "drawn from the seed",
    /// it consumes no stream, and it is never metered in genesis notes.
    /// `Some(Metaphysics::Inert)` re-affirms the default; the tier is
    /// resolved once at the top of [`crate::generate`] and carried on the
    /// globe so every metaphysics-gated derivation reads the same answer.
    pub metaphysics: Option<Metaphysics>,
}

pub use hornvale_kernel::genesis::GenesisError;

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
    if let Some(m) = pins.metaphysics {
        out.push(format!("metaphysics={}", metaphysics_key(m)));
    }
    out
}

/// The round-trippable spelling of a metaphysical tier, as `pin_strings`
/// emits it and `parse_pin` accepts it.
/// type-audit: bare-ok(identifier-text)
fn metaphysics_key(m: Metaphysics) -> &'static str {
    match m {
        Metaphysics::Inert => "inert",
        Metaphysics::Thaumic => "thaumic",
    }
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
        "metaphysics" => {
            pins.metaphysics = Some(match value {
                "inert" => Metaphysics::Inert,
                "thaumic" => Metaphysics::Thaumic,
                other => {
                    return Err(format!(
                        "metaphysics: expected inert or thaumic, got '{other}'"
                    ));
                }
            });
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

    /// claim: invariant(forall pin-strings; false-positive seed-loop flag — the
    /// loop's `s` binds a pin string, not a seed)
    #[test]
    fn pin_strings_round_trip_through_parse_pin() {
        let pins = TerrainPins {
            plates: Some(12),
            ocean_fraction: Some(0.65),
            supercontinent: Some(true),
            globe_level: Some(6),
            continents: Some(5),
            metaphysics: Some(Metaphysics::Thaumic),
        };
        let mut rebuilt = TerrainPins::default();
        for s in pin_strings(&pins) {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
    }

    /// The whole point of the gate: an unpinned world is metaphysically
    /// inert, so every metaphysics-gated overlay reads its reserved floor
    /// and no existing world moves. Both halves are asserted — the pin is
    /// absent, and the resolution of that absence is `Inert`.
    #[test]
    fn the_default_world_is_metaphysically_inert() {
        let pins = TerrainPins::default();
        assert!(
            pins.metaphysics.is_none(),
            "an unpinned world must be inert"
        );
        assert_eq!(
            pins.metaphysics.unwrap_or(Metaphysics::Inert),
            Metaphysics::Inert
        );
        assert!(!Metaphysics::Inert.is_charged());
        assert!(Metaphysics::Thaumic.is_charged());
    }

    /// A pinned tier must survive the `pin_strings` -> `scenario-pin` fact ->
    /// `parse_pin` round trip `windows/worldgen` rebuilds a world through;
    /// without this arm a pinned world would rebuild from its own ledger as
    /// an inert one.
    #[test]
    fn metaphysics_round_trips_through_its_pin_string() {
        for tier in [Metaphysics::Inert, Metaphysics::Thaumic] {
            let pins = TerrainPins {
                metaphysics: Some(tier),
                ..TerrainPins::default()
            };
            let strings = pin_strings(&pins);
            assert_eq!(strings.len(), 1, "one pinned field, one string");
            let mut rebuilt = TerrainPins::default();
            parse_pin(&strings[0], &mut rebuilt).unwrap();
            assert_eq!(rebuilt.metaphysics, Some(tier));
        }
        assert!(
            parse_pin("metaphysics=arcane", &mut TerrainPins::default())
                .unwrap_err()
                .contains("inert or thaumic")
        );
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
