//! Settlement scenario pins: a parameter supplied instead of drawn, mirroring
//! terrain's `TerrainPins` (`hornvale_terrain::pins`). Settlement placement
//! itself is domain logic (`hornvale_settlement::place`); the pin type lives
//! at the composition root because it is the root that decides which floor
//! to hand `place`.

/// The scenario pins for settlement placement. `None` = drawn default;
/// `Some` = supplied by the experimenter and conditioned on.
/// type-audit: bare-ok(ratio: min_suitability), bare-ok(identifier-text: species)
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SettlementPins {
    /// Minimum suitability required to place a settlement (legal `[0, 1]`);
    /// the placement floor defaults to 0.25 when `None`. Since the founder
    /// pass (MAP-22 K=1), each placed species' single best cell is reserved
    /// BEFORE the floor applies, so a high floor caps a species at its one
    /// founder settlement but can never drive it to zero — pre-founder
    /// behaviour, where a high enough floor emptied a species entirely, is
    /// gone by design (`hornvale_settlement::place_tagged`).
    pub min_suitability: Option<f64>,
    /// Restrict the placed species set to this one species; `None` = all
    /// registry species. Validated against the species registry at build.
    pub species: Option<String>,
}

impl SettlementPins {
    /// Render every pinned field as a round-trippable `key=value` string.
    /// Unpinned (`None`) fields emit nothing.
    /// type-audit: bare-ok(identifier-text)
    pub fn pin_strings(&self) -> Vec<String> {
        let mut out = Vec::new();
        if let Some(f) = self.min_suitability {
            out.push(format!("min-suitability={f}"));
        }
        if let Some(s) = &self.species {
            out.push(format!("species={s}"));
        }
        out
    }
}

/// Parse one `key=value` pin string (as produced by
/// `SettlementPins::pin_strings`) into `pins`, overwriting whichever field it
/// names. Unknown keys or malformed values are user-facing errors naming the
/// offending key/value.
/// type-audit: bare-ok(identifier-text: s), bare-ok(prose: return)
pub fn parse_pin(s: &str, pins: &mut SettlementPins) -> Result<(), String> {
    let (key, value) = s
        .split_once('=')
        .ok_or_else(|| format!("malformed pin '{s}': expected key=value"))?;
    match key {
        "min-suitability" => {
            let f: f64 = value
                .parse()
                .map_err(|_| format!("min-suitability: invalid number '{value}'"))?;
            pins.min_suitability = Some(f);
        }
        "species" => pins.species = Some(value.to_string()),
        other => return Err(format!("unknown settlement pin key '{other}'")),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_pins_pin_nothing() {
        assert!(SettlementPins::default().pin_strings().is_empty());
    }

    #[test]
    fn pin_strings_round_trip_through_parse_pin() {
        let pins = SettlementPins {
            min_suitability: Some(0.5),
            ..SettlementPins::default()
        };
        let mut rebuilt = SettlementPins::default();
        for s in pins.pin_strings() {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
    }

    #[test]
    fn species_pin_round_trips_through_parse_pin() {
        let pins = SettlementPins {
            species: Some("kobold".into()),
            ..SettlementPins::default()
        };
        let mut rebuilt = SettlementPins::default();
        for s in pins.pin_strings() {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
    }

    #[test]
    fn unknown_keys_and_bad_values_are_user_facing_errors() {
        let mut pins = SettlementPins::default();
        assert!(
            parse_pin("min-suitability", &mut pins)
                .unwrap_err()
                .contains("key=value")
        );
        assert!(
            parse_pin("population=9", &mut pins)
                .unwrap_err()
                .contains("unknown")
        );
        assert!(
            parse_pin("min-suitability=lots", &mut pins)
                .unwrap_err()
                .contains("invalid")
        );
    }
}
