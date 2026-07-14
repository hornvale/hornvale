//! Settlement scenario pins: a parameter supplied instead of drawn, mirroring
//! terrain's `TerrainPins` (`hornvale_terrain::pins`). Settlements now
//! condense off the demography carrying-capacity field
//! (`hornvale_demography`), so the old `min-suitability` placement floor is
//! retired; only the species restriction remains.

/// The scenario pins for settlement placement. `None` = drawn default;
/// `Some` = supplied by the experimenter and conditioned on.
/// type-audit: bare-ok(identifier-text: species)
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SettlementPins {
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
    fn species_pin_round_trips_through_parse_pin() {
        let pins = SettlementPins {
            species: Some("kobold".into()),
        };
        let mut rebuilt = SettlementPins::default();
        for s in pins.pin_strings() {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
    }

    #[test]
    fn unknown_keys_are_user_facing_errors() {
        let mut pins = SettlementPins::default();
        assert!(
            parse_pin("species", &mut pins)
                .unwrap_err()
                .contains("key=value")
        );
        assert!(
            parse_pin("population=9", &mut pins)
                .unwrap_err()
                .contains("unknown")
        );
        // The retired `min-suitability` pin is now just an unknown key: a
        // legacy value is rejected as such, never panics.
        assert!(
            parse_pin("min-suitability=0.5", &mut pins)
                .unwrap_err()
                .contains("unknown")
        );
    }
}
