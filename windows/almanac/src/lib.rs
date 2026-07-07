//! The almanac window: render a world as a one-page document. Windows may
//! depend on domains (they present them); domains never depend on windows.
#![warn(missing_docs)]

use hornvale_astronomy::SkyReport;
use hornvale_climate::ClimateReport;
use hornvale_kernel::Phenomenon;
use hornvale_religion::Belief;
use hornvale_settlement::VillageInfo;
use hornvale_terrain::PlaceInfo;

/// Everything the almanac needs, gathered by the composition root.
pub struct AlmanacContext {
    /// The world seed, for the title.
    pub seed: u64,
    /// The sky at genesis.
    pub sky: SkyReport,
    /// The climate at the world's first place.
    pub climate: ClimateReport,
    /// Phenomena salient at the world's first place, salience-ranked.
    pub phenomena: Vec<Phenomenon>,
    /// Known places.
    pub places: Vec<PlaceInfo>,
    /// The tectonic globe's headline lines, from the composition root.
    pub land_lines: Vec<String>,
    /// The globe's biome/habitability headline lines, from the composition root.
    pub biome_lines: Vec<String>,
    /// The settlement, if any.
    pub village: Option<VillageInfo>,
    /// The settlement's castes, lowest to highest.
    pub castes: Vec<String>,
    /// Recorded beliefs.
    pub beliefs: Vec<Belief>,
    /// The world's cycles, reader-facing; empty for constant-sky worlds.
    pub calendar_lines: Vec<String>,
    /// The night sky as a sentence; `None` for constant-sky worlds.
    pub night_sky: Option<String>,
    /// Notes recorded during sky genesis; empty for constant-sky worlds.
    pub genesis_notes: Vec<String>,
    /// Headline lines describing the world's people: how many settlements,
    /// and the flagship's name, population, and biome.
    pub settlement_lines: Vec<String>,
}

/// Render the one-page world document as markdown. Deterministic: same
/// context, same bytes.
pub fn render(ctx: &AlmanacContext) -> String {
    let mut doc = String::new();
    doc.push_str(&format!("# The Almanac of Seed {}\n\n", ctx.seed));

    doc.push_str("## The Sky\n\n");
    doc.push_str(&format!("{}\n\n", ctx.sky.description));
    doc.push_str(&format!(
        "Visible bodies: {}.\n\n",
        ctx.sky.bodies.join(", ")
    ));

    if !ctx.phenomena.is_empty() {
        doc.push_str("Salient phenomena, most attention-demanding first:\n\n");
        for p in &ctx.phenomena {
            doc.push_str(&format!(
                "- [{:.2}] *{}* — {}\n",
                p.salience, p.kind, p.description
            ));
        }
        doc.push('\n');
    }

    if let Some(night_sky) = &ctx.night_sky {
        doc.push_str(&format!("{night_sky}\n\n"));
    }

    if !ctx.calendar_lines.is_empty() {
        doc.push_str("## The Calendar\n\n");
        for line in &ctx.calendar_lines {
            doc.push_str(&format!("- {line}\n"));
        }
        doc.push('\n');
        // Nested inside the calendar-lines check deliberately: genesis
        // notes only ever exist alongside a generated calendar (both are
        // empty for constant-sky worlds), so this stays in lockstep with
        // world_builder's calendar_lines/genesis_notes pairing.
        if !ctx.genesis_notes.is_empty() {
            doc.push_str("Notes from genesis:\n\n");
            for note in &ctx.genesis_notes {
                doc.push_str(&format!("- {note}\n"));
            }
            doc.push('\n');
        }
    }

    doc.push_str("## The Land\n\n");
    for line in &ctx.land_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.land_lines.is_empty() {
        doc.push('\n');
    }
    for line in &ctx.biome_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.biome_lines.is_empty() {
        doc.push('\n');
    }
    if ctx.places.is_empty() {
        doc.push_str("No places are known.\n\n");
    } else {
        for place in &ctx.places {
            doc.push_str(&format!("- **{}** — {}\n", place.name, place.biome));
        }
        doc.push_str(&format!(
            "\n{} ({:.0}°C)\n\n",
            ctx.climate.description, ctx.climate.temperature_c
        ));
    }

    doc.push_str("## The People\n\n");
    for line in &ctx.settlement_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.settlement_lines.is_empty() {
        doc.push('\n');
    }
    match &ctx.village {
        None => doc.push_str("No settlements are known.\n\n"),
        Some(v) => {
            doc.push_str(&format!(
                "The goblin village of **{}**, population {}.\n\n",
                v.name, v.population
            ));
            if !ctx.castes.is_empty() {
                doc.push_str(&format!(
                    "Castes, lowest to highest: {}.\n\n",
                    ctx.castes.join(", ")
                ));
            }
        }
    }

    doc.push_str("## The Gods\n\n");
    if ctx.beliefs.is_empty() {
        doc.push_str("No beliefs are recorded.\n\n");
    } else {
        for belief in &ctx.beliefs {
            doc.push_str(&format!(
                "> {}\n>\n> — a belief derived from the phenomenon *{}*\n\n",
                belief.tenet, belief.source_kind
            ));
        }
    }

    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    doc
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::EntityId;

    fn sample_context() -> AlmanacContext {
        AlmanacContext {
            seed: 42,
            sky: SkyReport {
                description: "A golden sun hangs fixed at zenith.".to_string(),
                bodies: vec!["the sun".to_string()],
            },
            climate: ClimateReport {
                temperature_c: 18.0,
                description: "Mild and temperate.".to_string(),
            },
            phenomena: vec![Phenomenon {
                kind: "celestial-body".to_string(),
                description: "a golden sun fixed at zenith".to_string(),
                period_days: None,
                salience: 1.0,
            }],
            places: vec![PlaceInfo {
                id: EntityId(1),
                name: "the Vale".to_string(),
                biome: "temperate forest".to_string(),
            }],
            land_lines: vec![
                "The globe breaks into 23 plates; the sea claims 63% of its surface.".to_string(),
            ],
            biome_lines: vec![],
            village: Some(VillageInfo {
                id: EntityId(2),
                name: "Bolnar".to_string(),
                population: 60,
                located_in: Some(EntityId(1)),
            }),
            castes: vec!["slave".to_string(), "chief".to_string()],
            beliefs: vec![Belief {
                id: EntityId(3),
                tenet: "the Ever-Flame never blinks.".to_string(),
                source_kind: "celestial-body".to_string(),
            }],
            calendar_lines: vec![],
            night_sky: None,
            genesis_notes: vec![],
            settlement_lines: vec![],
        }
    }

    #[test]
    fn render_contains_every_section_and_datum() {
        let doc = render(&sample_context());
        for expected in [
            "# The Almanac of Seed 42",
            "## The Sky",
            "zenith",
            "## The Land",
            "23 plates",
            "the Vale",
            "temperate forest",
            "## The People",
            "Bolnar",
            "60",
            "slave",
            "chief",
            "## The Gods",
            "Ever-Flame",
            "celestial-body",
            "a golden sun fixed at zenith",
            "[1.00]",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn render_is_deterministic() {
        assert_eq!(render(&sample_context()), render(&sample_context()));
    }

    #[test]
    fn empty_world_renders_honestly() {
        let ctx = AlmanacContext {
            village: None,
            castes: vec![],
            beliefs: vec![],
            places: vec![],
            ..sample_context()
        };
        let doc = render(&ctx);
        assert!(doc.contains("No settlements are known."));
        assert!(doc.contains("No beliefs are recorded."));
        assert!(!doc.contains("## The Calendar"));
    }

    #[test]
    fn calendar_section_renders_between_sky_and_land() {
        let ctx = AlmanacContext {
            calendar_lines: vec![
                "The year is 365.2 local days (365.2 standard days).".to_string(),
                "The first moon circles every 29.5 local days — 12.4 months to a year.".to_string(),
            ],
            night_sky: Some("By night: a hard blue-white star that does not wander.".to_string()),
            genesis_notes: vec!["a showpiece neighbor was forced by pin".to_string()],
            ..sample_context()
        };
        let doc = render(&ctx);

        assert!(doc.contains("## The Calendar"));
        let sky_pos = doc.find("## The Sky").unwrap();
        let calendar_pos = doc.find("## The Calendar").unwrap();
        let land_pos = doc.find("## The Land").unwrap();
        assert!(sky_pos < calendar_pos, "Calendar must come after Sky");
        assert!(calendar_pos < land_pos, "Calendar must come before Land");

        assert!(doc.contains("- The year is 365.2 local days (365.2 standard days)."));
        assert!(
            doc.contains("- The first moon circles every 29.5 local days — 12.4 months to a year.")
        );
        assert!(doc.contains("Notes from genesis:"));
        assert!(doc.contains("- a showpiece neighbor was forced by pin"));

        // The night sky reads as part of the Sky section, before Calendar.
        let night_sky_pos = doc
            .find("By night: a hard blue-white star that does not wander.")
            .unwrap();
        assert!(sky_pos < night_sky_pos && night_sky_pos < calendar_pos);
    }
}
