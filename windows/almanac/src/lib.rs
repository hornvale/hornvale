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
    /// The settlement, if any.
    pub village: Option<VillageInfo>,
    /// The settlement's castes, lowest to highest.
    pub castes: Vec<String>,
    /// Recorded beliefs.
    pub beliefs: Vec<Belief>,
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

    doc.push_str("## The Land\n\n");
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
    }
}
