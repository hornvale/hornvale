//! The Focalizer seam interface: render a vantage as prose. Tier 0 is one
//! honest templated passage from real data. The examine contract: every
//! noun the prose mentions is in `nouns`, and only those are examinable.

use crate::Vantage;

/// A rendered vantage: prose plus its noun catalog.
/// type-audit: bare-ok(prose: prose), bare-ok(artifact: nouns)
#[derive(Debug, Clone, PartialEq)]
pub struct Focalized {
    /// The passage `look` prints.
    pub prose: String,
    /// (noun, datum) pairs — the examinable catalog, in prose order.
    pub nouns: Vec<(String, String)>,
}

/// Render a vantage as prose.
pub trait Focalizer {
    /// One focalized rendering of the vantage.
    fn render(&self, vantage: &Vantage) -> Focalized;
}

/// Capitalize the first character, leaving the rest alone — the biome noun now
/// opens the sentence.
/// type-audit: bare-ok(prose: s), bare-ok(prose: return)
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

/// Tier 0: one templated passage. Repetitive across rooms by design — The
/// Uncommon Ground buys variety and absorbs into this surface.
pub struct TemplateFocalizer;

impl Focalizer for TemplateFocalizer {
    fn render(&self, v: &Vantage) -> Focalized {
        let biome = v.locale.biome.clone();
        let descriptor = v.locale.regime.descriptor.clone();
        let village = v.village.name.clone();
        let sky_noun = "sky".to_string();
        // A walker does not STAND in the sea. The verb follows the medium, and
        // the water column distinguishes floating on the surface from hanging
        // in the water below it — the same category error The Shoal fixed for
        // the descriptors, one clause up.
        // The narrator does not say what the occupant's body is doing.
        //
        // "You stand in coral reef" asserted a posture nothing had computed:
        // the renderer knows the medium and the band, and knows nothing about
        // legs, fins, wings, boats, or sleep. Adding "swim" and "walk the
        // floor" would only have made the unsourced claim more specific. So
        // the description describes the PLACE — the convention tabletop
        // read-aloud text arrived at for the same reason, that the body
        // belongs to whoever owns it.
        //
        // A sourced stance is still possible later; it wants the liveness
        // layer to supply a real activity, and this seam is where it would go.
        // On the surface, the depth zone beneath is not where the observer
        // is: the sea's own name for that place is simply the open water.
        let named = if v.locale.biome_kind.is_marine() && !v.submerged {
            "open water".to_string()
        } else {
            biome.clone()
        };
        let place = capitalize_first(&named);
        let prose = format!(
            "{place} — {descriptor} — in the lands of {village}. The {sky_noun} above: {}",
            v.sky
        );
        let nouns = vec![
            (
                biome,
                format!(
                    "{:.1} °C the year round, moisture {:.2}, {:.0} m elevation.",
                    v.locale.fields.temperature_c,
                    v.locale.fields.moisture,
                    v.locale.fields.elevation_m
                ),
            ),
            (
                descriptor,
                format!(
                    "The ground here: {} (strangeness {:.0}).",
                    v.locale.regime.descriptor, v.locale.regime.strangeness
                ),
            ),
            (
                village,
                format!("{} souls call it home.", v.village.population),
            ),
            (sky_noun, v.sky.clone()),
        ];
        Focalized { prose, nouns }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{mint_flagship, observable};
    use hornvale_kernel::{Seed, World, WorldTime};
    use hornvale_locale::LocaleContext;
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn seam_world() -> World {
        build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    fn vantage_at(day: f64) -> Vantage {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let agent = mint_flagship(&world, &ctx).unwrap();
        observable(&world, &ctx, &agent, WorldTime { day }).unwrap()
    }

    #[test]
    fn every_noun_appears_in_the_prose() {
        let f = TemplateFocalizer.render(&vantage_at(0.0));
        assert!(!f.prose.is_empty());
        let prose = f.prose.to_lowercase();
        for (noun, detail) in &f.nouns {
            assert!(
                prose.contains(&noun.to_lowercase()),
                "noun '{noun}' must be mentioned by look"
            );
            assert!(!detail.is_empty(), "noun '{noun}' must have a datum");
        }
    }

    #[test]
    fn the_focalization_is_deterministic() {
        let a = TemplateFocalizer.render(&vantage_at(0.0));
        let b = TemplateFocalizer.render(&vantage_at(0.0));
        assert_eq!(a.prose, b.prose);
        assert_eq!(a.nouns, b.nouns);
    }

    #[test]
    fn the_day_threads_through_to_the_sky() {
        let v = vantage_at(120.0);
        let f = TemplateFocalizer.render(&v);
        let sky = f
            .nouns
            .iter()
            .find(|(n, _)| n == "sky")
            .expect("sky is a noun");
        assert_eq!(sky.1, v.sky, "the sky noun carries the day's sky report");
    }
}
