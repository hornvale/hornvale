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

/// Tier 0: one templated passage. Repetitive across rooms by design — The
/// Uncommon Ground buys variety and absorbs into this surface.
pub struct TemplateFocalizer;

impl Focalizer for TemplateFocalizer {
    fn render(&self, v: &Vantage) -> Focalized {
        let biome = v.locale.biome.clone();
        let aspect = v.locale.texture.aspect.clone();
        let village = v.village.name.clone();
        let sky_noun = "sky".to_string();
        let prose = format!(
            "You stand in {biome} — {aspect} — in the lands of {village}. \
             The {sky_noun} above: {}",
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
                aspect,
                format!(
                    "The ground here: {} (relief {:+.2}).",
                    v.locale.texture.aspect, v.locale.texture.relief_jitter
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
