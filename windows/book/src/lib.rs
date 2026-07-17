//! The Book window: render a world's committed classification facts as
//! Common sentences. Reads only the ledger; realizes via `domains/language`.
#![warn(missing_docs)]

use hornvale_kernel::{Value, World};
use hornvale_language::clause::{ClauseSpec, Definiteness, Frame, Number, realize_common};

/// One world's volume of The Book: the seed it was rendered from plus the
/// sentences the ledger's `is-a` facts realize.
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(prose: lines)
pub struct BookVolume {
    /// The seed that generated the world this volume renders.
    pub seed: u64,
    /// One Common sentence per rendered `is-a` fact, in ledger commit order.
    pub lines: Vec<String>,
}

/// Render a volume: one Common sentence per `is-a` fact, subject resolved to
/// its `name` (or a synthetic `Entity <id>` label when genuinely unnamed).
pub fn render_volume(world: &World) -> BookVolume {
    let mut lines = Vec::new();
    for fact in world.ledger.find(hornvale_kernel::world::IS_A) {
        let Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject = world
            .ledger
            .text_of(fact.subject, hornvale_kernel::NAME)
            .map(str::to_string)
            .unwrap_or_else(|| format!("Entity {}", fact.subject.0));
        lines.push(realize_common(&ClauseSpec {
            frame: Frame::Classify,
            subject,
            complement: kind.clone(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
        }));
    }
    BookVolume {
        seed: world.seed.0,
        lines,
    }
}

/// Predicates present in the ledger that C1's grammar cannot yet render:
/// registered predicates with at least one committed fact, excluding those
/// the grammar already covers (`is-a`), sorted and deduped.
/// type-audit: bare-ok(identifier-text)
pub fn uncovered_predicates(world: &World) -> Vec<String> {
    use std::collections::BTreeSet;
    const COVERED: &[&str] = &["is-a"];
    let mut gaps: BTreeSet<String> = BTreeSet::new();
    for predicate in world.registry.predicates() {
        let name = predicate.name.as_str();
        if !COVERED.contains(&name) && world.ledger.find(name).next().is_some() {
            gaps.insert(name.to_string());
        }
    }
    gaps.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constant(seed: u64) -> World {
        use hornvale_astronomy::SkyPins;
        use hornvale_terrain::TerrainPins;
        use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

        build_world(
            hornvale_kernel::Seed(seed),
            &SkyPins::default(),
            SkyChoice::Constant,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("constant world builds")
    }

    #[test]
    fn coverage_flags_name_as_uncovered() {
        let world = constant(1);
        let gaps = uncovered_predicates(&world);
        assert!(
            gaps.contains(&"name".to_string()),
            "name has no construction yet: {:?}",
            gaps
        );
        assert!(
            !gaps.contains(&"is-a".to_string()),
            "is-a is covered: {:?}",
            gaps
        );
    }

    #[test]
    fn volume_states_the_planet_is_a_planet() {
        use hornvale_astronomy::SkyPins;
        use hornvale_terrain::TerrainPins;
        use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

        let world = build_world(
            hornvale_kernel::Seed(1),
            &SkyPins::default(),
            SkyChoice::Constant,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 1 builds");

        let vol = render_volume(&world);
        assert!(
            vol.lines.iter().any(|l| l.ends_with(" is a planet.")),
            "the volume classifies the planet: {:?}",
            vol.lines
        );
    }
}
