//! The explain window: narrate a world's derivation by reading its committed
//! facts and joining them against the known derivation DAG. Reads only the
//! ledger — never the in-memory system — which is how it validates that the
//! ledger is self-describing (SKY-15 / TOOL-1 fact-reading tier).
#![warn(missing_docs)]

use hornvale_astronomy::facts;
use hornvale_kernel::{EntityId, Value, World};
use hornvale_language::CommonVocabulary;

/// Locate the world entity: the unique subject carrying a `star-class` fact.
fn world_entity(world: &World) -> Option<EntityId> {
    world
        .ledger
        .find(facts::STAR_CLASS)
        .map(|f| f.subject)
        .next()
}

/// Read a functional Number fact off `subject`.
fn num(world: &World, subject: EntityId, predicate: &str) -> Option<f64> {
    match world.ledger.value_of(subject, predicate) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// Read a functional Text fact off `subject`.
fn text(world: &World, subject: EntityId, predicate: &str) -> Option<String> {
    world.ledger.text_of(subject, predicate).map(str::to_string)
}

/// Narrate the sky's derivation chain from the world's committed facts.
/// `None` if the world has no generated sky. Each node is tagged with its
/// provenance in the derivation DAG (rolled / derived / pinned) and its value
/// read from the ledger; the join of DAG and values is the explanation.
///
/// `vocab` is the world's assembled Common vocabulary
/// (`hornvale_worldgen::common_vocabulary`) — the root fills it, this window
/// receives it, and the star class's word comes through the same declared-word
/// seam every other concept's does.
/// type-audit: bare-ok(artifact: return)
pub fn explain_sky(world: &World, vocab: &CommonVocabulary) -> Option<String> {
    let e = world_entity(world)?;
    let class_concept = text(world, e, facts::STAR_CLASS)?;
    // The ledger holds the class's registered concept id; render it through
    // the author's-frame display used everywhere else this fact surfaces
    // (`windows/book`). Resolution is total, so — unlike the retired
    // `class_display` lookup this replaced — there is no unrecognized-id arm
    // to fall back to: an id with no declared word still renders as a word,
    // never as a raw registry key.
    let class = vocab.word_for(&class_concept);
    let star_mass = num(world, e, facts::STAR_MASS_SOLAR)?;
    let luminosity = num(world, e, facts::STAR_LUMINOSITY_SOLAR)?;
    let zone_in = num(world, e, facts::HAB_ZONE_INNER_AU)?;
    let zone_out = num(world, e, facts::HAB_ZONE_OUTER_AU)?;
    let anchor_mass = num(world, e, facts::ANCHOR_MASS_EARTH)?;
    let orbit = num(world, e, facts::ANCHOR_ORBIT_AU)?;
    let insolation = num(world, e, facts::INSOLATION_REL)?;

    // Was the orbit pinned? The DAG's orbit leaf is pinned iff a year-days
    // scenario-pin was committed. `pin_strings` (domains/astronomy/src/pins.rs)
    // renders that pin as "year-days={value}" — the only pin string
    // containing "year" — and anchor.rs derives the orbit from it via
    // Kepler's third law, so this branch activates under a `--year-days`
    // style pin (unexercised by seed 42's default, unpinned build).
    let pinned_orbit = world.ledger.facts_about(e).any(|f| {
        f.predicate == facts::SCENARIO_PIN
            && matches!(&f.object, Value::Text(t) if t.contains("year"))
    });
    let orbit_tag = if pinned_orbit { "pinned" } else { "rolled" };

    // Edge wording is computed from the same 2-decimal-rounded values the
    // prose displays (via {:.2}), so a reader doing mental math on the shown
    // figures never sees a midpoint call that looks inconsistent with them.
    let rounded_zone_in = (zone_in * 100.0).round() / 100.0;
    let rounded_zone_out = (zone_out * 100.0).round() / 100.0;
    let rounded_orbit = (orbit * 100.0).round() / 100.0;
    let edge = if rounded_orbit < (rounded_zone_in + rounded_zone_out) / 2.0 {
        "the warm edge"
    } else {
        "the cool edge"
    };

    // The article stays the literal `"a"` it has always been. It is wrong for
    // the two vowel-initial displays ("a orange dwarf (K)"), but that predates
    // this migration and fixing it is a prose change to a committed artifact,
    // not part of moving the lookup behind the vocabulary.
    let star_clause = format!("Its star is a {class}");

    let mut out = String::new();
    out.push_str(&format!(
        "This world receives {insolation:.2}× Earth's sunlight (insolation, global annual mean).\n"
    ));
    out.push_str(&format!(
        "{star_clause} — mass {star_mass:.2} M☉ (rolled) — giving luminosity \
         {luminosity:.2} L☉ (derived, L = M³·⁵) and a habitable zone of {zone_in:.2}–{zone_out:.2} AU \
         (derived, 0.95√L–1.37√L).\n"
    ));
    out.push_str(&format!(
        "The anchor world — mass {anchor_mass:.2} M⊕ (rolled) — orbits at {orbit:.2} AU ({orbit_tag}), \
         so insolation = {luminosity:.2} / {orbit:.2}² = {insolation:.2} (derived, L/a²), near {edge} \
         of the zone.\n"
    ));

    // Moons: the committed count fact on the world entity.
    let moon_count = match num(world, e, facts::MOON_COUNT) {
        Some(n) => n.round() as usize,
        None => world
            .ledger
            .facts_about(e)
            .filter(|f| f.predicate == facts::MOON_MASS_LUNAR)
            .count(),
    };
    if moon_count == 0 {
        out.push_str("It has no moons.\n");
    } else {
        out.push_str(&format!("It has {moon_count} moon(s) (rolled count).\n"));
    }

    // Neighbors: one entity each, discovered by the is-neighbor flag.
    let neighbor_count = world.ledger.find(facts::IS_NEIGHBOR).count();
    out.push_str(&format!(
        "{neighbor_count} notable neighbor star(s) stand fixed in its night sky (rolled).\n"
    ));

    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::pins::SkyPins;
    use hornvale_astronomy::register_concepts;
    use hornvale_astronomy::system::generate;
    use hornvale_kernel::Seed;

    fn world_with_sky(seed: u64) -> World {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
        let subject = w.ledger.mint_entity();
        facts::genesis(&mut w, subject, &outcome).unwrap();
        w
    }

    /// The composition root's assembly, reproduced over the one domain these
    /// astronomy-only fixtures register. `hornvale_worldgen::common_vocabulary`
    /// is the real thing; this window does not depend on it (it presents a
    /// single domain and stays lean), so its callers hand the vocabulary in.
    ///
    /// Keep this in step with `COMMON_WORD_SOURCES` in `windows/worldgen`: a
    /// domain added there whose words this window renders must be declared
    /// here too, or these fixtures stop matching the real assembly.
    fn vocab_for(w: &World) -> CommonVocabulary {
        let mut vocab = CommonVocabulary::build(&w.registry).expect("the registry resolves");
        for (concept, word) in hornvale_astronomy::common_words() {
            vocab.declare(concept, word);
        }
        vocab
    }

    #[test]
    fn explain_sky_narrates_the_insolation_chain_from_the_ledger() {
        let w = world_with_sky(42);
        let text = explain_sky(&w, &vocab_for(&w)).expect("a world with sky facts explains");
        assert!(text.contains("sunlight") || text.contains("insolation"));
        assert!(text.contains("luminosity"));
        let e = w
            .ledger
            .find(facts::STAR_MASS_SOLAR)
            .next()
            .unwrap()
            .subject;
        let mass = match w.ledger.value_of(e, facts::STAR_MASS_SOLAR) {
            Some(Value::Number(n)) => *n,
            _ => panic!("star mass fact present"),
        };
        assert!(
            text.contains(&format!("{mass:.2}")),
            "narration names the star mass figure {mass:.2}: {text}"
        );
        // The narration is derived purely from committed facts.
    }

    #[test]
    fn explain_sky_is_none_without_sky_facts() {
        let w = World::new(Seed(7)); // empty ledger
        assert!(explain_sky(&w, &vocab_for(&w)).is_none());
    }

    #[test]
    fn explain_sky_reads_the_ledger_only_not_the_system() {
        // A world reconstructed from JSON has no in-memory System — only facts.
        let w = world_with_sky(1);
        let json = w.to_json();
        let reloaded = World::from_json(&json).unwrap();
        assert_eq!(
            explain_sky(&w, &vocab_for(&w)),
            explain_sky(&reloaded, &vocab_for(&reloaded))
        );
    }
}
