//! The Projection seam interface and its subset contract. Tier 0 is the
//! identity projection; knowledge accumulates per session (observation
//! history, metaplan §3.2). Fog, inference, false belief: The Vessel's.

use crate::Vantage;
use hornvale_book::{LineError, parse_line};
use hornvale_kernel::{RoomId, Value, World, WorldTime};
use hornvale_language::clause::ParseContext;
use hornvale_locale::LocaleContext;
use hornvale_species::PerceptionVector;
use std::collections::{BTreeMap, BTreeSet};

/// What an agent knows: key → value. Projection-derived entries (the
/// `room/<id>` and `settlement/<id>/<field>` shapes written by
/// [`Projection::project`]) carry the subset contract — every one
/// re-derivable from ground truth, checked by [`knowledge_is_subset`].
/// Heard entries (the `"{subject}::{predicate}"` shape written by
/// [`absorb_common`]) are deliberately outside that contract: `write` (the
/// Echo's `tell`, renamed at the Vessel Stitch, T2) can transfer a false
/// belief into a listener's knowledge, and heard ≠ true is the epistemic
/// point (The Echo, spec §6).
/// type-audit: bare-ok(artifact: 0)
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Knowledge(pub BTreeMap<String, String>);

impl Knowledge {
    /// Fold another projection into this knowledge (walking accumulates).
    pub fn absorb(&mut self, other: Knowledge) {
        self.0.extend(other.0);
    }
}

/// The knowledge-transfer seam (The Echo T4): absorb one Common sentence
/// into `knowledge`, spoken-to-heard. Parses `line` against `ctx` via T3's
/// `parse_line`, then records exactly what the sentence surfaced — the
/// transfer law (spec §6): what transits is what the sentence says, no
/// more, no less. Records `"{subject}::is-a"` → the recovered
/// classification, then one `"{subject}::{predicate}"` → surface-value
/// entry per aggregated fact the sentence carried (moons, star, day
/// length, …). A number's surface text matches how the sentence carried
/// it: a whole count prints bare (`"2"`), a fractional quantity keeps its
/// digits (`"1.5"`) — this mirrors `domains/language`'s `quantity`
/// rendering closely enough that a listener's belief reads naturally
/// alongside the sentence it came from. Returns the number of entries
/// absorbed (`1 + parsed.facts.len()`).
/// type-audit: bare-ok(prose: line), bare-ok(count: return)
pub fn absorb_common(
    knowledge: &mut Knowledge,
    line: &str,
    ctx: &ParseContext,
) -> Result<usize, LineError> {
    let parsed = parse_line(line, ctx)?;
    let subject = &parsed.subject;
    knowledge
        .0
        .insert(format!("{subject}::is-a"), parsed.kind.clone());
    let mut absorbed = 1;
    for (predicate, value) in &parsed.facts {
        let surface = match value {
            Value::Text(t) => t.clone(),
            Value::Number(n) if n.fract() == 0.0 => format!("{n:.0}"),
            Value::Number(n) => format!("{n}"),
            Value::Entity(id) => id.0.to_string(),
            Value::Flag(b) => b.to_string(),
        };
        knowledge
            .0
            .insert(format!("{subject}::{predicate}"), surface);
        absorbed += 1;
    }
    Ok(absorbed)
}

/// The Vessel Stitch's Knowledge→reader-set adapter (T2): every heard key
/// (`"{subject}::{predicate}"`, [`absorb_common`]'s own shape) split on its
/// `"::"` separator into the `(subject, predicate)` pair
/// `hornvale_book::esoteric_lines` accepts as its `reader` — the initiated
/// unlock is exactly "have you been told this fact", nothing more. The
/// walking-derived shapes ([`IdentityProjection`]'s `room/<id>` and
/// `settlement/<id>/<field>`, `/`-separated) carry no `"::"` and are
/// silently skipped, not converted — the floor's only unlock channel is
/// `write` (spec §3.2; the observational unlock is a recorded followup,
/// not scope). A key with no `"::"` at all is likewise skipped; today
/// there is none (every entry either `absorb_common` or a projection ever
/// writes carries one separator or the other), asserted by
/// `reader_set_splits_every_key`.
/// type-audit: bare-ok(identifier-text: return)
pub fn reader_set(knowledge: &Knowledge) -> BTreeSet<(String, String)> {
    knowledge
        .0
        .keys()
        .filter_map(|key| key.split_once("::"))
        .map(|(subject, predicate)| (subject.to_string(), predicate.to_string()))
        .collect()
}

/// knowledge = project(ground_truth, vantage, perception).
pub trait Projection {
    /// Project a vantage into knowledge entries.
    fn project(&self, vantage: &Vantage, perception: &PerceptionVector) -> Knowledge;
}

/// Tier 0: the identity projection — the vantage, verbatim.
pub struct IdentityProjection;

impl Projection for IdentityProjection {
    fn project(&self, vantage: &Vantage, _perception: &PerceptionVector) -> Knowledge {
        let mut k = BTreeMap::new();
        k.insert(
            format!("room/{}", vantage.locale.id),
            serde_json::to_string(&vantage.locale).expect("locale serializes"),
        );
        k.insert(
            format!("settlement/{}/name", vantage.village.id.0),
            vantage.village.name.clone(),
        );
        k.insert(
            format!("settlement/{}/population", vantage.village.id.0),
            vantage.village.population.to_string(),
        );
        Knowledge(k)
    }
}

/// The subset contract, mechanically, for projection-derived entries only:
/// every one must re-derive from the world. `room/<id>` re-describes
/// through the locale window; settlement entries re-read the ledger.
/// Heard entries (the `::`-keyed shape [`absorb_common`] writes) are
/// deliberately outside this contract — a listener can absorb a false
/// heard statement, so a `::`-keyed key is an unknown shape here by
/// design, not an oversight; callers must not run heard knowledge through
/// this check. Unknown key shapes among the checked (non-heard) entries
/// are violations.
/// type-audit: bare-ok(prose: return)
pub fn knowledge_is_subset(
    k: &Knowledge,
    world: &World,
    ctx: &LocaleContext,
    at: WorldTime,
) -> Result<(), String> {
    for (key, value) in &k.0 {
        let parts: Vec<&str> = key.split('/').collect();
        match parts.as_slice() {
            ["room", id] => {
                let raw: u64 = id.parse().map_err(|_| format!("bad room key {key}"))?;
                let addr = RoomId(raw).unpack().map_err(|e| format!("{key}: {e:?}"))?;
                let truth = ctx.describe(&addr, at).map_err(|e| format!("{key}: {e}"))?;
                let truth = serde_json::to_string(&truth).expect("locale serializes");
                if truth != *value {
                    return Err(format!("{key} diverges from ground truth"));
                }
            }
            ["settlement", id, field] => {
                let raw: u64 = id.parse().map_err(|_| format!("bad key {key}"))?;
                let entity = hornvale_kernel::EntityId::new(raw)
                    .ok_or_else(|| format!("bad key {key}: entity id 0 is never valid"))?;
                let truth = match *field {
                    "name" => world
                        .ledger
                        .text_of(entity, hornvale_kernel::NAME)
                        .map(str::to_string),
                    "population" => match world
                        .ledger
                        .value_of(entity, hornvale_settlement::POPULATION)
                    {
                        Some(Value::Number(n)) => Some((*n as u32).to_string()),
                        _ => None,
                    },
                    _ => None,
                };
                if truth.as_deref() != Some(value.as_str()) {
                    return Err(format!("{key} diverges from ground truth"));
                }
            }
            _ => return Err(format!("unknown knowledge key shape: {key}")),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{mint_flagship, observable};
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::{Seed, World, WorldTime};
    use hornvale_locale::LocaleContext;
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn seam_world() -> World {
        build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    #[test]
    fn the_identity_projection_is_a_subset_of_truth() {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let agent = mint_flagship(&world, &ctx).unwrap();
        let at = WorldTime { day: 0.0 };
        let vantage = observable(&world, &ctx, &agent, at).unwrap();
        let k = IdentityProjection.project(&vantage, &agent.perception);
        assert!(!k.0.is_empty(), "the agent knows something");
        knowledge_is_subset(&k, &world, &ctx, at).unwrap();
    }

    #[test]
    fn knowledge_accumulates_across_rooms_and_stays_a_subset() {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let mut agent = mint_flagship(&world, &ctx).unwrap();
        let at = WorldTime { day: 0.0 };
        let mut k = IdentityProjection.project(
            &observable(&world, &ctx, &agent, at).unwrap(),
            &agent.perception,
        );
        let before = k.0.len();
        // step to a lateral neighbor and absorb its projection
        agent.position = agent.position.neighbors()[0].clone();
        k.absorb(IdentityProjection.project(
            &observable(&world, &ctx, &agent, at).unwrap(),
            &agent.perception,
        ));
        assert!(k.0.len() > before, "walking grows knowledge");
        knowledge_is_subset(&k, &world, &ctx, at).unwrap();
    }

    #[test]
    fn a_corrupted_entry_fails_the_subset_check() {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let agent = mint_flagship(&world, &ctx).unwrap();
        let at = WorldTime { day: 0.0 };
        let mut k = IdentityProjection.project(
            &observable(&world, &ctx, &agent, at).unwrap(),
            &agent.perception,
        );
        k.0.insert("settlement/999999/name".into(), "Liesburg".into());
        assert!(knowledge_is_subset(&k, &world, &ctx, at).is_err());
    }

    fn generated(seed: u64) -> World {
        build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("generated world builds")
    }

    /// The Echo T4's transfer law: a Common sentence the Book renders
    /// survives transit into a listener's `Knowledge` — the game seam
    /// proving text is a lossless carrier, not just T3's round trip inside
    /// the Book itself.
    #[test]
    fn a_spoken_fact_survives_transit_into_knowledge() {
        let world = generated(1);
        let ctx = hornvale_book::parse_context(&world);
        let volume = hornvale_book::render_volume(&world);
        let planet_line = volume
            .lines
            .iter()
            .find(|l| l.contains(" is a planet"))
            .expect("seed 1 renders a planet line");

        let mut heard = Knowledge::default();
        let n = absorb_common(&mut heard, planet_line, &ctx).expect("the listener parses Common");
        assert!(n >= 3, "classification + at least two aggregated facts");
        // What the listener now knows is what the sentence said:
        let subject = planet_line.split(" is ").next().unwrap();
        assert_eq!(
            heard.0.get(&format!("{subject}::is-a")).map(String::as_str),
            Some("planet")
        );
        // The surface values, pinned exactly — one integral, one
        // fractional, so BOTH number-formatting branches are verified
        // against how the sentence carried them. Seed 1's planet line
        // (see windows/book's seed-1 test: two moons, day 1.5507196 →
        // "about 1.5") says "with two moons" and "about 1.5 standard
        // days"; the listener's belief reads back "2" and "1.5".
        assert!(
            planet_line.contains("with two moons")
                && planet_line.contains("about 1.5 standard days"),
            "seed 1's planet line carries both number fragments: {planet_line}"
        );
        assert_eq!(
            heard
                .0
                .get(&format!("{subject}::moon-count"))
                .map(String::as_str),
            Some("2"),
            "a whole count surfaces bare"
        );
        assert_eq!(
            heard
                .0
                .get(&format!("{subject}::day-length-std"))
                .map(String::as_str),
            Some("1.5"),
            "a fractional quantity keeps its digits"
        );
    }

    /// The Vessel Stitch T2's adapter law: every key `absorb_common` ever
    /// writes carries the `"::"` separator, so `reader_set` skips nothing
    /// today — the skipped-key arm is reachable only by a future knowledge
    /// shape, not this one.
    #[test]
    fn reader_set_splits_every_key() {
        let world = generated(1);
        let ctx = hornvale_book::parse_context(&world);
        let volume = hornvale_book::render_volume(&world);
        let planet_line = volume
            .lines
            .iter()
            .find(|l| l.contains(" is a planet"))
            .expect("seed 1 renders a planet line");
        let mut heard = Knowledge::default();
        absorb_common(&mut heard, planet_line, &ctx).expect("the listener parses Common");

        let reader = reader_set(&heard);
        assert_eq!(
            reader.len(),
            heard.0.len(),
            "every heard key splits on '::' — none skipped"
        );
        let subject = planet_line.split(" is ").next().unwrap();
        assert!(
            reader.contains(&(subject.to_string(), "is-a".to_string())),
            "the classification key splits into (subject, \"is-a\")"
        );
        assert!(
            reader.contains(&(subject.to_string(), "moon-count".to_string())),
            "the moon-count key splits into (subject, \"moon-count\")"
        );
    }
}
