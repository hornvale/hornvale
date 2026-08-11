//! Promotion: which founders a people remembers, and how their identity is
//! derived.
//!
//! This lives in the composition root rather than in `domains/person` because
//! it reads `domains/history`'s occupation records, and a domain crate may
//! reach only the kernel (decision 0002).

use hornvale_history::flesh::{RoleHandle, founder_handle};
use hornvale_history::record::OccupationRecord;
use hornvale_kernel::{EntityId, KindId};
use std::collections::BTreeMap;

use crate::{language_of_wc, morph_options};

/// How many founders one people remembers.
///
/// A constant per *holder*, not per world and not a ratio: oral genealogies
/// hold roughly constant depth however much time has passed, because the
/// binding constraint is transmission rather than history length. The world's
/// cast is therefore the sum over peoples of `min(MEMORY_DEPTH, occupations)`,
/// which grows when the species roster grows and needs no retuning.
/// type-audit: bare-ok(count)
pub const MEMORY_DEPTH: usize = 20;

/// One remembered founder: an identity plus where it came from.
/// type-audit: bare-ok(index: occupation), waiver(decision-0014: founded)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Founder {
    /// The stable identity, expandable by `persona_of`.
    pub handle: RoleHandle,
    /// Index into the records slice this founder was selected from.
    pub occupation: usize,
    /// The people who remember this founder.
    pub people: KindId,
    /// The community whose occupation they founded.
    pub community: EntityId,
    /// The occupation's founding day — this founder's birth.
    pub founded: f64,
}

/// An occupation a people could **not** remember, because another selected
/// occupation was indistinguishable from it.
///
/// This is the record of an authorized fidelity cut, not an error: see
/// [`select_founders`] for why a drop is possible at all and what it costs.
/// type-audit: bare-ok(index: occupation), bare-ok(index: kept)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UnrememberedFounder {
    /// The handle this occupation shares with the one kept in its place.
    pub handle: RoleHandle,
    /// Index into the records slice of the occupation that was dropped.
    pub occupation: usize,
    /// Index into the records slice of the occupation kept in its place — the
    /// one that ranked first under the promotion order.
    pub kept: usize,
    /// The people who could not remember it.
    pub people: KindId,
}

/// What a world remembers of its founders, and what it could not.
///
/// The second half is deliberately part of the return value rather than a
/// side effect: a fidelity cut that no caller can see is the thing this
/// project refuses, and a test must be able to read the cut off one build.
#[derive(Clone, Debug, PartialEq)]
pub struct FounderCast {
    /// The founders the world remembers, in promotion order. Handles are
    /// distinct across the whole cast.
    pub remembered: Vec<Founder>,
    /// The founders it could not remember — empty on all but roughly two
    /// worlds in a thousand.
    pub unremembered: Vec<UnrememberedFounder>,
}

/// The founders a world remembers: per people, the `MEMORY_DEPTH` occupations
/// with the largest peak population.
///
/// Ranking is `(peak_population DESC, site ASC, founded ASC, handle ASC)` — a
/// total order over *distinguishable* records: the first three keys alone can
/// tie (an exact three-way match), so the handle breaks it. Iteration is over
/// a `BTreeMap`, so the result does not depend on input order.
///
/// # Indistinguishable occupations are dropped, not fatal
///
/// [`hornvale_history::flesh::founder_handle`] keys on
/// `(people, site, founded, ended, peak_population)` and excludes the
/// occupation's entity id (decision 0051), so two occupations agreeing on all
/// five derive the **same handle by construction**. Every measured pair is one
/// physical story: a same-day founding-and-flight cascade, `founded == ended`,
/// one site, one people, one day. The four ranking keys tie for such a pair by
/// the same construction, so the order alone cannot separate them.
///
/// This function used to `assert!` that no two cast members shared a handle,
/// and a world that produced one **died**: seeds 283 and 705 both sit inside
/// the census range 0–999, so the once-per-campaign census could not run. A
/// generator that dies on a legal seed is a liveness bug whatever the key is,
/// and the campaign's diagnosis measured that *no* candidate key is provably
/// total — the best scored (parent **and** ender material keys) still leaves
/// two whole-record twin pairs in 3000 worlds. So the guard could never have
/// been discharged by widening alone.
///
/// What happens instead, by Nathan's ruling in The Radiation: the **first**
/// member of a handle-equal group under the ranking above is promoted and the
/// rest are dropped into [`FounderCast::unremembered`]. Nothing is backfilled,
/// so that people ends one short of `MEMORY_DEPTH`: the world forgets a
/// founder rather than remembering a different one, which is the smaller and
/// more honest of the two shapes. The cost is real and bounded — measured over
/// the census range, **2 worlds in 1000 lose exactly one founder each**; every
/// other founder in every other world is untouched, because the drop can only
/// fire where the handles were already equal.
///
/// Widening the key is the known correct fix and is **deferred as an epoch**
/// (it changes every founder's name in every world): the idea registry's
/// `MEM-founder-handle-epoch` row carries the scoring and the cost.
///
/// Determinism: the kept member follows from the existing ranking, and the
/// scan below walks peoples in `BTreeMap` order and each people in ranked
/// order, so `unremembered` is itself a deterministic sequence. Where a
/// handle-equal pair ties the ranking outright — which is the whole of this
/// case — the stable sort falls through to the records' own order, i.e. ledger
/// commit order, the same fallback
/// [`hornvale_history::record::layer_key`] already documents and relies on.
///
/// # Panics
///
/// If the promoted cast still carries a duplicate handle. That is unreachable
/// while the drop below stands; it is kept as a post-condition because
/// `promote` turns a handle into a person's name, and two people sharing a
/// name would be a silent merge of two identities.
pub fn select_founders(records: &[OccupationRecord]) -> FounderCast {
    let mut by_people: BTreeMap<&'static str, Vec<usize>> = BTreeMap::new();
    for (i, r) in records.iter().enumerate() {
        by_people.entry(r.core.people.0).or_default().push(i);
    }

    let mut remembered = Vec::new();
    let mut unremembered = Vec::new();
    // Handle -> the record index promoted under it. Global rather than
    // per-people, so the cast-wide uniqueness the old assert claimed is what
    // the drop restores, not a weaker per-people version of it.
    let mut kept_by_handle: BTreeMap<u64, usize> = BTreeMap::new();
    for idxs in by_people.values_mut() {
        idxs.sort_by(|&a, &b| {
            let (x, y) = (&records[a], &records[b]);
            y.core
                .peak_population
                .cmp(&x.core.peak_population)
                .then(x.core.site.0.cmp(&y.core.site.0))
                .then(x.core.founded.total_cmp(&y.core.founded))
                // The handle is the last ranking key, and two records reaching
                // this leg with the same handle tie here too — which is exactly
                // the case the drop below handles. Deliberately unchanged: a
                // fifth key would reorder records that tie the first four
                // *below* the MEMORY_DEPTH cut in worlds that never collide,
                // moving worlds this repair must leave byte-identical.
                .then(founder_handle(x).0.cmp(&founder_handle(y).0))
        });
        for &i in idxs.iter().take(MEMORY_DEPTH) {
            let r = &records[i];
            let handle = founder_handle(r);
            if let Some(&kept) = kept_by_handle.get(&handle.0) {
                unremembered.push(UnrememberedFounder {
                    handle,
                    occupation: i,
                    kept,
                    people: r.core.people,
                });
                continue;
            }
            kept_by_handle.insert(handle.0, i);
            remembered.push(Founder {
                handle,
                occupation: i,
                people: r.core.people,
                // The Scaffold deleted `OccupationRecord::community`; the field
                // held the occupation's OWN entity under a misleading name, and
                // `reconstruct_occupation` now sets that same value as `id`.
                // Numerically identical, so promotion keys on what it always
                // did — NOT `founded_from`, and not a re-derivation, both of
                // which compile cleanly and silently change the subject
                // (`docs/retrospectives/the-scaffold.md`).
                community: r.id,
                founded: r.core.founded,
            });
        }
    }

    let mut seen = std::collections::BTreeSet::new();
    for f in &remembered {
        assert!(
            seen.insert(f.handle.0),
            "two promoted founders share handle {:#x} — the drop above is the \
             one thing standing between a handle collision and two people \
             carrying one name, so reaching here means it was removed or \
             bypassed, not that a new collision appeared.",
            f.handle.0
        );
    }
    FounderCast {
        remembered,
        unremembered,
    }
}

/// Promote every remembered founder into a ledger person.
///
/// Reads occupation records back out of the committed ledger — the `History`
/// value is local to an earlier stage's closure and out of scope here.
///
/// Birth is `founded − age_at_maturity` and death is `birth + lifespan`, both
/// from `domains/species::allometry::life_history`, which draws nothing. The
/// `person-died` fact is committed only once that day has passed at `now`; a
/// living person is the absence of one. A species with no lifespan
/// (`Ametabolic`) yields no death fact either, which reads as "not known to have
/// died", and one with no maturity falls back to founding day as birth.
pub fn promote(
    world: &mut hornvale_kernel::World,
    wc: &crate::components::WorldComponents,
) -> Result<Vec<EntityId>, crate::BuildError> {
    let records = crate::occupation_records(world);
    let now = world
        .ledger
        .find("history-now")
        .filter_map(|f| match f.object {
            hornvale_kernel::Value::Number(n) => Some(n),
            _ => None,
        })
        .last()
        .unwrap_or(0.0);

    // The second half of the cast is the authorized fidelity cut, and it is
    // bound rather than swallowed so that the one place it is discarded is
    // visible. A dropped founder is simply not promoted — there is no fact for
    // "a founder this people failed to remember", and inventing a predicate
    // for a thing that fires in two worlds per thousand would put an
    // essentially unreachable row in the registry (`person-died`, The
    // Particular). The record lives in `select_founders`' return value, where
    // `windows/worldgen/tests/founder_collision.rs` pins its size per seed.
    let FounderCast {
        remembered: cast,
        unremembered: _,
    } = select_founders(&records);
    let mut seeds = Vec::with_capacity(cast.len());
    for f in &cast {
        let life = wc
            .biosphere
            .get(&f.people)
            // `schedule` is The Long Age's third time-law input: a paced kind
            // matures later at unchanged mass, so passing the kind's own
            // schedule (rather than ALLOMETRIC) is what keeps a founder's birth
            // day consistent with the species it belongs to.
            .map(|b| hornvale_species::life_history(b.mass, b.metabolic_class, b.schedule));
        // A founder was already grown when they founded, so birth precedes the
        // founding by a maturity. This goes NEGATIVE for day-0 settlements —
        // the history record begins at day 0 and the founder did not. Honest,
        // not clamped (spec D4).
        let maturity_days = life
            .as_ref()
            .and_then(|l| l.age_at_maturity)
            .map_or(0.0, |y| y.days());
        let birth_day = f.founded - maturity_days;
        // Death follows BIRTH by a lifespan, not the founding, and is committed
        // only once it has already passed at `now`.
        let death = life
            .as_ref()
            .and_then(|l| l.lifespan)
            .map(|y| birth_day + y.days())
            .filter(|d| *d <= now);
        // Named here, where the language machinery already stands. `Namer` holds
        // no mutable stream and derives fresh per call, so this draw is on a
        // path disjoint from every other name in the world.
        let ph = language_of_wc(world, wc, f.people.0);
        let namer = hornvale_language::Namer::new(&world.seed, f.people.0, &ph);
        let mind = wc
            .psyche
            .get(&f.people)
            .expect("a placed people carries a mind vector");
        let society = wc
            .society
            .get(&f.people)
            .expect("a placed people carries a society vector");
        let name = namer
            .name(
                hornvale_language::NameKind::Person,
                f.handle.0,
                &morph_options(mind, society),
            )
            .roman;
        seeds.push(hornvale_person::PersonSeed {
            community: f.community,
            name,
            birth_day,
            founding_day: f.founded,
            death_day: death,
        });
    }
    hornvale_person::genesis(world, &seeds).map_err(crate::BuildError::from)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{
        Ended, Founding, Function, Notability, Occupation, OccupationRecord, TechHorizon,
    };
    use hornvale_kernel::{CellId, EntityId, KindId};

    fn rec(people: &'static str, site: u32, founded: f64, peak: u32) -> OccupationRecord {
        OccupationRecord {
            core: Occupation {
                people: KindId(people),
                site: CellId(site),
                founded,
                ended: None,
                peak_population: peak,
                tech: TechHorizon::Neolithic,
                function: Function::Agrarian,
                deity: None,
                tongue: None,
                cause: None,
                notability: Notability::Common,
            },
            id: EntityId::new(1).expect("nonzero"),
            ended_by: Ended::Nature,
            founded_from: Founding::Genesis(CellId(site)),
        }
    }

    #[test]
    fn each_people_is_capped_at_memory_depth() {
        let mut records = Vec::new();
        for i in 0..(MEMORY_DEPTH as u32 + 5) {
            records.push(rec("goblin", i, f64::from(i), 100 - i));
        }
        records.push(rec("kobold", 900, 0.0, 7));
        let cast = select_founders(&records);
        let goblins = cast
            .remembered
            .iter()
            .filter(|f| f.people.0 == "goblin")
            .count();
        let kobolds = cast
            .remembered
            .iter()
            .filter(|f| f.people.0 == "kobold")
            .count();
        assert_eq!(
            goblins, MEMORY_DEPTH,
            "a populous people is capped at the depth"
        );
        assert_eq!(kobolds, 1, "a people with one occupation gets one founder");
        assert!(
            cast.unremembered.is_empty(),
            "distinguishable occupations drop nobody"
        );
    }

    #[test]
    fn selection_takes_the_largest_and_is_order_independent() {
        let forward = vec![rec("goblin", 1, 0.0, 5), rec("goblin", 2, 0.0, 99)];
        let mut backward = forward.clone();
        backward.reverse();
        let a = select_founders(&forward).remembered;
        let b = select_founders(&backward).remembered;
        assert_eq!(a.len(), 2);
        assert_eq!(
            a.iter().map(|f| f.handle.0).collect::<Vec<_>>(),
            b.iter().map(|f| f.handle.0).collect::<Vec<_>>(),
            "the cast does not depend on input order"
        );
        assert_eq!(a[0].community, forward[1].id);
    }

    #[test]
    fn a_tie_resolves_the_same_way_whichever_order_it_arrives_in() {
        // Same people, site, founded and peak; different `ended`, so different
        // handles and a genuine three-way tie in the first three keys.
        let mut a = rec("goblin", 3, 100.0, 42);
        a.core.ended = Some(500.0);
        let mut b = rec("goblin", 3, 100.0, 42);
        b.core.ended = Some(900.0);

        let forward = select_founders(&[a.clone(), b.clone()]).remembered;
        let backward = select_founders(&[b, a]).remembered;
        let key = |c: &[Founder]| -> Vec<(u64, EntityId, f64)> {
            c.iter()
                .map(|f| (f.handle.0, f.community, f.founded))
                .collect()
        };
        assert_eq!(
            key(&forward),
            key(&backward),
            "a tie must resolve identically whichever order it arrives in — \
             Task 3 keys facts on `community`, so a flip would attribute a \
             person to a different settlement"
        );
    }

    #[test]
    fn indistinguishable_occupations_promote_one_and_record_the_other() {
        // Two records identical in every field the handle keys on. This used to
        // abort the whole build; it now costs one remembered founder, and the
        // loss is in the return value rather than nowhere.
        let a = rec("goblin", 7, 50.0, 60);
        let b = a.clone();
        let cast = select_founders(&[a, b]);
        assert_eq!(
            cast.remembered.len(),
            1,
            "one of two indistinguishable occupations is promoted"
        );
        assert_eq!(
            cast.unremembered.len(),
            1,
            "and the other is recorded, not discarded silently"
        );
        assert_eq!(
            cast.unremembered[0].handle, cast.remembered[0].handle,
            "the drop is justified by the shared handle and nothing else"
        );
        assert_eq!(
            (cast.unremembered[0].kept, cast.unremembered[0].occupation),
            (0, 1),
            "the earlier member of the ranking is kept; the later one is dropped"
        );
    }

    #[test]
    fn a_drop_costs_one_founder_and_is_not_backfilled() {
        // A people with MEMORY_DEPTH + 1 occupations, two of which are
        // indistinguishable. If the drop backfilled, the cast would still be
        // MEMORY_DEPTH deep and the twin below the cut would be pulled up.
        let mut records = Vec::new();
        for i in 0..(MEMORY_DEPTH as u32 - 2) {
            records.push(rec("goblin", i, f64::from(i), 100 - i));
        }
        let twin = rec("goblin", 500, 500.0, 50);
        records.push(twin.clone());
        records.push(twin);
        // Ranks last, so it sits just below the cut and would be the backfill.
        records.push(rec("goblin", 600, 600.0, 1));
        let cast = select_founders(&records);
        assert_eq!(
            cast.remembered.len(),
            MEMORY_DEPTH - 1,
            "the world forgets a founder rather than remembering a different one"
        );
        assert_eq!(cast.unremembered.len(), 1);
        assert!(
            cast.remembered
                .iter()
                .all(|f| f.occupation != records.len() - 1),
            "the occupation below the cut must stay below it"
        );
    }
}
