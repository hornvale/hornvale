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

/// The founders a world remembers: per people, the `MEMORY_DEPTH` occupations
/// with the largest peak population.
///
/// Ranking is `(peak_population DESC, site ASC, founded ASC, handle ASC)` — a
/// total order: the first three keys alone can tie (an exact three-way
/// match), so the handle breaks it. Iteration is over a `BTreeMap`, so the
/// result does not depend on input order.
///
/// # Panics
///
/// If two selected founders share a handle. That would mean two occupations
/// indistinguishable in every semantic field both reached the cast, and they
/// would silently become one identity with one name. Failing loudly is the
/// point: this is the campaign's determinism guard, not a formality.
pub fn select_founders(records: &[OccupationRecord]) -> Vec<Founder> {
    let mut by_people: BTreeMap<&'static str, Vec<usize>> = BTreeMap::new();
    for (i, r) in records.iter().enumerate() {
        by_people.entry(r.people.0).or_default().push(i);
    }

    let mut cast = Vec::new();
    for idxs in by_people.values_mut() {
        idxs.sort_by(|&a, &b| {
            let (x, y) = (&records[a], &records[b]);
            y.peak_population
                .cmp(&x.peak_population)
                .then(x.site.0.cmp(&y.site.0))
                .then(x.founded.total_cmp(&y.founded))
                // Total order, so the doc's claim is structural rather than
                // lucky. Two records reaching this leg with the same handle are
                // precisely what the uniqueness assert below rejects, so this
                // defers to the guard rather than hiding from it.
                .then(founder_handle(x).0.cmp(&founder_handle(y).0))
        });
        for &i in idxs.iter().take(MEMORY_DEPTH) {
            let r = &records[i];
            cast.push(Founder {
                handle: founder_handle(r),
                occupation: i,
                people: r.people,
                community: r.community,
                founded: r.founded,
            });
        }
    }

    let mut seen = std::collections::BTreeSet::new();
    for f in &cast {
        assert!(
            seen.insert(f.handle.0),
            "two selected founders share handle {:#x} — occupations \
             indistinguishable in every semantic field both reached the cast, \
             so they would become one person with one name. Widen the key in \
             founder_handle rather than suppressing this.",
            f.handle.0
        );
    }
    cast
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{
        Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
    };
    use hornvale_kernel::{CellId, EntityId, KindId};

    fn rec(people: &'static str, site: u32, founded: f64, peak: u32) -> OccupationRecord {
        let e = EntityId::new(1).expect("nonzero");
        OccupationRecord {
            people: KindId(people),
            community: e,
            lineage: e,
            site: CellId(site),
            founded,
            ended: None,
            peak_population: peak,
            tech: TechHorizon::Neolithic,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: None,
            ended_by: Ended::Nature,
            founded_from: Founding::Genesis(CellId(site)),
            notability: Notability::Common,
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
        let goblins = cast.iter().filter(|f| f.people.0 == "goblin").count();
        let kobolds = cast.iter().filter(|f| f.people.0 == "kobold").count();
        assert_eq!(
            goblins, MEMORY_DEPTH,
            "a populous people is capped at the depth"
        );
        assert_eq!(kobolds, 1, "a people with one occupation gets one founder");
    }

    #[test]
    fn selection_takes_the_largest_and_is_order_independent() {
        let forward = vec![rec("goblin", 1, 0.0, 5), rec("goblin", 2, 0.0, 99)];
        let mut backward = forward.clone();
        backward.reverse();
        let a = select_founders(&forward);
        let b = select_founders(&backward);
        assert_eq!(a.len(), 2);
        assert_eq!(
            a.iter().map(|f| f.handle.0).collect::<Vec<_>>(),
            b.iter().map(|f| f.handle.0).collect::<Vec<_>>(),
            "the cast does not depend on input order"
        );
        assert_eq!(a[0].community, forward[1].community);
    }

    #[test]
    fn a_tie_resolves_the_same_way_whichever_order_it_arrives_in() {
        // Same people, site, founded and peak; different `ended`, so different
        // handles and a genuine three-way tie in the first three keys.
        let mut a = rec("goblin", 3, 100.0, 42);
        a.ended = Some(500.0);
        let mut b = rec("goblin", 3, 100.0, 42);
        b.ended = Some(900.0);

        let forward = select_founders(&[a.clone(), b.clone()]);
        let backward = select_founders(&[b, a]);
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
    #[should_panic(expected = "share handle")]
    fn indistinguishable_occupations_in_the_cast_are_a_hard_error() {
        // Two records identical in every field the handle keys on. The live
        // corpus must never produce this; the guard must fire when it does.
        let a = rec("goblin", 7, 50.0, 60);
        let b = a.clone();
        let _ = select_founders(&[a, b]);
    }
}
