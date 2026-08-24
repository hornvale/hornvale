//! Hornvale sentiment window: layer 1 of the evaluative-beliefs program
//! (spec §5) — a pure, world-invariant derivation of a snap-judgment
//! predisposition between the fifteen modeled peoples, read entirely from
//! the authored species/demography/language catalogs. **No world is
//! built**: every distance here is a function of two peoples' authored
//! attribute vectors alone, computable before any seed exists.
//!
//! [`catalog`] reads the fifteen peoples' [`PeopleTraits`] once; [`Axis`]
//! enumerates the eight world-invariant axes; [`axis_distance`] computes
//! one axis's normalized `[0,1]` distance between two peoples. Higher
//! layers (Task 2 and beyond) fold these eight distances into a single
//! weighted predisposition — that composition is deliberately out of scope
//! here (0021: this crate must never itself encode a preference between two
//! named peoples, only structural attribute distance).
#![warn(missing_docs)]

mod axes;
mod judgment;
mod weights;

pub use axes::{Axis, axis_distance};
pub use judgment::{Emotion, Judgment, snap_judgment};
pub use weights::weight_vector;

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{Mass, ResourceVector};
use hornvale_language::speech::{ArticulationVector, articulation_registry};
use hornvale_species::{
    BiosphereTraits, ConditionNiche, HabitatRealm, LifeSchedule, MindVector, PerceptionVector,
    SocietyVector, ThermalStrategy, TrophicMode, biosphere_registry, habitat_realm_registry,
    perception_registry, psyche_registry, society_registry,
};

/// One people, as this crate identifies it — the species catalog's stable
/// key. There is no separate label enum for the fifteen settling peoples;
/// this is a re-export of `hornvale_species::KindId`, not a new type.
pub type PeopleId = hornvale_species::KindId;

/// One people's read-out attribute bundle: exactly the fields the eight
/// snap-judgment axes need, read once from the species, demography and
/// language catalogs. `preys_on` is precomputed against the **whole**
/// fifteen-people roster (`hornvale_demography::niche::predation` needs the
/// full trophic web to resolve predator/prey direction), so a
/// `PeopleTraits` cannot be derived in isolation — [`catalog`] is the only
/// constructor a caller should use for real data; tests may build synthetic
/// instances directly via the public fields to probe a single axis's
/// structural properties.
#[derive(Clone, Debug, PartialEq)]
pub struct PeopleTraits {
    /// This people's catalog key.
    pub id: PeopleId,
    /// Surface or subterranean realm (sparse in the catalog; a people
    /// absent from `habitat_realm_registry` defaults to `Surface`).
    pub habitat: HabitatRealm,
    /// The resource-utilization niche (diet), feeding `DietPredation`.
    pub niche: ResourceVector,
    /// The four-axis environmental condition-tolerance profile, feeding
    /// `ConditionNiche`.
    pub condition_niche: ConditionNiche,
    /// Adult body mass, feeding `SizeThreat` and (with `thermal_strategy`
    /// and `schedule`) `Reproductive`.
    pub mass: Mass,
    /// Thermal strategy, feeding `Reproductive` via
    /// `hornvale_species::reproductive_tempo`.
    pub thermal_strategy: ThermalStrategy,
    /// Trophic mode, carried through from the biosphere component. **Nothing
    /// reads it yet** (THE GOSSAN) — see [`hornvale_species::TrophicMode`].
    pub trophic_mode: TrophicMode,
    /// Life-history pacing, feeding `Reproductive` the same way.
    pub schedule: LifeSchedule,
    /// Social-organization vector, feeding `Sociality`.
    pub society: SocietyVector,
    /// Individual-mind vector, feeding [`crate::weight_vector`]'s threat
    /// multiplier (`threat_response`). Added in Task 2; no Task-1 axis
    /// reads it.
    pub mind: MindVector,
    /// Perception/activity vector, feeding `ActivityCycle`.
    pub perception: PerceptionVector,
    /// Speech articulation vector, feeding `Language`.
    pub articulation: ArticulationVector,
    /// The other peoples this people preys upon, per
    /// `hornvale_demography::niche::predation`'s mass-windowed trophic
    /// check run over the full fifteen-people roster. Feeds the
    /// directional half of `DietPredation`.
    pub preys_on: BTreeSet<PeopleId>,
}

/// Read the fifteen settling peoples' attribute bundles from the species,
/// demography and language catalogs. No world is built — every field
/// returned is authored data.
///
/// Enumeration is driven **entirely** by
/// `hornvale_species::society_registry`, which holds exactly the fifteen
/// peoples and nothing else. Every other registry consulted here
/// (`biosphere_registry`, `habitat_realm_registry`, `perception_registry`,
/// `articulation_registry`) carries extra non-peopled or non-settling kinds
/// (dragons, beasts) and is read only by `.get(&kind)` against ids that
/// already came from `society_registry`, so nothing but the fifteen can
/// ever enter the result.
pub fn catalog() -> BTreeMap<PeopleId, PeopleTraits> {
    let society = society_registry();
    let biosphere = biosphere_registry();
    let habitat = habitat_realm_registry();
    let perception = perception_registry();
    let articulation = articulation_registry();
    let psyche = psyche_registry();

    // `society.ids()` is deterministic ascending-by-key; that order becomes
    // the `u32` index `niche::predation` wants.
    let ids: Vec<PeopleId> = society.ids().copied().collect();
    let predation_input: Vec<(u32, Mass, ResourceVector)> = ids
        .iter()
        .enumerate()
        .map(|(index, id)| {
            let bio = biosphere_row(&biosphere, id);
            (index as u32, bio.mass, bio.niche.clone())
        })
        .collect();
    let predation = hornvale_demography::niche::predation(&predation_input);

    ids.iter()
        .enumerate()
        .map(|(index, id)| {
            let bio = biosphere_row(&biosphere, id);
            let preys_on: BTreeSet<PeopleId> = predation
                .get(&(index as u32))
                .into_iter()
                .flatten()
                .map(|prey_index| ids[*prey_index as usize])
                .collect();
            let traits = PeopleTraits {
                id: *id,
                habitat: habitat.get(id).copied().unwrap_or(HabitatRealm::SURFACE),
                niche: bio.niche.clone(),
                condition_niche: bio.condition_niche,
                mass: bio.mass,
                thermal_strategy: bio.thermal_strategy,
                trophic_mode: bio.trophic_mode,
                schedule: bio.schedule,
                society: *society
                    .get(id)
                    .expect("id came from society_registry().ids(), so it has a row"),
                mind: *psyche.get(id).unwrap_or_else(|| {
                    panic!("{id:?} is a settling people but has no psyche_registry row")
                }),
                perception: *perception.get(id).unwrap_or_else(|| {
                    panic!("{id:?} is a settling people but has no perception_registry row")
                }),
                articulation: *articulation.get(id).unwrap_or_else(|| {
                    panic!("{id:?} is a settling people but has no articulation_registry row")
                }),
                preys_on,
            };
            (*id, traits)
        })
        .collect()
}

/// Read one people's biosphere row, panicking with a diagnostic if
/// `society_registry` and `biosphere_registry` have drifted apart (both are
/// hand-authored; a settling people missing its universal biosphere row is
/// an authoring bug, not a runtime condition this crate should paper over).
fn biosphere_row<'a>(
    biosphere: &'a hornvale_kernel::ComponentStore<PeopleId, BiosphereTraits>,
    id: &PeopleId,
) -> &'a BiosphereTraits {
    biosphere
        .get(id)
        .unwrap_or_else(|| panic!("{id:?} is in society_registry but not biosphere_registry"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn catalog_holds_exactly_the_fifteen_peoples() {
        let cat = catalog();
        assert_eq!(cat.len(), 15, "society_registry defines exactly 15 peoples");
    }

    #[test]
    fn catalog_is_deterministic() {
        assert_eq!(catalog(), catalog());
    }

    #[test]
    fn only_drow_is_subterranean_among_the_fifteen() {
        let cat = catalog();
        let subterranean: Vec<PeopleId> = cat
            .values()
            .filter(|t| t.habitat == HabitatRealm::Subterranean)
            .map(|t| t.id)
            .collect();
        assert_eq!(
            subterranean,
            vec![hornvale_species::KindId("drow")],
            "habitat_realm_registry also lists rust-monster and xorn as \
             Subterranean, but neither is a settling people, so catalog() \
             (keyed off society_registry) must not surface them"
        );
    }
}
