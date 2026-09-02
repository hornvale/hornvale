//! Settlement-selection helpers for a possession (The Hand, Task 3): which
//! settlement a session drives from, and the walk-band depth every derived
//! body shares. There is no `Agent` type here any more — `Session` drives a
//! member of its own derived roster (a [`crate::body::Body`]) rather than
//! a second, separately-minted representation of the same villager.

use crate::VesselError;
use hornvale_kernel::{Value, World, math};
use hornvale_settlement::{LATITUDE, LONGITUDE, VillageInfo};
use hornvale_species::{perception_registry, species_of};

/// The canonical walk depth — **a re-export of
/// [`hornvale_locale::walk_depth`], not a second definition.**
///
/// It was defined here until The Pavement, and that placement was the cause
/// of a defect rather than a neutral choice: the dependency chain is
/// `locale -> scene -> vessel -> cli`, so a definition in this crate was
/// unreachable from `windows/locale` and `windows/scene`, and sixteen sites
/// across those crates restated `globe_level() + 6` instead of calling it —
/// including `hornvale locale`'s and `hornvale surrounds`' production
/// `--depth` defaults, which fell a whole band behind when the offset moved
/// and reddened nothing, because the stale copies were consistent with each
/// other. The function reads nothing but `LocaleContext::globe_level`, so it
/// belongs in the crate that owns that type. The name stays exported here
/// because `hornvale_vessel::walk_depth` is what this window's own callers
/// (and `cli`) already spell.
pub use hornvale_locale::walk_depth;

/// Fail loudly if `village`'s species is unknown to the perception registry —
/// the same two-step check `mint_at` used to run ahead of minting an `Agent`,
/// now its own function since nothing mints anything any more (The Hand,
/// Task 3).
///
/// `liveness::body_at` (the shared body derivation `Session::start` and
/// `derive_npcs` both call) is deliberately infallible here: an unresolved
/// species silently falls back to the manikin's neutral perception, which is
/// exactly the tolerant behaviour `derive_npcs` always wanted for every
/// OTHER settlement's NPC — a possession's own roster should not refuse to
/// start because some unrelated settlement's species is unrecognized. The
/// DRIVEN body is different: a player commanding an unrecognized species was
/// always a loud error before The Hand, not a silent fallback to "goblin",
/// so `Session::start` calls this on the SELECTED settlement, ahead of
/// deriving the roster, to keep that contract byte-for-byte even though the
/// value this computes is thrown away in favour of `body_at`'s own
/// resolution (identical whenever this check passes).
pub(crate) fn check_species_known(world: &World, village: &VillageInfo) -> Result<(), VesselError> {
    let species = species_of(world, village.id)
        .ok_or_else(|| VesselError::NoSpecies(village.name.clone()))?;
    // `species` is free text read from the ledger (a committed `Value::Text`),
    // not a `KindId` — resolve it against the perception component registry by
    // its `KindId` label, failing loudly if unknown. Today's perception
    // roster is exactly the six settling peoples and, since The Vigil, the
    // three dragons (`check_integrity` enforces speech ⊆ perception, not the
    // converse — a future non-speaking perceiver stays expressible), so an
    // unknown or plain-fauna label fails here. A dragon label cannot reach
    // this path anyway: `species` is read from a SETTLEMENT.
    perception_registry()
        .iter()
        .find(|(k, _)| k.0 == species.as_str())
        .ok_or_else(|| VesselError::NoSpecies(species.clone()))?;
    Ok(())
}

/// The world's most-populous settlement — population descending, then id
/// ascending.
///
/// Deliberately the SAME *comparator* `ordered_for_derivation` (`liveness.rs`)
/// uses, so possession introduces no new tie-break rule. Not the same resulting
/// **order**: `ordered_for_derivation` then hoists the home settlement to the
/// front, so its first element is the home settlement and this function's is
/// the most-populous one. Only the comparator is shared.
pub fn most_populous_settlement(world: &World) -> Option<VillageInfo> {
    let mut all = hornvale_settlement::all_settlements(world);
    all.sort_by(|a, b| b.population.cmp(&a.population).then(a.id.cmp(&b.id)));
    all.into_iter().next()
}

/// A settlement's committed numeric fact, or a loud `NoPosition`.
fn number_fact(
    world: &World,
    id: hornvale_kernel::EntityId,
    predicate: &str,
) -> Result<f64, VesselError> {
    match world.ledger.value_of(id, predicate) {
        Some(Value::Number(n)) => Ok(*n),
        _ => Err(VesselError::NoPosition(format!(
            "settlement {} has no {predicate} fact",
            id.0
        ))),
    }
}

/// A settlement's lat/lon → unit-sphere position — shared so
/// `liveness::derive_npcs` (the-quickening) and `liveness::body_at` (The
/// Hand) home every derived body, possessed or not, the same way.
/// Panics if the settlement lacks committed `LATITUDE`/`LONGITUDE` facts:
/// a settlement-genesis invariant (every `is-settlement` subject gets both
/// unconditionally — `domains/settlement/src/genesis.rs`), never a runtime
/// condition reachable from a real generated world.
/// type-audit: bare-ok(coordinate: return)
pub(crate) fn settlement_position(
    world: &World,
    settlement: hornvale_kernel::EntityId,
) -> [f64; 3] {
    let lat = number_fact(world, settlement, LATITUDE)
        .unwrap_or_else(|e| panic!("settlement-genesis invariant violated: {e}"));
    let lon = number_fact(world, settlement, LONGITUDE)
        .unwrap_or_else(|e| panic!("settlement-genesis invariant violated: {e}"));
    math::unit_sphere_from_lat_lon(lat, lon)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::{Seed, World};
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
    fn a_known_species_settlement_passes_the_check() {
        let world = seam_world();
        let village = hornvale_settlement::village_info(&world).expect("seed 42 has a flagship");
        assert!(check_species_known(&world, &village).is_ok());
    }

    #[test]
    fn a_settlement_without_facts_has_no_lat_lon() {
        // World::new skips genesis: no settlement facts, so `village_info`
        // itself already returns `None` — `settlement_position` is never
        // reachable without a real settlement to pass it.
        let world = World::new(Seed(42));
        let ctx = LocaleContext::build(&world).unwrap();
        assert!(hornvale_settlement::village_info(&world).is_none());
        // `walk_depth` has no settlement dependency at all. Compared against
        // the re-export's source rather than a restated `+ 7`: this crate no
        // longer states the arithmetic anywhere, which is the whole point of
        // the move (see the re-export's doc above).
        assert_eq!(walk_depth(&ctx), hornvale_locale::walk_depth(&ctx));
    }
}
