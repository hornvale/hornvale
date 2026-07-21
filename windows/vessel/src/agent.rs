//! The Agent seam interface: an addressable individual derived lazily,
//! game-side — never committed to the ledger (spec: reversibility rule).

use crate::VesselError;
use crate::streams::VESSEL_AGENT;
use hornvale_kernel::{RoomAddr, Value, World, math};
use hornvale_locale::LocaleContext;
use hornvale_settlement::{LATITUDE, LONGITUDE, VillageInfo, village_info};
use hornvale_species::{PerceptionVector, perception_registry, species_of};

/// A minted agent's id, drawn deterministically from the world seed.
/// type-audit: bare-ok(index: 0)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AgentId(pub u64);

/// An addressable individual bound to a vantage. Derived, never stored.
/// type-audit: bare-ok(identifier-text: species)
#[derive(Debug, Clone, PartialEq)]
pub struct Agent {
    /// Deterministic id (seed + mint inputs).
    pub id: AgentId,
    /// The species whose perception this agent carries.
    pub species: String,
    /// The species' authored perception vector (the dragon-test slot).
    pub perception: PerceptionVector,
    /// Where the agent stands, at walk depth.
    pub position: RoomAddr,
    /// The settlement this agent was minted from.
    pub village: VillageInfo,
}

/// The canonical walk depth: six levels below the canonical grid — the
/// same default as `hornvale locale`.
/// type-audit: bare-ok(count: return)
pub fn walk_depth(ctx: &LocaleContext) -> u32 {
    ctx.globe_level() + 6
}

/// Mint the flagship settlement's agent: its argmax cell's containing room
/// at walk depth, its species' perception vector. Fails loudly with the
/// physical reason — generation never retries.
pub fn mint_flagship(world: &World, ctx: &LocaleContext) -> Result<Agent, VesselError> {
    let village = village_info(world).ok_or(VesselError::NoSettlement)?;
    let species = species_of(world, village.id)
        .ok_or_else(|| VesselError::NoSpecies(village.name.clone()))?;
    // `species` is free text read from the ledger (a committed `Value::Text`),
    // not a `KindId` — resolve it against the perception component registry by
    // its `KindId` label, failing loudly if unknown. Only peopled kinds carry a
    // perception row, so an unknown or fauna label fails here.
    let perception = *perception_registry()
        .iter()
        .find(|(k, _)| k.0 == species.as_str())
        .map(|(_, p)| p)
        .ok_or_else(|| VesselError::NoSpecies(species.clone()))?;
    // Same lat/lon → unit-sphere routing as `hornvale locale --at` (kernel
    // math keeps it platform-exact); shared with `settlement_position` so
    // derived NPCs (the-quickening) place themselves the same way.
    let position = settlement_position(world, village.id);
    let position = RoomAddr::containing(position, walk_depth(ctx));
    let id = AgentId(
        position
            .seed(world.seed)
            .derive_typed(VESSEL_AGENT)
            .stream()
            .next_u64(),
    );
    Ok(Agent {
        id,
        species,
        perception,
        position,
        village,
    })
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

/// A settlement's lat/lon → unit-sphere position — the same derivation
/// `mint_flagship` uses for the possessed agent's room, shared so
/// `liveness::derive_npcs` (the-quickening) homes its NPCs the same way.
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
    let (la, lo) = (lat.to_radians(), lon.to_radians());
    [
        math::cos(la) * math::cos(lo),
        math::cos(la) * math::sin(lo),
        math::sin(la),
    ]
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
    fn mint_is_deterministic_and_at_walk_depth() {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let a = mint_flagship(&world, &ctx).unwrap();
        let b = mint_flagship(&world, &ctx).unwrap();
        assert_eq!(a, b, "same world → identical mint");
        assert_eq!(a.position.depth(), walk_depth(&ctx));
    }

    #[test]
    fn the_minted_position_is_describable() {
        let world = seam_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let agent = mint_flagship(&world, &ctx).unwrap();
        let locale = ctx
            .describe(&agent.position, hornvale_kernel::WorldTime { day: 0.0 })
            .unwrap();
        assert!(!locale.biome.is_empty());
    }

    #[test]
    fn a_world_without_settlements_refuses_the_mint() {
        // World::new skips genesis: no settlement facts.
        let world = World::new(Seed(42));
        let ctx = LocaleContext::build(&world).unwrap();
        assert!(matches!(
            mint_flagship(&world, &ctx),
            Err(VesselError::NoSettlement)
        ));
    }
}
