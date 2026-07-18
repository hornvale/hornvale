//! The Projection seam interface and its subset contract. Tier 0 is the
//! identity projection; knowledge accumulates per session (observation
//! history, metaplan §3.2). Fog, inference, false belief: The Vessel's.

use crate::Vantage;
use hornvale_kernel::{RoomId, Value, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_species::PerceptionVector;
use std::collections::BTreeMap;

/// What an agent knows: key → value, every entry re-derivable from ground
/// truth (the subset contract).
/// type-audit: bare-ok(artifact: 0)
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Knowledge(pub BTreeMap<String, String>);

impl Knowledge {
    /// Fold another projection into this knowledge (walking accumulates).
    pub fn absorb(&mut self, other: Knowledge) {
        self.0.extend(other.0);
    }
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

/// The subset contract, mechanically: every entry must re-derive from the
/// world. `room/<id>` re-describes through the locale window; settlement
/// entries re-read the ledger. Unknown key shapes are violations.
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
}
