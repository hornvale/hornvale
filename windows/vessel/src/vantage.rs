//! The Vantage-query seam interface: what is observable from here, now,
//! to this agent. Refinement steps 1 and 6 only — no elaboration.

use crate::{Agent, VesselError};
use hornvale_kernel::{World, WorldTime};
use hornvale_locale::{Locale, LocaleContext};
use hornvale_settlement::VillageInfo;

/// Everything observable from the agent's position at `at`.
/// type-audit: bare-ok(prose: sky)
#[derive(Debug, Clone, PartialEq)]
pub struct Vantage {
    /// The room, as the locale window describes it (ground truth).
    pub locale: Locale,
    /// The possession's frozen day.
    pub day: WorldTime,
    /// The settlement the agent was minted from.
    pub village: VillageInfo,
    /// The sky over this day, from the world's sky provider.
    pub sky: String,
}

/// Bundle the locale room, the mint settlement, and the day's sky into a
/// vantage. Pure over (world, agent, at).
pub fn observable(
    world: &World,
    ctx: &LocaleContext,
    agent: &Agent,
    at: WorldTime,
) -> Result<Vantage, VesselError> {
    let locale = ctx
        .describe(&agent.position, at)
        .map_err(VesselError::Locale)?;
    let sky = hornvale_worldgen::sky_report(world, at)
        .map_err(|e| VesselError::Build(e.to_string()))?
        .description;
    Ok(Vantage {
        locale,
        day: at,
        village: agent.village.clone(),
        sky,
    })
}
