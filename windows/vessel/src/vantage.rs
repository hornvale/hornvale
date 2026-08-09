//! The Vantage-query seam interface: what is observable from here, now,
//! to this agent. Refinement steps 1 and 6 only — no elaboration.

use crate::{Agent, VesselError};
use hornvale_kernel::{World, WorldTime};
use hornvale_locale::{Locale, LocaleContext};
use hornvale_settlement::VillageInfo;

/// Everything observable from the agent's position at `at`.
/// type-audit: bare-ok(prose: sky), bare-ok(flag: submerged), bare-ok(identifier-text: sky_bodies)
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
    /// One `(display noun, datum)` per body `sky` names, in the order the
    /// sentence names them. Carried alongside `sky` rather than parsed back
    /// out of it, because only the sky report itself knows which words are a
    /// body's own phrase.
    pub sky_bodies: Vec<(String, String)>,
    /// Whether the vantage is DOWN in the water column rather than on its
    /// surface. Land vantages are never submerged.
    pub submerged: bool,
}

/// Bundle the locale room, the mint settlement, and the day's sky into a
/// vantage. Pure over (world, agent, at).
pub fn observable(
    world: &World,
    ctx: &LocaleContext,
    agent: &Agent,
    at: WorldTime,
) -> Result<Vantage, VesselError> {
    observable_at(world, ctx, agent, at, None)
}

/// [`observable`], optionally from a stratum within the water column rather
/// than from the surface — the depth band's vantage.
pub fn observable_at(
    world: &World,
    ctx: &LocaleContext,
    agent: &Agent,
    at: WorldTime,
    stratum: Option<hornvale_climate::Stratum>,
) -> Result<Vantage, VesselError> {
    let locale = ctx
        .describe_at(&agent.position, at, stratum)
        .map_err(VesselError::Locale)?;
    // The walker's own cell, not the capital's: the sky over *here*, dimmed by
    // the weather *here*. (`at` is already this function's WorldTime, so the
    // cell gets its own name rather than shadowing it.)
    let cell = ctx
        .terrain()
        .nearest_cell(locale.latitude, locale.longitude);
    let report =
        hornvale_worldgen::sky_report_from(world, at, ctx.terrain(), ctx.climate(), Some(cell))
            .map_err(|e| VesselError::Build(e.to_string()))?;
    Ok(Vantage {
        submerged: matches!(stratum, Some(st) if st != hornvale_climate::Stratum::Surface),
        locale,
        day: at,
        village: agent.village.clone(),
        sky: report.description,
        sky_bodies: report.body_phrases,
    })
}
