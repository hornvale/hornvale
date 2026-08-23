//! The Vantage-query seam interface: what is observable from here, now,
//! to this agent. Refinement steps 1 and 6 only — no elaboration.

use crate::VesselError;
use crate::liveness::Npc;
use hornvale_kernel::{RoomAddr, World, WorldTime};
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
    /// The body's settlement, or a neutral fallback for a body with none.
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

/// Bundle the locale room, the settlement, and the day's sky into a vantage.
/// Pure over (world, npc, position, at).
pub fn observable(
    world: &World,
    ctx: &LocaleContext,
    npc: &Npc,
    position: &RoomAddr,
    at: WorldTime,
) -> Result<Vantage, VesselError> {
    observable_at(world, ctx, npc, position, at, None)
}

/// Whether `stratum` sits in a water-medium realm — the question is about
/// the MEDIUM a rung belongs to, never about which rung of a ladder you are
/// standing on. `None` (the surface projection) and every rock stratum
/// answer `false`; every stratum [`hornvale_climate::Realm::WATERWORLD`]
/// holds answers `true`.
/// type-audit: bare-ok(flag)
pub fn submerged_in(stratum: Option<hornvale_climate::Stratum>) -> bool {
    match stratum {
        Some(st) => hornvale_climate::Realm::WATERWORLD.strata().contains(&st),
        None => false,
    }
}

/// [`observable`], optionally from a stratum within the water column rather
/// than from the surface — the depth band's vantage.
///
/// `npc.village` is `None` for a wild creature; `Vantage::village` stays
/// non-optional (every existing reader — `knowledge.rs`'s settlement facts,
/// `focalize.rs`'s prose) assumes `Some`, so a `None` here resolves to a
/// neutral stand-in ([`crate::liveness::village_or_fallback`]) rather than
/// unwrapping. Unreached today (a driven body is always settlement-derived —
/// `derive_npcs` places it at the roster's front), but a wild body could
/// legitimately reach this once a later task admits driving one.
pub fn observable_at(
    world: &World,
    ctx: &LocaleContext,
    npc: &Npc,
    position: &RoomAddr,
    at: WorldTime,
    stratum: Option<hornvale_climate::Stratum>,
) -> Result<Vantage, VesselError> {
    let locale = ctx
        .describe_at(position, at, stratum)
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
        submerged: submerged_in(stratum),
        locale,
        day: at,
        village: crate::liveness::village_or_fallback(npc),
        sky: report.description,
        sky_bodies: report.body_phrases,
    })
}
