//! The Vantage-query seam interface: what is observable from here, now,
//! to this agent. Refinement steps 1 and 6 only — no elaboration.

use crate::VesselError;
use crate::body::Body;
use hornvale_kernel::{Facet, World, WorldTime};
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
    npc: &Body,
    position: &Facet,
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

/// Whether a body moving this way can meet what `access` demands — the
/// REACH half of The Tidemark's spec §3.8, and [`hornvale_climate::Access`]'s
/// first production reader.
///
/// # Reach is not residence, and this is the seam that keeps them apart
///
/// A kind's [`hornvale_species::HabitatRealm`] answers where it *lives*:
/// which vertices it holds, what placement scores it on, what the
/// coexistence balance governs. It does NOT answer where it can go, and the
/// availability mask it feeds is `{0.0, 1.0}` with no middle value, so
/// "amphibious" is not sayable as a realm at all. It is sayable as a
/// capability, and this is where the two halves of that capability meet: a
/// realm publishes how it is entered ([`hornvale_climate::Realm::access`]),
/// and a body publishes how it moves
/// ([`hornvale_species::locomotion_registry`]).
///
/// `sea-elf` is the case that forces the distinction and the reason this
/// function exists. It carries `SWIM` and it is **deliberately confined to
/// the shelf band in residence** — `radiation_affinity::
/// the_sea_elf_is_confined_to_the_shelf_band` pins that, because authored to
/// the whole ocean the kind would hold ~27,000 vertices against wood-elf's
/// ~800. Asking this function rather than the realm row is what lets a sea
/// elf dive the whole column and meet tritons and abyssal elves where those
/// peoples live, while holding only the shelf. Nothing here reads
/// `HabitatRealm`, and nothing here may: the moment reach consults residence
/// the separation is gone.
///
/// # Nine arms and no wildcard
///
/// `Access` has nine variants and every one is answered by name. A wildcard
/// would silently admit whatever variant an aerial realm adds next to every
/// body that walks, which is precisely the conflation this function exists
/// to end — the same reason [`crate::underground::Underground::admits`]
/// spells out its `Fly` arm rather than folding it into a catch-all.
/// type-audit: bare-ok(flag)
#[must_use]
pub fn reaches(access: hornvale_climate::Access, locomotion: hornvale_species::Locomotion) -> bool {
    use hornvale_climate::Access;
    match access {
        // Entered by descending through water. This is the one arm the
        // campaign turns on, and it makes the same demand
        // `MovementMode::Swim` already makes of a flooded underworld passage.
        //
        // **It is a claim about the COLUMN, not about getting wet**, and the
        // distinction is [`hornvale_species::Locomotion::swim`]'s own:
        // the field is documented "crosses DEEP water", and the absent row
        // it defaults from, `WALKER`, is documented "walks and WADES". So a
        // walker treads the sunlit band at the top and goes no further;
        // descending past it is what this arm refuses. See
        // [`deepest_reachable_band`], which is where that boundary is drawn
        // and where the caller actually asks.
        Access::Dive => locomotion.swim,
        // "Simply being there; the default band" — the overworld asks a body
        // for nothing.
        Access::Default => true,
        // The void exists and is unreachable, by anything, however it moves.
        Access::Sealed => false,
        // The seven rock rungs. Every one of these is a way IN THROUGH ROCK,
        // and rock asks a body for no locomotion it might lack: a walker
        // enters a cave mouth, a worked way, a gate, a shaft net, a merged
        // settlement, and seeps through a crack, exactly as a swimmer does.
        // What gates those is a door or a lantern, which is
        // `Underground::admits`'s business and not a capability of the body.
        Access::Crack
        | Access::CaveMouth
        | Access::WorkedWay
        | Access::Gate
        | Access::ShaftNet
        | Access::Merged => true,
    }
}

/// How deep into a water column of `depth` bands a body moving this way can
/// go — the one question the walk band actually asks, spelled once so the
/// `dive` verb and the `descend` menu cannot drift apart about it.
///
/// Returns the number of bands the body may occupy, counting from the top:
///
/// - a body that meets [`hornvale_climate::Realm::WATERWORLD`]'s declared
///   access reaches the whole column, `depth`;
/// - one that does not reaches **exactly one** band, the sunlit
///   [`hornvale_climate::Stratum::Epipelagic`] film at the top, because
///   `WALKER` "walks and wades" and wading is the top of the water;
/// - an empty column is zero either way, which is dry land.
///
/// **Not a bare `bool`, and the reason is a measurement.** The first draft of
/// this campaign's reach work refused the water column outright to every
/// non-swimmer, which is what "reach is a capability" sounds like it means.
/// Run against `hornvale-vessel`'s suite it reddened **12 of 1,234 tests**,
/// and only four of them were about diving: the rest were walk-determinism
/// transcripts and the committed client session fixtures, moved because a
/// body that used to enter the water no longer did. That is a larger claim
/// than spec §3.8 makes — §3.8 says a sea elf reaches the DEEP bands where
/// tritons and abyssal elves live, not that a human drowns in the shallows —
/// and `Locomotion::swim`'s own doc ("crosses deep water") was already the
/// narrower reading. The boundary is drawn here rather than in the verb so
/// that a future aerial realm asks the same shaped question.
///
/// Asked through [`reaches`] against the realm's own `access` row, never a
/// second copy of `Access::Dive` written here: the realm is the authority on
/// how it is entered, and a literal would stop tracking it the day that row
/// moved.
/// type-audit: bare-ok(count: depth), bare-ok(count: return)
#[must_use]
pub fn deepest_reachable_band(depth: usize, locomotion: hornvale_species::Locomotion) -> usize {
    if reaches(hornvale_climate::Realm::WATERWORLD.access, locomotion) {
        depth
    } else {
        depth.min(1)
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
    npc: &Body,
    position: &Facet,
    at: WorldTime,
    stratum: Option<hornvale_climate::Stratum>,
) -> Result<Vantage, VesselError> {
    let locale = ctx
        .describe_at(position, at, stratum)
        .map_err(VesselError::Locale)?;
    // The walker's own vertex, not the capital's: the sky over *here*, dimmed by
    // the weather *here*. (`at` is already this function's WorldTime, so the
    // vertex gets its own name rather than shadowing it.)
    let vertex = ctx
        .terrain()
        .nearest_vertex(locale.latitude, locale.longitude);
    let report =
        hornvale_worldgen::sky_report_from(world, at, ctx.terrain(), ctx.climate(), Some(vertex))
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
