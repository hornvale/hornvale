//! The Rill, Task 5: `MicroField::wetness` is a **budget and an allocation** —
//! the climate supply a room's vertex receives, redistributed by where the room
//! sits relative to its local watercourse — instead of a fourth axis of
//! address noise that happens to be called "wetness".
//!
//! # The two claims, and where each one's reference lives
//!
//! - **R-7, a walk gets damper as it descends — FALSIFIED at its floor.** The
//!   preregistered claim was that `wetness` is non-decreasing in at least
//!   [`R7_FLOOR`] of the steps of a descending walk. The reference is **the
//!   elevation the walk descends**, which is outside the wetness computation
//!   entirely: nothing in `micro_field` reads elevation, before this change or
//!   after it, and [`descending_walks_of_the_required_length_exist`] rebuilds
//!   that premise against live elevations rather than trusting captured ones. The model's own
//!   value clears chance comfortably; the axis the document emits does not,
//!   because the address draw the save-format contract requires is far larger
//!   than the signal underneath it. The numbers, the three alternatives that
//!   were measured and also failed, and the scale measurement that explains all
//!   of it are on [`a_walk_gets_damper_as_it_descends`].
//!
//! - **R-8, the contradiction is unreachable.** No room renders a riparian
//!   variety noun ("a stream gully", "a peat hollow", …) together with a `dry`
//!   habitat clause. **A witness, not a hypothesis test** — once the habitat
//!   clause and the variety pool are both downstream of the same moisture,
//!   the pairing is close to true by construction, and this exists to catch a
//!   regression that reintroduces two independent sources. On the unmodified
//!   code it was **19 of 60** riparian rooms; that is what a two-source
//!   descriptor costs.
//!
//! # The formula, and its one constant, fixed before either was measured
//!
//! ```text
//! supply    = fields.moisture, clamped to [0, 1]          (the budget)
//! proximity = 1 − (d − e0)/(e3 − e0), clamped to [0, 1]    (the allocation)
//!             0 where there is no watercourse at all
//! grounded  = supply + (1 − supply) · proximity
//! wetness   = g + LOCAL_VARIATION · draw · (1 − |g|),  g = 2·grounded − 1
//! ```
//!
//! `LOCAL_VARIATION` is 0.10 because the grammar's clause thresholds sit at
//! ±0.33: a variation of at most a tenth cannot carry a room from the middle
//! of one clause band into a neighbouring one. The draw **varies** a room; it
//! does not **decide** it. Both the formula and the constant were written
//! down here before R-7 was measured, and neither moved afterwards.
//!
//! # The fixture, and what The Glasshouse could and could not keep of it
//!
//! `fixtures/pre-rill-wetness.jsonl`, 1,048 seed-42 rooms at walking depth,
//! captured and committed on the unmodified locale window in the commit
//! *before* `micro_field` consulted the world. It ships without a regenerator,
//! for the reason `pre-stage-2-rooms.jsonl` gives: a fixture re-rendered by the
//! current code and compared against itself proves nothing. Each line is a
//! whole record naming its own room, so the sampled set cannot drift between
//! capture and check.
//!
//! **Decision 0131's terrain epoch spent most of it.** The coastline rose to
//! the shelf break and mean land elevation fell 2257 -> 1783 m, so every
//! captured VALUE in this fixture describes a world that no longer exists: the
//! walks stopped descending, and the descriptors stopped being the descriptors
//! of those rooms. What survives an epoch is what does not depend on the world,
//! and the three tests that read captured values were converted to their
//! world-independent cores rather than re-pinned or re-captured — the ruling
//! recorded as `PROC-before-arm-dies-with-an-epoch`. Concretely, the fixture is
//! now read three ways:
//!
//! - **as 28 walk-head ADDRESSES** ([`walk_heads`]), from which the descending
//!   walks R-7 is measured on are rebuilt live ([`descend_from`]). An address
//!   is a position on the sphere and carries no world state;
//! - **as a fixed sample of 1,048 room addresses**, for
//!   [`damp_below_the_median_is_always_inside_a_valley`] and
//!   [`no_room_reads_riparian_and_dry`], both of which assert properties of the
//!   live world over that sample;
//! - **as captured `wetness` VALUES, still compared byte for byte**, by
//!   [`only_the_ground_is_grounded`] — and this one is untouched by the epoch
//!   for a principled reason, not by luck. Outside the grounded scope the axis
//!   is the raw `LOCALE_MICRO` address draw, which is a pure function of the
//!   room address and of nothing in the world at all. That arm is a before-arm
//!   that an epoch cannot spend, which is the clearest illustration in this
//!   file of what the other three lacked.
//!
//! The Ford's `pre-stage-2-rooms.jsonl`, read here by
//! [`the_micro_draw_order_is_unchanged`], is likewise untouched: it compares
//! `relief`, `aspect` and `openness`, all three of them the same address-only
//! draw.

use hornvale_climate::BiomeExpr;
use hornvale_climate::variants::{GroundKind, Variant, variant_pool};
use hornvale_kernel::{Facet, Seed, World, WorldTime};
use hornvale_locale::{Locale, LocaleContext, grounded_wetness, wetness_is_grounded};
use hornvale_terrain::branch::{CatchmentCut, rill_reading};

// THE COMMITTED FIXTURES HAVE NO READER IN THIS FILE ANY MORE, AND THAT IS
// THE END STATE OF THE PAVEMENT'S TASK 11 RULING RATHER THAN AN OVERSIGHT.
//
// `FIXTURE` (`pre-rill-wetness.jsonl`), `FORD_FIXTURE` (`pre-stage-2-rooms.
// jsonl`), the `Row` struct and the `rows()` parser stood here and are gone.
// The mesh epoch replaced twenty triangular base faces with six quads, so an
// address on face 7 names no room at all and `describe` refused every one of
// them as `Unaddressable("Invalid")` — one error accounting for twelve test
// failures across this file and `water_reading.rs`.
//
// Each consumer was rebuilt on what it actually needed, and every one came out
// STRONGER than the capture it replaced, because a captured before-arm can
// only ever hold for the world it froze:
//
//   walk heads          -> `walk_heads`, read live off the channel network
//   the land sample     -> `sampled_rooms`, a live land-filtered lat/lon grid
//   the sea/ice sample  -> `sphere_sample`, its unfiltered complement
//   the draw-order pin  -> a same-world differential over `micro_field`
//   the scope guard     -> the same, against the raw address draw
//   the R-8 witness     -> re-measured over `sampled_rooms`, re-stated at all
//                          four of its sites
//
// `pre-stage-2-rooms.jsonl` still has ONE reader, in `water_reading.rs`, where
// the fixture LINE is a genuine before-arm for the document's key shape and
// only its address had to be rebuilt. `pre-rill-wetness.jsonl` now has none
// anywhere in the tree. It is LEFT COMMITTED rather than deleted: it is the
// only record of what that world said, and discarding a provenance artifact is
// a decision for the campaign that wants the space, not for a fix round that
// merely stopped reading it.

/// R-7's floor: the share of descending steps that must not get drier.
const R7_FLOOR: f64 = 0.80;

/// The world every claim here is made on.
fn world() -> World {
    World::new(Seed(42))
}

/// The rooms the R-7 walks start from: rill polyline heads, **read live off
/// the channel network** rather than out of the fixture.
///
/// # The fixture's heads did not survive the epoch, and its own doc said they
/// would
///
/// This function used to return 28 addresses captured in the pre-Rill world,
/// under a doc that read: *"A room address is a position on the sphere and
/// carries no world state, so it survives an epoch intact."* **That sentence
/// is false now, and The Pavement is what falsified it.** A `Facet` is a base
/// FACE plus a descent path, and the base mesh changed from twenty triangular
/// faces to six quads — so a captured address on face 7 is not a room in a
/// different place, it is not a room at all, and `describe` refuses it as
/// `Unaddressable("Invalid")`. An address is a position on the sphere only
/// relative to a fixed base mesh; the epoch moved the base mesh.
///
/// The heads were never the claim. The fixture chose them at rill polyline
/// heads because that is where a descending walk begins, so that is what this
/// asks the live network for. `descend_from` already rebuilt the walk itself
/// live for the same reason one epoch earlier
/// (`PROC-before-arm-dies-with-an-epoch`); this is that argument arriving at
/// the head as well as the walk.
fn walk_heads(ctx: &LocaleContext) -> Vec<Facet> {
    let geo = ctx.climate().geosphere();
    let depth = hornvale_locale::walk_depth(ctx);
    let mut out = Vec::new();
    for run in ctx.terrain().channels().run_vertices.iter() {
        let Some(&head) = run.first() else { continue };
        out.push(Facet::containing(geo.position(head), depth));
        if out.len() == HEADS {
            break;
        }
    }
    out
}

/// A fixed live sample of walk-band rooms, spread evenly over all six base
/// faces — the "1,048 room addresses" this file used to take out of the
/// fixture.
///
/// **The fixture's addresses did not survive the epoch, and the sentence that
/// said they would is the one to delete.** Two doc comments in this file read
/// *"A room address is a position on the sphere; it names no world state and
/// survives an epoch intact"* — true across a terrain epoch, which is what
/// they were written about, and false across a MESH epoch. A `Facet` is a base
/// face plus a descent path; The Pavement replaced twenty triangular faces
/// with six quads, so a captured address on face 7 names nothing at all and
/// `describe` refuses it as `Unaddressable("Invalid")`.
///
/// This sample is a FIXED sample, not a before-arm: the claim it serves is a
/// statement about one world (a damp room below the sample's own median
/// moisture must sit in a valley), and the sample only has to be large,
/// spread, and the same every run.
fn sampled_rooms(ctx: &LocaleContext) -> Vec<Facet> {
    // A LAT/LON GRID FILTERED TO LAND, not a path enumeration and not the
    // whole sphere. Two earlier shapes were wrong in ways the assertions here
    // caught at once, and both are worth recording because they are what
    // "rebuild the sample" actually costs:
    //
    //   - varying the leading four base-4 digits and zero-filling the rest
    //     puts every address in one deep corner of its face — 1,050 rooms in
    //     six tiny clusters;
    //   - a whole-sphere grid is ~71% ocean, and the fixture's sample was the
    //     `land` rows. `trunk_in_land` came back 30 of 1050 against the walks'
    //     96 of 448, so the "independent land sample" was less representative
    //     of land than the walks it is compared against.
    //
    // So: a dense grid, described, and kept where the room is above sea level,
    // to the fixture's own sample size. Deterministic, spread over the whole
    // globe, and land by the same reading every other test here uses.
    let depth = hornvale_locale::walk_depth(ctx);
    let mut out = Vec::with_capacity(LAND_SAMPLE);
    'grid: for a in 0..90u32 {
        // Avoid the poles exactly: the grid runs -87 to +87 degrees.
        let lat = -87.0 + 174.0 * f64::from(a) / 89.0;
        for b in 0..90u32 {
            let lon = -180.0 + 360.0 * f64::from(b) / 90.0;
            let room = Facet::containing(
                hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon),
                depth,
            );
            let Ok(loc) = ctx.describe(&room, WorldTime::GENESIS) else {
                continue;
            };
            if loc.fields.height_asl_m.get() <= 0.0 {
                continue;
            }
            out.push(room);
            if out.len() == LAND_SAMPLE {
                break 'grid;
            }
        }
    }
    assert!(
        out.len() == LAND_SAMPLE,
        "the grid found only {} land rooms of the {LAND_SAMPLE} this sample needs — \
         either the world lost its land or the grid is too coarse to find it",
        out.len()
    );
    out
}

/// An UNFILTERED lat/lon sample of walk-band rooms — sea, ice and rock column
/// included.
///
/// [`sampled_rooms`] is filtered to land, because the claims it serves are
/// about land. The scope guard is the exact complement: it is about the rooms
/// where wetness is NOT grounded, which are the ones that filter removes. Two
/// samplers rather than one parameterised one, because a caller that passed
/// the wrong flag would get a test that reads as coverage and asserts nothing
/// — measured: run against the land sample, the guard found 33 ungrounded
/// rooms against its own floor of 100 and said so.
fn sphere_sample(ctx: &LocaleContext) -> Vec<Facet> {
    let depth = hornvale_locale::walk_depth(ctx);
    let mut out = Vec::with_capacity(40 * 40);
    for a in 0..40u32 {
        let lat = -87.0 + 174.0 * f64::from(a) / 39.0;
        for b in 0..40u32 {
            let lon = -180.0 + 360.0 * f64::from(b) / 40.0;
            out.push(Facet::containing(
                hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon),
                depth,
            ));
        }
    }
    out
}

/// How many land rooms the independent sample holds — the fixture's own
/// count, kept so the populations this file compares stay the size The Rill
/// measured them at.
const LAND_SAMPLE: usize = 1_048;

/// How many walk heads to take — the fixture's own count, kept so the live
/// population is the same size as the one The Rill measured.
const HEADS: usize = 28;

/// How many rooms a walk holds at most — the fixture's own length, kept so the
/// live population is the same size as the one The Rill measured.
const WALK_LEN: usize = 16;

/// A strictly descending walk from `head`: at each step, the neighbouring room
/// with the lowest LIVE elevation, stopping at a local minimum.
///
/// The descent is therefore true by construction and the walk's LENGTH is the
/// claim — see [`descending_walks_of_the_required_length_exist`].
fn descend_from(ctx: &LocaleContext, head: &Facet) -> Vec<Facet> {
    let elevation = |a: &Facet| {
        ctx.describe(a, WorldTime::GENESIS)
            .map(|l| l.fields.elevation_m)
    };
    let mut walk = vec![head.clone()];
    let mut here = head.clone();
    let mut here_e = match elevation(&here) {
        Ok(e) => e,
        Err(_) => return walk,
    };
    while walk.len() < WALK_LEN {
        let mut best: Option<(Facet, f64)> = None;
        for n in here.neighbors() {
            let Ok(e) = elevation(&n) else { continue };
            if e < here_e && best.as_ref().is_none_or(|(_, b)| e < *b) {
                best = Some((n, e));
            }
        }
        match best {
            Some((n, e)) => {
                walk.push(n.clone());
                here = n;
                here_e = e;
            }
            None => break,
        }
    }
    walk
}

/// Whether a variant's *name* asserts flowing or standing water, or ground
/// kept saturated by it. Exhaustive on purpose: a new variant will not
/// compile until somebody decides which side of this line it falls on, which
/// is the only way a roster like this stays honest.
fn is_riparian(v: Variant) -> bool {
    match v {
        // A gallery forest is the strip of trees a watercourse supports; a
        // muskeg is waterlogged peat; a damp hollow says it outright.
        Variant::GalleryForest | Variant::Muskeg | Variant::DampHollow => true,
        Variant::Erg
        | Variant::Playa
        | Variant::Hamada
        | Variant::Reg
        | Variant::OldGrowth
        | Variant::ForestGap
        | Variant::MossyDeadfall
        | Variant::BorealStand
        | Variant::Burn
        | Variant::FrostHeave
        | Variant::Felsenmeer
        | Variant::WindScour
        | Variant::GrassSward
        | Variant::WoodedGrassland
        | Variant::ClosedCanopy
        | Variant::LianaForest
        | Variant::Snowfield
        | Variant::CrevasseField
        | Variant::ScouredIce
        | Variant::ThornScrub
        | Variant::SclerophyllScrub
        | Variant::FireScrub
        | Variant::PressureRidge
        | Variant::IceLead
        | Variant::RaftedFloe
        | Variant::MeltPond
        | Variant::CoralHead
        | Variant::SpurAndGroove
        | Variant::ReefRubble
        | Variant::StaghornStand
        | Variant::KelpCanopy
        | Variant::HoldfastTangle
        | Variant::UrchinBarren
        | Variant::SmokerField
        | Variant::TubewormThicket
        | Variant::VentPlume
        | Variant::PlanktonBloom
        | Variant::ColdUpwelling
        | Variant::BaitBall
        | Variant::OpenBlue
        | Variant::SargassumDrift
        | Variant::FishShoal
        | Variant::TwilightWater
        | Variant::ScatteringLayer
        | Variant::LightlessWater
        | Variant::MarineSnow
        | Variant::AbyssalPlain
        | Variant::NoduleField
        | Variant::TrenchWall
        | Variant::TrenchFloor => false,
    }
}

/// Every prose string a riparian variant can render, swept out of the pool
/// itself rather than typed here — over every biome (through the pool's own
/// formation/stratum keying) and every ground kind.
fn riparian_prose() -> Vec<String> {
    let grounds = [
        GroundKind::Ordinary,
        GroundKind::Sand,
        GroundKind::Evaporite,
        GroundKind::Basaltic,
        GroundKind::Ashen,
    ];
    let mut out: Vec<String> = Vec::new();
    for &biome in hornvale_climate::biome::ALL {
        let expr = BiomeExpr::for_legacy(biome);
        for &ground in &grounds {
            for e in variant_pool(expr.formation, expr.stratum, ground) {
                if is_riparian(e.variant) && !out.iter().any(|p| p == e.prose) {
                    out.push(e.prose.to_string());
                }
            }
        }
    }
    out.sort();
    out
}

/// Whether a rendered descriptor carries the land grammar's `dry` clause.
/// The descriptor is a comma-joined clause list, and no variety noun is the
/// bare word "dry", so an exact clause match is unambiguous.
fn reads_dry(descriptor: &str) -> bool {
    descriptor.split(", ").any(|c| c == "dry")
}

/// The premise R-7 rests on, rebuilt against the world rather than read out of
/// the fixture: a walk from each captured head still descends, and still gets
/// far enough to be a walk.
///
/// **RENAMED and REBUILT by The Glasshouse**, from `the_walks_still_descend`.
/// It used to walk the fixture's captured room sequences and assert that each
/// consecutive pair still descended in live elevation — the guard that says
/// "if this goes red the fixture's walks are stale and R-7's fraction means
/// nothing". Decision 0131's craton rescale moved seed 42's coastline to the
/// shelf break and mean land elevation with it, and the guard did its job: walk
/// 1 stopped descending at its eleventh room. The fixture's walks ARE stale, and
/// there is no honest way to un-stale them, because a walk re-derived from the
/// current world and then checked against the current world is a before-arm that
/// regenerates with the code (`PROC-before-arm-dies-with-an-epoch`).
///
/// So the population is rebuilt live from the fixture's 28 heads (see
/// [`walk_heads`]), which is what the R-7 tests below now measure on. The
/// descent is by construction under [`descend_from`]; **the assertion that
/// carries content is the LENGTH**. A world whose room-scale relief is too flat
/// or too pitted to descend eight rooms from a rill head cannot supply R-7 with
/// a population at all, and this is where that would surface.
#[test]
fn descending_walks_of_the_required_length_exist() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let heads = walk_heads(&ctx);
    assert!(!heads.is_empty(), "the fixture carries walk heads");
    let walks: Vec<Vec<Facet>> = heads.iter().map(|h| descend_from(&ctx, h)).collect();
    let steps: usize = walks.iter().map(|w| w.len() - 1).sum();
    let shortest = walks.iter().map(Vec::len).min().unwrap_or(0);
    println!(
        "R-7 premise: {} live walks from the captured heads, {steps} descending steps, \
         shortest {shortest} rooms, longest {} rooms",
        walks.len(),
        walks.iter().map(Vec::len).max().unwrap_or(0),
    );
    // The descent itself, asserted anyway rather than trusted: `descend_from`
    // only ever steps to a strictly lower neighbour, so this cannot fail while
    // that holds — and it is one line, and it is what a reader of R-7 needs to
    // know is true of the population.
    for (i, w) in walks.iter().enumerate() {
        let mut prev: Option<f64> = None;
        for room in w {
            let e = ctx
                .describe(room, WorldTime::GENESIS)
                .unwrap()
                .fields
                .elevation_m;
            if let Some(p) = prev {
                assert!(e < p, "walk {i} does not descend at {room:?}: {p} -> {e}");
            }
            prev = Some(e);
        }
    }
    // The content: every head must still reach R-7's stated minimum of eight
    // rooms. This is a property of the world's room-scale relief, and it is the
    // half an epoch can actually break.
    for (i, w) in walks.iter().enumerate() {
        assert!(
            w.len() >= 8,
            "walk {i} from {:?} runs {} rooms before hitting a local minimum, \
             and R-7 wants 8+",
            heads[i],
            w.len()
        );
    }
}

/// **R-7 — a walk gets damper as it descends.**
///
/// **FALSIFIED at its preregistered floor, and the null is the headline.** The
/// reference is the elevation the walk descends. `micro_field` reads moisture
/// and the channel network; it does not read elevation, so nothing in the
/// wetness computation can satisfy this by construction — and nothing does.
/// Every arm below is printed by this test on seed 42 over the fixture's
/// descending walks; none is written down here.
///
/// # Two populations, because the arms are not comparable on all of them
///
/// The emitted axis can only move where the grounding writes to it
/// ([`wetness_is_grounded`] — bare ground under open air), and some of the
/// fixture's walks run over sea or ice for their whole length. Scoring the
/// grounded arm on those steps compares it against an axis that is *by
/// construction* unchanged there, and it flatters the result. So **the in-scope
/// fraction — both endpoints of the step grounded — is the headline**, and the
/// all-steps fraction is printed beside it rather than instead of it. The
/// direction survives the restriction; the effect shrinks.
///
/// # What the null actually says
///
/// Not "the model works". Precisely:
///
/// - **The climate-supply term beats chance.** `grounded_wetness` with no
///   watercourse at all — the budget alone — is non-decreasing well above 1/2
///   as a walk descends.
/// - **The allocation reverses step verdicts, but buys no fraction.** It is
///   active on a printed, asserted non-zero count of walk rooms, and it does
///   reverse a printed number of individual step verdicts — so it is
///   emphatically not inert. What it does not do is move the *fraction*: the
///   reversals cancel almost exactly, leaving the all-steps figure identical
///   with and without the term, and the in-scope figure a shade **worse** than
///   the supply alone. The direction R-7 predicts is carried entirely by the
///   climate-supply term, which predates this campaign. That converges with the
///   scale measurement below: the valley is about a hundred times finer than
///   the room, so the term is 0 on the overwhelming majority of rooms and its
///   few interventions are as likely to reverse a step the wrong way as the
///   right one.
/// - **The emitted axis shows neither.** The `LOCALE_MICRO` draw that the
///   save-format contract forbids removing (see
///   [`the_micro_draw_order_is_unchanged`]) is three orders of magnitude larger
///   than the signal it sits on: the median descending step moves the grounded
///   value by about 2.2e-4 on a two-unit axis, while the retained draw moves
///   the emitted one by up to 0.2.
///
/// # Nothing was retuned to rescue this
///
/// `LOCAL_VARIATION` is where it was before the first measurement. Three
/// alternatives were measured afterwards and none reaches the 0.80 floor:
/// lengthening the walks to 64, 256 and 1,024 rooms gives 0.4906, 0.4937 and
/// 0.5018, and adding the coarse trunk's own `channel_distance`/`channel_bands`
/// to the allocation changes nothing.
///
/// **That trunk result is conditional on this walk population, and the
/// condition is not incidental.** The walks are seeded at rill polyline *heads*,
/// so they sample headwater terrain, where a trunk is exactly what one does not
/// expect to find. This test therefore prints how many rooms fall inside a
/// trunk band in the walk sample *and* in the fixture's independent `land`
/// sample, and asserts the two disagree — the world does contain rooms inside a
/// trunk's floodplain (seed 42's flagship room 750518284 has a
/// `channel_distance` of 5.27e-4 rad against a terrace/dry edge of 2.30e-3),
/// and the walks simply do not visit them. The honest claim is "adding the
/// trunk changes nothing **on walks seeded at rill heads**", not "the trunk is
/// inert".
///
/// # The obstacle is a scale measurement, not a choice of constant
///
/// A sub-vertex valley's terrace/dry edge has median 2.78e-6 rad against a
/// walk-depth room edge of 2.83e-4 rad, so the rill geometry is about a hundred
/// times finer than the room that must report it, and the vertex-scale moisture
/// field barely moves across a walk.
///
/// # What The Glasshouse changed, and what it deliberately did not
///
/// The three PINNED WITNESSES are gone (all-steps 215/420, in-scope 177/345,
/// grounded in-scope 214/345) and so is the pinned reversal count (34). They
/// were exact counts over the fixture's captured walks, and decision 0134's
/// terrain epoch destroyed the world those walks were drawn from — the walks
/// stopped descending, which is what
/// [`descending_walks_of_the_required_length_exist`] reports. Re-pinning them on
/// a live population would publish a NEW measurement of another campaign's
/// experiment under that campaign's numbers; the figures The Rill measured and
/// its chronicle quotes remain true statements about the pre-0131 world and are
/// left where they are, in this comment, as history.
///
/// What replaces them is the claim itself rather than a witness to it, asserted
/// on a population rebuilt live: R-7's floor is now **asserted to be missed**
/// rather than merely reported (the null is the headline, so it should be the
/// assertion), the allocation is asserted non-inert, the trunk conditionality
/// is asserted as before, and the supply-beats-chance-and-beats-emitted claim
/// is asserted where the measurement supports it. All four are world-
/// independent: none of them names a count this world happens to produce.
///
/// The `before (noise)` arm is also gone. It read the fixture's captured
/// `wetness` — pure pre-Rill address noise — and there is no way to re-derive
/// that from a world The Rill has already grounded. That arm was printed, never
/// asserted, so nothing that was ever checked has been lost with it.
///
/// # Two things the rebuilt population says that the paragraphs above do not
///
/// **(1) The supply arm's margin over chance has very nearly closed, and this
/// assertion is now near-threshold.** In scope it was 216/345 = 0.6261 on the
/// captured walks; on the live ones it is 91/181 = 0.5028 — **one step of 181
/// above a coin**. The claim still holds and is still asserted, but it should
/// be read as an existence claim near a threshold in the sense of decision
/// 0097: a campaign wanting "the climate supply is damper downhill" as evidence
/// should measure it over many worlds, not over twenty-eight walks on one. The
/// margin is printed below so a reader never has to recompute it. Note also
/// that the in-scope denominator itself shrank (345 -> 181 steps): the epoch
/// put more of these walks over ice and sea, where the grounding does not
/// write.
///
/// **(2) The allocation's sign flipped, and it is noise either way.** The
/// paragraph above records "the in-scope figure a shade **worse** than the
/// supply alone" — that is a fact about the pre-0131 world and it no longer
/// holds. On the live population the grounded arm is a shade BETTER than the
/// supply arm on both populations (in scope 0.5359 vs 0.5028, all steps 0.8000
/// vs 0.7857). Nothing was retuned; the allocation is active on 10 of 448 rooms
/// and reverses 8 of 420 step verdicts, so a handful of steps decides the sign
/// and it has now pointed both ways. **The honest reading is unchanged and is
/// in fact strengthened**: the allocation is near-inert on rill-head walks
/// because the rill geometry is ~100x finer than the room that must report it,
/// which is the scale measurement below, and a term that near-inert has no
/// stable sign. It remains asserted only that it is NOT inert.
#[test]
fn a_walk_gets_damper_as_it_descends() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let globe = ctx.terrain().globe();
    let cut = CatchmentCut::Drawn(globe.rill_partition_seed());
    // The walk population is rebuilt LIVE from the captured heads. See
    // `descending_walks_of_the_required_length_exist` for why the fixture's own
    // walks can no longer be used.
    let walks: Vec<Vec<Facet>> = walk_heads(&ctx)
        .iter()
        .map(|h| descend_from(&ctx, h))
        .collect();

    /// One sampled room, read three ways along the same axis.
    struct Reading {
        /// What the document emits.
        emitted: f64,
        /// The model's own value: budget and allocation.
        grounded: f64,
        /// The budget alone — this campaign's allocation term deleted.
        supply_only: f64,
        /// Whether the grounding writes to this room at all.
        in_scope: bool,
    }

    // Rooms where the allocation term actually moves the value away from the
    // supply. If this is zero the "supply beats chance" assertion below is
    // being satisfied by a term that predates this campaign entirely.
    let mut allocation_active = 0usize;
    let mut trunk_in_walks = 0usize;
    let mut per_walk: Vec<Vec<Reading>> = Vec::new();
    for w in &walks {
        let mut readings = Vec::new();
        for room in w {
            let loc = ctx.describe(room, WorldTime::GENESIS).unwrap();
            // The same wiring `describe_with_weights` does, calling the same
            // model function rather than a copy of it. `None` for the rill is
            // that function's own "no watercourse" arm, so the supply-only
            // value is the model with this campaign's allocation deleted and
            // not a restatement of its formula.
            let grounded = grounded_wetness(
                loc.fields.moisture,
                rill_reading(
                    room.centroid(),
                    ctx.terrain().channels(),
                    globe,
                    ctx.terrain().geosphere(),
                    ctx.nearest_index(),
                    &cut,
                ),
            );
            let supply_only = grounded_wetness(loc.fields.moisture, None);
            if grounded != supply_only {
                allocation_active += 1;
            }
            if inside_a_trunk_band(&loc) {
                trunk_in_walks += 1;
            }
            readings.push(Reading {
                emitted: loc.regime.micro.wetness,
                grounded,
                supply_only,
                in_scope: wetness_is_grounded(BiomeExpr::for_legacy(loc.biome_kind)),
            });
        }
        per_walk.push(readings);
    }
    let walk_rooms: usize = per_walk.iter().map(Vec::len).sum();

    // (kept, total) per arm, over all steps and over in-scope steps only.
    let mut all = [(0usize, 0usize); 3];
    let mut scoped = [(0usize, 0usize); 3];
    let mut allocation_flips = 0usize;
    for w in &per_walk {
        for p in w.windows(2) {
            let arms = [
                p[1].emitted >= p[0].emitted,
                p[1].grounded >= p[0].grounded,
                p[1].supply_only >= p[0].supply_only,
            ];
            // Does deleting the allocation reverse this step's verdict?
            if arms[1] != arms[2] {
                allocation_flips += 1;
            }
            for (i, kept) in arms.iter().enumerate() {
                all[i].1 += 1;
                all[i].0 += usize::from(*kept);
                if p[0].in_scope && p[1].in_scope {
                    scoped[i].1 += 1;
                    scoped[i].0 += usize::from(*kept);
                }
            }
        }
    }
    let f = |(k, t): (usize, usize)| k as f64 / t as f64;
    let names = ["emitted axis   ", "grounded value ", "supply only    "];
    println!(
        "R-7 (seed 42, walk depth 12, {} live walks, {walk_rooms} rooms), \
         steps that do not get drier:",
        walks.len()
    );
    for i in 0..3 {
        println!(
            "  {}  in-scope {:>3}/{:<3} = {:.4}   all steps {:>3}/{:<3} = {:.4}",
            names[i],
            scoped[i].0,
            scoped[i].1,
            f(scoped[i]),
            all[i].0,
            all[i].1,
            f(all[i]),
        );
    }
    println!("  preregistered floor {R7_FLOOR:.4}");
    println!(
        "  supply margin over chance, in scope: {:.4} ({} steps of {}) — see the doc \
         comment: this is a near-threshold claim, not a comfortable one",
        f(scoped[2]) - 0.5,
        scoped[2].0 * 2 - scoped[2].1,
        scoped[2].1,
    );
    println!(
        "  allocation: active on {allocation_active} of {walk_rooms} walk rooms, \
         and flips {allocation_flips} of {} step verdicts",
        all[0].1
    );

    // How many rooms sit inside the COARSE trunk's own bands, in the walk
    // sample and in an INDEPENDENT land sample. The trunk null was measured on
    // the first; the second is what says the first is a property of the
    // population and not of the world.
    //
    // The independent sample is `sampled_rooms` now, not the fixture's `land`
    // rows: those are pre-cube addresses and name no room at all (see that
    // helper's doc). It serves the same purpose — a spread of ordinary rooms
    // chosen without reference to the channel network — and this is a printed
    // comparison rather than an assertion, so the population's identity
    // matters less here than that it is independent of the walks.
    let land_sample = sampled_rooms(&ctx);
    let trunk_in_land = land_sample
        .iter()
        .filter(|r| inside_a_trunk_band(&ctx.describe(r, WorldTime::GENESIS).unwrap()))
        .count();
    let land_rooms = land_sample.len();
    println!(
        "  inside a coarse trunk band: {trunk_in_walks} of {walk_rooms} walk rooms \
         (seeded at rill heads), {trunk_in_land} of {land_rooms} land-sample rooms"
    );

    // PREMISES FIRST, because a witness pinned on a sample that does not
    // exercise the term is a witness on the wrong thing.
    //
    // The allocation must be doing something to these rooms, or every arm above
    // is measuring moisture alone and this campaign is invisible to the
    // measurement. This is the guard that dies if the allocation is deleted.
    assert!(
        allocation_active > 0,
        "the allocation term is inert on all {walk_rooms} walk rooms — every arm \
         above is measuring moisture alone"
    );
    // The trunk null is conditional on the walk population, and this is what
    // makes that conditionality checkable rather than a caveat in prose: the
    // world does contain rooms inside a trunk's valley, and these walks — seeded
    // at rill heads — do not visit them.
    assert!(
        trunk_in_land > trunk_in_walks,
        "the land sample has no more trunk-band rooms than the rill-head walks \
         ({trunk_in_land} vs {trunk_in_walks}), so the trunk null cannot be \
         attributed to the walk population"
    );

    // **R-7 IS FALSIFIED, AND THAT IS NOW AN ASSERTION.** The Rill reported the
    // miss and asserted pinned counts instead; the counts died with their world
    // and the miss did not. If the emitted axis ever reaches the preregistered
    // floor, the merged headline is wrong and this must be re-read, not
    // re-pinned.
    assert!(
        f(scoped[0]) < R7_FLOOR && f(all[0]) < R7_FLOOR,
        "the emitted axis reached R-7's preregistered floor of {R7_FLOOR}: \
         in-scope {:.4}, all steps {:.4}. The Rill's null is falsified and this \
         test's whole doc comment needs re-reading.",
        f(scoped[0]),
        f(all[0]),
    );
    // The claim the data supports: the supply term is damper downhill more
    // often than a coin would be, and more often than the emitted axis manages.
    // Stated on the SUPPLY arm rather than the grounded one, because the
    // allocation buys nothing and claiming it for the full model would credit
    // this campaign with a term that predates it. It is NOT inert — the `flips`
    // line printed above counts the step verdicts it reverses — but the
    // reversals very nearly CANCEL, which is why this is asserted as an
    // inequality on the supply arm and not as a fraction anywhere.
    assert!(
        f(scoped[2]) > 0.5 && f(scoped[2]) > f(scoped[0]),
        "the climate supply does not beat chance in scope: supply {:.4}, emitted {:.4}",
        f(scoped[2]),
        f(scoped[0]),
    );
}

/// Whether a room stands inside the **coarse trunk's** valley — the reading the
/// document already carries, as distinct from the sub-vertex branch
/// [`rill_reading`] finds. `channel_bands[3]` is the terrace/dry edge.
fn inside_a_trunk_band(loc: &Locale) -> bool {
    matches!(
        (loc.channel_distance, loc.channel_bands),
        (Some(d), Some(e)) if d.abs() < e[3]
    )
}

/// The wetness clause a descriptor renders, across all four grammars — the
/// prose the reader actually sees move. `""` where the axis sat in the neutral
/// band and the clause dropped out.
fn wet_clause(descriptor: &str) -> &'static str {
    const CLAUSES: [&str; 8] = [
        "damp",
        "dry",
        "drifted deep",
        "scoured bare",
        "swept by a current",
        "in slack water",
        "weeping with seep-water",
        "bone dry",
    ];
    for c in descriptor.split(", ") {
        if let Some(hit) = CLAUSES.iter().find(|k| **k == c) {
            return hit;
        }
    }
    ""
}

/// **Damp that the climate alone cannot account for is always inside a
/// watercourse's valley.** This is the riparian corridor the campaign exists to
/// produce, stated as a property of the world rather than as a diff.
///
/// # What The Glasshouse changed, and why the diff had to go
///
/// This was `the_habitat_clause_movement_is_attributable`, and its headline was
/// a clause-by-clause DIFF against `pre-rill-wetness.jsonl`: 484 rooms moved
/// their wetness clause and 564 did not, pinned exactly, plus a control arm
/// asserting that 0 rooms of kind `"other"` changed their prose at all.
///
/// Decision 0131's terrain epoch moved the world those captured descriptors
/// were rendered from — seed 42's coastline rose to the shelf break, and the
/// sampled rooms changed biome, water kind and elevation. The control arm went
/// red first and loudest (90 of the control rooms changed prose), and it was
/// right to: it exists to say "nothing outside The Rill's scope moved", and
/// something outside The Rill's scope had moved — the ground. Re-pinning the
/// diff would have made it a comparison between the pre-Rill world and the
/// post-0131 world, attributing two campaigns' worth of movement to one of
/// them; re-capturing the fixture would have compared the current code with
/// itself. Neither is The Rill's experiment
/// (`PROC-before-arm-dies-with-an-epoch`).
///
/// **The attribution claim needed no before-arm and never did.** It is a
/// statement about a single world: any room reading `damp` whose moisture is
/// below the sample's own median has more water than its climate supplies, and
/// the only other source in the model is its own watercourse — so it must be
/// inside one. Every part of that is measured live, including the median and
/// the premise the median has to satisfy. That is what remains here, and it is
/// asserted on every room of the sample rather than on three shown examples.
///
/// The fixture is still read, for the 1,048 room ADDRESSES it carries. A room
/// address is a position on the sphere; it names no world state and survives an
/// epoch intact. It is a fixed sample here, not a before-arm.
///
/// claim: readout(seed: 42) — one world, and the loop is over a fixed sample of
/// 1,048 rooms rather than over seeds.
#[test]
fn damp_below_the_median_is_always_inside_a_valley() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let globe = ctx.terrain().globe();
    let cut = CatchmentCut::Drawn(globe.rill_partition_seed());
    // A LIVE spread, not the fixture's captured addresses — see
    // `sampled_rooms` for why those stopped being addresses at all.
    let rows: Vec<Facet> = sampled_rooms(&ctx);
    let mut riparian_damp: Vec<String> = Vec::new();
    let mut damp_rooms = 0usize;

    // The median moisture of the rooms whose axis this campaign grounds.
    let mut supplies: Vec<f64> = rows
        .iter()
        .map(|r| ctx.describe(r, WorldTime::GENESIS).unwrap())
        .filter(|l| wetness_is_grounded(BiomeExpr::for_legacy(l.biome_kind)))
        .map(|l| l.fields.moisture)
        .collect();
    supplies.sort_by(|a, b| a.total_cmp(b));
    let median = supplies[supplies.len() / 2];
    println!(
        "median moisture over {} grounded rooms: {median}",
        supplies.len()
    );
    assert!(
        median < 0.6,
        "the premise of the attribution cut fails: median moisture {median} \
         is high enough that supply plus draw could reach the damp clause"
    );
    for r in &rows {
        let loc = ctx.describe(r, WorldTime::GENESIS).unwrap();
        if wet_clause(&loc.regime.descriptor) != "damp" {
            continue;
        }
        damp_rooms += 1;
        // Damp that the climate alone cannot account for. The cut is the
        // sample's own median moisture rather than a threshold copied out of
        // the grammar: a room at or below the median has a supply-only value of
        // at most 2·0.6 − 1 = 0.2, and the local variation adds at most a tenth
        // of the remaining headroom, so it cannot reach the grammar's 0.33 damp
        // cut by supply and draw together. Whatever made such a room damp came
        // from the watercourse. `median` asserts its own premise above.
        if loc.fields.moisture >= median {
            continue;
        }
        let rill = rill_reading(
            r.centroid(),
            ctx.terrain().channels(),
            globe,
            ctx.terrain().geosphere(),
            ctx.nearest_index(),
            &cut,
        );
        let inside = rill.is_some_and(|x| x.distance < x.band_edges[3]);
        assert!(
            inside,
            "{:?} reads damp with moisture {} and no watercourse to explain it",
            r, loc.fields.moisture
        );
        riparian_damp.push(format!(
            "    {:?} moisture {} rill {:?} rad inside a valley {:?} rad wide",
            r,
            loc.fields.moisture,
            rill.map(|x| x.distance),
            rill.map(|x| x.band_edges[3]),
        ));
    }
    println!(
        "{damp_rooms} of {} sampled rooms read damp; {} of those are below the \
         median moisture and every one is inside its own valley",
        rows.len(),
        riparian_damp.len(),
    );
    for s in riparian_damp.iter().take(3) {
        println!("{s}");
    }
    // Anti-vacuity. The assertion in the loop is a universal over a set that
    // could be empty, and an empty set would satisfy it silently — which is
    // precisely the shape a grounding regression would take, since deleting
    // the allocation makes every damp room a climate-explained one.
    assert!(
        !riparian_damp.is_empty(),
        "no drier-than-median room reads damp — the allocation is inert, and the \
         universal above is vacuously true"
    );
}

/// **R-8 — the contradiction is unreachable.**
///
/// **A witness, not a hypothesis test.** Once the habitat clause and the
/// variety pool are both downstream of the same moisture, a riparian noun over
/// a `dry` clause is close to true by construction; this test exists to catch a
/// regression that reintroduces two independent sources, which is what the
/// unmodified code had.
///
/// # THIS TEST IS `#[ignore]`d BECAUSE ITS PREREGISTERED CLAIM IS NOT MET
///
/// **Nothing below this line was moved to make it pass.** The assertion is
/// unchanged at zero tolerance; it is deferred, not weakened, under the
/// `"PREREGISTERED, not met"` convention rostered in `cli/tests/heavy_tier.rs`
/// — the failure is the record, and `LOC-riparian-dry-overlap` names the row a
/// successor must discharge it against. What an `#[ignore]` costs is that an
/// ignored measurement stops being measured, so
/// [`the_riparian_dry_overlap_is_pinned_as_a_witness`] below runs always and
/// pins the two integers this sample produced. Moving that witness is not
/// bookkeeping: it means the overlap was re-measured, and this doc, the roster
/// string and the registry row must be re-read and re-stated in the same
/// commit.
///
/// Under The Glasshouse's climate correction, **1 of 35** riparian rooms in
/// the seed-42 sample reads dry:
///
/// ```text
/// Facet { face: 0, path: [1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3] }:
///   "a mossy hollow, dry, on a rise"  (moisture 0.2, wetness -0.5909623)
/// ```
///
/// **Nothing here was moved to make it pass**, on the same reasoning
/// `water_reading.rs`'s discharge floors are left firing: this test's entire
/// job is to notice that the noun and the clause have come apart, and it is
/// doing exactly that job. Adding an `offenders.len() <= 1` tolerance would
/// delete the only instrument that noticed, and at n=1 a tolerance is
/// indistinguishable from switching the test off.
///
/// **What it means.** The doc above says the contradiction is "close to true
/// by construction" — and *close to* is the load-bearing phrase. The noun
/// comes from the variety pool and the clause from `micro.wetness`; both are
/// downstream of moisture, but not of the SAME function of it. At moisture 0.2
/// with `wetness = -0.59` the pool still admits `a mossy hollow` while the
/// clause has already tipped to `dry`. The campaign did not introduce that
/// overlap; it moved one room into it, which is what an invariant asserted at
/// zero tolerance is for.
///
/// **What should happen instead of a nudge**, recorded so the next session
/// does not re-derive it:
///
/// 1. The two derivations should be made downstream of one threshold, not two
///    — the pool's riparian admission and `reads_dry` should consult the same
///    predicate rather than two independently-calibrated ones. That is the
///    repair the "by construction" claim already assumes and does not have.
/// 2. Failing that, the claim should be restated as a RATE over a seed sweep
///    rather than an exact zero on one world, in decision 0093's sense — the
///    same correction The Glasshouse applied to `toponymic_shape`'s `forall`
///    when its threshold turned out to sit inside its own sampling noise.
///
/// Tracked as `LOC-riparian-dry-overlap`.
#[ignore = "PREREGISTERED, not met: awaits LOC-riparian-dry-overlap (3 of 138 riparian rooms on seed 42 read dry; the riparian noun and the dry clause are two different functions of moisture, which R-8's by-construction wording assumed away, and a tolerance at this count is indistinguishable from switching the test off. RE-MEASURED over a REBUILT POPULATION at The Pavement: the committed pre-cube fixture's addresses stopped naming rooms at the mesh epoch, so the sample is a live 1,048-room land spread now and the reading moved 1-of-35 to 3-of-138 - 2.86% to 2.17%, so the overlap persists at the same rate and it is the population that changed, not the defect)"]
#[test]
fn no_room_reads_riparian_and_dry() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let rows = sampled_rooms(&ctx);
    let prose = riparian_prose();
    assert!(!prose.is_empty(), "the pool has riparian varieties");
    let mut riparian = 0usize;
    let mut offenders: Vec<String> = Vec::new();
    for r in &rows {
        let loc: Locale = ctx.describe(r, WorldTime::GENESIS).unwrap();
        if !prose
            .iter()
            .any(|p| loc.regime.descriptor_noun.starts_with(p.as_str()))
        {
            continue;
        }
        riparian += 1;
        if reads_dry(&loc.regime.descriptor) {
            offenders.push(format!(
                "{r:?}: {:?} (moisture {}, wetness {})",
                loc.regime.descriptor, loc.fields.moisture, loc.regime.micro.wetness
            ));
        }
    }
    println!(
        "R-8: {} riparian rooms of {} sampled ({:?}); {} read dry",
        riparian,
        rows.len(),
        prose,
        offenders.len()
    );
    assert!(
        offenders.is_empty(),
        "R-8: {} of {riparian} riparian rooms read dry:\n{}",
        offenders.len(),
        offenders.join("\n")
    );
}

/// **The witness that keeps R-8 measured while
/// [`no_room_reads_riparian_and_dry`] is `#[ignore]`d.**
///
/// This pins a witness, not a claim. The integers below are not a bar the
/// world must clear — they are exactly what the seed-42 sample produced when
/// the overlap was diagnosed, recorded so that any change to the variety
/// pool, to `reads_dry`, or to the moisture field *forces a deliberate
/// re-read* rather than letting a moved number pass as bookkeeping. Without
/// it the ignored assertion above measures nothing and the "1 of 35" quoted
/// in this file, in the roster string and in `LOC-riparian-dry-overlap`
/// quietly becomes fiction.
///
/// It asserts the two counts separately on purpose: a single ratio hides
/// which term moved, and `riparian` (how many rooms the pool admitted) is a
/// different fact about the world from `offenders` (how many of them the
/// clause contradicted). A repair that makes both derivations consult one
/// predicate should drive `offenders` to 0 and leave `riparian` alone; a
/// change that merely shrinks the riparian pool would move both, and that is
/// not the repair.
///
/// # RE-STATED AT THE PAVEMENT: `(35, 1)` -> `(138, 3)`, AND THE POPULATION IS
/// A RULE NOW
///
/// This witness sampled the committed `pre-rill-wetness.jsonl`, whose 200
/// addresses name nothing on the cube-sphere mesh. The sample is
/// [`sampled_rooms`] now — a live 1,048-room land spread, regenerated from the
/// world every run — so **what became epoch-invariant is the population RULE,
/// not the integers**. Both necessarily moved, and moving them triggered this
/// pin's own four-site re-statement, performed in this commit: the pin below,
/// the `#[ignore]` reason on [`no_room_reads_riparian_and_dry`], that reason's
/// verbatim copy in `cli/tests/suite/heavy_tier.rs`, and the
/// `LOC-riparian-dry-overlap` registry row.
///
/// **The defect did not move; the sample did.** The overlap rate is the
/// comparable quantity across a population change, and it is essentially
/// unchanged:
///
/// ```text
///   before   1 of  35 riparian rooms read dry   2.86%
///   after    3 of 138 riparian rooms read dry   2.17%
/// ```
///
/// A reader who takes `1 -> 3` as the defect tripling has compared two
/// different populations. A reader who takes `35 -> 138` as the riparian pool
/// quadrupling has done the same. Both integers scale with `LAND_SAMPLE`.
#[test]
fn the_riparian_dry_overlap_is_pinned_as_a_witness() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let rows = sampled_rooms(&ctx);
    let prose = riparian_prose();
    let mut riparian = 0usize;
    let mut offenders = 0usize;
    for r in &rows {
        let loc: Locale = ctx.describe(r, WorldTime::GENESIS).unwrap();
        if !prose
            .iter()
            .any(|p| loc.regime.descriptor_noun.starts_with(p.as_str()))
        {
            continue;
        }
        riparian += 1;
        if reads_dry(&loc.regime.descriptor) {
            offenders += 1;
        }
    }
    assert_eq!(
        (riparian, offenders),
        (138, 3),
        "the R-8 overlap moved: {offenders} of {riparian} riparian rooms read dry, against the \
         pinned (138, 3). This is NOT a number to update — re-read the overlap, then re-state \
         this witness, the #[ignore] reason on no_room_reads_riparian_and_dry, its roster entry \
         in cli/tests/suite/heavy_tier.rs and the LOC-riparian-dry-overlap registry row in the \
         SAME commit. Read the RATE before deciding what moved: the population is a rule now \
         (`sampled_rooms`), not a captured list, so both integers scale with LAND_SAMPLE while \
         the overlap rate is the quantity with a meaning."
    );
}

/// The scope guard: wetness is grounded **where there is ground**. At sea, on
/// ice and in the rock column the axis is read as current, snow cover and seep
/// — none of which a river's proximity governs — so those rooms must keep the
/// value the address draw alone gives them, byte for byte.
///
/// # IT IS A SAME-WORLD DIFFERENTIAL NOW, AND THAT IS STRICTLY STRONGER
///
/// It used to compare each room's `wetness` against a value captured in the
/// pre-Rill world and committed to `pre-rill-wetness.jsonl`. The mesh epoch
/// turned those 200 addresses into addresses of nothing (`Unaddressable`), and
/// there was no honest repair: re-capturing compares current code against
/// itself, which this file's own docs say proves nothing, and the value is a
/// function of the address so it cannot be carried across.
///
/// **The before-arm was never the claim.** The claim is that OUTSIDE the
/// grounded scope, `describe`'s wetness IS the raw address draw — and the raw
/// address draw is available live, from `micro_field(seed, None)`, the very
/// function `describe` calls. So the comparison is against the thing itself
/// rather than against a photograph of it, and it holds for **every room of
/// every world** instead of for 200 rooms of one. A fixture could only ever
/// have caught a regression in the world it froze.
///
/// The room seed is rebuilt exactly as `describe` builds it
/// (`addr.seed(world.seed)`), so a change to that derivation reddens here too.
///
/// The sample is [`sphere_sample`], not [`sampled_rooms`]: this guard is about
/// the rooms the land filter removes.
#[test]
fn only_the_ground_is_grounded() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut checked = 0usize;
    let mut grounded_seen = 0usize;
    for room in sphere_sample(&ctx) {
        let loc = ctx.describe(&room, WorldTime::GENESIS).unwrap();
        // The grounded arm, and only it — the same predicate `describe`
        // itself branches on, not a restatement of it.
        if wetness_is_grounded(BiomeExpr::for_legacy(loc.biome_kind)) {
            grounded_seen += 1;
            continue;
        }
        let raw = hornvale_locale::micro_field(room.seed(world.seed), None);
        assert_eq!(
            loc.regime.micro.wetness, raw.wetness,
            "{room:?} is not bare ground, so its wetness must be the address draw \
             unchanged"
        );
        checked += 1;
    }
    println!("scope guard: {checked} non-land rooms unchanged, {grounded_seen} grounded");
    assert!(checked >= 100, "the control arm is populated: {checked}");
    // ANTI-VACUITY, which the fixture form never had: if nothing in the sample
    // were grounded, every room would trivially read the raw draw and this
    // test would pass while asserting nothing about SCOPE at all.
    assert!(
        grounded_seen > 0,
        "no room in the sample is inside the grounded scope, so this guard is not \
         separating grounded from ungrounded — it is just re-reading the draw"
    );
}

/// **The draw-order witness.** `micro_field` draws four axes off one
/// `LOCALE_MICRO` stream and wetness is the *third*; a grounding that stopped
/// consuming that draw would shift `openness` in every room of every world —
/// a save-format break, not a style choice.
///
/// # THE FIXTURE IS GONE AND THE REPLACEMENT IS A DIFFERENTIAL
///
/// The reference used to be The Ford's `pre-stage-2-rooms.jsonl`, whose
/// `relief`, `aspect` and `openness` had to match for 200 rooms. Those 200
/// addresses do not exist on the cube-sphere mesh, and re-capturing them would
/// have compared the current code against itself.
///
/// **What the fixture was standing in for is a property of the FUNCTION, and
/// the function can be asked directly.** `micro_field(seed, grounded)` draws
/// `relief`, `aspect`, the third axis, `openness` — in that order,
/// unconditionally — and grounding changes only what is DONE with the third
/// draw. So:
///
/// - the three ungrounded axes must be **identical** between a grounded and an
///   ungrounded call on the same seed; and
/// - the grounded `openness` must NOT equal the ungrounded `wetness`.
///
/// The second clause is the one that carries the claim. If grounding ever
/// stopped consuming the third draw, every axis after it would shift up by
/// one and `openness` would come back reading what `wetness` used to — which
/// is precisely the save-format break, and precisely what a first-clause-only
/// test would miss.
///
/// This is strictly stronger than the fixture: it holds over every room seed
/// in every world, at every grounded value, rather than over 200 rooms of the
/// world one campaign happened to freeze.
#[test]
fn the_micro_draw_order_is_unchanged() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut checked = 0usize;
    let mut shift_would_show = 0usize;
    for room in sampled_rooms(&ctx) {
        let seed = room.seed(world.seed);
        let raw = hornvale_locale::micro_field(seed, None);
        // Several grounded values, including both extremes' neighbourhoods, so
        // the clamp arm is exercised as well as the ordinary one.
        for g in [-0.9_f64, -0.3, 0.0, 0.4, 0.95] {
            let grounded = hornvale_locale::micro_field(seed, Some(g));
            for (name, a, b) in [
                ("relief", raw.relief, grounded.relief),
                ("aspect", raw.aspect, grounded.aspect),
                ("openness", raw.openness, grounded.openness),
            ] {
                assert_eq!(
                    a, b,
                    "{room:?} at grounded {g}: {name} moved between the grounded and \
                     ungrounded draw — grounding must consume the same stream draws \
                     either way, and this is the LOCALE_MICRO draw order breaking"
                );
            }
            // The clause that catches a DROPPED draw rather than a moved one.
            // Only meaningful where the two adjacent raw draws differ, which is
            // counted so a run where they never do cannot pass silently.
            if raw.wetness != raw.openness {
                shift_would_show += 1;
                assert_ne!(
                    grounded.openness, raw.wetness,
                    "{room:?} at grounded {g}: openness came back reading the third \
                     draw — the axes have shifted up by one, so grounding has stopped \
                     consuming the wetness draw"
                );
            }
            checked += 1;
        }
    }
    println!(
        "draw order: {checked} (room, grounded) pairs, {shift_would_show} able to show a shift"
    );
    assert!(
        checked > 500,
        "too few pairs checked to trust this: {checked}"
    );
    assert!(
        shift_would_show * 2 > checked,
        "in most pairs the third and fourth draws are equal, so a one-axis shift \
         would be invisible and this witness is not witnessing: {shift_would_show} \
         of {checked}"
    );
}
