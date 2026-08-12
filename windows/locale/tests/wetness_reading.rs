//! The Rill, Task 5: `MicroField::wetness` is a **budget and an allocation** —
//! the climate supply a room's cell receives, redistributed by where the room
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
//!   after it, and [`the_walks_still_descend`] re-checks that premise against
//!   live elevations rather than trusting the captured ones. The model's own
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
//! # The fixture
//!
//! `fixtures/pre-rill-wetness.jsonl`, 1,048 seed-42 rooms at walking depth,
//! captured and committed on the unmodified locale window in the commit
//! *before* `micro_field` consulted the world. It ships without a regenerator,
//! for the reason `pre-stage-2-rooms.jsonl` gives: a fixture re-rendered by the
//! current code and compared against itself proves nothing. Each line is a
//! whole record naming its own room, so the sampled set cannot drift between
//! capture and check.

use hornvale_climate::variants::{GroundKind, Variant, variant_pool};
use hornvale_climate::{BiomeExpr, Formation, Medium};
use hornvale_kernel::{RoomAddr, Seed, World, WorldTime};
use hornvale_locale::{Locale, LocaleContext, grounded_wetness};
use hornvale_terrain::branch::{CatchmentCut, rill_reading};
use serde_json::Value;

/// The before-arm: one record per sampled room.
const FIXTURE: &str = include_str!("fixtures/pre-rill-wetness.jsonl");

/// The Ford's before-arm, reused here for one thing only: it carries
/// `relief`, `aspect` and `openness` for 200 rooms, captured by a different
/// campaign long before this one, which is what makes it a credible witness
/// that the `LOCALE_MICRO` draw order did not move.
const FORD_FIXTURE: &str = include_str!("fixtures/pre-stage-2-rooms.jsonl");

/// R-7's floor: the share of descending steps that must not get drier.
const R7_FLOOR: f64 = 0.80;

/// One captured room.
struct Row {
    kind: String,
    id: u64,
    step: u64,
    room: RoomAddr,
    wetness: f64,
    descriptor: String,
    height_asl_m: f64,
    biome: String,
}

impl Row {
    /// The biome the room read at capture, prefixed by its sampling bucket —
    /// enough to place a spot-checked room without re-deriving anything.
    fn biome_or_kind(&self) -> String {
        format!("{} / {}", self.kind, self.biome)
    }
}

/// Parse the fixture once.
fn rows() -> Vec<Row> {
    FIXTURE
        .lines()
        .map(|l| {
            let v: Value = serde_json::from_str(l).expect("fixture line is JSON");
            Row {
                kind: v["kind"].as_str().expect("kind").to_string(),
                id: v["id"].as_u64().expect("id"),
                step: v["step"].as_u64().expect("step"),
                room: RoomAddr {
                    face: v["face"].as_u64().expect("face") as u8,
                    path: v["path"]
                        .as_array()
                        .expect("path")
                        .iter()
                        .map(|d| d.as_u64().expect("path digit") as u8)
                        .collect(),
                },
                wetness: v["wetness"].as_f64().expect("wetness"),
                descriptor: v["descriptor"].as_str().expect("descriptor").to_string(),
                height_asl_m: v["height_asl_m"].as_f64().expect("height_asl_m"),
                biome: v["biome"].as_str().expect("biome").to_string(),
            }
        })
        .collect()
}

/// The world every claim here is made on.
fn world() -> World {
    World::new(Seed(42))
}

/// The fixture's walks, each ordered by step.
fn walks(rows: &[Row]) -> Vec<Vec<&Row>> {
    let mut out: Vec<Vec<&Row>> = Vec::new();
    for r in rows.iter().filter(|r| r.kind == "walk") {
        while out.len() <= r.id as usize {
            out.push(Vec::new());
        }
        out[r.id as usize].push(r);
    }
    for w in &mut out {
        w.sort_by_key(|r| r.step);
    }
    out
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

/// The premise R-7 rests on, checked against the world rather than against the
/// fixture's own captured numbers: every walk still descends, strictly, at
/// every step. If this goes red the fixture's walks are stale and R-7's
/// fraction means nothing.
#[test]
fn the_walks_still_descend() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let rows = rows();
    let walks = walks(&rows);
    assert!(!walks.is_empty(), "the fixture carries walks");
    let mut steps = 0usize;
    for (i, w) in walks.iter().enumerate() {
        assert!(w.len() >= 8, "walk {i} is {} rooms, R-7 wants 8+", w.len());
        let mut prev: Option<f64> = None;
        for r in w {
            let loc = ctx.describe(&r.room, WorldTime::GENESIS).unwrap();
            let e = loc.fields.elevation_m;
            if let Some(p) = prev {
                assert!(
                    e < p,
                    "walk {i} does not descend at {:?}: {p} -> {e}",
                    r.room
                );
                steps += 1;
            }
            prev = Some(e);
        }
    }
    println!(
        "R-7 premise: {} walks, {steps} descending steps",
        walks.len()
    );
}

/// **R-7 — a walk gets damper as it descends.**
///
/// **FALSIFIED at its preregistered floor, and the null is the headline.** The
/// reference is the elevation the walk descends. `micro_field` reads moisture
/// and the channel network; it does not read elevation, so nothing in the
/// wetness computation can satisfy this by construction — and nothing does.
/// Measured on seed 42, over the fixture's 28 descending walks:
///
/// | arm | non-decreasing steps |
/// |---|---|
/// | the emitted axis, before this campaign (pure address noise) | 210/420 |
/// | the emitted axis, as shipped | printed below |
/// | [`grounded_wetness`] alone, without the retained draw | printed below |
///
/// **Two findings, and they point opposite ways.** The grounded value *does*
/// get damper as a walk descends, well above the 1/2 a coin gives — the model
/// works. The *emitted* axis does not, because the `LOCALE_MICRO` draw that
/// the save-format contract forbids removing (see
/// [`the_micro_draw_order_is_unchanged`]) is three orders of magnitude larger
/// than the signal it sits on: the median step moves the grounded value by
/// about 2.2e-4 on a two-unit axis, while the retained draw moves the emitted
/// one by up to 0.2.
///
/// **Nothing was retuned to rescue this.** `LOCAL_VARIATION` is where it was
/// before the first measurement. Three alternatives were measured and none
/// reaches the 0.80 floor either: including the coarse trunk's own
/// `channel_distance`/`channel_bands` in the allocation changes nothing at all
/// (0 of 448 walk rooms fall inside a trunk band), and lengthening the walks to
/// 64, 256 and 1,024 rooms gives 0.4906, 0.4937 and 0.5018. The obstacle is
/// physical, not a choice of constant: a sub-cell valley's terrace/dry edge has
/// median 2.78e-6 rad against a walk-depth room edge of 2.83e-4 rad, so the
/// rill geometry is about a hundred times finer than the room that must report
/// it, and the cell-scale moisture field barely moves across a walk.
///
/// The assertions below are therefore the two that the measurement supports: a
/// pinned witness on the emitted fraction, and the real, non-vacuous claim that
/// the grounded value beats chance. R-7's own floor is reported, not asserted.
#[test]
fn a_walk_gets_damper_as_it_descends() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let globe = ctx.terrain().globe();
    let cut = CatchmentCut::Drawn(globe.rill_partition_seed());
    let rows = rows();
    let walks = walks(&rows);

    let (mut total, mut emitted, mut ground, mut before) = (0usize, 0usize, 0usize, 0usize);
    for w in &walks {
        let mut pairs: Vec<(f64, f64, f64)> = Vec::new();
        for r in w {
            let loc = ctx.describe(&r.room, WorldTime::GENESIS).unwrap();
            // The same wiring `describe_with_weights` does, calling the same
            // model function rather than a copy of it.
            let g = grounded_wetness(
                loc.fields.moisture,
                rill_reading(
                    r.room.centroid(),
                    ctx.terrain().channels(),
                    globe,
                    ctx.terrain().geosphere(),
                    ctx.nearest_index(),
                    &cut,
                ),
            );
            pairs.push((loc.regime.micro.wetness, g, r.wetness));
        }
        for p in pairs.windows(2) {
            total += 1;
            if p[1].0 >= p[0].0 {
                emitted += 1;
            }
            if p[1].1 >= p[0].1 {
                ground += 1;
            }
            if p[1].2 >= p[0].2 {
                before += 1;
            }
        }
    }
    let f_emitted = emitted as f64 / total as f64;
    let f_ground = ground as f64 / total as f64;
    println!(
        "R-7 (seed 42, walk depth 12, {} walks, {total} descending steps), \
         steps that do not get drier:\n  \
         before this campaign  {before}/{total} = {:.4}\n  \
         emitted axis          {emitted}/{total} = {f_emitted:.4}\n  \
         grounded value alone  {ground}/{total} = {f_ground:.4}\n  \
         preregistered floor   {R7_FLOOR:.4} — NOT MET by the emitted axis",
        walks.len(),
        before as f64 / total as f64,
    );
    // The pinned witness: the emitted fraction is deterministic, so it is a
    // change detector on a number this campaign now owns.
    assert_eq!(
        (emitted, total),
        (215, 420),
        "the emitted R-7 fraction moved from the value The Rill measured"
    );
    // The real claim: the grounded value is damper downhill more often than a
    // coin would be, and more often than the emitted axis manages.
    assert!(
        f_ground > 0.5 && f_ground > f_emitted,
        "the grounding does not beat chance: grounded {f_ground:.4}, emitted {f_emitted:.4}"
    );
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

/// The prose movement this campaign bought, against the committed before-arm:
/// how many sampled rooms changed their wetness clause, and in which direction.
///
/// **The net direction is toward `dry`, and that is the point.** A uniform draw
/// made a third of every world damp and a third dry with no regard for the
/// climate; a room's moisture is usually well below the `damp` threshold, so
/// grounding the axis takes the damp clause away from most rooms and gives it
/// to the ones that have water. What replaces the lost damp is not a uniformly
/// drier world but an *attributable* one: the assertion below is that rooms
/// still read `damp` in country the climate alone would leave dry, and that
/// each such room is inside its own watercourse's valley.
///
/// Every count is computed and printed rather than written down, so none of
/// them can drift from the population it describes. Three rooms that moved from
/// `dry` to `damp` are shown against the two numbers the new value is made of.
///
/// claim: readout(seed: 42) — one world, and the loop is over the fixture's
/// 1,048 rooms rather than over seeds; the pinned movement count is a witness
/// on that committed before-arm, not a claim about worlds in general.
#[test]
fn the_habitat_clause_movement_is_attributable() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let globe = ctx.terrain().globe();
    let cut = CatchmentCut::Drawn(globe.rill_partition_seed());
    let rows = rows();
    let mut moves: Vec<(String, String, usize)> = Vec::new();
    let mut unchanged = 0usize;
    let mut spot: Vec<String> = Vec::new();
    let mut riparian_damp: Vec<String> = Vec::new();
    let mut control = 0usize;

    // The median moisture of the rooms whose axis this campaign grounds.
    let mut supplies: Vec<f64> = rows
        .iter()
        .map(|r| ctx.describe(&r.room, WorldTime::GENESIS).unwrap())
        .filter(|l| {
            let e = BiomeExpr::for_legacy(l.biome_kind);
            e.realm.medium == Medium::AirOverRock && e.formation != Formation::Ice
        })
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
        let loc = ctx.describe(&r.room, WorldTime::GENESIS).unwrap();
        let (was, now) = (
            wet_clause(&r.descriptor),
            wet_clause(&loc.regime.descriptor),
        );
        if r.kind == "other" && loc.regime.descriptor != r.descriptor {
            control += 1;
        }
        // Damp that the climate alone cannot account for. The cut is the
        // sample's own median moisture rather than a threshold copied out of
        // the grammar: a room at or below the median has a supply-only value of
        // at most 2·0.6 − 1 = 0.2, and the local variation adds at most a tenth
        // of the remaining headroom, so it cannot reach the grammar's 0.33 damp
        // cut by supply and draw together. Whatever made such a room damp came
        // from the watercourse. `median` asserts its own premise below.
        if now == "damp" && loc.fields.moisture < median {
            let rill = rill_reading(
                r.room.centroid(),
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
                r.room, loc.fields.moisture
            );
            riparian_damp.push(format!(
                "    {:?} moisture {} rill {:?} rad inside a valley {:?} rad wide",
                r.room,
                loc.fields.moisture,
                rill.map(|x| x.distance),
                rill.map(|x| x.band_edges[3]),
            ));
        }
        if was == now {
            unchanged += 1;
            continue;
        }
        match moves.iter_mut().find(|(a, b, _)| a == was && b == now) {
            Some(e) => e.2 += 1,
            None => moves.push((was.to_string(), now.to_string(), 1)),
        }
        if was == "dry" && now == "damp" && spot.len() < 3 {
            let rill = rill_reading(
                r.room.centroid(),
                ctx.terrain().channels(),
                globe,
                ctx.terrain().geosphere(),
                ctx.nearest_index(),
                &cut,
            );
            spot.push(format!(
                "  {:?} [{}]\n    was {:?}\n    now {:?}\n    \
                 moisture {} (the budget), rill distance {:?} rad against a \
                 terrace/dry edge of {:?} rad (the allocation), \
                 trunk distance {:?} rad, height {} m",
                r.room,
                r.biome_or_kind(),
                r.descriptor,
                loc.regime.descriptor,
                loc.fields.moisture,
                rill.map(|x| x.distance),
                rill.map(|x| x.band_edges[3]),
                loc.channel_distance,
                r.height_asl_m,
            ));
        }
    }
    moves.sort_by(|a, b| b.2.cmp(&a.2).then(a.0.cmp(&b.0)).then(a.1.cmp(&b.1)));
    let moved: usize = moves.iter().map(|(_, _, n)| n).sum();
    println!(
        "clause movement over {} sampled rooms: {moved} moved, {unchanged} unchanged",
        rows.len()
    );
    for (a, b, n) in &moves {
        println!("  {n:>4}  {a:?} -> {b:?}");
    }
    println!("three rooms that moved from dry to damp:");
    for s in &spot {
        println!("{s}");
    }
    let to_damp: usize = moves
        .iter()
        .filter(|(_, b, _)| b == "damp")
        .map(|(_, _, n)| n)
        .sum();
    let from_damp: usize = moves
        .iter()
        .filter(|(a, _, _)| a == "damp")
        .map(|(_, _, n)| n)
        .sum();
    println!("  damp gained by {to_damp} rooms, lost by {from_damp}");
    println!(
        "  {} rooms below median moisture read damp (the climate cannot account for them)",
        riparian_damp.len()
    );
    for s in riparian_damp.iter().take(3) {
        println!("{s}");
    }
    assert_eq!(
        control, 0,
        "{control} control-arm rooms changed their prose"
    );
    // The pinned witness: this is a deterministic count on a committed
    // before-arm, so it is a change detector on the campaign's headline.
    assert_eq!(
        (moved, unchanged),
        (484, 564),
        "the clause movement against the committed before-arm shifted"
    );
    // The claim: the damp clause survives where the climate alone cannot
    // account for it, and every such room is inside its own valley. This is
    // the riparian corridor the campaign exists to produce.
    assert!(
        !riparian_damp.is_empty(),
        "no drier-than-median room reads damp — the allocation is inert"
    );
    assert_eq!(
        spot.len(),
        3,
        "three dry-to-damp rooms were available to show"
    );
}

/// **R-8 — the contradiction is unreachable.**
///
/// **A witness, not a hypothesis test.** Once the habitat clause and the
/// variety pool are both downstream of the same moisture, a riparian noun over
/// a `dry` clause is close to true by construction; this test exists to catch a
/// regression that reintroduces two independent sources, which is what the
/// unmodified code had.
#[test]
fn no_room_reads_riparian_and_dry() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let rows = rows();
    let prose = riparian_prose();
    assert!(!prose.is_empty(), "the pool has riparian varieties");
    let mut riparian = 0usize;
    let mut offenders: Vec<String> = Vec::new();
    for r in &rows {
        let loc: Locale = ctx.describe(&r.room, WorldTime::GENESIS).unwrap();
        if !prose
            .iter()
            .any(|p| loc.regime.descriptor_noun.starts_with(p.as_str()))
        {
            continue;
        }
        riparian += 1;
        if reads_dry(&loc.regime.descriptor) {
            offenders.push(format!(
                "{:?}: {:?} (moisture {}, wetness {})",
                r.room, loc.regime.descriptor, loc.fields.moisture, loc.regime.micro.wetness
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

/// The scope guard: wetness is grounded **where there is ground**. At sea, on
/// ice and in the rock column the axis is read as current, snow cover and seep
/// — none of which a river's proximity governs — so those rooms must keep the
/// value the address draw alone gives them, byte for byte.
#[test]
fn only_the_ground_is_grounded() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let rows = rows();
    let mut checked = 0usize;
    for r in &rows {
        let loc = ctx.describe(&r.room, WorldTime::GENESIS).unwrap();
        let expr = BiomeExpr::for_legacy(loc.biome_kind);
        // The grounded arm, and only it: bare ground under open air. Ice is
        // land too, but its clause is drift and scour rather than damp and
        // dry, so a river's proximity has nothing to say about it.
        if expr.realm.medium == Medium::AirOverRock && expr.formation != Formation::Ice {
            continue;
        }
        assert_eq!(
            loc.regime.micro.wetness, r.wetness,
            "{:?} is not bare ground, so its wetness must be the address draw unchanged",
            r.room
        );
        checked += 1;
    }
    println!("scope guard: {checked} non-land rooms unchanged");
    assert!(checked >= 100, "the control arm is populated: {checked}");
}

/// **The draw-order witness.** `micro_field` draws four axes off one
/// `LOCALE_MICRO` stream and wetness is the *third*; a grounding that stopped
/// consuming that draw would shift `openness` in every room of every world —
/// a save-format break, not a style choice.
///
/// The reference is The Ford's `pre-stage-2-rooms.jsonl`, captured by another
/// campaign before this one existed. Its `relief`, `aspect` and `openness` must
/// still match exactly, for all 200 rooms, which they can only do if the draw
/// order is untouched.
#[test]
fn the_micro_draw_order_is_unchanged() {
    let world = world();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut checked = 0usize;
    for line in FORD_FIXTURE.lines() {
        let v: Value = serde_json::from_str(line).expect("fixture line is JSON");
        let room = RoomAddr {
            face: v["face"].as_u64().expect("face") as u8,
            path: v["path"]
                .as_array()
                .expect("path")
                .iter()
                .map(|d| d.as_u64().expect("path digit") as u8)
                .collect(),
        };
        let micro = ctx
            .describe(&room, WorldTime::GENESIS)
            .unwrap()
            .regime
            .micro;
        let old = &v["regime"]["micro"];
        for (name, now) in [
            ("relief", micro.relief),
            ("aspect", micro.aspect),
            ("openness", micro.openness),
        ] {
            assert_eq!(
                now,
                old[name].as_f64().expect(name),
                "{room:?}: {name} moved — the LOCALE_MICRO draw order changed"
            );
        }
        checked += 1;
    }
    println!("draw order: {checked} rooms x 3 unmoved axes");
    assert_eq!(checked, 200, "the whole Ford fixture was checked");
}
