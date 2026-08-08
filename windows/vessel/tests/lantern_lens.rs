//! The presentation lens (The Lantern, Task 8, spec §7).
//!
//! **The lens is built last on purpose, and these guards are what keep it from
//! becoming load-bearing.** Every claim in spec §6 reads *unlensed* colour: H1
//! is the claim that could genuinely have failed — if bedrock varied too little
//! every stone wall would look alike — and a saturation-boosting lens would have
//! hidden exactly that. So the lens must transform the *picture* and nothing
//! else, and it must be provable that it did not reach the model or any
//! committed artifact.
//!
//! **The measurement that constrains the transform.** H1 swept 1505 settlements
//! across eight seeds and read their stone under a 5800 K reference light. The
//! median pair differs by 41 `u8` steps — but **p10 = 1**: a tenth of settlement
//! pairs differ by a single step, because settlements cluster on shared rock
//! classes. A lens that compresses dynamic range erases that whole decile while
//! the median goes on looking fine. Two guards below check the tail rather than
//! the middle: [`the_lens_separates_every_pair_the_model_can_still_tell_apart`]
//! proves it exhaustively over the input range, and
//! `the_lens_preserves_every_pair_h1_can_barely_tell_apart` (in
//! `lantern_fabric.rs`, where the real sweep already lives) proves it on the
//! measured stone itself.

use hornvale_kernel::Seed;
use hornvale_vessel::lens::{self, HIGHLIGHT_LIFT, Lens, SEPARATION_CEILING, SHADOW_KNEE};
use hornvale_vessel::{PossessOpts, Session, SessionPlan, SpatialChannel};
use hornvale_worldgen::SkyChoice;
use std::collections::BTreeSet;

/// The chamber colours Task 6 measured on the real seam, quoted so the lens is
/// judged against the band it actually has to make legible rather than against
/// the whole `u8` range.
///
/// Walls run `[13, 13, 1]` to `[88, 88, 73]`, floors reach `[136, 136, 91]`, and
/// the dimmest *visible* cell in the sweep is `[2, 2, 0]` (H4a). Note how dark
/// and how narrow that is: the campaign's own finding is that the chamber never
/// reaches black, so what the room needs is not more contrast at the top but
/// separation down where everything is crushed together near zero.
const CHAMBER_BAND: [[u8; 3]; 4] = [[2, 2, 0], [13, 13, 1], [88, 88, 73], [136, 136, 91]];

/// The brightest channel value H1's real stone produced, measured this
/// campaign: 87 distinct triples spanning 107 to 217. The lens's saturation
/// floor must stay clear of it, or the lens would collapse pairs the model
/// worked to keep apart.
/// type-audit: bare-ok(render-internal)
const H1_BRIGHTEST_MEASURED: u8 = 217;

/// A real world's chamber plan, taken through a live possession carrying
/// `lens`.
///
/// The whole seam runs inside `Session::snapshot`, so reading the plan off a
/// session is the only way to assert on what the game actually emits — and it
/// is the only way to catch a lens that has leaked into the artifact path.
fn chamber_plan_with_lens(seed: u64, lens: Lens) -> SessionPlan {
    let world = hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .unwrap_or_else(|e| panic!("seed {seed} builds: {e:?}"));
    let (mut session, _) = Session::start(
        &world,
        &PossessOpts {
            lens,
            ..Default::default()
        },
    )
    .unwrap_or_else(|e| panic!("seed {seed} possesses: {e:?}"));
    session.handle("enter");
    match session
        .snapshot()
        .expect("a live session snapshots")
        .spatial
    {
        SpatialChannel::Chamber { plan } => plan,
        SpatialChannel::Walk { .. } => {
            panic!("seed {seed}: `enter` did not put the possession inside a building")
        }
    }
}

/// Disclosable and defeatable (`RENDER-9`). An unlensed mode is what makes this
/// a lens rather than a lie, and "unlensed" has to mean the model's own bytes —
/// not a gentler filter.
///
/// Swept over the chamber band and the two extremes rather than one colour: a
/// single-sample identity check would pass for a transform that is the identity
/// at exactly one point.
///
/// FIRES WHEN: `Lens::Off` stops being the identity anywhere.
#[test]
fn the_lens_can_be_declined() {
    for rgb in CHAMBER_BAND.iter().copied().chain([
        [0, 0, 0],
        [12, 34, 56],
        [255, 255, 255],
        [217, 217, 217],
    ]) {
        assert_eq!(
            lens::apply(&Lens::Off, rgb),
            rgb,
            "Lens::Off must hand back the model's own bytes, unchanged"
        );
    }
}

/// The lens must move the picture by an amount **no rounding could produce**.
///
/// This is the campaign's live failure class aimed straight at this task: a `u8`
/// rounding step silently absorbed a regression last campaign, and a lens whose
/// effect is under one step is indistinguishable from no lens at all — a test
/// asserting merely "the lens changed something" would pass on rounding noise.
///
/// The magnitude asserted is **8 steps** on the chamber's own wall band. Why 8:
/// one `u8` step is the quantization grain itself, so ±1 is exactly the noise
/// floor; 8 steps is eight times that floor and about 3 % of the output range —
/// large enough that no rounding, and no reordering of the arithmetic, could
/// account for it. The *measured* move on that band is +11 and +24, so the
/// assertion sits well inside the effect rather than on its edge.
///
/// FIRES WHEN: the lens is weakened to the point where it is doing nothing, or
/// silently becomes the identity.
#[test]
fn the_lens_is_a_deliberate_change_not_rounding_noise() {
    let wall = [13u8, 13, 1];
    let lit_wall = [88u8, 88, 73];
    for rgb in [wall, lit_wall] {
        let after = lens::apply(&Lens::default(), rgb);
        let moved = (0..3)
            .map(|s| u16::from(after[s]).abs_diff(u16::from(rgb[s])))
            .max()
            .expect("three slots");
        eprintln!("lens: {rgb:?} -> {after:?} ({moved} u8 steps)");
        assert!(
            moved >= 8,
            "the lens moved {rgb:?} to {after:?}, only {moved} u8 steps — a \
             change that small is indistinguishable from rounding, which is \
             the shape of a filter that ships green and does nothing"
        );
    }
}

/// **The p10 guard, stated as a property.** For every pair of inputs the model
/// can still tell apart by one `u8` step, the lens must leave them apart.
///
/// Exhaustive over `0..=SEPARATION_CEILING`, which is the whole range the model
/// produces: H1's brightest real stone is 217 and the chamber's brightest floor
/// is 136, both below the ceiling. Above the ceiling the curve saturates, and
/// the second half of this test proves that boundary is *real* rather than
/// decorative — a guard that only checked the preserved half could not tell a
/// working ceiling from a mis-stated one.
///
/// FIRES WHEN: the transform's slope drops below 1 anywhere in the model's own
/// range — the compression that would erase H1's p10 decile while its median
/// went on looking fine.
#[test]
fn the_lens_separates_every_pair_the_model_can_still_tell_apart() {
    // A `const` block, not a runtime `assert!`: both operands are constants, so
    // clippy is right that this cannot vary at run time — and pushing it to
    // compile time is strictly better than silencing the lint. The relation is
    // load-bearing, not decorative: it is the claim that the lens's one lossy
    // region lies above everything H1's real stone produced.
    const {
        assert!(
            SEPARATION_CEILING > H1_BRIGHTEST_MEASURED,
            "the lens's separation ceiling is at or below the brightest value \
             H1's real stone produced: the lens would collapse pairs the model \
             worked to keep apart"
        );
    }
    for v in 0..SEPARATION_CEILING {
        let lo = lens::apply(&Lens::default(), [v, v, v])[0];
        let hi = lens::apply(&Lens::default(), [v + 1, v + 1, v + 1])[0];
        assert!(
            hi > lo,
            "the lens collapsed {v} and {} onto {lo}: a one-step distinction \
             the model produced has been destroyed",
            v + 1
        );
    }
    // The ceiling is a real boundary, not a comfortable one. Everything above it
    // saturates, and saying so out loud is what stops a future reader from
    // reading the sweep above as "the lens never loses anything".
    let top = lens::apply(&Lens::default(), [255, 255, 255])[0];
    let at_ceiling = lens::apply(
        &Lens::default(),
        [SEPARATION_CEILING, SEPARATION_CEILING, SEPARATION_CEILING],
    )[0];
    assert_eq!(
        (top, at_ceiling),
        (255, 255),
        "the lens is documented to saturate at and above {SEPARATION_CEILING}; \
         if it no longer does, the ceiling constant is wrong"
    );
}

/// The rod carries no hue, and neither may the lens.
///
/// Seed 42's possessed bugbear is a **dichromat**: its projection puts the same
/// value in the red and green slots of every triple it emits. A lens that
/// treated the three slots as independent — a warm tint, say, biased toward red
/// — would hand that eye a hue distinction its own physiology never produced,
/// which is a lie about the observer rather than a filter over the picture.
/// This lens is therefore **one scalar curve applied identically to all three
/// slots**: whatever equality the observer put into a triple survives it.
///
/// FIRES WHEN: the lens gains a per-channel term.
#[test]
fn the_lens_gives_a_dichromat_no_hue_it_did_not_have() {
    for v in 0u8..=255 {
        for b in [0u8, 1, 37, 128, 200, 255] {
            let out = lens::apply(&Lens::default(), [v, v, b]);
            assert_eq!(
                out[0], out[1],
                "a dichromat emitted [{v}, {v}, {b}] and the lens returned \
                 {out:?}, inventing a red/green distinction the eye cannot make"
            );
        }
    }
}

/// The lens is **one-way**. It transforms the emitted triple and nothing else —
/// never the illuminant, never the reflectance, never a fact. Brightening an
/// illuminant changes the world; brightening an output changes the picture.
///
/// Asserted on the whole session snapshot rather than on the light field alone,
/// because the snapshot is the union of everything the model derived on this
/// turn: if the lens had reached upstream of `sense()` — into the light field,
/// the fabric, the observer — the two documents would differ somewhere.
///
/// FIRES WHEN: the lens is applied before `sense` rather than after.
#[test]
fn the_lens_never_touches_the_illuminant_or_the_reflectance() {
    let world = hornvale_worldgen::build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let mut document = Vec::new();
    for lens in [Lens::Off, Lens::default()] {
        let (mut session, _) = Session::start(
            &world,
            &PossessOpts {
                lens,
                ..Default::default()
            },
        )
        .expect("seed 42 possesses");
        session.handle("enter");
        document.push(hornvale_vessel::snapshot_json(
            &session.snapshot().expect("a live session snapshots"),
        ));
    }
    assert_eq!(
        document[0], document[1],
        "the lens moved something the model derived: it is not downstream of \
         sense(), and every claim in spec §6 reads colour through this path"
    );
}

/// Lensed colour must never land in a committed artifact — if it did, the lens
/// parameters would become a save-format-class contract, versioned and frozen
/// like a seed label. Screen only.
///
/// # Two assertions, because one of them cannot see the mutation that matters
///
/// The obvious guard is the *relative* one: build the plan twice, once with the
/// lens off and once with it on, and require the palettes to match. That catches
/// a lens wired to the session's own setting — and **nothing else**. Applying
/// the lens unconditionally inside `plan_of` lenses both sides equally, so the
/// comparison stays green while every committed byte is filtered. That is not
/// hypothetical: it is exactly the mutation this task prescribed, and the
/// relative assertion alone passed it.
///
/// So the second assertion is *absolute*, and it works off a structural fact
/// about the curve rather than off a comparison. The lantern curve's image is a
/// **proper subset** of `0..=255`: it maps 0 to 0 and 1 to 3, so no input
/// whatever produces a 1 or a 2. A palette containing a value outside that image
/// therefore cannot have come through the lens — whichever code path applied it.
/// The image is computed from `apply` itself rather than hard-coded, so retuning
/// the curve moves the detector with it.
///
/// The chamber does carry such values, and that is the campaign's own H4a
/// finding rather than a lucky fixture: the dimmest visible cell renders
/// `[2, 2, 0]`, and this room's darkest wall is `[13, 13, 1]`.
///
/// FIRES WHEN: a plan or snapshot is built with the lens applied — by either
/// route.
#[test]
fn a_committed_snapshot_carries_unlensed_colour() {
    let unlensed = chamber_plan_with_lens(42, Lens::Off);
    let lensed = chamber_plan_with_lens(42, Lens::default());

    let would_move = unlensed
        .palette
        .iter()
        .filter_map(|e| e.color)
        .filter(|&c| lens::apply(&Lens::default(), c) != c)
        .count();
    assert!(
        would_move > 0,
        "no palette colour in this chamber is moved by the lens, so the \
         comparison below would pass even if the lens HAD leaked into the \
         artifact — the guard would be vacuous"
    );
    eprintln!("the lens would move {would_move} of this chamber's palette colours");

    assert_eq!(
        unlensed.palette, lensed.palette,
        "the lens reached a serialized artifact by way of the session's own \
         lens setting"
    );

    let image: BTreeSet<u8> = (0..=255u8)
        .map(|v| lens::apply(&Lens::default(), [v; 3])[0])
        .collect();
    for (which, plan) in [("unlensed", &unlensed), ("lensed", &lensed)] {
        let outside: Vec<u8> = plan
            .palette
            .iter()
            .filter_map(|e| e.color)
            .flatten()
            .filter(|v| !image.contains(v))
            .collect();
        assert!(
            !outside.is_empty(),
            "every channel value in the {which} plan's palette lies inside the \
             lens's image ({} of 256 values reachable), which is what a palette \
             that has BEEN through the lens looks like. Either the lens leaked \
             into plan_of, or this chamber stopped producing the near-black \
             cells H4a measured — check which before touching this assertion.",
            image.len()
        );
    }
}

/// The curve's two segments must meet with **matching slope**, or the knee
/// becomes a visible seam and — worse — the guarantee that separation is
/// preserved at the join stops holding.
///
/// The relation is not a taste: the shadow segment lifts `SHADOW_KNEE` to
/// `SHADOW_KNEE + HIGHLIGHT_LIFT`, and its slope at the knee is
/// `(1 + LIFT/KNEE) * GAMMA`. Setting that to exactly 1 — the highlight
/// segment's slope — pins `GAMMA = KNEE / (KNEE + LIFT)`.
///
/// FIRES WHEN: someone retunes `SHADOW_KNEE` or `HIGHLIGHT_LIFT` without
/// re-deriving `SHADOW_GAMMA`, which would silently put a compressing stretch
/// into the middle of the model's range.
#[test]
fn the_two_segments_of_the_curve_meet_with_matching_slope() {
    let knee = f64::from(SHADOW_KNEE);
    let lift = f64::from(HIGHLIGHT_LIFT);
    let required = knee / (knee + lift);
    assert!(
        (lens::SHADOW_GAMMA - required).abs() < 1e-12,
        "SHADOW_GAMMA is {} but the knee requires {required}: the shadow \
         segment no longer meets the highlight segment at slope 1",
        lens::SHADOW_GAMMA
    );
    assert_eq!(
        SEPARATION_CEILING,
        255 - HIGHLIGHT_LIFT,
        "the separation ceiling is exactly where the constant lift runs out of \
         headroom; a hand-edited constant here would misreport where the lens \
         starts losing distinctions"
    );
}
