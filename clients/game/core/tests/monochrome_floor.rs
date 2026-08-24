//! The monochrome floor, end to end: with `NO_COLOR` in force, the FULL
//! composed screen must be identical — glyph for glyph, weight for weight,
//! ink for ink — to the screen rendered with colour available. Colour may
//! fail; nothing else may move.
//!
//! Each committed fixture (the walk band and the chamber band) is rendered
//! twice through [`hornvale_game_core::render_with`], once per colour
//! regime, and the two grids compared cell by cell. A companion assertion
//! walks the COLOURED render for at least one non-`Plain` ink, so a world
//! where colour never resolves cannot pass vacuously by producing two
//! identical Plain screens.

use hornvale_game_core::{CommandLine, Focus, Grid, Ink};

const WALK: &str = include_str!("fixtures/session-seed-42-turn-0.json");
const CHAMBER: &str = include_str!("fixtures/session-seed-42-chamber.json");

/// A comfortable size — well above the 80×24 floor, so the plate, entry
/// pane and strip all have room.
const W: u16 = 100;
const H: u16 = 30;

/// Serialises this binary's `NO_COLOR` regime flips. This integration test
/// is its own process (separate from the lib unit-test binary), and its
/// tests run as threads of that one process under plain `cargo test`, so a
/// local lock suffices here; the lib-side counterpart lives in
/// `hornvale_game_core::cell::test_env`.
static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// Run `f` with `NO_COLOR` removed, restoring whatever it was after.
/// The env ops are `unsafe` because they are UB under concurrency;
/// SAFETY here rests on the caller holding [`ENV_LOCK`], which serialises
/// every env read/write across this binary's threaded tests.
fn with_no_color_removed<R>(f: impl FnOnce() -> R) -> R {
    with_no_color_inner(None, f)
}

/// Run `f` with `NO_COLOR` set to a non-empty value, restoring the prior
/// state after. Same safety argument as [`with_no_color_removed`].
fn with_no_color_set<R>(value: &str, f: impl FnOnce() -> R) -> R {
    with_no_color_inner(Some(value), f)
}

fn with_no_color_inner<R>(value: Option<&str>, f: impl FnOnce() -> R) -> R {
    let _env = ENV_LOCK.lock().unwrap();
    let saved = std::env::var_os("NO_COLOR");
    // SAFETY: the caller holds ENV_LOCK, so no sibling thread in this test
    // binary touches the environment concurrently.
    unsafe { std::env::remove_var("NO_COLOR") };
    if let Some(v) = value {
        // SAFETY: as above — ENV_LOCK is held for the whole body.
        unsafe { std::env::set_var("NO_COLOR", v) };
    }
    let out = f();
    match saved {
        Some(v) => {
            // SAFETY: as above — ENV_LOCK is held for the whole body.
            unsafe { std::env::set_var("NO_COLOR", v) };
        }
        None => {
            // SAFETY: as above — ENV_LOCK is held for the whole body.
            unsafe { std::env::remove_var("NO_COLOR") };
        }
    }
    out
}

/// The full composed screen at [`W`]×[`H`] under one colour regime.
fn render(json: &str) -> Grid {
    hornvale_game_core::render_with(
        json,
        W,
        H,
        Focus::Cli,
        None,
        CommandLine::default(),
        None,
        None,
        None,
    )
    .unwrap()
    .0
}

/// Cell-for-cell identity of everything colour is not allowed to move:
/// glyphs and weights. Ink is the one thing the floor EXPECTS to move —
/// `Plain` is what a declined colour claim resolves to — so it is asserted
/// separately: the plain render must be entirely Plain.
///
/// The grids must also be the same size, and the plain render must carry
/// no non-Plain ink anywhere: a survivor would mean colour was resolving
/// with the reader's refusal in force.
fn assert_identical(a: &Grid, b: &Grid, label: &str) {
    assert_eq!(a.width(), b.width(), "{label}: width moved");
    assert_eq!(a.height(), b.height(), "{label}: height moved");
    for y in 0..a.height() {
        for x in 0..a.width() {
            let ca = a.get(x, y).expect("dense grid");
            let cb = b.get(x, y).expect("dense grid");
            assert_eq!(ca.glyph, cb.glyph, "{label}: glyph moved at ({x},{y})");
            assert_eq!(ca.weight, cb.weight, "{label}: weight moved at ({x},{y})");
        }
    }
    for y in 0..b.height() {
        for x in 0..b.width() {
            let cb = b.get(x, y).expect("dense grid");
            assert!(
                matches!(cb.ink, Ink::Plain),
                "{label}: the NO_COLOR render is tinted at ({x},{y})"
            );
        }
    }
}

fn check(json: &str, label: &str) {
    let coloured = with_no_color_removed(|| render(json));
    let plain = with_no_color_set("1", || render(json));
    assert_identical(&coloured, &plain, label);
    let tinted = (0..coloured.height())
        .flat_map(|y| (0..coloured.width()).map(move |x| (x, y)))
        .filter(|&(x, y)| {
            coloured
                .get(x, y)
                .is_some_and(|c| !matches!(c.ink, Ink::Plain))
        })
        .count();
    assert!(
        tinted > 0,
        "{label}: the coloured render carries no tinted cell — colour never resolved, so the comparison above is vacuous"
    );
}

/// One test, not two: both bands are checked SEQUENTIALLY here because
/// each check mutates `NO_COLOR`, and `cargo test` runs a binary's tests
/// as threads of one process — two mutating tests would race on the
/// variable and observe each other's regime. (Under nextest's
/// process-per-test isolation either shape would be safe.)
#[test]
fn the_monochrome_floor_holds_in_both_bands() {
    check(WALK, "walk band");
    check(CHAMBER, "chamber band");
}
