//! `Driver`'s acceptance tests: the containment (only JSON crosses out) and
//! the loop (a verb advances the turn).

use hornvale_game::driver::Driver;
use hornvale_game::input::Action;
use hornvale_game_core::{CommandLine, Focus, Source, render_with, spread};
use std::collections::BTreeSet;

/// The driver's ONLY output is snapshot JSON. If this ever returns a typed
/// value, the containment in The Quire spec section 6 has been broken.
#[test]
fn the_driver_yields_json_the_core_can_render() {
    let d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let json = d.snapshot();
    let g = hornvale_game_core::render(&json, 80, 24).expect("core must render the live snapshot");
    assert_eq!(g.width(), 80);
    assert_eq!(g.height(), 24);
}

/// A turn advances. This is the loop the whole client is.
#[test]
fn handling_a_verb_advances_the_turn() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let before = hornvale_game_core::Snapshot::parse(&d.snapshot())
        .unwrap()
        .turn;
    d.handle("look");
    let after = hornvale_game_core::Snapshot::parse(&d.snapshot())
        .unwrap()
        .turn;
    assert_eq!(after, before + 1);
}

/// `handle`'s return value is a DIFFERENT question from `snapshot()`'s, now
/// that the two are no longer the same channel (The Stylus, Task 3):
/// `handle` reports whether the possession RELEASED, and `snapshot()` is
/// still where the reply text lives. An ordinary verb must report `false`.
#[test]
fn handle_return_value_reports_whether_the_session_released() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let released = d.handle("look");
    assert!(!released, "an ordinary verb must not report a release");
}

/// `--target most-populous-settlement` must land in a DIFFERENT settlement
/// than the flagship default, so the two snapshots' `self.settlement` fields
/// must disagree. If they ever agreed, `target` would be silently ignored by
/// the driver.
///
/// **Neither body is minted.** Possession SELECTS an already-derived roster
/// member (The Hand, decision 0227): `Session` is `{ bodies, driven }`, the
/// `AgentId` draw is retired, and the variant chooses which settlement's
/// existing resident is driven.
///
/// The two settlements' names and populations are **not** restated here: they
/// are a reading of one world rather than an invariant, and `PossessTarget`'s
/// own doc carries them along with the campaign that last moved them. This
/// asserts only the property the variant needs, which is that the two differ.
#[test]
fn the_most_populous_target_selects_a_different_settlement_than_flagship() {
    let flagship = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let popular =
        Driver::start(42, hornvale_vessel::PossessTarget::MostPopulousSettlement).unwrap();
    let a = hornvale_game_core::Snapshot::parse(&flagship.snapshot()).unwrap();
    let b = hornvale_game_core::Snapshot::parse(&popular.snapshot()).unwrap();
    assert_ne!(a.me.settlement, b.me.settlement);
}

/// Run `f` with `NO_COLOR` removed, restoring whatever it was after.
/// SAFETY: the env ops are UB under concurrency. This integration binary
/// is one process; under nextest each test is its own process, and under
/// plain `cargo test` the sibling tests in this file never touch the
/// environment, so no concurrent access is possible either way.
fn with_no_color_removed<R>(f: impl FnOnce() -> R) -> R {
    let saved = std::env::var_os("NO_COLOR");
    // SAFETY: see above — no sibling thread in this binary reads or writes
    // the environment.
    unsafe { std::env::remove_var("NO_COLOR") };
    let out = f();
    match saved {
        // SAFETY: as above.
        Some(v) => unsafe { std::env::set_var("NO_COLOR", v) },
        // SAFETY: as above.
        None => unsafe { std::env::remove_var("NO_COLOR") },
    }
    out
}

/// A released possession's parting line is not a separate channel — it
/// still lands in `snapshot()`, same as any other turn, even though
/// `handle` itself now reports the release as a `bool` rather than
/// returning the JSON.
#[test]
fn releasing_still_returns_a_parseable_snapshot() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let released = d.handle("release");
    assert!(released, "\"release\" must report a release");
    let snap = hornvale_game_core::Snapshot::parse(&d.snapshot())
        .expect("release still yields a snapshot");
    assert!(!snap.narration.prose.is_empty());
}

/// The cursor is a plate primitive. At a band whose resolver does not exist
/// yet, it still moves and still reports — it refuses honestly rather than
/// resolving against the wrong plate. Faking a resolution here would be
/// worse than refusing, because a wrong name is indistinguishable from a
/// right one.
///
/// `Driver` has no `for_test` seam (see the task report: adding one to
/// widen the public surface for a single test was rejected). The smallest
/// honest way to reach a chamber-band session is the same one
/// `scripts/possession-chamber.txt` and the committed
/// `session-seed-42-chamber.json` fixture already use — a single `enter`
/// from seed 42's flagship opening position — so this test drives a real
/// `Driver` through it rather than inventing a lighter-weight seam.
#[test]
fn map_focus_at_an_unresolved_band_refuses_rather_than_resolving() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    driver.handle("enter");
    let snap = hornvale_game_core::Snapshot::parse(&driver.snapshot())
        .expect("a live session always yields a parseable snapshot");
    assert!(
        matches!(snap.spatial, hornvale_game_core::Spatial::Chamber { .. }),
        "seed 42's flagship must land in the chamber band after one `enter`"
    );

    submit_line(&mut driver, "map");
    driver.apply(Action::CursorBy(1, 0));
    assert!(
        driver.cursor().is_some(),
        "the cursor must exist at every band"
    );
    // UNCHANGED, and briefly wasn't (Task 6, fix round 1's F3/F5). Task 6
    // dropped `world_plate_for_redraw`'s gate to `Focus::Map` alone, which
    // handed the chamber band the world raster and made this refusal false —
    // so it was retargeted to assert the opposite while the doc above went on
    // saying "faking a resolution here would be worse than refusing". F5
    // restored the chamber band's own renderer, so the original assertion is
    // true again and the doc, the name and the body agree once more.
    assert_eq!(driver.strip_text(), Some("nothing here yet"));
}

/// The walk band DOES have a resolver (the terrain-feature index, scoped to
/// the observer's own vertex — see `driver.rs`'s module doc for why cursor
/// motion does not change which vertex is queried this campaign). Focusing
/// the map at seed 42's flagship opening position (walk band) must report
/// a real name, not the unresolved-band refusal — this is what would catch
/// a regression that accidentally routed every band through the same
/// refusal.
#[test]
fn map_focus_at_the_walk_band_resolves_a_real_name() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let snap = hornvale_game_core::Snapshot::parse(&driver.snapshot()).unwrap();
    assert!(matches!(
        snap.spatial,
        hornvale_game_core::Spatial::Walk { .. }
    ));

    submit_line(&mut driver, "map");
    let strip = driver.strip_text();
    assert!(strip.is_some(), "the walk band must resolve to something");
    assert_ne!(
        strip,
        Some("nothing here yet"),
        "the walk band has a real resolver and must not report the unresolved-band refusal"
    );
}

/// FIX ROUND 1: the cursor must genuinely track. Moving it off the
/// observer's own box changes the resolved vertex, and therefore the strip
/// text, rather than recomputing the same answer regardless of position.
///
/// **Why the covering assertion is "names something" -> "unnamed terrain"
/// rather than two different NAMED features**, checked and worth recording:
/// a swept probe over every reachable box within seed 42's flagship
/// walk-band view found every `Some` box resolves to the SAME nearest
/// terrain `Vertex` as the observer (`Vertex(22195)`, confirmed by
/// instrumenting `resolve_walk_band` directly) — the visible neighbourhood
/// spans `distance_rad` on the order of 1e-4 (tens of metres), while
/// `NearestVertexIndex` snaps onto one of only 40,962 vertices tiling the whole
/// globe (roughly hundreds of kilometres apart at seed 42's `GLOBE_LEVEL`).
/// So a walk-band session structurally cannot cross a terrain-feature
/// boundary within view except by standing within metres of one — a real
/// scale mismatch this fix exposed, not a limitation of the test. What IS
/// reachable, and does prove the cursor is read: a box the observer's own
/// chart has no cell in at all (`cell_at` returns `None`, screen offset
/// `(15, 10)`, five columns west of centre `(20, 10)`) — a different,
/// real, honestly-reported outcome from the observer's own box.
/// Enter the map and climb to [`hornvale_game::plate::BAND_B_RUNG`] — the
/// walk band's own rung.
///
/// **New at The Hachure's Stage 0.** `submit_line(.., "map")` used to land on
/// band B; the map now opens at `plate::map_entry_rung`, the coarsest rung the
/// terrain mesh can fill, because band B is seven rungs finer than the grid and
/// a plate there draws one vertex's reading everywhere. Tests below that are
/// about BAND-B behaviour — the sight caption, resize re-centring — now have to
/// say so, and this is them saying it through the public gestures a reader
/// actually has.
///
/// Bounded rather than a `while`, the same reason the in-module helpers are:
/// an unbounded climb would hang the suite if `apply_zoom`'s zoom-in arm
/// regressed to a no-op.
fn enter_band_b(driver: &mut Driver) {
    submit_line(driver, "map");
    for _ in 0..(hornvale_game::plate::BAND_B_RUNG - hornvale_game::plate::GLOBE_RUNG) {
        driver.apply(Action::Zoom(1));
    }
    assert_eq!(
        driver.window().depth,
        hornvale_game::plate::BAND_B_RUNG,
        "the helper must land on the walk band's rung"
    );
}

#[test]
fn moving_the_cursor_off_the_observers_box_changes_the_strip() {
    // The strip carries the sight-disclosure caption only when colour is
    // allowed (the `bugbear` assertion below reads it), so the whole body
    // runs with `NO_COLOR` removed — hermetic against a developer's
    // exported `NO_COLOR` — and the prior state is restored after.
    with_no_color_removed(|| {
        let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
        submit_line(&mut driver, "map");
        let at_observer = driver.strip_text().map(str::to_string);
        // The strip carries a disclosure after the name (Task 5): the name
        // first, then the honesty line.
        let at_observer_text = at_observer
            .as_deref()
            .expect("the strip always reports once the map is focused");
        assert!(
            at_observer_text.starts_with("Vngashngatva"),
            "the observer's own box must resolve to seed 42's real landmass name, got {at_observer_text:?}"
        );
        // **THE HONESTY LINE THIS READS IS THE RESOLUTION ONE, NOT THE SIGHT
        // ONE (The Hachure, Stage 0).** It used to assert `"bugbear"` — the
        // sight caption — which `band_b_keeps_the_sight_caption_and_a_coarse_
        // rung_does_not` documents as band-B-only, and which this test got for
        // free while `map` landed on band B. The map now opens at
        // `plate::map_entry_rung`, so the sight caption is legitimately
        // absent here and the resolution disclosure (decision 0123) is what
        // rides along instead.
        //
        // The claim under test is unchanged — "a disclosure rides along with
        // the name" — and the sight caption's own band-B/coarse split keeps
        // its dedicated test, so nothing is uncovered by reading this one
        // instead. Asserted on the phrase rather than a character count,
        // because the count moves with the rung.
        assert!(
            at_observer_text.contains("to one terrain reading"),
            "the disclosure rides along: {at_observer_text:?}"
        );

        // **RETARGETED by Task 6, and the DISTANCE is the retarget.** Five
        // columns west used to land on a box the observer's own chart had no
        // cell in, so the resolver honestly said `unnamed terrain`. Band B
        // is the raster now: five columns west is five band-B TILES west,
        // real ground, and it resolves. It also resolves to the SAME
        // feature, because a band-B tile is far finer than the mesh the
        // terrain lives on — some hundred tiles share one grid-level vertex
        // — so a five-tile step genuinely cannot change the answer and
        // asserting it did would pin a falsehood.
        //
        // The property under test is unchanged: resolution must be a
        // function of the cursor, not of the observer. So the cursor is
        // driven far enough to cross a real vertex boundary — past the
        // plate's own edge, which SCROLLS the window (spec §4.2), which is
        // the only way to travel that far at this rung.
        //
        // **`i16::MIN` WAS A NO-OP AS SOON AS THE CHART'S WIDTH WAS CORRECT,
        // AND THAT IS WORTH THE PARAGRAPH (fix round 1).** This used to scroll
        // a magic 41 times by `i16::MIN`. `move_cursor` turns the cursor's
        // spill into `window.origin_col += spill` under
        // `rem_euclid(virtual_w)` — so a spill of exactly `virtual_w` moves
        // the window nowhere at all. `plate::base_facet_arc_rad` was returning
        // the ICOSAHEDRON's edge angle, making the chart 46,490 columns wide,
        // and `-32,768 mod 46,490` is a real 13,722-column move; on the
        // cube-sphere's own 32,768 the same action is a whole wrap and the
        // window does not budge. Forty-one no-ops later the strip was
        // unchanged — a red that says nothing about the property under test.
        //
        // So the step is a named ODD number of columns. Every chart width this
        // module can produce is `4 * 2^depth`, a power of two, so an odd step
        // is coprime with it and no repetition can ever land back on the
        // starting residue. And the distance is DRIVEN UNTIL THE ANSWER MOVES
        // rather than pinned, with a bound: that tests "the strip is a
        // function of the cursor" without also pinning the chart's width, and
        // a resolution that ever made this unreachable would redden on the
        // bound rather than on an arithmetic coincidence.
        driver.apply(Action::CursorBy(-5, 0));
        const SCROLL_COLS: i16 = -4097;
        const SCROLL_BUDGET: usize = 64;
        let mut scrolls = 0usize;
        let far_west = loop {
            driver.apply(Action::CursorBy(SCROLL_COLS, 0));
            scrolls += 1;
            let now = driver.strip_text().map(str::to_string);
            if now != at_observer || scrolls >= SCROLL_BUDGET {
                break now;
            }
        };
        eprintln!(
            "the strip changed after {scrolls} westward scrolls of {} columns each",
            -i32::from(SCROLL_COLS)
        );
        assert_ne!(
            far_west,
            at_observer,
            "the strip did not change in {SCROLL_BUDGET} westward scrolls of {} columns \
             each — the strip must change once the cursor has travelled off the \
             observer's own terrain, because resolution is a function of the cursor \
             and not of the possession",
            -i32::from(SCROLL_COLS)
        );

        // And coming back must restore the observer's own answer — proving
        // the dependency runs both ways, not just away from the start.
        let mut fresh = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
        submit_line(&mut fresh, "map");
        assert_eq!(
            fresh.strip_text(),
            at_observer.as_deref(),
            "the same state must resolve the same answer"
        );
    });
}

/// FIX ROUND 2, **RETARGETED by The Quadrat's Task 6.** The original
/// asserted that a resize CHANGED the strip: at the 20-row floor screen
/// `(20, 10)` was the observer's own chart box, and at 40 rows the real
/// centre moved to row 18, so `(20, 10)` became a box with no chart cell in
/// it and the strip fell to `unnamed terrain`. Both halves of that premise
/// are gone — band B draws the raster, so every screen position is real
/// ground, and the window re-centres on the observer as part of the resize,
/// so the observer is at the NEW plate's middle rather than at the old
/// one's.
///
/// The surviving property is the one that made the original worth having,
/// stated against what is now drawn: **a resize must leave the observer at
/// the middle of the plate it just resized to, not at the middle of the one
/// before it.** That is what `Driver::resize` threading the live terminal
/// height actually buys, and a resize that ignored the new height would put
/// the observer visibly off-centre — the same class of defect, on the same
/// axis, observable in the picture instead of in a refusal string.
#[test]
fn resize_re_centres_band_b_on_the_observer_at_the_new_plate_height() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    enter_band_b(&mut driver);

    // The observer's own marker, and where it sits, at two terminal heights.
    let marker_row = |driver: &mut Driver, w: u16, h: u16| -> (u16, u16) {
        let plate = driver
            .world_plate_for_redraw(w, h)
            .expect("band B draws a plate");
        let found = (0..plate.height())
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .find(|&(x, y)| plate.get(x, y).is_some_and(|c| c.glyph == Some('@')))
            .expect("the observer is drawn on their own band-B plate");
        (found.0, found.1)
    };

    driver.resize(80, 24);
    let (_, floor_h) = (0u16, 20u16); // world_plate_dims(80, 24) -> 40x20
    let at_floor = marker_row(&mut driver, 80, 24);
    assert!(
        at_floor.1 < floor_h,
        "sanity: the marker is inside the floor's own plate, got {at_floor:?}"
    );

    driver.resize(80, 40);
    let at_taller = marker_row(&mut driver, 80, 40);
    assert_ne!(
        at_taller, at_floor,
        "the marker must move with the plate it is drawn on — a resize that kept \
         the old origin would leave it at the same cell"
    );
    // The real assertion: centred on the NEW plate. `world_plate_dims(80,
    // 40)` is 40x36 — the WIDTH moved under Task 9's `MIN_ENTRY_WIDTH`
    // ceiling (an 80-column terminal can spare 40), the height did not —
    // so the middle row is still 18, the very row the original test
    // computed by hand and then asserted a refusal about.
    let (_, new_h) = (40u16, 36u16);
    assert_eq!(
        at_taller.1,
        new_h / 2,
        "the observer must sit at the middle row of the plate the resize produced"
    );
}

/// The cursor's clamp must also track the real plate height (the reviewer's
/// "while you are there" check): at a 40-row terminal the plate is 36 rows
/// tall, so the cursor must be able to reach row 35 -- unreachable if the
/// clamp were still pinned to the 20-row floor.
///
/// **The second size is The Quadrat's Task 9, and the first cannot stand in
/// for it.** The plate's height used to be derived as
/// `world_plate_width(w, h) / GLYPH_ASPECT`, which agreed with
/// `content_height(h)` for every width the old width rule could produce —
/// that rule's own ceiling was `GLYPH_ASPECT * content_height(h)`, so
/// halving it landed exactly on `content_height(h)` and the two spellings
/// were indistinguishable at 80x40. Task 9's floor of half the terminal
/// breaks that identity on a WIDE, SHORT terminal: at 200x50 the plate is
/// 100 columns and `content_height` is 46, so the retired derivation would
/// claim 50 rows and let the cursor walk four rows past the bottom of a
/// plate the page has no room to draw. That is the Task 3a review finding
/// with the axes swapped, and 200x50 is the size that sees it.
#[test]
fn the_cursor_clamp_tracks_the_real_plate_height_too() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    driver.resize(80, 40);
    submit_line(&mut driver, "map");
    driver.apply(Action::CursorBy(0, 1000)); // drive it hard into the bottom clamp
    let cursor = driver
        .cursor()
        .expect("the map always has a cursor once focused");
    assert_eq!(
        cursor.y, 35,
        "the clamp must reach row 35 (content height 36, 0-indexed) at a 40-row terminal, \
         not stop at the 20-row floor's row 19"
    );

    driver.resize(200, 50);
    driver.apply(Action::CursorBy(0, 1000));
    let wide = driver
        .cursor()
        .expect("the map always has a cursor once focused");
    assert_eq!(
        wide.y,
        spread::content_height(50) - 1,
        "the clamp must stop at the last row the page actually draws (content \
         height {}, 0-indexed), not at half the plate's own width",
        spread::content_height(50)
    );
}

/// FIX ROUND 1 (Task 3a's own review, regression), **RETARGETED by The
/// Quadrat's Task 6**, from
/// `map_focus_alone_does_not_activate_the_world_view`. The bug it was
/// written for: `main.rs`'s `redraw` used to compute
/// `Some(driver.world_plate(w, h))` whenever `focus() == Focus::Map`, with
/// no rung gate — so a 210x56 redraw drew a 104-column Mercator while the
/// cursor stayed clamped to the OLD 40-column plate and the strip resolved
/// the walk band's own chart, which the Mercator had silently replaced: a
/// picture and a cursor/strip that no longer agreed at all, and columns
/// 40..104 permanently uncursorable (shipped across
/// `9f69e4e2a`/`81d940d9c`/`64c80be36`, chronicled in
/// `book/src/chronicle/the-stride.md`/`the-stylus.md`).
///
/// **The gate that fixed it is gone, because band B now DRAWS the raster.**
/// Asserting `world_plate_for_redraw(..).is_none()` under `Focus::Map`
/// would now pin the arctic-corner state Task 6 exists to leave behind. But
/// the DEFECT is a coherence defect, not a gating one, and coherence is
/// still checkable and still exactly what could regress: the picture, the
/// cursor's reach and the strip must all be about the same plate. So this
/// asserts that trio directly, which is strictly closer to the original
/// complaint than the gate ever was.
///
/// It still discriminates the original bug: under it the plate was 104
/// columns wide and the cursor could reach 39, which the reach assertion
/// below fails outright.
#[test]
fn map_focus_at_band_b_keeps_the_picture_the_cursor_and_the_strip_on_one_plate() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let (w, h) = (210u16, 56u16);
    driver.resize(w, h);
    submit_line(&mut driver, "map");
    assert_eq!(driver.focus(), Focus::Map);

    let world_plate = Some(
        driver
            .world_plate_for_redraw(w, h)
            .expect("band B draws a plate now — Task 6"),
    );
    let plate_w = spread::world_plate_width(w, h);
    assert!(
        plate_w > spread::PLATE_WIDTH,
        "the test terminal must produce a plate wider than the retired fixed \
         walk-band width, or the reach assertion below proves nothing"
    );

    let json = driver.snapshot();
    let empty_line = String::new();
    let cmd_line = CommandLine {
        text: &empty_line,
        caret: 0,
    };
    let (grid, _) = render_with(
        &json,
        w,
        h,
        driver.focus(),
        driver.cursor(),
        cmd_line,
        driver.strip_text(),
        driver.echo(),
        world_plate.as_ref(),
        driver.strip_offset(),
        None,
    )
    .unwrap();

    // ONE: the picture is the raster WITH the perception overlay on it —
    // both channels present in the plate region. `Source::World` alone would
    // mean the overlay was lost (Ruling 19's concern); `Source::Chart` alone
    // would mean the raster was.
    let content_h = spread::content_height(h);
    let sources: BTreeSet<Source> = (0..content_h)
        .flat_map(|y| (0..plate_w).map(move |x| (x, y)))
        .filter_map(|(x, y)| grid.get(x, y).filter(|c| !c.is_blank()).map(|c| c.source))
        .collect();
    assert!(
        sources.contains(&Source::World),
        "band B's plate must carry the terrain raster, got {sources:?}"
    );
    assert!(
        sources.contains(&Source::Chart),
        "band B's plate must carry the perception overlay, got {sources:?}"
    );

    // TWO: the cursor reaches the plate that was actually drawn. This is the
    // original bug, stated positively.
    driver.apply(Action::CursorBy(i16::MAX, 0));
    assert_eq!(
        driver.cursor().expect("the map is focused").x,
        plate_w - 1,
        "every drawn column must be reachable — the old gate left 40..{plate_w} dead"
    );

    // THREE: the strip resolves against that same plate, caption included.
    let strip = driver.strip_text();
    assert!(
        strip.is_some_and(|t| t.contains("clamped at")),
        "the strip must be the raster's own answer, got {strip:?}"
    );
}

/// **THE QUADRAT, TASK 9 — the campaign's third reported defect, stated
/// against the view it was actually reported about.** Nathan's complaint
/// named "the map, when we're not in map mode": the picture you look at
/// while WALKING must be the square raster and must claim at least half
/// the terminal.
///
/// Task 6 put band B on the raster and the suite went green, but only under
/// `Focus::Map`. The default focus is `Focus::Walk` (decision 0160), and
/// nothing anywhere asserted what a redraw draws in it — so ordinary play
/// went on getting the old hex scatter in a 40-column pane with every test
/// passing. This is the assertion whose absence made that possible, and it
/// deliberately never submits `map`: the driver stays in the focus it
/// starts in.
///
/// Both halves are asserted from the COMPOSED PAGE, not from the gate or
/// the width function — the defect was that a correct plate and a correct
/// width rule were never brought together in this focus, which only the
/// page can witness.
#[test]
fn the_walk_view_draws_the_raster_across_at_least_half_the_terminal() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let (w, h) = (200u16, 50u16);
    driver.resize(w, h);
    assert_eq!(
        driver.focus(),
        Focus::Walk,
        "this test is about the DEFAULT focus; submitting anything here would          void it"
    );

    let world_plate = driver.world_plate_for_redraw(w, h);
    assert!(
        world_plate.is_some(),
        "the walk view must be handed the raster — the focus gate is what          left this campaign's third defect half fixed"
    );

    let json = driver.snapshot();
    let empty_line = String::new();
    let (grid, _) = render_with(
        &json,
        w,
        h,
        driver.focus(),
        driver.cursor(),
        CommandLine {
            text: &empty_line,
            caret: 0,
        },
        driver.strip_text(),
        driver.echo(),
        world_plate.as_ref(),
        driver.strip_offset(),
        None,
    )
    .unwrap();

    // ONE: the pane. Measured off the page — the run of raster/overlay
    // columns from column 0 along a mid-content row — never recomputed from
    // `world_plate_width`, which would agree with any rule including the
    // one this task replaces.
    let row = spread::content_height(h) / 2;
    let drawn = |x: u16| {
        grid.get(x, row)
            .is_some_and(|c| c.source == Source::World || c.source == Source::Chart)
    };
    let cols = (0..w).take_while(|&x| drawn(x)).count() as u16;
    assert!(
        cols * 2 >= w,
        "the walk view's map claimed {cols} of {w} columns, under half"
    );
    assert!(
        cols > spread::PLATE_WIDTH,
        "VACUOUS GUARD: {w}x{h} must exceed the retired fixed width, or this          passes against the very code it replaces"
    );
    assert!(
        w - cols >= spread::MIN_ENTRY_WIDTH,
        "the walk view's entry pane got {} columns, under the legible minimum",
        w - cols
    );

    // TWO: the PICTURE is the raster with the perception overlay on it, not
    // the old hex scatter. `Source::Chart` alone would be the scatter —
    // which is why `World` is asserted rather than merely "something is
    // drawn" — and `World` alone would mean the observer was lost.
    let sources: BTreeSet<Source> = (0..spread::content_height(h))
        .flat_map(|y| (0..cols).map(move |x| (x, y)))
        .filter_map(|(x, y)| grid.get(x, y).filter(|c| !c.is_blank()).map(|c| c.source))
        .collect();
    assert!(
        sources.contains(&Source::World),
        "the walk view's map must be the terrain raster, got {sources:?}"
    );
    assert!(
        sources.contains(&Source::Chart),
        "the walk view's map must carry the perception overlay, got {sources:?}"
    );

    // THREE: focus still decides what focus is FOR. No cursor is reported
    // and no strip text is, in the walk view — widening the picture must not
    // have smuggled the map mode's furniture into it.
    assert!(driver.cursor().is_none(), "the walk view reports no cursor");
    assert!(
        driver.strip_text().is_none(),
        "the walk view reports no map strip"
    );
}

/// THE QUADRAT, TASK 9, FIX ROUND 1 — **Important 1: the walk view may
/// never lose the player.**
///
/// Six keystrokes from the opening screen: `m a p ↵`, `-`, `Esc`. Task 9
/// un-gated the raster so the walking view draws it, but nothing returned
/// the ladder's rung on the way out, so the plate showed a coarse chart
/// with the perception overlay correctly refusing off band B — no `@`, no
/// creatures, no cursor, no strip — while the entry pane went on narrating
/// the player's immediate surroundings. The picture and the prose described
/// different places.
///
/// **Both doors are asserted**, because the map has two and the strip's own
/// invariant has been broken once already by exactly that (see the
/// `FocusAndType` arm's comment in `driver.rs`): `Esc` through
/// `toggle_focus`, and any printable key through `FocusAndType`, which
/// leaves for the command line. A fix wired into one of them would pass a
/// test that knew about one of them.
///
/// The assertion is `Source::Chart` ON THE COMPOSED PAGE — the perception
/// overlay's own channel, which is what refuses off band B. Asserting the
/// rung directly would pass against a fix that reset the rung and left the
/// window in the arctic corner of a 23,245-wide chart, which is the other
/// half of the same defect.
#[test]
fn leaving_the_map_at_a_coarse_rung_returns_the_walker_to_their_own_band() {
    for door in ["esc", "type"] {
        let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
        let (w, h) = (200u16, 50u16);
        driver.resize(w, h);

        submit_line(&mut driver, "map");
        assert_eq!(driver.focus(), Focus::Map);
        let entry_rung = driver.window().depth;
        driver.apply(Action::Zoom(-1));

        // VACUOUS GUARD: the zoom must actually have coarsened the ladder,
        // or the exit below proves nothing at all.
        //
        // **RETARGETED BY THE HACHURE, STAGE 0, AND THE OLD FORM IS NOW
        // UNAVAILABLE RATHER THAN MERELY WEAKER.** This guard used to assert
        // `Source::Chart` was ABSENT at the coarse rung, and the assertion
        // after the exit that it was PRESENT — the pair discriminating "the
        // walker got their own band back". Stage 0 draws the observer at
        // every rung (`compose_perception_layer`), because a map that opens
        // coarser must still show where you stand, so `Chart` is present on
        // both sides and the pair cannot discriminate any more.
        //
        // Nor does COUNTING discriminate: seed 42's flagship band carries a
        // mark on the observer's own facet and nowhere else, and `here`
        // outranks a mark on the same box, so band B and a coarse rung both
        // paint exactly one `@`.
        //
        // So the discriminator is the rung PLUS the picture. The old comment
        // rejected asserting the rung because a fix could "reset the rung and
        // leave the window in the arctic corner of a 23,245-wide chart" — but
        // that is precisely what the observer being ON the plate rules out,
        // so the conjunction is strictly stronger than the source set it
        // replaces, not a concession.
        assert!(
            driver.window().depth < entry_rung,
            "VACUOUS GUARD: the zoom must actually have coarsened the ladder, \
             still at rung {}",
            driver.window().depth
        );

        match door {
            "esc" => {
                driver.apply(Action::ToggleFocus);
            }
            _ => {
                driver.apply(Action::FocusAndType('x'));
                assert_eq!(driver.focus(), Focus::Cli, "the second door lands on Cli");
            }
        }
        assert_ne!(
            driver.focus(),
            Focus::Map,
            "the map was left by the {door} door"
        );

        let plate = driver
            .world_plate_for_redraw(w, h)
            .expect("the walk view is handed the raster");
        let sources: BTreeSet<Source> = (0..spread::content_height(h))
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .filter_map(|(x, y)| plate.get(x, y).filter(|c| !c.is_blank()).map(|c| c.source))
            .collect();
        assert!(
            sources.contains(&Source::Chart),
            "after leaving the map by the {door} door the plate carries no \
             perception overlay — the player is not on their own picture; got \
             {sources:?}"
        );
        assert_eq!(
            driver.window().depth,
            hornvale_game::plate::BAND_B_RUNG,
            "the {door} door must hand the walker back their own band"
        );
        let observed = (0..spread::content_height(h))
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .any(|(x, y)| plate.get(x, y).is_some_and(|c| c.glyph == Some('@')));
        assert!(
            observed,
            "the {door} door reset the rung but left the window somewhere the \
             observer is not — the other half of the same defect"
        );
        assert!(
            plate.to_plain_text().contains('@'),
            "after leaving the map by the {door} door the observer is not drawn"
        );
    }
}

/// THE QUADRAT, TASK 9, FIX ROUND 1 — **the walk view follows the walker.**
///
/// `chart::draw` anchors the observer to the plate's centre by
/// construction, so the picture Task 9 replaced had this property for free.
/// The raster is drawn through a STORED window, and the centring was called
/// only on arrival at the map and on resize — neither of which is a step.
/// Measured before the fix, at this exact size: row 23 → 18 over eight
/// `go n`s, about one row per 1.6 steps, walking clean off a 46-row plate in
/// under forty.
///
/// Arrow keys ARE the walking gesture in `Focus::Walk` (`input::action_for`
/// maps them to `Move`, not `CursorBy`), so this is the most ordinary thing
/// a player does, and the walk view offers no scroll gesture to undo it
/// with.
///
/// **Forty steps, not eight.** Eight would pass against no fix at all: the
/// drift is slow, and a test that walked only a few steps would watch the
/// observer sit two rows off centre and call it centred. The distance is
/// chosen to exceed the plate's own half-height.
#[test]
fn the_walk_view_follows_the_walker_across_a_long_walk() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let (w, h) = (200u16, 50u16);
    driver.resize(w, h);

    let observer = |driver: &mut Driver| -> (u16, u16) {
        let p = driver
            .world_plate_for_redraw(w, h)
            .expect("the walk view is handed the raster");
        (0..p.height())
            .flat_map(|y| (0..p.width()).map(move |x| (x, y)))
            .find(|&(x, y)| p.get(x, y).is_some_and(|c| c.glyph == Some('@')))
            .expect("the observer is drawn on their own walk plate")
    };

    let start = observer(&mut driver);
    for _ in 0..40 {
        driver.handle("go n");
    }
    let after = observer(&mut driver);
    assert_eq!(
        after, start,
        "walking must scroll the plate under the observer, not the observer \
         across the plate — started at {start:?}, ended at {after:?}"
    );

    // The other axis, because a fix on one is not a fix on both: longitude
    // WRAPS where latitude clamps (spec 4.2), and they are different code.
    for _ in 0..40 {
        driver.handle("go e");
    }
    assert_eq!(
        observer(&mut driver),
        start,
        "walking east must scroll the plate too"
    );
}

/// **H2.** A whole line typed key by key reaches the sim and its answer
/// comes back. The sweep in `input.rs` tests single keys and cannot see a
/// defect in SEQUENCES — a buffer that drops every third character would
/// pass it and fail here (spec §8).
#[test]
fn a_whole_typed_line_reaches_the_sim_and_its_answer_returns() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    for c in "look".chars() {
        d.apply(Action::Type(c));
    }
    assert_eq!(
        d.line_text(),
        "look",
        "the buffer must hold every character typed"
    );
    d.apply(Action::Submit);

    let typed = d.snapshot();
    let mut direct = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    direct.handle("look");
    assert_eq!(
        typed,
        direct.snapshot(),
        "typing `look` must produce exactly what handle(\"look\") produces"
    );
}

/// **Spec §6.** `Enter` on an empty buffer must not advance the world. The
/// buffer is the last point of reversibility before an irreversible act,
/// and a stray keypress must not cost the player a turn of the world.
///
/// **Review finding (Task 3, Needs fixes):** the original version of this
/// test asserted only `!released` and an unchanged `snapshot()` — both of
/// which also hold if the guard were DELETED, because `Session::handle("")`
/// is itself a silent no-op (no turn, no `last_text` change). That made the
/// test pass "by coincidence of the sim's tolerance rather than by proving
/// the guard ran" — it could not tell "the driver never called the
/// session" apart from "the driver called it with an empty string and the
/// sim shrugged." So this version first puts real state in place (a
/// genuine submit of `"look"`, populating `echo` and `history`) and THEN
/// submits empty, checking that the guard-protected state — `echo()` and a
/// `HistoryPrev` recall — survives untouched. A guard-less regression would
/// push `""` onto history and overwrite `echo` with `Some("")`, which
/// `d.snapshot()` alone can never see (verified by mutation — see the task
/// report).
#[test]
fn enter_on_an_empty_line_costs_no_turn() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    for c in "look".chars() {
        d.apply(Action::Type(c));
    }
    d.apply(Action::Submit);
    assert_eq!(
        d.echo(),
        Some("look"),
        "a real submit must land in echo before the empty-submit probe below"
    );

    let before = d.snapshot();
    let released = d.apply(Action::Submit);
    assert!(!released);
    assert_eq!(
        d.snapshot(),
        before,
        "an empty submit must change nothing at all"
    );
    assert_eq!(
        d.echo(),
        Some("look"),
        "an empty submit must not overwrite the last real echo with an empty one"
    );

    d.apply(Action::HistoryPrev);
    assert_eq!(
        d.line_text(),
        "look",
        "an empty submit must not push an empty entry onto history, shadowing the real one"
    );
}

/// **Ledger #8, and the campaign's exit guarantee.** Both synonyms the sim
/// honours must end the loop. `quit` is the one the old sent-string check
/// could not see, and once `Q` types a `Q` this is the only way out of the
/// client.
#[test]
fn both_release_synonyms_end_the_possession() {
    for line in ["release", "quit"] {
        let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
        assert!(d.handle(line), "`{line}` must report a release");
    }
}

/// An ordinary verb must NOT report a release — otherwise the loop ends on
/// the first command and the test above passes vacuously.
#[test]
fn an_ordinary_verb_does_not_report_a_release() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    assert!(!d.handle("look"));
}

/// **Spec §6.** The submitted line is echoed so the record shows what was
/// ASKED, not only what was answered. A journal that records only replies
/// is not one.
#[test]
fn the_submitted_line_is_echoed_into_the_entry() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    for c in "look".chars() {
        d.apply(Action::Type(c));
    }
    d.apply(Action::Submit);
    assert_eq!(d.echo(), Some("look"));
}

/// The buffer empties on submit — a command must not be left behind to be
/// sent twice.
#[test]
fn submitting_empties_the_buffer() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    for c in "look".chars() {
        d.apply(Action::Type(c));
    }
    d.apply(Action::Submit);
    assert_eq!(d.line_text(), "");
}

/// A printable key pressed while the map is focused returns focus to the
/// CLI *and* types — one keypress, not two (spec §2). Reaching the map now
/// takes an explicit route — submitting exactly `map` — because startup
/// focus is Walk and Esc toggles between Walk and Cli, never onto Map.
#[test]
fn a_printable_key_on_the_map_bounces_focus_and_types() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    submit_line(&mut d, "map");
    assert_eq!(d.focus(), Focus::Map);
    d.apply(Action::FocusAndType('l'));
    assert_eq!(d.focus(), Focus::Cli);
    assert_eq!(d.line_text(), "l");
}

/// Type `line` into the buffer and submit it through [`Action::Submit`] —
/// the same route a player's keystrokes take, never reaching into private
/// state.
fn submit_line(d: &mut Driver, line: &str) -> bool {
    for c in line.chars() {
        d.apply(Action::Type(c));
    }
    d.apply(Action::Submit)
}

/// Startup focus is the walk mode: arrow keys move immediately.
#[test]
fn startup_focus_is_walk() {
    let d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    assert_eq!(d.focus(), Focus::Walk);
}

/// The Esc ladder: Walk→Cli→Walk by toggling; submitting exactly `map`
/// enters the map; Esc from the map lands back on Walk (never Cli).
#[test]
fn esc_and_map_submissions_walk_the_three_focuses() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    d.apply(Action::ToggleFocus);
    assert_eq!(d.focus(), Focus::Cli);
    d.apply(Action::ToggleFocus);
    assert_eq!(d.focus(), Focus::Walk);

    assert!(
        !submit_line(&mut d, "map"),
        "an ordinary submit must not release"
    );
    assert_eq!(d.focus(), Focus::Map);
    d.apply(Action::ToggleFocus);
    assert_eq!(d.focus(), Focus::Walk, "Esc from the map must land on Walk");
}

/// An arrow-key movement under Walk executes like a submitted line: echoed,
/// pushed onto history (so it recalls), buffer untouched, and `apply`
/// reports `handle`'s release bool (false for an ordinary move).
#[test]
fn a_move_action_echoes_historys_and_leaves_the_buffer_alone() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    let released = d.apply(Action::Move("north"));
    assert!(!released, "an ordinary move must not release");
    assert_eq!(d.echo(), Some("north"));
    assert_eq!(d.line_text(), "", "a move must not involve the buffer");
    d.apply(Action::HistoryPrev);
    assert_eq!(d.line_text(), "north", "the move must be recallable");
}

/// Only BARE `map` enters the map focus: surrounding whitespace is
/// tolerated, arguments are not. The rule mirrors the sim's own
/// bare-from-argument split (`Session::handle`'s `rest.is_empty()` guards
/// on `map` and `eyes`), not a first-token rule — `map out 2` has `map` as
/// its first token and must NOT focus.
///
/// `map out 2` is the case that pins the decision, because the plausible
/// wrong answer ("it drew a chart, focus it") is wrong twice over:
/// `Session::map` takes `&self`, so no argument form moves the plate at
/// all, and the plate is redrawn from `Spatial` every turn regardless. See
/// the driver's Submit arm for the full reasoning.
#[test]
fn only_bare_map_enters_the_map() {
    for (line, want_map) in [
        ("map", true),
        (" map ", true),
        ("map x", false),
        ("map out", false),
        ("map out 2", false),
        ("examine map", false),
    ] {
        let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
        submit_line(&mut d, line);
        assert_eq!(
            d.focus(),
            if want_map { Focus::Map } else { Focus::Walk },
            "submitting {line:?}"
        );
    }
}

/// The Newel, Task 4: completion sees the map's names, not only the
/// current turn's narration. This is the exact gap the task brief names —
/// `examine Dvoashngashngo` resolved while `enter Dvoas[TAB]` completed
/// nothing — reproduced end to end through the real driver, with no world
/// change needed: `Dvoashngashngo` is a bugbear the walk-band chart marks
/// from the flagship's own starting room (an `"agent"` mark, never gated
/// by decision 0670 — see `hornvale_game_core::ChartMarks`'s own doc), but
/// it is not part of the room's own `narration.nouns` catalog at all.
#[test]
fn a_chart_only_agent_name_completes_from_a_fresh_driver() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");

    // Sanity: the name really is chart-only at genesis, or this test would
    // prove nothing about the widening — `CurrentTurnNouns` alone would
    // already complete it.
    let snap = hornvale_game_core::Snapshot::parse(&d.snapshot()).unwrap();
    assert!(
        !snap
            .narration
            .nouns
            .iter()
            .any(|n| n.noun == "Dvoashngashngo"),
        "sanity: this name must not already be in the current turn's narration \
         (seed 42's genesis roster moved)"
    );

    for c in "examine Dvoashngas".chars() {
        d.apply(Action::Type(c));
    }
    d.apply(Action::Complete);
    assert_eq!(
        d.line_text(),
        "examine Dvoashngashngo",
        "a chart-only agent's name must complete once the chart scope is registered"
    );
}

// ---------------------------------------------------------------------------
// The Sett, Task 3: the walk view draws the rose raster.
//
// Everything below reads ONE world — the committed seed-42 fixture, taken
// twice from disk so the driver can own its copy while the assertions keep a
// terrain of their own. `Driver::start(42, ..)` would derive a second world
// from scratch and there would be nothing holding the two to each other; a
// campaign about which facet is in which box cannot afford that ambiguity.
// ---------------------------------------------------------------------------

/// `plate`'s own `RIVER_GLYPH`, which is private to that module.
///
/// Restated here rather than exported, and it is safe to restate for one
/// reason worth stating too: it is the ONE glyph on a walk plate that
/// `plate::glyph_for` can never return, so a box drawn with it came off the
/// line layer and off nothing else. If that ever stops being true these
/// tests go red rather than quiet — the terrain comparison would start
/// skipping boxes that carry a real terrain reading, and both counts below
/// are asserted exactly.
const WATERCOURSE_GLYPH: char = '"';

/// The walk-band raster's own terrain reading of one facet, as a glyph — the
/// SAME question `plate::draw_terrain_layer_from_raster` answers, asked
/// through the two public functions rather than through the drawing code.
///
/// `ctx: None`, `WorldTime::GENESIS`, `season: 0` and `cache: None` are inert
/// for this comparison and are not a shortcut: `TileTerrain::water` and
/// `::band` — the only two fields `plate::glyph_for` reads — are functions of
/// the terrain, the geosphere and the address alone. The light, the season
/// and the reflectance cache move the INK, which these assertions
/// deliberately do not read.
/// The terrain, geosphere and nearest-vertex index the assertions below take
/// a facet's own reading through — the reading side, held apart from the
/// driver's own copy of the same three so that neither can be mistaken for
/// evidence about the other.
// Named construction site (decision 0092): `terrain_of` re-derives the
// tectonic globe once here, for the same reason `Driver::start_from_world`
// and `wash.rs`'s own fixture each carry this allow at their `terrain_of`
// call. Once per test, never per box.
#[allow(clippy::disallowed_methods)]
fn seed_42_reading(
    world: &hornvale_kernel::World,
) -> (
    hornvale_terrain::GeneratedTerrain,
    hornvale_kernel::Geosphere,
    hornvale_kernel::NearestVertexIndex,
) {
    let terrain =
        hornvale_worldgen::terrain_of(world).expect("the committed seed-42 world sculpts");
    let geo = terrain.geosphere().clone();
    let nearest = hornvale_kernel::NearestVertexIndex::new(&geo);
    (terrain, geo, nearest)
}

fn walk_glyph(
    terrain: &hornvale_terrain::GeneratedTerrain,
    geo: &hornvale_kernel::Geosphere,
    nearest: &hornvale_kernel::NearestVertexIndex,
    memo: &mut hornvale_kernel::RoomMeshMemo,
    facet: &hornvale_kernel::Facet,
) -> char {
    let t = hornvale_game::plate::terrain_at_facet(
        terrain,
        geo,
        nearest,
        memo,
        facet,
        facet.centroid(),
        None,
        hornvale_kernel::WorldTime::GENESIS,
        0,
        None,
    );
    hornvale_game::plate::glyph_for(t.water, t.band)
}

/// The facet the rose raster's DEFINITION puts in box `(col, row)` of a plate
/// anchored at `anchor` with its centre at `(cc, cr)`: north or south to the
/// row, then east or west along it.
///
/// **Walked here from `rose.rs`'s module doc rather than by calling
/// `RoseRaster`.** A test that asked the transport where a facet went and
/// then checked the plate against that answer would pass on any transport at
/// all, including a broken one — the two would be the same object. This
/// re-derives the picture from `heading_rose` directly, so it agrees with the
/// plate only if the plate really is addressed by compass chain.
fn rose_facet(
    anchor: &hornvale_kernel::Facet,
    cc: u16,
    cr: u16,
    col: u16,
    row: u16,
) -> Option<hornvale_kernel::Facet> {
    // `Compass::all()` order: [N, Ne, E, Se, S, Sw, W, Nw].
    let step =
        |f: &hornvale_kernel::Facet, word: usize| hornvale_locale::heading_rose(f)[word].clone();
    let mut here = Some(anchor.clone());
    let (down, vertical) = if row >= cr {
        (4, row - cr)
    } else {
        (0, cr - row)
    };
    for _ in 0..vertical {
        here = here.as_ref().and_then(|f| step(f, down));
    }
    let (across, lateral) = if col >= cc {
        (2, col - cc)
    } else {
        (6, cc - col)
    };
    for _ in 0..lateral {
        here = here.as_ref().and_then(|f| step(f, across));
    }
    here
}

/// NATHAN'S REPORT, AS AN ASSERTION. The box one step from the mark must
/// hold the terrain of the facet `heading_rose` names for that direction —
/// the facet the arrow key actually moves to.
///
/// **All four cardinals, and both seed-42 starts, and neither widening is a
/// garnish.** The plan asks for the west box at the flagship start. Measured
/// against the Mercator plate this task replaced, ALL FOUR of the flagship
/// start's cardinal boxes were already right — the observer stands at
/// latitude −4.00°, near enough the equator that a compass step and a
/// Mercator column agree in the immediate neighbourhood of the mark — so the
/// single-box test the plan sketches passes on the very projection it was
/// written to reject, and so does a four-box one at that start alone. The
/// `MostPopulousSettlement` start is where the same four boxes discriminate:
/// its SOUTH box drew `'"'` where `','` was wanted. Both starts are kept,
/// because the flagship one is the report's own subject and its agreement is
/// a fact worth pinning rather than hiding.
///
/// The plate-wide statement of the same property — which is what reddens
/// most loudly, 29 boxes of 799 — is
/// `the_walk_plate_draws_the_rose_raster_box_for_box`.
///
/// **Non-vacuity, two guards.** Each start's plate must carry more than one
/// distinct glyph, or it is uniform ocean and any raster would pass; and the
/// eight sampled boxes together must not all expect the same glyph, or the
/// assertion is not discriminating between facets at all.
#[test]
fn the_box_left_of_the_mark_is_where_the_left_arrow_goes() {
    let (terrain, geo, nearest) = seed_42_reading(&hornvale_worldgen::fixture::seed_42_world());
    let mut memo = hornvale_kernel::RoomMeshMemo::default();

    let mut sampled: BTreeSet<char> = BTreeSet::new();
    let mut compared = 0u32;
    let mut watercourse = 0u32;
    for (start, target) in [
        ("the flagship", hornvale_vessel::PossessTarget::Flagship),
        (
            "the most populous settlement",
            hornvale_vessel::PossessTarget::MostPopulousSettlement,
        ),
    ] {
        let mut d = Driver::start_from_world(hornvale_worldgen::fixture::seed_42_world(), target)
            .expect("the committed seed-42 world starts a possession");
        d.resize(80, 24);
        let anchor = d.observer_facet();
        let plate = d
            .world_plate_for_redraw(80, 24)
            .expect("the walk band is handed a plate");
        let (cc, cr) = (plate.width() / 2, plate.height() / 2);

        let drawn: BTreeSet<Option<char>> = (0..plate.height())
            .flat_map(|row| (0..plate.width()).map(move |col| (col, row)))
            .map(|(col, row)| plate.get(col, row).and_then(|c| c.glyph))
            .collect();
        assert!(
            drawn.len() > 1,
            "vacuity guard: {start}'s whole plate reads as one glyph ({drawn:?}), \
             so every raster ever written would pass this"
        );

        let rose = hornvale_locale::heading_rose(&anchor);
        // (compass word, the box it must be drawn in, the arrow key that goes there)
        for (word, (col, row), key) in [
            (0usize, (cc, cr - 1), "up"),
            (2, (cc + 1, cr), "right"),
            (4, (cc, cr + 1), "down"),
            (6, (cc - 1, cr), "left"),
        ] {
            let facet = rose[word].as_ref().unwrap_or_else(|| {
                panic!("{start} is not a cube corner: all four arrows must exist")
            });
            let drew = plate.get(col, row).and_then(|c| c.glyph);
            if drew == Some(WATERCOURSE_GLYPH) {
                // A box the line layer painted carries no terrain glyph left
                // to compare, exactly as a box the perception layer painted
                // does not. Counted rather than silently passed over, and
                // WHERE those boxes may legitimately be is
                // `a_watercourse_paints_the_facets_it_runs_through`'s
                // assertion, not this one's.
                watercourse += 1;
                continue;
            }
            let want = walk_glyph(&terrain, &geo, &nearest, &mut memo, facet);
            sampled.insert(want);
            compared += 1;
            assert_eq!(
                drew,
                Some(want),
                "at {start}: the box at ({col}, {row}) — one step {key} of the \
                 mark at ({cc}, {cr}) — does not draw the facet the {key} arrow \
                 moves to. That is the whole of The Sett: a plate addressed by \
                 projection puts a place beside you that you cannot walk to."
            );
        }
    }
    assert!(
        sampled.len() > 1,
        "vacuity guard: all eight sampled neighbour boxes expect the same glyph \
         ({sampled:?}), so this assertion is not discriminating between facets"
    );
    assert_eq!(
        (compared, watercourse),
        (8, 0),
        "all eight sampled cardinal boxes carry terrain glyphs to compare; \
         the most populous settlement's cardinal neighbourhood no longer \
         intersects a watercourse after the Underworld population epoch"
    );
}

/// THE WHOLE PLATE, box for box: every drawn box of the walk view holds the
/// terrain of the facet the compass chains reach, not of whatever facet a
/// Mercator column happened to unproject onto.
///
/// **This is the assertion the four-cardinal test above cannot make.** Near
/// the equator the two rasters agree in the neighbourhood of the mark and
/// diverge as you move away from it: measured against the Mercator plate this
/// task replaced, 29 of 799 comparable boxes disagreed at the seed-42
/// flagship start — a shoreline drawn as a diagonal by the projection and as
/// a near-vertical coast by the rose.
///
/// **`Source::Chart` boxes are skipped, and nothing else is.** The perception
/// overlay (the `@`, and any marks) is `scene/surrounds/v2` data rather than
/// world terrain and paints over the ground; the count of skipped boxes is
/// asserted so a future overlay that started covering half the plate could
/// not quietly shrink what this compares. The FEATURE layer paints
/// `Source::World` and is not skipped — at this start it draws nothing, and
/// the assertion would redden loudly if that changed, which is the honest
/// behaviour while that layer still projects (Task 4).
#[test]
fn the_walk_plate_draws_the_rose_raster_box_for_box() {
    let (terrain, geo, nearest) = seed_42_reading(&hornvale_worldgen::fixture::seed_42_world());
    let mut memo = hornvale_kernel::RoomMeshMemo::default();

    let mut d = Driver::start_from_world(
        hornvale_worldgen::fixture::seed_42_world(),
        hornvale_vessel::PossessTarget::Flagship,
    )
    .expect("the committed seed-42 world starts a possession");
    d.resize(80, 24);
    let anchor = d.observer_facet();
    let plate = d
        .world_plate_for_redraw(80, 24)
        .expect("the walk band is handed a plate");
    let (cc, cr) = (plate.width() / 2, plate.height() / 2);

    let mut compared = 0u32;
    let mut skipped = 0u32;
    let mut watercourse = 0u32;
    let mut distinct: BTreeSet<char> = BTreeSet::new();
    let mut wrong: Vec<String> = Vec::new();
    for row in 0..plate.height() {
        for col in 0..plate.width() {
            let drawn = plate
                .get(col, row)
                .expect("every box of the plate is in bounds");
            let Some(facet) = rose_facet(&anchor, cc, cr, col, row) else {
                // A refused bearing ends its chain and nothing fills the
                // boxes past it (ledger decision #4). None exist at this
                // start; the arm is here because the raster's contract has
                // one, not because this plate exercises it.
                skipped += 1;
                continue;
            };
            let want = walk_glyph(&terrain, &geo, &nearest, &mut memo, &facet);
            distinct.insert(want);
            if drawn.source == Source::Chart {
                skipped += 1;
                continue;
            }
            if drawn.glyph == Some(WATERCOURSE_GLYPH) {
                // THE LINE LAYER, BACK ON THIS PLATE AT TASK 4. A box it
                // painted carries no terrain glyph left to compare — the
                // same reason a `Source::Chart` box is skipped above, one
                // channel over. Both counts are asserted below so a layer
                // that started covering half the plate could not quietly
                // shrink what this compares, and WHERE a watercourse may
                // legitimately land is
                // `a_watercourse_paints_the_facets_it_runs_through`'s
                // assertion rather than this one's.
                watercourse += 1;
                continue;
            }
            compared += 1;
            if drawn.glyph != Some(want) {
                wrong.push(format!(
                    "({col},{row}): drew {:?}, want {want:?}",
                    drawn.glyph
                ));
            }
        }
    }

    assert!(
        distinct.len() > 1,
        "vacuity guard: the whole plate reads as one glyph ({distinct:?}), so \
         every raster ever written would pass this"
    );
    assert_eq!(
        skipped, 1,
        "exactly one box — the observer's own mark — is expected to be covered \
         by the perception overlay at this start; {skipped} were. A count that \
         has moved is evidence about the overlays, not a fixture to update."
    );
    assert!(
        compared > 700,
        "vacuity guard: only {compared} boxes were compared"
    );
    assert_eq!(
        watercourse, 41,
        "the line layer paints 41 of this plate's 800 boxes at this start \
         ({watercourse} were counted). A count that has moved is evidence about \
         the watercourses, not a fixture to update."
    );
    assert!(
        wrong.is_empty(),
        "{} of {compared} boxes of the walk view do not hold the facet the \
         compass chains reach:\n  {}",
        wrong.len(),
        wrong.join("\n  ")
    );
}

// The Sett, Task 4: every overlay places through the same seam the terrain
// raster was drawn with.
//
// **These are sited where the two rasters DISAGREE, and that siting was
// measured rather than assumed** (ledger "Plan defect 7"). At the seed-42
// flagship's opening position the Mercator window is re-centred on the
// observer every turn, so the observer's own facet projects to the plate's
// centre — which is also the rose raster's centre — and the packet carries
// exactly one drawable facet, the observer's own. Every overlay therefore
// agrees with the raster there for reasons that have nothing to do with
// placement being correct (ledger S16).
//
// ONE STEP SOUTH is where it comes apart, and the disagreement was measured
// before any of this was written: after `go s` the facet the walker just
// left — the flagship settlement, carrying 68 agent marks and the roster's
// own settlement site — sits at rose box (col 20, row 9) on a 40x20 plate,
// and Mercator projects it to (row 10, col 20), the observer's own box. So
// the mark and the site are drawn ON TOP OF the player, one box from where
// the walker could reach them. The other three cardinals, and both
// cardinals at the `MostPopulousSettlement` start, agree at one step; south
// from the flagship is the one that discriminates.
// ---------------------------------------------------------------------------

/// The facet arc at `anchor`'s own depth, in radians — the centre-to-centre
/// angle to its own north neighbour. Derived from `heading_rose` rather than
/// from a mesh constant, so it is the same quantity the raster's boxes are
/// spaced by.
fn facet_arc(anchor: &hornvale_kernel::Facet) -> f64 {
    let north = hornvale_locale::heading_rose(anchor)[0]
        .clone()
        .expect("the seed-42 starts are not cube corners");
    let (a, b) = (anchor.centroid(), north.centroid());
    hornvale_kernel::math::acos((a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0))
}

/// Every box of a `w`x`h` plate anchored at `anchor`, paired with the facet
/// the compass chains put there — [`rose_facet`] over the whole plate, keyed
/// by packed [`hornvale_kernel::FacetId`] so a lookup is a map hit.
///
/// Built from `heading_rose` and NOT from `RoseRaster`'s own inverse, for
/// the reason [`rose_facet`]'s doc gives: a test that asked the transport
/// where a facet went would pass on any transport at all.
fn boxes_by_facet(
    anchor: &hornvale_kernel::Facet,
    w: u16,
    h: u16,
) -> std::collections::BTreeMap<u64, (u16, u16)> {
    let (cc, cr) = (w / 2, h / 2);
    let mut map = std::collections::BTreeMap::new();
    for row in 0..h {
        for col in 0..w {
            let Some(f) = rose_facet(anchor, cc, cr, col, row) else {
                continue;
            };
            let Ok(id) = f.pack() else { continue };
            map.entry(id.0).or_insert((col, row));
        }
    }
    map
}

/// THE PERCEPTION OVERLAY PLACES BY FACET, NOT BY PROJECTION. A marked facet
/// standing NEXT TO the observer draws in that facet's own box — the box the
/// arrow key would take the walker to — and not in the observer's.
///
/// **`Source::Chart` is the instrument, and it is the layer's own
/// provenance.** `draw_perception_layer` is the only thing on this plate
/// that writes `Source::Chart` (the terrain and feature layers both write
/// `Source::World`), so a box carrying it was painted by the perception
/// packet and by nothing else. That makes "the mark landed here" assertable
/// without reaching for a private glyph constant.
///
/// **What it looked like before the seam.** Mercator projected the departed
/// facet onto the observer's own box, where `perception_boxes`' collision
/// rule — the observer never loses their own box — dropped it outright. So
/// the plate carried ONE `Source::Chart` box and the crowd the walker had
/// just stepped away from was drawn nowhere at all.
///
/// **Non-vacuity, four guards**: the step must actually have moved the
/// observer; the box under assertion must genuinely hold the facet that was
/// left (`heading_rose` names it NORTH of where the walker now stands); the
/// plate must not read as a single glyph; and the observer's own box must
/// still be painted by this layer, so a layer that drew nothing at all could
/// not satisfy the assertion by making its absence look like agreement.
#[test]
fn a_neighbouring_mark_draws_in_its_own_box() {
    let mut d = Driver::start_from_world(
        hornvale_worldgen::fixture::seed_42_world(),
        hornvale_vessel::PossessTarget::Flagship,
    )
    .expect("the committed seed-42 world starts a possession");
    d.resize(80, 24);
    let left_behind = d.observer_facet();
    d.handle("s");
    let anchor = d.observer_facet();
    assert_ne!(
        anchor, left_behind,
        "vacuity guard: `go s` did not move the observer, so there is no \
         neighbouring facet for a mark to stand on"
    );
    assert_eq!(
        hornvale_locale::heading_rose(&anchor)[0].as_ref(),
        Some(&left_behind),
        "vacuity guard: the facet the walker left must be the one `heading_rose` \
         names NORTH of where they now stand, or the box asserted below is not \
         the box that facet belongs in"
    );

    let plate = d
        .world_plate_for_redraw(80, 24)
        .expect("the walk band is handed a plate");
    let (cc, cr) = (plate.width() / 2, plate.height() / 2);
    let drawn: BTreeSet<Option<char>> = (0..plate.height())
        .flat_map(|row| (0..plate.width()).map(move |col| (col, row)))
        .map(|(col, row)| plate.get(col, row).and_then(|c| c.glyph))
        .collect();
    assert!(
        drawn.len() > 1,
        "vacuity guard: the whole plate reads as one glyph ({drawn:?})"
    );
    assert_eq!(
        plate.get(cc, cr).map(|c| c.source),
        Some(Source::Chart),
        "vacuity guard: the observer's own box is not painted by the perception \
         layer, so this plate cannot witness where that layer puts anything"
    );

    let chart: Vec<(u16, u16)> = (0..plate.height())
        .flat_map(|row| (0..plate.width()).map(move |col| (col, row)))
        .filter(|&(col, row)| plate.get(col, row).map(|c| c.source) == Some(Source::Chart))
        .collect();
    assert_eq!(
        plate.get(cc, cr - 1).map(|c| c.source),
        Some(Source::Chart),
        "the settlement the walker just stepped out of carries the packet's marks, \
         and it stands one box NORTH of the mark — the box the `up` arrow goes to. \
         The perception layer drew nothing there; it drew at {chart:?}. A layer \
         that places through a projection puts the crowd beside you on top of you."
    );
    assert_eq!(
        chart,
        vec![(cc, cr - 1), (cc, cr)],
        "exactly two facets of the packet are drawable at this position — the \
         observer's own and the settlement just left — so the perception layer \
         must paint exactly those two boxes. A count that has moved is evidence \
         about the packet, not a fixture to update."
    );
}

/// THE FEATURE LAYER PLACES THROUGH THE SAME SEAM. A point site standing on
/// a NEIGHBOUR of the observer draws in that neighbour's box.
///
/// **Drawn onto a bare `Grid`, not read off the composed plate, and that is
/// forced rather than chosen.** At this position the site and the perception
/// packet's marks stand on the SAME facet — the settlement just left — and
/// the perception layer composes last, so the composed plate can only ever
/// witness the upper of the two. Drawing this layer alone onto empty paper
/// makes every painted box this layer's own, which is what lets the
/// assertion be a set equality rather than a spot check.
///
/// **The roster is the world's own**, read through the same public builder
/// `Driver::start_from_world` uses (`plate::settlements_of`), so this is not
/// a fixture that could disagree with what the client actually draws.
///
/// **Non-vacuity, three guards**: exactly one settlement of the whole
/// 307-vertex roster lands on this 40x20 plate at all, so the set equality
/// below is asserted against a roster that really does have something to
/// place; the box it belongs in must not be the plate's centre, where a
/// re-centred Mercator window would put the observer's own facet and where
/// any placement whatsoever would look right; and the SAME roster through
/// `Placement::Mercator` must land somewhere else, which is what makes this
/// test sited at a disagreement rather than at a coincidence.
#[test]
fn a_site_on_the_neighbour_draws_in_the_neighbours_box() {
    let world = hornvale_worldgen::fixture::seed_42_world();
    let (_terrain, geo, nearest) = seed_42_reading(&world);
    let settlements = hornvale_game::plate::settlements_of(&world, &geo, &nearest);
    let sites: Vec<hornvale_game::plate::MapSite> = settlements
        .into_iter()
        .map(|(vertex, population)| hornvale_game::plate::MapSite {
            kind: hornvale_vessel::site::SiteKind::Settlement,
            vertex,
            placed: None,
            population,
        })
        .collect();

    let mut d = Driver::start_from_world(
        hornvale_worldgen::fixture::seed_42_world(),
        hornvale_vessel::PossessTarget::Flagship,
    )
    .expect("the committed seed-42 world starts a possession");
    d.resize(80, 24);
    d.handle("s");
    let anchor = d.observer_facet();
    let plate = d
        .world_plate_for_redraw(80, 24)
        .expect("the walk band is handed a plate");
    let (w, h) = (plate.width(), plate.height());
    let (cc, cr) = (w / 2, h / 2);

    let depth = anchor.path.len() as u32;
    let by_facet = boxes_by_facet(&anchor, w, h);
    // Where each settlement BELONGS, derived from the compass chains alone:
    // the facet containing its coordinate, looked up in the chain-walked
    // picture. Nothing here consults `RoseRaster`.
    let want: BTreeSet<(u16, u16)> = sites
        .iter()
        .filter_map(|site| {
            let c = site.coord(&geo);
            let p = hornvale_kernel::math::unit_sphere_from_lat_lon(c.latitude, c.longitude);
            let f = hornvale_kernel::Facet::containing(p, depth);
            by_facet.get(&f.pack().ok()?.0).copied()
        })
        .collect();
    assert_eq!(
        want.len(),
        1,
        "vacuity guard: {} of the roster's {} settlements land on this plate; the \
         assertion below needs exactly one to place",
        want.len(),
        sites.len()
    );
    let &at = want.iter().next().expect("just asserted one");
    assert_ne!(
        at,
        (cc, cr),
        "vacuity guard: the settlement belongs in the observer's own box, where a \
         re-centred Mercator window puts the observer too — every placement ever \
         written would pass"
    );

    let mut memo = hornvale_game::rose::RoseMemo::new();
    let raster = hornvale_game::rose::RoseRaster::build(&anchor, w, h, &mut memo);
    let no_volcanoes = BTreeSet::new();
    let mut mercator_paper = hornvale_game_core::Grid::new(w, h);
    hornvale_game::plate::draw_feature_layer(
        &mut mercator_paper,
        &geo,
        &hornvale_game::plate::Placement::Mercator {
            f: d.frame(),
            win: d.window(),
        },
        false,
        &sites,
        &no_volcanoes,
        &[],
        d.discovered(),
    );
    let mercator_painted: BTreeSet<(u16, u16)> = (0..h)
        .flat_map(|row| (0..w).map(move |col| (col, row)))
        .filter(|&(col, row)| mercator_paper.get(col, row).and_then(|c| c.glyph).is_some())
        .collect();
    assert_ne!(
        mercator_painted, want,
        "vacuity guard: the projection already puts this roster where the compass \
         chains do, so this test is sited at an agreement and cannot witness the \
         defect it exists for (ledger 'Plan defect 7')"
    );
    // THE POSITIVE CONTROL, and it is the whole reason the Mercator draw
    // above is in this test rather than only in its history. This layer took
    // a `Frame` and a `Window` before The Sett's Task 4 and could not be
    // handed a placement at all, so no version of this test could run red
    // against the old SIGNATURE. What can still be executed is the old
    // PLACEMENT, and this is it: it puts the settlement on the walker's own
    // box, where the perception layer's `@` then covers it entirely.
    assert!(
        mercator_painted.contains(&(cc, cr)),
        "the projection is expected to put this roster on the observer's own box \
         at ({cc}, {cr}) — that is the defect this test witnesses. It put it at \
         {mercator_painted:?}, so the control no longer reproduces the old \
         behaviour and the assertion below is no longer sited at a disagreement."
    );

    let mut paper = hornvale_game_core::Grid::new(w, h);
    hornvale_game::plate::draw_feature_layer(
        &mut paper,
        &geo,
        &hornvale_game::plate::Placement::Graph(&raster),
        false,
        &sites,
        &no_volcanoes,
        &[],
        d.discovered(),
    );
    let painted: BTreeSet<(u16, u16)> = (0..h)
        .flat_map(|row| (0..w).map(move |col| (col, row)))
        .filter(|&(col, row)| paper.get(col, row).and_then(|c| c.glyph).is_some())
        .collect();
    assert_eq!(
        painted, want,
        "the feature layer must paint the box holding the facet the site stands \
         on, and no other. It painted {painted:?}; the compass chains put the \
         roster at {want:?}. The projection puts it at {mercator_painted:?} — on \
         the walker's own box, one step from where they could reach it."
    );
}

/// THE LINE LAYER PLACES THROUGH THE SAME SEAM. A watercourse paints the
/// boxes holding the facets it actually runs through.
///
/// **The walk view drew no rivers at all between Task 3 and this one**, and
/// that was deliberate rather than an oversight: `rasterize_rivers` places
/// through `mercator::project`, so on a graph-addressed plate it would have
/// painted watercourses into boxes that do not hold the facets they run
/// through (`draw_terrain_layer_from_raster`'s own doc records the choice).
/// This is the assertion that says the line layer is back and correct.
///
/// **Two assertions, and they answer different questions.** The first is
/// PRESENCE, and it is the one that reddens on the unfixed code: exactly one
/// point of the channel network's own polylines falls inside a facet this
/// plate draws, and the box holding that facet must carry the watercourse
/// glyph. The second is PLACEMENT: every box the line layer painted must
/// hold a facet whose centre lies within one facet's width of the network,
/// measured with `SphericalPolyline::signed_distance` — the kernel's own
/// instrument, which knows nothing about either raster.
///
/// **Non-vacuity**: the count of network points inside the plate is asserted
/// rather than assumed, the witnessed box must not be the observer's own
/// (where the perception layer would cover it), and the painted set must not
/// be empty.
#[test]
fn a_watercourse_paints_the_facets_it_runs_through() {
    let world = hornvale_worldgen::fixture::seed_42_world();
    let (terrain, _geo, _nearest) = seed_42_reading(&world);

    let mut d = Driver::start_from_world(
        hornvale_worldgen::fixture::seed_42_world(),
        hornvale_vessel::PossessTarget::Flagship,
    )
    .expect("the committed seed-42 world starts a possession");
    d.resize(80, 24);
    let anchor = d.observer_facet();
    let plate = d
        .world_plate_for_redraw(80, 24)
        .expect("the walk band is handed a plate");
    let (w, h) = (plate.width(), plate.height());
    let (cc, cr) = (w / 2, h / 2);
    let depth = anchor.path.len() as u32;
    let arc = facet_arc(&anchor);
    let by_facet = boxes_by_facet(&anchor, w, h);

    // The one point of the network that falls inside a facet this plate
    // draws — found by addressing each polyline point and asking the
    // chain-walked picture, never `RoseRaster`.
    let net = terrain.channels();
    let inside: BTreeSet<(u16, u16)> = net
        .polylines
        .iter()
        .flat_map(|line| line.points.iter())
        .filter_map(|&p| {
            let f = hornvale_kernel::Facet::containing(p, depth);
            by_facet.get(&f.pack().ok()?.0).copied()
        })
        .collect();
    assert_eq!(
        inside.len(),
        1,
        "vacuity guard: {} network points land in facets this plate draws; the \
         presence assertion below needs exactly one witness",
        inside.len()
    );
    let &witness = inside.iter().next().expect("just asserted one");
    assert_ne!(
        witness,
        (cc, cr),
        "vacuity guard: the witness is the observer's own box, which the \
         perception layer paints over"
    );

    // RIVER_GLYPH is private to `plate`; it is the one glyph on this plate
    // that `glyph_for` can never return, so a box drawn with it came off the
    // line layer and off nothing else.
    let river: BTreeSet<(u16, u16)> = (0..h)
        .flat_map(|row| (0..w).map(move |col| (col, row)))
        .filter(|&(col, row)| plate.get(col, row).and_then(|c| c.glyph) == Some('"'))
        .collect();
    assert!(
        river.contains(&witness),
        "a channel of the network runs through the facet drawn at {witness:?}, and \
         the walk view drew no watercourse there. It drew watercourses at \
         {river:?}."
    );

    assert!(
        !river.is_empty(),
        "vacuity guard: the line layer painted nothing at all"
    );
    let mut adrift: Vec<String> = Vec::new();
    for &(col, row) in &river {
        let facet = rose_facet(&anchor, cc, cr, col, row).expect("a painted box holds a facet");
        let p = facet.centroid();
        let d_rad = net
            .polylines
            .iter()
            .map(|line| line.signed_distance(p).abs())
            .fold(f64::INFINITY, f64::min);
        if d_rad > arc {
            adrift.push(format!("({col},{row}): {d_rad:.8} rad from any channel"));
        }
    }
    assert!(
        adrift.is_empty(),
        "{} of {} watercourse boxes hold facets more than one facet's width \
         ({arc:.8} rad) from any channel in the network:\n  {}",
        adrift.len(),
        river.len(),
        adrift.join("\n  ")
    );
}

/// A redraw that has not moved costs no mesh searches at all (The Sett,
/// Task 9).
///
/// `plate::terrain_at_facet` resolves corner weights at two addresses per
/// box — the grid-level ancestor, and the box's own facet for The Hachure's
/// bilinear height blend. The second was un-memoised until Task 9, and since
/// the graph arm keeps no [`hornvale_game::tiles::TileCache`] to amortise it
/// against, it ran four `NearestVertexIndex` scans per box on **every**
/// frame: ledger S21 measured that one line at 6.821 ms of a 9.712 ms
/// walk-band redraw, 65.3%.
///
/// Asserted on `Driver::rose_mesh_memo_counters`, never on a duration — a
/// timing assertion in a suite is a flake on a loaded box.
///
/// **The first draw's own count is the non-vacuity half**, and it is the
/// half that fails when the memoisation is removed: a cold walk plate must
/// miss on the order of its own box count, because nearly every box of the
/// rose raster addresses a distinct facet. A memo that never misses is a
/// memo nothing is asking.
#[test]
fn a_stationary_walk_redraw_costs_no_new_mesh_searches() {
    let (w, h) = (
        hornvale_game_core::MIN_WIDTH,
        hornvale_game_core::MIN_HEIGHT,
    );
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    // `Driver::start` opens on the walk band, so this is the graph arm with
    // no gesture at all — `enter_band_b` would submit `map` and take the
    // Mercator arm instead, which is the very path this is not about.
    let plate = driver
        .world_plate_for_redraw(w, h)
        .expect("the flagship opens on the walk band");
    let boxes = u64::from(plate.width()) * u64::from(plate.height());
    let (_, cold_misses) = driver.rose_mesh_memo_counters();

    assert!(
        cold_misses * 2 > boxes,
        "a cold {}x{} walk plate took {cold_misses} mesh searches over {boxes} boxes; \
         the height blend is not consulting the memo at all",
        plate.width(),
        plate.height()
    );

    let _ = driver.world_plate_for_redraw(w, h);
    let (warm_hits, warm_misses) = driver.rose_mesh_memo_counters();
    assert_eq!(
        warm_misses, cold_misses,
        "redrawing without moving must take no further mesh searches"
    );
    assert!(
        warm_hits > cold_misses,
        "the second draw served {warm_hits} consults from a memo of {cold_misses} \
         entries: it cannot have redrawn the whole plate"
    );
}

/// The walk view's mesh memo is emptied once it outgrows the plate it is
/// serving (The Sett, Task 9).
///
/// [`hornvale_kernel::RoomMeshMemo`] is the KERNEL's and is append-only by
/// design — nothing ever invalidates an entry, because a key carries
/// everything its derivation reads — and `Driver::rose_mesh_memo` is a
/// field, so it would otherwise grow for the life of the process. Eviction
/// belongs to this owner rather than to the kernel type: three other
/// consumers hold one, all of them scoped to a tick or a session, and none
/// of them has this problem.
///
/// **Both directions, because only one of them is the loud failure.** A
/// bound that never fires leaks; a bound that fires every frame silently
/// returns the client to the un-memoised cost, with no observable
/// difference in any answer. So this drives a plate large enough to overrun
/// a small plate's cap, asserts the next small redraw empties the memo, and
/// then asserts the redraw AFTER that does not — which is the same
/// stationary-reuse claim the test above makes, re-asserted on the far side
/// of a clear.
///
/// **The second direction is asserted on HITS, and the reason is a mutation
/// this test failed.** An entry count cannot see a clear-every-frame bug at
/// all: the memo refills to the same number each time, so the obvious
/// `settled == after_clear` holds under exactly the defect it was written
/// against. `if true || ...` left this test green — the sibling above is
/// what reddened. Hits reset with the memo and cannot be refilled by the
/// draw that follows, so they can.
#[test]
fn the_walk_views_mesh_memo_is_emptied_once_it_outgrows_the_plate() {
    let (small_w, small_h) = (
        hornvale_game_core::MIN_WIDTH,
        hornvale_game_core::MIN_HEIGHT,
    );
    let (big_w, big_h) = (240u16, 70u16);
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();

    let small = driver
        .world_plate_for_redraw(small_w, small_h)
        .expect("the flagship opens on the walk band");
    let small_cap = Driver::rose_mesh_memo_cap(small.width(), small.height());
    let big = driver
        .world_plate_for_redraw(big_w, big_h)
        .expect("a larger terminal draws a larger plate");
    let (_, filled) = driver.rose_mesh_memo_counters();

    // Non-vacuity: without this the clear below could never be reached and
    // the test would pass by never exercising the branch it names.
    assert!(
        filled > small_cap,
        "a {}x{} plate filled the memo to {filled} entries, which does not exceed the \
         {}x{} plate's cap of {small_cap}: the clear below is unreachable",
        big.width(),
        big.height(),
        small.width(),
        small.height()
    );

    let _ = driver.world_plate_for_redraw(small_w, small_h);
    let (hits_after_clear, after_clear) = driver.rose_mesh_memo_counters();
    assert!(
        after_clear <= small_cap,
        "the memo held {after_clear} entries after a redraw at the smaller plate, past \
         its own cap of {small_cap}"
    );
    assert!(
        after_clear < filled,
        "the memo was not emptied at all: {after_clear} entries against {filled}"
    );

    let _ = driver.world_plate_for_redraw(small_w, small_h);
    let (hits_settled, settled) = driver.rose_mesh_memo_counters();
    assert_eq!(
        settled, after_clear,
        "the bound fired on a redraw that was inside its own cap, which returns the \
         walk view to the un-memoised cost while changing no answer anyone can see"
    );
    // The counts alone cannot say that, and this is the assertion that
    // actually holds the other direction: a memo emptied before EVERY draw
    // refills to the same number every time, so `settled == after_clear`
    // is satisfied by the failure it was written to catch. What a clear
    // cannot fake is the HITS, which reset with it — so the third redraw
    // must serve its whole plate, two addresses per box, out of the second
    // redraw's entries.
    //
    // Found by mutation, not by reading: `if true || ...` left this test
    // green and reddened only its sibling above.
    let small_boxes = u64::from(small.width()) * u64::from(small.height());
    assert_eq!(
        hits_settled - hits_after_clear,
        2 * small_boxes,
        "the third redraw served {} consults from the second's entries, not the \
         {} a {}x{} plate asks for: the memo was emptied under it",
        hits_settled - hits_after_clear,
        2 * small_boxes,
        small.width(),
        small.height()
    );
}
