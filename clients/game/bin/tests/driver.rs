//! `Driver`'s acceptance tests: the containment (only JSON crosses out) and
//! the loop (a verb advances the turn).

use hornvale_game::driver::Driver;
use hornvale_game::input::Action;
use hornvale_game_core::{CommandLine, Focus, Source, render_with, spread};

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
/// existing resident is driven. The test's name still says `mints` and is left
/// alone deliberately: the rename is a code change and belongs in its own
/// commit, not folded into a prose correction. It is cheap when someone does
/// it — this crate is outside the cargo workspace, so no `nextest` filter and
/// no `subfloor-roster.tsv` entry names this test; only `make game-check` runs
/// it. Carried as a followup in `docs/retrospectives/the-hand.md`.
///
/// The two settlements' names and populations are **not** restated here: they
/// are a reading of one world rather than an invariant, and `PossessTarget`'s
/// own doc carries them along with the campaign that last moved them. This
/// asserts only the property the variant needs, which is that the two differ.
#[test]
fn the_most_populous_target_mints_at_a_different_settlement_than_flagship() {
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
        // The strip now carries the sight-disclosure caption after the name
        // (Task 5): the name first, then the honesty line.
        let at_observer_text = at_observer
            .as_deref()
            .expect("the strip always reports once the map is focused");
        assert!(
            at_observer_text.starts_with("Vngashngatva"),
            "the observer's own box must resolve to seed 42's real landmass name, got {at_observer_text:?}"
        );
        assert!(
            at_observer_text.contains("bugbear"),
            "the disclosure rides along: {at_observer_text:?}"
        );

        // Five columns west: no chart cell projects onto this box (verified by
        // instrumenting `chart::cell_at` directly), so the resolver honestly
        // reports UNNAMED_TERRAIN rather than repeating the observer's name.
        driver.apply(Action::CursorBy(-5, 0));
        let five_west = driver.strip_text().map(str::to_string);
        assert_ne!(
            five_west, at_observer,
            "the strip must change when the cursor moves off the observer's box"
        );
        let five_west_text = five_west
            .as_deref()
            .expect("the strip always reports once the map is focused");
        assert!(
            five_west_text.starts_with("unnamed terrain"),
            "five west is honestly unnamed, got {five_west_text:?}"
        );

        // And moving back must restore the observer's own answer — proving the
        // dependency runs both ways, not just away from the start.
        driver.apply(Action::CursorBy(5, 0));
        assert_eq!(driver.strip_text(), at_observer.as_deref());
    });
}

/// FIX ROUND 2: `resolve_walk_band` must resolve against the plate's REAL
/// content height, not a fixed floor assumption. `Driver::resize` threads
/// the live terminal height in exactly the way `main`'s `play` loop does;
/// this test plays the same proof the round-2 reviewer ran directly
/// against seed 42's real flagship chart: at the default cursor position
/// `(20, 10)` (the plate's centre at the 20-row floor), a taller terminal
/// (40 rows -> content height 36, centre row 18) makes `(20, 10)` a
/// different, off-centre box — no chart cell lands there — so the strip
/// must change from the observer's real name to `UNNAMED_TERRAIN`, not
/// silently keep answering as if the plate were still 20 rows tall.
#[test]
fn resize_re_resolves_against_the_real_plate_height() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    submit_line(&mut driver, "map");
    let at_floor_height = driver.strip_text().map(str::to_string);
    assert!(
        at_floor_height
            .as_deref()
            .is_some_and(|t| t.starts_with("Vngashngatva")),
        "at the 20-row floor, screen (20, 10) is the observer's own box, got {at_floor_height:?}"
    );

    // A 40-row terminal: real content height is 40 - RESERVED_ROWS(4) = 36
    // (the same arithmetic `spread::compose`/`content_height` apply), so
    // the real centre row is 18, not 10 -- the cursor's SCREEN position
    // (20, 10) has not moved, but it is no longer the observer's box.
    driver.resize(80, 40);
    let at_taller_height = driver.strip_text().map(str::to_string);
    assert_ne!(
        at_taller_height, at_floor_height,
        "resizing must re-resolve against the real plate height, not repeat the floor's stale answer"
    );
    let at_taller_text = at_taller_height
        .as_deref()
        .expect("the strip always reports once the map is focused");
    assert!(
        at_taller_text.starts_with("unnamed terrain"),
        "no chart cell lands on (20, 10) once the real centre moves to (20, 18), so \
         the walk-band chain comes up empty there (not None -- the strip always \
         reports something once the map is focused; NOTHING_HERE_YET is reserved \
         for a resolver-absent band), got {at_taller_text:?}"
    );

    // Move the cursor to what is NOW the real centre -- it must resolve
    // the observer's own name again, proving the resolver is keyed off the
    // plate's live height end to end, not merely detecting a mismatch.
    driver.apply(Action::CursorBy(0, 8)); // (20, 10) -> (20, 18), the new centre
    assert_eq!(driver.strip_text(), at_floor_height.as_deref());
}

/// The cursor's clamp must also track the real plate height (the reviewer's
/// "while you are there" check): at a 40-row terminal the plate is 36 rows
/// tall, so the cursor must be able to reach row 35 -- unreachable if the
/// clamp were still pinned to the 20-row floor.
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
}

/// FIX ROUND 1 (Task 3a's own review, regression): `Focus::Map` alone must
/// NOT activate the world view. The bug this pins: before the fix,
/// `main.rs`'s `redraw` computed `Some(driver.world_plate(w, h))` whenever
/// `driver.focus() == Focus::Map`, with no further gate. But `Focus::Map`
/// already meant something else, shipped across `9f69e4e2a`/`81d940d9c`/
/// `64c80be36` and chronicled in `book/src/chronicle/the-stride.md`/
/// `the-stylus.md`: a cursor over the WALK BAND's own small chart, with the
/// strip naming the feature it points at. Reusing the same focus value for
/// the world view retired that shipped feature into misleading dead UI: a
/// 210x56 redraw would draw a 104-column Mercator while the cursor stayed
/// clamped to the OLD 40-column plate, resolving the walk band's own chart,
/// which the Mercator had silently replaced -- a picture and a cursor/strip
/// that no longer agreed at all, and columns 40..104 permanently
/// uncursorable. This test drives the SAME call `main.rs`'s `redraw` makes
/// (`Driver::world_plate_for_redraw`, then `render_with`) and checks what
/// actually lands on the `Grid` and the strip -- it would have FAILED
/// against the pre-fix `main.rs` logic (reproduced in the `regressed_grid`
/// block below, built by calling `render_with` the OLD, ungated way).
#[test]
fn map_focus_alone_does_not_activate_the_world_view() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    submit_line(&mut driver, "map");
    assert_eq!(driver.focus(), Focus::Map);

    let (w, h) = (210u16, 56u16);

    // The gate itself: the world view defaults OFF, and nothing in this
    // campaign yet turns it on (Task 3b owns that gesture), so the gated
    // plate must be `None`.
    assert!(
        driver.world_plate_for_redraw(w, h).is_none(),
        "the world view must default OFF -- Task 3b owns the gesture that turns it on"
    );

    let json = driver.snapshot();
    let empty_line = String::new();
    let cmd_line = CommandLine {
        text: &empty_line,
        caret: 0,
    };
    // Hoisted because `world_plate_for_redraw` takes `&mut self` since the
    // plate memo (perf/world-plate-memo) -- it cannot share an expression
    // with the immutable reads below. Same call, same arguments, same
    // assertions; only the borrow is sequenced.
    let world_plate = driver.world_plate_for_redraw(w, h);
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

    // The walk band's own chart must still be drawn somewhere in the plate
    // region -- a regression that reinstates the old unconditional
    // `Some(driver.world_plate(w, h))` replaces the whole region with the
    // Mercator instead, and no cell anywhere would carry `Source::Chart`.
    let content_h = spread::content_height(h);
    let chart_drawn = (0..content_h).any(|y| {
        (0..spread::PLATE_WIDTH).any(|x| grid.get(x, y).is_some_and(|c| c.source == Source::Chart))
    });
    assert!(
        chart_drawn,
        "the walk band's own chart must still draw when the world view is off"
    );

    // And no `Source::World` cell may appear anywhere the Mercator would
    // have claimed had the old bug still gated on `Focus::Map` alone --
    // the full fit width, not just the old fixed `PLATE_WIDTH`.
    let would_be_world_width = spread::world_plate_width(w, h);
    let world_leaked = (0..content_h).any(|y| {
        (spread::PLATE_WIDTH..would_be_world_width)
            .any(|x| grid.get(x, y).is_some_and(|c| c.source == Source::World))
    });
    assert!(
        !world_leaked,
        "no world content may appear while the view is off"
    );

    // The strip must still resolve the walk band's own real name -- the
    // exact feature the regression silently retired.
    assert!(
        driver
            .strip_text()
            .is_some_and(|t| t.starts_with("Vngashngatva")),
        "the strip must still resolve against the walk band, got {:?}",
        driver.strip_text()
    );

    // Reproduce the OLD, buggy gate directly, to show the assertions above
    // really do discriminate it: `Some(driver.world_plate(w, h))`
    // unconditionally once focus is `Focus::Map`, with no `world_view`
    // check at all.
    let regressed_plate = driver.world_plate(w, h);
    let (regressed_grid, _) = render_with(
        &json,
        w,
        h,
        driver.focus(),
        driver.cursor(),
        cmd_line,
        driver.strip_text(),
        driver.echo(),
        Some(&regressed_plate),
        driver.strip_offset(),
        None,
    )
    .unwrap();
    let regressed_chart_drawn = (0..content_h).any(|y| {
        (0..spread::PLATE_WIDTH).any(|x| {
            regressed_grid
                .get(x, y)
                .is_some_and(|c| c.source == Source::Chart)
        })
    });
    assert!(
        !regressed_chart_drawn,
        "sanity check: the old unconditional gate must NOT draw the walk band's chart \
         (if it does, this test's own discrimination is broken, not the fix)"
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
