//! `Driver`'s acceptance tests: the containment (only JSON crosses out) and
//! the loop (a verb advances the turn).

use hornvale_game::driver::Driver;
use hornvale_game::input::Action;
use hornvale_game_core::Focus;

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

/// `--target most-populous-settlement` must mint at a DIFFERENT settlement
/// than the flagship default — seed 42's flagship is Googo (pop 68), the
/// most-populous settlement is Toa (pop 84), so the two snapshots'
/// `self.settlement` fields must disagree. If they ever agreed, `target`
/// would be silently ignored by the driver. (Both agents are MINTED; the
/// target chooses the settlement, not an existing resident.)
#[test]
fn the_most_populous_target_mints_at_a_different_settlement_than_flagship() {
    let flagship = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let popular =
        Driver::start(42, hornvale_vessel::PossessTarget::MostPopulousSettlement).unwrap();
    let a = hornvale_game_core::Snapshot::parse(&flagship.snapshot()).unwrap();
    let b = hornvale_game_core::Snapshot::parse(&popular.snapshot()).unwrap();
    assert_ne!(a.me.settlement, b.me.settlement);
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
fn look_mode_at_an_unresolved_band_refuses_rather_than_resolving() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    driver.handle("enter");
    let snap = hornvale_game_core::Snapshot::parse(&driver.snapshot())
        .expect("a live session always yields a parseable snapshot");
    assert!(
        matches!(snap.spatial, hornvale_game_core::Spatial::Chamber { .. }),
        "seed 42's flagship must land in the chamber band after one `enter`"
    );

    driver.apply(Action::ToggleFocus);
    driver.apply(Action::CursorBy(1, 0));
    assert!(
        driver.cursor().is_some(),
        "the cursor must exist at every band"
    );
    assert_eq!(driver.strip_text(), Some("nothing here yet"));
}

/// The walk band DOES have a resolver (the terrain-feature index, scoped to
/// the observer's own cell — see `driver.rs`'s module doc for why cursor
/// motion does not change which cell is queried this campaign). Entering
/// look mode at seed 42's flagship opening position (walk band) must report
/// a real name, not the unresolved-band refusal — this is what would catch
/// a regression that accidentally routed every band through the same
/// refusal.
#[test]
fn look_mode_at_the_walk_band_resolves_a_real_name() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let snap = hornvale_game_core::Snapshot::parse(&driver.snapshot()).unwrap();
    assert!(matches!(
        snap.spatial,
        hornvale_game_core::Spatial::Walk { .. }
    ));

    driver.apply(Action::ToggleFocus);
    let strip = driver.strip_text();
    assert!(strip.is_some(), "the walk band must resolve to something");
    assert_ne!(
        strip,
        Some("nothing here yet"),
        "the walk band has a real resolver and must not report the unresolved-band refusal"
    );
}

/// FIX ROUND 1: the cursor must genuinely track. Moving it off the
/// observer's own box changes the resolved cell, and therefore the strip
/// text, rather than recomputing the same answer regardless of position.
///
/// **Why the covering assertion is "names something" -> "unnamed terrain"
/// rather than two different NAMED features**, checked and worth recording:
/// a swept probe over every reachable box within seed 42's flagship
/// walk-band view found every `Some` box resolves to the SAME nearest
/// terrain `CellId` as the observer (`CellId(22195)`, confirmed by
/// instrumenting `resolve_walk_band` directly) — the visible neighbourhood
/// spans `distance_rad` on the order of 1e-4 (tens of metres), while
/// `NearestCellIndex` snaps onto one of only 40,962 cells tiling the whole
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
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    driver.apply(Action::ToggleFocus);
    let at_observer = driver.strip_text().map(str::to_string);
    assert_eq!(
        at_observer.as_deref(),
        Some("Vngashngatva"),
        "the observer's own box must resolve to seed 42's real landmass name"
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
    assert_eq!(five_west.as_deref(), Some("unnamed terrain"));

    // And moving back must restore the observer's own answer — proving the
    // dependency runs both ways, not just away from the start.
    driver.apply(Action::CursorBy(5, 0));
    assert_eq!(driver.strip_text(), at_observer.as_deref());
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
    driver.apply(Action::ToggleFocus);
    let at_floor_height = driver.strip_text().map(str::to_string);
    assert_eq!(
        at_floor_height.as_deref(),
        Some("Vngashngatva"),
        "at the 20-row floor, screen (20, 10) is the observer's own box"
    );

    // A 40-row terminal: real content height is 40 - RESERVED_ROWS(4) = 36
    // (the same arithmetic `spread::compose`/`content_height` apply), so
    // the real centre row is 18, not 10 -- the cursor's SCREEN position
    // (20, 10) has not moved, but it is no longer the observer's box.
    driver.resize(40);
    let at_taller_height = driver.strip_text().map(str::to_string);
    assert_ne!(
        at_taller_height, at_floor_height,
        "resizing must re-resolve against the real plate height, not repeat the floor's stale answer"
    );
    assert_eq!(
        at_taller_height.as_deref(),
        Some("unnamed terrain"),
        "no chart cell lands on (20, 10) once the real centre moves to (20, 18), so \
         the walk-band chain comes up empty there (not None -- Look mode always \
         reports something; NOTHING_HERE_YET is reserved for a resolver-absent band)"
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
    driver.resize(40);
    driver.apply(Action::ToggleFocus);
    driver.apply(Action::CursorBy(0, 1000)); // drive it hard into the bottom clamp
    let cursor = driver.cursor().expect("look mode always has a cursor");
    assert_eq!(
        cursor.y, 35,
        "the clamp must reach row 35 (content height 36, 0-indexed) at a 40-row terminal, \
         not stop at the 20-row floor's row 19"
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
#[test]
fn enter_on_an_empty_line_costs_no_turn() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    let before = d.snapshot();
    let released = d.apply(Action::Submit);
    assert!(!released);
    assert_eq!(
        d.snapshot(),
        before,
        "an empty submit must change nothing at all"
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
/// CLI *and* types — one keypress, not two (spec §2).
#[test]
fn a_printable_key_on_the_map_bounces_focus_and_types() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("genesis");
    d.apply(Action::ToggleFocus);
    assert_eq!(d.focus(), Focus::Map);
    d.apply(Action::FocusAndType('l'));
    assert_eq!(d.focus(), Focus::Cli);
    assert_eq!(d.line_text(), "l");
}
