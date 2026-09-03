//! The Rack's roster tests: what the struct of arrays guarantees a caller
//! that only ever sees a live `Session`.
//!
//! **Not in the commit gate today.** A new vessel test is invisible to
//! `gate-commit` until the next green chamber job rewrites
//! `docs/timings/subfloor-roster.tsv` (that file's own header says so); this
//! module runs in the stage gate from its first commit.
//!
//! **Task 2 is byte-identical, and the proof lives next door.** The claim
//! that collapsing four parallel fields into one roster changed no observable
//! output is carried by `session_snapshot.rs`'s `v2_bytes_are_pinned`
//! (`tests/fixtures/session-seed-42.json`, run WITHOUT `REBASELINE=1`) and by
//! `the_roll.rs`'s existing order and append batteries — all of which pass
//! unchanged. This module adds what those cannot see: the columns the rack
//! introduces, which nothing in Task 2 renders.

use hornvale_vessel::{PossessOpts, PossessTarget, Session, WorldContext};

/// Seed 42, the world every session below is started over — read from the
/// committed fixture (decision 0607), not rebuilt.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::seed_42_world()
}

/// Any seed's world under default pins: seed 42 from the committed fixture,
/// any other seed built in full (seed 7 is the moving-population witness this
/// module needs and has no fixture — the roster row for this file names it).
fn world_at(seed: u64) -> hornvale_kernel::World {
    if seed == 42 {
        return world();
    }
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("the seed builds")
}

/// At turn 0 every slot's `position` column holds the body's own `home` — the
/// seed `Roster::push` writes, and (before any `agent-at` fact is committed)
/// exactly what `liveness::agent_position` independently folds for that body.
///
/// **This is the VIEW ≡ SCAN invariant at its first instant** (spec §3.4):
/// the column and the ledger fold must agree at every read, and turn 0 is the
/// one moment where the agreement is entirely the seed's doing. Task 3 makes
/// the tick keep it true; this pins the starting point it has to keep. The
/// driven body's own agreement is checked against `Session::position`, which
/// IS the ledger fold — the only one of the two readings this crate exposes
/// to an integration test.
///
/// **What this test canNOT see, measured rather than assumed.** At seed 42's
/// flagship the roster is 68 bodies with exactly ONE distinct `home` room
/// between them, and `home == resource` for all 68. So this test pins *which
/// room* the seed reads, and cannot discriminate one slot's seed from
/// another's at all. The plan's prescribed mutation — seed `position` from
/// `body.resource` — is a NULL here for exactly that reason: run, and this
/// test stayed green. It reddens `roster.rs`'s
/// `a_write_moves_one_slot_only` instead, whose hand-built bodies have
/// distinct homes and a shared resource on purpose; that unit test is where
/// the per-slot discrimination actually lives.
///
/// MUTATION THIS MUST FAIL AGAINST: seed `position` one refinement level
/// coarser than the body's home in `Roster::push`
/// (`self.position.push(body.home.parent().unwrap_or_else(|| body.home.clone()));`).
/// Run and observed: `assertion left == right failed: slot 0 (Kvoavnga)
/// stands at its home / left: Facet { face: 1, path: [2, 3, 3, 1, 0, 0, 1, 2,
/// 0, 2, 3, 3] } / right: Facet { … , 1] }` — the seed names a room the body
/// is not in.
#[test]
fn at_turn_zero_every_slot_stands_at_home() {
    let world = world();
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let roster = session.roster();
    assert!(
        roster.len() > 1,
        "seed 42's flagship derives a roster worth checking, got {}",
        roster.len()
    );
    assert_eq!(
        roster.positions().len(),
        roster.len(),
        "the position column has one entry per body"
    );
    for (i, body) in roster.bodies().iter().enumerate() {
        assert_eq!(
            roster.positions()[i],
            body.home,
            "slot {i} ({}) stands at its home",
            body.label
        );
    }
    assert_eq!(
        roster.positions()[roster.driven().0],
        session.position(),
        "the driven slot's column agrees with the ledger's own fold"
    );
}

/// Every column the roster holds is the same length, on a real derived
/// session rather than a hand-built fixture — the alignment `roster.rs`'s own
/// unit test proves for `push` in isolation, checked here against the append
/// sites a live `Session` actually uses (`start_held`'s settled cast and its
/// herds). `slot_of` answers every one of them.
///
/// MUTATION THIS MUST FAIL AGAINST: drop the `self.felt.push(felt);` line
/// from `Roster::push` (`let _ = felt;` in its place, so the parameter stays
/// used). Run and observed: `assertion left == right failed: felts / left: 0
/// / right: 68`.
#[test]
fn a_live_sessions_columns_are_aligned() {
    let world = world();
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let roster = session.roster();
    let n = roster.len();
    assert!(n > 1, "seed 42's flagship derives more than one body");
    assert_eq!(roster.bodies().len(), n, "bodies");
    assert_eq!(roster.keys().len(), n, "keys");
    assert_eq!(roster.positions().len(), n, "positions");
    assert_eq!(roster.felts().len(), n, "felts");
    assert_eq!(roster.on_roll().len(), n, "on_roll");
    for (i, body) in roster.bodies().iter().enumerate() {
        assert_eq!(
            roster.slot_of(body.entity).map(|slot| slot.0),
            Some(i),
            "slot {i} ({}) answers with its own index",
            body.label
        );
    }
}

/// The driven slot names the driven body when it is NOT slot `0` — the case
/// a flagship possession cannot exercise, since `derive_npcs` hoists the
/// flagship's own body to the front and every default session drives slot 0.
///
/// A `PossessTarget::Creature` session over a body chosen from the middle of
/// the roster is what makes the assertion non-vacuous: `driven()` must be
/// that body's slot, `driven_body()` must be that body, and the mask entry
/// there must be forced on (spec §3.8: the body you are is always advanced).
///
/// MUTATION THIS MUST FAIL AGAINST: return `Slot(0)` from `Roster::driven()`.
/// Run and observed: `assertion left != right failed: the chosen body is not
/// slot 0 / left: 0 / right: 0` — the roster reports the flagship's slot for
/// a session that possessed something else.
#[test]
fn a_non_zero_driven_slot_names_its_own_body() {
    let world = world();
    let ctx = WorldContext::build(&world).expect("seed 42 builds a context");
    let (flagship, _) = Session::start_in(&ctx, &PossessOpts::default()).expect("starts");
    // A body from the middle of the roster, so neither the front nor the back
    // could pass by accident.
    let target = flagship.bodies()[flagship.bodies().len() / 2].clone();
    assert_ne!(
        target.entity,
        flagship.driven_body().entity,
        "the chosen body must not be the flagship's own, or this proves nothing"
    );
    let (session, _) = Session::start_in(
        &ctx,
        &PossessOpts {
            target: PossessTarget::Creature(target.entity),
            ..Default::default()
        },
    )
    .expect("a derived creature is possessable");
    let roster = session.roster();
    let driven = roster.driven();
    assert_ne!(driven.0, 0, "the chosen body is not slot 0");
    assert_eq!(
        roster.slot_of(target.entity),
        Some(driven),
        "the reverse index and the driven slot agree"
    );
    assert_eq!(
        roster.bodies()[driven.0].entity,
        target.entity,
        "the driven slot indexes the possessed body"
    );
    assert_eq!(
        session.driven_body().entity,
        target.entity,
        "and so does the session's own accessor"
    );
    assert!(
        roster.on_roll()[driven.0],
        "the driven body is always on the roll"
    );
}

/// P3 — **VIEW ≡ SCAN**: at every read, every slot's `position` column equals
/// the ledger's own fold for that body (spec §3.4, and the global constraint
/// this campaign carries: "a disagreement is a writer bug"). Checked after
/// each verb of a real script — `look`, `go north`, `wait 30`, `enter`, `go
/// north` — so the invariant is asserted on a stationary turn, a walking
/// turn, a tick that advances the whole population, and a band change.
///
/// **The two halves are independently derived.** The column is written by
/// `Session::wait` from what the walk returned (`liveness::Written`) and by
/// `Session::commit_agent_at` for the driven body's own verbs; the scan is
/// `Session::position_of`, which folds `agent-at` facts out of the ledger
/// through `liveness::agent_position` and never consults the column. They
/// agree only if every writer is right.
///
/// **TWO SEEDS, AND THE SECOND ONE IS NOT DECORATION.** Seed 42's flagship —
/// this module's world everywhere else — commits **zero** `agent-at` facts
/// for its 68 bodies across 500 simulated days (measured directly: spans of
/// 1, 5, 30, 100 and 365 all report 0). Its population never changes room at
/// all, so on seed 42 alone every non-driven slot's column still holds the
/// seed its append wrote, the ledger fold returns that same home, and
/// DELETING the tick's entire write-back leaves this test green. Seed 7's
/// population does walk — 74 of its 102 bodies leave home inside 30 days —
/// so that is where the tick's half is actually witnessed. The two assertions
/// at the end refuse to let either half go vacuous again without saying so.
///
/// **Not in the commit gate today** — see this module's own header.
///
/// MUTATION THIS MUST FAIL AGAINST (the tick's half): in `Session::wait`,
/// drop the write-back — replace the `for w in written` loop's body with
/// `let _ = w;`. Run and observed, on the seed-7 pass:
/// `assertion `left == right` failed: after "wait 30", slot 1
/// (Kwawkwapzow) — the column and the ledger's own fold disagree
///   left: Facet { face: 1, path: [3, 0, 3, 1, 3, 2, 2, 1, 1, 1, 2, 3, 0] }
///  right: Facet { face: 1, path: [3, 0, 3, 3, 1, 0, 1, 1, 1, 1, 0, 1, 1] }`
/// — and the SAME mutation is a NULL on seed 42 alone (run: green), which is
/// what the two-seed paragraph above exists for.
///
/// MUTATION THIS MUST FAIL AGAINST (the driven body's half): in
/// `Session::commit_agent_at`, drop `self.roster.place(driven,
/// position.clone());` (`let _ = driven;` in its place). Run and observed, on
/// the seed-42 pass: `assertion `left == right` failed: after "go north",
/// slot 0 (Kvoavnga) — the column and the ledger's own fold disagree
///   left: Facet { face: 1, path: [2, 3, 3, 1, 0, 0, 1, 2, 0, 2, 3, 3, 1] }
///  right: Facet { face: 1, path: [2, 3, 3, 1, 0, 0, 1, 2, 0, 3, 2, 2, 0] }`
#[test]
fn every_slots_position_is_the_ledgers() {
    // Seed 42's flagship: the driven body's own writer, which its `go north`
    // exercises and nothing else in this suite pins.
    let (driven_moved, _) = walk_a_script(42);
    assert!(
        driven_moved,
        "seed 42's script must move the DRIVEN body, or `commit_agent_at`'s \
         write is untested"
    );
    // Seed 7: a population that actually walks, which is the only way to
    // witness the tick's own write-back.
    let (_, other_moved) = walk_a_script(7);
    assert!(
        other_moved,
        "seed 7's script must move somebody the TICK walks, or the tick's \
         write-back is untested"
    );
}

/// Walk one seed's session through the script, checking VIEW ≡ SCAN after
/// every verb, and report whether the driven body and any other body actually
/// left home — the two vacuity questions the caller asserts on.
fn walk_a_script(seed: u64) -> (bool, bool) {
    let world = world_at(seed);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    // The invariant must already hold before anything moves, or a later
    // agreement could be an accident of nothing having happened.
    check_view_equals_scan(&session, "start");
    let (mut driven_moved, mut other_moved) = (false, false);
    for verb in ["look", "go north", "wait 30", "enter", "go north"] {
        let _ = session.handle(verb);
        check_view_equals_scan(&session, verb);
        // THE GUARDS READ THE LEDGER, NOT THE COLUMN. Asking whether the
        // COLUMN has left home would take the answer from the very thing
        // under test: a writer that never wrote would report "nobody moved",
        // which reads as a fixture that failed to exercise the code rather
        // than as the bug it is. `position_of` is the independent half.
        let (driven, len) = (session.roster().driven(), session.roster().len());
        for i in 0..len {
            let home = session.roster().bodies()[i].home.clone();
            let moved = session.position_of(hornvale_vessel::roster::Slot(i)) != home;
            if i == driven.0 {
                driven_moved |= moved;
            } else {
                other_moved |= moved;
            }
        }
    }
    (driven_moved, other_moved)
}

/// The invariant under POSSESSION, which the free-session sweep above cannot
/// reach — and the case that broke it (Task 3 fix round 1), and the case The
/// Minute repaired (spec §3.1, §3.2).
///
/// **Why possession is a different question.** Free, the walk is asked
/// through a `PlayerController` that always Holds, and `Hold` never moves
/// `st.pos` — so the walk's ending room and the ledger's agree trivially.
/// Possessed, it is asked through an `ImposedController`, which acts: the
/// body walks to water and drinks mid-wait (`session.rs`'s own comment at
/// the call site says so). Before The Minute, `Session::wait` discarded that
/// walk's facts UNCONDITIONALLY — the player's verbs were what the body DID,
/// and that walk only ever supplied what the host WANTED — so the walk's
/// ending room was one **the ledger never recorded**, and fix round 1 (Task
/// 3) kept the `position` column from repeating that lie by writing it from
/// `commit_agent_at`'s own `Roster::place` only, never the discarded walk.
///
/// **Since The Minute, the walk's facts ARE committed**, so the column may
/// now be written from the walk too: `Roster::write` sets `position` and
/// `felt` together, and the position it writes is one `agent_position` will
/// agree with because the facts that make it true were just committed. The
/// column is still a VIEW — every writer of it must agree with the
/// ledger — and this test is what holds both writers (`place` from the
/// player's own verbs, `write` from the tick) to that.
///
/// Seed 7 because seed 42's flagship population never moves at all (see
/// `every_slots_position_is_the_ledgers`), and a possessed body that never
/// walks cannot exhibit this.
///
/// MUTATION THIS MUST FAIL AGAINST — and this one is not hypothetical, it is
/// the code as it stood before this fix round: write the driven slot's
/// position from the solo walk (`self.roster.write(driven_slot,
/// driven_written.position, driven_written.felt);` in place of the
/// felt-only `resolve` method this fix round predates — `resolve` is gone
/// now (The Minute, spec §3.2 deleted it), because the driven walk's facts
/// are committed and `write` is honest for every slot). Run and observed:
/// `assertion `left == right` failed: after "!wait 5", slot 0 (Zhaqbwawshow) —
/// the column and the ledger's own fold disagree
///   left: Facet { face: 1, path: [3, 0, 3, 1, 3, 2, 2, 1, 1, 3, 1, 3, 3] }
///  right: Facet { face: 1, path: [3, 0, 3, 1, 3, 2, 2, 1, 1, 1, 2, 3, 0] }`
///
/// The Minute extended the script to the eight-wait shape its P2 measures,
/// so the column is checked after every accumulating step of the walk, not
/// just three.
#[test]
fn a_possessed_sessions_columns_are_the_ledgers_too() {
    let world = world_at(7);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let _ = session.handle("!possess");
    assert!(
        session.possessor().is_some(),
        "possession must actually be open, or this is the free sweep again"
    );
    check_view_equals_scan(&session, "!possess");
    for verb in [
        "!wait 1", "!wait 5", "!wait 5", "!wait 5", "!wait 5", "!wait 5", "!wait 5", "!wait 5",
        "!wait 30",
    ] {
        let _ = session.handle(verb);
        check_view_equals_scan(&session, verb);
    }
    // The felt column must have been written, or the driven walk never ran
    // and this test proves nothing about it. (Position is deliberately NOT
    // asserted to have moved: the whole point is that the possessed body's
    // own walk moves nothing the ledger records.)
    assert!(
        session.driven_mode().is_some(),
        "the driven body's own walk must have resolved something across the \
         nine-wait script, or nothing here exercised the driven writer"
    );
}

/// Every slot's `position` column against the ledger's own fold, at one
/// moment. The scan half is deliberately a separate call per slot rather than
/// a batch: `Session::position_of` is the ledger fold, and calling it once
/// per slot is what makes the comparison per-slot rather than aggregate.
fn check_view_equals_scan(session: &Session, after: &str) {
    let roster = session.roster();
    for i in 0..roster.len() {
        assert_eq!(
            roster.positions()[i],
            session.position_of(hornvale_vessel::roster::Slot(i)),
            "after {after:?}, slot {i} ({}) — the column and the ledger's \
             own fold disagree",
            roster.bodies()[i].label
        );
    }
}

/// The driven slot carries what the three deleted side-fields carried: the
/// accessors are `None` before the first `!wait` and `Some` after it, they
/// report the driven body's OWN tick (not a neighbour's), and
/// `override_record` still accumulates across ticks.
///
/// **`None` before the first `!wait` is the contract, and it is not free.**
/// The `felt` column is SEEDED at the append (Task 2), so a naive
/// `felts()[driven]` would answer `Some` from the moment a session starts —
/// silently promoting a stateless read into "the host's own resolution", the
/// exact confusion `Felt`'s doc says the column must not create. The `written`
/// flag is what keeps the promise; this test is what holds it to it.
/// `controller_swap.rs` and `ask_verb.rs` exercise the `Some` arm heavily and
/// would not notice the `None` arm going wrong in either direction.
///
/// MUTATION THIS MUST FAIL AGAINST (the accumulation): in `Session::wait`,
/// empty the `driven_overrides` fold (`let _ = drive;` as the loop body).
/// Run and observed: `the rider's overrides must accumulate across the
/// possession, got 0 then 0`.
///
/// MUTATION THIS MUST FAIL AGAINST (the `None` arm): make
/// `Roster::resolved_felt` unconditional (`Some(&self.felt[slot.0])`). Run
/// and observed: `assertion `left == right` failed: no tick has run, so the
/// driven body has reached no commitment mode / left: Some(Idle) / right:
/// None` — the append's stateless seed reported as a resolution.
#[test]
fn the_driven_slot_carries_what_the_side_fields_did() {
    let world = world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    assert_eq!(
        session.driven_mode(),
        None,
        "no tick has run, so the driven body has reached no commitment mode"
    );
    assert_eq!(
        session.driven_affect(),
        None,
        "…nor expressed a felt state of its own"
    );
    assert_eq!(
        session.driven_affect_object(),
        None,
        "…nor felt anything ABOUT a drive"
    );
    assert!(
        session.suppressed_drives().is_empty(),
        "…and has discarded no ranks: {:?}",
        session.suppressed_drives()
    );
    assert!(
        session.override_record().is_empty(),
        "…and overridden nothing: {:?}",
        session.override_record()
    );

    let _ = session.handle("!wait 30");
    let mode = session
        .driven_mode()
        .expect("one tick has run, so the driven body has a mode");
    let affect = session
        .driven_affect()
        .expect("…and a felt state its own arbitration expressed");
    let after_first: u32 = session.override_record().values().sum();

    let driven = session.roster().driven();
    assert_eq!(
        session.roster().felts()[driven.0].mode,
        mode,
        "driven_mode() reads the driven slot's own column"
    );
    assert_eq!(
        session.roster().felts()[driven.0].affect.label,
        affect,
        "driven_affect() reads the driven slot's own column"
    );
    assert_eq!(
        session.roster().felts()[driven.0].suppressed,
        session.suppressed_drives(),
        "suppressed_drives() reads the driven slot's own column"
    );

    let _ = session.handle("!wait 30");
    let after_second: u32 = session.override_record().values().sum();
    assert!(
        after_second > after_first && after_second > 0,
        "the rider's overrides must accumulate across the possession, got \
         {after_first} then {after_second}"
    );

    // THE DRIVEN SLOT, NOT SLOT 0. Every assertion above runs on a flagship
    // session, whose driven slot IS 0 — so a mis-indexed accessor reading
    // `felts()[0]` would pass every one of them. A `PossessTarget::Creature`
    // session drives a body from the middle of the roster, where slot 0 is
    // somebody else entirely, and slot 0 has been written by the population
    // walk on the same tick (it is on the roll), so the wrong read returns a
    // real, plausible felt state rather than a `None` that would give itself
    // away.
    let ctx = WorldContext::build(&world).expect("seed 42 builds a context");
    let (flagship, _) = Session::start_in(&ctx, &PossessOpts::default()).expect("starts");
    let target = flagship.bodies()[flagship.bodies().len() / 2].clone();
    let (mut ridden, _) = Session::start_in(
        &ctx,
        &PossessOpts {
            target: PossessTarget::Creature(target.entity),
            ..Default::default()
        },
    )
    .expect("a derived creature is possessable");
    let _ = ridden.handle("!wait 30");
    let driven = ridden.roster().driven();
    assert_ne!(driven.0, 0, "the ridden body is not slot 0");
    assert_eq!(
        ridden.driven_mode(),
        Some(ridden.roster().felts()[driven.0].mode),
        "driven_mode() reads slot {} — the body being ridden",
        driven.0
    );
    assert_eq!(
        ridden.driven_affect(),
        Some(ridden.roster().felts()[driven.0].affect.label),
        "driven_affect() reads the ridden body's slot"
    );
    assert_eq!(
        ridden.suppressed_drives(),
        ridden.roster().felts()[driven.0].suppressed,
        "suppressed_drives() reads the ridden body's slot"
    );
}

/// The walk-band CHART places every creature's mark from the roster's
/// `position` column, and it follows a creature that has walked away from
/// home (The Rack, final review).
///
/// **Why this test exists at all: the fold it replaces was UNPINNED.** Until
/// the final review, `purview_scene` folded `liveness::agent_position` once
/// per NPC to place its mark — ~67 ledger folds on every walk-band
/// `snapshot`, since `snapshot`'s `Walk` arm calls `purview(0)`. That fold
/// lived outside `session.rs` and therefore outside `TurnWork`, so
/// `turn_budget.rs::a_snapshot_performs_no_folds` read zero while all
/// sixty-seven of them ran one module over. Deleting it in favour of the
/// column is byte-identical by VIEW ≡ SCAN (decision 0597) — and that
/// byte-identity is exactly why nothing in the suite objected either way.
///
/// **MEASURED, and it is the reason this test is written rather than a doc
/// sentence.** With the fix in place, the replacement mutation — make
/// `purview_scene` read `&npc.home` instead of the room it is handed — was
/// applied to a scratch copy and the WHOLE vessel crate run against it:
/// `626 passed; 0 failed` (lib) and `399 passed; 0 failed` (suite). A NULL.
/// Nothing anywhere asserted that a creature's chart mark follows the
/// creature, because seed 42's flagship never leaves home (its residents
/// condense onto fresh water) and every other chart test is at that seed.
/// So the deleted fold could have been returning `home` all along and the
/// suite would have stayed green. This test closes that hole at a seed whose
/// residents actually walk.
///
/// MUTATION THIS MUST FAIL AGAINST: in `purview_scene`, take the mark's room
/// from the body rather than the column —
/// `for (npc, _at) in npcs { let at_room = &npc.home; … }`. Run and observed:
/// `the chart drew "Bwaakkwak" on a room where no creature of that name
/// stands — the mark did not follow the creature`. The SAME mutation against
/// the pre-existing suite is the null recorded above; this test is the
/// difference.
#[test]
fn the_chart_marks_a_creature_where_it_now_stands_not_where_it_lives() {
    let world = world_at(7);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    // Seed 7's residents leave home within thirty days — the same fact
    // `every_slots_position_is_the_ledgers` relies on, and the reason this
    // test is not written at seed 42.
    //
    // **SEVERAL waits, not one long one, and the difference is measured, not
    // stylistic.** A single `wait 60` from a fresh session draws 27 marks and
    // **zero** of them names a creature that has left home; the same sixty
    // days taken as `1, 4, 5, 10, 10, 30` draws 25 and **fifteen** do. A body
    // off the roll is frozen and caught up on return (The Roll), so how a
    // span is divided changes which bodies are near enough to be drawn while
    // they are away from home. The vacuity guard at the foot of this test is
    // what makes that difference visible instead of silently halving the
    // test.
    for span in [
        "wait 1", "wait 4", "wait 5", "wait 10", "wait 10", "wait 30",
    ] {
        let _ = session.handle(span);
    }

    // Read every body's true room ONCE, from the ledger fold rather than the
    // column — the independent half of VIEW = SCAN, so a broken column
    // cannot supply its own alibi.
    let roster = session.roster();
    let standing: Vec<hornvale_kernel::Facet> = (0..roster.len())
        .map(|i| session.position_of(hornvale_vessel::roster::Slot(i)))
        .collect();

    let chart = session.purview(0).expect("the walk-band chart draws");
    let mut checked = 0usize;
    // Bound to a local on its OWN line so `cargo fmt` cannot reflow the waiver
    // off it — fmt moved this waiver off its token twice while this test was
    // being written, which is the ratchet hazard this campaign's retrospective
    // records.
    let drawn_squares = &chart.cells; // lexicon: SurroundsCell is a chart AREA unit, never a mesh vertex
    for drawn in drawn_squares {
        for mark in drawn.marks.iter().filter(|m| m.kind == "agent") {
            // A label may name several bodies (the wild fauna are labelled by
            // species), so the claim is "SOME body of this name really stands
            // here" rather than "this exact body does" — which is the
            // strongest thing a chart mark can be held to, and is still false
            // the moment a mark is drawn from anything but a live position.
            let here = (0..roster.len()).any(|i| {
                roster.bodies()[i].label == mark.noun
                    && standing[i].pack().map(|f| f.0) == Ok(drawn.room)
            });
            assert!(
                here,
                "the chart drew {:?} on a room where no creature of that name \
                 stands — the mark did not follow the creature",
                mark.noun
            );
            // Non-vacuity: this mark is only evidence if the body it names has
            // actually left home, since at a static seed home and position are
            // the same room and every placement rule agrees.
            if (0..roster.len()).any(|i| {
                roster.bodies()[i].label == mark.noun
                    && standing[i].pack().map(|f| f.0) == Ok(drawn.room)
                    && standing[i] != roster.bodies()[i].home
            }) {
                checked += 1;
            }
        }
    }
    assert!(
        checked > 0,
        "no drawn mark named a creature that had left home, so this test \
         asserted nothing — pick a seed or a span where a resident walks \
         within the chart's own radius"
    );
}
