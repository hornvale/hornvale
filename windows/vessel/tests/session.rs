//! Buffer-driven verb-loop tests — the repl::run pattern.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn, run};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn seam_world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn opts() -> PossessOpts {
    PossessOpts {
        day: WorldTime { day: 0.0 },
        echo: false,
        wild_agents: true,
    }
}

#[test]
fn possession_opens_with_a_focalized_description() {
    let world = seam_world();
    let (_s, opening) = Session::start(&world, &opts()).unwrap();
    assert!(opening.contains("in the lands of"));
    assert!(
        opening.contains("[room "),
        "the opening carries the room id"
    );
}

#[test]
fn go_moves_and_back_retraces() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let home = s.agent().position.clone();
    // find a real direction from the current room's ways-on
    let ways = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    // Exact, word-boundary token match against the "Ways on: NE, NW, S."
    // line — a substring check (e.g. `ways.contains("N")`) would false-
    // positive "n" against "NE"/"NW" whenever neither bare "N" nor "S" is
    // actually offered.
    let tokens: Vec<String> = ways
        .split([' ', ',', '.'])
        .filter(|t| !t.is_empty())
        .map(str::to_lowercase)
        .collect();
    let dir = ["n", "ne", "e", "se", "s", "sw", "w", "nw"]
        .iter()
        .find(|d| tokens.iter().any(|t| t == *d))
        .copied()
        .expect("some way on");
    match s.handle(&format!("go {dir}")) {
        Turn::Out(t) => assert!(t.contains("[room ")),
        _ => panic!("go must not release"),
    }
    assert_ne!(s.agent().position, home, "go moved");
    s.handle("back");
    assert_eq!(s.agent().position, home, "back retraces");
}

/// The refusal is DIRECTIONAL as of The Lintel: coarse-ward (`exit`, toward
/// possessing a settlement or a culture) is still refused with the byte-pinned
/// sentence, but fine-ward (`enter`) now descends — see
/// `windows/vessel/tests/the_lintel.rs`. This test therefore narrowed to the
/// half it still covers, deliberately: The Seam's contract that BOTH directions
/// refuse was overturned by this campaign, not accidentally broken by it.
#[test]
fn the_coarse_ward_exit_refuses_diegetically() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let before = s.agent().position.clone();
    let out = match s.handle("exit") {
        Turn::Out(t) => t,
        _ => panic!("exit must not release"),
    };
    assert!(out.contains("grain of the world"), "diegetic refusal");
    assert_eq!(s.agent().position, before, "no movement");
}

/// Descending must never move the WALK-band position: the band change lives in
/// session state, so `enter` leaves `agent().position` exactly where it was.
/// That is what keeps `map`, `whoami`, `purview` and the NPC layer — all of
/// which read that field — unchanged by being indoors.
#[test]
fn entering_leaves_the_walk_band_position_alone() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let before = s.agent().position.clone();
    let reply = match s.handle("enter") {
        Turn::Out(t) => t,
        _ => panic!("enter must not release"),
    };
    // Without this, the test passes whether `enter` descended or answered
    // "Nothing here is built" — and position-invariance is trivially true in
    // the second case. `Session::start` mints the flagship in its own
    // settlement, whose locale IS settlement territory, so a refusal here is a
    // real failure and not a geography accident.
    assert!(
        !reply.starts_with("Nothing here is built"),
        "the flagship's own locale is built, so this must actually descend: {reply:?}"
    );
    assert_eq!(
        s.agent().position,
        before,
        "the possession's walk-band position is untouched by descent"
    );
    s.handle("out");
    assert_eq!(s.agent().position, before);
}

#[test]
fn examine_honors_the_contract_and_release_ends() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let f = s.focalized().unwrap();
    for (noun, detail) in &f.nouns {
        match s.handle(&format!("examine {noun}")) {
            Turn::Out(t) => assert_eq!(&t, detail, "examine renders the datum"),
            _ => panic!("examine must not release"),
        }
    }
    match s.handle("examine the moon of unreason") {
        Turn::Out(t) => assert!(t.contains("You see no")),
        _ => panic!(),
    }
    assert!(matches!(s.handle("release"), Turn::Released(_)));
}

#[test]
fn wait_advances_the_day_and_moves_the_npc_layer_without_moving_you() {
    // The-quickening (T3): `wait` now runs the NPC layer's tick, so its
    // output narrates motion rather than re-describing the room. The
    // observation day still advances (visible via a follow-up `look`), and
    // the possessed agent itself still never moves — only the session's
    // owned NPC ledger evolves.
    let world = seam_world();
    let (mut s, opening) = Session::start(&world, &opts()).unwrap();
    assert!(opening.contains("day 0"));
    let home = s.agent().position.pack().unwrap().0;
    let out = match s.handle("wait 90") {
        Turn::Out(t) => t,
        _ => panic!("wait must not release"),
    };
    assert!(!out.is_empty(), "wait narrates what happened");
    match s.handle("look") {
        Turn::Out(t) => assert!(t.contains("day 90"), "the observation day moved"),
        _ => panic!("look must not release"),
    }
    assert_eq!(
        s.agent().position.pack().unwrap().0,
        home,
        "waiting does not move the possessed agent"
    );
    match s.handle("wait sideways") {
        Turn::Out(t) => assert!(t.contains("no span of days")),
        _ => panic!(),
    }
    match s.handle("wait inf") {
        Turn::Out(t) => assert!(
            t.contains("no span of days"),
            "non-finite span refused: {t}"
        ),
        _ => panic!(),
    }
}

#[test]
fn knows_grows_as_you_walk() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let before = s.knowledge().0.len();
    let ways = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!(),
    };
    // Exact, word-boundary token match — see `go_moves_and_back_retraces`'s
    // comment: a substring check false-positives "n" against "NE"/"NW".
    let tokens: Vec<String> = ways
        .split([' ', ',', '.'])
        .filter(|t| !t.is_empty())
        .map(str::to_lowercase)
        .collect();
    let dir = ["n", "ne", "e", "se", "s", "sw", "w", "nw"]
        .iter()
        .find(|d| tokens.iter().any(|t| t == *d))
        .copied()
        .expect("some way on");
    s.handle(&format!("go {dir}"));
    assert!(
        s.knowledge().0.len() > before,
        "walking accumulates knowledge"
    );
}

/// The Vessel Stitch T2: `tell` renamed `write` (G3, total — no alias).
/// Re-pins `tell_absorbs_a_spoken_common_sentence_into_knowledge`
/// (provenance: this test replaces it verbatim, verb and response
/// swapped) and adds the rename's own obligation — `tell` must now fall
/// through to the ordinary unknown-verb response.
#[test]
fn write_is_the_verb_and_the_margin_answers() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let volume = hornvale_book::render_volume(&world);
    let line = volume
        .lines
        .first()
        .expect("seed 42 renders at least one line");
    let before = s.knowledge().0.len();
    let out = match s.handle(&format!("write {line}")) {
        Turn::Out(t) => t,
        _ => panic!("write must not release"),
    };
    assert_eq!(
        out, "Written in the margin.",
        "the closed margin response, regardless of how many facts the sentence carried"
    );
    assert!(
        s.knowledge().0.len() > before,
        "writing a fact grows knowledge"
    );
    match s.handle("write") {
        Turn::Out(t) => assert!(t.contains("Write what?")),
        _ => panic!("write with no argument must not release"),
    }
    // The rename is total: no `tell` alias survives.
    match s.handle(&format!("tell {line}")) {
        Turn::Out(t) => assert!(
            t.contains("No verb 'tell'"),
            "tell falls through to the unknown-verb response: {t}"
        ),
        _ => panic!("tell must not release"),
    }
}

/// The Vessel Stitch T2's stitch law (spec §4.1, end to end): a fresh
/// session's `consult` shows the fallback and the day-0 reckoning's empty
/// arm; `write`-ing the moon sentence unlocks the initiated line, whose
/// rendered count is the LEDGER's own value — the mutation arm proves this
/// is not an echo of what was written: even a WRONG written count still
/// unlocks the key, but the printed value never moves (heard ≠ true,
/// printed — spec §1/§8.2).
#[test]
fn the_stitch_law_end_to_end() {
    let world = build_world(
        Seed(1),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 1 builds");
    let volume = hornvale_book::render_volume(&world);
    let planet_line = volume
        .lines
        .iter()
        .find(|l| l.contains(" is a planet"))
        .expect("seed 1 renders a planet line");
    assert!(
        planet_line.contains("with two moons"),
        "seed 1's planet line carries the two-moon fragment: {planet_line}"
    );

    let (mut s, _) = Session::start(
        &world,
        &PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: false,
            wild_agents: true,
        },
    )
    .unwrap();
    let before = match s.handle("consult") {
        Turn::Out(t) => t,
        _ => panic!("consult must not release"),
    };
    assert!(
        before.contains("The Book holds more for the initiated."),
        "nothing written yet: the fallback answers: {before}"
    );
    assert!(
        before.contains("The sky keeps no dates to number."),
        "day 0's true event count is zero — the empty arm: {before}"
    );

    match s.handle(&format!("write {planet_line}")) {
        Turn::Out(t) => assert_eq!(t, "Written in the margin."),
        _ => panic!("write must not release"),
    }
    let after = match s.handle("consult") {
        Turn::Out(t) => t,
        _ => panic!("consult must not release"),
    };
    assert!(
        // The Book Polish (2026-07-20): re-pinned with its subject. The Living
        // Community epoch: seed 1's rendered planet name re-derived
        // Vebe -> Xobo under the epoch's re-placement. The Wearing (this
        // merge): Xobo -> Booko — the 19 toponymic/quality concepts Task 3
        // registered shift the proto-root walk, so every lexicon-derived
        // name re-draws. The rebase onto The Toponym's cohort ordering
        // re-draws them once more: Booko -> Xoaboa. The Contour's epoch v2
        // (2026-08-02, history/bake/v2) re-mints the draw again: Xoaboa ->
        // Pao. Moon count ("two"), subject and sentence frame unchanged at
        // every step.
        after.contains("Pao has two moons, as the initiated count."),
        "the ledger's own moon-count, now unlocked: {after}"
    );
    assert!(
        !after.contains("The Book holds more for the initiated."),
        "the fallback no longer applies once something has unlocked: {after}"
    );

    // MUTATION ARM: a fresh session, told a WRONG count, still unlocks the
    // key — but the printed value is the ledger's, never the heard one.
    let (mut wrong, _) = Session::start(
        &world,
        &PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: false,
            wild_agents: true,
        },
    )
    .unwrap();
    let wrong_line = planet_line.replace("with two moons", "with nine moons");
    match wrong.handle(&format!("write {wrong_line}")) {
        Turn::Out(t) => assert_eq!(t, "Written in the margin."),
        _ => panic!("write must not release"),
    }
    let consulted = match wrong.handle("consult") {
        Turn::Out(t) => t,
        _ => panic!("consult must not release"),
    };
    assert!(
        consulted.contains("Pao has two moons, as the initiated count."),
        "heard 'nine' still renders the ledger's 'two' — heard is not true, printed: {consulted}"
    );
    assert!(
        !consulted.contains("nine"),
        "the wrong heard count never appears in what the Book confirms: {consulted}"
    );
}

/// The Vessel Stitch T2's day law (spec §4.2): `consult`'s heading tracks
/// the session's own day, monotone with play — day 0 at the start, the
/// session's actual (truncated) day after `wait`.
#[test]
fn the_day_law() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    match s.handle("consult") {
        Turn::Out(t) => assert!(
            t.starts_with("The Reckoning, at day 0."),
            "day-0 heading: {t}"
        ),
        _ => panic!("consult must not release"),
    }
    s.handle("wait 90");
    match s.handle("consult") {
        Turn::Out(t) => assert!(
            t.starts_with("The Reckoning, at day 90."),
            "the heading advances to the session's own day: {t}"
        ),
        _ => panic!("consult must not release"),
    }
}

/// The Vessel Stitch T2's purity law (spec §4.3): `consult` commits
/// nothing — the session's owned ledger is byte-identical before and
/// after, and `consult` never touches `Knowledge` either (only `write`
/// and walking do).
#[test]
fn the_purity_law() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let ledger_before = s.session_ledger_json();
    let knowledge_before = s.knowledge().clone();
    s.handle("consult");
    assert_eq!(
        s.session_ledger_json(),
        ledger_before,
        "consult must not commit anything to the ledger"
    );
    assert_eq!(
        *s.knowledge(),
        knowledge_before,
        "consult must not mutate Knowledge"
    );
}

#[test]
fn run_drives_a_script_deterministically() {
    let world = seam_world();
    let script = "look\nwhoami\nknows\nrelease\n";
    let mut out_a = Vec::new();
    let mut out_b = Vec::new();
    run(
        &world,
        PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: true,
            wild_agents: true,
        },
        std::io::Cursor::new(script),
        &mut out_a,
    )
    .unwrap();
    run(
        &world,
        PossessOpts {
            day: WorldTime { day: 0.0 },
            echo: true,
            wild_agents: true,
        },
        std::io::Cursor::new(script),
        &mut out_b,
    )
    .unwrap();
    assert_eq!(out_a, out_b, "byte-identical replays");
    let text = String::from_utf8(out_a).unwrap();
    assert!(text.contains("> look"), "echo mode echoes commands");
    assert!(text.contains("in the lands of"));
}

/// The room prints "Ways on: SE, N, SW." — every one of those tokens must be
/// a command you can actually type. This is the exact bug: the parser already
/// accepted them, but the verb dispatch never reached it.
#[test]
fn every_printed_way_out_is_a_command_you_can_type() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let ways = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    let line = ways
        .lines()
        .find(|l| l.starts_with("Ways on:"))
        .expect("a room lists its ways out")
        .to_string();
    let tokens: Vec<String> = line
        .trim_start_matches("Ways on:")
        .trim_end_matches('.')
        .split(',')
        .map(|t| t.trim().to_lowercase())
        .filter(|t| !t.is_empty())
        .collect();
    assert!(!tokens.is_empty(), "no exits to test: {line}");
    for t in tokens {
        let out = match s.handle(&t) {
            Turn::Out(o) => o,
            _ => panic!("a direction must not release"),
        };
        assert!(
            !out.contains("No verb"),
            "the room printed '{t}' as a way out but the parser rejects it: {out}"
        );
        s.handle("back");
    }
}

/// Long-form names work as bare commands too.
#[test]
fn long_direction_names_work_as_bare_commands() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let out = match s.handle("northeast") {
        Turn::Out(o) => o,
        _ => panic!("must not release"),
    };
    assert!(!out.contains("No verb"), "{out}");
}

/// A genuine non-verb still reports itself honestly — the fallthrough must not
/// swallow the error path.
#[test]
fn a_genuine_non_verb_still_reports_itself() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let out = match s.handle("xyzzy") {
        Turn::Out(o) => o,
        _ => panic!("must not release"),
    };
    assert!(out.contains("No verb 'xyzzy'"), "{out}");
}

/// The sky follows the walker. While weather was resolved from the flagship
/// settlement, a possession saw the capital's sky no matter how far it walked.
#[test]
fn the_sky_follows_the_walker() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let mut skies = std::collections::BTreeSet::new();
    for _ in 0..40 {
        let out = match s.handle("look") {
            Turn::Out(t) => t,
            _ => panic!("look must not release"),
        };
        if let Some(l) = out.lines().find(|l| l.contains("The sky is")) {
            skies.insert(l.to_string());
        }
        // Follow whatever exit this room actually offers.
        let dir = out
            .lines()
            .find(|l| l.starts_with("Ways on:"))
            .and_then(|l| l.trim_start_matches("Ways on:").split(',').next())
            .map(|d| d.trim().trim_end_matches('.').to_lowercase());
        if let Some(d) = dir {
            s.handle(&d);
        }
        s.handle("wait 3");
    }
    assert!(
        skies.len() > 1,
        "the sky never changed across a long walk: {skies:?}"
    );
}

/// Occlusion hides a percept; it must never erase knowledge already held. The
/// walker's `knows` ledger may only grow as the sky clouds over and clears.
#[test]
fn clouding_over_does_not_unlearn_what_was_seen() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let count = |t: &str| -> usize {
        t.split_whitespace()
            .next()
            .and_then(|n| n.parse().ok())
            .expect("knows reports a leading count")
    };
    s.handle("look");
    let before = match s.handle("knows") {
        Turn::Out(t) => count(&t),
        _ => panic!("knows must not release"),
    };
    for _ in 0..30 {
        s.handle("wait 9");
        s.handle("look");
    }
    let after = match s.handle("knows") {
        Turn::Out(t) => count(&t),
        _ => panic!("knows must not release"),
    };
    assert!(
        after >= before,
        "knowledge shrank from {before} to {after} as the sky changed"
    );
}

/// A bare compass token carries `go`'s indoor refusal. The bare-direction
/// fallthrough dispatches to `go` directly, so the guard has to be repeated on
/// that arm — otherwise `n` typed indoors slips past the refusal that typing
/// `go n` correctly receives.
#[test]
fn a_bare_direction_indoors_is_refused_exactly_as_go_is() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    // Find a room we can step inside, then compare the two spellings.
    let mut entered = false;
    for _ in 0..40 {
        if let Turn::Out(t) = s.handle("enter")
            && !t.contains("nothing")
            && !t.contains("no ")
        {
            entered = true;
            break;
        }
        let out = match s.handle("look") {
            Turn::Out(t) => t,
            _ => break,
        };
        let dir = out
            .lines()
            .find(|l| l.starts_with("Ways on:"))
            .and_then(|l| l.trim_start_matches("Ways on:").split(',').next())
            .map(|d| d.trim().trim_end_matches('.').to_lowercase());
        match dir {
            Some(d) if !d.is_empty() => {
                s.handle(&d);
            }
            _ => break,
        }
    }
    if !entered {
        // No enterable structure reachable in this walk; the guard is still
        // asserted by the indoor `go` tests that The Lintel shipped.
        return;
    }
    let bare = match s.handle("n") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    let spelled = match s.handle("go n") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert_eq!(
        bare, spelled,
        "the bare direction must refuse indoors exactly as `go <dir>` does"
    );
}

/// Walk to the sea, then descend and rise through the water column. This is
/// the campaign's whole claim: a coordinate at sea is more than one place.
#[test]
fn the_water_column_is_a_place_you_can_be() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    // A fixed compass cycle cannot make progress on a 3-exit triangular mesh;
    // biasing the attempts westward drifts the walker to the coast, and the
    // failed attempts are harmless no-ops.
    let mut afloat = String::new();
    for _ in 0..600 {
        for d in ["w", "nw", "sw"] {
            s.handle(d);
        }
        if let Turn::Out(t) = s.handle("look")
            && t.contains("Open water")
        {
            afloat = t;
            break;
        }
    }
    assert!(
        !afloat.is_empty(),
        "the walker never reached water; the column cannot be tested"
    );

    // On the surface: afloat on open water, not standing in the floor's biome.
    assert!(afloat.contains("Open water —"), "{afloat}");

    // Down: a different place at the same coordinate.
    let under = match s.handle("dive") {
        Turn::Out(t) => t,
        _ => panic!("dive must not release"),
    };
    assert!(
        !under.contains("Open water —"),
        "diving must leave the surface: {under}"
    );
    assert_ne!(
        afloat, under,
        "the surface and the water below it rendered identically"
    );

    // Lateral movement is refused while under, and says so diegetically.
    let lateral = match s.handle("n") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert!(!lateral.contains("No verb"), "{lateral}");
    assert!(lateral.contains("Surface first"), "{lateral}");

    // And back up.
    let up = match s.handle("surface") {
        Turn::Out(t) => t,
        _ => panic!("surface must not release"),
    };
    assert!(up.contains("You break the surface"), "{up}");
    assert!(up.contains("Open water —"), "{up}");
}

/// On land there is no column, and the refusal says why rather than reading
/// as a parse failure.
#[test]
fn there_is_nothing_to_dive_into_on_dry_land() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let out = match s.handle("dive") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert!(out.contains("no water here"), "{out}");
    let up = match s.handle("surface") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert!(up.contains("already at the surface"), "{up}");
}

/// The Deep Realm, Task 5: at a cell with no cave, `delve` refuses and names
/// the absence — the first of the three outcomes `dive`'s own doc warns a
/// descent verb must distinguish. The other two (a cave whose entrance is
/// SEALED vs. a cave that actually descends) are exercised in
/// `windows/vessel/src/session.rs`'s own internal tests
/// (`delve_has_three_distinguishable_outcomes`), which need a hand-picked
/// cave cell — a terrain cell spans many walk-band rooms, so a test cannot
/// reliably steer a walk to land on one specific outcome, let alone a
/// SEALED one specifically (only ~48.5% of caves, Task 3), and only
/// `session.rs`'s own tests can reach the private `delve_at` seam that
/// sidesteps needing to.
///
/// This mirrors `there_is_nothing_to_dive_into_on_dry_land` exactly: the
/// flagship's own starting cell has no cave (measured, not assumed — the
/// seed-42 fixture's cave count over land is nonzero but sparse, and the
/// starting cell is never one of them), so no walk is needed to observe this
/// outcome.
#[test]
fn there_is_no_cave_at_the_flagships_own_starting_cell() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let out = match s.handle("delve") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert!(out.contains("no cave here"), "{out}");
    let up = match s.handle("climb") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert!(
        up.contains("not underground"),
        "climb with nothing to climb out of must name that: {up}"
    );
}
