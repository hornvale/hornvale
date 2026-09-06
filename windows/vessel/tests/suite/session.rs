//! Buffer-driven verb-loop tests — the repl::run pattern.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Tableau, Turn, run};
use hornvale_worldgen::{SettlementPins, build_world};

/// Seed 42's world under default pins, read from the committed fixture rather
/// than rebuilt (decision 0607) — byte-identical to the build this replaced.
fn seam_world() -> World {
    hornvale_worldgen::seed_42_world()
}

fn opts() -> PossessOpts {
    PossessOpts {
        day: WorldTime::GENESIS,
        echo: false,
        wild_agents: true,
        eyes: hornvale_vessel::eyes::Eyes::Own,
        lens: hornvale_vessel::lens::Lens::Off,
        target: hornvale_vessel::PossessTarget::Flagship,
        tableau: None,
    }
}

/// A bearing `look` text does NOT refuse, read without depending on the
/// (now usually silent) exits clause — The Ken, spec §4.3.
///
/// Ordinary ground prints no exits clause at all any more; that silence IS
/// the room offering all eight bearings (see the unreachability argument at
/// the `closing` construction site in `session.rs`), so this only needs to
/// consult a refusal clause when the room prints one — the rare cube-corner
/// case — and otherwise any of the eight is fine. Several tests in this file
/// used to parse a direction off "the nearest ground lies ..." or a bare
/// "Ways on: ..." token list; that text is gone from ordinary ground now, so
/// they share this instead.
fn an_open_bearing(look: &str) -> String {
    let refused: Vec<String> = look
        .lines()
        .find(|l| l.contains("Every direction here is open but"))
        .map(|l| {
            l.trim_start_matches("Every direction here is open but ")
                .split(';')
                .next()
                .unwrap_or("")
                .split(", ")
                .map(|w| w.trim().to_lowercase())
                .filter(|w| !w.is_empty())
                .collect()
        })
        .unwrap_or_default();
    ["n", "ne", "e", "se", "s", "sw", "w", "nw"]
        .iter()
        .map(|w| w.to_string())
        .find(|w| !refused.contains(w))
        .expect("a room cannot refuse all eight bearings")
}

#[test]
fn possession_opens_with_a_focalized_description() {
    let world = seam_world();
    let (_s, opening) = Session::start(&world, &opts()).unwrap();
    assert!(opening.contains("in the lands of"));
    assert!(
        opening.contains("[room "),
        "the opening carries a room header"
    );
}

#[test]
fn go_moves_and_back_retraces() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let home = s.position();
    // find a real direction from the current room's ways-on
    let ways = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    let dir = an_open_bearing(&ways);
    match s.handle(&format!("go {dir}")) {
        Turn::Out(t) => assert!(t.contains("[room ")),
        _ => panic!("go must not release"),
    }
    assert_ne!(s.position(), home, "go moved");
    s.handle("back");
    assert_eq!(s.position(), home, "back retraces");
}

/// Spec §4, Task 6 (The Prospect): a facet holding a site gains a clause
/// naming it in the walk-band prose; a facet with no site says NOTHING.
/// Silence is honest — it makes the density gap visible instead of papering
/// over it, and most facets are expected to stay silent after this campaign.
///
/// **Deliberately not asserted on "Doaba".** The plan's own Step 1 test (and
/// this task's dispatch) checked `flagship.text().contains("Doaba")`, but
/// "in the lands of Doaba" is ALREADY in the flagship's prose before this
/// task — it comes from `Vantage::village` (the possession's own home
/// settlement), a wholly different mechanism from `Site`. A `Site`'s own
/// `name` stays `None` for every kind until a later task attaches one (see
/// `windows/vessel/src/site.rs`'s doc on `Site::name`: caves and exotic
/// sites never carry one, and a settlement's is wired in Task 7), so a
/// "Doaba" check would pass whether or not this task's clause exists at all
/// — exactly the vacuous-test shape this campaign has repeatedly caught
/// (progress ledger, Ruling B / Task 3). Asserted on the clause's own marker
/// text instead, which the empty facet must never emit.
#[test]
fn a_sited_locale_names_it_and_an_empty_one_stays_silent() {
    let world = seam_world();
    let (mut s, opening) = Session::start(&world, &opts()).unwrap();
    // `Session::start` mints the flagship inside its own settlement's built
    // territory (`entering_leaves_the_walk_band_position_alone` already
    // relies on this), so the walk-band opening must announce a site.
    assert!(
        opening.contains("You can enter"),
        "a built facet must announce its site: {opening:?}"
    );

    // Three steps east of the flagship — ~3.4 km at this walk resolution —
    // is the spec's own §1 example of a facet holding nothing (`enter`
    // refuses there: "There is nothing here to enter."). Verified live
    // rather than merely assumed: `enter`'s own gate (`Session::enter`) reads
    // the SAME `brief_here().site` this test's assertion below depends on, so
    // the refusal is direct evidence the facet is siteless of every kind, not
    // only unsettled.
    for _ in 0..3 {
        match s.handle("go e") {
            Turn::Out(_) => {}
            _ => panic!("go must not release"),
        }
    }
    let enter = match s.handle("enter") {
        Turn::Out(t) => t,
        _ => panic!("enter must not release"),
    };
    assert!(
        enter.contains("nothing here to enter"),
        "fixture assumption failed: three tiles east must hold no site: {enter:?}"
    );
    let here = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    assert!(
        !here.contains("You can enter"),
        "a siteless facet must stay silent about entering: {here:?}"
    );
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
    let before = s.position();
    let out = match s.handle("exit") {
        Turn::Out(t) => t,
        _ => panic!("exit must not release"),
    };
    assert!(out.contains("grain of the world"), "diegetic refusal");
    assert_eq!(s.position(), before, "no movement");
}

/// Descending must never move the WALK-band position: the band change lives in
/// session state, so `enter` leaves `agent().position` exactly where it was.
/// That is what keeps `map`, `whoami`, `purview` and the NPC layer — all of
/// which read that field — unchanged by being indoors.
#[test]
fn entering_leaves_the_walk_band_position_alone() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let before = s.position();
    let reply = match s.handle("enter") {
        Turn::Out(t) => t,
        _ => panic!("enter must not release"),
    };
    // Without this, the test passes whether `enter` descended or was
    // refused — and position-invariance is trivially true in the refusal
    // case too. `Session::start` mints the flagship in its own settlement,
    // whose locale IS settlement territory, so a refusal here is a real
    // failure and not a geography accident. Asserted as a POSITIVE signal
    // (the chamber's own "Ways on" signature), not as the absence of a
    // refusal string: a negative assertion against wording survives that
    // wording changing out from under it and silently stops guarding
    // anything (The Prospect, Ruling 1).
    assert!(
        reply.contains("Ways on"),
        "the flagship's own locale is built, so this must actually descend: {reply:?}"
    );
    assert_eq!(
        s.position(),
        before,
        "the possession's walk-band position is untouched by descent"
    );
    s.handle("out");
    assert_eq!(s.position(), before);
}

#[test]
fn examine_honors_the_contract_and_release_ends() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let f = s.focalized().unwrap();
    for n in &f.nouns {
        match s.handle(&format!("examine {}", n.display)) {
            Turn::Out(t) => assert_eq!(&t, &n.datum, "examine renders the datum"),
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
    // observation day still advances, and the possessed agent itself still
    // never moves — only the session's owned NPC ledger evolves.
    //
    // **Re-pointed by The Ken's Task 3.** The day used to be readable straight
    // off the room header ("day 0", then "day 90" after the wait); Task 3
    // removed the day from the header entirely, so this now reads `s.day()`
    // directly — the render-independent accessor, not a re-pinned prose
    // phrase that would go vacuous the moment the wording moved again.
    let world = seam_world();
    let (mut s, _opening) = Session::start(&world, &opts()).unwrap();
    assert_eq!(s.day(), WorldTime::GENESIS, "the possession opens at day 0");
    let home = s.position().pack().unwrap().0;
    let out = match s.handle("wait 90") {
        Turn::Out(t) => t,
        _ => panic!("wait must not release"),
    };
    assert!(!out.is_empty(), "wait narrates what happened");
    match s.handle("look") {
        Turn::Out(_) => {}
        _ => panic!("look must not release"),
    }
    assert_eq!(s.day().as_std_days(), 90.0, "the observation day moved");
    assert_eq!(
        s.position().pack().unwrap().0,
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
    let dir = an_open_bearing(&ways);
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
            day: WorldTime::GENESIS,
            echo: false,
            wild_agents: true,
            eyes: hornvale_vessel::eyes::Eyes::Own,
            lens: hornvale_vessel::lens::Lens::Off,
            target: hornvale_vessel::PossessTarget::Flagship,
            tableau: None,
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
        // Pao. The Tense (2026-08-05) re-mints it once more, and it lands back
        // where it already was two renames ago: Pao -> Xoaboa. The Burr
        // (Task 4): admitting an alveolar trill as an ordinary manner
        // reseeds every family's root assignment (`ROOT_EPOCH` v3 -> v4,
        // a decision recorded at this campaign's close): Xoaboa -> Booko, landing back on The Wearing's
        // own spelling by coincidence of the draw. Moon count ("two"),
        // subject and sentence frame unchanged at every step.
        after.contains("Booko has two moons, as the initiated count."),
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
            day: WorldTime::GENESIS,
            echo: false,
            wild_agents: true,
            eyes: hornvale_vessel::eyes::Eyes::Own,
            lens: hornvale_vessel::lens::Lens::Off,
            target: hornvale_vessel::PossessTarget::Flagship,
            tableau: None,
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
        // See the primary arm's re-pin note above (The Burr, Task 4):
        // Xoaboa -> Booko.
        consulted.contains("Booko has two moons, as the initiated count."),
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
            day: WorldTime::GENESIS,
            echo: true,
            wild_agents: true,
            eyes: hornvale_vessel::eyes::Eyes::Own,
            lens: hornvale_vessel::lens::Lens::Off,
            target: hornvale_vessel::PossessTarget::Flagship,
            tableau: None,
        },
        std::io::Cursor::new(script),
        &mut out_a,
    )
    .unwrap();
    run(
        &world,
        PossessOpts {
            day: WorldTime::GENESIS,
            echo: true,
            wild_agents: true,
            eyes: hornvale_vessel::eyes::Eyes::Own,
            lens: hornvale_vessel::lens::Lens::Off,
            target: hornvale_vessel::PossessTarget::Flagship,
            tableau: None,
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

/// The Ken: openness is the default and a wall is news. On ordinary ground
/// — nothing refused, every bearing carrying ground — the clause said
/// "No direction here is closed; the nearest ground lies N, NE, E, SE, S,
/// SW, W, NW.", which spends twelve words twice asserting that nothing is
/// unusual. It now says nothing at all.
#[test]
fn ordinary_ground_says_nothing_about_its_exits() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let look = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    assert!(
        !look.contains("No direction here is closed"),
        "the vacuous clause survived: {look:?}"
    );
    assert!(
        !look.contains("the nearest ground lies N, NE, E, SE, S, SW, W, NW"),
        "the all-eight enumeration survived: {look:?}"
    );
}

/// Every bearing a room implies is a way out must be a command you can
/// actually type. This is the exact bug: the parser already accepted them,
/// but the verb dispatch never reached it.
///
/// **Re-pointed by The Ken.** The room used to print "the nearest ground
/// lies SE, N, SW." on every ordinary turn, and this test read the token
/// list straight off that sentence. Spec §4.3 makes the clause silent on
/// ordinary ground (nothing refused, every bearing carrying ground) — there
/// is nothing left to parse a list out of there. But "ordinary ground" is
/// exactly the row where every one of the eight compass words IS a real way
/// out (that's what makes the clause safe to omit — see the unreachability
/// argument at the `closing` construction site in `session.rs`), so the
/// same invariant is tested directly against all eight rather than against
/// a vanished list. The guard below confirms the fixture is still ordinary
/// ground and not one of the 24 cube-corner rooms, where this blanket walk
/// would be unsound.
#[test]
fn every_printed_way_out_is_a_command_you_can_type() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let look = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    assert!(
        !look.contains("Every direction here is open but"),
        "fixture assumption failed: the flagship start refuses a bearing, \
         so this test's blanket eight-word walk needs a different room: {look:?}"
    );
    for t in ["n", "ne", "e", "se", "s", "sw", "w", "nw"] {
        let out = match s.handle(t) {
            Turn::Out(o) => o,
            _ => panic!("a direction must not release"),
        };
        assert!(
            !out.contains("No verb"),
            "an ordinary-ground bearing must be typeable: '{t}' was rejected: {out}"
        );
        s.handle("back");
    }
}

/// The Ken: the header was telemetry in the character's voice —
/// "[room 3733133217, day 0]". Neither datum is lost: `!whoami` already
/// answers "A bugbear of Doaba (agent 3286669968037249024), day 0, room
/// 3733133217.", which is the author's-instrument frame and the right home
/// for both. Controller ruling (ledger #5) supersedes spec §4.2's "share a
/// derivation with the sky line": no such derivation exists to share (the
/// phase logic is baked into `sky_at`'s description string, and
/// `domains/astronomy` exposes no `daypart` accessor), so the header now
/// carries a place and nothing about time at all.
#[test]
fn the_turn_header_carries_no_facet_id_and_no_decimal_day() {
    let world = seam_world();
    let (mut s, opening) = Session::start(&world, &opts()).unwrap();
    let look = match s.handle("look") {
        Turn::Out(t) => t,
        _ => panic!("look must not release"),
    };
    for text in [opening, look] {
        let header = text.lines().next().expect("a turn opens with a header");
        assert!(
            !header.contains("3733133217"),
            "a raw facet id survived in the header: {header:?}"
        );
        assert!(
            !header.contains('.'),
            "a decimal day survived in the header: {header:?}"
        );
    }
}

/// Issue one command and unwrap the turn's text — none of this file's turns
/// releases the possession. `pub(crate)`: The Ken, Task 5 reuses this from
/// `the_roll.rs`, a sibling module in the same `suite` binary.
pub(crate) fn say(session: &mut Session<'_>, cmd: &str) -> String {
    match session.handle(cmd) {
        Turn::Out(t) | Turn::Released(t) => t,
    }
}

/// Every noun a `"Here: ..."` line names, in prose order: split on the
/// group separator and strip the line's own leading label and trailing
/// period. Test-local — production code never parses its own prose back.
fn presence_nouns(here: &str) -> Vec<String> {
    here.trim_start_matches("Here: ")
        .trim_end_matches('.')
        .split("; ")
        .map(str::to_string)
        .collect()
}

/// The staged three-dragon cast that reproduces #12 deterministically —
/// seed 42, `{"cast":[{"species":"white-dragon"},{"species":"black-dragon"},
/// {"species":"red-dragon"}]}` — committed here (The Ken, Task 4) because
/// Task 5 depends on three same-suffix labels sharing one room, which is
/// otherwise a search rather than a stipulation.
///
/// Leaks the world to get a `'static` session out of a zero-argument helper
/// — acceptable in test code, and the shape every caller of this function
/// wants: `let (mut session, _) = open_staged_dragons_session();` with
/// nothing to keep alive. `pub(crate)`: The Ken, Task 5 reuses this from
/// `the_roll.rs`, a sibling module in the same `suite` binary.
pub(crate) fn open_staged_dragons_session() -> (Session<'static>, String) {
    let world: &'static World = Box::leak(Box::new(seam_world()));
    let staged = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["white-dragon", "black-dragon", "red-dragon"])),
        ..opts()
    };
    Session::start(world, &staged).expect("a staged session starts")
}

/// The Ken: the game displayed a noun and then denied it existed.
/// `presence_line` rendered a wild group from `species` while `examine`
/// matched `label`, so the staged tableau reproducing #12 printed "Here: a
/// wild black-dragon; a wild red-dragon." and then answered "You see no a
/// wild black-dragon here." — the repo's own §6 contract ("every depicted
/// noun must answer") failing on the one roster that escaped it.
#[test]
fn every_noun_the_presence_line_shows_can_be_examined() {
    let (mut session, _) = open_staged_dragons_session();
    let look = say(&mut session, "look");
    let here = look
        .lines()
        .find(|l| l.starts_with("Here: "))
        .expect("the staged cast is present");

    for noun in presence_nouns(here) {
        let answer = say(&mut session, &format!("examine {noun}"));
        assert!(
            !answer.starts_with("You see no"),
            "the presence line showed {noun:?} and examine denied it: {answer:?}"
        );
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
///
/// **Re-pointed by The Ken.** This used to follow "whatever ground this room
/// actually names" by parsing the (now-silent-on-ordinary-ground) exits
/// clause — spec §4.3 — which left `dir` permanently `None` and the walker
/// standing still for the whole loop: it still happened to pass, because the
/// sky varies with elapsed time too, but it was no longer testing what its
/// own name claims. A fixed south-westward bias (matching
/// `the_water_column_is_a_place_you_can_be`'s own technique) makes real
/// progress instead — every bearing is a real way out on ordinary ground —
/// and `rooms.len() > 1` is the direct check that the walker actually moved,
/// so this cannot go quietly vacuous the same way twice.
///
/// **Re-pointed again by The Ken's Task 3.** The header used to carry the
/// room id, which is how `rooms` proved movement; Task 3 removed the id (and
/// the day) from the header entirely, so parsing it here would make `rooms`
/// a set of one forever regardless of whether the walker moved — a second
/// silent vacuity of exactly the shape this test's own doc comment already
/// warns about. `s.position()` is the room identity itself, not a rendering
/// of it, so it stays live no matter what the header prints.
#[test]
fn the_sky_follows_the_walker() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let mut skies = std::collections::BTreeSet::new();
    let mut rooms = std::collections::BTreeSet::new();
    for dir in std::iter::repeat(["w", "nw", "sw"]).flatten().take(40) {
        let out = match s.handle("look") {
            Turn::Out(t) => t,
            _ => panic!("look must not release"),
        };
        if let Some(l) = out.lines().find(|l| l.contains("The sky is")) {
            skies.insert(l.to_string());
        }
        rooms.insert(s.position());
        s.handle(dir);
        s.handle("wait 3");
    }
    assert!(rooms.len() > 1, "the walker never moved: {rooms:?}");
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
            .find(|l| l.contains("the nearest ground lies"))
            .and_then(|l| l.split("lies ").nth(1))
            .and_then(|l| l.split(',').next())
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
    //
    // The loop stops when `dive` SUCCEEDS, not when `look` merely mentions open
    // water, and The Tense is what showed the difference matters: the walker
    // reached a look containing "Open water" while standing somewhere `dive`
    // answered "There is no water here to go down into." The old condition was
    // a proxy for the precondition rather than the precondition, so the test
    // dived from dry land and read the failure as a column. Ask the verb.
    //
    // The budget is 3000 (was 600) because seed 42's re-placement seats the
    // possession much further inland — water is first reachable around
    // iteration 2400, measured.
    let mut afloat = String::new();
    for _ in 0..3000 {
        for d in ["w", "nw", "sw"] {
            s.handle(d);
        }
        let Turn::Out(look) = s.handle("look") else {
            continue;
        };
        if !look.contains("Open water") {
            continue;
        }
        if let Turn::Out(probe) = s.handle("dive")
            && !probe.contains("no water here")
        {
            s.handle("surface");
            afloat = look;
            break;
        }
    }
    assert!(
        !afloat.is_empty(),
        "the walker never reached a divable water column; it cannot be tested"
    );

    // On the surface: afloat on open water, not standing in the floor's biome.
    assert!(afloat.contains("Open water —"), "{afloat}");

    // A direction this room ACTUALLY offers. Hardcoding `n` was wrong and The
    // Tense exposed it: the exit check runs BEFORE the submersion rule, so on
    // a room that refuses `n` the reply is the corner refusal and the
    // lateral-refusal claim below is never reached — a green run on a
    // refusal it was not testing for, worse than a red one.
    //
    // **Re-pointed by The Ken (spec §4.3).** The technique used to read the
    // direction off "the nearest ground lies ..."; that clause is silent on
    // ordinary ground now, and open water is ordinary ground (unaffected by
    // Task 2 — only the exits clause's own conditionality changed, not what
    // `go` accepts). `an_open_bearing` is the shared fallback: it consults a
    // refusal clause when the room prints one (the rare cube-corner case The
    // Tense's own worry was about), and otherwise any of the eight is fine.
    let lateral_dir = an_open_bearing(&afloat);

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

    // F1: the submerged render must not claim a direction is open when `go`
    // refuses every lateral direction there (`SUBMERGED_LATERAL_REFUSAL`,
    // asserted two lines below). Decision 0141's "the listing now says what
    // is true" must hold in this band too, not only out of doors.
    assert!(
        !under.contains("No direction here is closed"),
        "the submerged render claims no direction is closed, but go refuses \
         all of them here: {under}"
    );

    // Lateral movement is refused while under, and says so diegetically.
    let lateral = match s.handle(&lateral_dir) {
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

/// The Deep Realm, Task 5: at a vertex with no cave, `delve` refuses and names
/// the absence — the first of the outcomes `dive`'s own doc warns a descent
/// verb must distinguish. The others are exercised in
/// `windows/vessel/src/session.rs`'s own internal tests
/// (`delve_has_three_distinguishable_outcomes`), which need a hand-picked
/// cave vertex — a terrain vertex spans many walk-band rooms, so a test
/// cannot reliably steer a walk to land on one specific outcome, and only
/// `session.rs`'s own tests can reach the private `delve_at` seam that
/// sidesteps needing to.
///
/// **There were THREE, then TWO, and there are THREE again — through a
/// different door.** The original third outcome was a cave whose entrance
/// address resolved to no chamber — SEALED — because a 0.5 per-address
/// existence coin refused roughly 48.5% of cave entrances. Spec §4.1 (The
/// Drift) deleted that coin, so that specific outcome is impossible rather
/// than rare: 0 of 48,316 caves over thirty worlds, still true today. The
/// Latch restored a third outcome by a different mechanism instead —
/// `delve_at` now gates on the address's seeded `BarrierState` before it
/// ever reaches the chamber lookup, and seed 42's terrain barred 639 of 874
/// cave mouths (`windows/vessel/tests/suite/passage.rs`). The internal test
/// was renamed once for losing an outcome and again for regaining one
/// through a different door; see its own doc comment for the full account.
///
/// This mirrored `there_is_nothing_to_dive_into_on_dry_land`: the flagship's
/// own starting vertex had no cave, so no walk was needed to observe the
/// refusal.
///
/// **THE SUBJECT MOVED, NOT THE VERB** (decision 0134, 2026-08-14). That was
/// always a measured contingency about one vertex, and the terrain epoch's new
/// coastlines put a cave under it — a SEALED one, whose entrance resolves to
/// no chamber. So the public path here briefly exercised the
/// chamber-unrealized refusal instead of the *no-cave* refusal. Both are
/// refusals that name what stopped you, which is the property this test
/// exists to hold through the public verb; which one the flagship's own
/// ground happens to produce is a fact about seed 42's karst, not about
/// `delve`.
///
/// The no-cave branch did not lose coverage: `delve_has_three_distinguishable_outcomes`
/// reaches it directly through `delve_column(None)` rather than by
/// standing somewhere that happens to qualify, so it can no longer be
/// falsified by a coastline moving.
///
/// **THE SUBJECT MOVED AGAIN** (The Glasshouse, Stage B Task 4, the
/// thermostat). The damped, greenhouse-forced insolation baseline
/// re-places seed 42's settlements a second time this campaign, and the new
/// flagship's own starting vertex has no cave at all — back to the *no-cave*
/// refusal, the same contingency this comment already names. Still a
/// refusal that names what stopped you, which is the property this test
/// holds regardless of which refusal fires — and since The Latch, delve at
/// this same public seam could in principle also land on a barred vertex; it
/// has not, at this seed and this starting position, so this test still
/// pins whichever of no-cave / chamber-unrealized it observes.
#[test]
fn the_flagships_own_starting_vertex_refuses_a_delve_and_names_why() {
    let world = seam_world();
    let (mut s, _) = Session::start(&world, &opts()).unwrap();
    let out = match s.handle("delve") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert!(out.contains("no cave"), "{out}");
    let up = match s.handle("climb") {
        Turn::Out(t) => t,
        _ => panic!("must not release"),
    };
    assert!(
        up.contains("not underground"),
        "climb with nothing to climb out of must name that: {up}"
    );
}

/// Bare `eyes` names whose eyes the chart is coloured through and the arity
/// of what they see (The Beholding, Task 5).
#[test]
fn the_eyes_verb_reports_whose_eyes_and_what_the_projection_drops() {
    let w = seam_world();
    let (mut s, _) = Session::start(&w, &opts()).unwrap();
    let out = match s.handle("!eyes") {
        Turn::Out(t) => t,
        Turn::Released(_) => panic!("!eyes must not release"),
    };
    let species = s.driven_body().species.clone();
    assert!(
        out.contains(&species),
        "the report must name whose eyes: {out}"
    );
    assert!(out.contains("channel"), "and the arity: {out}");
}

/// `eyes <name>` switches whose eyes colour the chart, and an unknown name
/// refuses loudly rather than guessing — naming what was asked for and
/// listing the roster (The Beholding, Task 5).
#[test]
fn eyes_switches_the_chart_and_an_unknown_name_lists_the_roster() {
    let w = seam_world();
    let (mut s, _) = Session::start(&w, &opts()).unwrap();
    let before = s.purview(0).unwrap();
    s.handle("!eyes kobold");
    let after = s.purview(0).unwrap();
    if s.driven_body().species != "kobold" {
        assert_ne!(
            before.cells.iter().map(|c| c.color).collect::<Vec<_>>(),
            after.cells.iter().map(|c| c.color).collect::<Vec<_>>(),
            "switching eyes must change the chart"
        );
    }
    let refusal = match s.handle("!eyes wyvern") {
        Turn::Out(t) => t,
        Turn::Released(_) => panic!("!eyes must not release"),
    };
    assert!(
        refusal.contains("wyvern"),
        "name what was refused: {refusal}"
    );
    assert!(
        refusal.contains("bugbear"),
        "and list the roster: {refusal}"
    );
}

/// `map` draws the colour lens by default (Task 4's headline claim: a
/// possession sees as its own kind does, not a human narrator) and falls all
/// the way back to the plain terrain lens — no colour, no escape sequence —
/// when the eyes are declined (The Beholding, Task 5).
#[test]
fn map_renders_the_colour_lens_unless_the_eyes_are_off() {
    let w = seam_world();
    let (mut s, _) = Session::start(&w, &opts()).unwrap();
    let lit = match s.handle("map") {
        Turn::Out(t) => t,
        Turn::Released(_) => panic!("map must not release"),
    };
    assert!(
        lit.contains("[lens: colour"),
        "possession draws the colour lens: {lit}"
    );
    s.handle("!eyes off");
    let bare = match s.handle("map") {
        Turn::Out(t) => t,
        Turn::Released(_) => panic!("map must not release"),
    };
    assert!(
        bare.contains("[lens: terrain"),
        "eyes off falls back to terrain: {bare}"
    );
    assert!(!bare.contains('\u{1b}'), "and emits no escape sequences");
}

/// **A predicate's registered DOC is save-format state, and this pins that
/// `Session::start` registers The Chattel's two from the shared constants.**
///
/// `PredicateDef` derives `PartialEq` over `{name, functional, doc}` and
/// `ConceptRegistry::register_predicate` is idempotent only for an IDENTICAL
/// definition, so two registrations of `located-in` differing by one word are
/// a `ConflictingDefinition` — which `Session::start` meets behind an
/// `.expect`, i.e. as a panic. A world saved by `possess --out` carries its
/// registry (decision 0368), so the two registrations that must agree are not
/// even in one process: they are a saved world's and a later session's.
///
/// Task 5 shipped exactly that divergence inside one file — `session.rs`
/// registered "…: a room, a container, or a hand" while `thing.rs`'s test
/// helper registered "where a thing is on a day" — and nothing red, because
/// the two registries never met. The constants remove the possibility; this
/// test is the half that keeps `session.rs` reaching for them, and it reds
/// against a literal spelled here.
#[test]
fn a_sessions_registry_carries_the_shared_predicate_docs() {
    use hornvale_vessel::thing::{
        LOCATED_IN, LOCATED_IN_DOC, LOCKEDNESS, LOCKEDNESS_DOC, OPENNESS, OPENNESS_DOC,
    };

    let world = seam_world();
    let (s, _) = Session::start(&world, &opts()).unwrap();
    let played = s.into_played_world(Seed(42));

    for (name, doc) in [
        (LOCATED_IN, LOCATED_IN_DOC),
        (OPENNESS, OPENNESS_DOC),
        (LOCKEDNESS, LOCKEDNESS_DOC),
    ] {
        let def = played
            .registry
            .predicate(name)
            .unwrap_or_else(|| panic!("`{name}` is registered per-session"));
        assert_eq!(
            def.doc, doc,
            "`{name}`'s registered doc must BE the shared constant, not a \
             literal that happens to match it today: a divergence is a \
             ConflictingDefinition against a world an earlier session saved"
        );
    }
}

/// **A saved played world carries custody** — spec §6 acceptance 1's second
/// half, and the cheapest real work The Latch left behind (The Chattel, Task
/// 12).
///
/// It drives the ACTUAL round trip and not a sibling predicate: a real
/// possession walks into seed 1's store chamber, `take`s the key through
/// `Session::handle`, folds itself into a `World` with `into_played_world`
/// (which `possess --out` writes verbatim — decision 0368 carries the
/// per-session registry out with the evolved ledger, and 0171 rules a
/// player's acts are not filtered on the way), and a SECOND session started
/// over that world is asked `carrying` through the same verb loop.
///
/// # The one thing the save does not carry, and it is not the custody
///
/// **The session CLOCK is not persisted, and a naive re-possession therefore
/// reports empty hands.** `World` is `{seed, registry, ledger, derived_under}`
/// and holds no instant; `PossessOpts::default()` starts every possession at
/// noon of day 0. Custody is an as-of-day fold
/// (`thing::location_of` keeps the latest posting at or before the day
/// asked about), so a take committed on day 0.55 is invisible to a session
/// whose own now is 0.5 — the postings are in the file, correctly, and the
/// question was asked at the wrong instant. Measured, not inferred: the first
/// draft of this test asserted against `PossessOpts::default()` and read
/// `You are carrying nothing.` with the two `located-in` facts sitting in the
/// saved ledger.
///
/// So the re-possession names the day, which is exactly what `possess
/// --world <saved> --day D` already exposes on the command line. That is the
/// honest round trip rather than a workaround: the saved world is the durable
/// half and the instant is the reader's, the same way an almanac is rendered
/// *at* a day.
///
/// MUTATION THIS MUST FAIL AGAINST — the property is *that custody crosses
/// the save*, not that a live session can read its own ledger: make
/// `Session::into_played_world` filter the outgoing ledger to facts whose
/// predicate is not `thing::LOCATED_IN`. Every in-session assertion in the
/// campaign stays green and only this one reddens — `860 tests run: 857
/// passed, 3 failed`, the other two belonging to two unrelated mutations
/// carried in the same run. `Ledger` has no `retain`, so the filter is a
/// rebuild: a fresh `Ledger::default()` re-committing every fact whose
/// predicate is not `LOCATED_IN`. Confirmed 2026-08-29, unfiltered over the
/// whole crate:
///
/// ```text
/// assertion `left == right` failed: custody must survive `possess --out`
///   left: "You are carrying nothing."
///  right: "You are carrying a key."
/// ```
#[test]
fn custody_survives_a_save_and_a_re_possession() {
    let world = build_world(
        Seed(1),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 1 builds");

    let say = |s: &mut Session<'_>, line: &str| match s.handle(line) {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("`{line}` must not release: {t}"),
    };

    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 1 possesses");
    assert!(
        say(&mut session, "enter").starts_with("[chamber "),
        "the possession never got indoors, so nothing below is tested"
    );
    // The LOOMROOM's key, not the storeroom's: since Task 13's fix round the
    // storeroom key is inside a shut, locked chest and `take` no longer
    // reaches through a lid. This walks in to chamber index 2, takes the key
    // `the-key-by-the-loom` composes, and then carries it as far into the
    // building as the place goes — so the save below is taken with the key in
    // hand in a room it was not picked up in, which is a stronger starting
    // state for the round trip than the old one, not a weaker.
    //
    // It was ONE `enter` until The Custodian, which moved the key pattern off
    // `Role::Threshold` — the role every built structure has — so that a key
    // stopped standing in every dwelling's front room. Seed 1's flagship is
    // agrarian, so its index-2 chamber is the loomroom the key now stands in.
    for _ in 0..2 {
        assert!(
            say(&mut session, "enter further in").starts_with("[chamber "),
            "seed 1's structure no longer reaches the loomroom, so nothing \
             below is tested"
        );
    }
    assert_eq!(
        say(&mut session, "take a key"),
        "You take the key.",
        "precondition: seed 1's loomroom must hold a takeable key"
    );
    for _ in 0..4 {
        if !say(&mut session, "enter further in").starts_with("[chamber ") {
            break;
        }
    }
    assert_eq!(say(&mut session, "carrying"), "You are carrying a key.");

    let saved_at = session.day();
    let played = session.into_played_world(Seed(1));

    // The world really did travel as a FILE, not as a struct handed across:
    // `possess --out` writes JSON and `possess --world` reads it, and a
    // `located-in` object is a `Value::Entity`, which is the shape most likely
    // to be lost by a serde round trip.
    let on_disk = serde_json::to_string(&played).expect("a world always serializes");
    let reloaded: World = serde_json::from_str(&on_disk).expect("and always reloads");

    let (mut again, _) = Session::start(
        &reloaded,
        &PossessOpts {
            day: saved_at,
            ..PossessOpts::default()
        },
    )
    .expect("the saved world possesses again");
    assert_eq!(
        say(&mut again, "carrying"),
        "You are carrying a key.",
        "custody must survive `possess --out`"
    );
}
