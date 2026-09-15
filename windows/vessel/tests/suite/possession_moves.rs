//! The world moves without you: possess, wait across a phase, observe an NPC's
//! motion; and the same script is byte-deterministic.
use hornvale_vessel::{PossessOpts, Session, Turn};

/// Did THIS NPC commit a positional (`agent-at`) fact? Read from its own
/// `why` recount, the per-NPC provenance channel.
///
/// The three stay-put claims below used to read `Session::
/// committed_agent_at_count() == 0` instead. That accessor sums the whole
/// derived roster (the flagship settlement plus its two most populous
/// neighbours), so it can only stand in for "the NPC under test stayed put"
/// while EVERY derived NPC happens to sit on fresh water. The Tumult's
/// predation epoch ended that coincidence: at seed 42 the flagship
/// (`Doododoobodobaado`, rendered `Qvooshtvoagootao` before The Wearing) is
/// still on water and still drinks in place, but the neighbour then rendered
/// `Vootkeonoagootoaneo` was reseated off the river and its NPC now
/// wanders 18 steps looking for water. The proxy went nonzero while every
/// claim these tests actually make stayed true — so the proxy is narrowed to
/// the claim rather than re-pinned to 18, which would have frozen an
/// unrelated neighbour's water access into three tests that say nothing
/// about it.
fn walked(session: &mut Session, label: &str) -> bool {
    // `--steps` since The Warrant, Task 5: the default `!why` now ROLLS an
    // errand and the steps under it into one line, so the position
    // predicate's own doc no longer appears there. `--steps` is the same
    // recount this witness has always read — one line per committed
    // positional fact — and reading it keeps the witness the per-NPC
    // recount rather than swapping in a session-wide count, which is the
    // proxy this helper's own callers record narrowing away from.
    match session.handle(&format!("!why {label} --steps")) {
        Turn::Out(s) => s.contains("position on a day"),
        Turn::Released(_) => panic!("why never releases"),
    }
}

/// The first derived NPC that committed **no** positional fact across the wait
/// just taken — the one whose dated fact is a `drank`, and the subject the
/// stay-put narration claims below are about.
///
/// **Chosen by the property, not by roster position, and that is The Winze
/// T2b's correction.** Three tests here took `npc_labels().first()` and
/// asserted it stayed put, on a comment that read "the flagship's own, always
/// first in the derived roster … seed 42's flagship settlement structurally
/// cannot produce a departure any more". Spec amendment E's working ring scan
/// re-placed every settlement and the flagship landed off the river: it now
/// walks, the roster is two NPCs rather than three, and all three tests went
/// red at once on a premise none of them was actually about. Selecting on the
/// property means the next placement epoch moves the witness rather than the
/// test.
fn stay_put_npc(session: &mut Session, labels: &[String]) -> String {
    labels
        .iter()
        .find(|l| !walked(session, l))
        .unwrap_or_else(|| {
            panic!(
                "no derived NPC of {labels:?} drank in place — every one committed a \
                 positional fact, so the stay-put narration claims below have no \
                 subject and would hold vacuously"
            )
        })
        .clone()
}

fn world() -> hornvale_kernel::World {
    world_at(42)
}

fn world_at(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// A seed whose flagship roll actually WALKS — at least one resident commits
/// an `agent-at` across a seven-day wait — and which also drinks.
///
/// **New at The Roll (Task 7), and it exists because the roster changed
/// MEANING, not because the drive layer moved.** Before the roll a session
/// derived one body per each of three settlements plus four world-top wild
/// beasts, so seed 42's roster always contained somebody off the river: the
/// variable the caller below calls `flagship` was in fact a NEIGHBOURING
/// settlement's NPC, and that is the body that walked. The roll derives the
/// residents of the settlement you are standing IN, and seed 42's flagship
/// stands on a river (`"water":"river"` in the committed walk snapshot), so
/// all sixty-seven of its residents drink in place and the session commits
/// ZERO positional facts, ever — measured, not inferred.
///
/// Measured rather than searched at run time, the same trade
/// `session.rs`'s `CHAMBERED_SEED` records: seeds 0..16 each possessed and
/// waited seven days, reading `committed_agent_at_count`/
/// `committed_drank_count`. Eleven of the sixteen walk, so the property is
/// ordinary and it is seed 42 that is special. Seed 14 is the cheapest of
/// them (59 bodies, 673 positional facts) and its residents never reach water
/// at all, which is the stronger property `the_roll.rs`'s dormancy test needs
/// — that file pins the same number for that reason. The precondition below
/// fails loudly if an epoch moves it.
/// type-audit: bare-ok(index)
const WALKING_SEED: u64 = 14;

/// A seed whose flagship roll actually holds WILD bodies — a herd attractor
/// within `ROLL_HOPS` of the settlement you possess.
///
/// **Also new at The Roll (Task 7), and for the sibling reason.** The roll
/// derives the fauna standing within call rather than the world's top four
/// concentrations, so a wild body is no longer guaranteed: seed 42's flagship
/// has no herd attractor within two hops (the nearest is ~0.0197 of a unit
/// sphere away, ~125 km, and the world's 12,657 herd entries occupy 917
/// distinct walk-band rooms out of a depth-13 mesh's hundreds of millions).
/// It is not rare in general — so this is once again a fact about a
/// particular seed rather than about the feature.
///
/// **Moved 3 -> 9 at The Tidemark, and the evidence is now a committed probe
/// rather than a cited number.** That campaign's vent-expiry ending shifts
/// placement (`maybe_vent_failure` refounds, which mints ids and shifts every
/// subsequent community's identity), and seed 3's flagship stopped holding
/// wild bodies: `labels == peopled == 62`, which is the vacuity this test's
/// first assertion exists to catch. Re-seeding to keep a test green is only
/// legitimate when the FEATURE is intact and the seed is unlucky, so that was
/// measured rather than assumed — see [`wild_body_rate_over_seeds`]:
///
/// ```text
///   derive wild bodies: 11/24 -> [1, 6, 7, 8, 9, 10, 13, 16, 17, 21, 23]
///   cheapest: seed 9 (38 bodies, 16 of them wild)
/// ```
///
/// The Roll recorded 13/24 for the same sweep. **Do not read 13 -> 11 as this
/// campaign's cost**: many campaigns landed between the two measurements, and
/// nothing here ablates them apart. The claim this note makes is only the one
/// the number supports — the feature is plainly not broken, so the seed was
/// unlucky. An ablation against `cfg.vent_tenancy` is the instrument that
/// would settle attribution, and it was not run.
/// type-audit: bare-ok(index)
const WILD_SEED: u64 = 9;

/// The measurement `WILD_SEED`'s note cites, made reproducible.
///
/// The Roll (Task 7) recorded "13 of seeds 0..23 do derive wild bodies" as the
/// evidence that a seed without wild bodies is a fact about that seed and not
/// about the feature — but committed no probe, so the next campaign to move
/// `WILD_SEED` had to take the claim on trust or rebuild the sweep from
/// scratch. The Tidemark had to move it (its vent-expiry ending shifts
/// placement, and seed 3's flagship stopped holding wild bodies), so the sweep
/// is committed here rather than cited.
///
/// **Read it before re-seeding this file.** Re-picking a seed to make a test
/// pass is only legitimate when the FEATURE is intact and the seed is unlucky,
/// and that is exactly what the rate over 0..23 answers. A rate near The Roll's
/// 13/24 says the feature holds; a collapsed rate says something suppressed
/// wild bodies and a new seed would be hiding it.
///
/// claim: rate(forall-seed 0..24, >0 derive wild bodies) — the floor is the
/// only thing asserted, because the point is to justify a seed choice rather
/// than to pin a rate: a band would make this a gate on a quantity every
/// placement-touching campaign legitimately moves. The observed 11/24 is
/// printed, not asserted.
///
/// `#[ignore]`: builds 24 worlds at roughly 12 s each (~5 min). Run it with
/// `cargo nextest run -p hornvale-vessel --test suite -E 'test(wild_body_rate_over_seeds)' --run-ignored all --no-capture`
#[test]
#[ignore = "cost: builds 24 worlds (~5 min); a measurement, not a gate"]
fn wild_body_rate_over_seeds() {
    let mut with_wild = Vec::new();
    let mut without = Vec::new();
    println!("\n  seed  labels  peopled  wild  cheapest-metric(bodies)");
    for seed in 0..24u64 {
        let w = world_at(seed);
        let Ok((wild_session, _)) = Session::start(&w, &PossessOpts::default()) else {
            println!("  {seed:>4}  (no session)");
            continue;
        };
        let peopled = {
            let opts = PossessOpts {
                wild_agents: false,
                ..PossessOpts::default()
            };
            match Session::start(&w, &opts) {
                Ok((s, _)) => s.npc_labels().len(),
                Err(_) => continue,
            }
        };
        let labels = wild_session.npc_labels().len();
        let bodies = wild_session.bodies().len();
        let wild = wild_session
            .bodies()
            .iter()
            .filter(|b| b.village.is_none())
            .count();
        println!("  {seed:>4}  {labels:>6}  {peopled:>7}  {wild:>4}  {bodies:>6}");
        if labels > peopled && wild > 0 {
            with_wild.push((seed, bodies, wild));
        } else {
            without.push(seed);
        }
    }
    println!(
        "\n  derive wild bodies: {}/24 -> {:?}",
        with_wild.len(),
        with_wild.iter().map(|(s, _, _)| *s).collect::<Vec<_>>()
    );
    println!("  do not: {without:?}");
    if let Some((seed, bodies, wild)) = with_wild.iter().min_by_key(|(_, b, _)| *b) {
        println!("  cheapest: seed {seed} ({bodies} bodies, {wild} of them wild)");
    }
    assert!(
        !with_wild.is_empty(),
        "no seed in 0..24 derives wild bodies -- the FEATURE is broken, and \
         re-seeding `WILD_SEED` would hide it"
    );
}

#[test]
fn day_zero_session_is_unchanged_until_you_wait() {
    // The frozen behavior is preserved: before any `wait`, no agent-at exists.
    let w = world();
    let (session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    assert_eq!(session.committed_agent_at_count(), 0); // a test accessor added in this task
}

/// claim: structural(seed: none — world() fixture) — false-positive
/// seed-loop flag; `s` binds a &str label
#[test]
fn waiting_moves_an_npc_and_it_is_observed() {
    let w = world();
    // Peoples-only (The Wilding): this test isolates the settled on-water
    // settlement's invariant (its NPCs drink in place, never walk). The wild
    // beasts DO walk — that is The Quarry waking — and their motion is covered
    // by `a_wild_beast_walks_away_from_water_and_is_observed` below, not here.
    let opts = PossessOpts {
        wild_agents: false,
        ..PossessOpts::default()
    };
    let (mut session, _opening) = Session::start(&w, &opts).unwrap();
    // THE CONFLUENCE, MEASURED: pre-Confluence, an NPC started away from its
    // resource and a full drive cycle committed at least one `agent-at` (a
    // real walk). Settlement condensation now pulls seed 42's flagship
    // settlement (and its derived NPCs) directly onto fresh water (see
    // `liveness.rs`'s
    // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`
    // — 0 moves, drinks in place), so "the world moved on wait" is no longer
    // provable via the FLAGSHIP's `agent-at`: it is now provable via `drank`.
    // (The Tumult, 2026-07-26: the flagship is STILL on water and still
    // drinks in place; predation reseated a NEIGHBOUR off the river, so the
    // session-wide agent-at count is no longer 0 even though this claim is
    // unchanged. See `walked` at the top of this file.) Starting at
    // day 0.5 (PossessOpts::default), the seek threshold (0.85) is crossed
    // at world day ~5.667 — "wait 7" spans that crossing.
    let flagship = session
        .npc_labels()
        .first()
        .map(|s| (*s).to_string())
        .expect("a session always derives NPCs");
    let out = session.handle("wait 7");
    assert!(
        session.committed_drank_count() >= 1,
        "the world moved on wait (a drank fact committed)"
    );
    // **THE WALK MOVED TO ANOTHER WORLD AT THE ROLL (Task 7), AND THE CLAIM
    // IS UNCHANGED.** The Winze T2b recovered a real `agent-at` witness here
    // by observing that spec amendment E's ring scan had re-placed seed 42's
    // settlements and one of them landed off the river. What it was actually
    // observing was a NEIGHBOUR: `npc_labels().first()` used to be another
    // settlement's single NPC, because a session derived one body from each of
    // three settlements. The roll derives the residents of the settlement you
    // are standing in, and seed 42's flagship is ON the river — all
    // sixty-seven of its residents drink in place and the session commits zero
    // positional facts. So the witness is taken where the property lives (see
    // `WALKING_SEED`'s own note), rather than asserted where it no longer
    // does. Both halves are still kept, and both are still separate facts:
    // the drink above, at seed 42, and the walk here.
    assert!(
        !walked(&mut session, &flagship),
        "measured: seed 42's flagship stands on a river, so its own residents \
         drink in place and never commit a positional fact"
    );
    //
    // The witness is `walked()` — the per-NPC `!why` recount — and not
    // `committed_agent_at_count()`, which is what an earlier version of this
    // fix reached for. This test's name says "and it is observed", and a
    // session-wide fact count observes nothing about any particular
    // creature: it is exactly the proxy `walked()`'s own doc records
    // narrowing away from. The subject is chosen BY THE PROPERTY, the way
    // `stay_put_npc` chooses its own — a hardcoded label is what The Winze
    // T2b already had to correct here once.
    let walking = world_at(WALKING_SEED);
    let (mut walker, _) = Session::start(&walking, &opts).unwrap();
    let walker_labels: Vec<String> = walker
        .npc_labels()
        .into_iter()
        .map(str::to_string)
        .collect();
    walker.handle("wait 7");
    let moved = walker_labels
        .iter()
        .find(|label| walked(&mut walker, label))
        .cloned()
        .unwrap_or_else(|| {
            panic!(
                "no resident of seed {WALKING_SEED}'s flagship recounts a positional \
                 fact after a full drive cycle — a settlement whose people must leave \
                 home for water is the whole reason this seed is pinned, so an epoch \
                 has moved it and WALKING_SEED must be re-measured"
            )
        });
    assert!(
        walked(&mut walker, &moved),
        "measured: {moved} walks to reach water, and its own `why` recount \
         names the positional fact it committed"
    );
    // The wait output mentions motion (non-empty, references an NPC/movement).
    match out {
        hornvale_vessel::Turn::Out(s) => assert!(!s.is_empty()),
        _ => panic!("wait outputs prose"),
    };
}

#[test]
fn the_same_script_is_byte_deterministic() {
    let w = world();
    let run = || {
        let (mut s, _o) = Session::start(&w, &PossessOpts::default()).unwrap();
        for cmd in ["wait 1", "wait 1", "wait 1"] {
            let _ = s.handle(cmd);
        }
        s.session_ledger_json() // a test accessor: serde_json of the session ledger
    };
    assert_eq!(
        run(),
        run(),
        "same seed + same waits -> byte-identical session ledger"
    );
}

#[test]
fn a_colocated_npcs_drinking_in_place_is_not_narrated_as_a_false_departure() {
    // THE OBSERVATION PAYOFF, RE-MEASURED (T3 review originally proved this
    // via a genuine departure; The Confluence changes what "genuine" means
    // here): the possessed agent's own settlement is guaranteed to
    // contribute a derived NPC sharing the player's starting room, and
    // `wait`'s narration must never CLAIM a departure/arrival that did not
    // actually happen — only a real positional transition earns the named
    // branch; anything else falls to the generic "stirred" tally.
    //
    // Pre-Confluence, the co-located NPC departed on its first exploration
    // step (proven by name) but then explored indefinitely without
    // returning (measured in `liveness.rs`'s
    // `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding`
    // — a real, settlement-placement gap, not the belief mechanism).
    // The Confluence's settlement condensation moves seed 42's flagship
    // settlement directly ONTO fresh water (0 moves, drinks in place — the
    // SAME measurement above, re-run after the freshwater re-point): the
    // co-located NPC now never leaves the room at all, so neither a
    // departure NOR an arrival is ever the true event across a full drive
    // cycle. (The Confluence also put both derived NEIGHBOURS on water; The
    // Tumult's predation epoch, 2026-07-26, reseated one of them —
    // `Vootkeonoagootoaneo` — off the river, and its NPC now wanders. That
    // is invisible to the player's room and does not touch this claim, which
    // is about the CO-LOCATED NPC only.) This test asserts that reality
    // honestly: `wait` across the seek crossing (~5.667 days from day 0.5)
    // commits a `drank` (the world genuinely moved) but narrates it as the
    // generic "stirred" sensing, never inventing a named departure for an
    // NPC that stayed exactly where it was.
    //
    // The mechanism this test used to prove end-to-end (naming a REAL
    // departure/arrival) is not exercised here — seed 42's flagship
    // settlement structurally cannot produce one any more (`village_info`
    // always resolves to the same, now on-water, settlement). That
    // end-to-end coverage gap is captured as a followup (decision-ledger);
    // the naming logic itself (`Session::narrate_motion`) is unchanged code,
    // reviewed at the point The Confluence stopped touching it.
    //
    // Peoples-only (The Wilding): the wild beasts DO leave their rooms (The
    // Quarry, live); this test isolates the settled on-water NPC's stay-put
    // narration. The wild motion path has its own coverage below.
    let w = world();
    let opts = PossessOpts {
        wild_agents: false,
        ..PossessOpts::default()
    };
    let (mut session, _opening) = Session::start(&w, &opts).unwrap();
    let labels: Vec<String> = session
        .npc_labels()
        .into_iter()
        .map(str::to_string)
        .collect();
    assert!(!labels.is_empty(), "a session always derives NPCs");

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("wait never releases"),
    };

    // Cross the seek threshold (~5.667 days from day 0.5): a `drank` commits
    // (measured: `committed_drank_count() >= 1`), but no one's position
    // changed.
    let crossing_wait = out_text(session.handle("wait 7"));
    assert!(
        session.committed_drank_count() >= 1,
        "the world genuinely moved (a drank fact committed)"
    );
    assert!(
        !labels.iter().any(|l| crossing_wait.contains(l.as_str())),
        "no NPC actually departed or arrived, so none may be named: {crossing_wait}"
    );
    assert!(
        crossing_wait.contains("stirred"),
        "the generic sensing fallback must fire for a same-room drink event: {crossing_wait}"
    );

    // A subsequent wait, still sated, must stay quiet (no spurious
    // departure/arrival, and no further "stirred" noise once nothing at all
    // is committed).
    let still_here = out_text(session.handle("wait 1"));
    assert!(
        !labels.iter().any(|l| still_here.contains(l.as_str())),
        "a co-located NPC that never left must not be named as departing/arriving: {still_here}"
    );
    // The premise the whole test rests on, asserted last so the narration
    // reads above are taken on an unperturbed session: the co-located NPC
    // (the flagship's own, always first in the derived roster) really did
    // stay put, so "no departure happened" is a fact about it and not an
    // artifact of nobody being derived.
    // Selected by the property (see `stay_put_npc`): after The Winze T2b the
    // flagship's own NPC walks, and the roster's SECOND member is the one that
    // drinks in place. It panics rather than skipping if none does, so the
    // narration claims above cannot quietly become vacuous.
    let colocated = stay_put_npc(&mut session, &labels);
    assert!(
        !walked(&mut session, &colocated),
        "measured: {colocated} drank in place and never left the room"
    );
}

#[test]
fn why_recounts_an_npcs_dated_history_after_it_drinks() {
    // THE PROVENANCE READ (the-quickening T4): a committed `agent-at` or
    // `drank` is a dated, provenanced fact, so the world remembers —
    // `why <npc>` must recount it with the day it was asserted, not just
    // that it happened. (Renamed from "...after_it_moves": The Confluence's
    // on-water flagship settlement never moves at all — see below.)
    //
    // Peoples-only (The Wilding): scoped to the settled flagship NPC's own
    // dated history; the wild beasts' motion is covered separately below.
    let w = world();
    let opts = PossessOpts {
        wild_agents: false,
        ..PossessOpts::default()
    };
    let (mut session, _opening) = Session::start(&w, &opts).unwrap();
    let labels: Vec<String> = session
        .npc_labels()
        .into_iter()
        .map(str::to_string)
        .collect();
    assert!(!labels.is_empty(), "a session always derives NPCs");

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("why never releases"),
    };

    // Before any wait, NO NPC has a committed agent-at or `drank` yet (the
    // day-0 pin), so the recount below has nothing of the kind to name.
    //
    // **Asked of the LEDGER rather than of the prose, since The Roll (Task
    // 7).** This used to run `!why <label>` over the whole roster and assert
    // the answer never contains the word "day". Two things broke that. The
    // roster is a settlement's whole population now, so the loop was 67
    // recounts; and a RESIDENT carries dated identity facts of its own —
    // `derive_residents` commits `is-person` and `person-born`, whose doc
    // line is literally "the day this person was born" — so the substring
    // probe now fires on a fact that has nothing to do with a walk. The
    // claim was always "no positional or drink fact has been committed yet",
    // and the two counters say exactly that, cheaply and without a proxy.
    assert_eq!(
        session.committed_agent_at_count(),
        0,
        "before any wait, no NPC has committed a positional fact"
    );
    assert_eq!(
        session.committed_drank_count(),
        0,
        "before any wait, no NPC has committed a drink either"
    );

    // Advance across a full drive cycle (the-wanting: ~5.667 days to the
    // seek crossing) so the tick commits a dated fact. THE CONFLUENCE,
    // MEASURED: an on-water NPC's crossing commits a `drank`, never an
    // `agent-at`, and this test is about recounting THAT — a dated fact that
    // is not a move. (The Tumult, 2026-07-26: the roster stopped being
    // uniformly on-water, which is why the check below is per-NPC rather than
    // session-wide. **The Winze T2b:** it stopped being the FLAGSHIP that is
    // on water — spec amendment E's ring scan re-placed the settlements and
    // the flagship now walks — so the subject is selected by the property
    // instead of by roster position. Without that this test would have gone on
    // asserting `contains("day")` against an `agent-at` recount and quietly
    // stopped being about a drink at all.)
    session.handle("wait 7");
    assert!(
        session.committed_drank_count() >= 1,
        "the NPC satisfied its sustenance goal"
    );
    let label = &stay_put_npc(&mut session, &labels);
    assert!(
        !walked(&mut session, label),
        "measured: {label} drank in place; it never walks"
    );

    let recount = out_text(session.handle(&format!("!why {label}")));
    assert!(
        recount.contains(label.as_str()),
        "the recount leads with the NPC's own name: {recount}"
    );
    assert!(
        recount.contains("day"),
        "the recount names the day the drank was asserted: {recount}"
    );
    assert!(
        !recount.contains("No one here answers"),
        "the label must resolve to the NPC that actually drank: {recount}"
    );
    // THE CONFLUENCE'S PAYOFF, RE-MEASURED: The Foresight/Surmise era pinned
    // that this exact settlement's NPC never reaches water and only ever
    // recounts as "wandered, having found no water yet (thirst)" — an
    // IGNORANT explore step, never the believer's beeline or a drink.
    // Settlement condensation resolves that gap by moving the settlement,
    // not by making the agent smarter: the NPC now stands on fresh water
    // from the start, so the FIRST crossing is a `drank`, not a move at
    // all — the recount's provenance is the drink's own reason.
    // Mutation-verify: blanking `DriveMovements::step`'s "drank from the
    // river (thirst sated)" string in `liveness.rs` reds this assertion
    // while leaving the day/name/resolution checks above green (they don't
    // touch provenance text).
    assert!(
        recount.contains("drank from the river (thirst sated)"),
        "the recount names the drink's own reason: {recount}"
    );
}

/// `needs` renders a co-located NPC's felt state as diegetic prose, never a
/// number, and that prose tracks the creature over the drive cycle rather
/// than standing still.
///
/// **THE PHRASE IS THE TICK'S OWN RESOLUTION NOW (The Rack, Task 4, spec
/// §3.4), not a re-read.** Until this campaign, `needs` re-derived every
/// present creature's `Affect` from the ledger on every call — a stateless
/// *re-imagining* of the body without its own history, adopted because the
/// tick's answer was dropped before anyone could read it. It now reports
/// what that creature's own last resolution concluded: the arbitration that
/// actually moved it, with its alarm field, its mode hysteresis and its own
/// belief, at the walk's own instant. Between ticks a creature does not
/// re-feel. So the wordings below moved, and they moved for that reason
/// rather than because any drive changed.
///
/// **Re-derived, and SCOPED TO THE COMPANION, which the older version was
/// not.** It asserted `later.contains("eats its fill")` against the whole
/// 67-line reply, so any body in the room could have satisfied it. Under the
/// tick's own resolution every one of the flagship's 67 present bodies reads
/// the same phrase after a `wait 9` (`felt_phrase` buckets an `Affect` into
/// prose, and they all land in one bucket), so an unscoped `contains` would
/// now be satisfied by a creature this test is not about. Both readings are
/// taken from the companion's own line.
///
/// The measured pair, day 0.5 then after `wait 9`, on `bodies()[1]`
/// (`Dvoashngashngo`): *"settles down to rest"* → *"grows restless"*. As the
/// older comment already warned, the SPECIFIC readings are a fact about
/// which creature is placed and are re-measured whenever that changes; only
/// the "differs across the drive cycle" shape is the claim.
///
/// MUTATION THIS MUST FAIL AGAINST: make `Session::felt_of` return the
/// DRIVEN slot's felt (`&self.roster.felts()[self.roster.driven().0]`)
/// instead of the body's own. Run and observed: `after the tick the NPC
/// reports its own resolution: The Dvoashngashngo settles down to rest.` —
/// the possession's own resolution, reported under the companion's name.
#[test]
fn needs_reports_a_colocated_npcs_felt_state_and_it_differs_across_the_drive_cycle() {
    // The Hand, Task 3: the possessed body's own settlement no longer
    // guarantees a co-located NPC (see docs/retrospectives/the-hand.md) --
    // `bodies()[1]` is placed explicitly through the test seam,
    // `place_creature_at_me`. It is redundant at The Roll's roster (a
    // resident of the settlement you stand in is co-located by construction)
    // and kept because the test's claim is about a CO-LOCATED creature, and
    // asking for the placement keeps that premise stated rather than
    // incidental.
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let companion = session.bodies()[1].entity;
    let label = session.bodies()[1].label.clone();
    session.place_creature_at_me(companion);

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("needs never releases"),
    };
    // The companion's OWN line out of the reply — see the doc above on why
    // the whole reply will not do.
    let line_for = |reply: &str, label: &str| -> String {
        reply
            .lines()
            .find(|line| line.contains(label))
            .unwrap_or_else(|| panic!("the placed companion must be named in: {reply}"))
            .to_string()
    };

    let early_reply = out_text(session.handle("needs"));
    assert!(
        !early_reply.contains("No one else is here"),
        "the placed companion must be co-located at the start: {early_reply}"
    );
    let early = line_for(&early_reply, &label);
    assert!(
        early.contains("settles down to rest"),
        "the co-located NPC reads as tired at day 0.5: {early}"
    );

    session.handle("wait 9");
    let later = line_for(&out_text(session.handle("needs")), &label);
    assert!(
        later.contains("grows restless"),
        "after the tick the NPC reports its own resolution: {later}"
    );

    // THE MUTATION-VERIFIED ASSERTION: the felt state DIFFERS across the
    // drive cycle. Fixing the drive to a constant (e.g. always returning
    // 0.0) would make `early == later` and red this line.
    assert_ne!(
        early, later,
        "the felt state must differ across the drive cycle: {early} / {later}"
    );
}

#[test]
fn why_resolves_by_numeric_id_and_reports_an_unknown_target() {
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let listing = match session.handle("!npcs") {
        Turn::Out(s) => s,
        _ => panic!("!npcs must not release"),
    };
    let id: u64 = listing
        .lines()
        .nth(1)
        .and_then(|l| l.split(['[', ']']).nth(1))
        .and_then(|s| s.parse().ok())
        .expect("!npcs lists at least one [id] label line");
    // Advance across a full drive cycle (the-wanting) so the id-resolved
    // NPC has a committed, dated agent-at to recount.
    session.handle("wait 7");
    match session.handle(&format!("!why {id}")) {
        Turn::Out(s) => assert!(s.contains("day"), "id-resolved recount names a day: {s}"),
        _ => panic!("!why must not release"),
    }
    match session.handle("!why nobody-by-this-name") {
        Turn::Out(s) => assert!(s.contains("No one here answers")),
        _ => panic!("!why must not release"),
    }
}

#[test]
fn provoke_commits_one_player_authored_disposition_fact() {
    // THE FIRST PLAYER-AUTHORED FACT: `provoke` commits a disposition-shift
    // fact about a co-located NPC into the session-owned ledger, distinct
    // from every fact the world's own systems commit (the `player:`
    // provenance is what tells the two apart).
    //
    // The Hand, Task 3: the possessed body's own settlement no longer
    // guarantees a co-located NPC (see docs/retrospectives/the-hand.md); `bodies()[1]` is
    // placed explicitly through the test seam.
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    session.place_creature_at_me(session.bodies()[1].entity);
    let before = session.committed_disposition_count();
    let turn = session.handle("!provoke");
    let after = session.committed_disposition_count();
    match turn {
        Turn::Out(s) => assert!(
            s.to_lowercase().contains("provoke") || s.contains("bristle"),
            "diegetic acknowledgement, got: {s}"
        ),
        Turn::Released(s) => panic!("expected Out, got Released({s})"),
    }
    assert_eq!(after, before + 1, "exactly one disposition fact committed");
}

#[test]
fn a_repeat_same_day_provoke_is_a_ledger_no_op_and_the_narration_says_so() {
    // SAME-DAY DEDUP IS INTENTIONAL: one disposition shift per (NPC, day,
    // direction) — escalation is gated on time passing (a `wait`), not on
    // repeating the verb. Two `provoke`s on the same NPC with no
    // intervening `wait` produce a byte-identical `Fact` envelope, so
    // `Ledger::commit` returns `Ok(false)` (idempotent no-op) the second
    // time: `committed_disposition_count` must not double-count, and the
    // narration must be honest that nothing further landed.
    //
    // The Hand, Task 3: `bodies()[1]` is placed explicitly through the test
    // seam (see docs/retrospectives/the-hand.md) since the settlement no longer guarantees
    // co-location on its own.
    let w = world();
    let (mut session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    session.place_creature_at_me(session.bodies()[1].entity);

    let out_text = |t: Turn| match t {
        Turn::Out(s) => s,
        Turn::Released(s) => panic!("provoke never releases: {s}"),
    };

    let first = out_text(session.handle("!provoke"));
    assert_eq!(
        session.committed_disposition_count(),
        1,
        "the first provoke commits one fact"
    );
    assert!(
        first.to_lowercase().contains("provoke") || first.contains("bristle"),
        "the first provoke is the effect narration: {first}"
    );

    let second = out_text(session.handle("!provoke"));
    assert_eq!(
        session.committed_disposition_count(),
        1,
        "a same-day repeat is a ledger no-op: the count must not double-count"
    );
    assert!(
        second.contains("already"),
        "a same-day repeat must not claim a fresh effect: {second}"
    );
    assert_ne!(
        first, second,
        "the repeat narration must differ from the effect narration"
    );
}

#[test]
fn possession_with_no_act_leaves_session_ledger_unchanged() {
    // BYTE-IDENTITY GUARD: a read-only verb (`look`) must commit nothing —
    // the session ledger is byte-identical to a fresh, untouched session.
    let w = world();
    let a = Session::start(&w, &PossessOpts::default())
        .unwrap()
        .0
        .session_ledger_json();
    let (mut s, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    let _ = s.handle("look"); // a read-only verb
    assert_eq!(a, s.session_ledger_json(), "read-only verbs commit nothing");
}

#[test]
fn a_wild_beast_walks_away_from_water_and_is_observed() {
    // THE WILDING, LIVE — the settled tests' inverse. `PossessOpts::wild_agents`
    // (on by default) appends the world's wild beast agents to the peopled
    // NPCs. Unlike seed 42's on-water flagship settlement — whose peoples drink
    // in place and never walk (the peoples-only tests above) — the wild beasts
    // are placed at their concentrations (a herd, a lair) away from fresh water,
    // so crossing a full drive cycle commits real `agent-at` walks. This is the
    // population that DOES move: The Quarry's predator niche and the drive layer,
    // finally exercised by a live agent in possession.
    // **AT `WILD_SEED`, NOT SEED 42, SINCE THE ROLL (Task 7).** The roll
    // derives the fauna standing within call rather than the world's top four
    // concentrations, and seed 42's flagship has no herd attractor within two
    // hops — see `WILD_SEED`'s own note for the measurement and for why this
    // is a fact about a particular seed rather than about the feature. The
    // seed moved 3 -> 9 at The Tidemark for that same reason, and the sweep
    // behind it is `wild_body_rate_over_seeds` in this file rather than a
    // number quoted from a campaign nobody can re-run.
    let w = world_at(WILD_SEED);
    let (mut wild_session, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();

    // Wild agents enlarge the roster over the peoples-only session, and are
    // genuinely wild (village-less) bodies — checked against `Body.village`
    // rather than the label text: since The Ken, Task 4 (round two), a wild
    // body's label is bare `species`, matching the settled convention
    // (`derive_staged_npcs`'s doc: "the label carries NO article"), so a
    // label no longer contains the word "wild" at all.
    let peopled_count = {
        let opts = PossessOpts {
            wild_agents: false,
            ..PossessOpts::default()
        };
        let (s, _o) = Session::start(&w, &opts).unwrap();
        s.npc_labels().len()
    };
    let labels: Vec<String> = wild_session
        .npc_labels()
        .into_iter()
        .map(str::to_string)
        .collect();
    assert!(
        labels.len() > peopled_count,
        "wild agents enlarge the roster: {} vs peopled {peopled_count}",
        labels.len()
    );
    assert!(
        wild_session.bodies().iter().any(|b| b.village.is_none()),
        "at least one appended agent reads as a wild (village-less) beast: {labels:?}"
    );

    // Cross the seek crossing (~5.667 days from day 0.5): the wild beasts,
    // unlike the on-water peoples, commit real walks — a dated, provenanced
    // `agent-at` the world remembers.
    let out = match wild_session.handle("wait 7") {
        Turn::Out(s) => s,
        Turn::Released(_) => panic!("wait never releases"),
    };
    assert!(
        wild_session.committed_agent_at_count() >= 1,
        "a wild beast placed away from water committed at least one real walk"
    );
    assert!(!out.is_empty(), "the wait narrates the world's motion");
}

/// The seed-42 settled NPC guaranteed co-located with the possessed agent at
/// `PossessOpts::default()`'s starting room (day 0.5, before any `go`) — see
/// `probe_colocated_npc_label_at_day_zero` in this file's history and the
/// task-2 report for how this was discovered. It never walks (the
/// on-water flagship settlement's peoples drink in place), so it stays
/// co-located across every `wait` in these tests too.
///
/// Re-pinned at The Wearing's merge: the flagship's rendered name re-derived
/// `Qvooshtvoagootao` -> `Doododoobodobaado`. Re-pinned AGAIN at the rebase
/// onto The Toponym's cohort ordering, which reseeds every proto-root:
/// `Goodoogogootoodadoo` -> `Gootoogotoodaoka`. Re-pinned a third time at F7
/// (The Witness, 2026-07-30), which gates `Tonogenesis` on a prior merger
/// and reseeds every cascade: `Gootoogotoodaoka` -> `Goodogododaga`.
/// Measured off the session's own `npcs` listing, which returns the same
/// seven NPCs at the same entity ids (1865-1871) before and after — the NPC
/// and its co-location are unchanged; only the label moved.
///
/// Re-pinned once more at The Contour's epoch v2 (2026-08-02,
/// `history/bake/v2`): the BAKE label bump re-mints the cascade again,
/// `Goodogododaga` -> `Godogododaga` (one fewer `o`). Same seven NPCs at the
/// same entity ids, re-verified from `book/src/gallery/possession-seed-42.md`.
///
/// Re-pinned once more by The Generalist (2026-08-03): human joins the
/// coexistence stack as a sixth competitor, redeciding seed 42's settlement
/// placement once more, which re-mints the flagship's name again:
/// `Godogododaga` -> `Goodogododaga` (one more `o`, back to the pre-Contour
/// spelling by coincidence of the draw, not a reversion of the epoch).
///
/// It happened a SIXTH time with The Tense (2026-08-05): capacity gained an
/// era axis, seed 42 re-placed from 209 settlements to 122, and the flagship's
/// name went `Goodogododaga` -> **`Googo`**. Re-verified from
/// `book/src/gallery/possession-seed-42.md`, which is the source this constant
/// must always be read from: it renders `bugbear of Googo` in the room line,
/// the `map` legend and `whoami` alike.
///
/// NOTE the near-miss: seed 42's CHIEF bugbear settlement in the almanac is
/// `Dadogogodaga`, a different place entirely. Reading the rename off the
/// almanac instead of the possession artifact gives a plausible wrong answer,
/// because this NPC does not live in the chief settlement.
///
/// A SEVENTH time, with The Glasshouse (decision 0134, 2026-08-14): the
/// terrain epoch moves every coastline, so seed 42's settlements re-place
/// once more and the flagship's name goes `Googo` -> **`Goodo`**. Re-read
/// from `book/src/gallery/possession-seed-42.md` as the note above insists,
/// where it appears identically in the room line, the `map` legend and the
/// `whoami` output. The near-miss above still applies and still resolves the
/// same way: the almanac's chief bugbear settlement is a different place.
///
/// Seven renames across seven campaigns is now enough to say what this
/// constant is: a NAME, re-derived from the namer every time anything moves
/// settlement placement, and never a fact about the NPC. The co-location this
/// test needs has held through all seven.
///
/// An EIGHTH time, with The Glasshouse's Stage B Task 4 (the thermostat): the
/// damped, greenhouse-forced insolation baseline (replacing the fixed 288 K
/// blackbody one) re-places seed 42's settlements a second time this
/// campaign, and the flagship's name goes `Goodo` -> **`Doadaga`**. Re-read
/// from `book/src/gallery/possession-seed-42.md` as the note above insists.
///
/// A TENTH time (`the_first_mark.rs` records a ninth, `Doadaga` -> `Dooga`,
/// at The Glasshouse's close), now at The Burr (Task 4): admitting an
/// alveolar trill as an ordinary manner re-places seed 42's settlements yet
/// again, `Dooga` -> **`Doaba`**. Both copies moved together — see
/// `the_first_mark.rs`'s `the_two_grievance_npc_copies_agree`.
///
/// **The Hand, Task 3: no longer the flagship's own twin.** The pre-Hand
/// `GRIEVANCE_NPC` (`bugbear of Doaba`) WAS the possessed-body duplicate this
/// task deletes, co-located by construction. `bodies()[1]` is placed
/// explicitly through the test seam
/// (`Session::place_creature_at_me`, see docs/retrospectives/the-hand.md) instead, and is
/// RE-placed before every `!provoke`/`!soothe` below rather than trusted to
/// stay put across a `wait` — its own drive-seeking is free to walk it away
/// from the flagship the moment a tick runs, unlike the twin, whose home
/// WAS the flagship.
///
/// **An ELEVENTH move, and it changed the KIND of label rather than its
/// spelling** (The Roll, Task 7): `bodies()[1]` is now a RESIDENT of the
/// flagship, and a resident is a named person, so the label went from
/// `hobgoblin of Naabeena` (a species of a place) to `Dvoashngashngo` (a name
/// its settlement's namer drew). See `the_first_mark.rs`'s copy of this
/// constant for the fuller note; both moved together as always.
const GRIEVANCE_NPC: &str = "Kvashngobvo";

#[test]
fn grievance_accumulates_across_waits_and_crosses_the_hostility_threshold() {
    // GUARD THE FIXTURE FIRST. `would_turn_hostile` answers `false` for a
    // label it has never seen, so the NEGATIVE assertions below are satisfied
    // by a stale or misspelt `GRIEVANCE_NPC` just as well as by a real one —
    // only a positive assertion can fail. This constant has already gone
    // stale twice on settlement renames. Assert the NPC exists, so the next
    // rename fails HERE, saying so.
    assert!(
        Session::start(&world(), &PossessOpts::default())
            .unwrap()
            .0
            .npc_labels()
            .contains(&GRIEVANCE_NPC),
        "GRIEVANCE_NPC ({GRIEVANCE_NPC}) is not derived at day 0.5 — the settlement was \
         probably renamed; re-read `bodies()[1].label` from a fresh session"
    );

    // THE GRIEVANCE FOLD (Task 2, direct social consequence, not an ambient
    // drive tip): an un-provoked NPC carries zero grievance and is never
    // hostile.
    let w = world();
    let (a, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    assert!(
        !a.would_turn_hostile(GRIEVANCE_NPC),
        "un-provoked NPC is neutral"
    );

    // Provoking across three distinct days climbs grievance past the
    // threshold. Same-day repeats dedup (Task 1), so each provoke here is
    // separated by a `wait` — three distinct days of antagonism. `companion`
    // is re-placed before every act (The Hand, Task 3): its own drive-seeking
    // runs on each `wait` and is free to walk it away from the flagship,
    // where the pre-Hand twin's own home kept it put.
    let (mut b, _opening) = Session::start(&w, &PossessOpts::default()).unwrap();
    // Resolved BY LABEL, not by `bodies()[1]` (see the_first_mark's
    // place_companion doc): a later world merge inserted another body ahead
    // of the grievance NPC, and index-based placement silently placed the
    // wrong creature.
    let companion = b
        .bodies()
        .iter()
        .find(|bd| bd.label == GRIEVANCE_NPC)
        .expect("GRIEVANCE_NPC has a body")
        .entity;
    b.place_creature_at_me(companion);
    b.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 0.5: grievance 1
    b.handle("wait");
    b.place_creature_at_me(companion);
    b.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 1.5: grievance 2
    b.handle("wait");
    b.place_creature_at_me(companion);
    assert!(
        !b.would_turn_hostile(GRIEVANCE_NPC),
        "two provokes is below threshold"
    );
    b.handle(&format!("!provoke {GRIEVANCE_NPC}")); // day 2.5: grievance 3
    assert!(
        b.would_turn_hostile(GRIEVANCE_NPC),
        "three provokes crosses the threshold"
    );

    // soothe pulls back below the threshold (intent vs outcome).
    b.handle("wait");
    b.place_creature_at_me(companion);
    b.handle(&format!("!soothe {GRIEVANCE_NPC}")); // day 3.5: grievance 2
    assert!(
        !b.would_turn_hostile(GRIEVANCE_NPC),
        "soothe pulls the NPC back below hostile"
    );
}

#[test]
fn unprovoked_npcs_have_zero_grievance() {
    // BYTE-IDENTITY GUARD: with no player facts, every derived NPC's
    // grievance fold is exactly zero — an unplayed world (or a session that
    // never provokes/soothes) is byte-identical by construction.
    let w = world();
    let s = Session::start(&w, &PossessOpts::default()).unwrap().0;
    for label in s.npc_labels() {
        assert_eq!(
            s.npc_grievance(label),
            Some(0.0),
            "un-provoked NPC {label} must carry exactly zero grievance"
        );
    }
}
