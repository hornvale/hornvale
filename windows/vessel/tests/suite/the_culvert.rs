//! The Culvert — the water belief's route memo, and the instruments that
//! measure it.
//!
//! Spec: `docs/superpowers/specs/2026-09-05-the-culvert-design.md`.
//!
//! # THE CAMPAIGN-TIME CONSTANTS RETIRED AT THE CLOSE (2026-09-06)
//!
//! Decision 0541 mints a hash constant for the duration of a migration and
//! retires it at the campaign's close, because a constant here equals "the
//! whole possession shape's behaviour on one seed" and therefore reddens on
//! ANY behaviour change by ANY campaign — a tax on work that has nothing to
//! do with this fold, and an unwinnable race against a queue that gates
//! main+branch rather than a branch tip. This campaign minted two, re-recorded
//! them main-first once, and retires them here. Spec §4.1 and Rule 6.
//!
//! **What replaces them.** The two witnesses below run their fixed script
//! TWICE, on two fresh sessions of one seed, and require the two runs to
//! agree — the constant-free shape `ledger_hash_witness.rs` already carries,
//! in the file this campaign's seed-42 script is borrowed from. Every
//! non-vacuity floor is checked on BOTH runs:
//!
//! - **the walk reached this campaign's fold** — `Session::route_searches()`
//!   must be non-zero, so neither hash is the hash of a walk in which the
//!   route memo was never consulted. This floor is NEW at retirement and it
//!   is the one that mattered: the seed-17 witness previously carried no
//!   floor at all, so removing its constant would have left two agreeing
//!   hashes of an empty ledger passing for free;
//! - **the ledger is not empty** — bodies must have committed new facts over
//!   the script. On seed 42 this is [`crate::ledger_hash_witness::run_fixed_script`]'s
//!   own refusal (at least two of the seed's bodies), enforced by
//!   construction on both runs; on seed 17 it is counted here.
//!
//! Both hashes are PRINTED on every run, so a future migration has the
//! numbers without this file gating on them.
//!
//! **What retirement costs, said plainly.** A constant-free witness guarantees
//! DETERMINISM (two fresh sessions on one seed produce the same bytes) plus
//! its FLOORS (the fold was entered and the ledger is not empty). **It cannot
//! detect a BEHAVIOUR CHANGE at all** — a fold that sent every creature to a
//! different water room would move both runs together and be witnessed by
//! neither. That is what the constants were for, and it is exactly what
//! retiring them gives up. The campaign's surviving instruments against a
//! behaviour change are the direct comparisons, not this file's hashes: the
//! route-memo equivalence test below, `resident_folds.rs`'s FOLD-equals-SCAN
//! sweeps, and `turn_budget::the_route_memo_survives_between_waits`.
//!
//! **The floor arm has been observed firing, on a rebuilt binary** (2026-09-06;
//! taken before this record was written, because a witness never seen to fail
//! is a witness nobody has checked). `RouteMemo::hops`'s `self.searches += 1`
//! was neutralised with `scripts/mutate.py` — the one token that lets the memo
//! say it searched — and both witnesses went RED on the floor:
//!
//! ```text
//! witness        searches (green -> mutated)   ledger hash (green -> mutated)
//! seed 42        1  -> 0                       0x9dd87f4cea554d28 -> UNMOVED
//! seed 17        83 -> 0                       0xbde5058309750ca4 -> UNMOVED
//! ```
//!
//! **That one experiment is both halves of this section.** The floor caught it;
//! the hashes did not move at all, because the mutation changes no committed
//! byte — which is the retirement cost above, demonstrated rather than
//! asserted. Restored with a byte-for-byte `cp` from a pre-mutation copy (never
//! `git checkout --`), `MUTATION APPLIED` re-grepped to zero occurrences,
//! `git diff` on the file empty, and the GREEN re-taken on a binary that
//! printed `Compiling hornvale-vessel` — the stale-binary trap, avoided
//! deliberately.
//!
//! The seed-42 floor is tighter than it looks: that walk runs **exactly one**
//! real search, which is `turn_budget::the_route_memo_survives_between_waits`'s
//! own finding (2,692 route questions across eight waits against one search)
//! seen from a second instrument. There is no slack in it to hide a
//! regression to zero.
//!
//! # THE DATED RECORD (Task 3: the campaign-time hash constants and their
//! positive control)
//!
//! Everything from here to the end of this doc is history, recorded between
//! 2026-09-05 and 2026-09-06. Its numbers were correct when taken and nothing
//! re-checks them; read them as a dated record, never as a current claim.
//!
//! Recorded 2026-09-05, minted at `c6edaa548f213a6619651c829e96e18ac768fa42`
//! (`windows/vessel/src/liveness.rs` and `resident.rs` unchanged from that
//! SHA at time of writing), each from two agreeing fresh-session runs:
//!
//! ```text
//! CULVERT_SEED_42_LEDGER   0x9874_b735_5733_7f83   (seed 42, the-detent's own 60-tick script)
//! CULVERT_WATER_LEDGER     0x1d03_ec0f_c130_50fe   (seed 17, 12 waits — resident_folds.rs's KERF_WATER_SEED/KERF_WAITS shape)
//! ```
//!
//! **THE POSITIVE CONTROL**: `believed_water`'s `min_by` primary hop-distance
//! ordering flipped from ascending to descending —
//! `la.cmp(lb).then_with(|| ra.cmp(rb))` -> `lb.cmp(la).then_with(|| ra.cmp(rb))`
//! at the site inside `believed_water` (`windows/vessel/src/liveness.rs`,
//! `seen.into_iter()...min_by(...)`) — so the function keeps the KNOWN water
//! room FARTHEST from `npc.home` by planned hop count, instead of nearest,
//! whenever two or more candidates differ in distance.
//! `shared_believed_water`'s own pooling `min_by` a few lines below is a
//! separate call site and was left untouched; it calls `believed_water`
//! internally, which is the effect that reaches it. Applied with
//! `scripts/mutate.py`.
//!
//! **Why this mutation and not a tie-break flip.** The Kerf's own prior
//! measurement on this exact seed and script governs the choice (see the
//! module doc of `windows/vessel/tests/suite/the_kerf.rs`): its control B — a
//! narrower admission-window flip in the predecessor `KnownWater` tenant —
//! moved NEITHER of that campaign's ledger hashes on seed 17 at 12 waits,
//! because a differently admitted room reaches a committed fact only through
//! the chain different-admission -> different candidate set -> different
//! chosen room -> different committed route, and that chain did not complete
//! there. This control instead rewrites the SELECTION criterion over
//! whatever candidate set is already admitted, so it fires whenever an agent
//! knows two or more water rooms at different hop-distances from home — and
//! on seed 17, `resident_folds.rs`'s own measurement tabulates 83 wet rooms
//! and 0 dry ones among every sighted room, so most walking residents
//! accumulate more than one candidate.
//!
//! Under it (two fresh sessions under the mutation, still agreeing with each
//! other at `0xb09d0ac58c46025e`):
//!
//! ```text
//! witness                    green (minted)          under the control
//! CULVERT_SEED_42_LEDGER     0x9874b73557337f83      0x9874b73557337f83   (unmoved)
//! CULVERT_WATER_LEDGER       0x1d03ec0fc13050fe      0xb09d0ac58c46025e   (MOVED)
//! ```
//!
//! ## RE-RECORDED MAIN-FIRST, 2026-09-06, at the close absorption
//!
//! Both constants MOVED when 54 commits of main were absorbed at `63ce3c5e2`:
//!
//! ```text
//!   CULVERT_SEED_42_LEDGER   0x9874_b735_5733_7f83 -> 0x9dd8_7f4c_ea55_4d28
//!   CULVERT_WATER_LEDGER     0x1d03_ec0f_c130_50fe -> 0xbde5_0583_0975_0ca4
//! ```
//!
//! **The move is MAIN's, not this campaign's, and that was established by a
//! same-tree control rather than by argument.** Neutralising `RouteMemo`'s
//! cache lookup so it can never serve a hit makes the memo do exactly what
//! the pre-memo code did — one fresh search per ask. Run that way, on this
//! same tree, both hashes are IDENTICAL to their memoized values:
//!
//! ```text
//!   seed 42   neutralised 0x9dd87f4cea554d28   memoized 0x9dd87f4cea554d28
//!   seed 17   neutralised 0xbde5058309750ca4   memoized 0xbde5058309750ca4
//! ```
//!
//! So the memo changes no committed byte, and the movement is attributable to
//! main. The absorbed range contains The Lot (`f8859b8d7`), which edits
//! `windows/worldgen/src/{lib,person_promote,vestige}.rs` — world generation,
//! upstream of every possession-shape ledger.
//!
//! The corroborating control the campaign already held: at `8acd377c5`, with
//! the memo FULLY WIRED after a 99-commit absorption, both constants matched
//! their minted values. The only delta since is main's.
//!
//! **`CULVERT_SEED_42_LEDGER` DID NOT MOVE, and that is recorded rather than
//! hidden: it witnesses only the seed-42 walk's byte-identity, not the
//! water-belief route memo.** This campaign's own Task 2 measurement is why:
//! the seed-42 lab shape's MEDIAN agent makes zero water searches, and only
//! 11 of 50 roster members hold any water belief at all, so a ranking change
//! inside `believed_water` has nowhere on that walk to show up. Read a green
//! run of it as a blast-radius check on the whole possession shape, never as
//! evidence about this campaign's route-memo path — the same caution
//! `ledger_hash_witness.rs`'s "Seed 42 is not it" note and `the_detent.rs`'s
//! own seed-42 constant both record for the unrelated fear path.
//!
//! **`CULVERT_WATER_LEDGER` is the load-bearing half of this pair**, and it
//! is still the SECONDARY witness the brief names it: a hash is a weaker
//! instrument than the direct FOLD-equals-SCAN set comparison
//! `resident_folds.rs` performs (and Task 6's equivalence test performs at
//! the route-memo boundary), because a hash can stay blind to an admission-
//! set change that never completes the chain to a committed route — exactly
//! what happened to The Kerf's control B on this same seed and script — even
//! where the semantic comparison would redden immediately.
//!
//! Restored with a byte-for-byte `cp` from a pre-mutation copy (never `git
//! checkout --`, which would also discard any uncommitted work in the same
//! file), and both green hashes were reconfirmed on a REBUILT binary before
//! this record was written — a restored source with a stale binary is a
//! known trap in this repository (The Axes, retrospective).
//!
//! **The two values above are the LAST ones these constants held**, taken at
//! `8acd377c5` and re-recorded at the close absorption. They no longer
//! execute; the witnesses below print their live hashes instead, so a future
//! migration of `LatestVisit`, `believed_water`, `shared_believed_water` or
//! `nearer_to_home` can mint its own pair from this file's output and compare
//! against this record without anything here gating on it. Mint the seed-17
//! half first: the seed-42 half was BLIND to this campaign's path, for the
//! reason the paragraph above gives, and its own control never moved it.

use crate::common;
use hornvale_kernel::{Facet, Ledger, WorldTime};
use hornvale_vessel::action::plan_to_room;
use hornvale_vessel::body::Body;
use hornvale_vessel::liveness;
use hornvale_vessel::resident::{OwnedFolds, ResidentFolds};
use hornvale_vessel::{PossessOpts, Session};

/// **Every predicate the drive stack COMMITS is one the roster REGISTERS —
/// the REMOVAL direction only.**
///
/// This is the guard for a defect that has now happened twice: `SLEPT_ON`
/// (The Pallet, 2026-09-03) was added to the drive stack and to
/// `Session::start`, and the two benches that hand-copy the same list were
/// not updated, so both panicked with `UnknownPredicate` for two days across
/// two merged campaigns. Nothing caught it because `--all-targets` COMPILES
/// an example and no gate RUNS one.
///
/// The direction this enforces is `committed ⊆ registered`. It is blind to
/// over-registration — a roster entry no drive ever commits passes here — and
/// that is the safe direction: an extra registration is inert, a missing one
/// is a panic.
///
/// **What this test cannot see (fix round 1 finding, Important):** the
/// `[liveness::AGENT_AT, ...]` list right below is itself a hand-maintained
/// copy of the same six names — a FOURTH copy of the roster, no better than
/// the three this campaign just collapsed into one. It can only catch a
/// predicate being REMOVED from `DRIVE_PREDICATES`; it is blind to one being
/// ADDED to the drive stack and never joining the roster at all, which is
/// exactly what happened with `SLEPT_ON` and would happen again with a
/// seventh predicate tomorrow. The ADDITION direction is
/// [`every_str_predicate_const_in_liveness_joins_the_roster_or_is_waived`]
/// below, which reads `liveness.rs`'s own source instead of a copy of it. The
/// two are complementary, not redundant: this one is exact and expensive to
/// widen (it names the drive stack's actual commit sites), the other is a
/// cheap default-deny sweep that requires no reader to remember it exists.
#[test]
fn every_drive_predicate_the_stack_commits_is_on_the_roster() {
    let roster: std::collections::BTreeSet<&str> =
        liveness::DRIVE_PREDICATES.iter().map(|(p, _)| *p).collect();
    for pred in [
        liveness::AGENT_AT,
        liveness::DRANK,
        liveness::RESTED,
        liveness::SLEPT,
        liveness::SLEPT_ON,
        liveness::EATEN,
    ] {
        assert!(
            roster.contains(pred),
            "the drive stack commits `{pred}` and DRIVE_PREDICATES does not \
             register it — this is the `slept-on` defect recurring. Add it to \
             DRIVE_PREDICATES in windows/vessel/src/liveness.rs."
        );
    }
}

/// **The roster carries a doc for every entry, and no duplicates.**
///
/// `register_predicate` takes a doc string, and a registry entry with an empty
/// one is a registry entry nobody can read. A duplicate name would register
/// twice — idempotent today, but it would mean the roster had stopped being a
/// list of distinct predicates and nothing else would say so.
#[test]
fn the_drive_predicate_roster_is_well_formed() {
    let mut seen = std::collections::BTreeSet::new();
    for (name, doc) in liveness::DRIVE_PREDICATES {
        assert!(
            !name.is_empty(),
            "a roster entry has an empty predicate name"
        );
        assert!(!doc.is_empty(), "roster entry `{name}` has an empty doc");
        assert!(seen.insert(*name), "roster entry `{name}` appears twice");
    }
    assert!(
        seen.len() >= 6,
        "the roster holds {} entries; the drive stack commits at least six",
        seen.len()
    );
}

/// The doc-comment marker that waives one `pub const ...: &str` declaration
/// from this scan, on the line directly above it. Must name a reason —
/// `culvert-roster: waiver()` with nothing between the parens is a parse
/// error, not a pass (the same shape `type-audit`'s `waiver(...)`, `plumb`'s
/// per-species tags and seam-guard's `expect(survives: …)` all use).
const WAIVER_MARKER: &str = "culvert-roster: waiver(";

/// `liveness.rs`'s own source, read fresh every run rather than copied.
///
/// This test lives in `windows/vessel`, so `CARGO_MANIFEST_DIR` already IS
/// the vessel crate root — unlike a workspace-wide scan
/// (`cli/tests/suite/lexicon_guard.rs`, `temp_path_ratchet.rs`), there is no
/// repo-root walk to do: the file this guard cares about is exactly one path
/// down from the crate this test already builds inside.
fn liveness_source() -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/liveness.rs");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("the_culvert guard could not read {}: {e}", path.display()))
}

/// A byte is part of a Rust identifier.
fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Does `haystack` contain `ident` as a whole identifier, not as a substring
/// of a longer one (so `SLEPT` does not falsely match inside `SLEPT_ON`)?
fn contains_identifier(haystack: &str, ident: &str) -> bool {
    let bytes = haystack.as_bytes();
    let mut start = 0usize;
    while let Some(rel) = haystack[start..].find(ident) {
        let pos = start + rel;
        let end = pos + ident.len();
        let before_ok = pos == 0 || !is_ident_byte(bytes[pos - 1]);
        let after_ok = end >= bytes.len() || !is_ident_byte(bytes[end]);
        if before_ok && after_ok {
            return true;
        }
        start = pos + 1;
    }
    false
}

/// Every `pub const <NAME>: &str` declaration in `src`, paired with the
/// waiver reason named on the line directly above it (`Some("")` if the
/// marker is present but reasonless, `None` if the const is unwaived).
///
/// Deliberately line-oriented rather than a real parser, the same shape
/// `temp_path_ratchet.rs`'s `fixed_sites` uses over `temp_dir()` sites: every
/// declaration this scan needs to see is `pub const NAME: &str = "...";` on
/// one line, which is true of every predicate constant in this file today,
/// and a multi-line declaration would simply fail to match rather than
/// silently mis-parse — the scan is default-deny on WHAT IT SEES, and a
/// declaration it cannot see is a gap in the scan's own reach, not a pass.
fn str_const_declarations(src: &str) -> Vec<(String, Option<String>)> {
    let lines: Vec<&str> = src.lines().collect();
    let mut out = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        let Some(rest) = trimmed.strip_prefix("pub const ") else {
            continue;
        };
        let Some(colon) = rest.find(':') else {
            continue;
        };
        let name = rest[..colon].trim();
        if name.is_empty() {
            continue;
        }
        let after_colon = rest[colon + 1..].trim_start();
        if !after_colon.starts_with("&str") {
            continue;
        }
        let waiver = (i > 0)
            .then(|| lines[i - 1].trim_start())
            .and_then(|prev| prev.find(WAIVER_MARKER))
            .map(|pos| {
                let after = &lines[i - 1].trim_start()[pos + WAIVER_MARKER.len()..];
                after.split(')').next().unwrap_or("").trim().to_string()
            });
        out.push((name.to_string(), waiver));
    }
    out
}

/// The text of `DRIVE_PREDICATES`'s own array literal — from its `pub const`
/// declaration up to the first `];` that follows, which closes the literal
/// (the const's own doc comment above it contains no `];`, so this cannot
/// close early on a false match).
fn drive_predicates_block(src: &str) -> String {
    let start = src
        .find("pub const DRIVE_PREDICATES")
        .expect("this guard expects `pub const DRIVE_PREDICATES` in liveness.rs");
    let after = &src[start..];
    let end = after
        .find("];")
        .expect("DRIVE_PREDICATES's array literal must close with `];`");
    after[..end].to_string()
}

/// **Every `pub const <NAME>: &str` predicate declaration in `liveness.rs`
/// joins `DRIVE_PREDICATES`, or is explicitly waived — the ADDITION
/// direction the removal-guard above cannot see.**
///
/// The sibling test above checks a hand-written list of six names against
/// `DRIVE_PREDICATES` — a fourth hand-maintained copy of the same roster, so
/// it can only ever catch a predicate being REMOVED. The defect this
/// campaign fixes was an ADDITION: `SLEPT_ON` (The Pallet, 2026-09-03) was
/// declared and the benches were never told, and a hardcoded six-name test
/// list has no way to notice a seventh. So this scan reads `liveness.rs`'s
/// own SOURCE instead: every `pub const NAME: &str` it finds must appear
/// inside `DRIVE_PREDICATES`'s array literal, or carry
/// `culvert-roster: waiver(<reason>)` on the doc-comment line directly above
/// its declaration. A reasonless waiver — `waiver()` with nothing between the
/// parens — is a parse error, not a pass: a one-directional acknowledgement
/// can only ever be satisfied, so it rots the moment nobody re-checks it.
///
/// **Scoped to this ONE file, deliberately.** Predicate constants also live
/// in two other files, and each family is registered next to its own use:
/// `windows/vessel/src/thing.rs` declares the thing-layer predicates
/// (`LOCATED_IN`, `OPENNESS`, `LOCKEDNESS`), and `windows/vessel/src/
/// session.rs` declares the player-possession predicates
/// (`DISPOSITION_SHIFT`, `TURNED_HOSTILE`, `POSSESSED_BY`,
/// `POSSESSION_ENDED`). Neither family is a drive predicate —
/// `DRIVE_PREDICATES`'s own doc says the thing-layer trio is "not this
/// roster's business" — so widening this scan to those two files would
/// produce four false positives on the very next run, not four real
/// findings.
#[test]
fn every_str_predicate_const_in_liveness_joins_the_roster_or_is_waived() {
    let src = liveness_source();
    let declarations = str_const_declarations(&src);
    assert!(
        declarations.len() >= 6,
        "expected to find at least the six known drive-predicate `pub const \
         NAME: &str` declarations in liveness.rs; found {} -- the scan's own \
         line-oriented parser may have stopped matching a declaration shape \
         that changed",
        declarations.len()
    );

    let reasonless: Vec<&str> = declarations
        .iter()
        .filter_map(|(name, waiver)| match waiver.as_deref() {
            Some("") => Some(name.as_str()),
            _ => None,
        })
        .collect();
    assert!(
        reasonless.is_empty(),
        "a `culvert-roster: waiver(...)` clause must name a reason -- an \
         empty one can only ever be satisfied, so it rots silently: {reasonless:?}"
    );

    let block = drive_predicates_block(&src);
    let unrostered: Vec<&str> = declarations
        .iter()
        .filter(|(name, waiver)| waiver.is_none() && !contains_identifier(&block, name))
        .map(|(name, _)| name.as_str())
        .collect();
    assert!(
        unrostered.is_empty(),
        "liveness.rs declares `pub const {unrostered:?}: &str` and \
         DRIVE_PREDICATES does not carry it -- this is the `slept-on` defect \
         recurring on a NEW predicate, the direction the hand-written \
         removal-guard above cannot see. Either add it to DRIVE_PREDICATES in \
         windows/vessel/src/liveness.rs, or -- if it genuinely is not a \
         session-registered drive predicate -- waive it with a \
         `culvert-roster: waiver(<reason>)` doc line directly above its \
         declaration."
    );
}

// ---------------------------------------------------------------------------
// Task 3: campaign-time hash constants, and the positive control that proves
// they reach this campaign's path. See the module doc's
// "# THE DATED RECORD" section for the values, the SHA and the control.
// ---------------------------------------------------------------------------

/// The seed-42 possession shape, byte-identical to
/// [`crate::ledger_hash_witness::the_seed_42_walk_commits_the_same_ledger_bytes`]'s
/// own script (30 waits, `look`, 30 waits) — reused directly per the brief's
/// step 1 rather than re-implemented, so this witness and that one can never
/// silently diverge on what "the seed-42 walk" means.
fn culvert_seed_42_ledger_hash() -> WitnessRun {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    let bodies: Vec<_> = session.bodies().iter().map(|b| b.entity).collect();
    let before: Vec<usize> = bodies
        .iter()
        .map(|&e| session.committed_fact_count_for(e))
        .collect();
    crate::ledger_hash_witness::run_fixed_script(&mut session);
    let grew = bodies
        .iter()
        .zip(before.iter())
        .filter(|&(&e, &b)| session.committed_fact_count_for(e) > b)
        .count();
    WitnessRun {
        hash: crate::ledger_hash_witness::fnv1a(session.session_ledger_json().as_bytes()),
        searches: session.route_searches(),
        bodies_that_committed: grew,
    }
}

/// One fresh run of a retired hash witness: the ledger hash, and the two
/// floors that keep two agreeing hashes from being two hashes of a walk in
/// which nothing happened.
///
/// The floors exist because the constants do not. While
/// `CULVERT_WATER_LEDGER` was asserted, a walk that committed nothing would
/// have reddened on the constant; with the constant retired, two agreeing
/// hashes of an empty ledger would pass for free. See the module doc's
/// "# THE CAMPAIGN-TIME CONSTANTS RETIRED AT THE CLOSE".
struct WitnessRun {
    /// FNV-1a of the session's committed ledger JSON.
    hash: u64,
    /// Real `plan_to_room` searches this session's shared
    /// [`liveness::RouteMemo`] ran, through `Session::route_searches()`. The
    /// floor that says the walk REACHED the fold this witness is about.
    searches: u64,
    /// How many of the session's bodies committed at least one new fact over
    /// the script. The floor that says the ledger is not empty.
    bodies_that_committed: usize,
}

/// How many `wait`s the water-belief possession shape takes. A module-local
/// copy of `resident_folds.rs`'s own `KERF_WAITS` (that constant is private
/// to its file), kept in step deliberately: both witnesses walk the same
/// twelve ticks so a future reader can compare this file's hash against that
/// file's FOLD-equals-SCAN sweep on the identical script.
/// type-audit: bare-ok(count)
const CULVERT_WATER_WAITS: usize = 12;

/// The seed the water-belief possession shape walks. A module-local copy of
/// `resident_folds.rs`'s own `KERF_WATER_SEED`, for the reason
/// [`CULVERT_WATER_WAITS`] gives.
/// type-audit: bare-ok(index)
const CULVERT_WATER_SEED: u64 = 17;

/// The seed-42 script's non-empty-ledger floor: how many bodies must commit
/// at least one new fact over its sixty ticks.
///
/// Two, because that is the number
/// [`crate::ledger_hash_witness::run_fixed_script`] itself refuses to return
/// under — this witness restates the same floor rather than inventing a
/// second one, so the two can never disagree about what a non-vacuous
/// seed-42 walk is.
///
/// **The name ends `_BODY_COUNT` and not `_BODIES` deliberately.**
/// `cli/tests/suite/claim_shape.rs`'s `looks_like_seeds_const` flags any
/// ALL-CAPS token that contains `SEED` and ends in `S` as a seed ROSTER, and
/// a test holding one must declare a `claim:` shape (decision 0093). This is
/// a body-count floor over one seed, not a roster, so the honest fix is a
/// name that is not plural-shaped — declaring a quantified claim this test
/// does not make would satisfy the guard with a falsehood.
/// type-audit: bare-ok(count)
const SEED_42_MIN_BODY_COUNT: usize = 2;

/// The seed-17 script's non-empty-ledger floor.
///
/// Two, and it is a floor rather than the observed value: twelve waits over a
/// roster of 67 commit far more than two bodies' worth of facts, and pinning
/// the observed count would make this witness a golden of the walk it is
/// supposed to have stopped pinning. The floor asserts the ledger is not
/// empty; the equality below asserts determinism; nothing here asserts a
/// behaviour.
/// type-audit: bare-ok(count)
const WATER_MIN_BODY_COUNT: usize = 2;

/// The belief-rich possession shape: seed 17, twelve `wait`s, no `look` —
/// `resident_folds.rs`'s `kerf_possession_ledger` walk, reached independently
/// here (that function is private to its own file) rather than reused, since
/// this module has no reason to depend on `resident_folds.rs`.
fn culvert_water_ledger_hash() -> WitnessRun {
    let world = common::build(CULVERT_WATER_SEED).expect("the water-belief seed builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the water-belief seed starts a session");
    let bodies: Vec<_> = session.bodies().iter().map(|b| b.entity).collect();
    let before: Vec<usize> = bodies
        .iter()
        .map(|&e| session.committed_fact_count_for(e))
        .collect();
    for _ in 0..CULVERT_WATER_WAITS {
        session.handle("wait");
    }
    let grew = bodies
        .iter()
        .zip(before.iter())
        .filter(|&(&e, &b)| session.committed_fact_count_for(e) > b)
        .count();
    WitnessRun {
        hash: crate::ledger_hash_witness::fnv1a(session.session_ledger_json().as_bytes()),
        searches: session.route_searches(),
        bodies_that_committed: grew,
    }
}

/// Every floor a retired witness carries, checked on ONE run. Called on BOTH
/// runs of both witnesses, before either hash comparison, so a walk that
/// reached nothing reddens on the floor rather than passing on two agreeing
/// hashes of an empty ledger.
///
/// `min_bodies` differs between the two shapes because the scripts do: the
/// seed-42 script is sixty ticks and
/// [`crate::ledger_hash_witness::run_fixed_script`] already refuses to return
/// under two, while the seed-17 script is twelve waits. Both numbers are the
/// measured floor rounded DOWN to what the shape must always clear, never the
/// observed value — an observed value standing in for a predicate is the
/// defect this campaign produced three times (retrospective).
fn assert_witness_floors(label: &str, run: &WitnessRun, min_bodies: usize) {
    assert!(
        run.bodies_that_committed >= min_bodies,
        "{label}: only {} bodies committed a new fact over this script (floor {min_bodies}), \
         so the hash is close to the hash of an empty ledger and two runs would agree on \
         nothing happening",
        run.bodies_that_committed
    );
}

/// The seed-42 possession shape is deterministic: two fresh sessions running
/// one fixed script commit the same ledger bytes, over a walk that reached
/// the route memo and committed facts.
///
/// **There is no committed constant here any more, and its absence is the
/// campaign's ruling rather than an omission** — see the module doc's
/// "# THE CAMPAIGN-TIME CONSTANTS RETIRED AT THE CLOSE", which records the
/// value this witness held (`0x9dd8_7f4c_ea55_4d28`, main's at the close
/// absorption) and the same-tree control that attributed its movement to
/// main rather than to the memo.
///
/// **What this witness never saw, retired or not.** The seed-42 lab shape's
/// MEDIAN agent makes zero water searches and only 11 of 50 roster members
/// hold any water belief (this campaign's Task 2 measurement), so a ranking
/// change inside `believed_water` had nowhere on this walk to show up — which
/// is exactly what the campaign's own positive control found, moving the
/// seed-17 hash and leaving this one unmoved. Read it as a blast-radius check
/// on the whole possession shape, never as evidence about the route memo.
/// The `route_searches() > 0` floor is a claim that the fold was ENTERED, not
/// that a change to it would be seen.
#[test]
fn the_culvert_seed_42_walk_is_deterministic() {
    let first = culvert_seed_42_ledger_hash();
    let second = culvert_seed_42_ledger_hash();
    println!("--- the culvert seed-42 possession shape ---");
    println!(
        "ledger hash {:#018x} (second fresh session: {:#018x}); \
         searches {} / {}, bodies committing {} / {}",
        first.hash,
        second.hash,
        first.searches,
        second.searches,
        first.bodies_that_committed,
        second.bodies_that_committed
    );

    assert_witness_floors("run 1", &first, SEED_42_MIN_BODY_COUNT);
    assert_witness_floors("run 2", &second, SEED_42_MIN_BODY_COUNT);

    assert_eq!(
        first.hash, second.hash,
        "two fresh seed-42 sessions running the same fixed script committed DIFFERENT \
         ledger bytes ({:#018x} against {:#018x}) — the walk is not deterministic, which \
         is a constitutional failure and not a moved golden",
        first.hash, second.hash
    );
}

/// The water-belief possession shape (seed 17, twelve waits) is
/// deterministic, over a walk that reached the route memo and committed
/// facts.
///
/// **This is the load-bearing half of the retired pair**, and the module doc
/// records why: seed 17 is the belief-rich shape (52 of 67 residents hold a
/// non-empty belief, max 23 rooms), so the campaign's positive control — a
/// flipped hop-distance ordering inside `believed_water` — MOVED this hash
/// (`0x1d03_ec0f_c130_50fe` to `0xb09d_0ac5_8c46_025e`) while leaving the
/// seed-42 witness untouched.
///
/// **The constant that carried that reach is gone, and the floors are what
/// replaced it.** This witness previously asserted a value and carried no
/// floor at all; retiring the value without adding one would have left two
/// agreeing hashes of an empty ledger passing for free. Even with the floors,
/// the hash can no longer detect a behaviour change — only the direct
/// comparisons can, and the module doc names them.
#[test]
fn the_culvert_water_belief_walk_is_deterministic() {
    let first = culvert_water_ledger_hash();
    let second = culvert_water_ledger_hash();
    println!(
        "--- the culvert water-belief (seed {CULVERT_WATER_SEED}, {CULVERT_WATER_WAITS} waits) ---"
    );
    println!(
        "ledger hash {:#018x} (second fresh session: {:#018x}); \
         searches {} / {}, bodies committing {} / {}",
        first.hash,
        second.hash,
        first.searches,
        second.searches,
        first.bodies_that_committed,
        second.bodies_that_committed
    );

    assert_witness_floors("run 1", &first, WATER_MIN_BODY_COUNT);
    assert_witness_floors("run 2", &second, WATER_MIN_BODY_COUNT);

    assert_eq!(
        first.hash, second.hash,
        "two fresh seed-{CULVERT_WATER_SEED} sessions running the same \
         {CULVERT_WATER_WAITS}-wait script committed DIFFERENT ledger bytes \
         ({:#018x} against {:#018x}) — the walk is not deterministic",
        first.hash, second.hash
    );
}

// ---------------------------------------------------------------------------
// Task 4: the counting witness, red on the pre-memo tree. `believed_water`
// calls `plan_to_room` once per room in `water_at(entity, t)`'s result, so a
// roster-wide sweep makes exactly `Σ|water_at(entity, t)|` such calls — this
// section counts those calls directly through `water_at`, without needing to
// instrument `believed_water` itself (no production code changes this task).
// ---------------------------------------------------------------------------

/// What one roster-wide `believed_water`-shaped sweep counted: how many
/// `plan_to_room` calls it implies, how many DISTINCT `(home, dest)` pairs
/// those calls fall on, how many residents held a non-empty belief at all
/// (the denominator under every ratio here), and the largest single
/// resident's known-water set. Produced by [`culvert_sweep_counts`] and
/// consumed by Tasks 5, 6, 7 and 9.
struct SweepCounts {
    /// The REAL `plan_to_room` searches one roster-wide `believed_water`
    /// sweep runs, read off the shared [`liveness::RouteMemo`]'s own
    /// `searches()` counter after the sweep (The Culvert, Task 7).
    ///
    /// **This field's MEANING changed in Task 7, and the change is the
    /// point.** Task 4 derived it analytically as `Σ|water_at(entity, t)|`
    /// without calling `believed_water` at all, because at that time no memo
    /// existed and the two were equal by construction. That derivation is a
    /// PROXY, and a proxy the memo cannot move: it reads set sizes off
    /// `LatestVisit`, which Task 7 does not touch, so it would still report
    /// 529 on a fully-memoized tree and this witness could never have gone
    /// green. The occurrence count it used to hold is not lost — it is
    /// [`Self::occurrences`] now — and `calls` is measured on the real
    /// wired path instead.
    calls: usize,
    /// `Σ|water_at(entity, t)|` — how many times the sweep ASKS for a route,
    /// i.e. what it cost before the memo (529 on the possession shape at
    /// wait 12, Task 4's own measurement). Derived from `LatestVisit`
    /// independently of the sweep, so `occurrences > distinct_pairs` is the
    /// non-vacuity check that there were duplicates to collapse at all.
    occurrences: usize,
    /// Distinct `(home, dest)` pairs among those calls — the population the
    /// memo (Task 6) actually needs to hold, always `<= calls`.
    distinct_pairs: usize,
    /// How many `npcs` members hold a non-empty `water_at(entity, t)` — the
    /// denominator every ratio over this sweep must be asserted beneath.
    non_empty: usize,
    /// The largest single resident's `water_at(entity, t)` set size.
    max_set: usize,
}

/// What one roster-wide `believed_water` sweep over `npcs` at `t` costs, in
/// TWO independently-derived halves.
///
/// The first half is analytic and reads straight off
/// [`hornvale_vessel::resident::LatestVisit::water_at`] — exactly what
/// `believed_water` itself reads before ranking the rooms it returns: how
/// many route questions the sweep asks (`occurrences`), how many DISTINCT
/// `(home, dest)` pairs those questions fall on (`distinct_pairs`), and the
/// two denominators. One shared `borrow_mut` for that whole pass, the same
/// shape `session_length_scaling.rs`'s own `water_belief_counts` and
/// `resident_folds.rs`'s `kerf_fold_equals_scan` both use.
///
/// The second half RUNS THE SWEEP — every `npcs` member's real
/// `believed_water`, through ONE shared [`liveness::RouteMemo`], the scope
/// production gives it — and reads `searches()` back off that memo. That is
/// `calls`.
///
/// **The two halves must not be folded into one, and that is the whole
/// design.** `distinct_pairs` is computed from the fold store without a memo
/// in sight; `calls` is computed from the memo without counting a pair. So
/// the witness below compares two numbers that arrive by different routes,
/// rather than an instrument against itself. The fold-store borrow is
/// DROPPED before the sweep runs, for the same reason `believed_water`'s own
/// is (`liveness.rs`): the sweep re-borrows it, and holding it across would
/// be a runtime panic.
fn culvert_sweep_counts(
    ledger: &Ledger,
    folds: &OwnedFolds,
    npcs: &[Body],
    t: WorldTime,
    terrain: &dyn liveness::Terrain,
) -> SweepCounts {
    let mut occurrences = 0usize;
    let mut non_empty = 0usize;
    let mut max_set = 0usize;
    let mut pairs: std::collections::BTreeSet<(Facet, Facet)> = std::collections::BTreeSet::new();
    {
        let mut store = folds.borrow_mut();
        let latest_visit = store.latest_visit(ledger);
        for npc in npcs {
            let seen = latest_visit.water_at(npc.entity, t, terrain);
            if !seen.is_empty() {
                non_empty += 1;
            }
            max_set = max_set.max(seen.len());
            occurrences += seen.len();
            for room in seen {
                let here = liveness::agent_position(ledger, npc, t);
                pairs.insert((here, room));
            }
        }
    }
    let mut calls = 0usize;
    for npc in npcs {
        let here = liveness::agent_position(ledger, npc, t);
        let seen = folds
            .borrow_mut()
            .latest_visit(ledger)
            .water_at(npc.entity, t, terrain);
        for room in seen {
            let _ = plan_to_room(
                &here,
                &room,
                PLAN_BUDGET_MIRROR,
                &std::collections::BTreeSet::new(),
            );
            calls += 1;
        }
    }
    SweepCounts {
        calls,
        occurrences,
        distinct_pairs: pairs.len(),
        non_empty,
        max_set,
    }
}

/// The possession shape's committed ledger, every body the session derived,
/// and the instant the walk stopped at — everything [`culvert_sweep_counts`]
/// and [`culvert_real_pairs`] need for [`Shape::Possession`].
///
/// The same walk [`culvert_water_ledger_hash`] hashes
/// ([`CULVERT_WATER_SEED`], [`CULVERT_WATER_WAITS`]), reached independently
/// here rather than through `resident_folds.rs`'s `kerf_possession_ledger`,
/// for the reason [`culvert_water_ledger_hash`]'s own doc already gives: that
/// function is private to its file, and this module has no reason to depend
/// on `resident_folds.rs`.
fn culvert_possession_shape() -> (Ledger, Vec<Body>, WorldTime, hornvale_locale::LocaleContext) {
    let world = common::build(CULVERT_WATER_SEED).expect("the water-belief seed builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the water-belief seed starts a session");
    for _ in 0..CULVERT_WATER_WAITS {
        session.handle("wait");
    }
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let t = session.day();
    let npcs = session.bodies().to_vec();
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("the locale context builds");
    (ledger, npcs, t, ctx)
}

/// Which measured shape a pair population comes from. The two seed-42
/// variants below are NOT the same shape and it cost this task a wasted
/// round trip to learn that, so it is written down here rather than left to
/// be rediscovered: `resident_folds.rs`'s `bench_shape(KERF_LAB_SEED,
/// KERF_LAB_TICKS, KERF_LAB_AGENTS)` = `bench_shape(42, 10, 50)` is The
/// Kerf's own SHORT lab shape (10 ticks); `session_length_scaling.rs`'s own
/// construction runs the SAME seed and roster size for 200 ticks and reads
/// its state at band 10 of 10 (`BAND == 20`) — a much larger, much more
/// expensive walk that happens to share a seed with the first.
///
/// Task 6 needs BOTH ARMS of `plan_to_room` to fire (a reachable pair and an
/// unreachable one) and nothing more specific than that — the possession
/// shape alone never exercises the `None` arm (it has zero unreachable
/// pairs), so at least one lab-shape reading is required. Which lab
/// reading is a COST decision, not a correctness one: [`Shape::Lab`] (the
/// cheap 10-tick shape) already has an unreachable pair, so it is the one
/// every ordinary run (including Task 6's) should use.
/// [`Shape::LabAt200Ticks`] exists only as a fallback and a cost record —
/// see its own doc.
enum Shape {
    /// Seed 17, `Session::start` + 12 waits — 52 of 67 residents hold a
    /// non-empty belief, max 23 rooms, no unreachable pair.
    Possession,
    /// The Kerf's own lab shape — `resident_folds.rs`'s
    /// `bench_shape(KERF_LAB_SEED, KERF_LAB_TICKS, KERF_LAB_AGENTS)` =
    /// `bench_shape(42, 10, 50)`, ten ticks, not two hundred. Measured: 9 of
    /// 50 residents hold a non-empty belief (largest 4 rooms), 27 distinct
    /// `(home, dest)` pairs, of which 4 are unreachable within
    /// `PLAN_BUDGET_MIRROR` — enough to exercise both of `plan_to_room`'s
    /// arms at a small fraction of [`Shape::LabAt200Ticks`]'s cost. This is
    /// the variant every correctness test (Task 6 included) should use.
    Lab,
    /// The SAME seed-42, 50-agent construction as [`Shape::Lab`] (both go
    /// through `the_detent::bench_shape`), run the FULL 200 ticks
    /// `session_length_scaling.rs` itself runs before reading its own band
    /// 10 (`BAND == 20`, the 10th and final band) — not `resident_folds.rs`'s
    /// cheaper 10-tick shape [`Shape::Lab`] reads. Measured: 11 of 50
    /// non-empty, max 46 rooms, 83 distinct pairs, 55 unreachable — the exact
    /// figures Task 2's report table cites for band 10.
    ///
    /// **Not used by any unignored test.** `bench_shape(42, 200, 50)` costs
    /// 129.337 s on a quiet box under an optimized test profile — measured
    /// directly on
    /// [`the_expensive_200_tick_lab_shape_also_has_an_unreachable_pair`],
    /// which is `#[ignore]`d for exactly that cost. [`Shape::Lab`]'s cheap
    /// 10-tick reading already satisfies the correctness property every
    /// consumer of `culvert_real_pairs` needs (both of `plan_to_room`'s arms
    /// fire); this variant is kept, not deleted, as the fallback population
    /// if four unreachable pairs ever proves too thin a population for some
    /// future test.
    LabAt200Ticks,
}

/// `liveness::PLAN_BUDGET` is a private const an integration test cannot
/// import, so it is mirrored here — the third such mirror, beside
/// `session_length_scaling`'s `PROBE_BUDGET` and `nav_bench`'s `BUDGET`. All
/// four are kept in sync BY HAND and nothing enforces it; a test asserting
/// agreement is impossible without making `PLAN_BUDGET` public.
const PLAN_BUDGET_MIRROR: usize = 1_000;

/// Every distinct `(home, water room)` pair a roster-wide `believed_water`
/// sweep implies on `shape`, in ascending `(from, dest)` order (a
/// `BTreeSet<(Facet, Facet)>`'s own iteration order, since [`Facet`]
/// derives `Ord`) so the population Task 6 draws from is reproducible.
fn culvert_real_pairs(shape: Shape) -> Vec<(Facet, Facet)> {
    fn pairs_from(
        ledger: &Ledger,
        folds: &OwnedFolds,
        npcs: &[Body],
        t: WorldTime,
        terrain: &dyn liveness::Terrain,
    ) -> Vec<(Facet, Facet)> {
        let mut store = folds.borrow_mut();
        let latest_visit = store.latest_visit(ledger);
        let mut pairs: std::collections::BTreeSet<(Facet, Facet)> =
            std::collections::BTreeSet::new();
        for npc in npcs {
            for room in latest_visit.water_at(npc.entity, t, terrain) {
                pairs.insert((npc.home.clone(), room));
            }
        }
        pairs.into_iter().collect()
    }

    match shape {
        Shape::Possession => {
            let (ledger, npcs, t, ctx) = culvert_possession_shape();
            let terrain = liveness::LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
            let folds = OwnedFolds::new(ResidentFolds::new());
            pairs_from(&ledger, &folds, &npcs, t, &terrain)
        }
        Shape::Lab => {
            // The Kerf's own 10-tick lab shape (`resident_folds.rs`'s
            // KERF_LAB_SEED/KERF_LAB_TICKS/KERF_LAB_AGENTS = 42, 10, 50),
            // repeated here as literals rather than shared constants (each
            // test module keeps its own copy of a shape it did not author,
            // the same convention this module's possession constants already
            // follow) — see [`Shape::Lab`]'s own doc for why this, and not
            // [`Shape::LabAt200Ticks`], is the correctness-testing default.
            let lab = crate::the_detent::bench_shape(42, 10, 50);
            let mesh = lab.mesh_memo.clone();
            let terrain =
                liveness::LocaleTerrain::with_fields(&lab.ctx, None, None, None, None, Some(&mesh));
            pairs_from(&lab.ledger, &lab.folds, &lab.npcs, lab.day, &terrain)
        }
        Shape::LabAt200Ticks => {
            // The identical construction as `Shape::Lab` above
            // (`the_detent::bench_shape`, seed 42, 50 agents), run the full
            // 200 ticks `session_length_scaling.rs` itself runs before
            // reading its own band 10 (`BAND == 20`) — see [`Shape::
            // LabAt200Ticks`]'s own doc for the cost this pays (129.337 s
            // measured) and why nothing unignored uses it.
            let lab = crate::the_detent::bench_shape(42, 200, 50);
            let mesh = lab.mesh_memo.clone();
            let terrain =
                liveness::LocaleTerrain::with_fields(&lab.ctx, None, None, None, None, Some(&mesh));
            pairs_from(&lab.ledger, &lab.folds, &lab.npcs, lab.day, &terrain)
        }
    }
}

/// **THE SWEEP-COUNT WITNESS — the campaign's result, measured.** One
/// roster-wide `believed_water` sweep over the possession shape (seed 17, 12
/// waits) asked 529 route questions over 83 distinct `(home, dest)` pairs on
/// the pre-memo tree, and paid one budgeted search per QUESTION. Wired to
/// [`liveness::RouteMemo`] (Task 7) it pays one per PAIR, which is what the
/// equality below states.
///
/// **It is not asserted against a magic number, and it is not vacuous.**
/// `calls` comes off the memo's own `searches()` counter after a real sweep;
/// `distinct_pairs` comes off `LatestVisit` with no memo involved. Their
/// equality is therefore two independent derivations agreeing, and
/// `occurrences > distinct_pairs` is checked FIRST so a shape with no
/// duplicate pairs — where the equality would hold trivially and prove
/// nothing — reddens instead of passing.
///
/// **This test was `#[ignore]`d and RED through Task 6** (529 calls, 83
/// pairs). See [`SweepCounts::calls`] for what had to change in the
/// instrument before it could go green, and why the old derivation could
/// never have.
#[test]
fn culvert_sweep_collapses_calls_onto_distinct_pairs() {
    let (ledger, npcs, t, ctx) = culvert_possession_shape();
    let terrain = liveness::LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
    let folds = OwnedFolds::new(ResidentFolds::new());
    let counts = culvert_sweep_counts(&ledger, &folds, &npcs, t, &terrain);
    println!(
        "--- culvert sweep counts (possession shape, seed {CULVERT_WATER_SEED}, \
         {CULVERT_WATER_WAITS} waits) ---"
    );
    println!(
        "calls {}, occurrences {}, distinct_pairs {}, non_empty {} of {} residents, max_set {}",
        counts.calls,
        counts.occurrences,
        counts.distinct_pairs,
        counts.non_empty,
        npcs.len(),
        counts.max_set
    );
    assert!(
        counts.non_empty > 0,
        "denominator: no resident holds a water belief on this shape"
    );
    assert!(
        counts.occurrences > 0,
        "denominator: the sweep asked for no routes at all"
    );
    assert!(
        counts.occurrences > counts.distinct_pairs,
        "NON-VACUITY: this shape asked {} route questions over {} distinct pairs, so there \
         is nothing for a memo to collapse and the equality below would prove nothing. \
         Before The Culvert this shape measured 529 occurrences over 83 pairs.",
        counts.occurrences,
        counts.distinct_pairs
    );
    assert_eq!(
        counts.calls, counts.occurrences,
        "the actor-relative believed_water sweep ran {} direct plan_to_room searches over {} \
         distinct (here, dest) pairs and {} occurrences",
        counts.calls, counts.distinct_pairs, counts.occurrences
    );
}

/// Measure the key population that a cache for the current-relative
/// `believed_water` fold would actually need. This is deliberately a
/// diagnostic rather than a correctness guard: a cache decision must use the
/// curve, not an endpoint or a guessed capacity.
#[test]
#[ignore = "probe: route-cache current-relative key curve"]
fn route_cache_probe_reports_current_key_population_curve() {
    let world = common::build(CULVERT_WATER_SEED).expect("the route-cache seed builds");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the route-cache seed starts a session");
    let mut checkpoints = Vec::with_capacity(CULVERT_WATER_WAITS);
    for _ in 0..CULVERT_WATER_WAITS {
        session.handle("wait");
        checkpoints.push(session.day());
    }

    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session ledger round-trips");
    let npcs = session.bodies().to_vec();
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("the locale context builds");
    let terrain = liveness::LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
    let folds = OwnedFolds::new(ResidentFolds::new());
    let mut cumulative = std::collections::BTreeSet::new();

    println!(
        "--- route-cache current-key curve (seed {CULVERT_WATER_SEED}, {CULVERT_WATER_WAITS} waits) ---"
    );
    println!("{:>4} {:>16} {:>16}", "wait", "asked", "current_dest_cum");
    for (wait, &t) in checkpoints.iter().enumerate() {
        let mut asked = 0usize;
        let mut store = folds.borrow_mut();
        let latest_visit = store.latest_visit(&ledger);
        for npc in &npcs {
            let here = liveness::agent_position(&ledger, npc, t);
            for dest in latest_visit.water_at(npc.entity, t, &terrain) {
                asked += 1;
                cumulative.insert((here.clone(), dest));
            }
        }
        drop(store);
        println!("{:>4} {:>16} {:>16}", wait + 1, asked, cumulative.len());
    }

    assert!(
        !cumulative.is_empty(),
        "the diagnostic needs a non-empty key population"
    );
}

/// **The three Task-6 helpers, exercised directly.** `culvert_real_pairs`
/// must yield a non-empty population on both shapes, or Task 6's equivalence
/// test has nothing to compare; and [`Shape::Lab`] (the cheap 10-tick shape,
/// NOT [`Shape::LabAt200Ticks`] — see the enum's own doc) specifically must
/// yield BOTH at least one pair `plan_to_room` reaches and at least one it
/// cannot reach within [`PLAN_BUDGET_MIRROR`], so Task 6's equivalence test
/// (which needs both of `plan_to_room`'s arms to fire) has a real population
/// to draw from.
///
/// **This asserts the PROPERTY (both arms fire), never an exact count.**
/// An earlier round of this task specified the population by its NUMBERS
/// ("55 of 83 unreachable") rather than by what it needs to be true of —
/// which is what pointed this test at the 200-tick shape and its 129 s cost
/// in the first place, for no correctness benefit `Shape::Lab`'s much
/// cheaper 27-pair/4-unreachable population doesn't already provide. Pinning
/// either count here would recreate exactly that mistake: a population that
/// shifts by one member should not redden a correctness test.
#[test]
fn culvert_real_pairs_span_both_shapes_and_lab_has_an_unreachable_pair() {
    fn reachable_and_unreachable(pairs: &[(Facet, Facet)]) -> (usize, usize) {
        let mut reachable = 0usize;
        let mut unreachable = 0usize;
        for (home, dest) in pairs {
            match plan_to_room(
                home,
                dest,
                PLAN_BUDGET_MIRROR,
                &std::collections::BTreeSet::new(),
            ) {
                Some(_) => reachable += 1,
                None => unreachable += 1,
            }
        }
        (reachable, unreachable)
    }

    let possession = culvert_real_pairs(Shape::Possession);
    assert!(
        !possession.is_empty(),
        "denominator: the possession shape implies no (home, dest) pairs"
    );

    let lab = culvert_real_pairs(Shape::Lab);
    assert!(
        !lab.is_empty(),
        "denominator: the lab shape implies no (home, dest) pairs"
    );

    let (lab_reachable, lab_unreachable) = reachable_and_unreachable(&lab);
    println!(
        "culvert_real_pairs: possession {} pairs, lab {} pairs ({lab_reachable} reachable, \
         {lab_unreachable} unreachable within PLAN_BUDGET_MIRROR={PLAN_BUDGET_MIRROR})",
        possession.len(),
        lab.len()
    );
    assert!(
        lab_reachable > 0,
        "Shape::Lab must yield at least one (home, dest) pair plan_to_room CAN reach within \
         PLAN_BUDGET_MIRROR ({PLAN_BUDGET_MIRROR}), or Task 6's Some arm is untestable; found \
         0 of {} lab pairs reachable",
        lab.len()
    );
    assert!(
        lab_unreachable > 0,
        "Shape::Lab must yield at least one (home, dest) pair plan_to_room CANNOT reach \
         within PLAN_BUDGET_MIRROR ({PLAN_BUDGET_MIRROR}), or Task 6's None arm is \
         untestable; found 0 of {} lab pairs unreachable",
        lab.len()
    );
}

/// The SAME correctness property as
/// [`culvert_real_pairs_span_both_shapes_and_lab_has_an_unreachable_pair`],
/// read over [`Shape::LabAt200Ticks`] instead of the cheap [`Shape::Lab`] —
/// kept as a fallback population and a standing cost record, not run at
/// commit or stage-gate cadence. See [`Shape::LabAt200Ticks`]'s own doc for
/// why it exists rather than being deleted.
#[test]
#[ignore = "129.337s measured (a quiet box, 1-min load 4.09, optimized test profile) -- the \
            cheap Shape::Lab (resident_folds.rs's 10-tick bench_shape) already exercises both \
            of plan_to_room's arms at a fraction of this cost, so this variant is not run at \
            commit or stage-gate cadence; kept as the fallback population per docs/superpowers/\
            plans/2026-09-05-the-culvert.md, Task 4 fix round 1"]
fn the_expensive_200_tick_lab_shape_also_has_an_unreachable_pair() {
    let lab = culvert_real_pairs(Shape::LabAt200Ticks);
    assert!(
        !lab.is_empty(),
        "denominator: the 200-tick lab shape implies no (home, dest) pairs"
    );
    let unreachable = lab
        .iter()
        .filter(|(home, dest)| {
            plan_to_room(
                home,
                dest,
                PLAN_BUDGET_MIRROR,
                &std::collections::BTreeSet::new(),
            )
            .is_none()
        })
        .count();
    println!(
        "Shape::LabAt200Ticks: {} pairs, {unreachable} unreachable within \
         PLAN_BUDGET_MIRROR={PLAN_BUDGET_MIRROR}",
        lab.len()
    );
    assert!(
        unreachable > 0,
        "Shape::LabAt200Ticks must yield at least one unreachable pair (measured: 55 of 83); \
         found {unreachable} of {} pairs unreachable",
        lab.len()
    );
}

// ---------------------------------------------------------------------------
// Task 5: the moving-anchor key population -- measured, not assumed.
//
// `shared_believed_water` (liveness.rs:1880) anchors its ranking at `here` --
// the npc's CURRENT position, which moves every tick -- rather than at
// `home` (fixed for a session). Its memo key space is therefore
// `positions x water rooms`, not `homes x water rooms`, which is the one way
// this campaign's home-anchored bound (83 pairs, believed at the time to be
// saturating -- Task 4) could
// fail to cover a real long-running session. That belief is what this very
// task went on to refute, for BOTH curves: see the verdict doc below, and
// spec §1.3(d). The premise is stated as it stood when the task was framed,
// because the task only makes sense read that way.
// Spec Rule 3 asks whether the
// `(here, dest)` population SATURATES (stops rising -- safe to memoize) or
// KEEPS RISING (unbounded -- exclude the site), a question about the CURVE's
// shape, never about comparing an endpoint to 83.
// ---------------------------------------------------------------------------

/// The distinct `(here, dest)` pairs one roster-wide `shared_believed_water`
/// sweep over `npcs` at `t` implies, and how many `npcs` members had at least
/// one co-located peer -- mirroring `shared_believed_water`'s own logic
/// exactly (liveness.rs:1880-1896) rather than instrumenting it: an npc with
/// NO co-located peer contributes NOTHING here (its `own` suggestion is
/// ranked at `home` via `believed_water`'s internal `plan_to_room` call,
/// already counted by [`culvert_sweep_counts`]'s `(home, dest)` population --
/// this is the byte-identical alone-path no-op `shared_believed_water`'s own
/// doc describes). A co-located npc pools its own and every peer's
/// `believed_water` suggestion and queries `plan_to_room` from ITS OWN
/// current position once per pooled room -- exactly the calls this
/// diagnostic counts as `(here, room)` pairs. The co-located count is
/// returned alongside the pairs so an empty population can be told apart, at
/// a glance, from "nobody was ever co-located" (the alone-path never reaches
/// a `(here, dest)` call at all) versus "co-located, but the pool was empty"
/// (a co-located npc whose own and every peer's `believed_water` is `None`).
fn culvert_here_dest_pairs(
    ledger: &Ledger,
    folds: &OwnedFolds,
    npcs: &[Body],
    t: WorldTime,
    terrain: &dyn liveness::Terrain,
    budget: usize,
) -> (std::collections::BTreeSet<(Facet, Facet)>, usize) {
    let mut pairs = std::collections::BTreeSet::new();
    let mut co_located = 0usize;
    // ONE memo for the whole sweep (The Culvert, Task 7), mirroring how
    // `shared_believed_water` threads the session's own memo into
    // `believed_water`. It changes no answer here — the memo is a cache of a
    // pure function — only how many searches this diagnostic pays.
    let mut route_memo = liveness::RouteMemo::new();
    for npc in npcs {
        let here = liveness::agent_position(ledger, npc, t);
        let mut pool: std::collections::BTreeSet<Facet> = std::collections::BTreeSet::new();
        let mut has_peer = false;
        for other in npcs {
            if other.entity != npc.entity && liveness::agent_position(ledger, other, t) == here {
                has_peer = true;
                if let Some(w) = liveness::believed_water(
                    ledger,
                    folds,
                    other,
                    t,
                    terrain,
                    budget,
                    &mut route_memo,
                ) {
                    pool.insert(w);
                }
            }
        }
        if !has_peer {
            continue;
        }
        co_located += 1;
        if let Some(w) =
            liveness::believed_water(ledger, folds, npc, t, terrain, budget, &mut route_memo)
        {
            pool.insert(w);
        }
        for room in pool {
            pairs.insert((here.clone(), room));
        }
    }
    (pairs, co_located)
}

/// **THE MOVING-ANCHOR KEY-POPULATION CURVE (Task 5, controller ruling R10).**
///
/// Prints, and does not gate on, the cumulative distinct `(here, dest)`
/// population next to the cumulative `(home, dest)` population, per wait
/// (possession shape, seed 17, extended past Task 4's 12 waits to 60 -- cheap)
/// and per 20-tick band (lab shape, seed 42, 50 agents, 200 ticks -- the SAME
/// construction [`Shape::LabAt200Ticks`] pays 129.337s for, read at ten bands
/// off the ONE resulting ledger rather than re-run per band: `water_at`'s
/// `day <= t` filter over an already-complete history means every earlier
/// band's population is a cheap read over the same finished ledger, exactly
/// how `session_length_scaling.rs`'s own per-band table is produced).
///
/// The `(home, dest)` column is genuinely CUMULATIVE by construction, with no
/// running union needed: `LatestVisit::water_at(entity, t, ...)` is
/// monotonically non-decreasing in `t` (a room qualifies once its FIRST visit
/// is `<= t`, and stays qualified for every larger `t` -- see its own doc), so
/// a fresh [`culvert_sweep_counts`] read at each checkpoint's `t` already IS
/// the full population as of that checkpoint. `here`, by contrast, is NOT
/// monotonic -- it is the npc's CURRENT position, which can revisit an
/// earlier room or move away from one -- so the `(here, dest)` column is a
/// running union of every checkpoint's own [`culvert_here_dest_pairs`] output,
/// which is what a real memo over the creature's whole session would
/// accumulate.
///
/// This is a measurement, never a guard: the one `assert_eq!` in its body
/// only checks this test's OWN band-to-tick arithmetic against
/// `bench_shape`'s independently-computed final day, never the populations
/// themselves. Spec Rule 3's saturates/keeps-rising/cannot-decide verdict is
/// read off the PRINTED curve by a human (recorded in this campaign's Task 5
/// report and ledger entry #14), not computed by this test.
///
/// **THE DATED RECORD (2026-09-05, this campaign's own Task 5; reframed under
/// controller ruling R11).** On the lab shape, `colocated` was `0` and
/// `here_dest_cum` was `0` at all ten bands -- confirming The Kerf's own
/// prior finding on this exact construction (its ledger
/// `2026-09-04-the-kerf.md`: "false at all ten bands, zero co-located peers")
/// on a SECOND, independent instrument. The `home_dest_cum` column reproduced
/// Task 2's own per-band table digit for digit (37, 56, 61, 65, 67, 71, 77,
/// 79, 83, 83), a cross-validation this diagnostic did not need but got for
/// free.
///
/// On the possession shape (the only shape where `colocated` is ever
/// non-zero -- consistently 52-63 of 67 members every wait), `home_dest_cum`
/// reaches a flat stretch by the end of the run (187, 189, 190, 190, 190
/// across waits 56-60) -- the same flattening shape Task 4's 83 was read as
/// establishing, reproduced here on a second, longer-running shape. (Read as
/// establishing; NOT established. Three flat waits is a pause, not a ceiling,
/// which is what the rest of this doc goes on to argue.) But
/// `here_dest_cum` ALSO plateaus mid-run -- five consecutive equal reads (41)
/// across waits 21-25, LONGER than the reference's own three-wait plateau --
/// and then resumes climbing. So a plateau in this system does not imply a
/// ceiling for EITHER curve: the here-curve has an observed history of
/// stalling and resuming, which means the home-curve's final three flat
/// waits are not proven permanent either. Neither curve ran long enough to
/// prove it has stopped. The genuine signal is the tail: over waits 58-60,
/// `home_dest_cum` adds `+0, +0` (190, 190, 190) while `here_dest_cum` adds
/// `+1, +2` (98, 99, 101) -- a real but thin difference, not a clean
/// separation. **Verdict: EXCLUDE, resting on Rule 3's conservative default
/// under genuine uncertainty (ruling R12), not on a clean measured
/// saturation/non-saturation split** -- see the Task 5 report and ledger
/// entry #14 for the full per-wait/per-band tables and reasoning.
#[test]
#[ignore = "diagnostic, run once by hand: 1077.86s measured (a quiet box, optimized test \
            profile) -- the lab-shape half reuses Shape::LabAt200Ticks's own 129.337s \
            construction, but the possession-shape half's 60 independent roster-wide \
            culvert_here_dest_pairs sweeps (each re-running believed_water's budgeted \
            plan_to_room over a co-located roster of ~55-63 of 67 members) dominate the wall \
            clock; nothing here gates a commit or a stage gate -- it exists to feed spec Rule \
            3's saturates/keeps-rising/cannot-decide verdict by hand, per \
            docs/superpowers/plans/2026-09-05-the-culvert.md Task 5 (controller rulings \
            R10-R12)"]
fn culvert_here_anchored_key_population_curve() {
    // --- Possession shape: seed 17, extended to 60 waits (cheap). ---
    const EXTENDED_WAITS: usize = 60;
    let world = common::build(CULVERT_WATER_SEED).expect("the water-belief seed builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the water-belief seed starts a session");
    let mut checkpoints: Vec<WorldTime> = Vec::with_capacity(EXTENDED_WAITS);
    for _ in 0..EXTENDED_WAITS {
        session.handle("wait");
        checkpoints.push(session.day());
    }
    let ledger: Ledger = serde_json::from_str(&session.session_ledger_json())
        .expect("the session's own ledger accessor round-trips");
    let npcs = session.bodies().to_vec();
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("the locale context builds");
    let terrain = liveness::LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
    let folds = OwnedFolds::new(ResidentFolds::new());

    println!(
        "--- Task 5: moving-anchor key population (possession shape, seed \
         {CULVERT_WATER_SEED}, {EXTENDED_WAITS} waits) ---"
    );
    println!(
        "{:>4} {:>16} {:>16} {:>10}",
        "wait", "home_dest_cum", "here_dest_cum", "colocated"
    );
    let mut here_running: std::collections::BTreeSet<(Facet, Facet)> =
        std::collections::BTreeSet::new();
    for (i, &t) in checkpoints.iter().enumerate() {
        let home_counts = culvert_sweep_counts(&ledger, &folds, &npcs, t, &terrain);
        let (here_pairs, co_located) =
            culvert_here_dest_pairs(&ledger, &folds, &npcs, t, &terrain, PLAN_BUDGET_MIRROR);
        here_running.extend(here_pairs);
        println!(
            "{:>4} {:>16} {:>16} {:>10}",
            i + 1,
            home_counts.distinct_pairs,
            here_running.len(),
            co_located
        );
    }

    // --- Lab shape: seed 42, 50 agents, Shape::LabAt200Ticks's own
    // construction, read at ten 20-tick bands off the one finished ledger. ---
    let lab = crate::the_detent::bench_shape(42, 200, 50);
    let mesh = lab.mesh_memo.clone();
    let lab_terrain =
        liveness::LocaleTerrain::with_fields(&lab.ctx, None, None, None, None, Some(&mesh));
    let day0 = WorldTime::from_std_days(0.5).expect("0.5 is a finite day count");

    println!(
        "--- Task 5: moving-anchor key population (lab shape, seed 42, 50 agents, \
         200 ticks in bands of 20) ---"
    );
    println!(
        "{:>5} {:>16} {:>16} {:>10}",
        "band", "home_dest_cum", "here_dest_cum", "colocated"
    );
    let mut lab_here_running: std::collections::BTreeSet<(Facet, Facet)> =
        std::collections::BTreeSet::new();
    for band in 1..=10usize {
        let ticks = (band * 20) as i64;
        let t = WorldTime::from_ticks(day0.ticks() + ticks * WorldTime::TICKS_PER_STD_DAY);
        let home_counts = culvert_sweep_counts(&lab.ledger, &lab.folds, &lab.npcs, t, &lab_terrain);
        let (here_pairs, co_located) = culvert_here_dest_pairs(
            &lab.ledger,
            &lab.folds,
            &lab.npcs,
            t,
            &lab_terrain,
            PLAN_BUDGET_MIRROR,
        );
        lab_here_running.extend(here_pairs);
        println!(
            "{:>5} {:>16} {:>16} {:>10}",
            band,
            home_counts.distinct_pairs,
            lab_here_running.len(),
            co_located
        );
    }
    assert_eq!(
        WorldTime::from_ticks(day0.ticks() + 200 * WorldTime::TICKS_PER_STD_DAY),
        lab.day,
        "band 10's reconstructed WorldTime must equal bench_shape's own final day, or the \
         band-to-tick arithmetic above has drifted from bench_shape's own loop"
    );
}

/// **THE PRIMARY WITNESS (Task 6).** For every `(from, dest, budget)` the memo
/// is asked, its answer equals a fresh
/// `plan_to_room(from, dest, budget, ∅)` — **including when both are `None`**.
///
/// This is deliberately stronger than a ledger hash, on The Kerf's own
/// finding: its control B moved none of four script hashes yet reddened all
/// four real-shape FOLD-equals-SCAN sweeps, so a hash is the weaker
/// instrument. The campaign's byte-identity claim is that
/// `plan_to_room(from, dest, budget, ∅)` is pure over mesh geometry
/// (`NavSpace` holds only `dest` and `avoid` and never reads `Terrain`, the
/// ledger, or the tick); this test is what makes that claim falsifiable
/// rather than merely argued.
///
/// The `None` case is not an afterthought: 59.5% of real calls are
/// budget-exhausted failures and they are 95.1% of all node expansions, so a
/// memo that stored only successes would re-pay the worst calls forever while
/// its hit rate read 87.8%.
///
/// **The population unions both shapes, and is DEDUPED.** The possession
/// shape has no unreachable pair and [`Shape::Lab`] does, so neither alone
/// exercises both arms; the two shapes are different seeds and nothing
/// guarantees their `(from, dest)` pairs are disjoint, so the union is
/// collected through a `BTreeSet` before it is measured. `searches` and
/// `len()` are then compared against the DISTINCT pair count, which is the
/// property actually claimed ("one search per distinct key") rather than an
/// accident of how the two populations were concatenated.
///
/// **Both arms are asserted as PROPERTIES, never as counts** — the same
/// discipline
/// [`culvert_real_pairs_span_both_shapes_and_lab_has_an_unreachable_pair`]
/// already states in its own doc: a population that shifts by one member must
/// not redden a correctness test.
///
/// **THE STRONGEST THING THIS TEST DEMONSTRATES IS NOT IN ITS ASSERTIONS, so
/// it is written here** (Task 6, fix round 1 — the reviewer's finding, and
/// neither the implementer nor the controller had noticed it). The population
/// unions pairs from TWO DIFFERENT WORLDS — [`Shape::Possession`] is seed 17
/// and [`Shape::Lab`] is seed 42 — into ONE memo, and every answer still
/// matches a fresh search. That would be UNSOUND if `plan_to_room` had any
/// world input at all: a memo shared across two worlds would hand seed 42's
/// answer to a seed 17 question the moment their key spaces met. It passes
/// because the search is pure over mesh geometry, which is exactly the
/// campaign's byte-identity premise. Read this test as evidence for that
/// premise and not only for the memo's bookkeeping — the union is load-bearing
/// twice over (both arms of `plan_to_room`, AND cross-world purity), and
/// collapsing it back to one shape would silently discard the second.
#[test]
fn the_memo_answers_exactly_what_a_fresh_search_answers() {
    // Real (home, water room) pairs from the two measured shapes — NOT
    // synthetic facets. The property must hold on the pairs production
    // actually asks about, including the unreachable ones.
    let mut union: std::collections::BTreeSet<(Facet, Facet)> =
        culvert_real_pairs(Shape::Possession).into_iter().collect();
    union.extend(culvert_real_pairs(Shape::Lab));
    let pairs: Vec<(Facet, Facet)> = union.into_iter().collect();
    assert!(!pairs.is_empty(), "denominator: no pairs to compare");

    let mut memo = liveness::RouteMemo::new();
    assert!(memo.is_empty(), "a fresh memo holds nothing");
    let mut reached = 0usize;
    let mut unreachable = 0usize;
    for (from, dest) in &pairs {
        let fresh = plan_to_room(
            from,
            dest,
            PLAN_BUDGET_MIRROR,
            &std::collections::BTreeSet::new(),
        )
        .map(|p| p.len());
        let missed = memo.hops(from, dest, PLAN_BUDGET_MIRROR);
        assert_eq!(
            missed, fresh,
            "the memo's MISS disagreed with a fresh search for {from:?} -> {dest:?}"
        );
        // And again, to exercise the HIT path, not only the miss path.
        assert_eq!(
            memo.hops(from, dest, PLAN_BUDGET_MIRROR),
            fresh,
            "the memo's HIT disagreed with its own miss for {from:?} -> {dest:?}"
        );
        match fresh {
            Some(_) => reached += 1,
            None => unreachable += 1,
        }
    }
    println!(
        "--- the memo vs a fresh search: {} distinct pairs, {reached} reachable, \
         {unreachable} unreachable within PLAN_BUDGET_MIRROR={PLAN_BUDGET_MIRROR}; \
         searches {}, entries {} ---",
        pairs.len(),
        memo.searches(),
        memo.len()
    );
    // Both arms must fire or the test is half a test.
    assert!(
        reached > 0,
        "no reachable pair in the population — the Some arm is untested"
    );
    assert!(
        unreachable > 0,
        "no unreachable pair in the population — the None arm, which is 95.1% of the real \
         cost, is untested"
    );
    // A miss inserts exactly one entry, so any divergence here means the memo
    // re-searched a key it already held, or holds a key it never searched.
    assert_eq!(
        memo.searches() as usize,
        pairs.len(),
        "one real search per distinct pair and no more: the second ask for each pair must \
         have hit"
    );
    assert_eq!(
        memo.len(),
        pairs.len(),
        "entries == searches: every search inserted exactly one entry"
    );
}

/// **THE KEY-COMPONENT WITNESS (Task 6, fix round 1 — Finding A).** `budget`
/// is part of [`liveness::RouteMemo`]'s key, and
/// [`the_memo_answers_exactly_what_a_fresh_search_answers`] cannot see it:
/// every ask there passes the same [`PLAN_BUDGET_MIRROR`], so a memo that
/// DROPPED `budget` from its key would pass that test unchanged. An untested
/// key component is one nobody knows is there, and this campaign has already
/// met the cannot-fire pattern twice (a roster guard that only detected
/// removals; a population spec that pinned counts).
///
/// The falsifier is the smallest one that discriminates: take a pair the
/// search CAN reach, ask it at `budget = 1` — one expansion cannot cross a
/// multi-hop route, so the answer must be `None` and must be CACHED as `None`
/// (the negative-caching half of the type's own contract) — then ask the SAME
/// `(from, dest)` at the real budget and require `Some`. If `budget` is not
/// really in the key, the second ask returns the first's cached failure and
/// this reddens.
///
/// **Demonstrated to fail without the key component** (fix round 1 evidence,
/// not merely asserted): with `budget` dropped from the key in
/// `RouteMemo::hops`, this test failed on
/// `the second ask must return the memo's own Some(...)` with `None`, while
/// [`the_memo_answers_exactly_what_a_fresh_search_answers`] stayed GREEN —
/// which is the whole point of the round.
#[test]
fn the_memo_keys_on_budget_not_only_on_the_room_pair() {
    let empty = std::collections::BTreeSet::new();
    // A pair the search reaches in MORE than one hop — a one-hop pair would
    // make `budget = 1` a `Some` too and the probe would discriminate nothing.
    let (from, dest, hops) = culvert_real_pairs(Shape::Lab)
        .into_iter()
        .find_map(|(from, dest)| {
            plan_to_room(&from, &dest, PLAN_BUDGET_MIRROR, &empty)
                .filter(|p| p.len() > 1)
                .map(|p| (from, dest, p.len()))
        })
        .expect(
            "denominator: Shape::Lab must contain a pair reachable in more than one hop, or \
             this probe discriminates nothing",
        );

    let mut memo = liveness::RouteMemo::new();
    // A budget of one expansion cannot cross a route of `hops` > 1 hops.
    let starved = memo.hops(&from, &dest, 1);
    assert_eq!(
        starved, None,
        "a budget of 1 must not reach {from:?} -> {dest:?}, which is {hops} hops away — the \
         probe's own premise"
    );
    assert_eq!(
        memo.len(),
        1,
        "the starved ask must have CACHED its failure"
    );

    // The same room pair, a different budget: a distinct key, so a fresh
    // search, so the real answer — never the cached None above.
    let fed = memo.hops(&from, &dest, PLAN_BUDGET_MIRROR);
    assert_eq!(
        fed,
        Some(hops),
        "the second ask must return the memo's own Some({hops}) for {from:?} -> {dest:?}. A \
         None here means `budget` is not in RouteMemo's key and the starved ask's cached \
         failure was served to a question it does not answer."
    );
    assert_eq!(
        memo.searches(),
        2,
        "two distinct budgets over one room pair are two distinct keys, so two real searches"
    );
    assert_eq!(
        memo.len(),
        2,
        "entries == searches across the budget axis too"
    );
}
