//! The Culvert — the water belief's route memo, and the instruments that
//! measure it.
//!
//! Spec: `docs/superpowers/specs/2026-09-05-the-culvert-design.md`.
//!
//! # THE DATED RECORD (Task 3: the campaign-time hash constants and their
//! positive control)
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
//! **Retirement.** Per decision 0541 and the spec's Rule 6, these two
//! constants retire at this campaign's close, with this record kept as
//! history. Re-record main-first after every absorption that touches
//! `windows/vessel/src/resident.rs`'s `LatestVisit`, or
//! `windows/vessel/src/liveness.rs`'s `believed_water`, `shared_believed_water`
//! or `nearer_to_home`.

use crate::common;
use hornvale_vessel::liveness;
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
fn culvert_seed_42_ledger_hash() -> u64 {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");
    crate::ledger_hash_witness::run_fixed_script(&mut session);
    crate::ledger_hash_witness::fnv1a(session.session_ledger_json().as_bytes())
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

/// The belief-rich possession shape: seed 17, twelve `wait`s, no `look` —
/// `resident_folds.rs`'s `kerf_possession_ledger` walk, reached independently
/// here (that function is private to its own file) rather than reused, since
/// this module has no reason to depend on `resident_folds.rs`.
fn culvert_water_ledger_hash() -> u64 {
    let world = common::build(CULVERT_WATER_SEED).expect("the water-belief seed builds a world");
    let (mut session, _opening) = Session::start(&world, &PossessOpts::default())
        .expect("the water-belief seed starts a session");
    for _ in 0..CULVERT_WATER_WAITS {
        session.handle("wait");
    }
    crate::ledger_hash_witness::fnv1a(session.session_ledger_json().as_bytes())
}

/// The seed-42 possession shape's committed ledger, hashed. Minted from two
/// agreeing runs; see the module doc's dated record for the SHA and what the
/// positive control below did and did not move it under.
///
/// **This constant is BLIND to the water-belief path it was minted alongside,
/// and that is recorded rather than hidden** — the same finding
/// `ledger_hash_witness.rs`'s "Seed 42 is not it" note and `the_detent.rs`'s
/// own seed-42 constant both make for the fear path: seed 42's lab shape has
/// a MEDIAN agent making zero water searches (this campaign's own Task 2
/// measurement), so a green run here is a blast-radius check on the whole
/// walk, never evidence that the water-belief route memo did anything.
const CULVERT_SEED_42_LEDGER: u64 = 0x9874_b735_5733_7f83;

/// The water-belief possession shape's (seed 17, 12 waits) committed ledger,
/// hashed. Minted from two agreeing runs, and MOVED under the positive
/// control the module doc records — see there for the mutation, the SHA and
/// the before/after values. Unlike [`CULVERT_SEED_42_LEDGER`], this constant
/// is the one that actually witnesses the campaign's path: seed 17 is the
/// belief-rich shape (52 of 67 residents hold a non-empty belief, max 23
/// rooms), so a fold that changed which known water an agent walks toward
/// has somewhere to show up.
const CULVERT_WATER_LEDGER: u64 = 0x1d03_ec0f_c130_50fe;

/// Two fresh seed-42 sessions must commit the same ledger bytes, and that
/// hash must equal the minted constant. [`CULVERT_SEED_42_LEDGER`]'s own doc
/// records what this witness does and does not see.
#[test]
fn the_culvert_seed_42_ledger_hash_is_pinned() {
    let first = culvert_seed_42_ledger_hash();
    let second = culvert_seed_42_ledger_hash();
    println!("--- the culvert seed-42 ledger hash ---");
    println!("ledger hash {first:#018x} (second fresh session: {second:#018x})");
    assert_eq!(
        first, second,
        "two fresh seed-42 sessions running the same fixed script committed DIFFERENT \
         ledger bytes ({first:#018x} against {second:#018x}) — the walk is not \
         deterministic"
    );
    assert_eq!(
        first, CULVERT_SEED_42_LEDGER,
        "the seed-42 possession shape's committed ledger moved from the minted constant \
         ({first:#018x} against {CULVERT_SEED_42_LEDGER:#018x}) — re-record it main-first \
         per the module doc's dated-record discipline before trusting this witness again"
    );
}

/// Two fresh seed-17 (12-wait) sessions must commit the same ledger bytes,
/// and that hash must equal the minted constant. [`CULVERT_WATER_LEDGER`]'s
/// own doc records why this is the load-bearing half of the pair.
#[test]
fn the_culvert_water_belief_ledger_hash_is_pinned() {
    let first = culvert_water_ledger_hash();
    let second = culvert_water_ledger_hash();
    println!(
        "--- the culvert water-belief (seed {CULVERT_WATER_SEED}, {CULVERT_WATER_WAITS} waits) ledger hash ---"
    );
    println!("ledger hash {first:#018x} (second fresh session: {second:#018x})");
    assert_eq!(
        first, second,
        "two fresh seed-{CULVERT_WATER_SEED} sessions running the same {CULVERT_WATER_WAITS}-wait \
         script committed DIFFERENT ledger bytes ({first:#018x} against {second:#018x}) — the \
         walk is not deterministic"
    );
    assert_eq!(
        first, CULVERT_WATER_LEDGER,
        "the water-belief possession shape's committed ledger moved from the minted constant \
         ({first:#018x} against {CULVERT_WATER_LEDGER:#018x}) — re-record it main-first per the \
         module doc's dated-record discipline before trusting this witness again"
    );
}
