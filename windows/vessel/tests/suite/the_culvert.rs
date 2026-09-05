//! The Culvert — the water belief's route memo, and the instruments that
//! measure it.
//!
//! Spec: `docs/superpowers/specs/2026-09-05-the-culvert-design.md`.

use hornvale_vessel::liveness;

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
