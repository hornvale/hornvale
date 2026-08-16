//! Guards the "an allow-list gate cannot see that its own list went short"
//! defect (The Ballast, 2026-08-15): `gate-commit` runs exactly the tests
//! named in `docs/timings/subfloor-roster.tsv`, so a workspace crate with NO
//! entry there is invisible to the commit gate — compiled every commit,
//! tested never, printing the same large green number regardless.
//! `hornvale-hearsay` was found in exactly that state, and the documented
//! remedy ("it enters on the next green stage gate, which measures it and
//! rewrites the roster") had never once actually landed a byte — see
//! `scripts/lane-run.sh` and root `CLAUDE.md`'s corrected paragraph.
//!
//! # THE VERDICT IS THREE-VALUED
//!
//! Same ratchet idiom as `tropes check`, the timings baseline, type-audit's
//! `waiver(...)`, and seam-guard's `expect(survives: …)`: a check that failed
//! on the mere EXISTENCE of a gap would go red the moment one is found (which
//! it was, here, deliberately left unfixed by a DIFFERENT campaign) and stay
//! red forever, training everyone to ignore it. So it fails on NOVELTY:
//!
//!   - an absent crate with no [`DECLARED_ABSENT`] row -> RED (undeclared gap)
//!   - a declared crate, still absent                  -> green (known gap)
//!   - a declared crate that is now PRESENT             -> RED ("delete the
//!     declaration" — the roster caught up and the acknowledgement did not)
//!
//! The third case is what keeps a declaration honest: a one-directional
//! acknowledgement can only ever be satisfied, so it rots unless something
//! notices the moment it stops being true.
//!
//! # WHAT THIS DOES NOT DO
//!
//! It does not assert every *test* is rostered, only every *crate* has at
//! least one entry — the narrowest check that would have caught
//! `hornvale-hearsay`'s exact failure mode (a whole crate silently
//! unrepresented), without requiring this guard to understand
//! `subfloor_roster`'s duration-floor selection rule, which is
//! `windows/lab/src/timings.rs`'s job, not this one's.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;

/// `(crate, reason)` for a workspace crate this guard already knows has no
/// roster entry, and why that is not a defect for THIS guard to raise.
///
/// `docs/timings/subfloor-roster.tsv` itself is off limits to this campaign
/// (The Ballast) — see this file's header and the module doc. `campaign/the-
/// retelling` carries a committed 96-row fix restoring `hornvale-hearsay`'s
/// coverage; this declaration exists only so this guard does not go red
/// while that fix is in flight elsewhere, and it must be DELETED the moment
/// that campaign merges — the `roster_now_covers_every_declared_absent_crate`
/// direction below will turn red on its own once it does, which is the
/// signal to delete it, not a reason to pre-empt it.
// EMPTY, and that is the guard working exactly as designed. The Ballast
// declared `hornvale-hearsay` absent and instructed that the row be deleted
// once The Retelling's roster fix landed. It has: the crate now carries 32
// entries, harvested from a green stage gate. The third verdict — a declared
// crate that is now PRESENT is RED — is what forced this deletion rather than
// leaving a false acknowledgement standing.
const DECLARED_ABSENT: &[(&str, &str)] = &[];

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Every workspace crate name, via `cargo metadata` — the same source
/// `cli/tests/architecture.rs` reads, so this guard agrees with the layering
/// guard about what counts as a workspace crate.
fn workspace_crate_names() -> BTreeSet<String> {
    let output = Command::new(env!("CARGO"))
        .args(["metadata", "--format-version", "1", "--no-deps"])
        .output()
        .expect("cargo metadata should run");
    assert!(
        output.status.success(),
        "cargo metadata failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let meta: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("cargo metadata should emit JSON");
    meta["packages"]
        .as_array()
        .expect("metadata should list packages")
        .iter()
        .map(|pkg| {
            pkg["name"]
                .as_str()
                .expect("package should have a name")
                .to_string()
        })
        .collect()
}

/// Every crate name that owns at least one line in the committed roster —
/// the text before a line's first `::`, which is how every id in the file is
/// shaped (`<crate>::<binary>$<module>::<test>` or `<crate>::<test>`).
fn rostered_crate_names() -> BTreeSet<String> {
    let path = repo_root().join("docs/timings/subfloor-roster.tsv");
    let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{} is readable: {e}", path.display()));
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .filter_map(|l| {
            l.split_once("::")
                .map(|(crate_name, _)| crate_name.to_string())
        })
        .collect()
}

#[test]
fn every_workspace_crate_has_a_roster_entry_or_a_declared_reason() {
    let crates = workspace_crate_names();
    let rostered = rostered_crate_names();
    assert!(
        !crates.is_empty(),
        "cargo metadata named no workspace crates — this guard would be \
         asserting nothing"
    );
    assert!(
        !rostered.is_empty(),
        "docs/timings/subfloor-roster.tsv named no crates at all — either the \
         parsing above broke, or the file is empty; either way this guard \
         cannot tell a real gap from a parsing failure and must not proceed"
    );

    let declared_names: Vec<&str> = DECLARED_ABSENT.iter().map(|(c, _)| *c).collect();
    let declared: BTreeSet<&str> = declared_names.iter().copied().collect();
    assert_eq!(
        declared.len(),
        declared_names.len(),
        "DECLARED_ABSENT names the same crate more than once: {declared_names:?}"
    );
    for name in &declared_names {
        assert!(
            crates.contains(*name),
            "DECLARED_ABSENT names {name:?}, which is not a workspace crate at \
             all (renamed or removed?) — delete the stale declaration"
        );
    }

    let mut undeclared_absent = Vec::new();
    let mut stale_declarations = Vec::new();
    for name in &crates {
        let present = rostered.contains(name);
        let is_declared = declared.contains(name.as_str());
        if !present && !is_declared {
            undeclared_absent.push(name.clone());
        }
        if present && is_declared {
            stale_declarations.push(name.clone());
        }
    }

    assert!(
        undeclared_absent.is_empty(),
        "these workspace crates have NO entry in docs/timings/subfloor-roster.tsv, \
         so `gate-commit` compiles them and runs ZERO of their tests, printing a \
         green number that means nothing for them: {undeclared_absent:?}\n\
         Either the committed roster is stale — run `make lane-roster` to pull \
         the lane's last GREEN copy-out and review/commit it — or, if the fix \
         genuinely belongs to another in-flight campaign, add a (crate, reason) \
         row to DECLARED_ABSENT in this file instead of leaving the gap silent."
    );
    assert!(
        stale_declarations.is_empty(),
        "these crates are declared absent in DECLARED_ABSENT but the committed \
         roster now HAS entries for them — the declaration is stale and must be \
         deleted, not left standing (a one-directional acknowledgement that is \
         never re-checked is exactly the failure mode this guard exists to \
         close): {stale_declarations:?}"
    );
}
