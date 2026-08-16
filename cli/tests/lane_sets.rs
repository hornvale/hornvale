//! The set roster is the single source of truth for what a set is.
//!
//! # DIRECTION EACH CHECK ENFORCES
//!
//! All three directions this file names are now LIVE:
//!
//! - `every rostered set has a well-formed row` — blind to a set that exists
//!   in the Makefile and is missing from the roster.
//! - `CLAUDE.md names the roster file and does not restate its rows` — blind
//!   to a restatement in any other document.
//! - `every_dispatched_set_is_rostered` — every `gate-*` Makefile target
//!   names only rostered sets. It reads the Makefile for literal
//!   `lane-dispatch.sh <set>` invocations and asserts each `<set>` is in the
//!   roster; it does NOT see what `make lane SET=...` is invoked with at
//!   runtime (that argument is a make variable, not a literal name in the
//!   Makefile text — see the test's own doc comment) — `lane-dispatch.sh`
//!   validates that case itself, on the caller's side, when it runs.
//!
//! The asymmetry between the first two checks and the third is the reason the
//! third direction matters at all, not a detail to skim: the first two both
//! read the *roster* and are blind to a set the Makefile invents out of
//! nowhere; the third reads the *Makefile* and is blind to a rostered set
//! nobody ever runs. Neither implies the other, so both sides are checked.

use std::fs;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// `(name, gate, where, authors, command)` for each non-comment row.
fn roster() -> Vec<(String, String, String, String, String)> {
    let text = fs::read_to_string(repo_root().join("scripts/lane-sets.tsv"))
        .expect("scripts/lane-sets.tsv is readable");
    text.lines()
        .filter(|l| !l.starts_with('#') && !l.trim().is_empty())
        .map(|l| {
            let f: Vec<&str> = l.split('\t').collect();
            assert_eq!(
                f.len(),
                5,
                "every roster row has exactly five tab-separated columns; found {} in {l:?}",
                f.len()
            );
            (
                f[0].to_string(),
                f[1].to_string(),
                f[2].to_string(),
                f[3].to_string(),
                f[4].to_string(),
            )
        })
        .collect()
}

#[test]
fn every_rostered_set_is_well_formed() {
    let rows = roster();
    assert!(!rows.is_empty(), "the roster is empty — every set vanished");
    for (name, gate, wh, authors, command) in &rows {
        assert!(
            matches!(gate.as_str(), "commit" | "stage" | "campaign" | "merge"),
            "set {name:?} has gate {gate:?}; the rungs are commit, stage, campaign, merge"
        );
        assert!(
            matches!(wh.as_str(), "local" | "lane"),
            "set {name:?} has where={wh:?}; it is `local` or `lane`"
        );
        assert!(
            matches!(authors.as_str(), "yes" | "no"),
            "set {name:?} has authors={authors:?}; it is `yes` or `no`"
        );
        assert!(!command.trim().is_empty(), "set {name:?} has no command");
    }
}

#[test]
fn the_commit_gate_is_local_and_everything_else_is_the_lane() {
    for (name, gate, wh, _, _) in roster() {
        let expected = if gate == "commit" { "local" } else { "lane" };
        assert_eq!(
            wh, expected,
            "set {name:?} is gated at {gate:?} but runs {wh:?}. The commit gate \
             is the only local one; a `stage` or `campaign` set running locally \
             would produce a verdict on a contended, non-canonical box."
        );
    }
}

#[test]
fn claude_md_names_the_roster_and_does_not_restate_it() {
    let claude = fs::read_to_string(repo_root().join("CLAUDE.md")).expect("CLAUDE.md is readable");
    assert!(
        claude.contains("scripts/lane-sets.tsv"),
        "CLAUDE.md must name scripts/lane-sets.tsv as the roster's home, or a \
         reader has no way to find it"
    );
    // A restatement is what drifts. Two set names on one CLAUDE.md line is the
    // signature of an inline list; naming one set in prose is fine.
    //
    // WHAT THIS ENFORCES, EXACTLY: it matches the literal `` `name` `` — a set
    // name wrapped in single backticks. It is blind to two shapes a
    // restatement could still take: prose that writes `make gate` instead of
    // the bare set name `gate` (the overwhelmingly common form in this file),
    // and a restatement laid out as a table with one set name per line, which
    // never puts two names on the same line at all. Widening the matcher to
    // catch those would also catch ordinary sentences about `make gate` or
    // `make ci`, turning this into a guard that cries wolf on prose that never
    // restated anything — so, in the spirit of `cli/tests/heavy_tier.rs`'s
    // "THIS ROSTER IS A FLOOR, NOT A CENSUS", this check is a narrow trip-wire
    // for the one restatement shape it can tell apart from ordinary prose, not
    // a complete defense against every restatement shape.
    let names: Vec<String> = roster().into_iter().map(|r| r.0).collect();
    for line in claude.lines() {
        let hits = names
            .iter()
            .filter(|n| line.contains(&format!("`{n}`")))
            .count();
        assert!(
            hits < 3,
            "CLAUDE.md line restates the roster ({hits} set names on one line): \
             {line:?}. The roster is scripts/lane-sets.tsv; prose may point at \
             it but must not copy it — an inline list drifts the moment a set \
             is added, exactly as docs/generated-paths.txt exists to prevent."
        );
    }
}

/// Every `gate-*` target's set list is drawn from the roster — the direction
/// the other two checks in this file are structurally blind to.
///
/// `every_rostered_set_is_well_formed` reads the roster and cannot see a set
/// the Makefile invents; this reads the Makefile and cannot see a rostered set
/// nobody runs. Neither implies the other, which is why both exist.
#[test]
fn every_dispatched_set_is_rostered() {
    let makefile = fs::read_to_string(repo_root().join("Makefile")).expect("Makefile is readable");
    let rostered: Vec<String> = roster().into_iter().map(|r| r.0).collect();

    let mut dispatched = Vec::new();
    for line in makefile.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("@bash scripts/lane-dispatch.sh ")
            && let Some(name) = rest.split_whitespace().next()
        {
            // SKIP make-variable arguments. `make lane` dispatches whatever the
            // caller passes — `@bash scripts/lane-dispatch.sh "$(SET)" "$(REF)"`
            // — so the token there is `"$(SET)"`, not a set name, and asserting
            // it is rostered would fail on a target that is behaving correctly.
            // Found by checking this test against the real Makefile one task
            // before it was dispatched; the first draft asserted over it.
            //
            // The direction narrows accordingly and the doc must say so: this
            // enforces `every LITERALLY-NAMED dispatched set is rostered`. It is
            // blind to what `make lane` is invoked with at runtime, which is the
            // caller's business and is validated by lane-dispatch.sh itself.
            if !name.contains('$') {
                dispatched.push(name.trim_matches('"').to_string());
            }
        }
    }
    assert!(
        !dispatched.is_empty(),
        "found no `lane-dispatch.sh <set>` invocations in the Makefile. Either \
         the gate targets were renamed out from under this guard, or it is now \
         asserting nothing — which is the one outcome it must never quietly reach."
    );
    for name in &dispatched {
        assert!(
            rostered.contains(name),
            "the Makefile dispatches set {name:?}, which is not in \
             scripts/lane-sets.tsv. Add it to the roster, or fix the target: a \
             dispatched set with no roster row has no host policy, no gate, and \
             no declared command."
        );
    }
}
