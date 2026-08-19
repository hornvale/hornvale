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
//! - `every_phase_the_chamber_runs_is_rostered` — every set the merge
//!   queue's chamber names is in the roster. It reads
//!   `scripts/sluice-run.sh`'s two literal phase lists (`merge_phases` and
//!   `stage_phases`) and asserts each token has a roster row; it does NOT
//!   see what `HV_SLUICE_PHASES` is overridden with at runtime (that is the
//!   test harness's business, and the chamber refuses an unrostered phase
//!   itself at that point, with `no such set '<phase>'`).
//!
//! The asymmetry between the first two checks and the third is the reason the
//! third direction matters at all, not a detail to skim: the first two both
//! read the *roster* and are blind to a set the chamber invents out of
//! nowhere; the third reads the *chamber* and is blind to a rostered set
//! nobody ever runs. Neither implies the other, so both sides are checked.
//!
//! THE THIRD CHECK USED TO READ THE MAKEFILE, for literal
//! `@bash scripts/lane-dispatch.sh <set>` lines in the `gate-*` targets.
//! The Sluice (Task 12) deleted that dispatch path — a stage gate is a queue
//! entry now — so there are no such lines left anywhere. Re-pointing this at
//! the caller that replaced them is deliberate: a check whose input
//! disappears does not become correct, it becomes vacuous, and this file's
//! own `!is_empty()` guard exists precisely to make that outcome loud rather
//! than silent.

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

/// Every phase the chamber runs is drawn from the roster — the direction the
/// other two checks in this file are structurally blind to.
///
/// `every_rostered_set_is_well_formed` reads the roster and cannot see a set
/// the chamber invents; this reads the chamber and cannot see a rostered set
/// nobody runs. Neither implies the other, which is why both exist.
#[test]
fn every_phase_the_chamber_runs_is_rostered() {
    let chamber = fs::read_to_string(repo_root().join("scripts/sluice-run.sh"))
        .expect("scripts/sluice-run.sh is readable");
    let rostered: Vec<String> = roster().into_iter().map(|r| r.0).collect();

    // The chamber declares its two phase lists as plain shell assignments to
    // string literals, deliberately (see the script's own comment: order is
    // load-bearing and roster order is not phase order). Both are read here;
    // finding only one is itself a failure, because a rename that hid one
    // list from this scraper would leave that list unchecked while the test
    // still reported green on the other.
    let mut lists: Vec<(&str, Vec<String>)> = Vec::new();
    for key in ["merge_phases", "stage_phases"] {
        for line in chamber.lines() {
            let t = line.trim();
            if let Some(rest) = t.strip_prefix(&format!("{key}=\""))
                && let Some(inner) = rest.strip_suffix('"')
            {
                lists.push((key, inner.split_whitespace().map(str::to_string).collect()));
                break;
            }
        }
    }
    assert_eq!(
        lists.len(),
        2,
        "expected both `merge_phases=\"…\"` and `stage_phases=\"…\"` in \
         scripts/sluice-run.sh; found {}. Either the chamber's phase lists \
         were renamed out from under this guard, or it is now asserting \
         nothing — which is the one outcome it must never quietly reach. \
         (This check used to read the Makefile's `lane-dispatch.sh <set>` \
         lines; The Sluice deleted that path and re-pointed it here.)",
        lists.len()
    );
    for (key, phases) in &lists {
        assert!(
            !phases.is_empty(),
            "{key} is empty — the chamber would run no phases at all and \
             every merge would pass by testing nothing"
        );
        for name in phases {
            assert!(
                rostered.contains(name),
                "scripts/sluice-run.sh's {key} names set {name:?}, which is \
                 not in scripts/lane-sets.tsv. Add it to the roster, or fix \
                 the list: a phase with no roster row has no host policy, no \
                 gate, and no declared command — the chamber would exit 2 on \
                 it, mid-merge, holding the box."
            );
        }
    }
}
