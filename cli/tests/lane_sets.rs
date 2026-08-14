//! The set roster is the single source of truth for what a set is.
//!
//! # DIRECTION EACH CHECK ENFORCES
//!
//! These are three separate directions and none implies another:
//!
//! - `every rostered set has a well-formed row` — blind to a set that exists
//!   in the Makefile and is missing from the roster.
//! - `every `gate-*` Makefile target names only rostered sets` — that is the
//!   direction which catches the omission above.
//! - `CLAUDE.md names the roster file and does not restate its rows` — blind
//!   to a restatement in any other document.

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
            matches!(gate.as_str(), "commit" | "stage" | "campaign"),
            "set {name:?} has gate {gate:?}; the three gates are commit, stage, campaign"
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
