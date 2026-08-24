//! The Lexicon of Place's re-accretion guard: "cell" may not come back as a
//! word for a mesh **vertex**.
//!
//! **Why a check and not just a glossary.** "cell" arrived in this repository
//! by *convergent emergence* — every author reached for it independently, out
//! of the GIS/raster convention where a cell is an area. 447 distinct
//! `cell`-bearing identifiers and 16,201 occurrences accumulated over a
//! hundred campaigns without anyone deciding to. A one-time rename against a
//! pressure like that decays; only something that fails holds it.
//!
//! **Why a ratchet and not an allowlist of names.** After the rename, ~108
//! files still contain the word legitimately — a `Cell` in the chamber
//! lattice, a `SurroundsCell` in the chart, a markdown-table cell in the trope
//! report, a frozen wire literal. Those are AREAS, and the word is correct for
//! them. A gate that failed on the mere existence of "cell" would be red on
//! day one and stay red, which trains everyone to ignore it; an allowlist of
//! forty type names would happily pass a *new* vertex-sense `cell` local
//! sitting in the same file. So this fails on **novelty**, the same shape
//! `tropes check`, the timings baseline, type-audit's `waiver(...)` and
//! seam-guard's `expect(survives: …)` all use.
//!
//! **The rule:** a file may carry no more `cell`-bearing tokens than the
//! inventory records for it, and a file absent from the inventory may carry
//! none at all. Adding the word anywhere new fails. Removing it and lowering
//! the number is always allowed, and is the direction of travel.
//!
//! **The direction this enforces, stated because a check that does not say
//! what it is blind to reads as total.** It asserts *no file gained
//! `cell`-bearing tokens*. It is blind three ways, all deliberate:
//!
//! 1. **It counts; it does not classify.** Swapping a grid-sense `cell` for a
//!    vertex-sense one inside an inventoried file keeps the count and passes.
//!    Nothing mechanical can tell those apart — the test that works is "what
//!    is this expression typed as", and that needs a reader.
//! 2. It does not read the book, the specs, the plans or the decision records.
//!    Those are historical records of what was true when written and must
//!    never be swept, so a new *book chapter* may reintroduce the word freely.
//! 3. A line carrying a `lexicon: <reason>` waiver is not counted at all, so a
//!    waiver's reason is never re-checked once written.
//!
//! **To lower a number**, delete the word and re-run with
//! `HV_LEXICON_REBASELINE=1` to rewrite the inventory. **To raise one**, you
//! need a reason a human agreed with — which is what the file is for.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The committed inventory: `path<TAB>count`, ascending by path.
const INVENTORY: &str = "docs/audits/lexicon-inventory.tsv";

/// The marker that waives one line, and which must be followed by a reason.
const WAIVER: &str = "lexicon:";

/// The workspace root — the parent of `cli/`, where this test crate lives.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// Recursively collect `.rs` files under `dir`, skipping build output.
fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    paths.sort();
    for path in paths {
        if path
            .file_name()
            .is_some_and(|n| n == "target" || n == ".git")
        {
            continue;
        }
        if path.is_dir() {
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// Every Rust tree this guard covers: the whole workspace plus the browser
/// clients and the out-of-workspace tools. Discovered rather than listed, so a
/// new crate is covered the day it is added.
fn covered_dirs(root: &Path) -> Vec<PathBuf> {
    let mut dirs = vec![root.join("kernel"), root.join("cli")];
    for layer in ["domains", "windows", "clients", "tools"] {
        let Ok(entries) = std::fs::read_dir(root.join(layer)) else {
            continue;
        };
        let mut crates: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
        crates.sort();
        for c in crates {
            if c.is_dir() {
                dirs.push(c);
            }
        }
    }
    dirs
}

/// How many `cell`-bearing `\w+` tokens a line carries. Case-INSENSITIVE, so
/// `CELL_ID` counts: every hand-written verification grep in the campaign that
/// produced this file used `[Cc]ell` and was blind to ALL-CAPS, which hid ~80
/// occurrences across five crates.
fn cell_tokens(line: &str) -> usize {
    let mut n = 0;
    let mut cur = String::new();
    for ch in line.chars() {
        if ch.is_alphanumeric() || ch == '_' {
            cur.push(ch);
        } else {
            if cur.to_ascii_lowercase().contains("cell") {
                n += 1;
            }
            cur.clear();
        }
    }
    if cur.to_ascii_lowercase().contains("cell") {
        n += 1;
    }
    n
}

/// A line waives itself only with a reason after the marker.
fn waived(line: &str) -> bool {
    match line.split_once(WAIVER) {
        Some((_, reason)) => !reason.trim().is_empty(),
        None => false,
    }
}

/// `path -> count` over every covered Rust file that carries the word.
fn live_counts(root: &Path) -> BTreeMap<String, usize> {
    let mut found = BTreeMap::new();
    for dir in covered_dirs(root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            let rel = file
                .strip_prefix(root)
                .unwrap_or(&file)
                .to_string_lossy()
                .replace('\\', "/");
            // This file necessarily writes the word many times — in its own
            // prose and in its failure message. A guard that fails on its own
            // definition is a guard nobody can read.
            if rel.ends_with("lexicon_guard.rs") {
                continue;
            }
            let Ok(text) = std::fs::read_to_string(&file) else {
                continue;
            };
            let n: usize = text.lines().filter(|l| !waived(l)).map(cell_tokens).sum();
            if n > 0 {
                found.insert(rel, n);
            }
        }
    }
    found
}

/// The committed inventory, as `path -> count`.
fn recorded(root: &Path) -> BTreeMap<String, usize> {
    let text = std::fs::read_to_string(root.join(INVENTORY))
        .unwrap_or_else(|e| panic!("{INVENTORY} is missing or unreadable: {e}"));
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let (path, count) = l
                .rsplit_once('\t')
                .unwrap_or_else(|| panic!("malformed inventory row {l:?} (want path TAB count)"));
            (
                path.to_string(),
                count
                    .parse()
                    .unwrap_or_else(|e| panic!("malformed count in inventory row {l:?}: {e}")),
            )
        })
        .collect()
}

#[test]
fn no_vertex_sense_cell_comes_back() {
    let root = workspace_root();
    let live = live_counts(&root);

    if std::env::var("HV_LEXICON_REBASELINE").is_ok() {
        let mut out = String::from(
            "# The Lexicon of Place: how many `cell`-bearing tokens each Rust\n\
             # file may carry. See cli/tests/suite/lexicon_guard.rs.\n\
             #\n\
             # Every count here is the AREA sense (a lattice square, a chart\n\
             # square, a markdown-table cell) or a frozen serialized literal.\n\
             # A number may fall freely. Raising one needs a human's reason.\n",
        );
        for (path, n) in &live {
            out.push_str(&format!("{path}\t{n}\n"));
        }
        std::fs::write(root.join(INVENTORY), out).expect("inventory is writable");
        return;
    }

    let recorded = recorded(&root);
    let mut grew: Vec<String> = Vec::new();
    for (path, n) in &live {
        match recorded.get(path) {
            Some(cap) if n <= cap => {}
            Some(cap) => grew.push(format!("{path}: {cap} recorded, {n} now (+{})", n - cap)),
            None => grew.push(format!("{path}: not in the inventory, {n} now")),
        }
    }

    assert!(
        grew.is_empty(),
        "\"cell\" has grown, and in this repository it means a mesh VERTEX — a \
         POINT where fields are sampled. In GIS and raster convention a cell is \
         an AREA, so the word actively misleads a reader who knows the \
         convention and mis-teaches one who does not.\n\n\
         Use `Vertex` for the point and `Facet` for the patch. The vocabulary \
         is fixed in book/src/reference/lexicon-of-place.md.\n\n\
         If the thing really IS an area — a lattice square, a chart square, an \
         inflection-table cell — say so on the line and it stops counting:\n  \
         // {WAIVER} <why this one is an area>\n\
         A waiver with no reason is a failure, not a pass.\n\n\
         If it is a FROZEN serialized spelling (decision 0246: the code says \
         Facet while the label says \"room/face\", permanently), waive it the \
         same way with that as the reason.\n\n\
         Grew ({}):\n  {}",
        grew.len(),
        grew.join("\n  ")
    );
}

#[test]
fn a_waiver_needs_a_reason() {
    assert!(waived(
        "let x = 1; // lexicon: frozen wire value, decision 0246"
    ));
    assert!(
        !waived("let x = 1; // lexicon:"),
        "a bare marker must not waive — it is what the next reader believes \
         without being able to check it"
    );
    assert!(!waived("let vertices = 1;"));
}

#[test]
fn the_tokenizer_counts_affixed_bare_and_shouting_forms() {
    assert_eq!(cell_tokens("let land_cells = 1;"), 1);
    assert_eq!(cell_tokens("// every cell, once"), 1);
    assert_eq!(cell_tokens("CellMap<T> and cell_count"), 2);
    assert_eq!(
        cell_tokens("const ISLAND_CELL_CAP: usize = 200;"),
        1,
        "ALL-CAPS must count: the campaign's own greps used [Cc]ell and missed \
         ~80 occurrences across five crates by being case-sensitive"
    );
    assert_eq!(cell_tokens("let vertices = geo.vertices();"), 0);
}

#[test]
fn the_inventory_is_sorted_and_not_empty() {
    let root = workspace_root();
    let recorded = recorded(&root);
    assert!(
        !recorded.is_empty(),
        "an empty inventory would make this guard vacuous in the one direction \
         that matters: every legitimate file would read as `not in the \
         inventory` and the roster would be rewritten to silence it"
    );
    let text = std::fs::read_to_string(root.join(INVENTORY)).expect("inventory readable");
    let paths: Vec<&str> = text
        .lines()
        .filter(|l| !l.trim().is_empty() && !l.starts_with('#'))
        .filter_map(|l| l.rsplit_once('\t').map(|(p, _)| p))
        .collect();
    let mut sorted = paths.clone();
    sorted.sort_unstable();
    assert_eq!(paths, sorted, "{INVENTORY} must be sorted by path");
}
