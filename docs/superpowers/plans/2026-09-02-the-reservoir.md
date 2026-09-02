# The Reservoir Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the test suite one stored seed-42 world to read instead of
rebuilding, and a ratchet that refuses any new world-build site.

**Architecture:** A source-scan enforcement test in `cli/tests/suite/` freezes
every build-entry-point call site in a roster keyed by `<path> <count> <reason>`,
modelled exactly on the shipped `build_path_embedding.rs` guard. A loader in
`hornvale-worldgen` reads the already-committed `cli/tests/fixtures/world-seed-42.json`
at runtime. Seven test helpers then switch from building to loading, which moves
240 call sites without touching a single test body.

**Tech Stack:** Rust 2024, std only (`serde`/`serde_json`/`libm` are the whole
external allowlist), `cargo nextest`.

**Spec:** `docs/superpowers/specs/2026-09-02-the-reservoir-design.md`

**Ledger:** `docs/superpowers/ledgers/2026-09-02-the-reservoir.md` — append a
ruling per task boundary; it is a committed file, not scratch (The Cartulary).

## Global Constraints

- **No new dependency.** The allowlist is `serde`, `serde_json`, `libm`
  (`ALLOWED_EXTERNAL` in `cli/tests/suite/architecture.rs`, decision 0004/0041).
- **No `HashMap`/`HashSet`** anywhere — `BTreeMap`/`BTreeSet`/`Vec` only
  (decision 0005, enforced by `clippy.toml` `disallowed-types`).
- **No wall-clock time** in workspace code (decision 0001, same clippy list). The
  timing rows this plan asks for are recorded by `scripts/timed.sh`, not by Rust.
- **Every crate sets `#![warn(missing_docs)]`** — every new `pub` item, field and
  variant needs a one-line doc comment.
- **Every new `pub`-boundary primitive needs a `type-audit:` tag**
  (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`; decisions
  0027/0028). `make type-audit` is default-deny.
- **`cargo fmt` is the last step before every commit.** Fmt-gate skips are the
  most common review finding in this repo.
- **Layering is constitutional:** `kernel/` → `domains/*` → `windows/*` → `cli/`.
  This plan adds no new edge; worldgen is already a dependency of every crate
  that builds a world.
- **No new seeded draw, no stream-label change, no world-byte change.** If any
  task moves `cli/tests/fixtures/world-seed-42.json`, stop — that is an epoch
  event and this campaign has no mandate for one.
- **Commit gate:** `make gate-commit` before each commit that stages a Rust path.
  Docs-only commits skip it automatically (the `pre-commit` hook says so).
- **Never `--no-verify`.** Never disable a test to make it pass.

---

## File Structure

**Created:**
- `cli/tests/suite/world_build_sites.rs` — the ratchet guard. Scans the
  workspace for build-entry-point call sites, compares against the roster, three
  tests (new site refused / stale row refused / anti-vacuity).
- `cli/tests/fixtures/world-build-sites.tsv` — the roster. `<path>\t<count>\t<reasons>`.
- `windows/worldgen/src/fixture.rs` — the loader. One `pub fn seed_42_world()`
  plus its path constant.

**Modified:**
- `cli/tests/suite.rs` — declare the new module (one `#[path]` + `mod` pair).
- `windows/worldgen/src/lib.rs` — `pub mod fixture;` and a re-export; later, its
  own test helpers `generated`/`constant`.
- `windows/vessel/src/session.rs:7938` — `seam_world` body.
- `windows/scene/src/surrounds.rs:929` — `world` body.
- `windows/worldgen/tests/suite/exposure.rs` — `world` body.
- `windows/vessel/tests/suite/session.rs:9` — `seam_world` body.
- `windows/vessel/tests/suite/session_snapshot.rs` — `world` body.
- `windows/vessel/tests/suite/the_blocking.rs` — `world` body.
- `docs/decisions/0606-*.md`, `docs/decisions/0607-*.md` — new records.
- `book/src/chronicle/the-reservoir.md` — new chronicle entry.
- `docs/retrospectives/the-reservoir.md` — new retrospective.

**Deliberately NOT modified:** `cli/tests/fixtures/world-seed-42.json`,
`cli/tests/suite/lens_purity.rs`, `cli/tests/suite/repose_byte_identity.rs`. Those
three are the freshness guarantee. If a task needs to touch them, the scheme is
wrong — stop and escalate.

---

## Task 1: The ratchet guard and its roster

**Files:**
- Create: `cli/tests/suite/world_build_sites.rs`
- Create: `cli/tests/fixtures/world-build-sites.tsv`
- Modify: `cli/tests/suite.rs` (add the module declaration, alphabetically)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `cli/tests/fixtures/world-build-sites.tsv` in the format
  `<repo-relative path>\t<count>\t<reason-tally>`, where the tally is
  space-separated `reason:N` pairs summing to `count`. Later tasks edit rows in
  this file and lower `UNMIGRATED_CEILING` in the guard.

**Read first:** `cli/tests/suite/build_path_embedding.rs`. This task is that
file's shape with a different predicate. Copy its `rs_files`, its
`workspace_root`, and its three-test structure (offenders / only-shrinks /
anti-vacuity) rather than inventing new ones.

**Why `<path> <count>` and not `<path> <line>`:** a line-keyed row invalidates on
any edit above it, so the guard would redden on unrelated commits and be trained
away — exactly what `test_binary_ratchet.rs` warns about. `build_path_embedding.rs`
already solved this by counting occurrences per file. Spec §3.2 and §9.

**Name the direction this check enforces, and its known imprecision.** The guard
asserts *live ⊆ roster* (nothing builds that is not rostered) and *roster ⊆ live*
(nothing is rostered that no longer builds). It does **not** assert that a
rostered reason is the *correct* reason — only a human reading the site can say
that, which is why every row starts `unmigrated`.

It is a **textual** scan, so an entry-point name appearing in a comment or a
string literal counts as a site. `build_path_embedding.rs` has exactly this
property and accepts it in its own module doc — a false positive is worth taking
over a missed real build. Say so in this file's module doc too, so the next
reader does not mistake the count for a parse. If a comment ever forces a
spurious row, the honest fix is to reword the comment, not to start parsing Rust.

- [ ] **Step 1: Write the failing test file**

Create `cli/tests/suite/world_build_sites.rs`:

```rust
//! Every world build in this workspace is a named site on a roster
//! (decision 0606).
//!
//! A full world build costs ~3.0 s in a debug build, 80.8% of it in the
//! `climate+settlements` stage. Reading the committed seed-42 world instead
//! costs ~15 ms — a ~200x difference. Measured 2026-09-02; see
//! `docs/superpowers/specs/2026-09-02-the-reservoir-design.md` §1.
//!
//! nextest is process-per-test, so a per-process memo recovers nothing: of
//! 100 world-building processes observed in one run, 93 built exactly one
//! world. Decision 0032 recorded the same conclusion for the census.
//!
//! So this is a **ratchet, not a wall** — the shape `tropes check`, the
//! timings baseline, type-audit's `waiver(...)` and seam-guard all use. A
//! guard that failed on the mere existence of 350 grandfathered sites would
//! be red on day one and trained away by day two.
//!
//! **Reason codes** (spec §3.2). `build-path` asserts on the build itself
//! (byte-identity, stream consumption order, pin isolation). `artifacts`
//! needs `GeneratedTerrain`/`GeneratedClimate`, which are `Clone` but not
//! `Serialize`, so no fixture can supply them. `identity` needs a seed or pin
//! set with no committed fixture. `production` is a real build on a real code
//! path. `unmigrated` is grandfathered — **the only reason that may not
//! grow**, and `UNMIGRATED_CEILING` is what stops it.
//!
//! **Direction, and imprecision, stated rather than implied.** This asserts
//! *live ⊆ roster* and *roster ⊆ live*. It does NOT assert a row's reason is
//! the right reason — only a human reading the site can say that, which is
//! why every row starts `unmigrated`. And it is a TEXTUAL scan, so an entry
//! point named in a comment or a string counts as a site;
//! `build_path_embedding.rs` accepts the same tradeoff, a false positive
//! being worth taking over a missed real build. If a comment ever forces a
//! spurious row, reword the comment rather than start parsing Rust.
//!
//! **To add a build site deliberately:** add or bump its row in
//! `cli/tests/fixtures/world-build-sites.tsv` in the same commit, with a
//! reason that is not `unmigrated`, and say in the commit message why the
//! fixture could not serve it. **To migrate one:** lower the row's
//! `unmigrated` tally and lower `UNMIGRATED_CEILING` by the same amount.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The build entry points a roster row can name. Any call to one of these in
/// workspace source is a world build.
///
/// **Five, and `simulate_world` is deliberately not the sixth.**
/// `hornvale_lab::health::simulate_world(world: &World) -> Vec<AffectTrace>`
/// takes an ALREADY-BUILT world and derives terrain and climate from it, so
/// it is a decision-0092 weir site — already governed by clippy's
/// `disallowed-methods`, and carrying 0092's scoped `#[allow]` and its
/// "Named construction site" comment. 0092 governs derivation *from* a
/// world; this roster governs construction *of* one. Adding it here would
/// add 5 spurious rows across 3 files and blur two mechanisms that are
/// separate on purpose. Do not "complete" this list with it.
const ENTRY_POINTS: &[&str] = &[
    "build_world_to_with_artifacts",
    "build_world_observed",
    "build_world_to",
    "build_world",
    "history_for",
];

/// The number of `unmigrated` sites the roster may still carry. Lower it as
/// migrations land; never raise it.
const UNMIGRATED_CEILING: usize = 0; // Step 3 replaces this with the real count.

/// The workspace root — the parent of `cli/`, where this test crate lives.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// Recursively collect `.rs` files under `dir`, sorted.
fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    paths.sort();
    for path in paths {
        if path.is_dir() {
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// Every workspace crate's `src/` and `tests/` tree. Discovered, not listed,
/// so a new domain or window is covered the day it is added.
///
/// **`src/` is scanned as well as `tests/`, deliberately.** The single largest
/// helper in the repository, `seam_world` with 84 callers, lives in a
/// `#[cfg(test)] mod tests` block inside `windows/vessel/src/session.rs`. A
/// tests-only scan would miss it, and locating a test module's textual span
/// would be fragile. `production` is a reason code precisely so real build
/// paths in `src/` have somewhere honest to sit.
fn scanned_dirs(root: &Path) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let mut crates = vec![root.join("kernel"), root.join("cli")];
    for layer in ["domains", "windows"] {
        let Ok(entries) = std::fs::read_dir(root.join(layer)) else {
            continue;
        };
        let mut kids: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
        kids.sort();
        crates.extend(kids.into_iter().filter(|p| p.is_dir()));
    }
    for c in crates {
        for sub in ["src", "tests"] {
            let d = c.join(sub);
            if d.is_dir() {
                dirs.push(d);
            }
        }
    }
    dirs
}

/// Count build-entry-point calls in `text`, longest name first so that
/// `build_world_to` is not also counted as `build_world`.
fn count_sites(text: &str) -> usize {
    let mut remaining = text.to_string();
    let mut total = 0;
    for ep in ENTRY_POINTS {
        let needle = format!("{ep}(");
        total += remaining.matches(&needle).count();
        remaining = remaining.replace(&needle, "");
    }
    total
}

/// `repo-relative path -> number of build sites`, over the scanned trees.
fn live_sites() -> BTreeMap<String, usize> {
    let root = workspace_root();
    let mut found = BTreeMap::new();
    for dir in scanned_dirs(&root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            let text = std::fs::read_to_string(&file).unwrap_or_default();
            let count = count_sites(&text);
            if count > 0 {
                let rel = file
                    .strip_prefix(&root)
                    .unwrap_or(&file)
                    .to_string_lossy()
                    .into_owned();
                found.insert(rel, count);
            }
        }
    }
    found
}

/// One roster row: the total, and the per-reason tally.
struct Row {
    count: usize,
    reasons: BTreeMap<String, usize>,
}

/// The roster, as `path -> Row`.
fn roster() -> BTreeMap<String, Row> {
    include_str!("../fixtures/world-build-sites.tsv")
        .lines()
        .map(str::trim_end)
        .filter(|l| !l.trim().is_empty() && !l.starts_with('#'))
        .map(|l| {
            let mut cols = l.split('\t');
            let path = cols
                .next()
                .unwrap_or_else(|| panic!("malformed roster row: {l:?}"))
                .trim()
                .to_string();
            let count: usize = cols
                .next()
                .unwrap_or_else(|| panic!("roster row {l:?} has no count"))
                .trim()
                .parse()
                .unwrap_or_else(|e| panic!("roster row {l:?} count: {e}"));
            let mut reasons = BTreeMap::new();
            for pair in cols
                .next()
                .unwrap_or_else(|| panic!("roster row {l:?} has no reason tally"))
                .split_whitespace()
            {
                let (name, n) = pair
                    .rsplit_once(':')
                    .unwrap_or_else(|| panic!("roster row {l:?}: want `reason:N`, got {pair:?}"));
                let n: usize = n
                    .parse()
                    .unwrap_or_else(|e| panic!("roster row {l:?} reason {pair:?}: {e}"));
                *reasons.entry(name.to_string()).or_insert(0) += n;
            }
            (path, Row { count, reasons })
        })
        .collect()
}

/// Every reason code a row may name.
const REASONS: &[&str] = &[
    "build-path",
    "artifacts",
    "identity",
    "production",
    "unmigrated",
];

#[test]
fn no_unrostered_world_build_appears() {
    let live = live_sites();
    let roster = roster();

    let mut offenders = Vec::new();
    for (path, count) in &live {
        match roster.get(path) {
            None => offenders.push(format!("  {path}: {count} build(s), no roster row")),
            Some(row) if *count > row.count => offenders.push(format!(
                "  {path}: {count} build(s), roster allows {}",
                row.count
            )),
            Some(_) => {}
        }
    }

    assert!(
        offenders.is_empty(),
        "this workspace gained a world build:\n{}\n\n\
         A full build costs ~3.0 s in a debug build; reading the committed \
         seed-42 world costs ~15 ms. nextest is process-per-test, so each new \
         site pays in full, once per test, forever.\n\n\
         If the test only reads FACTS from the default seed-42 world, call \
         `hornvale_worldgen::seed_42_world()` instead and add no row. If it \
         genuinely must build, add the row to \
         cli/tests/fixtures/world-build-sites.tsv with a reason that is not \
         `unmigrated` ({REASONS:?}) and say in the commit why the fixture \
         cannot serve it.",
        offenders.join("\n")
    );
}

#[test]
fn the_roster_only_shrinks() {
    let live = live_sites();
    let roster = roster();

    let mut stale = Vec::new();
    for (path, row) in &roster {
        match live.get(path) {
            None => stale.push(format!("  {path}: row says {}, file has none", row.count)),
            Some(count) if *count < row.count => {
                stale.push(format!("  {path}: row says {}, file has {count}", row.count))
            }
            Some(_) => {}
        }
    }

    assert!(
        stale.is_empty(),
        "the roster over-counts — good news, builds were removed:\n{}\n\n\
         Lower or delete those rows in \
         cli/tests/fixtures/world-build-sites.tsv, and lower \
         UNMIGRATED_CEILING in this file by the number of `unmigrated` sites \
         you just retired. Checking both directions is what stops the roster \
         from rotting into a list nobody trusts.",
        stale.join("\n")
    );
}

#[test]
fn the_unmigrated_tally_never_grows() {
    let roster = roster();
    let total: usize = roster
        .values()
        .filter_map(|r| r.reasons.get("unmigrated"))
        .sum();
    assert!(
        total <= UNMIGRATED_CEILING,
        "the roster carries {total} `unmigrated` sites, ceiling is \
         {UNMIGRATED_CEILING}.\n\n\
         `unmigrated` is the one reason that may not grow: it means \
         'grandfathered, nobody has looked yet'. A genuinely necessary build \
         gets a real reason ({REASONS:?}) instead. Raising this ceiling \
         undoes the campaign that set it."
    );
}

#[test]
fn every_row_is_well_formed() {
    let roster = roster();
    for (path, row) in &roster {
        let tallied: usize = row.reasons.values().sum();
        assert_eq!(
            tallied, row.count,
            "roster row {path}: reasons sum to {tallied}, count says {}",
            row.count
        );
        for name in row.reasons.keys() {
            assert!(
                REASONS.contains(&name.as_str()),
                "roster row {path}: unknown reason {name:?}; want one of {REASONS:?}"
            );
        }
    }
}

#[test]
fn the_scan_actually_resolves_the_workspace() {
    // Anti-vacuity: every assertion above passes trivially if the scan finds
    // nothing, so pin that it looks in real directories and sees known sites.
    let root = workspace_root();
    let dirs = scanned_dirs(&root);
    assert!(
        dirs.iter().all(|d| d.is_dir()),
        "every scanned dir must exist: {dirs:?}"
    );
    assert!(
        dirs.len() >= 15,
        "expected src+tests for the kernel, cli, and every domain/window; got {}",
        dirs.len()
    );

    let live = live_sites();
    assert!(
        live.contains_key("windows/vessel/src/session.rs"),
        "the scan must see seam_world's build in vessel's session.rs; got {} entries",
        live.len()
    );
    assert!(
        live.contains_key("windows/scene/src/surrounds.rs"),
        "the scan must see the surrounds test helper's build; got {} entries",
        live.len()
    );

    // `build_world_to` must not double-count as `build_world`.
    assert_eq!(
        count_sites("build_world_to(a); build_world(b);"),
        2,
        "longest-first counting must not count build_world_to twice"
    );
    assert_eq!(count_sites("// nothing here"), 0);
}
```

- [ ] **Step 2: Declare the module and run the test to watch it fail**

Add to `cli/tests/suite.rs`, keeping the list alphabetical (it currently runs
`...`, `test_binary_ratchet`, `...` — insert near the `w` entries):

```rust
#[path = "suite/world_build_sites.rs"]
mod world_build_sites;
```

Create an empty placeholder roster so the `include_str!` compiles:

```bash
printf '# placeholder -- Step 3 generates the real roster\n' \
  > cli/tests/fixtures/world-build-sites.tsv
```

Run: `cargo test -p hornvale --test suite -- world_build_sites`
Expected: FAIL. `no_unrostered_world_build_appears` reports ~100 files with no
roster row, and `the_scan_actually_resolves_the_workspace` passes. That
combination is the proof the scan works before the roster exists.

**If `the_scan_actually_resolves_the_workspace` fails instead, stop.** The scan
is wrong and no roster generated from it would mean anything.

- [ ] **Step 3: Generate the roster, all rows `unmigrated`**

Every row starts `unmigrated` and a human reclassifies from there (spec §9): a
generated file of ~100 rows invites rubber-stamping, so the ratchet starts at its
maximum and every permanent reason code is a deliberate act.

```bash
cd "$(git rev-parse --show-toplevel)"
python3 - <<'PY'
import os, re
EP = ["build_world_to_with_artifacts","build_world_observed","build_world_to",
      "build_world","history_for"]
def count(text):
    total = 0
    for ep in EP:
        n = f"{ep}("
        total += text.count(n)
        text = text.replace(n, "")
    return total
crates = ["kernel","cli"] + [f"{l}/{d}" for l in ("domains","windows")
                             for d in sorted(os.listdir(l))]
rows = []
for c in crates:
    for sub in ("src","tests"):
        base = os.path.join(c, sub)
        for dp, dirs, files in os.walk(base):
            dirs[:] = [d for d in dirs if d != "target"]
            for f in sorted(files):
                if not f.endswith(".rs"): continue
                p = os.path.join(dp, f)
                n = count(open(p, errors="replace").read())
                if n: rows.append((p, n))
rows.sort()
total = sum(n for _, n in rows)
with open("cli/tests/fixtures/world-build-sites.tsv", "w") as fh:
    fh.write(
        "# Every world-build call site in the workspace, by file (decision 0606).\n"
        "#\n"
        "# Format: <repo-relative path>\\t<count>\\t<reason:N ...>\n"
        "#\n"
        "# A full build costs ~3.0 s in a debug build (80.8% of it in the\n"
        "# climate+settlements stage); reading the committed seed-42 world costs\n"
        "# ~15 ms. nextest is process-per-test, so every site pays in full, once\n"
        "# per test, forever.\n"
        "#\n"
        "# Reasons: build-path (asserts on the build itself), artifacts (needs\n"
        "# GeneratedTerrain/GeneratedClimate, which are not Serialize), identity\n"
        "# (a seed or pin set with no committed fixture), production (a real code\n"
        "# path), unmigrated (grandfathered -- the ONLY reason that may not grow).\n"
        "#\n"
        "# Keyed by count per file, never by line number: a line-keyed row\n"
        "# invalidates on any edit above it, which would redden this guard on\n"
        "# unrelated commits and get it trained away.\n"
        "#\n"
        f"# Generated 2026-09-02 at campaign start: {len(rows)} files, {total} sites,\n"
        "# every one `unmigrated` by construction. Reclassification is a human act.\n"
        "#\n"
    )
    for p, n in rows:
        fh.write(f"{p}\t{n}\tunmigrated:{n}\n")
print(f"{len(rows)} rows, {total} sites")
PY
```

Then set the ceiling in `cli/tests/suite/world_build_sites.rs` to the printed
site total:

```rust
const UNMIGRATED_CEILING: usize = 350; // replace with the number just printed
```

**Use the number the script prints, not 350.** 350 was measured at `25ee1d830`;
the branch may have moved.

- [ ] **Step 4: Run the tests to verify all five pass**

Run: `cargo test -p hornvale --test suite -- world_build_sites`
Expected: PASS, 5 tests.

- [ ] **Step 5: Prove the guard refuses a new site (the rejection demonstration)**

This is success criterion 1, and decision 0092 §4.1 did the same thing. A guard
nobody has watched refuse is not known to refuse.

Append a real, compiling build to a file that already has one, so its rostered
count is exceeded. `windows/worldgen/tests/suite/exposure.rs` is the target
because its own `world()` helper already builds, so every import the probe needs
is in scope — **verified: that file exists and holds 27 `world()` callers.** Read
its existing helper first and mirror its argument spelling.

```bash
cat >> windows/worldgen/tests/suite/exposure.rs <<'RS'

#[test]
fn hv_scratch_probe() {
    let _ = world();
    let _ = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(7),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    );
}
RS
cargo test -p hornvale --test suite -- world_build_sites 2>&1 | tail -30
```

Expected: FAIL from `no_unrostered_world_build_appears`, naming
`windows/worldgen/tests/suite/exposure.rs` with one more build than its row
allows, and printing the `seed_42_world()` advice.

**That single assertion covers both arms** — a brand-new file hits its `None`
branch ("no roster row"), an existing file hits its count branch ("roster allows
N"). Demonstrating the count branch is enough, and it needs no new file to be
declared in `tests/suite.rs`.

Paste the real failure output into the ledger entry for this task. Then revert
and confirm no trace:

```bash
git checkout -- windows/worldgen/tests/suite/exposure.rs
git status --porcelain   # must show only the intended new/modified files
cargo test -p hornvale --test suite -- world_build_sites   # green again
```

- [ ] **Step 6: Prove the guard refuses a stale row**

Success criterion 2, the direction that stops the roster rotting.

```bash
printf 'windows/nonexistent/src/nope.rs\t1\tunmigrated:1\n' \
  >> cli/tests/fixtures/world-build-sites.tsv
cargo test -p hornvale --test suite -- the_roster_only_shrinks 2>&1 | tail -20
```

Expected: FAIL, naming that row as over-counting. Paste the output into the
ledger, then remove the line and re-run to green.

- [ ] **Step 7: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add cli/tests/suite/world_build_sites.rs \
        cli/tests/fixtures/world-build-sites.tsv \
        cli/tests/suite.rs
git commit -m "test(reservoir): freeze every world-build site on a roster

A source-scan ratchet, modelled on build_path_embedding.rs: every
build-entry-point call site in the workspace appears in
cli/tests/fixtures/world-build-sites.tsv, keyed by count per file rather
than by line so unrelated edits do not redden it. Checked in both
directions, so a stale row is an error too.

Not a clippy disallowed-methods entry: decision 0092 records that the lint
is one on/off switch per scope, and grandfathering hundreds of sites would
silence decision 0041's libm bans in every one of them.

Every row starts unmigrated; reclassification is a human act. Refusal
demonstrated in both directions and reverted."
```

---

## Task 2: The fixture loader

**Files:**
- Create: `windows/worldgen/src/fixture.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod fixture;` and a re-export)
- Test: `windows/worldgen/tests/suite/fixture.rs` (new), declared in
  `windows/worldgen/tests/suite.rs`

**Interfaces:**
- Consumes: nothing from Task 1 (they are independent; Task 1's roster will
  simply gain a row for the new test file, handled in Step 6 here).
- Produces: `hornvale_worldgen::seed_42_world() -> hornvale_kernel::World` —
  the exact world `build_world(Seed(42), &SkyPins::default(),
  SkyChoice::Generated, &TerrainPins::default(), &SettlementPins::default())`
  returns. Tasks 3-5 call this and nothing else.

- [ ] **Step 1: Write the failing byte-identity test**

Create `windows/worldgen/tests/suite/fixture.rs`:

```rust
//! `seed_42_world()` must equal a live build, or every test that reads it is
//! asserting against fiction.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, seed_42_world};

/// The load and the build agree byte for byte, in both directions of the
/// serialization boundary.
#[test]
fn the_fixture_equals_a_live_build() {
    let built = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let loaded = seed_42_world();

    assert_eq!(
        loaded.seed, built.seed,
        "the fixture is a different seed than the build"
    );
    assert_eq!(
        loaded.ledger.len(),
        built.ledger.len(),
        "the fixture holds {} facts, a live build produces {}",
        loaded.ledger.len(),
        built.ledger.len()
    );
    // The whole contract, in one line: identical serialized bytes.
    assert_eq!(
        loaded.to_json(),
        built.to_json(),
        "the fixture has drifted from a live seed-42 build -- regenerate it \
         with `make rebaseline-goldens` ONLY after confirming the world was \
         meant to move, then treat that as an epoch event"
    );
}

/// Anti-vacuity: the assertion above would pass if both sides were empty.
#[test]
fn the_fixture_is_a_real_world() {
    let w = seed_42_world();
    assert!(!w.ledger.is_empty(), "the fixture's ledger is empty");
    assert!(
        w.ledger.len() > 20_000,
        "expected ~21,635 facts in the seed-42 world; got {}",
        w.ledger.len()
    );
}
```

Declare it in `windows/worldgen/tests/suite.rs` alongside the existing modules:

```rust
#[path = "suite/fixture.rs"]
mod fixture;
```

- [ ] **Step 2: Run it to verify it fails to compile**

Run: `cargo test -p hornvale-worldgen --test suite -- fixture`
Expected: FAIL — `cannot find function seed_42_world in crate hornvale_worldgen`.

A compile failure is the correct red here: the function does not exist yet. Note
that this proves nothing about the *assertion*, only about the name — Step 4 is
where the behavioural green arrives.

- [ ] **Step 3: Write the loader**

Create `windows/worldgen/src/fixture.rs`:

```rust
//! The committed seed-42 world, read instead of rebuilt (decision 0607).
//!
//! A full build costs ~3.0 s in a debug build; this read costs ~15 ms — a
//! ~200x difference, measured 2026-09-02. nextest is process-per-test, so an
//! in-process memo recovers nothing (93 of 100 world-building processes in
//! one observed run built exactly one world), which is why the carrier is a
//! file and not a `OnceLock`. Decision 0032 reached the same conclusion for
//! the census.
//!
//! **This module reads a byte-golden and does not author it.**
//! `cli/tests/suite/lens_purity.rs` and `cli/tests/suite/repose_byte_identity.rs`
//! build seed 42 and assert the file's bytes, so the fixture's freshness is
//! guarded by tests inside the suite rather than by a CI step — decision 0125
//! deleted CI, which is where decision 0032's own guarantee-split used to
//! live. `windows/worldgen/tests/suite/fixture.rs` pins load == build here.
//!
//! **What the fixture cannot carry:** `GeneratedTerrain` and
//! `GeneratedClimate` are `Clone` but deliberately not `Serialize`
//! ("Recomputed on demand, never serialized"). A caller needing those pays
//! the sculpt and the fit, so its saving is ~2.7x rather than ~200x, and it
//! keeps an `artifacts` row on the build-site roster.

use hornvale_kernel::World;

/// The committed seed-42 world, relative to THIS crate's manifest directory.
///
/// `env!("CARGO_MANIFEST_DIR")` expands against the crate containing the
/// macro — `windows/worldgen` — so this one fixed prefix resolves correctly
/// no matter which crate calls [`seed_42_world`]. Verified rather than
/// assumed: a probe placed a `pub fn` here and called it from an example in
/// `cli`, and it reported worldgen's directory, while `cli`'s own `../../`
/// resolved outside the repository. The prefix is crate-specific, which is
/// exactly why the loader lives in one crate instead of being copied.
const FIXTURE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../cli/tests/fixtures/world-seed-42.json"
);

/// The seed-42 world under default pins and a generated sky — byte-identical
/// to `build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated,
/// &TerrainPins::default(), &SettlementPins::default())`, read from the
/// committed fixture instead of rebuilt.
///
/// Read at runtime rather than `include_str!`-ed: the file is 5.5 MB, and
/// baking it into every test binary that wants a world would pay the
/// compilation-unit cost that dominates this project's gate.
///
/// # Panics
///
/// If the fixture is missing or does not parse. Both mean the checkout is
/// broken in a way no caller can sensibly handle, and a panic naming the path
/// is more useful than a `Result` every call site would `expect` anyway.
pub fn seed_42_world() -> World {
    let json = std::fs::read_to_string(FIXTURE).unwrap_or_else(|e| {
        panic!("the committed seed-42 world is unreadable at {FIXTURE}: {e}")
    });
    World::from_json(&json)
        .unwrap_or_else(|e| panic!("the committed seed-42 world does not parse: {e}"))
}
```

Add to `windows/worldgen/src/lib.rs`, beside the other `pub mod` lines:

```rust
pub mod fixture;
```

and beside the other re-exports:

```rust
pub use fixture::seed_42_world;
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-worldgen --test suite -- fixture`
Expected: PASS, 2 tests. This is the behavioural green — the fixture's bytes
equal a live build's.

- [ ] **Step 5: Confirm it works from another crate**

The cross-crate path claim is the one thing a same-crate test cannot check.

Run: `cargo test -p hornvale-vessel --lib -- session::tests 2>&1 | tail -3` after
temporarily adding, inside `windows/vessel/src/session.rs`'s test module:

```rust
    #[test]
    fn hv_scratch_cross_crate_fixture_loads() {
        assert!(!hornvale_worldgen::seed_42_world().ledger.is_empty());
    }
```

Expected: PASS. Then `git checkout -- windows/vessel/src/session.rs` and confirm
`git status --porcelain` is clean of it. Record the result in the ledger; Task 3
depends on this working.

- [ ] **Step 6: Update the roster for the new test file**

`windows/worldgen/tests/suite/fixture.rs` contains one `build_world(` call — the
one the byte-identity test needs. That is a `build-path` site, not `unmigrated`:
its whole job is to assert on the build.

Add to `cli/tests/fixtures/world-build-sites.tsv`, in sorted position:

```
windows/worldgen/tests/suite/fixture.rs	1	build-path:1
```

Do **not** raise `UNMIGRATED_CEILING` — this row carries no `unmigrated` sites.

Run: `cargo test -p hornvale --test suite -- world_build_sites`
Expected: PASS, 5 tests.

- [ ] **Step 7: fmt, type-audit, gate, commit**

```bash
cargo fmt
make type-audit          # FIXTURE is a private const, but run it: pub fn added
make gate-commit
git add windows/worldgen/src/fixture.rs windows/worldgen/src/lib.rs \
        windows/worldgen/tests/suite/fixture.rs windows/worldgen/tests/suite.rs \
        cli/tests/fixtures/world-build-sites.tsv
git commit -m "feat(worldgen): read the committed seed-42 world instead of rebuilding

seed_42_world() reads cli/tests/fixtures/world-seed-42.json -- 5.5 MB, 21,635
facts, already committed and already byte-asserted by lens_purity.rs and
repose_byte_identity.rs. ~15 ms against a ~3.0 s debug build.

Runtime read, not include_str!: 5.5 MB baked into every test binary would pay
the compilation-unit cost that dominates this project's gate. One fixed
../../ prefix, because env! expands against the defining crate -- verified by
probe, with cli's own ../../ resolving outside the repo as the negative
control.

Its own byte-identity test is a build-path roster row, not a migration."
```

---

## Task 3: Migrate the flagship — vessel's `seam_world` (84 + 24 callers)

**Files:**
- Modify: `windows/vessel/src/session.rs:7938` (`seam_world`)
- Modify: `windows/vessel/tests/suite/session.rs:9` (`seam_world`)
- Modify: `cli/tests/fixtures/world-build-sites.tsv`
- Modify: `cli/tests/suite/world_build_sites.rs` (lower `UNMIGRATED_CEILING`)

**Interfaces:**
- Consumes: `hornvale_worldgen::seed_42_world()` from Task 2.
- Produces: nothing new. 108 call sites of `seam_world()` are untouched.

**Why this task first:** it is the largest single win and the lowest risk. Both
helpers build *exactly* `Seed(42)` + all-default pins + `SkyChoice::Generated`,
which is byte-for-byte the fixture. `windows/vessel/src/session.rs`'s test module
alone has 117 `#[test]` functions, 85 of which call `seam_world()`, and its 103
named tests carry 1,554.1 CPU-s on lefford.

- [ ] **Step 1: Record the before-cost**

```bash
cd "$(git rev-parse --show-toplevel)"
uptime    # note the load average IN THE LEDGER; a contended number is not a cost
/usr/bin/time -p cargo test -p hornvale-vessel --lib -- session::tests 2>&1 | tail -5
```

Record wall/user/sys and the load average in the ledger. **If load average
exceeds ~5, say so and treat the number as an upper bound** — this repo has
discarded a 5-run pair taken at load 50 as 3.3x wrong.

- [ ] **Step 2: Change the two helper bodies**

`windows/vessel/src/session.rs` — replace the body of `seam_world`:

```rust
    /// Seed 42's world under default pins, read from the committed fixture
    /// rather than rebuilt (decision 0607). This helper's 85 callers each run
    /// in their own nextest process, so building here cost ~3.0 s per test;
    /// the read costs ~15 ms.
    fn seam_world() -> World {
        hornvale_worldgen::seed_42_world()
    }
```

`windows/vessel/tests/suite/session.rs` — replace the body of `seam_world`:

```rust
/// Seed 42's world under default pins, read from the committed fixture rather
/// than rebuilt (decision 0607) — byte-identical to the build this replaced.
fn seam_world() -> World {
    hornvale_worldgen::seed_42_world()
}
```

Leave `world_at(seed)` alone in both files. It is parameterised and its callers
pass `CHAMBERED_SEED` (11x), `42` (5x) and `13` (3x); it keeps an `identity` row.

Remove any import that is now unused (`build_world`, `SkyPins`, `TerrainPins`,
`SettlementPins`, `SkyChoice`, `Seed`) **only if `world_at` does not still use
it** — in `src/session.rs` it does, so most imports stay. `cargo fmt` plus
`make gate-commit`'s clippy will name any that became dead.

- [ ] **Step 3: Run the affected tests**

```bash
cargo test -p hornvale-vessel --lib -- session::tests 2>&1 | tail -5
cargo test -p hornvale-vessel --test suite -- session 2>&1 | tail -5
```

Expected: PASS, same test count as Step 1, no test body edited.

**If a test fails, it has found a real dependency on the build rather than on
the world** — most likely it needs `GeneratedTerrain`/`GeneratedClimate`, which
the fixture cannot carry. Do not "fix" the test. Revert that one helper, give the
file an `artifacts` roster row instead of `unmigrated`, and record which test and
why in the ledger. Scaling the task down is the correct outcome; forcing it is
not.

- [ ] **Step 4: Record the after-cost**

```bash
uptime
/usr/bin/time -p cargo test -p hornvale-vessel --lib -- session::tests 2>&1 | tail -5
```

Append a row to `docs/timings.md` via the project's own recorder if the shape
fits, and put before/after/load in the ledger either way.

- [ ] **Step 5: Update the roster and lower the ceiling**

Both files lose exactly one build site each. Recompute rather than assume:

```bash
grep -c 'build_world(' windows/vessel/src/session.rs windows/vessel/tests/suite/session.rs
```

Edit the two rows in `cli/tests/fixtures/world-build-sites.tsv` to their new
counts and tallies, and lower `UNMIGRATED_CEILING` in
`cli/tests/suite/world_build_sites.rs` by the number of `unmigrated` sites
retired.

Run: `cargo test -p hornvale --test suite -- world_build_sites`
Expected: PASS, 5 tests. The `the_roster_only_shrinks` test is what proves the
roster was actually updated rather than left stale.

- [ ] **Step 6: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/session.rs windows/vessel/tests/suite/session.rs \
        cli/tests/fixtures/world-build-sites.tsv \
        cli/tests/suite/world_build_sites.rs
git commit -m "perf(vessel): seam_world reads the fixture instead of rebuilding

108 call sites across two helpers, each previously paying a ~3.0 s debug
build in its own nextest process. Both built exactly Seed(42) + default pins
+ SkyChoice::Generated, which is byte-for-byte the committed fixture.

No test body changed. session::tests: <before> -> <after> wall (load <N>).
Roster rows lowered and UNMIGRATED_CEILING lowered to match."
```

Replace `<before>`, `<after>` and `<N>` with the real measurements.

---

## Task 4: Migrate scene and worldgen's exposure suite (58 callers)

**Files:**
- Modify: `windows/scene/src/surrounds.rs:929` (`world`)
- Modify: `windows/worldgen/tests/suite/exposure.rs` (`world`)
- Modify: `cli/tests/fixtures/world-build-sites.tsv`
- Modify: `cli/tests/suite/world_build_sites.rs` (`UNMIGRATED_CEILING`)

**Interfaces:**
- Consumes: `hornvale_worldgen::seed_42_world()` from Task 2.
- Produces: nothing.

Both helpers build `Seed(42)` + default pins + `Generated`. `surrounds.rs::world`
has 31 callers; `exposure.rs::world` has 27.

**Watch for `artifacts` dependence here specifically.** `surrounds.rs`'s helper is
immediately followed by `fn observer(w)` which calls
`hornvale_locale::LocaleContext::build(w)`. That is a named construction site
under decision 0092 and derives terrain and climate from the world. It will work
from a loaded world — it takes `&World` — but it pays the sculpt, so this task's
saving is the ~2.7x figure, not ~200x. Say so in the ledger rather than reporting
the headline number.

- [ ] **Step 1: Record the before-cost**

```bash
uptime
/usr/bin/time -p cargo test -p hornvale-scene --lib -- surrounds 2>&1 | tail -5
/usr/bin/time -p cargo test -p hornvale-worldgen --test suite -- exposure 2>&1 | tail -5
```

- [ ] **Step 2: Change `surrounds.rs::world`**

```rust
    /// Seed 42's world under default pins, read from the committed fixture
    /// rather than rebuilt (decision 0607). `observer` below still derives a
    /// locale context from it, so this saves the build and not the sculpt.
    fn world() -> hornvale_kernel::World {
        hornvale_worldgen::seed_42_world()
    }
```

- [ ] **Step 3: Change `exposure.rs::world`**

Read the existing body first and preserve its exact signature and doc comment
style. The body becomes:

```rust
    hornvale_worldgen::seed_42_world()
```

Keep the function's name, visibility and return type exactly as they were — 27
call sites depend on them.

- [ ] **Step 4: Run the affected tests**

```bash
cargo test -p hornvale-scene --lib -- surrounds 2>&1 | tail -5
cargo test -p hornvale-worldgen --test suite -- exposure 2>&1 | tail -5
```

Expected: PASS, same counts as Step 1. On any failure, apply Task 3 Step 3's rule
— revert that helper, give the file an `artifacts` row, record which test and why.

- [ ] **Step 5: Record the after-cost, update the roster, lower the ceiling**

Same shape as Task 3 Steps 4-5. Recompute the counts with `grep -c` rather than
assuming, and run `cargo test -p hornvale --test suite -- world_build_sites` to
green.

- [ ] **Step 6: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add windows/scene/src/surrounds.rs windows/worldgen/tests/suite/exposure.rs \
        cli/tests/fixtures/world-build-sites.tsv \
        cli/tests/suite/world_build_sites.rs
git commit -m "perf(scene,worldgen): two more seed-42 helpers read the fixture

58 call sites. Both built Seed(42) + default pins + Generated. scene's
helper still derives a locale context per test, so its saving is the ~2.7x
artifact-needing figure rather than ~200x -- measured, not assumed.

No test body changed. Roster and ceiling lowered to match."
```

---

## Task 5: Migrate vessel's remaining suite helpers, and worldgen's `generated` seed-42 arm (74 callers)

**Files:**
- Modify: `windows/vessel/tests/suite/session_snapshot.rs` (`world`, 16 callers)
- Modify: `windows/vessel/tests/suite/the_blocking.rs` (`world`, 15 callers)
- Modify: `windows/worldgen/src/lib.rs:12066` (`generated`, 43 of 53 callers)
- Modify: `cli/tests/fixtures/world-build-sites.tsv`
- Modify: `cli/tests/suite/world_build_sites.rs` (`UNMIGRATED_CEILING`)

**Interfaces:**
- Consumes: `hornvale_worldgen::seed_42_world()` from Task 2.
- Produces: nothing.

**This task is last and it is the riskiest, deliberately.**
`windows/worldgen/src/lib.rs::generated` is worldgen's *own* test helper, and the
fixture is authored by worldgen's build. Making worldgen's own tests read it
concentrates the coverage residue spec §5 names: a worldgen change that breaks
the build would then be caught by the `cli` golden tests and Task 2's
byte-identity test, not by worldgen's own 43 tests. That is an acceptable trade
because the golden compares all 21,635 facts, but it is the one place in this
plan where the trade is real, so it lands with its own commit and its own ledger
entry.

- [ ] **Step 1: Record the before-cost**

```bash
uptime
/usr/bin/time -p cargo test -p hornvale-vessel --test suite -- session_snapshot 2>&1 | tail -5
/usr/bin/time -p cargo test -p hornvale-vessel --test suite -- the_blocking 2>&1 | tail -5
/usr/bin/time -p cargo test -p hornvale-worldgen --lib 2>&1 | tail -5
```

- [ ] **Step 2: Change the two vessel suite helpers**

Both build `Seed(42)` + default pins + `Generated`. Read each existing body,
preserve the signature exactly, and replace the body with:

```rust
    hornvale_worldgen::seed_42_world()
```

- [ ] **Step 3: Change worldgen's `generated`, seed-42 arm only**

`generated(seed: u64)` is parameterised: 43 of its 53 callers pass `42`, the rest
pass `1`, `5`, `6`, `9`, `23` or a variable. So this is a branch, not a
replacement:

```rust
    /// A generated-sky world at `seed`. Seed 42 — 43 of this helper's 53
    /// callers — is read from the committed fixture rather than rebuilt
    /// (decision 0607); it is byte-identical to the build, pinned by
    /// `windows/worldgen/tests/suite/fixture.rs`. Every other seed still
    /// builds, because no fixture exists for it.
    fn generated(seed: u64) -> World {
        if seed == 42 {
            return crate::seed_42_world();
        }
        build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("the seed builds")
    }
```

**Read the existing body before writing this** and keep its exact `.expect(...)`
message and pin arguments — the non-42 arm must remain byte-identical to what it
was, or tests at other seeds change meaning.

**Do not touch `constant(seed)` at line 12055.** It builds under
`SkyChoice::Constant`, a different pin signature the seed-42 fixture does not
hold. It keeps its `identity` row. Spec §3.4.

- [ ] **Step 4: Run the affected tests**

```bash
cargo test -p hornvale-vessel --test suite -- session_snapshot 2>&1 | tail -5
cargo test -p hornvale-vessel --test suite -- the_blocking 2>&1 | tail -5
cargo test -p hornvale-worldgen --lib 2>&1 | tail -5
```

Expected: PASS, same counts as Step 1.

**Anti-vacuity check for the branch, required:** prove the seed-42 arm is actually
taken. Temporarily make the fixture path invalid (edit `FIXTURE` in
`windows/worldgen/src/fixture.rs` to a nonexistent filename) and run
`cargo test -p hornvale-worldgen --lib -- generated_worlds_are_deterministic`.
Expected: FAIL with the panic naming the missing path — which proves those tests
now read the fixture. Revert the path and re-run to green. Without this, a
`generated(42)` that silently still built would look identical to success.

- [ ] **Step 5: Record the after-cost, update the roster, lower the ceiling**

Same shape as Task 3 Steps 4-5.

Note that `windows/worldgen/src/lib.rs`'s roster count drops by only one even
though 43 call sites stopped building — the roster counts *sites in the file*, not
callers. Say so in the ledger so the numbers are not read as disagreeing.

- [ ] **Step 6: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/tests/suite/session_snapshot.rs \
        windows/vessel/tests/suite/the_blocking.rs \
        windows/worldgen/src/lib.rs \
        cli/tests/fixtures/world-build-sites.tsv \
        cli/tests/suite/world_build_sites.rs
git commit -m "perf(vessel,worldgen): the last seed-42 helpers read the fixture

74 more call sites. worldgen's generated(seed) branches: seed 42 reads the
fixture, every other seed still builds, and the branch is proven taken by
breaking the fixture path and watching those tests fail.

constant(seed) is deliberately untouched -- SkyChoice::Constant is a
different identity with no fixture, and authoring one is out of scope.

This concentrates spec 5's coverage residue: worldgen's own tests now read a
fixture worldgen authors. The cli goldens compare all 21,635 facts, which
dominates what those 43 tests each checked in slices."
```

---

## Task 6: Decisions, book, retrospective

**Files:**
- **Modify** (NOT create): `docs/decisions/0606-a-world-build-is-a-named-site.md`
  — Task 1 already created it as `Status: Proposed`, because
  `docs_consistency::decision_cites_in_sources_resolve` runs in the subfloor
  tier `gate-commit` executes, and the guard's module doc cites 0606. Without
  the stub, every intermediate task's commit gate would have reddened until this
  task landed. Your job is to flip it to `Accepted` and finish its content.
- Create: `docs/decisions/0607-the-seed-42-fixture-is-an-input.md`
- Create: `book/src/chronicle/the-reservoir.md`
- Create: `docs/retrospectives/the-reservoir.md`
- Modify: `book/src/SUMMARY.md` (chronicle entry)
- Modify: `docs/superpowers/ledgers/2026-09-02-the-reservoir.md` (final entries)

**Interfaces:**
- Consumes: the measured before/after numbers from Tasks 3-5.
- Produces: nothing code-facing.

**Definition of Done for every merged plan includes the project book** — a
chronicle entry plus a freshness sweep of stale chapters, and a re-score of any
Confidence Gradient bet this campaign moved (decision 0030).

- [ ] **Step 1: Finish decision 0606 — it already exists as `Proposed`**

`docs/decisions/0606-a-world-build-is-a-named-site.md` was created by Task 1 (see
this task's Files list for why). **Edit it; do not create it, and do not create a
second record for the same number.** Flip `Status: Proposed (2026-09-02)` to
`Accepted (2026-09-02)`, fill in the measured ceiling the campaign started at and
ended at, and confirm it carries a row in `docs/decisions/README.md`'s index
table. Then continue with the content guidance below.

Slug filename per decision 0026. Follow the format of
`docs/decisions/0092-derivation-at-named-sites.md`, which is this decision's
nearest sibling and should be cited in its **Relates to** line. Content: the
mechanism ruling from spec §3.1-§3.2 — a source-scan ratchet rather than a
`clippy.toml` entry, because `disallowed-methods` is one switch per scope and
0092 itself records a crate-level allow silencing 24 libm bans; the reason-code
taxonomy; and `unmigrated`-never-grows. State the measured ceiling the campaign
started at and ended at.

- [ ] **Step 2: Write decision 0607**

Content: the seed-42 fixture is an input as well as an assertion. Cite decision
0032 (calibration loads the census fixture) as the pattern, and record the
improvement on it explicitly: 0032 split its guarantee across a CI regeneration
step, and decision 0125 deleted CI, so here the freshness guard is a suite test
(`lens_purity.rs`, `repose_byte_identity.rs`, and Task 2's own byte-identity
test). Record what the fixture cannot carry (§2.2) and the ~2.7x-to-~200x spread.

- [ ] **Step 3: Write the chronicle entry**

`book/src/chronicle/the-reservoir.md`, at the book's deliberate altitude:
technical and mathematical, comprehensible without reading the code it may show.
The story worth telling is that two obvious answers were both wrong for the same
underlying reason — nextest's process model — and that the repository had already
written that down in decision 0032 four weeks earlier.

Add it to `book/src/SUMMARY.md` in chronicle order.

- [ ] **Step 4: Freshness sweep**

```bash
grep -rn "build_world\|seed 42" book/src --include=*.md | grep -v chronicle | head -30
```

Read the hits. Any chapter that describes tests building worlds is now stale.
Also check `book/src/open-questions.md` for a Confidence Gradient bet this moved;
if one moved, re-score it (decision 0030).

- [ ] **Step 5: Write the retrospective**

`docs/retrospectives/the-reservoir.md` — one page, process lessons, not product
(decision 0020). Candidates already visible before execution began:

- The user's two proposed remedies were both aimed slightly off-target, and the
  repository already contained the refutation of one (0032). Reading the decision
  log first would have shortened the analysis.
- `campaign-autopilot` still points at the ledger path The Cartulary superseded
  (captured as `PROC-autopilot-names-the-superseded-ledger-path`).
- The spec's own §3.4 conflated *concentration* with *reachability* and named two
  helpers a seed-42 fixture cannot serve. Caught at pre-plan verification, which
  is the step that exists for it — ledger entry #6.
- A full instrumented suite run was abandoned rather than extrapolated, leaving
  the campaign's headline prize a sample. Record whether that was the right call.

- [ ] **Step 6: Commit (docs-only, skips the gate)**

```bash
git add docs/decisions/0606-*.md docs/decisions/0607-*.md \
        book/src/chronicle/the-reservoir.md book/src/SUMMARY.md \
        docs/retrospectives/the-reservoir.md \
        docs/superpowers/ledgers/2026-09-02-the-reservoir.md
git commit -m "docs(reservoir): ratify 0606/0607, chronicle, retrospective"
```

- [ ] **Step 7: Regenerate artifacts and check drift**

Adding `pub fn seed_42_world` moves the committed type-audit report, and adding
two decision records moves the digest's in-force decision index. Both are
drift-checked, and both are commonly missed.

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

Expected: **non-empty** diff in `docs/audits/` (the type-audit report) and
`docs/digest/` (the decision index). Commit the regenerated artifacts:

```bash
git add docs/audits/ docs/digest/
git commit -m "chore(artifacts): regenerate after the reservoir"
```

Then re-run the drift check and expect an empty diff. **If `book/src/gallery/` or
any byte-golden moved, STOP** — that is a world-bytes change, this campaign has no
mandate for one, and `make rebaseline` must not be used to accept it.

---

## Self-Review

**1. Spec coverage.**

| Spec section | Task |
|---|---|
| §3.1 the ratchet mechanism | Task 1 |
| §3.2 roster + reason codes + `unmigrated` ceiling | Task 1 Steps 1, 3; Task 2 Step 6 |
| §3.3 the loader, runtime read, one prefix | Task 2 |
| §3.4 the 240 reachable sites across seven helpers | Tasks 3, 4, 5 (108 + 58 + 74 = 240) |
| §4 permanent exempt set | encoded as reason codes, Task 1 Step 1 |
| §5 coverage residue | Task 5 preamble and commit message |
| §6.1 guard refuses a new site | Task 1 Step 5 |
| §6.2 guard refuses a stale row | Task 1 Step 6 |
| §6.3 `unmigrated` cannot grow | Task 1 Step 1 (`the_unmigrated_tally_never_grows`) |
| §6.4 fixture equals a live build, goldens unmodified | Task 2 Step 1; File Structure "deliberately NOT modified" |
| §6.5 `seed_42_world()` byte-identical | Task 2 Step 1 |
| §6.6 no test outcome changes | Tasks 3-5 Step 4, with the revert-and-reclassify rule |
| §6.7 measured, not assumed | Tasks 3-5 Steps 1 and 4-5, load average required |
| §7 non-goals | Task 5 Step 3 leaves `constant` alone; no depth-scoping task |
| §8 decisions 0606/0607 | Task 6 Steps 1-2 |
| §9 roster site-key churn | resolved in Task 1 by `<path> <count>` keying |

Two gaps found and closed while reviewing: the type-audit/digest drift after
adding a `pub fn` and two decisions had no task (now Task 6 Step 7), and Task 2's
new test file needed its own roster row (now Task 2 Step 6).

**2. Placeholder scan.** No "TBD", no "add appropriate error handling", no
"similar to Task N" — Tasks 3, 4 and 5 each repeat their helper-body code in
full. Three intentional fill-ins remain, each a *measurement* the executor takes
rather than a decision they make: `UNMIGRATED_CEILING`'s initial value (Task 1
Step 3 prints it), and `<before>`/`<after>`/`<N>` in Task 3's commit message.
Each is labelled at the point of use.

**3. Type consistency.** `seed_42_world()` — one name, defined in Task 2 Step 3,
called in Tasks 3, 4, 5 and in Task 5's `crate::seed_42_world()` form (inside
worldgen, where `hornvale_worldgen::` would not resolve). Returns
`hornvale_kernel::World` at every site. Roster format
`<path>\t<count>\t<reason:N ...>` is identical in Task 1 Step 1's parser, Task 1
Step 3's generator, and Tasks 2-5's edits. `REASONS` and the reason codes in
§3.2 match exactly, including `production`.

---

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-09-02-the-reservoir.md`.
Six tasks, each ending in an independently testable deliverable and its own
commit. Tasks 1 and 2 are independent of each other; 3, 4 and 5 each depend only
on 2; 6 depends on the measurements from 3-5.
