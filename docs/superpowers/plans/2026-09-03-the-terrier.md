# The Terrier Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The world's occupation register (`occupations_by_vertex`) is
built once per `WorldContext` and read by every `Brief`, so a chamber turn
stops re-surveying the whole world two to five times, and a structural
ratchet keeps any whole-world history read off every session path.

**Architecture:** `WorldContext` (`windows/vessel/src/session.rs`) gains
`occupations: BTreeMap<Vertex, Vec<OccupationRecord>>`, filled at the end of
`build` by `hornvale_worldgen::occupations_by_vertex(world)` — a ledger
read that consumes no stream draw, placed after the five seeded
derivations. `brief::brief_of` takes `&BTreeMap<Vertex, Vec<OccupationRecord>>`
instead of `&World` and looks its vertex up. A source-scan test forbids the
three whole-world occupation readers anywhere under `windows/vessel/src`
except inside `WorldContext::build`'s body. A VIEW ≡ SCAN test asserts the
hoisted brief equals a fresh one at every locale two scripts visit. Two
bench Measured blocks and seven in-place corrections record what the 8 ms
actually was.

**Tech Stack:** Rust 2024, std-only plus `serde`/`serde_json`/`libm`.
`cargo nextest`; in-module tests in `session.rs`; `tests/suite/*.rs` behind
`windows/vessel/tests/suite.rs`.

**Spec:** `docs/superpowers/specs/2026-09-03-the-terrier-design.md`

**Ledger:** `docs/superpowers/ledgers/2026-09-03-the-terrier.md` — append a
ruling as it is made, do not batch to the end. Decision block: 0636–0645.

## Global Constraints

- **Layering** (`cli/tests/architecture.rs`): `kernel` → `domains/*` →
  `windows/*` → `cli`. `windows/vessel` already depends on
  `hornvale-worldgen` and `hornvale-history` (`windows/vessel/Cargo.toml:10-11`);
  no new edge. If the layering golden moves, STOP — nothing here should.
- **Dependencies**: `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. **No
  wall-clock** in sim or tests (`Instant` is banned by `clippy.toml`; the
  two `move_cost` examples carry the scoped `#[allow]`).
- **Determinism**: same seed, same script → byte-identical session
  goldens, gallery transcripts, client fixtures and world file. Spec §4 P5:
  after `make rebaseline`, only `docs/audits/` (the type-audit report, on
  the `brief_of` signature change) and `docs/digest/` (on the new decision
  record) may move. Anything under `book/src/gallery/`,
  `windows/vessel/tests/fixtures/`, `clients/game/core/tests/fixtures/`,
  `cli/tests/fixtures/world-seed-42.json`, an almanac,
  `book/src/laboratory/` or `book/src/domesday/` moving → **STOP** and
  report; it is a defect, never an epoch to accept.
- **The ledger is the only stored truth**: the hoisted map is never
  serialized and nothing derived from a `Brief` is committed (unchanged).
- **`WorldContext` holds no interior mutability and is not mutated after
  `build`** (`tests/suite/world_context.rs`'s closing comment is the
  contract). The new field is a plain `BTreeMap`, filled once.
- **The order of `WorldContext::build`'s five seeded derivations is a
  save-format contract** guarded only by the gallery transcripts. The map
  build goes AFTER them and reads the ledger only; a moved transcript means
  that promise was broken — see P5 above.
- **Every test carries a `MUTATION THIS MUST FAIL AGAINST:` line** naming a
  compiling edit the implementer FOUND and ran, with the observed red
  (decision 0353). A plan-prescribed mutation is a suggestion; if it is a
  null, find one that is not and say so in the test's doc.
- **Seed 42's flagship never changes room and its 68 bodies share one home**
  (The Rack, The Roll). A mutation over a POPULATION at seed 42 is a null;
  the brief varies by VERTEX, not by body, so this campaign's tests walk
  to a different vertex rather than waiting for a body to.
- **Ratchets that bite**: type-audit `check` is default-deny on every
  pub-boundary primitive (`brief_of`'s `walk_depth: u32` keeps its
  `bare-ok(count: walk_depth)`; a `&BTreeMap` parameter needs no tag);
  `#[ignore]` reasons need the `claim:` shape (decision 0093) — none are
  expected here; `cli/tests/suite/lexicon_guard.rs` counts bare `cell`
  tokens — write "square"/"vertex"/"slot", and if a `Cell` type must be
  named in a doc, waive it as the existing sites do
  (`// lexicon: AREA-sense chamber-lattice square, never a mesh vertex`);
  `tools/plumb` is default-deny over authored numeric constants in
  `src/` — a bound in a TEST file is not audited, a `const` in `src/` is;
  `tools/placement-audit` is default-deny on shape-twin placement tags —
  if `make gate-commit` refuses on it, read its message and tag as it says.
  The audit report regenerates in the SAME commit as a pub-boundary
  change: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report
  > docs/audits/type-audit-report.md`.
- **`cargo fmt` is the final step before every commit.** The pre-commit hook
  runs `make gate-commit` on Rust paths; never bypass; commit the
  `docs/timings.md` row separately as `chore(timings): record the-terrier
  task N gate runs`. Commit messages via `git commit -F <file>` written
  with the Write tool (a bash heredoc has leaked an `EOF)` into history
  before, and `"$(cat <<'EOF')"` breaks on apostrophes).
- **Run a test binary once, then grep the log**: `cargo test -p
  hornvale-vessel --test suite > /tmp/hv.log 2>&1; grep -E "^test
  result|FAILED|panicked|\.\.\. FAILED" /tmp/hv.log`. In-module tests:
  `cargo test -p hornvale-vessel --lib <filter> > /tmp/hv-lib.log 2>&1`.
- **A new vessel test is not in the commit gate until the next green
  chamber job rewrites `docs/timings/subfloor-roster.tsv`**; it runs in the
  stage gate from its first commit. Say this in the test's doc rather than
  claiming it gates commits today.
- **Push at every task boundary.** Absorb main at every stage boundary with
  `make sluice-stage BRANCH=campaign/the-terrier REF=$(git rev-parse HEAD)`.
- **The controller never writes to this worktree while an implementer holds
  it** (The Rack's retrospective): ledger entries and timings rows are
  committed at task boundaries only.
- **Wall-clock readings are reported, never tuned.** Quiet box = all three
  `uptime` load averages under 4 (The Repose). If the box is contended,
  take the reading anyway, quote the averages beside it, and say
  CONTENDED; do not wait for quiet and do not move a number to fit.
- Branch: `campaign/the-terrier`. Worktree: `.claude/worktrees/the-terrier`
  (main absorbed at `2e85f2d72` before Task 1, carrying The Brattice).

---

## File Structure

| File | Responsibility | Tasks |
| --- | --- | --- |
| `windows/vessel/tests/suite/the_terrier.rs` | **New.** The source ratchet: no whole-world occupation reader under `src/` outside `WorldContext::build` | 1 |
| `windows/vessel/tests/suite.rs` | `mod the_terrier;` | 1 |
| `windows/vessel/src/session.rs` | `WorldContext.occupations` field, filled in `build`; `brief_here` passes it; in-module P6 test (`brief_here` ≡ fresh map) at seeds 42 and 7 | 1, 2 |
| `windows/vessel/src/brief.rs` | `brief_of(occupations: &BTreeMap<Vertex, Vec<OccupationRecord>>, …)`; the `NOTE ON COST` rewritten as history | 1 |
| `docs/audits/type-audit-report.md` | Regenerated with the signature change | 1 |
| `windows/vessel/examples/move_cost.rs` | `## Measured — AFTER The Terrier` block; the AFTER-Rack interpretation paragraph corrected | 3 |
| `clients/game/bin/examples/move_cost.rs` | `### AFTER The Terrier` block with P6's rule; "the same shadowcast cost" corrected | 3 |
| `book/src/frontier/idea-registry.md` | `TOOL-chamber-snapshot-prices-a-shadowcast` corrected + `shipped`; `TOOL-tick-profile-2026-08` last sentence; new `PROC-a-two-point-difference-names-a-step-not-a-cost` | 4 |
| `book/src/chronicle/the-rack.md`, `docs/retrospectives/the-rack.md` | The two chronicle sentences and the retro follow-up bullet corrected in place, dated | 4 |
| `docs/decisions/0636-a-world-scoped-derivation-lives-on-worldcontext.md` | New decision | 4 |
| `docs/digest/decisions-in-force.md`, `docs/digest/intent-vs-reality.md` | Regenerated (`make rebaseline`) | 4 |
| `book/src/chronicle/the-terrier.md`, `book/src/SUMMARY.md`, `docs/retrospectives/the-terrier.md`, `docs/retrospectives/README.md` | Close | 5 |

---

## Stage 1 — the register

### Task 1: The ratchet goes red, then the hoist makes it green

**Files:**
- Create: `windows/vessel/tests/suite/the_terrier.rs`
- Modify: `windows/vessel/tests/suite.rs` (add `mod the_terrier;` in
  alphabetical position among the existing `mod` lines)
- Modify: `windows/vessel/src/session.rs` — `WorldContext` struct
  (`:680-706`), `WorldContext::build`'s tail (`:770-787`), the
  `hornvale_kernel` import list (`:30-33`), `brief_here` (`:7207-7217`)
- Modify: `windows/vessel/src/brief.rs` — `brief_of` (`:139-173`) and its
  imports (`:31-32`)
- Regenerate: `docs/audits/type-audit-report.md`

**Interfaces:**
- Consumes: `hornvale_worldgen::occupations_by_vertex(world: &World) ->
  BTreeMap<Vertex, Vec<OccupationRecord>>` (re-exported at
  `windows/worldgen/src/lib.rs:144-150`); `hornvale_history::record::OccupationRecord`
  (`Clone`, field `core: Occupation` with `function`, `tech`, `notability`,
  `people`, `peak_population`, `ended: Option<_>`); `hornvale_kernel::Vertex`.
- Produces: `WorldContext.occupations: BTreeMap<Vertex, Vec<OccupationRecord>>`
  (`pub(crate)`), and

```rust
pub fn brief_of(
    occupations: &BTreeMap<Vertex, Vec<OccupationRecord>>,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    place: &Facet,
    terrain: &dyn crate::liveness::Terrain,
    walk_depth: u32,
) -> Brief
```

- [ ] **Step 1: Write the ratchet test**

Create `windows/vessel/tests/suite/the_terrier.rs`:

```rust
//! The Terrier (spec §3.3): no session path re-surveys the world's
//! occupation register.
//!
//! `brief::brief_of` used to call `hornvale_worldgen::occupations_by_vertex`
//! — a reconstruction of EVERY committed occupation from the ledger — on
//! every call, and a chamber turn called it two to five times, at 8.7-28.8
//! ms each. That was the whole of what The Rack priced as "one shadowcast".
//! The map now lives on `WorldContext`, built once.
//!
//! A structural source scan, because the property is about what the code
//! does NOT contain, which no runtime assertion can witness
//! (`affordance.rs::no_verb_by_object_table_exists` and
//! `underground.rs::the_reach_seam_is_the_only_source_of_the_radius` are
//! this repo's precedents for the shape). A `TurnWork` counter was
//! considered and rejected: after the hoist no turn path reconstructs an
//! occupation, so the counter would have no writer — the permanently-green
//! zero The Rack argued against (decision 0598).
//!
//! **Direction this check enforces:** it forbids the PRESENCE of a
//! whole-world occupation read anywhere under `windows/vessel/src` except
//! inside `WorldContext::build`'s body. It does not prove the hoisted map is
//! complete or current — `session.rs`'s in-module
//! `the_hoisted_brief_is_the_fresh_brief_at_every_visited_locale` does that.
//!
//! Not in the commit gate until the next green chamber job rewrites
//! `docs/timings/subfloor-roster.tsv`; runs in the stage gate from its first
//! commit.

use std::path::{Path, PathBuf};

/// The three ways `windows/worldgen` hands back the world's occupations.
/// All three rescan `world.ledger` in full (`history_emit.rs:514-535, 573-584`).
const WHOLE_WORLD_READERS: [&str; 3] = [
    "occupations_by_vertex(",
    "occupations_at(",
    "occupation_records(",
];

/// Every `.rs` file under `windows/vessel/src`, recursively, sorted so a
/// failure names the same file on every box.
fn production_sources() -> Vec<PathBuf> {
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        let mut entries: Vec<_> = std::fs::read_dir(dir)
            .unwrap_or_else(|e| panic!("read_dir {}: {e}", dir.display()))
            .map(|e| e.expect("dir entry").path())
            .collect();
        entries.sort();
        for path in entries {
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|x| x == "rs") {
                out.push(path);
            }
        }
    }
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let mut out = Vec::new();
    walk(&root, &mut out);
    assert!(
        out.len() > 20,
        "positive control on the walk: only {} files under {} — the scan is \
         not looking at the crate",
        out.len(),
        root.display()
    );
    out
}

/// The file's production half with every comment line removed: everything
/// before the first `#[cfg(test)]`, minus lines whose trimmed form starts
/// with `//`. Comments are dropped so that a doc comment may NAME the
/// forbidden functions (brief.rs's rewritten cost note does) without the
/// scan reading prose as a call.
fn production_code(src: &str) -> String {
    src.split("#[cfg(test)]")
        .next()
        .expect("split always yields one piece")
        .lines()
        .filter(|l| !l.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// The brace-balanced body that follows `needle` in `src`: from the first
/// `{` after the needle to its matching `}`, inclusive. `None` if the needle
/// is absent. Naive about braces inside string literals, which is fine for
/// `WorldContext::build` (its only literals are error messages without
/// braces) and asserted by the controls in the test below.
fn block_body_after<'a>(src: &'a str, needle: &str) -> Option<&'a str> {
    let start = src.find(needle)? + needle.len();
    let open = start + src[start..].find('{')?;
    let mut depth = 0usize;
    for (i, ch) in src[open..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(&src[open..open + i + 1]);
                }
            }
            _ => {}
        }
    }
    None
}

/// The forbidden names appear in production code ONLY inside
/// `WorldContext::build`.
///
/// MUTATION THIS MUST FAIL AGAINST: restore the per-call read —
/// in `brief.rs`, replace `occupations.get(&vertex)` with
/// `hornvale_worldgen::occupations_by_vertex(world).get(&vertex)` (adding a
/// `world: &World` parameter). Observed red, against the pre-hoist tree at
/// `2e85f2d72`: <paste the assertion message the implementer saw>.
#[test]
fn no_session_path_re_surveys_the_occupation_register() {
    let session = std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("src/session.rs"),
    )
    .expect("session.rs");
    let build = block_body_after(
        &production_code(&session),
        "pub fn build(world: &'w World)",
    )
    .expect("session.rs must define WorldContext::build(world: &'w World)");
    // Controls on the extraction: the body we cut is the whole of `build`
    // and nothing past it.
    assert!(
        build.contains("Ok(WorldContext {"),
        "the extracted build body does not reach its own Ok(WorldContext {{ … }})"
    );
    assert!(
        !build.contains("fn context("),
        "the extracted build body overran into the next method"
    );
    // Positive control: the one permitted site does name a reader, so an
    // emptied scan cannot read as green.
    assert!(
        build.contains("occupations_by_vertex("),
        "positive control: WorldContext::build must build the register with \
         occupations_by_vertex"
    );

    let mut offenders = Vec::new();
    for path in production_sources() {
        let src = std::fs::read_to_string(&path).expect("source file");
        let mut code = production_code(&src);
        if path.ends_with("session.rs") {
            // Blank out the permitted block, keeping the rest of the file.
            code = code.replacen(build, "", 1);
        }
        for (n, line) in code.lines().enumerate() {
            for reader in WHOLE_WORLD_READERS {
                if line.contains(reader) {
                    offenders.push(format!(
                        "{}:{}: {}",
                        path.strip_prefix(env!("CARGO_MANIFEST_DIR"))
                            .unwrap_or(&path)
                            .display(),
                        n + 1,
                        line.trim()
                    ));
                }
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "a session path re-surveys the world's occupation register; the map \
         is built once on WorldContext and read from there (The Terrier, \
         spec §3.1):\n{}",
        offenders.join("\n")
    );
}

/// Positive control for the scanner: a line-scan that never matched would
/// pass the test above for the wrong reason.
#[test]
fn the_register_scanner_catches_a_per_call_read() {
    let offending = "let alive = hornvale_worldgen::occupations_by_vertex(world).remove(&v);";
    assert!(
        WHOLE_WORLD_READERS.iter().any(|r| offending.contains(r)),
        "positive control: the reader list must match a real call"
    );
    let prose_only = "// hoisted; occupations_by_vertex( is built once in WorldContext::build";
    assert!(
        production_code(prose_only).is_empty(),
        "a comment line must be dropped before the scan, or the rewritten \
         cost note would trip it"
    );
}

/// Control on the body extractor over a shape with nested braces.
#[test]
fn the_block_extractor_matches_nested_braces() {
    let src = "fn a() { let x = S { y: 1 }; if x.y == 1 { 2 } else { 3 } }\nfn b() { 0 }";
    assert_eq!(
        block_body_after(src, "fn a()"),
        Some("{ let x = S { y: 1 }; if x.y == 1 { 2 } else { 3 } }")
    );
    assert_eq!(block_body_after(src, "fn zzz()"), None);
}
```

Add `mod the_terrier;` to `windows/vessel/tests/suite.rs`.

- [ ] **Step 2: Run it against today's tree and record the red**

Run: `cargo test -p hornvale-vessel --test suite the_terrier > /tmp/hv.log 2>&1; grep -E "^test result|FAILED|panicked|re-surveys" /tmp/hv.log`

Expected: `no_session_path_re_surveys_the_occupation_register` FAILS. Two
possible reds, and both are the right one: either the positive control
(`build` does not yet name `occupations_by_vertex`) or the offender list
naming `src/brief.rs:161`. **Paste the message you actually saw** into the
test's `Observed red` line — replace the `<paste …>` placeholder; a plan
placeholder left in a shipped test is a review finding. The two controls
must PASS already.

- [ ] **Step 3: Hoist the register onto `WorldContext`**

In `windows/vessel/src/session.rs`, add `Vertex` to the `hornvale_kernel`
import (`:30-33`):

```rust
use hornvale_kernel::{
    ConceptRegistry, EntityId, Facet, FacetId, Fact, Ledger, Seed, TickSpan, Value, Vertex,
    World, WorldTime,
};
```

Add the field after `report` in `pub struct WorldContext<'w>` (`:706`):

```rust
    /// The world's occupation register (The Terrier, spec §3.1): every
    /// committed occupation, grouped by the vertex it stands on, reconstructed
    /// from `world.ledger` ONCE here and read by every `Brief` this context's
    /// sessions derive (`brief::brief_of`). A pure function of the immutable
    /// `World`, which is what makes it world-scoped like everything else on
    /// this type. Before this field, `brief_of` rebuilt the whole map on every
    /// call — 8.7-28.8 ms — and a chamber turn called it two to five times;
    /// that was the entire cost The Rack's chronicle attributed to "one
    /// shadowcast" (0.012 ms).
    ///
    /// Built AFTER the five seeded derivations in [`Self::build`] and
    /// consuming no stream draw: a ledger read, not a sixth derivation, so it
    /// cannot move the order the gallery transcripts guard.
    pub(crate) occupations:
        std::collections::BTreeMap<Vertex, Vec<hornvale_history::record::OccupationRecord>>,
```

(`session.rs` has no top-level `use std::collections::…` — verified
2026-09-03 — so the path is written in full, as the neighbouring
`settlement_rooms` field on `Session` does.)

In `WorldContext::build`, after the `let climate = Some(climate);` line
(`:781`) and before `Ok(WorldContext { … })`:

```rust
        // The occupation register (The Terrier). A READ over the committed
        // ledger — no `Stream` is touched — placed after the five derivations
        // above so that the order those transcripts guard is visibly not in
        // question. ~9-26 ms once per world (contended), against ~3 s for the
        // block above; it used to be paid on every `brief_of` call.
        let occupations = hornvale_worldgen::occupations_by_vertex(world);
        Ok(WorldContext {
            world,
            terrain,
            climate,
            ctx,
            wc,
            report,
            occupations,
        })
```

Then `brief_here` (`:7207`):

```rust
    /// The brief for wherever the possession currently stands. Reads the
    /// context's occupation register rather than re-surveying the world
    /// (The Terrier).
    fn brief_here(&self) -> crate::brief::Brief {
        let terrain = self.terrain_here();
        crate::brief::brief_of(
            &self.wctx.occupations,
            self.wctx.ctx.climate().geosphere(),
            self.wctx.ctx.nearest_index(),
            &self.position(),
            &terrain,
            self.walk_depth(),
        )
    }
```

- [ ] **Step 4: `brief_of` takes the register**

In `windows/vessel/src/brief.rs`, change the imports (`:31-32`) to:

```rust
use hornvale_history::record::{Function, Notability, OccupationRecord, TechHorizon};
use hornvale_kernel::{Facet, Geosphere, KindId, NearestVertexIndex, Vertex};
use std::collections::BTreeMap;
```

(`World` is no longer used here — drop it, or `-D warnings` refuses.)

Replace `brief_of` (`:135-173`) with:

```rust
/// Derive the brief for `place`. Every read is taken at the walk band, so a
/// chamber and its locale yield the same brief — which is what makes a
/// structure's chambers agree about what building they are in.
///
/// `occupations` is the world's occupation register,
/// `hornvale_worldgen::occupations_by_vertex(world)`, built ONCE by the
/// caller (`WorldContext::build`) and handed in. **History of this
/// parameter, kept because the note it replaces was right for five weeks
/// before anyone measured it:** from `4569d883d` (2026-07-27) to The Terrier
/// (2026-09-03) this function took `&World` and rebuilt the whole map on
/// every call, under a `NOTE ON COST` that said "if a profile shows it
/// mattering, hoist the map to the caller … do NOT memoize inside this
/// function, because a hidden cache in a derivation path is how derived
/// state stops being derived." The profile showed 8.7-28.8 ms per call and
/// two to five calls per indoor turn — the whole of what The Rack had
/// attributed to a 0.012 ms shadowcast. The note's prescription is what
/// shipped, and its prohibition still stands: there is no cache here, only
/// a parameter.
/// type-audit: bare-ok(count: walk_depth)
pub fn brief_of(
    occupations: &BTreeMap<Vertex, Vec<OccupationRecord>>,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    place: &Facet,
    terrain: &dyn crate::liveness::Terrain,
    walk_depth: u32,
) -> Brief {
    let locale = crate::depth::truncate_to_walk(place, walk_depth);
    let built = terrain.is_built(&locale);
    let cold = terrain.is_cold(&locale);
    let alive = containing_vertex(&locale, geo, index)
        .and_then(|vertex| occupations.get(&vertex))
        .and_then(|occs| occs.iter().find(|o| o.core.ended.is_none()));
    match alive {
        Some(o) => Brief::from_parts(
            Some(o.core.function),
            Some(o.core.tech),
            Some(o.core.notability),
            Some(o.core.people),
            o.core.peak_population,
            built,
            cold,
        ),
        None => Brief::from_parts(None, None, None, None, 0, built, cold),
    }
}
```

`occs.iter().find(...)` over a `Vec` sorted by `layer_key` finds the same
first living record `into_iter().find(...)` did over the removed `Vec` — the
order is the map's own and unchanged. If `Occupation`'s fields are not
`Copy` and the compiler objects to moving out of `o.core.*`, write
`o.core.function.clone()` etc.; do not restructure.

- [ ] **Step 5: Build, then run the ratchet and the vessel suite**

Run: `cargo build -p hornvale-vessel --all-targets 2>&1 | tail -5`
Expected: clean. Then:

Run: `cargo test -p hornvale-vessel --test suite > /tmp/hv.log 2>&1; grep -E "^test result|FAILED|panicked|\.\.\. FAILED" /tmp/hv.log`
Expected: every `test result: ok`; `no_session_path_re_surveys_the_occupation_register` PASSES.

Run: `cargo test -p hornvale-vessel --lib > /tmp/hv-lib.log 2>&1; grep -E "^test result|FAILED" /tmp/hv-lib.log`
Expected: `test result: ok`.

- [ ] **Step 6: Regenerate the type-audit report, fmt, gate, commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add windows/vessel/tests/suite/the_terrier.rs windows/vessel/tests/suite.rs \
        windows/vessel/src/session.rs windows/vessel/src/brief.rs \
        docs/audits/type-audit-report.md
git commit -F <message file>
```

Message: `feat(vessel): the occupation register lives on WorldContext, built once` with a body naming the measured per-call cost and the ratchet. The pre-commit hook runs `make gate-commit`. If it refuses on `placement-audit` or `plumb`, read the message and tag as it says — do not touch the substance. Then commit the `docs/timings.md` row separately. Push.

### Task 2: VIEW ≡ SCAN for the register (P6), in-module

**Files:**
- Modify: `windows/vessel/src/session.rs` — the in-module `mod tests`
  (search for `fn the_carried_lattice_is_the_one_the_place_derives` to find
  the neighbourhood; add the new test beside it)

**Interfaces:**
- Consumes: `Session::brief_here(&self)` (private; in-module), `Session::handle`,
  `Session::terrain_here`, `Session::position`, `Session::walk_depth`,
  `self.wctx.ctx`, `self.world`, `crate::brief::brief_of`,
  `hornvale_worldgen::seed_42_world()` (the committed fixture, decision
  0607) and `hornvale_worldgen::build_world(Seed(7), …)` — copy
  `tests/suite/the_rack.rs:28-40`'s `world_at` shape into the module if no
  equivalent helper exists there already (grep the test module for
  `fn seam_world` / `fn world_at` first and reuse whichever builds a full
  seed).

- [ ] **Step 1: Write the test**

```rust
    /// The Terrier, spec §4 P6: the brief read off the hoisted register equals
    /// the brief read off a FRESH `occupations_by_vertex(world)` at every
    /// locale a script visits — VIEW ≡ SCAN for the one world-scoped read that
    /// used to be done per call.
    ///
    /// Non-vacuity is asserted in both directions: the script must visit at
    /// least one locale with a living occupation (the flagship) and at least
    /// one without (open ground on another vertex), so a hoist that dropped
    /// the map reds on the first and one that returned a stale "alive" reds
    /// on the second. Seed 42 for the fixture; seed 7 as a second world whose
    /// vertices are not seed 42's.
    ///
    /// The walk north is bounded and checked rather than assumed: a room one
    /// step north may share the flagship's containing vertex, so the script
    /// keeps stepping until the brief reports no living occupation, and fails
    /// loudly if twelve steps never leave the vertex — that is the moment to
    /// choose a different bearing, not to weaken the assertion.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: in `WorldContext::build`, replace
    /// `hornvale_worldgen::occupations_by_vertex(world)` with
    /// `BTreeMap::new()`. Observed red: <paste>.
    #[test]
    fn the_hoisted_brief_is_the_fresh_brief_at_every_visited_locale() {
        for seed in [42u64, 7] {
            // `seam_world()` is the committed seed-42 fixture (decision 0607);
            // the module's own `world_at` builds any other seed and returns
            // an `Option` (`session.rs:10634`).
            let world = if seed == 42 {
                seam_world()
            } else {
                world_at(seed).expect("seed 7 builds under default pins")
            };
            let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
            let fresh = hornvale_worldgen::occupations_by_vertex(&world);
            assert_eq!(
                session.wctx.occupations, fresh,
                "seed {seed}: the hoisted register is not the fresh one"
            );
            let mut saw_alive = false;
            let mut saw_none = false;
            let mut steps = 0;
            loop {
                let hoisted = session.brief_here();
                let terrain = session.terrain_here();
                let scanned = crate::brief::brief_of(
                    &fresh,
                    session.wctx.ctx.climate().geosphere(),
                    session.wctx.ctx.nearest_index(),
                    &session.position(),
                    &terrain,
                    session.walk_depth(),
                );
                assert_eq!(
                    hoisted, scanned,
                    "seed {seed}, step {steps}: the hoisted brief disagrees with the scan"
                );
                if hoisted.function.is_some() {
                    saw_alive = true;
                } else {
                    saw_none = true;
                }
                if saw_alive && saw_none {
                    break;
                }
                assert!(
                    steps < 12,
                    "seed {seed}: twelve steps north never left the flagship's vertex \
                     (alive={saw_alive}, none={saw_none}); pick another bearing"
                );
                let _ = session.handle("go n");
                steps += 1;
            }
        }
    }
```

If `PartialEq` is not derived on `OccupationRecord`'s map value (it is —
`record.rs:202` derives `Clone, Debug, PartialEq`), compare lengths and
per-vertex `id`s instead and say so in the doc.

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-vessel --lib the_hoisted_brief_is_the_fresh_brief > /tmp/hv-lib.log 2>&1; grep -E "^test result|FAILED|panicked|vertex" /tmp/hv-lib.log`
Expected: PASS at both seeds. If the twelve-step guard fires, the flagship's
vertex is wide on that seed: change the bearing (`go e`) for that seed and
record which in the doc. If seed 42's fixture puts the possession where
`go n` is refused (water, a wall), read the reply and pick the bearing the
reply offers.

- [ ] **Step 3: Run the mutation, record the red, restore**

Apply the prescribed mutation with `scripts/mutate.py` (it refuses unless
the target is found and unique), run the test, paste the observed red into
the doc's `Observed red:` line, restore the file (`git checkout --
windows/vessel/src/session.rs` is NOT safe here — it would discard Step 1;
restore by re-applying the original line with `mutate.py` in reverse or by
editing), and re-run to green. If the mutation is a null (it cannot be —
an empty map makes every brief `function: None`, and the flagship's must be
`Some`), say so and find one that reds.

- [ ] **Step 4: fmt, gate, commit, push**

```bash
cargo fmt
git add windows/vessel/src/session.rs
git commit -F <message file>   # test(vessel): the hoisted brief equals the fresh one at every visited locale
```

Commit the timings row separately. Push.

**Stage boundary.** `make sluice-stage BRANCH=campaign/the-terrier REF=$(git rev-parse HEAD)`. Wait for `reported`, read `make sluice-log`. A red on `the_terrier` or the new in-module test is this campaign's; a red elsewhere is triaged before Stage 2, not skipped.

---

## Stage 2 — the readings and the corrections

### Task 3: Measure, and correct the two bench headers

**Files:**
- Modify: `windows/vessel/examples/move_cost.rs` — add a
  `## Measured — AFTER The Terrier` block ABOVE the existing
  `## Measured — AFTER the purview fix` block (newest first, as the file
  is ordered), and rewrite the AFTER-Rack interpretation paragraph
  (`:110-116`, "**The chamber band shows what a shadowcast costs …**")
- Modify: `clients/game/bin/examples/move_cost.rs` — add a
  `### AFTER The Terrier, default (\`dev\`) profile — P6` block after the
  AFTER-Rack block (`:80-134`), and correct the sentence "That is the same
  shadowcast cost …" (`:113-116`)

**Interfaces:** none new. Both examples run unchanged.

- [ ] **Step 1: Take the native reading**

Run: `uptime; cargo run --release -p hornvale-vessel --example move_cost > /tmp/mc-vessel.txt 2>&1; uptime`
Read `/tmp/mc-vessel.txt` in full. Record the two `uptime` lines. Quiet =
all three averages under 4; otherwise the block says CONTENDED and quotes
them.

- [ ] **Step 2: Take the client reading**

Run: `uptime; cargo run --manifest-path clients/game/bin/Cargo.toml --example move_cost > /tmp/mc-client.txt 2>&1; uptime`
(The default profile is the one the player launches, and the one P6 was
preregistered on.)

- [ ] **Step 3: Write the vessel block**

Insert, in the file's existing `//!` house style, a block that carries:
the date, box, SHA, profile, both `uptime` lines and the CONTENDED/quiet
verdict; the verbatim program output; then a **Verdict** paragraph
against spec §4 P2 and P4 with these decision rules applied
row-for-row:

- chamber `snapshot()` after `map` / `go n/e/s/w`: was 16.3–16.8 ms; P2
  line ≤ 3 ms.
- `enter` handle: was 33.7 ms; P2 line ≤ 3 ms.
- chamber `look` handle: was 16.5 ms; P2 line ≤ 1 ms.
- `Session::start`: P4 line ≤ +30 ms over the AFTER-Rack block's
  `Session::start 845 ms`, stated as a difference with its sign.
- Every outdoor row: the control; report whether it moved beyond noise.

State MET or MISSED per line. A MISS is reported with the number, never
tuned; if a chamber row misses P2 with the ratchet green, restore the
scratch prints (`derive_sighting`, `chamber_plan`, `brief_here`) for one
run, paste the split, and remove them before committing.

Then rewrite the AFTER-Rack paragraph (`:110-116`) so its numbers stand and
its reading is corrected in place:

```text
//! **The chamber band shows what a BRIEF costs, because the memo makes it
//! visible — and this paragraph used to say "one shadowcast".** 8.4 ms
//! after `enter`/`look`, 16.3-16.8 ms after `map` or a chamber `go` — same
//! chamber, same turn shape. `look` derives a `Session::sighting` for its
//! presence line and the snapshot on that turn reuses it (The Rack, Task 4);
//! `map` and `go n` derive none, so the snapshot pays for its own. The
//! difference, ~8 ms, was the sighting DERIVATION, and The Rack named it
//! after the step it is named for. Decomposed by The Terrier (2026-09-03):
//! the shadowcast at `SIGHT_RADIUS` 4 is 0.011-0.013 ms; the 8 ms was
//! `brief::brief_of` rebuilding `occupations_by_vertex(world)` — the whole
//! world's occupation register — on every call, once inside the sighting
//! and once more inside `chamber_sources`. See the AFTER-Terrier block
//! above for what a chamber snapshot costs with the register hoisted.
```

- [ ] **Step 4: Write the client block**

Same house style as the AFTER-Rack block: date, box, SHA, `uptime` before
and after with the verdict, the verbatim output, then P6 restated with its
rule — "**The outdoor range, stated once with its rule:** min and max over
EVERY outdoor row, `needs` included, rounded half-up to two decimals" —
and, for the indoor rows, MET/MISSED against ≤ 15 ms per row (`enter`, the
two chamber `look`s, `map`, `go n/e/s/w`). Correct `:113-116` in place:

```text
//! That was NOT a shadowcast cost, and this sentence used to say it was:
//! The Terrier (2026-09-03) decomposed the derivation and found the
//! shadowcast at 0.012 ms and the whole item in `brief_of` rebuilding the
//! world's occupation register per call — see the AFTER-Terrier block
//! below and `windows/vessel/examples/move_cost.rs`.
```

- [ ] **Step 5: Verify both examples still compile under the client gate's clippy, commit, push**

Run: `cargo clippy -p hornvale-vessel --all-targets -- -D warnings 2>&1 | tail -3` and
`cargo clippy --manifest-path clients/game/bin/Cargo.toml --all-targets -- -D warnings 2>&1 | tail -3`
Expected: clean (doc-comment-only edits).

```bash
cargo fmt
git add windows/vessel/examples/move_cost.rs clients/game/bin/examples/move_cost.rs
git commit -F <message file>   # docs(bench): the AFTER-Terrier readings; the 8 ms was the brief, not the cast
```

Push.

### Task 4: The corrections, the registry, decision 0636, rebaseline

**Files:**
- Modify: `book/src/frontier/idea-registry.md` (`:771`, `:774`; new PROC row
  in the PROC table — find `^| PROC-` rows and append in the same table)
- Modify: `book/src/chronicle/the-rack.md` (`:100`, `:243`)
- Modify: `docs/retrospectives/the-rack.md` (`:179`)
- Create: `docs/decisions/0636-a-world-scoped-derivation-lives-on-worldcontext.md`
- Regenerate: `docs/digest/decisions-in-force.md`,
  `docs/digest/intent-vs-reality.md`, `docs/audits/*` via `make rebaseline`

**Interfaces:** none. Prose and one decision record.

- [ ] **Step 1: Correct the registry**

Rewrite the body of `TOOL-chamber-snapshot-prices-a-shadowcast` (`:774`)
— the ID is permanent and stays — to:

```text
| TOOL-chamber-snapshot-prices-a-shadowcast | **CORRECTED BY THE TERRIER (2026-09-03): the ~8 ms was never the shadowcast.** This row was minted by The Rack from the difference between a chamber snapshot after `look` (8.4 ms, sighting memo hit) and after `map`/`go` (16.3-16.8 ms, memo miss), and named the step the derivation is named for. Decomposed: the shadowcast at `SIGHT_RADIUS` 4 is **0.011-0.013 ms**; `anchor_cells` under 0.1 ms; the whole 8.7-28.8 ms per call was `brief::brief_of` rebuilding `occupations_by_vertex(world)` — the world's entire occupation register — on EVERY call, two to five times per indoor turn (`enter` five). Its own `NOTE ON COST` had prescribed the hoist since 2026-07-27. Shipped: the register lives on `WorldContext`, built once; `brief_of` takes it; a source ratchet forbids a whole-world occupation read on any session path. The lesson is [[PROC-a-two-point-difference-names-a-step-not-a-cost]] | shipped | high (measured) | [The Terrier chronicle](../chronicle/the-terrier.md); [The Terrier spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-03-the-terrier-design.md) §1; decision 0636 |
```

In `TOOL-tick-profile-2026-08` (`:771`) replace the final clause "and a
chamber snapshot prices one shadowcast at ~8 ms" with "and a chamber
snapshot priced one BRIEF at ~8 ms — The Rack wrote 'one shadowcast' here
and The Terrier (2026-09-03) measured the cast at 0.012 ms; see
[[TOOL-chamber-snapshot-prices-a-shadowcast]]".

Add to the PROC table:

```text
| PROC-a-two-point-difference-names-a-step-not-a-cost | **A cost read off the difference between two timings is the WHOLE derivation between them, and it will be filed under the name of one step.** The Rack subtracted a memo-hit snapshot from a memo-miss one, got ~8 ms, and wrote "one shadowcast" into a chronicle, a retrospective, a registry row and two bench headers; a campaign was then opened on the row. Ten scratch `Instant` prints showed the cast at 0.012 ms and all of the cost in a whole-world ledger scan whose own comment had prescribed the fix for five weeks. Arithmetic alone would have caught it (a radius-4 cast visits ≤ 81 squares). Before a cost carries a step's NAME in a committed artifact, decompose the derivation; when a campaign is opened on a measured row, re-measure the split before designing | raw | high (measured) | [The Terrier chronicle](../chronicle/the-terrier.md); [The Terrier retrospective](https://github.com/hornvale/hornvale/blob/main/docs/retrospectives/the-terrier.md) |
```

Run: `cargo test -p hornvale --test suite docs_consistency > /tmp/hv-docs.log 2>&1; grep -E "^test result|FAILED|panicked" /tmp/hv-docs.log`
Expected: `ok` (unique IDs, resolvable wikilinks, legal statuses). The
chronicle link to `the-terrier.md` may be checked for existence — if
`docs_consistency` reds on a missing chronicle, create the chronicle stub
in Task 5's shape now (title line only is not enough for the book; write
the real chronicle in Task 5 and order the commits so the link resolves).

- [ ] **Step 2: Correct The Rack's chronicle and retrospective in place**

`book/src/chronicle/the-rack.md:100` — after the sentence ending "a better
argument for sharing it than anything available before measuring." append
a new paragraph:

```text
**Corrected by [The Terrier](./the-terrier.md), 2026-09-03: the 8 ms was
not the shadowcast.** The difference above is one sighting *derivation*,
and this chronicle named it after the step the derivation is named for.
Decomposed, the cast at radius four is 0.012 ms; the eight milliseconds
were the brief — `brief_of` rebuilding the world's whole occupation
register from the ledger on every call, twice per chamber snapshot — under
a cost note in its own source that had prescribed the hoist since July.
The number was right. The noun attached to it was a guess, and it became a
campaign brief.
```

`:243` (Honest limits) — after "and that is the largest single item left."
append: "*(Corrected 2026-09-03: the item was the brief's occupation
register, not the cast — see above and The Terrier.)*"

`docs/retrospectives/the-rack.md:179` — after "which needs `chamber_plan` to
want the sighting it already has." append: " **Corrected by The Terrier:**
the ~8 ms was `brief_of` re-surveying the world's occupation register per
call, not the shadowcast (0.012 ms); the cheap lever named here would have
saved one of two brief calls per snapshot and left the other."

- [ ] **Step 3: Write decision 0636**

Create `docs/decisions/0636-a-world-scoped-derivation-lives-on-worldcontext.md`
in the house shape (`0598`'s sections: title line, `**Status:** Accepted
(2026-09-03) · **Decider:** Nathan · **Campaign:** The Terrier`, then
`## Context`, `## The decision`, `## Consequences`, `## See also`):

- Context: `brief_of` rebuilt `occupations_by_vertex(world)` per call;
  measured 8.7–28.8 ms contended, 2–5 calls per indoor turn; the cost note
  from `4569d883d`; `WorldContext` (The Quire) is where terrain, climate,
  the locale context and the demography report already live for the same
  reason.
- The decision: **a derivation that is a pure function of the immutable
  `World` — reads `world.ledger` or the sculpted fields, draws nothing — is
  built once on `WorldContext` and read from there; no session path rebuilds
  one.** The guard is a structural source scan
  (`tests/suite/the_terrier.rs`), because a counter for a thing that no
  longer happens has no writer (0598's own argument). Memoising inside the
  derivation is still forbidden, for the cost note's reason.
- Consequences: `Session::start` pays the map once (measured, Task 3);
  `repossess` pays nothing; a future world-scoped read that appears on a
  turn path is a ratchet red, not a profile finding; the direction the scan
  enforces (presence, not completeness) and the VIEW ≡ SCAN test that
  covers the other half.
- See also: 0092 (named construction sites), 0598, The Quire's chronicle,
  the spec.

- [ ] **Step 4: Rebaseline and apply the decision rules**

Run: `make rebaseline > /tmp/rebaseline.log 2>&1; tail -5 /tmp/rebaseline.log`
Then: `git status --short -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`

Decision rules (spec §4 P5): only `docs/audits/` and `docs/digest/` moved →
`git add` them with this commit. Anything else in the list moved → STOP,
report the path and the diff's first hunk to the controller; do not
commit it.

- [ ] **Step 5: Commit and push**

```bash
git add book/src/frontier/idea-registry.md book/src/chronicle/the-rack.md \
        docs/retrospectives/the-rack.md docs/decisions/0636-*.md \
        docs/digest/ docs/audits/
git commit -F <message file>   # docs(terrier): the corrections, the register rows, decision 0636
```

Push.

### Task 5: Close — chronicle, retrospective, follow-ups, census, merge

**Files:**
- Create: `book/src/chronicle/the-terrier.md`; modify `book/src/SUMMARY.md`
  (add `- [The Terrier](./chronicle/the-terrier.md)` after The Rack's line, or
  after whichever chronicle is newest — read the list's order first)
- Create: `docs/retrospectives/the-terrier.md`; modify
  `docs/retrospectives/README.md` (one line, in the README's shape at `:230`)
- Modify: `docs/superpowers/ledgers/2026-09-03-the-terrier.md` (the
  follow-up register entries and the close entries)

- [ ] **Step 1: Write the chronicle** at the book's altitude (technical,
  comprehensible without the code). Sections, in this order: the finding
  (what the 8 ms was, with the decomposition table); the shape of the fix
  (the register on the context, read by the brief; why not a memo; why not
  dedupe); the numbers (Task 3's readings, MET/MISSED per line, contended
  or quiet stated); the ratchet and its direction; what the correction
  touched and why in place; honest limits (the walk-band 4.2 ms JSON floor
  is untouched; `session_cost`'s Mac-keyed ceilings are untouched; `enter`
  still makes four brief calls, each now microseconds — the number).
  Book titles are code-generated — check `book/src/chronicle/` for how the
  title line is written in the newest entry and match it.

- [ ] **Step 2: Write the retrospective** — process, not product: the
  attribution lesson (a two-point difference; the row became a brief); the
  fix was in the source's own comment for five weeks (a `NOTE ON COST` is
  a latent finding — grep for them); the campaign was renamed once the
  measurement contradicted the brief (worth the friction); the box was
  contended for every reading (state what that cost the numbers); deferred
  minors from each task's review, each with its outcome; follow-ups with
  reasons (the 2–5 per-turn brief calls with the post-hoist per-call
  figure; `chamber_interior_here`'s 17 sites; `CLIENT-cache-demography-report`
  as the next largest thing in `build`).

- [ ] **Step 3: Confidence Gradient** — read `book/src/open-questions.md`;
  no bet is expected to move. If one does, re-score it and say why.

- [ ] **Step 4: Commit, push, queue the census, then the merge**

```bash
git add book/src/chronicle/the-terrier.md book/src/SUMMARY.md \
        docs/retrospectives/the-terrier.md docs/retrospectives/README.md \
        docs/superpowers/ledgers/2026-09-03-the-terrier.md
git commit -F <message file>   # docs(terrier): chronicle and retrospective
git push
```

Then absorb main once more (`git merge origin/main`; regenerate any
aggregate that conflicts — `docs/audits/type-audit-report.md` and the
digest are regenerated, never text-merged; a registry row that both sides
edited is checked by eye for a duplicate ID), run `make gate-commit`,
push, and submit:

```bash
make sluice-census BRANCH=campaign/the-terrier REF=$(git rev-parse HEAD)
make sluice BRANCH=campaign/the-terrier REF=$(git rev-parse HEAD)
```

The merge commit needs a `Sluice-Headline:` trailer in the same block as
`Claude-Session:` with no blank line between them. Follow
`closing-a-campaign` from here; the census result (expected: zero columns
moved, and the positive control is that NOTHING else moved either — state
that a null with no control is not a result, and that here the control is
P5's clean drift diff) goes in the chronicle's numbers section as an
amendment if it lands after the chronicle is written.
