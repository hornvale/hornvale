# The Gnomon Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Install three instruments that read the project's own structure — a
first-occurrence index over the fact ledger, a per-world anomaly report over
the census, and a `refuted` status in the registry's closed vocabulary.

**Architecture:** Nothing new is built where something existing can be turned
ninety degrees. The index ships as census metric columns, because the census
plus duckdb already *is* an inverted index once the columns exist. The anomaly
report is the Domesday's transpose — same committed census, same percentiles
from `domesday::stats`, axis rotated from "column across worlds" to "world
across columns" — and implements no statistics of its own. The status change is
three consumer edits plus a decision record.

**Tech Stack:** Rust 2024, `windows/lab` (crate `hornvale-lab`), `cli` (crate
`hornvale`), duckdb via `make census-check`, mdbook.

**Spec:** `docs/superpowers/specs/2026-08-13-the-gnomon-design.md` — read it
first; this plan argues from it and does not restate its measurements.

## Global Constraints

- **No new percentile, census-loading, or ratchet implementation.** Spec §1.1.
  This campaign implements an essay whose headline finding is that five tools
  instantiate one pattern and the fifth was written anyway. A task that finds
  itself writing `fn percentile`, `fn load_census`, or a fourth append-only
  waiver list has gone wrong — stop and report rather than proceeding.
- **Dependencies**: `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec`. Float sorting uses
  `total_cmp`. Enforced workspace-wide by `clippy.toml`.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- Every crate sets `#![warn(missing_docs)]`; every public item, field and
  variant gets a one-line doc comment.
- `cargo fmt` as the final step before every commit.
- **Frozen constants, declared as frozen**: `k = 10` (columns per world),
  `tail_depth <= 0.01` (selection bar), `top 25` (worlds published),
  `recall@10 >= 0.60` (H1 success bar). Retuning any of these after seeing a
  result is a post-unblinding change and must be counted as one in the
  chronicle.
- **The census cost is read from `docs/timings.md`**, never from `CLAUDE.md`
  and never from this plan: `grep '| census |' docs/timings.md | tail`.
- **Censuses and the heavy tier run on lefford**, dispatched with a full SHA.
  The guard refuses elsewhere (decisions 0063/0079/0081).

---

## File Structure

| File | Responsibility |
|---|---|
| `windows/lab/src/metrics.rs` (modify) | The 19 `first-day-*` metric declarations and their shared extraction helper |
| `windows/lab/src/domesday/anomaly.rs` (create) | Per-world tail-depth scoring over the committed census. Pure read; no world construction |
| `windows/lab/src/domesday/mod.rs` (modify) | Wire the module; extend the `loading_never_builds_a_world` guard to cover it |
| `windows/lab/src/domesday/render.rs` (modify) | Render `anomalies.md` |
| `windows/lab/tests/anomaly_injection.rs` (create) | H1's injection battery. Heavy tier |
| `windows/lab/tests/anomaly_holdout.rs` (create) | H2's calibration control. Heavy tier |
| `cli/src/main.rs` (modify) | `lab anomalies [--seed N]` |
| `scripts/regenerate-artifacts.sh` (modify) | Render the anomalies page after the Domesday |
| `book/src/domesday/anomalies.md` (generated) | The committed artifact |
| `book/src/SUMMARY.md` (modify) | Book nav entry |
| `cli/tests/docs_consistency.rs` (modify) | `REGISTRY_STATUSES`; the required-parenthetical check |
| `book/src/frontier/idea-registry.md` (modify) | Vocabulary docs; the reclassified rows |
| `book/src/frontier/CLAUDE.md` (modify) | Authoring rule |
| `docs/decisions/0131-refuted-is-a-seventh-registry-status.md` (create) | The record |

Task order is deliberate: **Task 6 (the status) is independent of Tasks 1–5**
and can be done first if the lab work stalls. Tasks 2–5 depend on Task 1 only
for the census columns existing, and Task 4's report does not read them at all.

---

## Task 1: The first-occurrence extraction helper and one metric

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Test: `windows/lab/src/metrics.rs` (in-module `#[cfg(test)]`, following the
  file's existing convention)

**Interfaces:**
- Produces: `fn first_day(world: &World, predicate: &str, object: Option<&str>)
  -> MetricValue` — returns `MetricValue::Number(day)` for the smallest
  `Fact.day` among facts matching the predicate (and, when `object` is `Some`,
  whose object is `Value::Text` equal to it), or `MetricValue::Absent` when no
  fact matches or no matching fact carries a day.
- Produces: the metric name `first-day-is-settlement`.

- [ ] **Step 1: Establish which view rung carries the history bake's days.**

Do not assume. Write a scratch probe (a `#[test]` you delete in Step 3, or a
one-off `cargo run`) that builds seed 42 and prints, for `is-settlement`, the
count of facts and the count of distinct non-genesis `day` values at
`BuildDepth::Settlements` and at `BuildDepth::Full`.

**All nineteen metrics use `Extractor::Full` regardless of what the probe
finds** — ruled at preflight, see the ledger. The census selects `"metrics":
"all"` and therefore already builds to Full, so a shallower rung saves nothing
there; and the in-module test helper `extract(&FullView, name)`
(`windows/lab/src/metrics.rs:12066`) panics on a non-Full metric, so a mixed
roster would need two different test helpers for one roster. Uniformity wins.

The probe is therefore for the **record**, not for rung selection:

- Non-genesis days appear at `Full` → note at which depth they first appear and
  proceed.
- No non-genesis days at `Full` → **STOP and report.** The spec's §2.2
  measurement was taken on a `hornvale new` build; a divergence here means the
  lab's view chain differs from the CLI's, and that is a finding worth more
  than this task.

- [ ] **Step 2: Write the failing test**

Add to the `#[cfg(test)]` module in `windows/lab/src/metrics.rs`:

```rust
#[test]
fn first_day_is_settlement_is_present_and_finite_on_seed_42() {
    let v = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    match extract(&v, "first-day-is-settlement") {
        MetricValue::Number(d) => assert!(d.is_finite(), "a first day must be finite, got {d}"),
        other => panic!("expected a Number, got {other:?}"),
    }
}

#[test]
fn first_day_of_an_unmatched_object_is_absent() {
    let v = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    assert!(
        matches!(
            first_day(v.world(), "occ-people", Some("no-such-species")),
            MetricValue::Absent
        ),
        "an object that never occurs must be Absent, never 0.0"
    );
}
```

Match the file's own construction and extraction helpers — `FullView::build`
and the local `extract` / `m(...)` helper used by neighbouring tests. Read
those before writing; the names above are the shape, and the file is the
authority on the spelling.

- [ ] **Step 3: Run it and confirm it fails for the right reason**

Run: `cargo test -p hornvale-lab --lib first_day`
Expected: a **compile** failure naming `first_day` / the unknown metric.

Note for the record: a red from a compile error proves the symbol is absent,
nothing more. That is the correct and only claim at this point.

- [ ] **Step 4: Implement the helper**

```rust
/// The earliest `Fact.day` among facts matching `predicate` — and, when
/// `object` is `Some`, whose object is that exact text. `Absent` when nothing
/// matches, or when no matching fact is time-bound.
///
/// Absent is deliberately distinct from `Number(0.0)`: a key that never occurs
/// in a world and a key that occurs at genesis are different facts about that
/// world, and collapsing them would make the census unable to express the
/// difference.
fn first_day(world: &World, predicate: &str, object: Option<&str>) -> MetricValue {
    let mut best: Option<f64> = None;
    for f in world.ledger.find(predicate) {
        if let Some(want) = object {
            match &f.object {
                Value::Text(t) if t == want => {}
                _ => continue,
            }
        }
        let Some(d) = f.day else { continue };
        best = Some(match best {
            Some(b) if b <= d.day() => b,
            _ => d.day(),
        });
    }
    match best {
        Some(d) => MetricValue::Number(d),
        None => MetricValue::Absent,
    }
}
```

Confirm `MetricValue::Absent` is the correct variant name by reading the enum
at `windows/lab/src/metrics.rs:725` before writing this; use whatever the enum
actually calls it.

- [ ] **Step 5: Declare the first metric**

Add to the roster alongside the existing entries, following the exact
declaration form of `settlement-count` (`windows/lab/src/metrics.rs:1956`):

```rust
Metric {
    name: "first-day-is-settlement",
    doc: "Earliest world-day on which any settlement existed; \
          Absent if the world has none",
    summary: SummaryKind::Numeric {
        bucket_edges: &[0.0, 50_000.0, 150_000.0, 300_000.0, 500_000.0, 750_000.0],
    },
    domain: Domain::History,
    role: Role::Descriptor,
    extract: Extractor::Full(|v: &FullView| first_day(v.world(), "is-settlement", None)),
},
```

Use the extractor variant Step 1 established.

- [ ] **Step 6: Run and confirm green**

Run: `cargo test -p hornvale-lab --lib first_day`
Expected: PASS.

- [ ] **Step 7: Measure the blast radius — do not estimate it**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-gnomon-t1.txt`, then
grep the file. Record in the task report the **actual** count and names of
tests that reddened from adding one metric.

**Decision rule:**
- Only schema/count assertions reddened → update them and continue.
- A test reddened that asserts on *world content* → **STOP and report**. A new
  metric must not change any world; that would mean the extractor is mutating
  something.

- [ ] **Step 8: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/lab/src/metrics.rs
git commit -m "feat(lab): first-day extraction over the ledger, and the first index column"
```

---

## Task 2: The rest of the frozen roster — 19 columns

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Test: `windows/lab/src/metrics.rs` (in-module)

**Interfaces:**
- Consumes: `first_day` from Task 1.
- Produces: 19 metric names, listed below. Task 3's queries and Task 5's
  documentation reference these exact strings.

**The roster is authored and frozen. Do not derive it from observed objects** —
a roster that grows when a world happens to contain a new species makes the
census schema seed-dependent (spec §2.4).

- [ ] **Step 1: Enumerate the closed value sets from CODE, not from a world**

The spec's seed-42 measurement saw three `occ-cause` values. The emitting
match arm at `windows/worldgen/src/history_emit.rs:136` (`fn cause_label`) has
**five**: `famine`, `burned`, `plague`, `fled`, `migrated`. One world is an
anecdote; the code is the enumeration.

Confirm the same way for tech horizons (`TechHorizon` at
`domains/history/src/record.rs:41`: `neolithic`, `bronze`, `iron`,
`classical`) and read the emitting label function rather than the enum's
variant names — the label is what lands in the ledger.

- [ ] **Step 2: Write the roster-completeness test first**

```rust
/// The frozen roster. Adding to this list is a deliberate act: it widens the
/// census schema, so it is asserted here rather than derived.
const FIRST_DAY_METRICS: [&str; 19] = [
    // event onset, unkeyed
    "first-day-is-settlement",
    "first-day-is-ruin",
    "first-day-pays-tribute-to",
    "first-day-is-person",
    "first-day-person-died",
    "first-day-person-founded",
    // keyed on occ-people, on the four species the census already keys on
    "first-day-occ-people-goblin",
    "first-day-occ-people-kobold",
    "first-day-occ-people-hobgoblin",
    "first-day-occ-people-bugbear",
    // keyed on occ-tech, the closed TechHorizon set
    "first-day-occ-tech-neolithic",
    "first-day-occ-tech-bronze",
    "first-day-occ-tech-iron",
    "first-day-occ-tech-classical",
    // keyed on occ-cause, the closed cause_label set
    "first-day-occ-cause-famine",
    "first-day-occ-cause-burned",
    "first-day-occ-cause-plague",
    "first-day-occ-cause-fled",
    "first-day-occ-cause-migrated",
];

#[test]
fn every_first_day_metric_is_registered() {
    for name in FIRST_DAY_METRICS {
        assert!(
            registry().iter().any(|m| m.name == name),
            "the frozen first-occurrence roster names {name}, which is not registered"
        );
    }
}

#[test]
fn every_registered_first_day_metric_is_in_the_frozen_roster() {
    for m in registry().iter().filter(|m| m.name.starts_with("first-day-")) {
        assert!(
            FIRST_DAY_METRICS.contains(&m.name),
            "{} is registered but absent from the frozen roster — widening the \
             census schema is deliberate, so add it to FIRST_DAY_METRICS on purpose",
            m.name
        );
    }
}
```

Both directions are required and the doc comment must say which each enforces.
A one-directional check (`declared ⊆ registered`) is structurally blind to
over-admission and still reads as total to the next person.

The roster accessor is `registry()` (re-exported at `windows/lab/src/lib.rs:27`);
verified at preflight, so use it as written.

- [ ] **Step 3: Run and confirm it fails**

Run: `cargo test -p hornvale-lab --lib first_day_metric`
Expected: FAIL naming the 18 unregistered metrics.

- [ ] **Step 4: Declare the remaining 18**

Follow Task 1 Step 5's form exactly for each. The keyed ones pass
`Some("<object>")`:

```rust
Metric {
    name: "first-day-occ-people-kobold",
    doc: "Earliest world-day on which any occupation was held by kobolds; \
          Absent if kobolds never occupy a site",
    summary: SummaryKind::Numeric {
        bucket_edges: &[0.0, 50_000.0, 150_000.0, 300_000.0, 500_000.0, 750_000.0],
    },
    domain: Domain::History,
    role: Role::Descriptor,
    extract: Extractor::Full(|v: &FullView| first_day(v.world(), "occ-people", Some("kobold"))),
},
```

`Domain::History` for all nineteen: the Domesday chapter axis is what the
metric is *about*, and every one of these is about deep history. Do not split
them across `Demography` and `History` — the split would put two halves of one
roster on two Book pages.

- [ ] **Step 5: Run and confirm green**

Run: `cargo test -p hornvale-lab --lib first_day`
Expected: PASS, both directions.

- [ ] **Step 6: Confirm the Absent path is exercised, not merely written**

Run the roster over seed 42 and record which of the 19 are `Absent`. At least
one should be — the four keyed species cannot all occupy a world where seed 42
showed hobgoblin, gnoll, kobold, snow-elf, desert-dwarf and bugbear but not
goblin.

**Decision rule:** if all 19 are present on seed 42, the `Absent` branch is
untested by this fixture; add a seed where at least one key is missing, chosen
by probing, not guessed.

- [ ] **Step 7: `cargo fmt`, full suite, commit**

```bash
cargo fmt
cargo nextest run --workspace 2>&1 | tee /tmp/hv-gnomon-t2.txt
git add windows/lab/src/metrics.rs
git commit -m "feat(lab): the frozen first-occurrence roster — 19 index columns"
```

---

## Task 3: The conjunction query, proving the index is an index

**Files:**
- Create: `tools/census/queries/explore/emergent-conjunction.sql`
- Modify: `book/src/laboratory/` documentation page for the census queries (find
  it by `grep -rl 'interesting-worlds' book/`)

**Interfaces:**
- Consumes: the 19 metric names from Task 2.

This task is what makes the campaign's central claim checkable: that the census
*is* the inverted index. It cannot run until a census refresh has landed
(Task 7), so the query is authored here and **verified in Task 7**.

- [ ] **Step 1: Author the query**

```sql
-- The emergent conjunction: worlds where kobolds occupied a site, a ruin
-- exists, and tribute was paid -- and the world-time a replay must start from
-- to see all three. greatest() IS the maximum over three days; IS NOT NULL IS
-- the set intersection. This is the whole of TOOL-first-occurrence-index's
-- promise, expressed against the census rather than a bespoke index.
SELECT seed,
       greatest("first-day-occ-people-kobold",
                "first-day-is-ruin",
                "first-day-pays-tribute-to") AS replay_from
FROM "the-census"
WHERE "first-day-occ-people-kobold" IS NOT NULL
  AND "first-day-is-ruin"           IS NOT NULL
  AND "first-day-pays-tribute-to"   IS NOT NULL
ORDER BY replay_from
LIMIT 25;
```

- [ ] **Step 2: Document it beside its neighbours**

Add a paragraph to the page that documents `interesting-worlds.sql`, stating
the contrast explicitly: that file's header says *"'interesting' is a query,
not a generation stage"* and requires you to know the question; this one
answers a question about *when*, which no census column could answer before.

- [ ] **Step 3: Commit**

```bash
git add tools/census/queries/explore/emergent-conjunction.sql book/src/laboratory/
git commit -m "feat(census): the emergent-conjunction query — the index's payoff, in SQL"
```

---

## Task 4: The anomaly report

**Files:**
- Create: `windows/lab/src/domesday/anomaly.rs`
- Modify: `windows/lab/src/domesday/mod.rs`
- Modify: `windows/lab/src/domesday/render.rs`
- Modify: `cli/src/main.rs`
- Modify: `scripts/regenerate-artifacts.sh`
- Modify: `book/src/SUMMARY.md`
- Generated: `book/src/domesday/anomalies.md`

**Interfaces:**
- Consumes: `domesday::census::{load, Census}`, `domesday::stats` — **reuse,
  never reimplement** (Global Constraints).
- Produces:
  - `pub struct Flag { pub metric: String, pub depth: f64, pub value: f64 }`
  - `pub struct WorldAnomaly { pub seed: u64, pub flags: Vec<Flag> }`
  - `pub fn evaluable_columns(c: &Census) -> (Vec<String>, Vec<(String, String)>)`
    — `(evaluable, excluded_with_reason)`
  - `pub fn rank(c: &Census) -> Vec<WorldAnomaly>` — worlds ordered most
    anomalous first
  - `pub fn for_seed(c: &Census, seed: u64) -> Option<WorldAnomaly>`

- [ ] **Step 1: Write the exclusion test first**

```rust
/// A column no world can be an outlier on must be excluded, and the exclusion
/// must carry a reason. Enforces `excluded => has a stated reason`; it does
/// NOT enforce that every excluded column is genuinely degenerate — that
/// direction is `exclusions_agree_with_domesday` below.
#[test]
fn a_frozen_column_is_excluded_with_a_reason() {
    let c = load(Path::new("../../book/src/laboratory/generated/the-census")).expect("census loads");
    let (evaluable, excluded) = evaluable_columns(&c);
    assert!(
        !evaluable.iter().any(|m| m == "hue-depth-goblin"),
        "hue-depth-goblin is min == max across all 1000 worlds and cannot have a tail"
    );
    let reason = excluded.iter().find(|(m, _)| m == "hue-depth-goblin");
    assert!(reason.is_some_and(|(_, r)| !r.is_empty()),
        "an exclusion without a reason is an assertion nothing can check");
}

/// The partition claim from spec §3.3, stated as a test rather than a promise:
/// every column this report excludes is a column the Domesday already reports
/// as a weakness under D2 (frozen) or D4 (at-rail).
#[test]
fn exclusions_agree_with_domesday() {
    let c = load(Path::new("../../book/src/laboratory/generated/the-census")).expect("census loads");
    let (_, excluded) = evaluable_columns(&c);
    let findings = detect(&c, &[], &[]);
    for (metric, _) in &excluded {
        assert!(
            findings.iter().any(|f| &f.metric == metric
                && (f.detector.starts_with("D2") || f.detector.starts_with("D4"))),
            "{metric} is excluded here but reported by neither D2 nor D4 — the two \
             instruments were claimed to partition and do not"
        );
    }
}
```

The second test is the assertion the spec explicitly flags as **unverified**.
**Decision rule:**
- It passes → the partition claim is now established; say so in the report.
- It fails → **do not weaken the test.** Record which columns disagree and in
  which direction, and report. A column excluded here but invisible to D2/D4 is
  a gap in the Domesday, which is a finding, not a nuisance.

- [ ] **Step 2: Run and confirm it fails to compile**

Run: `cargo test -p hornvale-lab --lib anomaly`
Expected: FAIL — `evaluable_columns` not found.

- [ ] **Step 3: Implement exclusion and scoring**

Exclusion: a numeric/integer column with at least 50 present values is
evaluable unless `min == max` (reason: `"frozen: min == max across N worlds"`)
or both rails tie beyond a 1 % bucket (reason: `"both rails tied: X at min, Y
at max of N"`). Categorical and flag columns are never evaluable; state that in
the module doc with its reason (a tail is an ordering, and these have none).

Scoring, exactly as spec §3.4:

```rust
/// Two-sided tail depth: 0.0 is the most extreme value in the census, 0.5 the
/// median. Ranks are averaged across ties, so a value shared by many worlds
/// cannot make any one of them look extreme.
fn tail_depth(sorted: &[f64], value: f64) -> f64 { /* … */ }
```

Sort with `total_cmp` (no `partial_cmp().unwrap()`). Use `BTreeMap`, never
`HashMap`. A world's flags are its columns with `depth <= 0.01`; its score is
the flag count, ties broken by `sum(-ln(depth))`; `rank` publishes worlds
ordered by score descending.

Guard the log: `depth` can be exactly `0.0` for the census's extreme holder,
and `-ln(0.0)` is infinite. Decide and document the treatment — clamping to
the smallest representable depth `1.0 / (n - 1) as f64` keeps the tiebreak
finite and is the recommended choice. Assert it with a test on the actual
extreme-holding world, not a synthetic vector.

- [ ] **Step 4: Extend the no-world-building guard**

`windows/lab/src/domesday/mod.rs` holds `loading_never_builds_a_world`, which
scans `census.rs` for `build_world`, `BuildDepth`, `build_to`, `RunResult`.
Extend it to scan `anomaly.rs` under the same forbidden-word list.

Keep the assertion in `mod.rs`, not inside `anomaly.rs`: a forbidden-word list
embedded in the file it scans always finds itself. The existing comment says
so; preserve that reasoning in the extended version.

- [ ] **Step 5: Run and confirm green**

Run: `cargo test -p hornvale-lab --lib anomaly`
Expected: PASS.

- [ ] **Step 6: Render, wire the CLI, wire the regen**

- `render.rs`: a page carrying (a) the top 25 worlds with their flagged
  metrics, depths and values; (b) the **full exclusion roster with reasons**;
  (c) a header stating the frozen constants and that they are selection bars,
  not significance claims.
- `cli/src/main.rs:1297` dispatches `"domesday" => cmd_lab_domesday()`. Add
  `"anomalies" => cmd_lab_anomalies(args)` beside it, supporting `--seed N`
  (prints one world, writes nothing). Update `USAGE`; there is a test at
  `cli/src/main.rs:2213` asserting `USAGE` mentions subcommands — follow its
  pattern.
- `scripts/regenerate-artifacts.sh:512` runs `lab domesday`. Add the anomalies
  render immediately after it, with a comment noting it is a pure read over the
  same committed census.
- `book/src/SUMMARY.md`: add `  - [Anomalies](./domesday/anomalies.md)` after
  the History line at :94.

- [ ] **Step 7: Verify the drift check can actually fail against the new page**

`git diff --exit-code <path>` is silently vacuous against a path with no index
entry. `book/src/domesday/` is already tracked, so this should be a non-issue —
**verify it rather than assuming**:

```bash
cargo run -p hornvale -- lab anomalies
git add book/src/domesday/anomalies.md
printf '\n<!-- deliberate drift -->\n' >> book/src/domesday/anomalies.md
git diff --exit-code book/src/domesday/ ; echo "exit=$?"
```

Expected: `exit=1`. **Decision rule:** if it reports `exit=0`, the check is
vacuous and the page is untracked — fix that before continuing, because a
vacuous drift check is worse than none.

Then `git checkout -- book/src/domesday/anomalies.md` to discard the probe.

- [ ] **Step 8: `cargo fmt`, full suite, commit**

```bash
cargo fmt
cargo nextest run --workspace 2>&1 | tee /tmp/hv-gnomon-t4.txt
git add windows/lab/src/domesday/ cli/src/main.rs scripts/regenerate-artifacts.sh \
        book/src/SUMMARY.md book/src/domesday/anomalies.md
git commit -m "feat(lab): the anomaly report — the Domesday's transpose, per world"
```

---

## Task 5: H1, the injection battery — the campaign's headline

**Files:**
- Create: `windows/lab/tests/anomaly_injection.rs`
- Create: `windows/lab/tests/anomaly_holdout.rs`

**Interfaces:**
- Consumes: `anomaly::{rank, for_seed, evaluable_columns}` from Task 4.

Both files are **heavy tier**: every `#[ignore]` carries a reason with the
`heavy:` token (`cli/tests/heavy_tier.rs` checks the reason string verbatim).
They build worlds, so they run on lefford via `make heavy-remote REF=<full-sha>`.

- [ ] **Step 1: Build the positive control BEFORE the recall measurement**

This step is not optional and its order is not negotiable. A mutation proves
only what it perturbs, and a recall of zero is unreadable without it — it means
either "the report missed it" or "the mutation did nothing", which demand
opposite responses.

For each injection:

1. Assert the target text exists before substituting it
   (`assert!(src.contains(OLD), "TARGET NOT FOUND: {OLD}")`). A `cargo fmt`
   rewrap has previously made a single-line replacement match nothing and
   produced a green that looked like a robust implementation.
2. Rebuild the affected seeds and assert the affected metric's values **differ
   from baseline**. Record which metric moved and by how much.

**Decision rule:**
- The metric moved → proceed to recall.
- Nothing moved → that injection is void. Do not count it in either the
  numerator or the denominator, and say in the report that it was dropped and
  why. Silently dropping it would make the recall look better than it is.

- [ ] **Step 2: Choose the injections from inside the code**

Do **not** implement a mutation this plan prescribes. A plan author does not
know which constants share a stream or which are load-bearing; the implementer
does, after reading. The property each injection must demonstrate:

> a single-site change to one generative constant that (a) still compiles,
> (b) provably moves at least one evaluable census column, and (c) is not
> expected to move the whole census — an injection that moves everything tests
> nothing about ranking.

Find at least five satisfying all three. `make seam-guard-list` shows the
registered seams and their call-site counts without building, and is the
cheapest place to start; a broadly-called function makes a poor injection for
the same reason it makes a poor seam.

- [ ] **Step 3: Measure recall@10 and the false-positive arm**

- **Recall**: over the injection battery, the share where the provably-moved
  metric appears in the affected worlds' top-10 ranking. **Success: ≥ 0.60.**
- **False positives**: run the ranking twice with no perturbation; the number
  of worlds whose top-10 changes must be **zero**. The census is deterministic,
  so this is an identity check, not a statistic — a non-zero result means the
  scorer has a nondeterministic tie-break, which is a determinism bug and
  outranks everything else in this campaign.

- [ ] **Step 4: H2, the held-out calibration control**

Fit tail depths on the committed census (seeds 0–999); score seeds 1000–1199.
The share of held-out worlds with at least one column at `depth <= 0.01` should
fall within a factor of two of the in-census share.

Write into the test's own doc comment that **H2 is a calibration check and not
a usefulness measure** — a stationary distribution passes it while flagging
nothing useful. Without that sentence a later reader will cite a green H2 as
evidence the report works.

200 world-builds at the measured 8.3 s each is ≈28 minutes single-threaded.
Heavy tier, lefford, never the commit gate.

- [ ] **Step 5: Report the result whichever way it falls**

**Decision rule:**
- recall@10 ≥ 0.60 → the criterion is met; record the exact figure.
- recall@10 < 0.60 → **the hypothesis is falsified and that is the chronicle's
  headline.** Do not adjust `k`, the 0.01 bar, or the injection set to rescue
  it. If a post-unblinding change is genuinely warranted, make it, say so
  explicitly, and count it — three defensible corrections still means NOT
  RESOLVED.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/lab/tests/anomaly_injection.rs windows/lab/tests/anomaly_holdout.rs
git commit -m "test(lab): H1 injection recall and H2 held-out calibration for the anomaly report"
```

---

## Task 6: `refuted` — the seventh status

**Files:**
- Create: `docs/decisions/0131-refuted-is-a-seventh-registry-status.md`
- Modify: `cli/tests/docs_consistency.rs:361-368`
- Modify: `book/src/frontier/idea-registry.md:25-37`
- Modify: `book/src/frontier/CLAUDE.md:65`

**Interfaces:** none — independent of Tasks 1–5.

`0131` is the next free number (`docs/decisions/` ends at `0130`); confirm with
`ls docs/decisions/ | tail -3` before creating it, since a parallel campaign may
have taken it.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn refuted_is_an_admissible_status() {
    assert!(REGISTRY_STATUSES.contains(&"refuted"));
}

/// A `refuted` row must cite the campaign or decision that refuted it.
/// This is stricter than any other status carries, and deliberately: an
/// uncited refutation is an assertion with no way to check it, which is the
/// exact defect PROC-project-epistemology names.
#[test]
fn every_refuted_row_cites_its_evidence() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| normalize_status(&r.status) == "refuted")
        .filter(|r| !r.status.contains('('))
        .map(|r| r.id.clone())
        .collect();
    assert!(
        offenders.is_empty(),
        "refuted rows must cite what refuted them, e.g. `refuted (The Mire)`: {offenders:?}"
    );
}
```

Read `registry_rows()`'s actual struct field names before writing (`r.id`,
`r.status` above are the shape, not necessarily the spelling).

- [ ] **Step 2: Run and confirm both fail**

Run: `cargo test -p hornvale --test docs_consistency refuted`
Expected: FAIL on the first assertion.

- [ ] **Step 3: Open the vocabulary**

`cli/tests/docs_consistency.rs`: `[&str; 6]` → `[&str; 7]`, adding `"refuted"`.
The const's doc comment currently explains why the status vocabulary is closed
while category prefixes are open — extend it to say the vocabulary was opened
once, deliberately, by decision 0131, and that it is closed again at seven.

- [ ] **Step 4: Write the decision record**

`docs/decisions/0131-refuted-is-a-seventh-registry-status.md`, following the
form of a recent record (read `0130` first). It must carry:

- **The distinction, in one line**: `rejected` is a decision — we considered it
  and set it aside. `refuted` is a measurement — we tested it and reality said
  no.
- **Why the project needs it**: the method is preregistered falsification and
  several campaigns ship the null as the headline; 47 registry rows record a
  falsification in prose where nothing can count it, while only two put it in
  the Status cell.
- **The admission rule**: a row takes `refuted` when the row's own central
  claim was tested and found false, **and no artifact shipped from it.** A row
  that shipped a mechanism while refuting a prediction stays `shipped`.
- **The required citation**, and why it is stricter than every other status.
- **What was rejected**: `shipped → refuted`, because `normalize_status` takes
  the *head* of an arrow (asserted at `cli/tests/docs_consistency.rs:390`), so
  the drift check would police the origin token.
- **That the vocabulary is closed again at seven**, so this record is not a
  precedent for an eighth.

- [ ] **Step 5: Update the two prose consumers**

- `book/src/frontier/idea-registry.md:25-37` — add the seventh bullet with the
  one-line distinction, and change *"These six are the whole vocabulary… do not
  invent a seventh"* to state seven, cite 0131, and say do not invent an
  eighth. The sentence must remain an anti-drift guard, not become an
  invitation.
- `book/src/frontier/CLAUDE.md:65` — add `refuted` to the listed vocabulary.
- `docs/CLAUDE.md:58` — reads *"Status is one of the six documented values"*
  inside the enumerated list of drift-check assertions. Change six to seven.
  **This consumer was missing from the spec's original "complete" list**; it
  was found at preflight by grepping the claim instead of a remembered set of
  paths (ledger R5). Before editing, re-run that grep — the vocabulary may have
  a fifth reader nobody has named either:

  ```bash
  grep -rn --include=*.md --include=*.rs -iE \
    'six (are the whole|documented values)|one of the six|these six|\[&str; 6\]' . \
    --exclude-dir=target --exclude-dir=.git --exclude-dir=book/book
  ```

  Ignore hits in `docs/superpowers/plans/` and `docs/retrospectives/` — those
  are historical records of what was true when written, and are not edited.
- `docs/README.md:49` names a pipeline without enumerating statuses and does
  **not** change. Read it and confirm before leaving it alone.

- [ ] **Step 6: Run and confirm green**

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS.

- [ ] **Step 7: The reclassification audit — capped at 12**

The 47 candidate rows:

```bash
grep -inE '^\| .*(falsifi|refut|overturn|the null|disconfirm)' \
  book/src/frontier/idea-registry.md
```

For each, apply Step 4's admission rule. Expect a small set: 13 of the 47 are
`shipped` and most of those shipped something.

**Decision rule:**
- ≤ 12 rows qualify → reclassify them, each with its citation parenthetical,
  and list them in the task report with the one-line reason each qualified.
- \> 12 qualify → **STOP and report the list.** Do not expand scope; the cap
  exists because a large reclassification is a judgement call Nathan should
  make, not a mechanical outcome.
- 0 qualify → that is a finding worth stating plainly: the vocabulary is
  correct and forward-looking, and the essay's "greppable, countable" payoff
  begins with the next falsified campaign rather than retroactively.

- [ ] **Step 8: Regenerate the digest and commit**

Adding a decision record drifts `docs/digest/decisions-in-force.md`.

```bash
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions
cargo fmt
git add docs/decisions/ cli/tests/docs_consistency.rs book/src/frontier/ docs/digest/
git commit -m "feat(registry): refuted — a seventh status for a measured no (0131)"
```

---

## Task 7: Census refresh, artifact sweep, and the campaign close

**Files:** generated artifacts across the repo; `book/src/chronicle/the-gnomon.md`;
`docs/retrospectives/the-gnomon.md`; `book/src/frontier/idea-registry.md`.

- [ ] **Step 1: Absorb main and gate**

```bash
make preflight            # run FROM the branch
make gate
```

On an ancestry NO-GO, merge main INTO the branch and re-run the gate there.
Read the other active branches' chronicles, not just their diffs — `preflight`
mechanizes only the checkable half and has no opinion about whether two
campaigns changed the same idea incompatibly.

- [ ] **Step 2: Push, then dispatch the census with a FULL SHA**

Never a branch name — `HV_CENSUS_REF` feeds `reset --hard` and can land on a
stale local branch of that name over there.

```bash
git push -u origin campaign/the-gnomon
bash scripts/census-run.sh status    # is a heavy job already holding the box?
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical \
  HV_CENSUS_REF=<full-sha> scripts/census-run.sh'
```

Read the expected cost from `grep '| census |' docs/timings.md | tail`, not
from this plan and not from `CLAUDE.md`. Commit the regenerated goldens **on
lefford**, then push and fast-forward locally.

- [ ] **Step 3: Verify the index columns landed and are not all-Absent**

```bash
head -1 book/src/laboratory/generated/the-census/rows.csv | tr ',' '\n' | grep -c '^first-day-'
```

Expected: 19.

Then check the present/absent split per column from the regenerated Domesday
history page.

**Decision rule:**
- All 19 present with a spread of values → proceed.
- A column is `Absent` in all 1000 worlds → that key never occurs anywhere.
  Not necessarily wrong (`occ-cause-plague` may be genuinely unreachable), but
  it is a finding: report it, and check whether the emitting code path is
  reachable at all before assuming the metric is at fault. A registered but
  unreachable value is exactly the defect The Particular shipped and The Ell
  had to resolve.

- [ ] **Step 4: Run the conjunction query from Task 3**

```bash
make census-check
```

and run `emergent-conjunction.sql`. This is the moment the campaign's central
claim is either demonstrated or not.

**Decision rule:** if the query returns zero rows on all 1000 worlds, the
conjunction chosen in Task 3 is empty in this world-population. That is a fact
about the worlds, not a failure of the index — pick a conjunction that is
non-empty, record both, and say in the chronicle which conjunctions are empty,
because "no world has all three" is itself the kind of answer the index exists
to give cheaply.

- [ ] **Step 5: Dispatch the heavy tier for H1 and H2**

```bash
make heavy-remote REF=<full-sha>
```

- [ ] **Step 6: Full artifact sweep**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ \
  docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

**Decision rule, per path:**
- Only `docs/audits/` and/or `docs/digest/` moved → expected (new `pub` items
  drift the type-audit report; a new decision record drifts the digest).
  Regenerate and commit in the **same commit**.
- `book/src/domesday/` moved → expected; the census changed and the survey is a
  pure read over it.
- `book/src/laboratory/` moved **without** the census refresh in Step 2 having
  landed → **STOP and diagnose.**
- `book/src/gallery/` moved → **STOP.** This campaign draws nothing and commits
  no new predicate. Gallery movement means a stream consumption order changed,
  which is an epoch event.
- `clients/game/core/tests/fixtures/` moved → **STOP.** Same reasoning.

- [ ] **Step 7: The book and the record**

- Chronicle: `book/src/chronicle/the-gnomon.md`, plus its `SUMMARY.md` entry.
  Lead with H1's actual result, whichever way it fell.
- Freshness sweep of stale chapters; re-score `book/src/open-questions.md` if a
  Confidence Gradient bet moved.
- Retrospective: `docs/retrospectives/the-gnomon.md`. **Promote the worktree's
  `.superpowers/sdd/followups.md` into it before teardown** — that file is
  git-ignored and dies with the worktree. It carries F1 (`normalize_status`
  validates a row's *former* status), F2 (the 34 frozen census columns as a
  finding in their own right), and F3 (the frozen roster drifting as the
  history bake grows).
- Registry: flip `TOOL-first-occurrence-index`, `TOOL-anomaly-report` and
  `PROC-refuted-status` to `shipped`, repointing **Where** at the chronicle.
  **Amend `TOOL-first-occurrence-index`'s prose** — it currently claims a reach
  it does not have (spec §2.2: degenerate for 93 of 115 predicates, and the key
  is `(predicate, object)`, not predicate). Repointing **replaces** a row's
  prose; it does not append.

- [ ] **Step 8: Final gate and close**

```bash
make gate
make board-sync
```

Then `superpowers:finishing-a-development-branch` and `closing-a-campaign`.
G6 is a hard stop: present the post-G3 ledger digest to Nathan before merging.

---

## Self-Review

**Spec coverage.** §2.1–2.2 → Tasks 1–2. §2.3 → Task 3 + Task 7 Step 4. §2.4 →
Task 2 Step 2's two-directional frozen roster. §2.5 → Task 1 Step 7 (measured,
not estimated) and Task 7 Step 2. §3.1 → Task 4 Steps 3–4. §3.3 → Task 4 Step 1,
including the partition claim the spec flags as unverified. §3.4 → Task 4
Step 3. §3.5 → Task 5. §3.6 → Task 4 Step 6. §4.1–4.3 → Task 6 Steps 3–6. §4.4 →
carried as F1 into the retrospective, Task 7 Step 7. §5 → Task 7. §6 risks →
each has a step: the ratchet risk in Global Constraints, the no-op mutation in
Task 5 Step 1, roster drift in Task 2's frozen list, the census cost in Task 7
Step 2, the reclassification cap in Task 6 Step 7.

**Placeholders.** None. Every code step carries real code; every judgement step
carries a branch table rather than an expected outcome.

**Type consistency.** `first_day(&World, &str, Option<&str>) -> MetricValue` is
declared in Task 1 and used unchanged in Task 2. `evaluable_columns`, `rank`,
`for_seed`, `Flag`, `WorldAnomaly` are declared in Task 4's interface block and
used with the same names in Task 5. Metric name strings are declared once in
Task 2's `FIRST_DAY_METRICS` and referenced verbatim in Task 3's SQL and Task 7
Step 3's grep.

**One deliberate softness.** Several steps say "read the file for the spelling"
of an existing helper (`FullView::build`, `registry_rows()`'s fields,
`MetricValue`'s Absent variant). That is not a placeholder — it is the rule
against prescribing from outside the code. The plan gives the shape and the
line number; the implementer confirms the name.
