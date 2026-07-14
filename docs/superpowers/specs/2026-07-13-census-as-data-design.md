# Census as Data — Design

**Status:** implemented (2026-07-13; see
`docs/superpowers/plans/2026-07-13-census-as-data.md` and
`docs/retrospectives/census-as-data.md`)
**Date:** 2026-07-13
**Campaign:** census-as-data (Tier 3 of the three-tier ask; Tiers 1+2 were
fast-gate-tiers and the remote AWS gate, both merged)

## Problem

The 1000-world census is the project's richest dataset — ~110 metrics
spanning every domain, per fully-generated world — and today it is consumed
by *skimming text summaries*. The committed record is fragmented across five
studies with overlapping metric sets at mismatched seed counts (500 / 1000 /
2000 / 10,000), none self-describing: a `rows.csv` header names its columns
but nothing committed says what `plate-size-gini` means, what type it is, or
what precision floor its floats carry. There is no way to ask cross-metric
questions (`which tidally-locked worlds have pantheon-size > 6?`), no way to
ask longitudinal questions (`how did hypsometric-bimodality move when libm
landed?`), and no contract a future consumer — or a future where the data
outgrows git — can rely on.

Generation is *not* the problem: every study already builds complete worlds
start-to-end (star system → planet → tectonics → climate → species →
languages → religions) and merely records different column subsets. The
fragmentation is in the **measurement record**, so this campaign
consolidates the record and adds a query surface. The generators do not
change.

## Goal

One canonical, self-describing, versioned census artifact plus a thin
analysis harness, so the maintainer does data science on the world
population instead of skimming summaries:

- `SELECT`-shaped exploration over all metrics × 1000 worlds, typed and
  documented, in an interactive DuckDB session one make-target away.
- Longitudinal queries across git history ("the biography of seed 42").
- A schema contract (`schema.json`) that makes the artifact consumable
  without reading Rust source, and survives a future where the CSV leaves
  git for object storage.
- A smaller perpetual rebaseline bill (the duplicate 1000-world
  branches-family run retires).

## 1. Consolidation: the canonical census and the two tiers

### The live tier (regenerated, drift-checked, CI-probed)

Exactly two studies:

1. **`the-census`** — `studies/the-census.study.json`: **all metrics ×
   seeds 0–999 × default pin set**. This is `census-lands-drift` promoted:
   doubled from 500 to 1000 seeds and renamed, because "the drift guard for
   the lands campaign" is the wrong name for the project's canonical
   dataset. Artifacts land at `book/src/laboratory/generated/the-census/`
   (`rows.csv`, `schema.json`, summary, charts).
2. **`census-of-the-meeting`** — unchanged. Its solo-roster pin sets build
   genuinely different worlds (the null control); it is not a column subset
   of anything.

The rename ripples to a **closed consumer list**:
`windows/lab/tests/calibration.rs`, `fixture_staleness.rs`,
`depth_ladder.rs`, `scripts/ci-census-probe.sh` (`STUDIES` array),
`scripts/regenerate-artifacts.sh`, the Makefile `lab-diff` usage text, the
project `CLAUDE.md` command examples, the book's laboratory pages /
`SUMMARY.md`, and the study file itself
(`studies/census-lands-drift.study.json` → `studies/the-census.study.json`,
generated directory renamed with it).

### The frozen tier (preregistered evidence, never regenerated)

`branches-family` **moves to the frozen tier — not deleted**. Its committed
artifacts stay byte-for-byte (book chapters and the-branches chronicle cite
them), but it leaves `regenerate-artifacts.sh` and the CI probe list. Its
data is a strict projection of the canonical run: same seeds 0–999, same
default pins, 32 of the ~110 columns — metric extraction is read-only
observation of the built world, so the values are identical (verified for
seeds 0–3 by inspection; asserted for all 1000 seeds during migration, §5).
It also leaves `fixture_staleness.rs`: a frozen fixture is *intentionally*
stale, and keeping it in the staleness check would re-freeze it red on the
first physics change.

The pre-existing frozen studies (census-of-coasts, census-of-coasts-tuning,
census-of-skies, census-of-words, and the never-committed 10k censuses) are
untouched. Frozen studies with a committed `rows.csv` (coasts,
coasts-tuning, branches-family) become queryable through the harness with
epoch and provenance tags (§3); frozen studies with only summaries/charts
(skies, words) are documented as frozen but not mounted — there is no row
data to query.

Frozen is a statement about **evidence, not recoverability**: each frozen
study's producing commit is recorded in the harness manifest, so
`git checkout <commit> && cargo run -p hornvale -- lab run <study>`
re-derives any of them. A frozen study is a reproducible citation.

### Golden-pin discipline

The calibration means in `calibration.rs` are computed over 500 seeds today
and shift when the sample doubles; they re-pin **in the same commit** as the
fixture rebaseline (the established rule — never deferred to a later
commit). The ADR 0016 branches-family pins do **not** move: same seeds,
same values, different file.

## 2. `schema.json`: the self-describing manifest

Emitted by `publish()` alongside `rows.csv` for every published study, from
the same `RunResult` — it can never disagree with the CSV it sits next to.
Committed and drift-checked like everything else under `generated/`. Shape:

```json
{
  "schema_version": 1,
  "study": {
    "name": "the-census",
    "description": "…",
    "seeds": { "from": 0, "count": 1000 },
    "pin_sets": [ { "label": "default", "pins": [], "roster": null } ]
  },
  "conventions": { "float_quantization": "8-significant-digits" },
  "rows": { "count": 1000, "fnv1a64": "0x9c3f0a1b2c3d4e5f" },
  "columns": [
    { "name": "seed",    "kind": "integer" },
    { "name": "pin_set", "kind": "categorical" },
    { "name": "star-class", "kind": "categorical",
      "doc": "Spectral class of the host star", "rung": "sky" },
    { "name": "ocean-fraction", "kind": "numeric",
      "doc": "…", "buckets": [0.2, 0.4, 0.6, 0.8], "rung": "terrain" },
    { "name": "tidally-locked", "kind": "flag", "doc": "…", "rung": "sky" },
    { "name": "refusal", "kind": "categorical" }
  ]
}
```

Field-by-field:

- **`columns` covers the whole CSV** — framing columns (`seed`, `pin_set`,
  `refusal`) included, in exact header order. A consumer types the file
  from the manifest alone: no sniffing, no special-casing. The kind
  vocabulary is the serialized `SummaryKind` plus `integer` for `seed`:
  `numeric` → `DOUBLE`, `flag` → `BOOLEAN`, `categorical` → `VARCHAR`,
  `integer` → `BIGINT`; every column nullable (`Absent` → empty field →
  NULL).
- **`doc` and `rung`** ride along from the metric registry — the difference
  between a dataset and a documented dataset. `rung` (the metric's
  build-depth) also answers "which metrics exist at terrain depth", which
  the deferred shallow-census extension (§7) will need.
- **`buckets`** (numeric metrics only) exports the histogram edges so
  harness-side distributions reproduce the summary/chart bucketing exactly.
  Floats pass through the kernel's `quantize` like every serialized float.
- **`conventions.float_quantization`** declares the precision floor
  (decision 0033's 8-significant-digit emit-boundary quantization) — the
  map-legend field: coordinates are meaningless without the datum, and
  nothing else committed states it.
- **`rows.count`** = `seeds.count × |pin_sets|`, the packing-slip check —
  validation is one `COUNT(*)` away. Refused worlds still emit a row, so
  the identity is exact.
- **`rows.fnv1a64`** — FNV-1a 64-bit hash of the exact `rows.csv` bytes,
  rendered `0x` + 16 lowercase hex digits. Binds manifest ↔ data (the
  harness hard-errors on a mismatched pair instead of silently mis-typing
  columns) and is the "git keeps `schema.json` + a content hash, data lives
  elsewhere" contract for the cloud-scale future. Content hashes are
  drift-stable (same world → same bytes → same hash; physics changes move
  both files in the same commit) — unlike producing-commit hashes, which
  stay banned. FNV-1a is ~10 lines of std-only Rust; this is integrity
  binding, not security, so no crypto-crate temptation.
- **Deliberately absent: timestamps and commit hashes.** The file must be
  byte-stable under regeneration at an unchanged world. Producing-commit
  provenance is git's job (live tier) or the harness manifest's (frozen
  tier).
- **`schema_version`** bumps only on shape/convention changes (quantization
  width, header framing, manifest structure) — never on additive metric
  growth, which is the normal, unversioned case that `"metrics": "all"`
  absorbs automatically as new domains land.
- **Reserved, not yet populated: `unit` per column.** Units currently live
  in metric names (`day-length-hours`). The structured field's vocabulary
  is exactly what The Datum / kernel-units doctrine is deciding; populate
  when that merges.

### Backfill for the frozen tier

Frozen studies with a `rows.csv` predate `schema.json`. Each gets a
**one-time backfilled manifest** — same format plus top-level
`"backfilled": true` — derived from the current registry (their metric
names all still exist in it), committed once, never regenerated. The flag
records that the manifest is a later annotation of the record, not part of
it. Suggested mechanism: a `lab backfill-schema <study.json> <rows.csv>`
subcommand (reads the study and the CSV header, emits the manifest);
implementation plan decides the final shape.

### Implementation surface

One new `render_schema(result)` in the lab window (serde_json, deterministic
key order), the FNV-1a helper, one line in `publish()`, tests (§5).

## 3. The harness: `tools/census/`

Outside the workspace, like `tools/type-audit` — the sim never depends on
it; it consumes committed artifacts. External tools: `duckdb` and `python3`
(brew), bash scripts shellcheck'd.

```
tools/census/
  manifest.json       # what's mounted: the one hand-written file
  build.sh            # materialize census.duckdb from committed artifacts
  history.sh          # longitudinal extraction from git history
  queries/
    explore/          # discovery mode
      seed-biography.sql       # one world's row across all commits/epochs
      interesting-worlds.sql   # filter template (moons >= 3 AND ...)
      metric-drift.sql         # metric distribution across history epochs
    calibrate/        # invention mode
      golden-pins.sql          # recompute pinned constants from the fixture
  .build/             # census.duckdb + extracted blobs (gitignored)
```

**`manifest.json`** — the registry of mounted datasets, three entry kinds:

- **Live studies** (`the-census`, `census-of-the-meeting`): path to the
  study dir; tier `live`.
- **Frozen studies** (branches-family, census-of-coasts,
  census-of-coasts-tuning): path, tier `frozen`, `epoch` label, **producing
  commit** (the reproducible-citation field), a one-line note on why
  frozen.
- **Sidecar tables**: `docs/timings.md` (markdown table → CSV via awk in
  `build.sh`, mounted as `timings`) — the performance ledger joins the same
  database, so "did the census get slower as metrics grew" is a join, not a
  spelunk.

**`build.sh`** — for each study entry: read `schema.json` (python3),
**validate** — row count matches `rows.count`, FNV-1a64 matches, CSV header
matches `columns` order; any mismatch is a hard error naming the file and
the failed check — then emit `CREATE VIEW` DDL with explicit types from the
kind mapping, and finally build the unified long view:

```
census_long(study, tier, epoch, seed, pin_set, metric, kind,
            value_num, value_text, value_flag)
```

via UNPIVOT DDL **derived from the schema.jsons at build time, never
committed** (derive-don't-duplicate). The output database is a throwaway in
`.build/`.

**`history.sh <study>`** — walks
`git log --first-parent main -- <rows.csv path>`, extracts each blob,
converts wide→long per version (absorbing column drift across history), and
loads `census_history` = `census_long` columns +
`(commit, commit_date, epoch_label)`, where `epoch_label` is the commit
subject (on `--first-parent main` that is almost always a merge/campaign
subject) — so "the census as of the Crust close" is a WHERE clause and `seed-biography.sql` is `WHERE seed = 42 ORDER BY commit_date`.
Historical snapshots predate `schema.json`; typing falls back to the
current schema for columns that still exist and VARCHAR for retired ones.

**Make targets** (main Makefile, matching type-audit's pattern):

- `make census` — build + validate + open the DuckDB CLI on the result.
- `make census-query Q="SELECT ..."` — one-shot, non-interactive: build if
  stale, run the query, print, exit. The scriptable surface — for future
  Claude sessions mid-campaign, for `census-check` itself, for any tool
  that wants an answer rather than a session. One `duckdb -c` line, and
  the difference between a personal exploration tool and an instrument any
  consumer can read.
- `make census-history STUDY=the-census` — extract history, then open.
- `make census-check` — build + validate + one smoke query per mounted
  dataset + the golden-pins match. The harness's own gate; local-only (brew
  tools), listed in the `gate-full` checklist, **not** in `make gate`.

**Canned-query doctrine:** queries are committed but their outputs are not
drift-checked (the database is a throwaway). The exception in spirit is
`queries/calibrate/golden-pins.sql`, which recomputes every pinned
calibration constant from the fixture and compares against the pinned
values inlined in the query (`SELECT computed, pinned, computed = pinned AS
ok`). The duplication of pin values between Rust tests and SQL is
**deliberate** — an independent second path from fixture to pin;
`census-check` fails on any `ok = false`. When pins re-pin, the SQL updates
in the same commit, exactly like the fixture.

**Graduation doctrine (the explore → preregister edge):** an ad-hoc query
that finds something must not die in the throwaway database. The paved
path: promising ad-hoc query → committed canned query under
`queries/explore/` → if it keeps earning its keep, a metric in the registry
or a preregistered study. Corollary: the queries you *cannot* write — joins
that want a column no metric provides — are the "unobserved organs" signal
(TOOL-unobserved-organs); the harness doubles as the instrument that finds
its own missing instruments. Both halves are stated in the book's "The
Census as Data" section (§6), which also names `golden-pins.sql`'s promoted
side effect: it is the **pin-provenance report** — every pinned calibration
constant in the test suite is reproducible as a query against the committed
fixture.

**Error-handling doctrine:** everything fails loudly at *mount time*, not
query time — missing brew tool (named, with the install command),
validation mismatch (named file + failed check), manifest entry pointing at
a nonexistent path. No silent skips: a dataset that cannot mount aborts the
build, because a query that silently omits a study is a wrong answer that
looks right.

## 4. Versioning doctrine

- The live census is always **the census as of HEAD**. Git history is the
  archive; `history.sh` is the reader.
- `schema_version` semantics as in §2: shape changes bump it; additive
  metric growth never does.
- Frozen studies are irreversible as *evidence*, reproducible as
  *computations* (producing commit in the manifest).
- **Forward compatibility:** at cloud scale (a 10⁶-world census under the
  AWS toolkit) the CSV leaves git for object storage; git keeps
  `schema.json` + `rows.fnv1a64`. The manifest already carries everything
  that regime needs; nothing in this design assumes the data is in git
  except the (replaceable) paths in `tools/census/manifest.json`.

## 5. Testing

**Rust side (in `make gate`):**

- `render_schema`: deterministic output; `columns` order agrees with
  `render_csv`'s header; kind mapping covers every `SummaryKind`;
  `rows.count` identity; FNV-1a64 known-vector tests.
- `publish()` file-count and returned-paths tests updated (+1 file per
  study).
- Existing calibration / fixture-staleness / depth-ladder tests re-pointed
  at `the-census`.
- `branches_family_calibration.rs` reads its 32 columns from the canonical
  fixture (ADR 0016 pins unchanged).

**Migration one-shots (implementation-plan steps, not permanent tests):**

- Before re-pointing the family calibrations: assert the canonical
  fixture's 32 family columns are byte-identical to the old
  branches-family fixture across all 1000 seeds. Divergence is a
  determinism bug to be found *now*, not papered over.
- Golden means in `calibration.rs` re-pinned in the same commit as the
  500→1000 rebaseline.

**Harness side (local, `make census-check`):** mount validation, smoke
queries, golden-pins match; `shellcheck` on `build.sh` and `history.sh`.

## 6. Process integration (Definition of Done)

- **Decision log**: one entry — the canonical-census consolidation (one
  live all-metrics census; branches-family run retired to frozen; the
  analysis harness lives outside the workspace like type-audit; the
  append-only epoch-directory alternative refused).
- **Book**: laboratory overview gains "The Census as Data" (live/frozen
  tier doctrine, how to cite a frozen study, the graduation doctrine and
  pin-provenance framing from §3); frozen studies' pages get a
  frozen-at-commit banner line; chronicle entry at close; Confidence
  Gradient re-score if a bet moved.
- **Idea registry**: two new `raw` rows — `TOOL-shallow-wide-census`
  (depth-laddered 10k-seed wide census at terrain rungs; kin of MAP-25 and
  TOOL-22) and `TOOL-census-leaves-git` (the object-storage regime of §4).
- **Timing ledger**: the 1000-seed canonical regeneration gets a `timed.sh`
  row.
- **`docs/README.md`**: documentation-map line for `tools/census/`.
- Campaign retrospective per decision 0020.

## 7. Alternatives considered and refused

- **Long-format committed artifact** (`rows-long.csv`: seed, pin_set,
  metric, value): natively queryable, but ~110× the rows, noisy diffs, and
  either breaks the fixture loaders (replacing wide) or doubles committed
  bytes (alongside wide). The long form is better *derived* than *stored*.
- **Rust-native `lab query`/`lab export`**: std-only Rust reinventing SQL
  is the wrong tool for open-ended exploration; the one advantage
  (drift-checked query outputs) is only needed for calibrations, which
  already live in Rust tests.
- **Append-only epoch directories** (each rebaseline appends a snapshot
  instead of overwriting): git *is* the append-only store; duplicating it
  in the working tree doubles bytes for nothing.
- **Type-specimen pointer** (designating seed 42 the exemplar row in the
  schema): serves no query; seed 42's canonicity is cultural, not
  schematic.
- **Mid-file schema changes**: one manifest governs one whole file, ever; a
  study whose shape must change is a new study or an epoch suffix, per
  existing convention.
- **SHA-256 for the content hash**: no crypto crate is allowed and ~100
  lines of hand-rolled SHA-256 buys nothing over FNV-1a for integrity
  binding (this is mismatch detection, not security).
- **Depth-laddered 10k shallow census** and **cloud-scale census**: real
  extensions, deferred to the idea registry (§6), not built now.

## 8. Costs (all one-time)

- One 1000-seed all-metrics regeneration on the AWS spot box
  (`make regen-remote` / regen-git.sh — census regeneration is never
  local, owner directive 2026-07-13; release; on the order of the
  current census bill — and the retired branches-family run claws back
  roughly a third of every future rebaseline).
- The rename ripple over the closed consumer list (§1).
- Golden-pin re-pins (§1, §5).
- Frozen-tier backfill manifests (§2).
