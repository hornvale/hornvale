# Campaign L0: The Hornvale Lab — Design

**Date:** 2026-07-06
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Provenance:** First moonshot of the adopted moonshot track; greenlit from the
frontier conversation. The Lab makes every later moonshot measurable.

---

## 1. Goal

Turn Hornvale's determinism into an instrument: a batch experiment harness
(`hornvale lab`) that sweeps seeds × pin-sets, extracts per-world metrics,
and publishes summaries and charts as drift-checked book artifacts. First
study: **the Census of Skies** — distributions of everything the sky
generator produces, plus the belief split as instrument calibration against
tier-0 religion's known ground truth.

## 2. Design principles

1. **Studies are data; metrics are code.** A study file selects named
   extractors from a registry — it never defines computation. This is the
   line that keeps the config from becoming a DSL.
2. **The instrument checks itself.** Study files, summaries, and charts join
   the same drift-check regime as the almanacs: CI reruns a small pinned
   study and fails on divergence.
3. **Measure what players see.** The runner builds full worlds
   (`build_world`), never a lightweight shortcut path — metrics observe the
   ledgered reality.
4. **Format:** JSON (`studies/*.study.json`), riding the existing
   serde_json dependency. Nathan prefers YAML ergonomically; rejected solely
   on the no-new-deps rule, revisit if the budget ever loosens.
5. **Calibration before discovery.** Study 001 deliberately includes a
   metric with known ground truth (belief kind is a pure function of
   rotation regime at tier 0) so the instrument is validated the day it
   ships — alongside genuinely unknown numbers (the moon-refusal rate).

## 3. The study format

```json
{
  "name": "census-of-skies",
  "description": "Distributions of everything the sky generator produces.",
  "seeds": { "from": 0, "count": 10000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": "all"
}
```

- `pins` entries are the **existing pin-string format** (`"moons=0+3"`,
  `"rotation=locked"`), parsed by the same `parse_pin` the CLI and `sky_of`
  use — one parser, no drift.
- `metrics` is `"all"` or an array of registry names.
- Validation fails loudly with the reason: unknown metric names, malformed
  pins, empty seed ranges, duplicate pin-set labels.
- `name` must match `[a-z0-9-]+` and is used in output paths.

## 4. The runner (`windows/lab`)

A window crate (observes worlds; depends on kernel + domains + the cli's
composition logic — see §8 for the boundary note). For each
(seed × pin_set): `build_world(seed, pins, SkyChoice::Generated)`, apply
every selected metric, record one row. Sequential, deterministic,
byte-reproducible; ~10k worlds is minutes at author time. Worlds that fail
genesis under a pin-set (loud pins) are recorded as refusal rows with the
error reason — refusals are data, not crashes.

## 5. Tier-1 metrics registry

All cheap reads off `StarSystem`, `Calendar`, and world facts. Each metric
is a named extractor with a one-line doc (rendered by
`hornvale lab list-metrics`):

star-class; day-length-hours (absent if locked); tidally-locked flag;
year-std-days; year-local-days (absent if locked); obliquity-degrees;
moon-count-admitted; moon-count-drawn-vs-admitted (**the refusal rate** —
first genuinely unknown number); total-tide; months-per-year (per moon);
neighbor-count; neighbor-classes; brightest-neighbor-class;
empty-night-rate contributor (has-moons-or-neighbors-visible);
belief-kind (eternal | cyclic — calibration metric); genesis-note-count.

Future campaigns extend the registry; the runner never changes.

## 6. Outputs

Three tiers:

- **Raw:** per-world CSV to gitignored `lab-out/<study>/` — never committed.
- **Summary:** aggregated tables as generated markdown, committed under
  `book/src/laboratory/` (counts, percentages, histogram bucket tables;
  deterministic bucket boundaries declared per metric in the registry).
- **Charts:** deterministic hand-rolled SVG (histograms and bar charts, no
  plotting dependencies — same discipline as the First Light BMP),
  committed alongside the summaries.

## 7. The book: The Laboratory

New top-level section:

- **Overview chapter** — what a study is, how to author one, how the
  drift-check keeps results honest (prose, at the book's altitude).
- **Study 001: The Census of Skies** — generated tables and charts via
  includes; analysis prose written at close and gated by Nathan's
  comprehension review. The analysis must answer at least one previously
  unanswerable question (the moon-refusal rate) and show the calibration
  metric matching ground truth.

## 8. CLI and boundaries

- `hornvale lab run <path>` and `hornvale lab list-metrics` (std-only
  arg parsing, house style).
- Boundary note: the runner needs `build_world`, which lives in the cli
  crate (the composition root). Since windows must not depend on the bin
  crate, **the composition root (world_builder + its accessors) extracts to
  a new `windows/worldgen` library crate**; the cli re-exports it, and
  `windows/lab` depends on it (windows may depend on windows — the
  constitution constrains only domains). This extraction also discharges
  the long-deferred "lib target for WASM reuse" chore — the same cut
  serves both, and it must be behavior-preserving (the committed almanac
  artifacts are the referee, as in every Phase-0-style refactor).

## 9. CI

The artifact step gains: run the pinned **drift study**
(`studies/census-drift.study.json`, same shape as 001 but 500 seeds),
regenerate its committed summary + charts, include in the existing
`git diff --exit-code` net. The full 10k study runs at author time only.

## 10. Testing

- Study-file validation: each failure mode loudly named.
- Runner determinism: same study file twice → byte-identical raw CSV and
  summaries.
- Metric unit tests against hand-constructed worlds (locked world has no
  day-length row; a seed-23-style degradation yields genesis-note-count 1).
- Calibration assertion as a *test*: over the drift study, belief-kind
  eternal ⇔ tidally-locked, exactly.
- CI drift check green; full gate as always.

## 11. Exit criteria

1. `hornvale lab run studies/census-of-skies.study.json` reproduces the
   committed Study 001 summary byte-identically.
2. The census chapter answers the moon-refusal-rate question with a number.
3. The calibration metric matches tier-0 ground truth exactly.
4. The Laboratory section is live in the published book, drift-checked.

## 12. Deferred

Parallel execution; statistical tests (arrive with the first hypothesis
study); config-defined metrics (never, per §2.1); the workshop-paper draft
(consumes the Lab; separate effort); YAML (dependency budget).

## 13. Risks

- **Chart quality without a plotting library** — hand-rolled SVG must be
  readable and honest (axis labels, bucket edges); the dataviz design pass
  happens at plan time, and Nathan's read of the chapter is the taste gate.
- **Composition-root extraction** (§8) touches the cli crate's structure;
  it is the only part of L0 with refactor risk, and it pre-pays the WASM
  chore, which is why it belongs here rather than later.
- **Runtime creep**: if 10k × full build_world exceeds comfortable
  author-time minutes, the study shrinks to 5k before the runner grows
  complexity — measurement first, optimization on evidence.
