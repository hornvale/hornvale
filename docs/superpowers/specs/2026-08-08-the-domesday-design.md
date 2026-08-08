# The Domesday — a generated survey of the thousand worlds

**Status:** spec, awaiting G3 · **Date:** 2026-08-08 · **Branch:** `the-domesday`
· **Base:** `6e469717`

Part IV of the Book programme (Nathan's five-part sketch, 2026-08-08). Advances
UNI-29 (the self-describing program) and UNI-28 (the View discipline);
generalizes decision 0110's census-as-shared-pass argument from checks to prose.

---

## 1. What this is

A generated Book part that reads the committed census — **1,000 worlds ×
194 metrics** — and renders a human-readable survey of what Hornvale actually
produces. Two purposes, in Nathan's words:

1. demonstrate that the generator produces a broad variety of rich worlds;
2. **make weaknesses visible so they can be addressed.**

The second is the one with teeth, and it is the reason this is Part IV rather
than a nicer summary page.

## 2. Why it is cheap, and what is actually missing

Almost all the machinery exists.

| already exists | where |
|---|---|
| 1,000 rows × 194 columns | `book/src/laboratory/generated/the-census/rows.csv` |
| per-column `kind` (`numeric`/`categorical`/`flag`/`integer`), `doc`, `rung`, `buckets` | `…/the-census/schema.json` |
| deterministic distribution rendering | `windows/lab/src/summary.rs::render_summary` |
| the gate reads the committed fixture rather than rerunning | decision 0032 |
| a check that is a function of one world belongs in the census | decision 0110 |

Four things are missing, and they are the whole campaign:

- **Summary statistics.** `render_summary` emits distributions only — no min,
  max, mean, median, mode, or quartiles for the 133 numeric metrics.
- **A domain axis.** `rung` is a build-depth, not a subject: 105 of 194 metrics
  land in `full`, mixing religion, language, and naming. Metric names carry no
  usable prefix (`vestige-density`, `pantheon-size`, `cult-form`).
- **Comparators.** No real or imagined world to measure against.
- **Weakness detection.** Nothing mechanically flags a degenerate distribution.

## 3. The evidence that this works before it is built

Registry row SKY-19 already carries a finding of exactly the shape this part
is meant to surface, measured from this census:

> median land temperature is −11.9 °C, ~20 °C below Earth's; **651/1000 worlds
> are ice-dominant** and 946/1000 ice-or-alpine; median waterfall count is 0.
> Correlations against mean land temperature: ocean-fraction r = −0.000,
> obliquity r = +0.041, total-tide r = +0.023, year-std-days r = −0.245.
> **The climate is not merely cold, it is near-uninfluenced by its own
> astronomy.**

That is a serious defect in world generation, and it is currently buried in a
registry row. **The Domesday's detectors must find it.** It is the campaign's
acceptance test, not an illustration.

## 4. Design

### 4.1 The domain axis

Add a **required** `domain` to metric registration. `MetricKind`-adjacent, a
closed vocabulary: `astronomy`, `terrain`, `climate`, `hydrology`, `biology`,
`settlement`, `society`, `religion`, `language`, `naming`, `history`.

- **Default-deny**, in the repo's idiom (`type-audit`, `claim_shape`): a metric
  without a domain fails a test. There is no "unclassified" bucket, because a
  bucket is where 105 metrics would go to die again.
- ~86 metrics inherit their domain from `rung` (`astronomy`/`terrain`/
  `settlements`). The 105 in `full` are classified by hand, assisted by their
  visible name clusters (pantheon/cult/belief/head-deity → `religion`;
  name/homophony/lexicon/confusable/divergence → `language` or `naming`).
- This axis is **reused by Part II** (Nathan's sketch: *A. Astronomy,
  B. Geology, …*), which is half its justification.

### 4.2 Statistics, by metric kind

Computed from the committed `rows.csv`, never by re-running the census.

| kind | rendered |
|---|---|
| `numeric` | n, absent-count, min, p25, median, p75, max, mean, mode-bucket |
| `integer` | as numeric, plus exact mode |
| `categorical` | value → count → share, descending by count then lexicographic |
| `flag` | true/false counts and shares (both rows always present) |

All float output goes through `hornvale_kernel::quantize` at the emit boundary,
as `render_csv` already does — the survey is a committed artifact and must be
byte-identical on regeneration.

**Determinism:** ties break lexicographically; sorting uses `total_cmp`. No
`HashMap`. `Absent` is counted and reported, never silently skipped — a metric
that is absent on 900 worlds is itself a finding.

### 4.3 Comparators — data, not prose

A committed JSON file (decision 0012: config is JSON), `studies/comparators.json`:

```json
{
  "worlds": [
    { "name": "Earth", "kind": "real",
      "values": { "ocean-fraction": 0.71, "obliquity-deg": 23.44,
                  "mean-land-temp-c": 14.0, "moon-count": 1 } }
  ]
}
```

Real worlds first (Earth, Mars, Titan). Imagined worlds (Arrakis, Hoth) are
*allowed* but each value needs a cited source in a `note` field, because an
invented number presented beside a measured one is precisely the confusion this
programme exists to remove.

A comparator entry naming a metric that does not exist **fails a test** — the
same stale-anchor discipline `tools/type-audit` enforces on tags.

### 4.4 Weakness detectors — preregistered, mechanical

Six detectors. Five are pure computation over `rows.csv`; only D5 requires an
authored expectation, and that is the whole judgment surface of this campaign.

| id | fires when | authored input |
|---|---|---|
| **D1 Degenerate** | a `categorical`/`flag` metric where one value holds ≥ 80 % of worlds | none |
| **D2 Invariant** | a `numeric` metric with zero variance across 1,000 worlds | none |
| **D3 Narrow** | a `numeric` metric whose p25..p75 spans < 5 % of its min..max range | none |
| **D4 At-rail** | median equals min or max (e.g. "median waterfall count is 0") | none |
| **D5 Decoupled** | a metric an expectation says should track a driver, but \|r\| < 0.10 | `studies/expectations.json` |
| **D6 Off-comparator** | median differs from a comparator by more than a declared band | `studies/comparators.json` |

`studies/expectations.json` holds pairs the physics implies:

```json
{ "expect": [
  { "metric": "mean-land-temp-c", "tracks": "insolation-rel",
    "why": "insolation is the dominant term in a radiative balance",
    "min_abs_r": 0.10 }
]}
```

**Thresholds are preregistered here and are not to be tuned after seeing the
output** (decision 0016). If a threshold turns out to fire on everything or
nothing, that is a finding to report and re-preregister in a named commit — not
a dial to quietly turn.

### 4.5 Where it lives, and what generates it

`windows/lab` already renders census pages and already depends on nothing above
it. The survey renderer is a new module there — `windows/lab/src/domesday.rs` —
reading the committed `rows.csv` + `schema.json`, exactly as decision 0032's
calibration path already reads the committed fixture.

**It does not re-run the census.** The gate must never pay for 1,000 worlds.

Output: `book/src/domesday/` — one generated page per domain plus an index, each
carrying the `<!-- GENERATED FILE — do not edit -->` header the book's other
generated pages use, wired into `book/src/SUMMARY.md`.

Regeneration goes through `scripts/regenerate-artifacts.sh` (the single source
of truth called by both `make rebaseline` and CI) and is drift-checked with
`git diff --exit-code`. **`book/src/domesday/` must be `git add`ed in the
commit that introduces it** — `git diff --exit-code` is silently vacuous
against an untracked path (The Digest, Task 7).

### 4.6 The prose budget

Per domain page: one authored paragraph of framing, stored as a fact and
assembled — the `self-map-line` pattern The Digest proved. Everything else is
computed. No sentence in this part may state a number that is not read from
`rows.csv` at render time.

## 5. Non-goals

- **Not re-running the census.** Reads the committed artifact only.
- **Not Part II.** The domain axis is shared; the Science prose is not in scope.
- **Not new metrics.** 194 exist; this part surveys them. A gap this survey
  reveals becomes a follow-on, per 0110.
- **Not charts.** SVGs already generate per metric; link them, do not reinvent.
- **Not tuning worldgen.** The Domesday reports weaknesses; fixing them is
  a separate campaign and a separate decision.

## 6. Preregistered success criteria

Frozen before implementation (decision 0016). A falsified prediction is a
finding, not a failure.

- **S1.** Every one of the 194 metrics carries a `domain`; a metric without one
  fails a test (default-deny, demonstrated red on a deliberately unclassified
  metric).
- **S2.** **The detectors find SKY-19's climate defect.** Specifically: D1 fires
  on the ice-dominant share, D4 fires on median waterfall count = 0, and D5
  fires on mean-land-temp vs insolation. If they do not, either the detectors or
  the thresholds are wrong and that is the campaign's headline.
- **S3.** Regenerating the survey twice produces byte-identical output; mutating
  one value in `rows.csv` makes the drift check exit non-zero (**demonstrated
  RED on command**, with the target asserted present before mutation).
- **S4.** A comparator naming a nonexistent metric fails a test.
- **S5.** The survey costs the gate nothing measurable — it reads a committed
  CSV. Rendering time is reported; if it exceeds 5 s, say so.
- **S6.** No number appears in the rendered prose that was not read from
  `rows.csv` at render time (checked by mutation: change a value, confirm the
  rendered sentence changes).

**Falsification clause.** If the detectors fire on so many metrics that the
output is unreadable — or on so few that known defects slip through — the
campaign reports the threshold calibration as its headline finding and ships
the raw survey without the weakness section rather than tuning thresholds to
look good.

## 7. Risks

| risk | mitigation |
|---|---|
| Detector thresholds tuned to flatter the output | Preregistered in §4.4; retuning is a named commit, never silent |
| The domain classification becomes a bikeshed | Closed vocabulary, default-deny, ~105 judgments made once |
| Comparator numbers invented rather than sourced | `note` field required on imagined worlds; test forbids unknown metrics |
| The survey re-runs the census by accident | Reads `rows.csv` only; §4.5 and a test assert it |
| A new generated directory's drift check is vacuous | `git add` in the introducing commit (The Digest, Task 7) |

## 8. Open — for G3

- **The `domain` vocabulary is Nathan's call.** Eleven values are proposed in
  §4.1; the split between `society`/`settlement` and between `language`/`naming`
  is arguable, and it is the axis Part II will inherit.
- **D1's 80 % threshold** would *not* fire on the ice-dominant share (65.1 %).
  Either the threshold is wrong or ice-dominance needs D6 against Earth instead.
  This is preregistration doing its job before the code exists — flagged rather
  than silently set to 60 %.
- **Imagined-world comparators**: allowed with sources, or real worlds only for
  v1? Leaning real-only, to keep the first survey unimpeachable.
