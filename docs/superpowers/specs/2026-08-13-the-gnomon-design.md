# The Gnomon — design

**Campaign**: The Gnomon
**Branch**: `campaign/the-gnomon`
**Date**: 2026-08-13
**Rows**: `TOOL-first-occurrence-index`, `TOOL-anomaly-report`,
`PROC-refuted-status`
**Source essay**: `book/src/frontier/frontier.md` §"The instrument turned
inward — the project as its own subject" (committed `d608b3ba`)

---

## 1. What this campaign is

The essay measures an asymmetry: every instrument Hornvale owns reads
**evidence** — measurements over generated worlds — and almost none reads
**structure**. The Laboratory, the census, the Domesday, seam-guard, the type
audit, the timings baseline and the drift checks all answer *what is true of
the world we generated*. The digest alone answers *what is true of the project
generating it*.

This campaign installs the three near-term rungs the essay names as modest,
each of which points an existing instrument ninety degrees:

| Row | Instrument today | Turned |
|---|---|---|
| `TOOL-first-occurrence-index` | the census reads world state | …to read *when* state arrived |
| `TOOL-anomaly-report` | the Domesday reads a column across worlds | …to read a world across columns |
| `PROC-refuted-status` | the registry records what we intend | …to record what reality refused |

They are one campaign because they are one move: **the corpus is too large to
hold, the structure is small enough to hold, and nobody has extracted the
structure.**

Six further rows argued in the same essay stay registered and out of scope:
`TOOL-tap`, `TOOL-watchpoint`, `TOOL-exogenous-input`,
`TOOL-intervention-stream`, `TOOL-ablation`, `TOOL-shape-index`.

### 1.1 The governing discipline for this campaign specifically

The essay's own headline finding is that **five tools instantiate one ratchet
pattern, the governing documents say so in prose, and the fifth was written
anyway.** A campaign implementing that essay is uniquely exposed to committing
the defect inside its own remedy (cf. `a-repair-carries-the-defect-it-repairs`,
and The Hollow, whose repair committed both defects).

Two consequences bind every task below:

- **No new percentile, census-loading, or ratchet implementation.** §3 reuses
  `windows/lab/src/domesday/census.rs` and `stats.rs` verbatim. A task that
  finds itself writing `fn percentile` has gone wrong.
- **Every claim about generated output or tool behaviour carries a
  command-and-output pair**, at drafting time. Every measured number in this
  spec was produced by a command run on 2026-08-13 against the committed tree;
  none is inferred.

---

## 2. The first-occurrence index

### 2.1 What it is

Per seed, per key, the earliest world-day on which a matching fact was
committed. This turns an emergent conjunction — a species *and* a condition
*and* a place, all at once — from a search over worlds into a set intersection
plus a maximum over three numbers, and hands back the world-time from which a
replay should begin.

### 2.2 The key is `(predicate, object)`, and the domain is narrower than the row claims

**Measured** — seed 42 at `BuildDepth::Full`, 13,533 facts
(`cargo run -p hornvale -- new --seed 42`, then a scan of the emitted JSON):

```
distinct predicates:                    115
predicates with ANY non-genesis day:     22
facts that are time-bearing:          9,173  (67.8% of 13,533)
facts at day 0.0:                     4,328
facts with no day at all:                32
```

Two findings follow, and both correct the registry row:

**(a) The index is degenerate for 93 of 115 predicates.** Their first
occurrence is identically `WorldTime::GENESIS`. The index has content only over
the history bake — `occ-founded`, `occ-ended`, `occ-cause`, `occ-people`,
`occ-tech`, `occ-site`, `occ-peak`, `occ-function`, `occ-notability`,
`occ-founded-from`, `occ-ended-by`, `is-occupation`, `is-settlement`,
`is-ruin`, `pays-tribute-to`, `population`, `cell-id` — and over persons:
`is-person`, `person-born`, `person-died`, `person-founded`, `name`. That is
not fatal; it is where the interesting queries live. But the row overstates its
reach and is amended at close.

**(b) Keyed on predicate alone, the motivating query cannot be expressed.**
Object cardinality within the 22, seed 42:

```
occ-people   n=704  distinct=15   hobgoblin 283, gnoll 171, kobold 139, snow-elf 43, …
occ-tech     n=704  distinct= 4   classical 465, iron 133, bronze 86, neolithic 20
occ-cause    n=474  distinct= 3   fled 234, migrated 220, famine 20
occ-function n=704  distinct= 1   agrarian
occ-notability n=704 distinct= 1  common
```

"A species and a condition" is a query over *objects*. Keyed on predicate,
every species shares one first-day for `occ-people` and the conjunction is
unaskable.

### 2.3 Materialisation: the census is the index

The index ships as **census metric columns**, not as a new artifact.

The reason is that the payoff query is then already available. `make
census-check` runs duckdb over `book/src/laboratory/generated/the-census/`
(`tools/census/queries/`), so the conjunction the row promises is:

```sql
SELECT seed, greatest("first-day-occ-people-kobold",
                      "first-day-is-ruin",
                      "first-day-pays-tribute-to") AS replay_from
FROM "the-census"
WHERE "first-day-occ-people-kobold" IS NOT NULL
  AND "first-day-is-ruin"           IS NOT NULL
  AND "first-day-pays-tribute-to"   IS NOT NULL
ORDER BY replay_from;
```

`greatest()` *is* the maximum over three days; `IS NOT NULL` *is* the set
intersection; `replay_from` *is* the world-time a replay should start from.
Nothing is built that the census does not already build.

This also inherits, at no cost: the committed drift check, the Domesday
chapters, the chart pipeline, `census-history`'s multi-snapshot view, and
`seed-biography.sql`.

Rejected alternatives are in the decision ledger (#5): a long-format
`generated/first-occurrence/index.csv` costs a second 1000-seed sweep of
roughly one census, and a new generated directory must be `git add`ed before
`git diff --exit-code` can ever fail against it — nothing in
`regenerate-artifacts.sh` guards that. A live CLI query commits nothing and so
is never drift-checked.

### 2.4 The roster is declared, frozen, and small

Metrics are added to `windows/lab/src/metrics.rs` in the existing `Metric`
form (`name` / `doc` / `summary` / `domain` / `role` / `extract`), all with
`Role::Descriptor`, `Extractor` at the rung the history bake requires, and
`Domain::History` or `Domain::Demography` as the Domesday chapter demands.

The roster is **authored, not derived**. A mechanical expansion over
(predicate × observed object) would grow whenever a world happens to contain a
new species, which makes the census schema seed-dependent — a save-format-
adjacent hazard. The frozen roster, to be enumerated in the implementation
plan, is bounded at **40 columns** and drawn only from §2.2's list.

Naming: `first-day-<predicate>` for unkeyed predicates,
`first-day-<predicate>-<object>` for keyed ones. A world in which the key never
occurs emits **Absent**, which the census already represents and which the
Domesday already reports as `n present / n absent`.

### 2.5 Costs, stated plainly

- **Census refresh on lefford.** Read the cost from
  `grep '| census |' docs/timings.md | tail`, never from `CLAUDE.md`. The
  current figure is **949.579 s** (row stamped 2026-08-13T19:01:49Z,
  `cpu_ratio` 28.56 on 40 cores) after The Millrace. Dispatch with a full SHA;
  the guard refuses off-host (decisions 0063/0079/0081).
- **Test reddening.** Adding lab metrics reddens the roster-wide tests
  (`windows/lab/tests/metric_roster_safety.rs` applies every metric;
  `windows/lab/src/domesday/render.rs` renders every metric into a Book page).
  Budget for it; the plan measures the actual count on the first metric added
  rather than carrying an estimate.
- **Artifact churn.** Each new numeric column produces a
  `the-census-default-<metric>.svg` chart and a Domesday chapter entry.
- **Extraction cost is negligible and this is measured, not assumed**: a scan
  over 13,533 facts is microseconds against the 8.3 s a seed-42 `Full` build
  takes (`time cargo run -p hornvale -- new --seed 42` → `real 0m8.296s`).

---

## 3. The anomaly report

### 3.1 What it is, and what it is not

Per world, which of its metric values sit deep in the tail of the census
distribution for that column — so a world volunteers its own outliers instead
of waiting for someone to ask the right question.

**It is the Domesday's transpose, not a second Domesday.**
`windows/lab/src/domesday/` already runs eight preregistered detectors over the
committed census, and `stats.rs::numeric` already computes min/p25/median/p75/
max. Domesday asks *is this column weak across 1000 worlds*; the anomaly report
asks *is this world strange across 204 columns*. Same census, same percentiles,
axis rotated ninety degrees.

**It reuses `domesday::census::load` and `domesday::stats` and implements
neither.** See §1.1.

**It is the autonomous end of a spectrum whose manual end already exists.**
`tools/census/queries/explore/interesting-worlds.sql` opens with *"'interesting'
is a query, not a generation stage… Edit freely."* That template requires you to
know the question first. This removes that requirement; it does not replace the
template.

**It never builds a world.** The `loading_never_builds_a_world` guard in
`windows/lab/src/domesday/mod.rs` scans `census.rs` for `build_world`,
`BuildDepth`, `build_to`, `RunResult`. The anomaly module inherits the same
discipline and the same guard, extended to cover it.

### 3.2 The tautology, and the design that escapes it

The failure mode Nathan named: if the prior is census percentiles and the
evaluation set is that same census, the report flags 0.1 % of worlds *by
construction* and the number means nothing. Cf. The Domesday, where a
preregistered study read a tautology because no insolation column existed.

The finding that resolves it: **the tautology is manufactured by the
threshold, not by the prior.** Decomposing the report into
`prior × scorer × labeler × presenter` localises the defect entirely in the
labeler. A *ranking* asserts nothing about how many worlds are anomalous; a
*threshold* asserts 0.1 %. So the report ships a ranking, and the preregistered
criterion scores the ranking, never the selection bar.

### 3.3 The evaluable surface is 107 columns, and the exclusions are named

**Measured** on the committed census
(`book/src/laboratory/generated/the-census/rows.csv`, 1000 seeds ×
204 metric columns; kinds from `schema.json`):

```
column kinds:     numeric 144, categorical 35, flag 26, integer 1
numeric/integer columns with >=50 present worlds, tested for whether a
1% tail can be isolated at all:
    usable for a percentile tail:                                107
    DEGENERATE (frozen, or both rails tied beyond a 1% bucket):   34
```

Degenerate examples, verbatim from the run: `hue-depth-goblin`,
`hue-depth-kobold`, `channel-connectivity`, `channel-band-monotonicity`,
`core-homophony-{goblin,hobgoblin,bugbear,kobold}`,
`homophony-merger-share-*`, `confusable-homophony-*`, `tone-count-*`,
`lifespan-years-*`, `age-at-maturity-years-*`, `basal-metabolic-rate-w-*` —
all `min == max` across all 1000 worlds — plus `flagship-structure-size`, which
ties 59 worlds at its min and 941 at its max.

A percentile rule over a frozen column flags either zero worlds or all
thousand. **The exclusion roster is therefore a committed artifact**, not an
implementation detail: `book/src/domesday/anomalies.md` publishes which columns
were excluded and why, so a column that later becomes evaluable (or ceases to
be) shows up in the drift check.

The exclusions are not novel: Domesday **D2** (frozen) and **D4** (at-rail)
already report exactly these columns as *column* weaknesses. The two
instruments partition rather than overlap, and the spec asserts that: a column
excluded here must be reported by D2 or D4 there.

### 3.4 Scoring

For world `w` and evaluable column `c` with `n` present values, let `r` be
`w`'s rank of `c` among those values, ties averaged, and define

```
tail_depth(w, c) = min(r, n-1-r) / (n-1)          # 0 = most extreme, 0.5 = median
```

- A world's **report** is its ten columns of smallest `tail_depth`, ascending.
  `k = 10` is frozen here.
- A world's **score** is the count of its columns with `tail_depth <= 0.01`
  ("how many ways is this world extreme"), ties broken by the sum of
  `-ln(tail_depth)` over those columns.
- The committed artifact ranks worlds by score and publishes the top **25**.

`0.01`, `k = 10` and `top 25` are **frozen selection bars, not significance
claims**, and they are frozen by the same precedent Domesday set: D1's 0.80
share bar, D3's 5 % IQR bar and D5's effect-size bands are all declared
external or frozen constants carrying no tuning freedom. Retuning any of them
after seeing a result is a post-unblinding change and must be counted as one
(cf. `count-the-post-unblinding-changes`).

Ordering by minimum `tail_depth` alone was considered and rejected: with 107
columns and two rails, roughly 214 worlds hold an extreme and tie at exactly
zero, so the ordering would be degenerate.

### 3.5 Preregistered hypothesis and success criteria

Frozen here, before the code that would move them (decision 0016). A study
JSON has no hypothesis field and nothing mechanical compares a result to this,
so this section is the freeze.

**H1 (headline, injection).** *The report's per-world metric ranking
concentrates a planted perturbation.* For a perturbation applied to a single
generative constant or seam, the metric that the perturbation demonstrably
moved appears in the affected worlds' top-10 ranking.

- **Success**: recall@10 ≥ 0.60 across the injection battery.
- **Falsification**: recall@10 < 0.60. This is a finding, published as the
  headline, not a failure to be retuned away.
- **The label is independent of the census**: it comes from which constant was
  perturbed, not from any percentile.

**H1's mandatory positive control.** A mutation proves only what it perturbs.
Before recall is computed, each injection must be shown to have **moved the
world**: the affected census columns must differ from baseline, asserted by
diff, and the target text must be asserted present before substitution
(`assert old in src`). Without this, a recall of zero is unreadable — it means
either "the report missed it" or "the mutation did nothing", and those demand
opposite responses. Cf. `an-empty-diff-needs-a-positive-control` and
`a-mutation-proves-only-what-it-perturbs`; a `cargo fmt` rewrap has previously
made a single-line replacement match nothing and produced a green that looked
like a robust implementation.

**H1's false-positive arm.** The same battery run with **no** perturbation
must produce a stable ranking; the number of unperturbed worlds whose top-10
changes between two identical runs must be **zero** (the census is
deterministic, so this is an identity check, not a statistic).

**H2 (control, held-out calibration).** *The census percentiles generalise to
seeds the census never saw.* Fit tail depths on seeds 0–999; score seeds
1000–1199. The share of held-out worlds with at least one column at
`tail_depth <= 0.01` should fall within a factor of two of the in-census share.

- **H2 is explicitly a calibration check, not a usefulness measure.** A
  stationary distribution passes H2 while flagging nothing useful. It is
  reported alongside H1 and can never substitute for it.
- H2 requires building 200 worlds outside the census. At the measured 8.3 s
  per `Full` build this is ≈28 minutes single-threaded and belongs in the
  **heavy tier** (`heavy:` ignore-reason token), dispatched to lefford with
  `make heavy-remote REF=<full-sha>`, never in the commit gate.

**What is deliberately not claimed.** The report is not asserted to find
defects. H1 measures whether it *would* rank a known perturbation; whether an
unknown defect exists to be found is not a question this campaign can answer,
and a flag is a pointer for a human, never a verdict.

### 3.6 Surface

- `windows/lab/src/domesday/anomaly.rs` — pure read, no world construction.
- `hornvale lab anomalies` — regenerates the committed artifact.
- `hornvale lab anomalies --seed N` — one world's ranking, printed, uncommitted.
- `book/src/domesday/anomalies.md` — committed, drift-checked, added to
  `scripts/regenerate-artifacts.sh` and to the `git diff --exit-code` list in
  `CLAUDE.md`.

**`book/src/domesday/` already has an index entry, so this is not a new
generated directory** and the `git add` hazard of §2.3 does not apply. The plan
verifies that by command rather than assuming it.

---

## 4. `refuted` — the seventh status

### 4.1 Why it needs a decision record

`book/src/frontier/idea-registry.md:35` reads: *"These six are the whole
vocabulary and a drift-check enforces it. Category prefixes are open — coin a
new one when an idea needs it — but statuses are not; do not invent a
seventh."*

That is a deliberate anti-drift guard, so overriding it is decision-shaped
rather than edit-shaped. The campaign writes
`docs/decisions/NNNN-refuted-status.md` and the registry's sentence becomes a
citation instead of a contradiction. Precedent for settling vocabulary in the
log: 0015, 0012.

`refuted` is distinct from `rejected`, and the record must say so in one line:
**`rejected` is a decision — we considered it and set it aside. `refuted` is a
measurement — we tested it and reality said no.** A project whose method is
preregistered falsification, and several of whose campaigns ship the null as
the headline, has a word for the first and not the second.

### 4.2 Admission rule, and why there is no sweep

**Measured**: 47 registry rows mention falsification vocabulary
(`falsifi|refut|overturn|the null|disconfirm`) anywhere in the row. By current
status: 19 `raw`, 13 `shipped`, 11 `elaborated`, 1 `rejected`, 1 `ratified`.
Only **two** carry it in the Status parenthetical — `SOC-criticality`
("shipped (slices 1–2 — falsified twice)") and one `shipped (qualified: …)`
row. So falsification is recorded almost entirely in row *prose*, where
nothing can count it.

A mechanical sweep is wrong: the 13 `shipped` rows shipped code **and**
refuted a prediction, and one token cannot say both. Demoting them loses the
shipped fact.

**The admission rule, preregistered:** a row takes `refuted` when **the row's
own central claim was tested and found false, and no artifact shipped from
it.** A row that shipped a mechanism while refuting a prediction stays
`shipped`; its falsification stays in prose and is out of this campaign's
reach.

Each reclassified row cites its evidence in the parenthetical, mirroring
`ratified (0009)`: `refuted (The Mire)` or `refuted (0117)`. The parenthetical
is **required**, and the drift check enforces its presence for this status
specifically — an uncited `refuted` is an assertion with no way to check it,
which is the exact defect `PROC-project-epistemology` names.

The plan enumerates which of the 47 qualify; the enumeration is the reviewable
deliverable, capped at **12 rows**. If more than 12 qualify, the plan stops and
reports rather than expanding scope.

### 4.3 Consumers that move together

Grepped; this list is complete:

- `cli/tests/docs_consistency.rs:361` — the `REGISTRY_STATUSES` const
  (currently `[&str; 6]`), plus a new test for the required parenthetical.
- `book/src/frontier/idea-registry.md:25-37` — "How to read a row"; the
  seventh bullet and the amended "do not invent an eighth" sentence.
- `book/src/frontier/CLAUDE.md:65` — the authoring rule.
- `docs/README.md:49` names a pipeline without enumerating statuses, so it
  does **not** change. Verified by reading it.

### 4.4 A latent defect found and deliberately not fixed here

`normalize_status` takes the **head** of a transition arrow:
`normalize_status("rejected → ratified") == "rejected"` is an asserted test at
`cli/tests/docs_consistency.rs:390`. The drift check therefore validates a
row's *former* status, not its current one. This is why `shipped → refuted`
was rejected as the mechanism (ledger #8).

It looks like a real defect, but fixing it silently re-validates every existing
arrow row and is not this campaign's scope. Captured as a followup.

---

## 5. Definition of done

Standard campaign DoD, plus:

- `make gate` green; `make rebaseline` with an empty diff across
  `book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
  docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/`.
  **Decision rule, not a prediction** (per the imperative-mood lesson): if only
  `docs/audits/` and `docs/digest/` moved, regenerate and commit in the same
  commit; if `book/src/gallery/` moved, STOP — that is an epoch event and this
  campaign draws nothing; if `book/src/laboratory/` moved without a census
  refresh having been dispatched, STOP and diagnose before committing.
- Census refreshed on lefford with a full SHA, goldens committed there, pushed,
  fast-forwarded locally. Cost read from `docs/timings.md`, not from this spec.
- Heavy tier dispatched via `make heavy-remote REF=<full-sha>` for H2.
- `cargo test --manifest-path tools/board/Cargo.toml` if anything touches the
  board (nothing here should).
- Chronicle entry in `book/src/chronicle/`; freshness sweep; Confidence
  Gradient re-scored if a bet moved.
- Retrospective in `docs/retrospectives/the-gnomon.md`.
- Registry rows updated: the three in scope flipped to `shipped`, and
  `TOOL-first-occurrence-index`'s prose amended per §2.2 — it currently
  overstates its reach.
- **H1's result published whichever way it falls.** A recall below 0.60 is the
  chronicle's headline, not a reason to retune `k`.

---

## 6. Risks

| Risk | Response |
|---|---|
| A sixth percentile implementation lands inside the campaign that exists to name that defect | §1.1; a task writing `fn percentile` has gone wrong |
| The injection battery's mutations are no-ops and the null is unreadable | §3.5's mandatory positive control, asserted before substitution |
| The frozen metric roster grows seed-dependently and the census schema becomes non-deterministic | §2.4 — authored roster, capped at 40, drawn from §2.2's list only |
| A census refresh is budgeted off `CLAUDE.md` instead of `docs/timings.md` and is wrong by 2× or 20× | §2.5 states the command; the block itself says not to trust its own number |
| `refuted` reclassification becomes a 47-row sweep | §4.2 — capped at 12; over-cap stops and reports |
| Parallel campaigns collide semantically with a clean `make preflight` GO | Read other branches' chronicles at each stage boundary, not just their diffs |
