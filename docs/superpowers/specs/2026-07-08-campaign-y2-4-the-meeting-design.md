# Campaign 4 (Year 2): The Meeting — Design

**Date:** 2026-07-08
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-07-year-2-metaplan-design.md` (§8 binds this campaign; Constitution §2 governs)
**Provenance:** The capstone campaign of Year 2 and its final campaign, following
Campaign Y2-3 (The Tongues, merged 2026-07-08). Year 2 built two peoples layer
by layer — psychology (Y2-1), perception (Y2-2), language (Y2-3). The Meeting
adds almost no new machinery; its job is to **prove, measure, and present** what
the four prior campaigns built, and to cash the year's exit criterion (metaplan
§2). This spec opens with the book (book-driven development) and answers to the
metaplan; every binding decision in metaplan §8 carries here unchanged unless
recorded otherwise below.

---

## 1. Goal

Cash the Year-2 exit criterion (metaplan §2): one world carrying two species
that differ only in their authored parameter vectors yields legibly different
languages and religions from the same sky — and prove it with a preregistered
comparative study suite, a falsifiable blind-attribution metric, and a null
control that rules out stream noise as the explanation. Deliver the capstone
artifact that puts the two peoples side by side on one seed, every divergence
recountable by `why` to a psychology, perception, or articulation parameter.
Close the book on Year 2.

The Meeting introduces exactly one piece of new generative machinery — a
**study-scoped species roster** that lets the Lab build worlds with a chosen set
of species without touching the shipped generation path — and uses it to stand
up the **null control**. Everything else is measurement, formalization, and
presentation of already-working parts.

## 2. Design principles

1. **The shipped path does not move.** `hornvale-worldgen`'s default roster is
   exactly today's `{goblin, kobold}`. Every shipped world, almanac, census,
   and reference artifact is **byte-identical** to its pre-Meeting form,
   asserted by test (§3, §8). The goblin-identity guarantee (metaplan §5, risk
   3) is restated at the roster boundary: adding a roster parameter must change
   nothing about a default-roster build.
2. **The twin never ships.** The null-control twin exists only inside Lab
   studies and tests. No real world ever carries three peoples; the species
   registry that shipped worlds read is unchanged.
3. **Preregister direction, pin exact after (ADR 0016).** Every suite
   hypothesis states its direction before the census runs. Exact rates and
   counts are pinned only after measurement, never tuned to pass. A failed
   threshold is an alarm to investigate and surface to the owner, not a number
   to lower (the Y2-2 blind-attribution precedent: the honest 0.875 was pinned,
   the rule left untouched).
4. **A null result must not be tunable.** The distributional-indistinguishability
   claim (§4.2) passes against a threshold **derived from sampling theory**, not
   from the observed distances — so "the twin looks like a goblin" cannot be
   engineered by widening a post-hoc tolerance.
5. **No new machinery beyond the roster.** Distributional distances are
   hand-rolled on `std`; no new dependencies (ADR 0004). No new domain, no
   vector widening, no new generation on the shipped path. The suite reuses the
   existing metric extractors and `pick_kobold` rule wherever they already
   answer a hypothesis.

## 3. The species roster

`hornvale-worldgen`'s world build gains an explicit **species roster**: the
ordered set of `SpeciesDef`s a world is built from. Today this set is implicit —
`species::registry()`, read at the composition root. The Meeting makes it a
parameter with a default equal to that registry.

- **Default preserves byte-identity.** A build with the default roster produces
  the same ledger, in the same order, as today. This is the campaign's
  load-bearing determinism guarantee and gets a direct byte-identity test
  (default-roster world at a fixed seed equals the pre-Meeting committed world).
  The roster parameter lives at the composition root only; no domain crate is
  edited to add it (layering, ADR 0002).
- **The twin, and why the roster is *solo*.** `goblin-twin` is a **clone of
  goblin's `SpeciesDef`** — all three vectors (psychology, perception,
  articulation) identical — under the fresh name `"goblin-twin"`. Because every
  labeled stream keys on the species *name* (`settlement/goblin-twin/population`,
  the naming salt), the twin has **identical vectors and independent noise**.
  It cannot, however, be placed *beside* goblin in one world: placement scores
  come purely from the psychology vector with no stream noise, and `place_tagged`
  (`domains/settlement/src/placement.rs`) breaks exact ties by tag order, so two
  identical-vector species score identically at every cell and the first-tagged
  one greedily takes the entire spaced scatter — the second places nothing. The
  null control therefore uses **two solo rosters**, `[goblin]` and
  `[goblin-twin]`, run over the same seeds. With no competitor each lands in the
  **identical cells** (identical scores, identical tie-breaks), so placement,
  biome, subsistence, and observed phenomena are identical per seed; the two
  differ *only* in name-salted draws — the generated names, and the population
  (`draw_population` and `draw_species_population` share one capacity curve and
  jitter, drawing the same distribution from independent streams). That is
  exactly the confound metaplan §2 requires controlled: identical vectors,
  independent stream noise.
- **Where the roster is chosen.** The roster is a Lab concern, expressed in the
  study format (§5) and reachable in tests. The CLI's shipped verbs
  (`new`, `almanac`, …) always use the default roster; the roster is not a
  user-facing world-generation pin.

## 4. The null control

Metaplan §2 requires the twin to be **both** distributionally indistinguishable
from goblins **and** to score at chance on blind attribution. The Meeting runs
the two solo rosters (§3) over one shared seed range and, comparing the two by
seed, asserts both, preregistered (ADR 0016).

### 4.1 At chance (primary tooth)

The existing `pick_kobold` structural rule (`windows/lab/src/metrics.rs`) — a
symmetric function of an unordered pair of pantheon signatures, lexical channels
deliberately absent — is run per seed on the `(goblin-solo, goblin-twin-solo)`
signature pair (reconstructed in the calibration from the two pin sets' recorded
signature columns, since the two live in separate builds). Because both are
goblin-vectored and land in identical cells, their signatures are identical
except where population noise crosses a structural threshold, so the rule mostly
returns *indistinguishable* and, when it does decide, picks the twin no better
than a coin. **Preregistered direction:** the twin-picking rate among decided
pairs is statistically indistinguishable from 0.5, and the indistinguishable
rate is high — in stark contrast to the standard roster's 0.875 / perfect-on-
mooned (metaplan §2, Study 007). Exact rates and counts are pinned as a
calibration row after measurement.

### 4.2 Distributionally indistinguishable (supporting tooth)

A new set of `goblin-twin` signature metrics lets the calibration compare, per
structural metric, the distribution of `goblin-twin-solo` against the
distribution of `goblin-solo` over the shared seed range:

- **Categorical / flag metrics** (head-deity domain, cult form, verticality,
  subsistence, …): **total-variation distance** between the two empirical
  distributions.
- **Numeric metrics** (pantheon size, population, name length, surplus, …):
  **standardized mean difference** (mean gap in pooled-standard-deviation
  units).

**Preregistered pass criterion:** every distance falls within a bound derived
from **sampling theory**, not from the observed distances — the multinomial / CLT
two-sample sampling-error envelope at the census's per-metric sample size. The
**correlation adjustment** now runs in the safe direction: because the two solo
builds share the same seed, land in the same cells, and observe the same
phenomena, their signatures are **positively** correlated (they diverge only
through independent population/name noise), so the independent-two-sample
envelope is a **conservative upper bound** — the true distances are smaller than
independence predicts, never larger. The calibration states this explicitly and
pins the measured distances beneath the independence bound; no inflation factor
is needed (contrast the rejected two-slot design, where competition would have
*anti*-correlated the species and demanded one). If any distance exceeds the
bound, that is a STOP-and-report condition (the twin is not behaving like a
goblin — a real finding), not a bound to widen.

The distributional metric is a structure-only comparison; no lexical channel
(names, epithets, tenets) enters it, mirroring `PantheonSig`'s discipline
(the twins' names *do* differ — that is the point of the noise — so a
name-length distance is expected near zero but is not the claim's substance).

## 5. Study 009 and the roster schema

**Schema extension.** Each `PinSet` (`windows/lab/src/study.rs`) gains an
**optional roster field**: the name of the species roster that pin set's worlds
are built from. The field is optional with `#[serde(default)]` `None` meaning the
shipped roster, so **every existing `studies/*.study.json` is unchanged and still
validates and produces byte-identical rows** (no migration, no new CSV column —
the roster is a build input selected per pin set, and the existing `pin_set`
label already distinguishes populations). Roster names resolve against a **small
closed set the Lab knows** — `default` (the shipped `{goblin, kobold}`),
`goblin-solo` (`[goblin]`), and `goblin-twin-solo` (`[goblin-twin]`) — with an
unknown name a loud `StudyError`. This is not an arbitrary user-authored
species-injection surface (ontology-trap posture, metaplan §5).

**Study 009, "The Census of the Meeting."** A new committed study carrying:

1. the **comparative divergence suite** over the standard roster at 10,000
   worlds (§6), and
2. the **null-control solo populations** — the `[goblin]` and `[goblin-twin]`
   rosters as two pin sets over one shared seed range (§4).

The committed, CI-drift-checked artifact is at **500 seeds** —
`studies/census-of-the-meeting.study.json` with its generated summary under
`book/src/laboratory/generated/census-of-the-meeting/`, added to CI's "Artifacts
are current" step exactly as `census-lands-drift` is (the established precedent:
only the 500-seed drift censuses are committed and CI-checked; the 10k runs are
author-time-only). The **10k headline figures** for `study-009.md` come from an
author-time run of the same study file bumped to 10,000 seeds, its provenance
noted on the page as Studies 006–008 do for theirs.

**Calibrations.** Null-control calibrations (§4.1, §4.2) run in the test gate at
500 seeds off a **new solo-roster `LazyLock` census** in
`windows/lab/tests/calibration.rs` that loads `census-of-the-meeting`. This is a
genuinely different population from the shared `DRIFT` census (solo rosters, not
the shipped `{goblin, kobold}`), so it is a *second* shared `LazyLock`, not a
re-run of the same one — the "hang off the shared run, don't re-run the census"
rule (Y2-3 infra note) is honored by sharing the solo census across the
null-control calibrations, not by forcing it onto the standard `DRIFT`.

## 6. The comparative suite (formalization + audit)

The suite metaplan §8 names is, in the main, **already built** as preregistered
calibrations hanging off the drift census:

| Metaplan §8 suite row | Existing calibration(s) |
| --- | --- |
| head-deity domain × activity cycle × lock state | `goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar`, `head_deity_is_eternal_exactly_when_tidally_locked` |
| social verticality × psychology (status basis, in-group radius) | `pantheon_verticality_matches_stratification`, `the_slave_rung_is_an_exact_function_of_rank_surplus_and_scale`, `kobold_structures_never_enslave_and_top_out_with_elders` |
| naming-morphology divergence × articulatory envelope | `epithet_honorific_is_true_for_goblin_and_false_for_kobold`, `phonotactic_validity_is_true_for_every_generated_name`, `name_length_distributions_are_measured_and_pinned` |
| blind-attribution at threshold across 10k | `blind_attribution_beats_chance_decisively` (Study 007) |
| null control | **new** (§4) |

The Meeting's job here is **formalization, not invention**: present these
scattered calibrations as **one preregistration ledger** on the Study 009 page
(§7) — each hypothesis, its preregistered direction, and its measured/pinned
result, the null control included, so a reader sees the whole falsifiability
structure in one place. During planning the three named cross-tabs are audited
against what exists and a **new metric is added only where a genuine gap is
found** (none is expected to be large — coverage is already broad; any addition
is a Rust extractor per ADR 0011, preregistered directionally before it runs).

## 7. The book (opens the campaign)

Book-driven development: the book chapters that frame the campaign are drafted
first.

**`book/src/gallery/the-meeting-seed-42.md`** — the capstone artifact, successor
to `the-gods-seed-42.md`. **Seed 42, one fixed sky**, the two peoples side by
side; prose in the-gods lineage quoting a committed two-species almanac. It is
broader than the-gods page, which is narrowly about sky × rotation × perception
lens: The Meeting's capstone traces divergence across **all three vectors** on a
single fixed sky —

- **psychology** → why the two flagships land in different cells and grow
  different social ladders,
- **perception** → why the two pantheons are differently headed,
- **articulation** → why the names and the myth-voices differ,

every divergence recountable by `why <id>` in the REPL
(`windows/historiography::recount`) to a committed species-vector fact.
`the-gods-seed-42.md` remains as the earlier Year-1/Eyes-era artifact with a
**forward pointer** to the new capstone. The page is hand-authored prose quoting
a **committed** almanac (freshness-swept, drift-checked like the other gallery
pages), not a new generated artifact — no new CLI verb (metaplan §8).

**`book/src/laboratory/study-009.md`** — the preregistration ledger (§6) and the
null control (§4), presented as the falsifiability capstone of the year: the
comparative suite's hypotheses and results, and the twin's at-chance /
within-bound demonstration read against the standard roster's decisive
separation.

## 8. Determinism, identity, and the re-baseline

- **Byte-identity (shipped path).** A default-roster world at a fixed seed
  equals its pre-Meeting committed form, asserted by test. No shipped-generation
  code changes; the roster parameter's default path is the only path shipped
  verbs take.
- **Determinism (solo path).** The solo rosters are deterministic and
  **pin-isolated**: `goblin-twin` draws its own labeled streams; pinning sky or
  terrain consumes the same draws on the solo path as on the unpinned path
  (pin-isolation tested, as every new drawn path is). The twin adds no new
  save-format contract to the shipped world — it is never serialized into a
  shipped `World`.
- **No new stream labels on the shipped path.** `goblin-twin`'s labels
  (`settlement/goblin-twin/…`) exist only inside solo-roster builds; they are
  derived by the same labeling scheme as any species name, so no new constant is
  minted and no shipped manifest changes.
- **Re-baseline, once, and light.** Because no shipped-generation code changes,
  the shipped-path outputs regenerate **byte-identical** and this is *verified*,
  not assumed (a drift there is a bug in the roster refactor): the gallery
  almanacs, the 10k author-time censuses' existing columns, and — importantly —
  **every existing column of the committed `census-lands-drift`**. The one
  intended change to a committed pre-Meeting artifact is that `census-lands-drift`
  (`metrics: "all"`) gains the new `goblin-twin`/cyclic-share metric columns, all
  `Absent` on the shipped `{goblin, kobold}` roster — new columns, no changed
  values, exactly as each prior campaign's added metrics re-baselined it. The
  genuinely new regeneration at campaign end is: `census-of-the-meeting`'s
  summary, the capstone almanac quoted by the new gallery page, and the
  model-card / freshness sweep. One re-baseline task, at the end (metaplan §9).

## 9. Constitutional compliance

Two-domain layering untouched — the roster lives at the composition root, no
domain edited (ADR 0002). No new dependencies; distributional distances
hand-rolled on `std` (0004). All registries and rosters ordered `BTreeMap`/`Vec`
(0005). No new drawn quantity on the shipped path; `goblin-twin`'s draws use the
existing name-keyed labeling, no new permanent label (0006). Seeds never
retried (0007). The roster selector is JSON (0012). No ML at runtime — the twin
is authored data, the distances are arithmetic (0009). New suite metrics (if
any) are Rust extractors (0011). All suite claims preregistered (0016). No
`HashMap`/`HashSet`, no wall-clock (Constitution). DoD includes the book (0013):
the capstone, the Study 009 page, two model cards, the chronicle, the freshness
sweep, and the concept-registry review.

## 10. The book close

- **Model cards.** `book/src/domains/species.md` and
  `book/src/domains/language.md` each carry a model card declaring every
  parameter **derived / approximated / drawn / authored** (most species and
  language parameters are authored data or seeded draws — the cards state which
  plainly, and note any banked-but-idle dimensions, e.g. deliberation latency,
  crepuscular activity, voice-loudness's banked derivation).
- **Chronicle.** The Year-2 chronicle entry for The Meeting
  (`book/src/chronicle/`), closing the year.
- **Freshness sweep.** The gallery pages and domain chapters that quote almanac
  text or cite the suite (the-gods gallery's forward pointer, the laboratory
  overview, the species/perception/language/religion chapters where they name
  the exit criterion or the null control).
- **Concept-registry review.** A review of the campaign's predicate vocabulary
  (no new shipped predicates are expected; the review confirms it).

The book carries **no engineering-process vocabulary and no idea-registry IDs**
(the `cli/tests/docs_consistency.rs` lint enforces this); `docs/vision` stays out
of the book.

## 11. Success criteria

1. **Byte-identity holds.** Default-roster worlds and all shipped gallery
   almanacs are byte-identical to their pre-Meeting forms; `census-lands-drift`'s
   existing columns are byte-identical (it gains only new `Absent` twin columns);
   the 10k author-time censuses' existing columns are byte-identical (test + CI
   drift check).
2. **The null control passes, both teeth.** Comparing the `goblin-solo` and
   `goblin-twin-solo` populations by seed: blind attribution is at chance (§4.1),
   and every structural distribution distance is within the (conservative)
   sampling-theory bound (§4.2) — both preregistered, both pinned after
   measurement.
3. **The suite is a legible preregistration ledger.** Study 009 presents every
   comparative hypothesis with its preregistered direction and measured result,
   the null control among them; Study 009 is committed and drift-checked at 10k.
4. **The capstone recounts.** `the-meeting-seed-42.md` puts the two peoples side
   by side on seed 42 under one sky, and every divergence it names is traceable
   by `why` to a psychology, perception, or articulation parameter.
5. **The book is closed.** Two model cards, the Year-2 chronicle, a completed
   freshness sweep, and the concept-registry review are merged; the book does
   not lag merged reality.
6. **The gate is green.** `cargo test --workspace`, `cargo fmt --check`,
   `cargo clippy --workspace --all-targets -- -D warnings`, and the CI artifact
   drift check all pass.

## 12. Explicitly deferred

No new domain; no vector widening (any widening needs its own campaign, metaplan
risk 1). No new generation on the shipped path. No lexicon, syntax, sound
change, or deep time (the no-lexicon line stays bright, metaplan risk 4). No
society across the full settlement scatter, no diffusion/relatedness studies, no
epistemic layer, no per-individual variation (all Year-3+ per metaplan §9). The
twin is a control instrument, not a third people — it never ships.
