# The Chorus (C4) — Design

**Date:** 2026-07-18
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop)
**Campaign:** C4 of the self-writing-book program
(metaplan: [program metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md) §3 C4, §8 flag 3)
**Theory:** registry rows LANG-36 (the epistemic filter stack) and LANG-41
(the measurable dial); LANG-40 grounds the later grammaticalization, not this
floor.

This is the campaign that turns "what goblins understand" on — and it
carries the program's biggest bet: **if the distinctiveness × recoverability
dial does not separate good accounts from bad in practice, the chorus falls
back to a taste gate** (metaplan §8 flag 3). The bet gets a preregistered
falsification protocol (§6), not a hope.

---

## 1. What this is

Through C3, The Book has one voice: the god's-eye gazetteer, plus each
people's single self-statement in its own tongue. C4 adds the chorus: for
every placed people, a **cultural account** of the same entries — ground
truth composed through that culture's epistemic filters — rendered as an
**emic** paragraph in Common, with a sparse **etic margin** carrying exactly
the ground-truth facts the filters lost or corrupted (the Neverending-Story
second register). The god's-eye gazetteer is unchanged; it is revealed as the
null-filter special case.

A cultural account is **derived, never authored** (LANG-36): four per-culture
filters, each a pure function of state Hornvale already computes —

| Filter | Question | Existing input (all shipped) |
|---|---|---|
| lexicon | has the concept? | `ExposureClass` (Steeped/KnowsOf/Unknown+`GapReason`) per culture |
| knowledge | could it know this fact? | vantage (settled cells, `exposure_of`'s rules) × capability, derived from the `PerceptionVector` (activity, night_vision, sky_attention) |
| ontology/salience | carves & ranks it how? | the culture's held concepts + the `PerceptionVector` as salience weights |
| valence | values it how? | the `PsychVector` (the `voice_params` derivation precedent) |

The causal-model filter is **C5's** (metaplan). Authoring cost stays
O(cultures × schemas): the only authored artifacts are small build-state
tables (a per-predicate observability table, a valence-framing table), never
per-culture-per-fact content.

## 2. What exists (verified in code, 2026-07-18)

- `windows/book` renders `lines` (god's-eye Common), `tongue_lines`
  (self-statements), `tongue_gaps` (a hand-enumerated planet-gap report —
  C3's retro names deriving it as C4's first structural task).
- `domains/language` holds the pure mechanisms (clause, grammar, lexicon,
  register); `windows/worldgen` derives per-culture params from authored
  vectors (`voice_params`) — the derivation pattern C4 copies.
- **No TECH-1 capability exists in code.** Species differ epistemically today
  only through perception, psychology, and placement.
- `hornvale-lab` metrics are `fn(&FullView)` in a single `registry()`;
  the canonical census runs `"metrics": "all"` — **every registered metric
  is census-visible** (carve-out, §7/§9).
- The Echo's corpus law is in the standing gate: every Common Book line must
  parse and re-realize byte-identically.
- Parallel campaign in flight: *The Correspondence* (draft spec on main,
  PROC-16) types concept-registration edges — adjacent, not colliding; its
  concept migrations would move lexeme-space if they land mid-C4 (absorb at
  stage boundaries; verify census-clean per absorption, C2's lesson).

## 3. Architecture

### 3.1 The structured account (`domains/language/src/account.rs`)

The measured object is **structured and surface-free**; Common rendering is
downstream. This makes "differs beyond vocabulary" structural: the dial never
sees strings, and two accounts that differ only in word-choice are literally
identical at the measured layer.

```rust
/// One ground-truth fact as the filter stack receives it: predicate name,
/// subject label, value — domain-neutral (language imports no sibling).
pub struct GroundFact { pub subject: String, pub predicate: String, pub value: FactValue }

/// What one filter stack did with one ground fact.
pub enum Disposition {
    /// Survived all four filters.
    Kept,
    /// Lost, with the recountable layer that lost it.
    Lost(LossReason),        // NoConcept(GapReason) | BeyondCapability { domain, required, held }
    /// Corrupted: re-classified into the culture's own held concept
    /// (e.g. `planet` → `earth`); carries both.
    Substituted { truth: String, theirs: String },
}

/// A culture's account: every ground fact with its disposition, in the
/// culture's own salience order, plus a per-entry valence stance.
pub struct Account { pub entries: Vec<AccountEntry>, /* … */ }

/// Pure; deterministic; no draws. The identity params yield the god's-eye
/// account (every fact Kept, ground order, neutral stance).
pub fn account_of(ground: &[GroundFact], params: &AccountParams) -> Account
```

`AccountParams` bundles: the culture's concept holdings (from
`ExposureClass`), the observability table, per-domain capability scalars,
salience weights, and a valence stance table. All fields are derived or
authored build-state; `AccountParams::identity()` is the null filter.

Filters apply in LANG-36's order: **lexicon → knowledge →
ontology/salience → valence.** Distortion decomposes per-filter (selection /
substitution / order / framing), so a mis-calibrated culture names *which*
filter overshot.

### 3.2 Derivation at the composition root (`windows/worldgen`)

`account_params(world, species)` — the `voice_params` twin. Specifically:

- **Observability table** (authored build-state, the construction-table
  pattern; lives at the composition root because it names cross-domain
  predicates): each Book predicate → a requirement class — `Manifest`
  (anyone: `instance-of`, coexisting peoples), `SkyGraded(threshold)`
  (`moon-count`: night observation), `Instrumental` (`star-class`: spectral
  taxonomy — beyond every floor culture), `CrossReferential`
  (`day-length-std`: the *standard-day* unit is etic; LANG-44 numeracy stays
  banked), and `is-a planet` (beyond floor capability → the ontology filter
  substitutes the culture's best held carving, the universal-stratum
  `earth`).
- **Per-domain capability, derived not stored** (LANG-36's determinism
  guard): sky-capability = a pure function of the `PerceptionVector`
  (activity cycle, night_vision, sky_attention); ground-vantage = settled
  cells (as `exposure_of` already computes). **This derivation is a seam:**
  TECH-1's knowledge ladder (MAP-18) later replaces the derivation function
  without touching the filter contract.
- **Salience weights** from the `PerceptionVector` (a sky-rapt people orders
  sky facts first); **valence stances** from the `PsychVector`
  (threat_response, in_group_radius → how coexisting peoples are framed).
  Exact tables fixed at plan time; every input already exists and is
  authored per species.

Zero new seeded draws. Zero new concepts. Zero new facts. No ledger or
save-format change of any kind.

### 3.3 The Book surface (`windows/book`)

Each volume gains a chorus section: per placed people, an **emic paragraph**
in Common (the ethnographer's-translation register — tongue-rendered
accounts are C7's, after LANG-40's depth vector) followed by its **etic
margin**: the sparse structured diff `ground_truth \ emic` (Lost and
Substituted entries only), rendered in a second typographic register.
Illustrative shape (surface values derived at build time; exact constructions
fixed at plan time):

> **As the Vavako tell it.** Vebe is the earth, with two moons. The Vavako
> are goblins. The Babako are hobgoblins.
> *— yet in truth: a planet; orbiting a yellow-white dwarf (F); its day
> lasts about 1.5 standard days.*

Every Common **sentence** in the chorus (emic and margin alike) is subject to
the corpus law: any new construction (the framing appositive, the margin's
truth-clause) ships bidirectional (realize + parse) or the standing gate
reddens. Scaffolding (the section header, glosses) sits outside the corpus
exactly as tongue-line glosses do today.

### 3.4 Structural prep (C3's inheritance, first tasks)

1. **The render inventory.** The per-tongue coverage report is derived from
   an inventory of renderable items (the construction table + tongue
   probes), replacing the hand-enumerated `{self-statement, planet}` list —
   a future renderable fact auto-enters the report.
2. **Promote the planet-probe's success path.** `realize_tongue(planet)`'s
   `Ok` lands in `tongue_lines`; today it silently vanishes (only the `Err`
   arm is consumed). A realizable fact can never again disappear without a
   trace.

### 3.5 The dial (`windows/lab`, census-visible)

LANG-41 operationalized. All metrics are computed on **structured accounts**
(never surface strings), `Extractor::Full`, registered in `registry()` (0011
metrics are code; census-as-data 0045/0046 — the census's `"metrics": "all"`
makes them population-measurable, which the vacuity check *needs*). The
measured quantities (exact roster and formulas fixed at plan time; a small
fixed set, ≤ 8):

- **Distinctiveness** — mean pairwise account divergence (disposition-set
  symmetric difference + rank distance over order + stance delta), and mean
  divergence from ground truth. The anti-uncanny axis.
- **Recoverability** — the fraction of ground-truth facts reconstructible
  from the *emic account alone* plus the culture's filter params (Lost ⇒
  unrecoverable; Substituted ⇒ recoverable iff the substitution is injective
  under the params; order/framing lossless). The anti-gibberish axis.
  (Emic ∪ margin ⊇ ground truth holds by construction — the margin is the
  complement — so recoverability is only meaningful pre-margin.)
- **Calibration** — per domain, distortion must fall as capability rises
  (rank-based monotonicity across a world's cultures; population-read across
  the census). A culture off the line is a *detectable bug*, and the
  per-filter decomposition names the filter.
- **Vacuity (population)** — inter-account variance; `MetricValue::Absent`
  on worlds with < 2 placed peoples. Reported **alongside filter-param
  spread**: goblin and hobgoblin perception vectors sit near the same
  baseline, so a low-variance world can be honest clone-inputs rather than a
  vacuous stack — the pair of numbers distinguishes the two readings (the
  roster, not the stack, sets the attainable diversity budget — LANG-45).

A dedicated preregistered study (`studies/the-chorus.study.json` — studies
are data, 0011) runs the dial over a modest seed range for the Lab chapter's
calibration readout; seed count fixed at plan time under the local-runtime
budget (the census remains the only 1000-world instrument).

## 4. The laws (standing tests)

1. **The null-filter law.** `account_of(ground, identity)` reproduces the
   god's-eye volume's content exactly — the gazetteer *is* the chorus's
   degenerate case, asserted byte-identically at the surface.
2. **The derivation law.** Accounts are pure functions of the world: same
   seed → byte-identical chorus (the C3 reconstruction idiom).
3. **The margin law.** For every culture: emic ∪ margin ⊇ ground truth, and
   the margin contains *only* Lost/Substituted entries (sparseness — it
   fires nowhere else).
4. **The corpus law (inherited, extended).** Every Common sentence the
   chorus emits parses and re-realizes byte-identically.
5. **The known-groups law.** The falsification protocol's criteria (§6),
   as a standing test over the measured seeds.
6. **Coverage.** The derived render inventory subsumes today's report
   line-for-line on seeds 1–3 (no regression), and the planet-probe success
   path is exercised by a test that grants a synthetic tongue the `planet`
   concept (mutation-verified, not narrated).

## 5. Determinism and blast radius

- **No epoch.** Zero new draws, zero new streams, zero new concepts, zero
  new facts. Genesis is byte-identical (The Individuation's shadow posture);
  `world.json` for every seed is untouched. Filters and their tables are
  **build-state** (metaplan ratified decision #1): changing them
  drift-checks the Book, never a save.
- **Local artifact regen:** `the-book.md` gains chorus sections (existing
  sections byte-identical — the null-filter law pins this); no other gallery
  artifact moves.
- **Census (carve-out):** the dial metrics enter the census schema. One
  authorized AWS regen pre-merge (`make regen-remote`), after an
  absorb + preflight (the regen-races-the-epoch lesson). **Requested of
  Nathan explicitly at G3; never run locally; never assumed.** Until it
  runs, census fixtures lag by exactly the new columns — the standing
  chosen trade.
- **type-audit:** new pub-boundary primitives tagged at introduction
  (re-pin lag was The Echo's noted recurrence).

## 6. The bet, preregistered (metaplan §8 flag 3)

**Claim under test:** distinctiveness × recoverability separates good
accounts from bad — the uncanny↔gibberish dial is measurable, not taste.

**Protocol (known-groups validation):** three reference configurations, all
deterministic build-state constants —

- **Null** (`AccountParams::identity()`): the uncanny pole — ground truth in
  every voice.
- **Shipped**: the derived params of §3.2.
- **Pathological** (documented constructor): the gibberish pole — filters at
  destroy-everything strength (every fact Lost or Substituted arbitrarily).

**Preregistered criteria** (a standing test, run over the study seeds):

1. The dial **orders the poles**: distortion(null) < distortion(shipped) <
   distortion(pathological), with null ≈ 0 recoverability-loss and
   pathological ≈ 0 recoverability.
2. **Shipped sits in the good band**: distinctiveness above the null pole's
   by a preregistered margin AND recoverability above the pathological
   pole's by a preregistered margin (numeric thresholds fixed at plan time,
   before any measurement — preregistration discipline).
3. **Non-vacuity**: on multi-people worlds with non-clone param spreads,
   inter-account variance > 0.

**Reading the result:** criteria 1 or 3 failing (poles inseparable, metrics
degenerate) **falsifies the bet** → the chorus still ships, the dial demotes
to diagnostics, and the calibration gate falls back to taste — the metaplan's
named fallback, reported honestly at close. Criterion 2 failing alone is
**calibration work, not bet failure**: the instrument reads; the derived
strengths need tuning. The distinction is the campaign's honest-measurement
spine (measure-don't-narrate; the Concordance generator lesson applies to
the reference configurations — the pathological pole must actually exercise
every disposition, asserted, not assumed).

## 7. Non-goals

- The **causal-model filter** and schema library — C5 (LANG-37/38).
- **Doctrine**, institutions, reader-relative rendering — C6 (LANG-39).
- **Tongue-rendered accounts**, evidential/noun-class morphology, the
  grammaticalization-depth vector — C7 (LANG-40 full).
- **Diachrony** — C8 (LANG-42).
- **Numeracy registers** — LANG-44 stays banked (quantities that survive the
  knowledge filter render at Common's existing surface).
- **TECH-1 as content**: capability here is a derivation seam, not a tech
  ladder; no capability facts, no knowledge ladder rungs.
- **No new concepts, predicates, or draws** (C2's lexeme-displacement
  lesson: concept-adds are not lexeme-space-additive; C4 adds none).

## 8. Registry and decisions

- LANG-36 → `spec'd` at spec commit; → `shipped (floor: 4 of 5 filters)` at
  close. LANG-41 → `spec'd`; → `shipped` (or `shipped (dial falsified —
  taste fallback)` — the honest flip) at close.
- New row **LANG-45** (captured with this spec): the roster is the chorus's
  diversity budget — inter-account variance is bounded above by the
  species-authoring spread of perception/psych vectors; a vacuity reading
  must be attributed against input spread before indicting the filter stack.
- Candidate ADR at close: *accounts are derived views; filter tables are
  build-state* (the metaplan's decision #1 applied to the epistemic layer).

## 9. Flagged for G3

1. **The bet and its protocol (§6)** — confirm the known-groups criteria and
   the bet-failure-vs-calibration distinction; this is the program's biggest
   bet, per the metaplan flagged first.
2. **Census carve-out — authorization requested:** the dial metrics are
   census-visible by design (the vacuity check is a population metric). One
   AWS regen pre-merge. Nathan's explicit go/no-go; a no leaves the fixtures
   lagging until he schedules it.
3. **Determinism posture** — zero draws / zero concepts / no epoch / genesis
   byte-identical; filters are build-state. Confirm the shadow posture.
4. **Emic accounts render in Common at the floor** (the
   ethnographer's-translation register); tongues deepen at C7. Confirm the
   narrowing *out loud* (C3's retro lesson: name a narrowing the program doc
   didn't).
5. **Surface shape** (§3.3's illustrative block): the "As the ⟨people⟩ tell
   it" header, the italic margin register, and the margin's truth-clause
   construction — taste-adjacent surface Nathan may want to adjust.
6. **The homogeneity reading** — seed 1's chorus (goblin + hobgoblin,
   near-clone perception) may measure honestly low-variance; the paired
   variance/spread reporting is the designed answer, not padding the floor
   to game the dial.
