# The Explanations (C5) — Design

**Date:** 2026-07-18
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop)
**Campaign:** C5 of the self-writing-book program
(metaplan: [program metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md) §3 C5)
**Theory:** registry rows LANG-37 (the causal-schema library) and LANG-38
(the metaphor-mapping layer). Builds directly on C4's shipped filter stack
(LANG-36) — this is the fifth filter, the one C4 deferred.

---

## 1. What this is

C4's chorus says what each culture *holds*; C5 says **why they think it is
so**. The causal-model filter turns a culture's engagement with a fact into
a folk explanation, derived — never authored per culture — from three
authored, closed, build-state tables and the world's own generated
furniture:

> **As the Vavako tell it.**
> The Vavako are goblins — ourselves.
> The Babako are hobgoblins — neighbors.
> Vebe is the earth. The day returns because ⟨sun-deity⟩ walks the sky —
> briskly.
>
> *In truth, Vebe is a planet with two moons, orbiting a yellow-white
> dwarf (F); its day lasts about 1.5 standard days.*

(Surface illustrative; constructions fixed at plan time. The margin is
unchanged — an explanation is not a recovery, so the true value stays in
the second register.)

The three-layer architecture is LANG-38's, kept deliberately un-collapsed
(the UNI-28 antichain lesson): a **substrate** of embodied source-domains
(motion, force, container, path, balance, link, verticality, …), read two
ways — the **schema read** (a source-domain as a causal story: the ~12
embodied schemas of LANG-37) and the **mapping read** (a source-domain as a
descriptive projection: duration-as-travel) — then **lexicalization**
(walks/rides/crawls) through a conjunction of gates and a β-weighted draw.

## 2. What exists (verified in code, 2026-07-18, main @fa81a4c)

- C4 shipped: `hornvale_language::account` (four filters,
  `Disposition::{Kept, Lost, Substituted}`, the dial measures), worldgen's
  `chorus.rs` (params derivation, `accounts_of`), the Book's chorus
  sections with the corpus-law-bidirectional surface, the dial in the
  census. The causal filter is the named gap.
- **Binding furniture:** `hornvale_religion::Belief { deity, epithet,
  source_kind, sentiment, high_god }` — every deity carries the phenomenon
  it mythologizes (`derived-from-phenomenon`), so the sun-linked and
  moon-linked deities of each culture's own pantheon are directly
  addressable; cyclic beliefs carry committed periods.
  `hornvale_culture::subsistence(BiomeClass, coastal)` (Farming / Herding /
  Fishing / Foraging); the psych vector (`status_basis`, `sociality`);
  `Stream::weighted_index` as the draw primitive.
- `packs.rs` is the authored-closed-table precedent the schema library
  copies (static `&'static [Entry]` tables; register-once; drift-checked
  downstream).
- The Echo's standing gate: any new Common construction ships
  bidirectional.

## 3. Architecture

### 3.1 The substrate and the two reads (`domains/language/src/schemas.rs`)

Pure, kernel-only, authored build-state — the packs.rs shape:

- **Source-domain substrate** (closed): motion, force, container/boundary,
  path/journey, balance, link, verticality, kinship, flow, cycle.
- **The schema table** (closed, 12 rows — LANG-37's list): force-dynamics,
  agentive/animist, substance-flow, container/boundary, path/journey,
  balance/equilibrium, link/sympathy, kinship/descent,
  moral-accounting/debt, cycle/return, essence/telos, plus the row each
  reads from the substrate, the **fact-shapes it can explain**, and the
  **slot types it must bind** (an agent slot for agentive; a kin slot for
  kinship; none for cycle).
- **The mapping read**: per source-domain, the descriptive projection and
  its closed **lexeme table** with per-lexeme gates — sub-frame
  (subsistence: Herding → mounted verbs; Fishing → rowing/steering;
  Farming/Foraging → walking/carrying), agent-fit (the deity's sentiment:
  Eternal → unhurried variants), and manner slot.
- **The fact-shape classifier** (the pass-1 hidden hub, now first-class):
  a fact's shape — cyclic-event, high-scalar-state, count, taxonomy —
  derived from its predicate's observability row plus its value type; the
  shape is what admits schemas.

Explanation is a new disposition in `account.rs`:

```rust
/// The causal filter's contribution (C5): the fact is held AS EXPLAINED —
/// a schema fired, bound to the culture's own furniture, and lexicalized.
Explained {
    /// What the knowledge filter said underneath (Lost precision or Kept).
    underlying: Box<Disposition>,
    schema: SchemaId,
    /// The bound agent (a deity name), when the schema has an agent slot.
    agent: Option<String>,
    /// The drawn lexeme and its manner (surface-free identifiers).
    lexeme: LexemeId,
    manner: Manner,
}
```

Structured and surface-free, like everything the dial measures.

### 3.2 Selection, binding, and the draws (`windows/worldgen/src/chorus.rs` extension)

- **Prior** (derived, no draw): schema weights per (culture, domain) from
  subsistence (pastoral → kinship/cycle up; hunter → animist up; coastal →
  flow up — LANG-37's ecological gating), psychology, and sociality.
- **β, the monomania dial** (derived): from `status_basis` (Knowledge →
  low β, domain-partitioned explainers; Rank → high β) with a `sociality`
  modifier; exact numbers at plan time. β sharpens the prior before the
  draw.
- **Schema draw** (seeded, render-time): one per (species, domain,
  fact-shape), label `language/<species>/schema/<domain>/<fact-shape>`
  declared in language's streams module — the prior is per-domain (that is
  what β's monomania sharpens), but the fact-shape gate admits different
  schema subsets per shape, so the draw resolves per shape; a culture
  stays coherent within a shape class (its cyclic events share one story)
  without forcing the day's schema onto a count fact. **Why a draw at all:** deriving the choice wholly from culture
  vectors would ship astrology as science — C3's copula precedent; the
  draw is the contingency honesty mechanism (ledger #1).
- **Binding** (derived): the schema's slots bind from the culture's own
  furniture — the agent slot from `beliefs_held_by` via `source_kind` (the
  sun-linked belief explains the day). **Unbindable → don't fire** (the
  entry stays plain `Lost`/`Kept`); no synthetic agents, ever.
- **Lexeme draw** (seeded, render-time): over the gate-surviving lexemes,
  label `language/<species>/lexeme/<fact-key>`. Mostly-deterministic by
  construction (the gates usually leave 1–3 candidates).
- **Manner** (derived, fully emic — ledger #5): a cyclic deity's manner is
  its period's **rank among the culture's own cyclic beliefs** (the 1.5-day
  sun against the 29-day moon → the sun-god is the brisk one). No etic
  anchor; a single-cyclic pantheon takes the neutral manner.

### 3.3 What fires at the floor (ledger #2)

Per culture, at most two explanations (composition depth 1, the dry
register):

1. **The day** — the universal lived cycle. C4 loses `day-length-std` as a
   value (`CrossReferential`); C5 wraps that loss:
   `Explained { underlying: Lost(BeyondCapability), … }`. The emic account
   speaks the explanation; the margin still carries the number.
2. **The moons, where kept** — a culture that holds `moon-count` (kobold,
   bugbear) may explain what it sees: `Explained { underlying: Kept, … }`.

Selection is real competition (ledger #4): the day's fact-shape admits
several schemas (agentive, cycle/return, path/journey, balance); the
derived prior weights them; cultures can genuinely explain the same sky
through different schemas.

### 3.4 The surface and the corpus law (`windows/book`)

One new Common construction, bidirectional per the Echo gate: the
because-clause — `⟨emic fact clause⟩ because ⟨agent⟩ ⟨lexeme verb-phrase⟩
⟨manner⟩.` (agentless schemas realize their own closed frames: "the day
returns, as all things return" for cycle — exact tables at plan time).
Every inventory the parse needs is closed (the lexeme tables, the manner
adverbs, the schema frames), so inversion is table lookup; every chorus
line — now including explanation sentences — must round-trip
byte-identically. Tongue-rendered explanations are **C7's**.

### 3.5 Dead metaphors (LANG-38's loop, floor slice)

The live/dead boundary ships as a **guard, not a dynamic**: lexicalized
compounds already in the lexicon (`sea` = "many water") are dead mappings
and are never re-explained as live (the double-dip guard is a test).
Lexicalization-over-time — the engine's output re-entering the lexicon —
is deferred (its own registry note; it is structurally the etymology
engine's territory).

## 4. The laws (standing tests)

1. **The null law:** identity params produce zero `Explained` dispositions
   — the god's-eye volume is untouched, byte-identical.
2. **The binding law:** every `Explained` entry's agent names a belief the
   culture actually holds (checked against `beliefs_held_by`); a culture
   with no sky-linked belief has no day-explanation and its entry is plain
   `Lost` (the unbindable guard, exercised by a measured seed or a
   synthetic pantheon).
3. **The derivation law:** same seed → byte-identical explanations (the
   render-time streams are permanent labels; the C3 reconstruction idiom).
4. **The corpus law:** every explanation sentence parses and re-realizes
   byte-identically.
5. **The anti-vacuity pair (ledger #3, in lieu of census metrics):**
   *lexeme-level* — cultures whose gate-sets differ must not all draw the
   same verb across the measured seeds (the uncanny-literalism check);
   *schema-level* — priors that differ must be able to select different
   schemas (asserted over the measured seed set; if every culture always
   picks the same schema, the competition is vacuous and the test says so).
6. **The margin law, inherited:** an explanation never removes margin
   content — `Explained{underlying: Lost}` still margins the true value.

## 5. Determinism and blast radius

- **No epoch. No ledger change. Genesis byte-identical.** The only new
  seeded draws are the two render-time leaf draws on NEW stream labels in
  language's streams module (new labels are additive-safe; the
  lexicon/grammar precedent — nothing about an explanation is persisted).
- **No new concepts, no new predicates, no new facts.**
- **No new lab metrics — zero census schema change, zero AWS regen**
  (ledger #3). C5's measurement is the standing-test law set (§4.5).
- Local artifact regen only: `the-book.md` chorus sections grow
  explanation sentences; the 50-seed chorus study fixture is expected
  byte-identical (the study's six metrics read dispositions that C5 only
  wraps — **verified at plan time, not assumed**: if `Explained` changes
  any dial value, that is a flagged finding, not a silent re-pin).
- type-audit tags on all new pub primitives at introduction.

## 6. Non-goals

- Doctrine, institutions, reader-relative rendering — C6 (LANG-39).
- Tongue-rendered explanations, evidential morphology — C7 (LANG-40).
- Diachrony, lexicalization-over-time, schism — C8 / the etymology engine.
- Religion's tenet renderer (`render_line`) is untouched — explanations
  live in the Book's chorus, not in worship.
- No cosmogony (why the earth exists) — the scope trap; the floor explains
  the day and the moons only.
- No new census metrics, no AWS spend (ledger #3).

## 7. Registry and decisions

- LANG-37 → `spec'd` at spec commit; → `shipped (floor: full library +
  competition; 2 fact-classes fire)` at close. LANG-38 → `spec'd`; →
  `shipped (floor: substrate + mapping read + lexeme gates; dead-metaphor
  guard static)` at close.
- Registry note at close: lexicalization-over-time deferred to the
  etymology engine (a LANG-38 Where amendment, not a new row, unless the
  close surfaces a genuinely new mechanism).
- Ledger #1–#8 promoted into this section at G3 resolution.

## 8. Flagged for G3

1. **Determinism posture:** two NEW seeded stream labels
   (`language/<species>/schema/<domain>/<fact-shape>`, `language/<species>/lexeme/
   <fact-key>`), render-time only — new labels are additive-safe, no
   epoch, genesis byte-identical. This is the program's
   minimize-new-draws principle applied: the draws exist because deriving
   surface choice from psychology would ship astrology (C3's copula
   precedent) — confirm the posture.
2. **Census carve-out — explicitly NOT spent:** no new metrics, no regen
   (ledger #3). The chorus study fixture is expected byte-identical and
   verified at plan time. Confirm you're happy deferring explanation
   metrics to a future batched metric campaign.
3. **The manner frame (ledger #5):** pantheon-internal period rank, no
   etic anchor — the one place a lazy design would have leaked
   standard-days into a culture's mouth. Confirm the frame.
4. **β from `status_basis`** (ledger #6): Knowledge → pluralist, Rank →
   monomanic. Semantically motivated but the least-precedented derivation
   in the campaign — veto point if it reads as astrology rather than
   sociology.
5. **Surface shape** (§1's illustrative block): the because-clause, the
   agentless frames, the manner adverb's placement — taste-adjacent.
