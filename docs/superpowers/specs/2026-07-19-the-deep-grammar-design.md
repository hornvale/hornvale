# The Deep Grammar (C7) — Design

**Date:** 2026-07-19
**Status:** **Approved at G3 (2026-07-19)** — non-additive tongue-line blast radius confirmed; census untouched; ledger digest due at G6.
**Campaign:** C7 of the self-writing-book program
(metaplan: [program metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md) §3 C7)
**Theory:** registry row LANG-40, full — the five filters grammaticalize;
the floor slice (drawn order/copula/articles) shipped in C3, and
`grammar.rs`'s own module doc names C7 as its extension seam. LANG-43
(irregularity as cascade residue) is adjacent theory; this campaign lays
its substrate without opening it.

---

## 1. What this is

Until now the worldview lived in *what* the tongues say. This campaign
puts it **in the words themselves**: evidential morphology (how does the
speaker know?) and noun-class morphology (what kind of thing is it?),
assembled morphologically from each tongue's own phonology, with a drawn
grammaticalization-depth vector deciding how much of it is obligatory.
The Book's payoff is one visible contrast — the same proposition, twice:

> *(the Tongues section, goblin — a deep-evidential draw)*
> Saa We Vavako**-⟨WITNESSED⟩**. (in the goblin tongue: "The Vavako are
> goblins.")
> Saa Vebe ⟨earth-word⟩**-⟨WITNESSED⟩**. ("Vebe is the earth.")
>
> *(the priesthood's section)*
> Saa Vebe ⟨earth-word⟩**-⟨TAUGHT⟩**. ("Vebe is the earth — as it is
> taught.")

(Illustrative shape only: every morpheme is a drawn form, not authored
text; a species whose depth draw lands at 0 shows no marking — Common
never does; the exact seed-1 surfaces are measured then pinned.)

The evidential's **value is never a new judgment** — it is the shipped
epistemic state, read out: a Steeped-grounded statement is *witnessed*
(the lexicon's exposure class — LANG-40's knowledge→evidentiality mapping
executed on existing state); an institutionally transmitted line is
*taught* (C6's doctrine speaking in-tongue — the reportative used in
instruction, attested typology); an explanation-derived statement would
be *inferred* (C5's `Explained{Lost}` — defined in the enum, unreachable
at the Classify-only floor, synthetically tested and loud-guarded).

## 2. What exists (verified, main @7143d3f)

- `TongueGrammar { order, copula: Option<String>, articles }` +
  `realize_tongue(TongueClause { subject, complement_concept })` —
  render-only, Classify-only; the copula's form is drawn from the
  tongue's own phonology by the `proto_root` syllable-fill (zero
  authored surface — the mechanism every new morpheme copies).
- The family machinery: `build_lexicon` draws proto-forms per FAMILY and
  evolves them into daughters through the exceptionless cascade — the
  pipeline the cognate morphemes ride.
- `ExposureClass::{Steeped, KnowsOf}` per (culture, concept); C5's drawn
  schema posture per (species, domain, fact-shape); C6's doctrine voices
  and transmission structure.
- The Echo addendum: **only Common is bidirectional; tongues are
  render-only** — no parse obligation for any tongue line.
- The Book's Tongues section (self-statements + the derived probe
  inventory) and the doctrine sections (C6).

## 3. Architecture

### 3.1 The depth vector (drawn; `domains/language/src/grammar.rs`)

Two categories at the floor (the other three filters defer: voice/valency
rides the day tongues gain verbs; honorifics ride valence):

- `evidential_depth`, `noun_class_depth` ∈ {None, Particle, Affix} —
  drawn per species on new permanent labels
  (`language/<species>/grammar/depth/evidential`,
  `language/<species>/grammar/depth/noun-class`), weighted by typological
  frequency (unmarked systems are the majority; grammaticalized ones the
  minority — exact weights at plan time). Common is fixed shallow (no
  marking, no draws). **Drawn, not derived: deriving grammar type from
  psychology is astrology** (C3's copula stance, already constitutional
  in this module).

### 3.2 The morphemes (family-cognate, cascade-evolved)

Each marker's **proto-form draws once per family** (the copula's
syllable-fill, at the proto level), then **evolves into each daughter
through the existing Neogrammarian cascade** — goblin and hobgoblin carry
*cognate* evidential morphemes with regular sound differences, exactly as
their roots do. Depth (whether a species marks at all) stays
species-keyed: morpheme substance is conservative, grammaticalization is
fast — both facts are real typology. Labels:
`language/family/<family>/morph/evidential/<value>`,
`language/family/<family>/morph/class/<value>`, plus the drawn class-marker
position `language/<species>/grammar/class-position` (prefix/suffix — the
one position typology varies; the evidential's position is FIXED
predicate-final: on the copula when overt, encliticized to the predicate
nominal otherwise — don't draw what reality doesn't vary).

### 3.3 Noun class and the animacy coherence link

Two classes — animate/inanimate, the typological floor. **Assignment is
derived, zero draws:** a base table (people-kinds animate;
celestial/terrain concepts inanimate) overridden by the culture's own C5
posture — **a culture whose drawn day-schema is Agentive promotes
celestial nouns to the animate class.** The noun-class system and the
agentive cosmology are one worldview fact read at two layers (LANG-40's
coherence constraint executed on the shipped schema draw — no new draw,
no stored table, no astrology: the contingency entered once, at C5's
draw). Marker on the noun only; agreement spreading is deferred.

### 3.4 Morphological assembly (`domains/language`)

The "not string-concat" mandate: morpheme forms live as **segment
sequences**; affixation concatenates segments and re-renders the
romanization through the existing `render_views`/`segments_of` machinery,
so boundary phonotactics fall out of the same renderer every generated
word already uses. `TongueClause` gains `evidential: Evidential`
(3-valued) and the realizer consults the grammar's depth + the
complement's derived class. Particle-depth marks with a free-standing
drawn word in the drawn constituent position; Affix-depth binds
morphologically.

### 3.5 The Book surface (`windows/book`, `windows/worldgen`)

- The Tongues section gains **the emic world-statement** per placed
  people — `⟨Planet⟩ ⟨cop⟩ ⟨their carving⟩` (realizable for every
  culture: `earth` is universal-stratum Steeped; the C3 planet-gap line
  remains for the etic concept, unchanged) — evidential Witnessed.
- Each organized culture's doctrine section gains **one in-tongue taught
  line**: the same world-statement with the Taught evidential — the
  visible morphological contrast on one proposition pair.
- Glosses in Common as today; tongues stay render-only (no corpus-law
  obligation — the Echo addendum, restated).
- Worldgen supplies the derivation glue (exposure→evidential readout,
  schema-posture→animacy, doctrine transmission); the composition-root
  pattern as in C4–C6.

## 4. The laws (standing tests)

1. **The readout law:** every rendered evidential equals the shipped
   epistemic state (Steeped ⟹ Witnessed on folk lines; doctrine lines ⟹
   Taught) — asserted per measured seed; Inferred is synthetically
   exercised and its unreachable-at-floor status carries a loud guard.
2. **The cognate law:** within a family, two daughters' evidential
   morphemes descend from ONE proto draw — asserted by re-deriving the
   proto form and checking each daughter's form equals its
   cascade-evolution (the C3 reconstruction idiom, at the morpheme
   level).
3. **The coherence law:** for every culture, celestial-noun animacy ==
   (day-schema is Agentive) — the two layers can never disagree, by
   construction and by test.
4. **The depth law:** a species with depth None shows zero marking; depth
   draws are deterministic; Common never marks (fixed shallow).
5. **The self-statement law, inherited:** still never gaps — affixes are
   drawn forms and always exist; the world-statement law joins it (every
   placed people renders its emic world-statement — `earth` is Steeped by
   the universal stratum).
6. **The assembly law:** affixed forms round-trip through the
   romanization machinery (segments → roman) — no naked string
   concatenation across a morpheme boundary (asserted structurally: the
   affixed word's romanization equals rendering its joined segment
   sequence).
7. **Census stability:** lexicons byte-identical (the affix draws are new
   streams, never lexeme-space) — the language reference pages regenerate
   byte-identical and `make census-check` stays ok, verified at the
   surface task.

## 5. Determinism and blast radius

- **No epoch. Zero new facts/concepts/predicates.** New render-time
  stream labels only (depth ×2 per species, class-position per species,
  morpheme proto-forms per family per value) — additive; the
  stream-manifest reference page regenerates in the label-adding task
  (the C5 F1 rule).
- **Deep tongues' existing Book lines CHANGE** — declared blast radius
  (ledger #7): a depth draw landing on a species reshapes its
  self-statement surface ("Saa We Vavako." may gain marking). This is a
  derived-view change (grammar is build-state, decision 0058), locally
  regenerated and drift-checked; **every test pinning an exact tongue
  string re-pins in the same commit** (the golden-pin rule) — glosses
  included.
- Census: untouched and verified (law 7). No new metrics, no AWS.

## 6. Non-goals

- Verbal morphology, agreement spreading, case, voice/valency marking —
  the because-clause stays Common-only; tongues gain verbs in a later
  campaign.
- Honorific/valence morphology — deferred with its filter.
- Irregularity (LANG-43): the cascade substrate ships here; paradigm
  slots and analogical leveling stay raw.
- Tongue parsing — render-only stands (the Echo addendum).
- No changes to accounts, the dial, the conflict map, or any census
  metric.

## 7. Registry and decisions

- LANG-40 → `shipped (full: depth vector + evidential/noun-class
  morphology + animacy coherence)` at close (today: `shipped (floor
  slice)`).
- LANG-43's row gains a Where note at close: substrate shipped (family
  morphemes through the cascade), row stays raw.
- Ledger #1–#8 promoted here at G3 resolution.

## 8. Flagged for G3

1. **Blast radius (leads):** existing tongue lines in the committed Book
   CHANGE where depth draws land on a species — the first campaign in
   this program whose surface delta is not purely additive. Grammar is
   build-state (no epoch; worlds untouched); artifacts regenerate
   locally; pinned strings re-pin in the drifting commit. Confirm you're
   comfortable with the Book's tongue lines moving.
2. **Determinism:** new label families (species depth/position;
   FAMILY-level morpheme proto-forms — the first family-keyed draws since
   the proto-lexicon itself); zero facts/concepts; genesis byte-identical;
   census verified stable (law 7).
3. **The typology calls:** evidential position fixed predicate-final
   (near-universal) while class position is drawn (genuinely varies);
   depth weighted toward unmarked systems (most tongues will show
   nothing — the deep ones are the marked minority, as on Earth). Veto
   point if you want more of the roster marking.
4. **The coherence direction:** animacy DERIVES from the C5 schema draw
   (agentive cosmology ⟹ animate sky), never a second draw — the
   LANG-40 constraint executed literally. Confirm.
5. **Surface taste:** the emic world-statement line, the doctrine's
   taught line, and gloss phrasing.
