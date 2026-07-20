# The Residue — Design

**Date:** 2026-07-20
**Status:** Draft (G3 review pending).
**Campaign:** LANG-43, "irregularity as cascade residue" — a new mechanism
in `domains/language`. Complements LANG-40 (fully shipped by The Deep
Grammar: the per-species `MorphDepth` grammaticalization-depth vector,
family-cognate morpheme forms, segment-level affixation).

---

## 1. What this is

Hornvale never authors an irregular-forms table. Instead, this campaign
authors **paradigm slots** — a plural formation, a past-tense formation:
proto-level affixes on a root, per grammatical category — and lets the
*existing*, already-shipped, exceptionless Neogrammarian cascade (`evolve`,
pure and total) run over each slot's proto-form independently. Because a
cascade's rules are phonologically conditioned (a stop lenites between
vowels; a cluster simplifies at a boundary; a final segment is lost), the
same root's bare form and its affixed form sit in *different* phonological
environments — so evolving them independently sometimes lands them in
different places than evolving the root once and mechanically gluing on a
separately-evolved affix would. That divergence **is** irregularity: real
languages get *foot/feet* from a conditioned vowel shift (umlaut), *was/were*
from a conditioned voicing alternation (Verner's Law), and whole strong-verb
classes from fossilized vowel-grade alternations (ablaut) — all residue of
regular sound change interacting with a fixed grammatical shape, not
lexically special-cased exceptions.

The authored counter-force is **analogical leveling**: some of that
divergence gets ironed back out over time, and — as in every real language —
it survives disproportionately in a small number of roots rather than
scattering evenly. This campaign derives *which* roots resist leveling from
each root's own phonological shape (§3.3), so where irregularity survives is
itself derived, never drawn.

## 2. What exists (verified against `main`@`7349c08d`)

- **`evolve(proto: &[Segment], cascade: &Cascade, ph: &Phonology) ->
  Derivation`** (`domains/language/src/etymology.rs:589`) — pure, total,
  already exercised on standalone morpheme proto-forms as well as whole
  roots (`morphology.rs`'s `evolve_axis`, line 244, evolves each
  evidential/noun-class marker independently). This campaign adds no new
  phonological engine code — it calls `evolve` on a *newly assembled*
  proto sequence, nothing more.
- **`affix(stem: &[Segment], affix: &[Segment], position: ClassPosition) ->
  MorphForm`** (`morphology.rs:310`) — joins two segment sequences at the
  segment level (never string concatenation) and renders the join in one
  pass. Used today only *after* both sides are already evolved (the
  "regular" order); this campaign calls the same function *before*
  evolution too (the "cascade-native" order), on the very same primitive.
- **`MorphDepth` (`None`/`Particle`/`Affix`) + `morph_depths(seed, species)
  -> (MorphDepth, MorphDepth, ClassPosition)`** (`morphology.rs:73,149`) —
  a per-species, per-category weighted draw gating whether a grammatical
  category grammaticalizes at all, and how. Two categories exist today
  (evidentiality, noun class), each with its own preregistered weight table
  and its own permanent stream family
  (`language/<species>/grammar/depth/<axis>`). This campaign extends the
  *mechanism* to two more categories (§3.1) with their own weight tables —
  LANG-40's epistemic-worldview framing for evidentiality/noun-class is not
  touched or reused; only the None/Particle/Affix draw shape is.
- **`morph_forms`/`evolve_axis`/`draw_morph_proto`** (`morphology.rs:198-
  260`) — the family-cognate pattern: one proto-form per (family, axis,
  value), drawn once and shared by every daughter, each daughter's surface
  form diverging only through its own cascade. Paradigm affixes follow the
  identical pattern (§3.2).
- **No existing paradigm/inflection concept.** Confirmed by search: no
  `Paradigm`, `Inflect`, `Plural`, or `Tense` type anywhere in
  `domains/language/src/`. This is genuinely new ground, not a rename of
  something that already exists.
- **No frequency/usage-salience field anywhere.** LANG-43's own registry
  row claims one "already in the packs" — false. `packs.rs`'s only numeric
  field, `ladder_rank: u8`, gates Berlin-&-Kay color-ladder acquisition
  order, unrelated to word frequency. §3.3 replaces this with a derived
  signal that needs no such field.
- **The cascade model is single-hop.** Every `evolve(` call site in the
  crate goes proto→one-daughter directly; none chain a daughter's own
  output through a further `evolve`. There is no family-tree genealogy
  engine. This campaign's leveling mechanism is scoped to match (§3.4).
- **`dictionary-generated.md` renders only citation forms** (word, IPA,
  proto, derivation) per concept — no inflected-form columns exist today to
  extend or risk drifting.

## 3. Architecture

### 3.1 Grammaticalization depth extends to Number and Tense

`morph_depths` gains two more per-species draws, `number_depth` and
`tense_depth`, each `None`/`Particle`/`Affix`, each with its own
preregistered weight table (typologically motivated — most attested
languages mark number and/or tense morphologically at least optionally, so
these weights need not mirror evidentiality/noun-class's; exact values are
a plan-time tuning decision, not a spec-time one). `TongueMorphology` gains
the two depth fields and two `class_position`-style attachment-side fields.
This is a mechanical extension of an already-shipped, stable pattern — new
permanent streams only (`language/<species>/grammar/depth/number`,
`.../tense`, plus their position streams), nothing existing changes shape.

**Only the `Affix` depth engages this campaign's mechanism.** `None` and
`Particle` behave exactly as they already do for evidentiality/noun-class
today (no marking, or a free word) — irregularity is fundamentally a
property of *bound*, phonologically-fused inflection, so a language that
never binds Number or Tense morphologically has nothing for §3.3 to act on.
This is a deliberate, real typological possibility (isolating languages),
not an oversight.

### 3.2 Paradigm slots are family-cognate, exactly like existing morphemes

For each category with `Affix` depth, each value (`"plural"`; `"past"`)
gets one proto-affix per **family**, drawn once and shared by every
daughter — the identical `draw_morph_proto` pattern evidentiality/noun-class
markers already use (new stream family:
`language/family/<family>/morph/number/plural`,
`language/family/<family>/morph/tense/past`). Nothing new is invented here;
this is the existing family-cognate law applied to two more axes.

### 3.3 The cascade-native cell, the regular cell, and their divergence

For a root with an `Affix`-depth category, two candidate modern forms are
computed **using only the two existing primitives from §2, sequenced
differently**:

- **Regular** (today's implicit order): evolve the root's own proto
  independently (already happens for every root), evolve the affix's own
  family proto independently (§3.2, mirroring `evolve_axis`), then `affix()`
  the two *already-evolved* forms together.
- **Cascade-native** (the new order): `affix()` the root's proto and the
  affix's proto together **first** — assembling one unevolved sequence —
  then run that whole joined sequence through the *same* `evolve` call
  (same cascade, same phonology) as one unit.

Because `evolve`'s rules are environment-conditioned, the joined proto
sequence sits in a phonological environment the separately-evolved pieces
never did (a root-final stop is now medial before the affix's vowel; a
cluster forms at the boundary that didn't exist before). Where a rule's
firing differs between the two paths, the two candidate forms diverge —
`cascade_native.modern != regular.modern` — and that divergence is the
irregular candidate. Where no rule cares about the boundary, the two paths
coincide and the root is regular, entirely as a byproduct of its own
phonological shape, never authored per-root. Every candidate form remains a
full `Derivation` (§2), so every irregular form still carries a printable
step-by-step derivation, the same standing law every root's citation form
already satisfies.

### 3.4 Analogical leveling: a derived, zero-draw resistance rank

Within one category (e.g., every root taking the Plural slot in one
daughter language), rank the divergent (candidate-irregular) roots by their
own proto-root segment *length*, shortest first. This is Zipf's law of
abbreviation (1935) made literal: shorter words correlate with higher
frequency of use across real languages, so length is a genuine, independently-
motivated proxy for the "resists leveling" signal real analogical leveling
keys on — not an invented heuristic, and not the false "already in the
packs" claim LANG-43's own registry row makes (§2).

A fixed, authored **leveling fraction** (a plan-time tuning constant, e.g.
the shortest quartile of divergent roots per category) decides survival: the
shortest-ranked divergent roots keep their `cascade_native` form (the
irregular survives); the rest regularize to their `regular` form (leveling
wins). This is a **deterministic rank threshold, not a weighted draw** — the
entire mechanism, start to finish, consumes zero new `Seed`/`Stream` draws
beyond the two already-existing draws it reuses (§3.1's depth draw, §3.2's
family-cognate proto draw). Purity and total-function status (§2's standing
law) extend cleanly to the new orchestration because nothing in §3.3 or
§3.4 is itself a draw.

## 4. The laws (standing tests)

1. **Purity, extended:** the whole paradigm-cell computation (regular
   form, cascade-native form, resistance rank, surviving form) is a pure
   function of (root proto, affix family-proto, cascade, phonology,
   leveling fraction) — same inputs, byte-identical output, every time.
2. **Non-degeneracy:** across a real seed's lexicon, divergence is neither
   universal nor empty — some roots in an Affix-depth category diverge,
   some don't. (A property test that found either extreme would indicate
   the cascade-native/regular computation collapsed into one path, or that
   no cascade rule ever conditions on the boundary — both bugs.)
3. **Leveling suppresses a strict subset:** the count of roots retaining
   their cascade-native form after leveling is strictly less than the raw
   divergence count (whenever the raw divergence count exceeds the leveling
   fraction's implied survivor count) — leveling must do something
   measurable, not merely relabel.
4. **The frequency prediction, preregistered:** shorter divergent roots are
   enriched among post-leveling survivors relative to the raw divergent
   population — measured directly (mean proto length of survivors vs. mean
   proto length of leveled-away roots), not narrated. This is the
   falsifiable claim behind §3.4's design; the test runs the real
   comparison before the campaign closes.
5. **Every irregular form carries a derivation:** no candidate form (regular
   or cascade-native) is ever a bare string — both remain full `Derivation`
   values through to whichever form survives.

## 5. Determinism and blast radius

**New permanent streams** (save-format-contract additions, never
renames): `language/<species>/grammar/depth/number`,
`.../depth/tense`, their attachment-position streams, and
`language/family/<family>/morph/number/plural`,
`.../morph/tense/past`. These are pure additions — no existing stream's
consumption order changes, so no existing world's genesis is affected;
only worlds that exercise the new `Affix`-depth branch produce new
morphology at all. Zero new randomness in the leveling mechanism itself
(§3.4). No change to `evolve`, `affix`, or any existing `MorphDepth`
category's weights or behavior. No Book or dictionary rendering surface in
V1 (§6) — the committed dictionary and Book artifacts are unaffected;
proof is a calibration study (§4), consistent with "studies are data,
metrics are code" (decision 0011).

## 6. Non-goals

- **No multi-generational leveling.** Leveling is single-shot per daughter
  language, matching the cascade's own one-hop architecture (§2). A
  family-tree genealogy engine that could let grammaticalization depth
  itself drift across generations (particle → clitic → affix, or the
  reverse) is real but out of scope — captured as LANG-46.
- **No Book, CLI, or dictionary rendering change.** This campaign ships the
  mechanism and its proof (a property/calibration study); surfacing
  inflected forms in `dictionary-generated.md` or the Book is a natural,
  separate follow-up once the mechanism exists to surface.
- **No change to evidentiality or noun-class's weights, forms, or
  behavior.** LANG-40 is untouched; only its `MorphDepth` *mechanism* is
  reused for two new, independently-weighted categories.
- **Only Number (Singular/Plural) and Tense (Present/Past) ship.** Two
  values each, matching the registry's own examples exactly — no
  additional values (dual number, future tense, aspect) and no additional
  categories.

## 7. Registry and decisions

- LANG-43 flips `raw` → `shipped` at close, repointing **Where** to this
  campaign's chronicle. The row's prose is corrected at close (not now) to
  describe the actual Zipf-length-derived leveling signal shipped, replacing
  the false "already in the packs" claim.
- LANG-46 (grammaticalization depth as a diachronic pathway, not a fixed
  snapshot) added to the idea registry during this brainstorm, `raw`/`low`
  — captured, not built.
- Full reasoning trail (including one ideonomy overturn on whether to reuse
  `MorphDepth`'s mechanism) is in the campaign's decision ledger,
  summarized at G3.

## 8. Flagged for G3

1. **Save-format additions (leads, per the autopilot policy on
   determinism-contract calls):** four new permanent stream families, all
   additions, none touching existing consumption order. Confirm.
2. **The registry correction itself:** LANG-43's own row asserted a
   frequency mechanism that doesn't exist. Confirm you're comfortable with
   the spec both naming this discrepancy plainly and replacing it with the
   Zipf-length derivation (§3.4) rather than, say, authoring a genuine new
   frequency field.
3. **The MorphDepth-reuse overturn (ledger #3):** extending LANG-40's
   mechanism to Number/Tense with fresh typological weights, rather than
   hard-coding universal affixation — your call on whether this reuse is
   welcome or whether you'd rather keep paradigms fully separate from
   LANG-40's machinery even at the cost of foreclosing isolating-typology
   daughters in V1.
4. **Scope:** Number + Tense only, single-shot per daughter, no rendering
   surface. Confirm this is the right size for one campaign, or whether it
   should shed further (e.g. Tense deferred to a follow-up) or grow (e.g. a
   minimal dictionary column).
