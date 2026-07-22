# The Solitary Tongue — language drift as a function of sociality × lifespan

**Campaign:** The Solitary Tongue — campaign 3 of the Dragons program
**Registry:** BIO-37 (the sociality × lifespan roster grid — the language half) ·
gives the three dragons speech (a Draconic `ArticulationVector` + lexicon) ·
makes `draw_cascade`'s drift-rate a function of `SocialForm` × `lifespan`
**Status:** G3 draft (awaiting Nathan)
**Date:** 2026-07-22

---

## 1. Problem

*The Eremite* gave the dragons a mind and left them mute — "the language of a
creature with no one to speak to is a later country." This is that country.

Two gaps meet here:

1. **Drift is uniform.** Every speaking kind draws the same 2–4 rule
   sound-change cascade (`draw_cascade`, `CASCADE_LEN_RANGE = (2, 4)`), so a
   language's distance from its ancestral proto is independent of *what speaks
   it*. A settled people that hands its tongue down every generation and a
   solitary ancient that has spoken alone for centuries drift by the same
   amount. That is wrong: drift happens at **transmission events**
   (generational hand-offs), and the two could not differ more in how many they
   have.
2. **The dragons cannot demonstrate it**, because they carry no `ArticulationVector`
   and no lexicon — they have a mind but no tongue.

This campaign closes both: it makes drift-rate a function of a creature's
**social organization and lifespan**, and it gives the three chromatic dragons a
Draconic tongue whose frozen, near-idiolectic character is the model's first
visible payoff. It is the language half of BIO-37 (the sociality × lifespan
grid) and the third campaign of the Dragons program (after 0064 potency, 0065
the solitary mind).

## 2. The model: drift ∝ transmission events ≈ sociality × (1 / lifespan)

A language changes when it is *passed on* — each transmission (a generation
learning from the last) is an opportunity for a sound change to enter and fix.
So the drift a lineage accumulates over a fixed span of world-time scales with
its **transmission count**, which scales with **community turnover**: many
short-lived speakers handing the tongue down often (a settled people) accrue
many transmissions; a single centuries-lived speaker (a solitary dragon) accrues
almost none.

This is exactly BIO-37's two axes:

- **Sociality** (`SocialForm`) sets the *breadth* of transmission — a Settled
  community transmits horizontally and vertically across many speakers; a
  Solitary creature transmits to no one.
- **Lifespan** (`allometry::lifespan(mass, class)`) sets the *rate* — a long
  life means few generational hand-offs per era.

A dragon is a community of one who lives for centuries → near-zero transmission
→ its tongue is **frozen**, its modern form barely distinguishable from the
ancestral proto (a conservative isolate, the linguistic reality of Icelandic or
a liturgical tongue, taken to its limit). A hypothetical short-lived solitary
would instead drift freely (no community to anchor a norm, many hand-offs) —
the model spans that too, though no such kind ships yet.

## 3. Design

### 3.1 The drift regime — `f(SocialForm, lifespan)`

Worldgen derives a **drift regime** (a cascade-length range) per speaking kind
from its biosphere (`social_form`, and `lifespan(mass, metabolic_class)`):

```
SocialForm    lifespan        cascade-length range   character
-----------   -------------   --------------------   ------------------------
Settled       any             (2, 4)  [UNCHANGED]    drifts; dialects diverge
Gregarious    any             (1, 2)                 moderate (banked; no
                                                     Gregarious kind speaks yet)
Solitary      long (≥ THRESH) (0, 1)                 frozen isolate (a dragon)
Solitary      short           (2, 4)                 drifts freely (banked)
```

`THRESH` is an authored lifespan cutoff (in `Years`) separating "long-lived
enough that transmissions are rare" from short-lived; dragons (D&D-canon
centuries, and heavy mass → long allometric lifespan) sit well above it. The
map is authored, not fit — a coarse regime table, refined only if a future kind
lands in an unexercised cell.

**The byte-identity keystone:** every kind that speaks *today* is `Settled`, and
`Settled → (2, 4)` — the current constant. So every existing speaker draws the
identical cascade it does now (§4).

### 3.2 Threading the regime, layering-clean

`domains/language` depends only on the kernel and **cannot read `SocialForm`**
(a species concern). So the regime is passed *in*, computed by the composition
root:

- Add `draw_cascade_with_regime(seed, species, regime: CascadeRegime)` to
  `domains/language`; `draw_cascade(seed, species)` becomes it with the default
  `Settled` regime `(2, 4)` — so every existing caller (chorus, lab,
  `branches_coverage`, the language tests) is **byte-identical** and untouched.
- `build_lexicon` (`lexicon.rs`, which calls `draw_cascade` internally) gains a
  `regime` parameter, threaded the same way; its worldgen entry points
  (`language_of` / `lexicon_of`) compute the regime from the kind's biosphere.
- `windows/worldgen` owns the map `(&BiosphereTraits) -> CascadeRegime` (reads
  `social_form` + `lifespan`) — the one place species and language meet.

`CascadeRegime` is a small language-owned type (a length range); no draw changes
for `Settled`, so no epoch for the peoples.

### 3.3 The dragons gain a tongue

- **Authored Draconic `ArticulationVector`** for `white/red/black-dragon` — one
  shared profile: harsh and **sibilant** (dragons hiss — high `sibilance`),
  **loud** (`voice_loudness` high), voiced, with an authored `ExoticManner`
  suited to a draconic hiss/growl if one fits; `tonality` left atonal for now.
  Values authored in §-detail at plan time, latent (dragons unplaced) but shaped
  for a distinctive Draconic phonology.
- **Stopgap lexicon entries** for the three dragons in `lexicon_registry`, so
  the `articulation.ids == lexicon.ids` invariant (from The Eremite's nested
  lattice) holds — the same minimal authored vocabulary shape the four peoples
  carry.
- The **`"draconic"` family already has a `family_proto`** (it is a 3-member
  family, so The Eremite's forward-proto check already requires and ships one —
  authored but until now unused). Once the dragons speak, that proto becomes the
  ancestral form the three chromatics evolve from — through the **frozen**
  regime, so their modern reflexes stay ≈ the proto and ≈ each other.

### 3.4 The payoff — an isolate beside a family

The three chromatic dragons share one proto and each draw a 0–1 rule cascade, so
Draconic comes out as a **conservative isolate**: the modern white/red/black
tongues are near-identical to their shared ancestor and to one another — low
inter-daughter divergence. Set beside the goblinoid family (2–4 rule cascades,
dialects that visibly diverge), the contrast *is* the model: the same machinery,
two regimes, because one people settles and drifts and one dragon broods alone
for a thousand years. This is measurable on the existing family/divergence
instruments (`branches-family` study; the homophony/divergence metrics).

## 4. Determinism & blast radius

**This campaign is ADDITIVE, not byte-identical, and NOT an epoch.**

- **The peoples are byte-identical.** Every existing speaker is `Settled → (2,4)`,
  so `draw_cascade`'s draws are unchanged; the authored articulation/lexicon of
  the four peoples is untouched; their per-world lexicons, phonology pages, and
  dictionary entries are identical. No existing labeled stream draw changes, so
  **no epoch**.
- **The dragons are additive.** Adding three keys to `articulation_registry` and
  `lexicon_registry` draws from *new* labeled streams (`language/<dragon>/…`),
  which cannot perturb the peoples' independent streams. The visible effect is
  **new Draconic sections** in the speaker-iterating artifacts.
- **Artifacts that GROW (re-pin, additively):** the dictionary reference page,
  the phonology reference page, and the `voice`/audio sample set gain Draconic;
  `render_cognates` gains a Draconic family section (three near-identical
  daughters). These are re-pinned by regeneration; the peoples' rows within them
  are unchanged (diff is pure addition).
- **The dragon tongue is otherwise latent:** dragons are unplaced, so no *world*
  (almanac, possession, census) gains a Draconic fact — only the
  speaker-catalog reference/gallery pages change.

**The consumer enumeration (the required de-risking — the lesson of The
Eremite).** Every articulation/lexicon/speaker consumer, and what it must do:

```
consumer                                   → dragons included?  effect
----------------------------------------   ------------------   -------------------
cli dictionary (render per speaker)          YES  (additive)    Draconic section added
cli phonology (render per speaker)           YES  (additive)    Draconic section added
cli audio / sample_names_for                 YES  (additive)    Draconic samples added
cli repl `word` (per speaker)                YES  (additive)    Draconic word shown
render_cognates / language_diverges_in       YES  (additive)    draconic family cognates
worldgen settlement roster / naming          NO   (Settled)     dragons don't settle
worldgen chorus / culture / religion         NO   (placed/Settled) dragons unplaced
lexicon_of / build_lexicon                   YES  (regime)      threads frozen regime
draw_cascade direct callers                  DEFAULT (2,4)      byte-identical (Settled)
```

The rule: **speaker-catalog** consumers (what a tongue *is*) additively include
the dragons; **settlement/placement** consumers (what a people *does* in a world)
stay `Settled`-gated. Plan Task 0 re-runs this grep on the merged base and
verifies the table before implementing — no consumer discovered at the gate.

## 5. Model-card delta

`draw_cascade`'s drift-rate moves from a global constant to a **derived,
authored regime** — a function of `social_form` and `lifespan`, both authored
inputs, computed at the composition root. The language chapter gains a note: a
tongue's distance from its proto is set by its speakers' transmission count
(sociality × lifespan), not a universal rate; Draconic is the frozen-isolate
limit.

## 6. Test plan

- **Regime map (new, worldgen unit):** `Settled → (2,4)`; a Solitary long-lived
  kind → the frozen range; the map is total over the roster. The Settled case is
  the byte-identity guard.
- **`draw_cascade` back-compat (new, language unit):** `draw_cascade(seed, sp)`
  equals `draw_cascade_with_regime(seed, sp, Settled)` for every seed — the
  default-regime byte-identity pin.
- **Frozen Draconic (new):** each dragon's evolved forms differ from the
  `"draconic"` proto in ≤ N segments (frozen), and the three chromatics'
  inter-daughter distance is below the goblinoid family's — the isolate-vs-family
  contrast, asserted on real derived lexicons.
- **Dragons speak (new, species/worldgen):** the three dragons carry an
  articulation + lexicon row; `check_integrity` still passes (speech ⊆ mind
  holds — dragons already have psyche); the peoples' languages are unchanged
  (a byte-identity assertion on a peoples' lexicon).
- **Additive-artifact verification (plan final task):** regenerate; confirm the
  dictionary/phonology/audio/cognate diffs are **pure additions** (Draconic
  sections) with every existing people's content byte-identical, and re-pin.
- **Existing suites + full `make gate`** green; no world/census golden changes.

## 7. Non-goals (deferred)

- **Dragon perception** — still deferred; a dragon speaks but does not yet
  perceive (the invariant allows speech ⊆ mind without perception).
- **Placing the menagerie** — dragons stay unplaced; Draconic changes only the
  speaker-catalog reference pages, no world. "The noticing" in the field is a
  later demography campaign.
- **Grammar / semantics of Draconic** — this ships phonology + drift (the sound
  of the tongue and its conservatism), not a distinct draconic grammar or
  meaning-space.
- **Per-chromatic differentiation** — the three chromatics share one Draconic
  articulation and proto; distinguishing red/white/black tongues is a later
  refinement.
- **The `SocioVector` / `SocialPsychVector` split** (Nathan-endorsed) — unrelated
  to language; stays a separate future campaign.
- **Deriving `SocialForm` from ecology** — still authored.

## 8. Proposed decision (ratify at close)

**00NN — language drift-rate is a function of sociality × lifespan.**
`draw_cascade` gains a regime parameter (a cascade-length range) supplied by the
composition root from a kind's `SocialForm` and `lifespan`; `Settled` keeps the
`(2, 4)` constant (byte-identical), a long-lived `Solitary` kind gets a frozen
`(0, 1)` regime. The three chromatic dragons gain an authored Draconic
`ArticulationVector` + lexicon and speak a frozen isolate off the shared
`"draconic"` proto. Additive to the speaker-catalog artifacts, byte-identical for
the peoples, not an epoch.
