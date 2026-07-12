# Phonology Epoch: Tone and Tonogenesis — design

**Date:** 2026-07-10
**Status:** Draft for review. Extends the lexicon-homophony campaign (Stages 1–2
shipped: the injective proto-root assignment, epoch `root/v2`). This spec covers
Stages 3–6, a **new phonology epoch** beyond `root/v2`. Confirm the open design
decisions in §10 before implementation begins.

## 1. Motivation

Stage 2 (the injective family-proto assignment) eliminated every *draw-side*
homophone — but the Stage-1 instrument then showed `homophony-merger-share`
reads **1.00** for every daughter: 100% of the residual is now the sound-change
cascade and `nativize` collapsing two *distinct* proto-roots onto one modern
form. bugbear still carries core homophony in 78.9% of worlds; goblin/hobgoblin/
kobold ~27–30%.

Two facts turn this from a patch into a model change:

1. **The residual and exotic-species expressiveness are the same problem.** The
   phoneme model is *segmental only* — 6 places × 8 manners (including clicks,
   ejectives, trills) × voicing, plus 5 canonical vowels, all gated per species
   by a 5-scalar articulation envelope. It has **no suprasegmental tier**: no
   tone, no phonation (growl/whine), no length, no nasalization. A phoneme
   inventory is *the alphabet of distinguishable signals a channel can carry*,
   and channel capacity is *dimensions × levels*. Homophony is insufficient
   capacity — worst exactly where dimensions are fewest. A future yuan-ti
   (serpentine: no labials, sibilant-heavy) or dragon (resonant, harsh) would
   hit this far harder than bugbear does.

2. **Tonogenesis unifies the merger-repair with the tonal-language feature, and
   it is diachronically real.** Tone in Chinese, Vietnamese, and many other
   languages *arose from lost segmental contrasts* — a voicing or coda
   distinction collapsed into pitch. That is precisely the residual here: when
   the cascade merges `foot`(\*Zhgoesh) and `moon`(\*Shgeosh) → *Goosh*, the two
   differed in the segment the merger destroyed. If the lost feature becomes a
   **tone**, the contrast survives as pitch — a *regular conditioned* change,
   not a lexical patch. This threads the constitutional needle that the two
   alternatives could not (see §2.2).

## 2. Design principles

### 2.1 Tone is a suprasegmental tier, orthogonal to the segmental inventory

Tone does not live inside a consonant's place/manner or a vowel's height. It is
a property of the **tone-bearing unit** — the syllable nucleus (its vowel).
Adding tone adds a dimension to the *nucleus*, leaving the consonant system and
the vowel-quality system untouched. Most species stay **atonal**: they carry a
single neutral tone with no contrast, and their rendered output is unchanged
except through the epoch reseed.

### 2.2 Tonogenesis is a REGULAR sound change, not a homophony patch

This is the load-bearing constraint. The Constitution forbids sporadic/lexical
rules (`evolve`'s founding invariant: *"no rule is ever sporadic/lexical; every
rule applies uniformly wherever its conditioning environment occurs"*). So
tonogenesis must **not** be "find the words that collided and fix them." It is a
sound rule of the form *"a syllable that lost feature X acquires tone Y,"*
applied to **every** syllable whose environment matches — whether or not that
syllable would have collided with anything.

Homophony reduction is then a *consequence*, not the trigger: two roots that the
segmental merger would have collapsed stay distinct **iff** they differed in the
tonogenetic conditioning feature. Where they lost the *same* feature, they still
merge — and that is correct (real tonogenesis does not resolve every homophone
either). This keeps `lexicon-regular` true by construction and preserves the
shared proto (cognates intact) — the properties Options A (a recorded lexical
dissimilation, which *is* sporadic) and B (a per-daughter proto re-assignment,
which breaks cognate descent) each sacrificed.

### 2.3 The floor is on channel CAPACITY, not segment count

A minimum-inventory-*count* floor would force a serpentine species to grow
un-serpentine consonants. The floor is instead on **distinguishable-syllable
capacity**, reachable via segments *or* tone: a few-place species meets the bar
with pitch and keeps its character. Capacity ≈ `|onsets| × |nuclei-fillings| ×
|codas| × |tones|`; the floor guarantees a minimum against the whole product.

### 2.4 Phonetic strategy is authored from body plan

Tone capacity is not a free scalar dialed at random — it is authored per species
from its articulatory anatomy (a serpent hisses at pitches; a dragon growls low;
humanoids are atonal). This is "models author, dice roll" (decision 0009): the
per-species tone propensity is an authored constant; the specific tone inventory
and assignments are seeded and deterministic.

### 2.5 Stay inside the espeak rendering ceiling

Every phonetic feature must render in all three views (roman, IPA, espeak). Tone
is IPA-native (tone letters ˥˦˧˨˩ / diacritics) and espeak-approximable. Phonation
(growl/whine) is IPA-native but espeak-weak, so it is **out of scope here** (§9)
and deferred to a later epoch. We add only what renders honestly.

## 3. The tone model

`domains/language/src/phoneme.rs`:

```rust
/// A syllable nucleus's pitch — the suprasegmental tier. `Neutral` is the
/// atonal default (no contrast); a tonal language draws from the level tones.
pub enum Tone { Neutral, High, Mid, Low }   // Contour tones deferred (§9)
```

The tone attaches to the **vowel** segment (the tone-bearing unit), so `evolve`'s
flat `Vec<Segment>` needs no structural change:

```rust
Segment::Vowel { height, backness, rounded, tone }   // tone: Tone
```

`Tone::Neutral` on every vowel reproduces today's atonal output. Consequences to
carry through (§8 checklist): `canonical_segments()` grows by a factor of the
tone count; `Segment`'s `total_cmp` ordering gains tone as the last key (stable
tie-break, determinism); `romanize`/`ipa`/`espeak_word` render the tone mark;
the phonotactic validator and `nativize` treat tone as carried-through (a vowel's
tone is preserved across every consonant rule, and folded to `Neutral` only if a
daughter's tone inventory lacks it — the vowel-quality nativization analog).

## 4. Tonogenesis (Stage 3)

A new `RuleKind` in the cascade family, drawn like the others but **only
effective in a tone-capable language** (a rule whose output is off the tone
inventory applies as identity — the same codomain constraint `evolve` already
enforces for segments):

```rust
RuleKind::Tonogenesis   // param selects the conditioning feature → tone map
```

The rule reads a *recorded* conditioning environment — the feature a *prior*
merging rule removed (the dropped onset's voicing for `ClusterSimplify`; the lost
coda's voicing for `FinalLoss`) — and writes the corresponding tone onto the
stranded nucleus. Because the conditioning feature is read from the derivation's
own step record (what was there before the merger), tonogenesis is a pure,
replayable function of `(proto, cascade, ph)` — `lexicon-regular` holds unchanged.

**Ordering contract:** tonogenesis must run *after* the feature-removing rule and
*before* the modern form is read, and it consumes no new stream draws beyond the
rule's own `param` (stream-consumption-order is a save-format contract). The
`Derivation.steps` record gains the tonogenetic step like any other
`AppliedRule`.

## 5. Tone capacity and the capacity floor (Stage 4)

`ArticulationVector` (`domains/species/src/lib.rs`) and the language `Envelope`
gain a scalar:

```rust
pub tonality: f64,   // 0.0 atonal (humanoid default) … 1.0 fully tonal
```

`draw_phonology` maps `tonality` to a **tone count** (1 = atonal → `Tone::Neutral`
only; 2–4 for tone-capable species) and draws which levels are in the inventory,
over a new seed-derivation leg (`…/phonology/tones`). Authored per shipped
species: goblinoid daughters and kobold stay `tonality = 0.0` (atonal — their
output changes only by the epoch reseed, *not* by gaining tone). The value earns
its keep when the bestiary grows (yuan-ti, birdfolk high; dragons via phonation
later).

**Capacity floor:** `draw_phonology` guarantees a minimum distinguishable-syllable
capacity. If the drawn segmental inventory falls below the floor, the shortfall is
made up first by admitting one more onset/coda template or vowel (segmental), and
— for a tone-capable species — by widening the tone inventory. An atonal species
that still falls short admits the extra segmental material (the narrow
minimum-inventory floor, as a subset of the capacity floor).

## 6. Rendering: tone across the three views (Stage 5)

- **roman**: a trailing tone diacritic or number on the vowel (e.g. `á` / `a1`);
  a fixed, deterministic convention, `Neutral` renders bare.
- **IPA**: tone letters (˥˦˧˨˩) or diacritics after the syllable.
- **espeak**: the espeak-ng tone hint where the voice supports it; where it does
  not, the segmental formulation stands and the tone is a known audio limit (§9).

All three remain **views, never stored** (the phoneme module's founding rule; the
audible-phonology spec). Committed name facts change because the rendered *string*
changes, not because a `Tone` is persisted.

## 7. The epoch and re-baseline

This is a **new phonology epoch** (a superset of `root/v2`): adding a tone draw to
`draw_phonology` and a tonogenesis rule to the cascade reseeds every phonology and
every derivation. Per epoch discipline (The Branches §6, decision on deliberate
regeneration):

- Every committed artifact regenerates in the same merge: three seed-42 almanacs,
  dictionary, proto-goblinoid page, phonology page, the two CI censuses,
  branches-family, the world.json + scene golden fixtures, the audio bank, the
  type-audit report.
- All calibration golden pins re-baseline in the drifting commit (name lengths,
  name-collision rate, homophony counts, the census name pins) — re-pinned to the
  honest measured values, never tuned to pass (ADR 0016).
- Cross-platform quantization is unaffected (tone is a discrete enum, not a float).

## 8. Determinism and layering checklist

- [ ] `Tone` enum + `Segment::Vowel.tone`; `canonical_segments()`, `total_cmp`,
      and the `"?"`-unreachable carry-forward all updated exhaustively.
- [ ] No `HashMap`/`HashSet`; `BTreeMap`/`Vec` only. No wall-clock.
- [ ] Tonogenesis is a *pure* replayable rule; `lexicon-regular-*` stays green.
- [ ] Stream-consumption order preserved: the tone draw and the tonogenesis
      `param` consume declared, stable stream legs (published in the manifest).
- [ ] Layering intact: tone lives in `domains/language`; `tonality` is authored in
      `domains/species`; no domain depends on another.
- [ ] Same seed + pins → byte-identical worlds, almanacs, artifacts (the CI drift
      check, extended to the audio bank).

## 9. Non-goals (this epoch)

- **Phonation** (breathy/creaky/harsh — dragon growls, whines): IPA-native but
  espeak-weak; deferred to its own later epoch.
- **Contour tones** (rising/falling), vowel length, nasalization, secondary
  articulation, a multi-manner exotic palette: not this epoch.
- Making the shipped goblinoid/kobold peoples tonal: they stay atonal by
  authoring; tone is for the future bestiary.

## 10. Open design decisions (confirm before Stage 3)

1. **Tone on the vowel `Segment` vs a parallel per-syllable tone track.** The spec
   proposes the vowel field (least disruptive to `evolve`'s flat sequence). Accept?
2. **Tonogenesis conditioning feature.** Proposed: the voicing of the segment the
   merging rule removed (voiced-loss → low, voiceless-loss → high — the historically
   attested direction). Confirm the feature and the tone-direction map.
3. **Atonal residual.** With the shipped peoples staying atonal, their merger
   residual is handled by the §5 capacity floor plus an accepted realistic tail.
   Confirm we accept a small atonal-core-homophony residual rather than forcing it
   to zero.
4. **Epoch scope.** Confirm the conscious phonology-epoch bump (bigger than
   `root/v2`) and the full artifact + audio re-baseline.

## 11. Success shape and the Lab battery

- `core-homophony-*` and `homophony-merger-share-*` (existing) re-measured: the
  tone-capable path drives the merger residual down for tonal species; the atonal
  path is bounded by the capacity floor.
- New metric `tone-count-<species>` (the drawn tone inventory size) — 1 for the
  shipped atonal peoples, exercised >1 by a tone-capable test species in the roster.
- New metric `distinguishable-capacity-<species>` ≥ the floor on every swept seed.
- `lexicon-regular-*`, `monophyly-goblinoid`, `inventory-closure-*` stay green
  (regularity and cognates preserved through the tone tier).
