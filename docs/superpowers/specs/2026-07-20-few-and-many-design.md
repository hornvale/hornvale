# Few and Many — Design

**Date:** 2026-07-20
**Status:** Draft (G3 review pending).
**Campaign:** LANG-44, "numeracy as a quantity register" — a new mechanism
in `domains/language`, plus one demonstrated integration point in
`windows/book`. Answers the bridge The Echo's own G3 review explicitly
banked: "numeracy as a per-listener comprehension filter... the first
purely quantitative filter in the epistemic stack."

---

## 1. What this is

Every existing epistemic filter in this project is speaker-side: a
culture's account of ground truth passes through lexicon, knowledge,
ontology, and valence filters (The Chorus), or a priesthood's account
through disclosure transformations (The Doctrine) — but once a sentence is
spoken, every listener who hears it understands it identically. The Echo
proved a real surface-value law — a heard fact carries exactly what the
sentence said ("about 1.5"), never the full-precision ground truth beneath
it — but stopped there deliberately, flagging in its own G3 review that
"most creatures should not understand '1.5' at all."

This campaign is the first **listener-side** filter: a numeral-system
capability drawn per species that caps how much of a heard quantity a
listener actually retains, independent of how precisely the speaker said
it. Two listeners hearing "its day lasts about 1.5 standard days" now
retain genuinely different things — one keeps "about 1.5," another keeps
only "more than one." This is the same divergent-accounts theme The Chorus
established, now applied to a single told quantity rather than a whole
worldview.

## 2. What exists (verified against `main`@`87c8d746`)

- **`domains/language/src/clause.rs`'s `cardinal(n: u64) -> String`** —
  renders 0–12 as English words, else bare digits. The FullCounting rung's
  renderer, unchanged.
- **`clause.rs`'s `quantity(x: f64) -> String`** — `"about {x
  truncated-to-1dp}"`. The Decimals rung's renderer, unchanged — this is
  the ONLY decimal-rendering path in the codebase today, and it is
  uniform: every reader understands "about 1.5" equally.
- **`windows/book/src/lib.rs`'s `fact_for(fragment: &str) -> Option<(String,
  Value)>`** — the reverse of `fragment_for`; for the day-length case
  specifically, strips `"its day lasts about "`/`" standard days"` and
  parses the remaining text (e.g. `"1.5"`) back to `Value::Number(1.5)` —
  recovering exactly the SURFACE value the sentence stated, never the
  full-precision ground truth. Called from `parse_line`, with no
  listener-identity parameter anywhere in the chain today.
- **No structured quantity ever flows through `clause.rs`'s
  `ClauseSpec`/`parse_common` round-trip** — verified: `ClauseSpec.
  modifiers: Vec<String>` carries already-rendered opaque text (e.g. `"with
  two moons"`); a numeral is baked into that string by the caller before
  it ever reaches this layer. The genuine integration point for a heard
  quantity is `fact_for`, not `ClauseSpec`.
- **No generic "ladder" abstraction exists.** `packs.rs`'s `PackDepths {
  hue, luminance }` / `in_ladder` is a two-field struct hardcoded to
  color's own two Berlin & Kay ladders — a per-CONCEPT lexical-inclusion
  gate (does this word exist), not a rendering-style selector (how does a
  number get said). It does not fit numeracy's shape and this campaign does
  not extend or reuse it; the closer precedent is `MorphDepth`
  (`domains/language/src/morphology.rs`) — a small ordered enum, drawn per
  species, gating a rendering behavior.
- **No fraction or body-tally renderer exists anywhere.**

## 3. Architecture

### 3.1 `NumeracyRung`, three variants, not five

The registry's own idea names five rungs (Subitizing, BodyTally,
FullCounting, Fractions, Decimals). This campaign ships three: **Subitizing,
FullCounting, Decimals** — the ladder's two ends plus its near-universal
middle default. BodyTally and Fractions are the two rungs with the LOWEST
typological frequency (real numeral-system typology: pure subitizing-only
and simple-fraction systems are both real but comparatively rare next to
full counting) and, unlike FullCounting (`cardinal`) and Decimals
(`quantity`), have zero existing code to build from — all new authored
rendering logic for comparatively little additional proof of the ladder
mechanism. Captured as a follow-up (idea registry LANG-47), not built now.

**Framing, stated explicitly to avoid a subtly wrong claim:** subitizing —
rapid, exact perception of roughly one to four items — is itself a
near-universal *pre-linguistic* human (and animal) capacity, not a rung
some cultures biologically lack. `NumeracyRung::Subitizing` means "this
language's numeral system builds no counting words beyond that universal
floor" — a claim about linguistic typology (historically contingent:
trade, agriculture, and measurement culture drive numeral-system growth),
never a claim about a species' cognitive capability. This mirrors
`MorphDepth`'s own established posture exactly: grammaticalization depth is
drawn, never derived from a psychology/intelligence vector, and numeracy
rung follows the identical discipline.

### 3.2 One render function, two directions

```
render_quantity_at_rung(x: f64, rung: NumeracyRung) -> String
```

- `Subitizing`: exact "one"/"two" for values near 1 and 2, "few" for a
  small range above that, "many" beyond — thresholds are a plan-time
  tuning constant, not fixed here.
- `FullCounting`: rounds to the nearest integer, renders via the existing
  `cardinal`.
- `Decimals`: unchanged — delegates to the existing `quantity`.

The SAME function serves both directions. A speaker's own rendering (today
fixed at Decimals — unchanged behavior, see §3.3) and a listener's
comprehension (§3.4) are the same codec run at two different rungs, not
two independently-authored mechanisms — matching `evolve`'s own
single-function-reused-for-multiple-roles precedent from The Residue.

### 3.3 The speaker's rung stays fixed at Decimals in V1

Real comprehension precision, in general, is `min(speaker_rung,
listener_rung)` — a channel-capacity framing: the coarser side of a
conversation caps what transfers, regardless of which side is coarser.
Letting the SPEAKER's rung vary (a subitizing culture rendering its own
facts as "many" rather than "about 1.5") is real, and correctly a
LANG-47 follow-up, not this campaign's job — it requires a richer
recovered-value representation than a bare `f64` (a subitizing speaker's
own utterance, "many," has no single exact value to recover from in the
first place, unlike a Decimals speaker's "about 1.5").

Fixing the speaker at Decimals — the ladder's highest/most-precise rung by
construction — makes the general formula collapse exactly to this
campaign's actual mechanism: `min(Decimals, listener_rung) ==
listener_rung`, always. So "comprehension simply degrades to the
listener's own rung" is not a convenient shortcut; it is the correct
formula for the fixed-speaker-at-the-ceiling case this campaign scopes
itself to, and the general `min()` form is exactly what a later campaign
would need once speaker-rung variation ships.

### 3.4 The comprehension integration point

A new function demonstrates the mechanism against real, already-existing
fragment text — without modifying `fact_for` itself or wiring into the
live vessel session:

```
comprehend_quantity(fragment: &str, listener_rung: NumeracyRung) -> Option<String>
```

For a fragment `fact_for` already knows how to invert to a
`Value::Number` (today, exactly the day-length case: `"its day lasts about
{x} standard days"`), this recovers the surface value exactly as
`fact_for` already does, then re-renders it via
`render_quantity_at_rung(recovered, listener_rung)`. A `Subitizing`
listener given `"its day lasts about 1.5 standard days"` retains
`render_quantity_at_rung(1.5, Subitizing)` (e.g. "more than one"); a
`FullCounting` listener retains the rounded cardinal; a `Decimals` listener
retains exactly what a listener retains today, unchanged.

### 3.5 `numeracy_rung` — the per-species draw

```
numeracy_rung(seed: &Seed, species: &str) -> NumeracyRung
```

Mirrors `morph_depths`'s exact shape: one permanent stream
(`language/<species>/grammar/numeracy-rung`), a preregistered weighted
draw over the three variants, FullCounting weighted heaviest (the
near-universal typological default), Decimals and Subitizing both minority
cases (exact weights are a plan-time tuning decision). Independent of any
psychology/culture vector — the anti-astrology line, restated (§3.1).

## 4. The laws (standing tests)

1. **Purity:** `render_quantity_at_rung` and `numeracy_rung` are pure —
   same inputs, byte-identical output, every time.
2. **The ladder is real, not cosmetic:** the three rungs render the SAME
   input value as genuinely different text (e.g. `1.5` at Subitizing ≠ at
   FullCounting ≠ at Decimals) — a property test that found two rungs
   producing identical output for a non-trivial input would indicate the
   ladder collapsed into fewer effective rungs than claimed.
3. **The fixed-speaker special case holds:** `comprehend_quantity` applied
   to the SAME fragment at every listener rung produces exactly
   `render_quantity_at_rung(recovered_value, listener_rung)` for each —
   i.e., the comprehension path and the direct render function agree,
   confirming §3.3's collapsed-`min()` claim is what the code actually
   does, not merely what the spec claims.
4. **No behavior change for the existing Decimals-only path:** every
   existing `fact_for`/`quantity`/`cardinal` test continues to pass
   unmodified — this campaign adds a new, parallel comprehension function;
   it does not alter the existing recovery/render functions themselves.

## 5. Determinism and blast radius

**One new permanent stream:** `language/<species>/grammar/numeracy-rung`
— a pure addition, no existing stream's consumption order changes, no
existing world's genesis is affected (nothing wires this draw into any
composition-root path yet — see §6). Zero new randomness in
`render_quantity_at_rung` or `comprehend_quantity` themselves — both are
pure functions of their arguments. `fact_for`, `quantity`, and `cardinal`
are read, never modified. No Book, CLI, or vessel-session rendering
surface changes in V1 (§6) — proof is a property test, consistent with
"studies are data, metrics are code" (decision 0011) and directly
mirroring The Residue's own precedent for a mechanism-shipping campaign.

## 6. Non-goals

- **BodyTally and Fractions rungs** — captured as LANG-47, deliberately
  deferred (§3.1).
- **Speaker-side rung variation** — the general `min(speaker, listener)`
  formula, captured as LANG-47 alongside the two missing rungs (§3.3).
- **No wiring into the vessel's live `write`/`consult` verbs.** This
  campaign proves the mechanism against `fact_for`'s existing recovery
  path; making a possessed character's own species rung automatically
  gate what they retain when writing a heard sentence into their margin is
  a real, separately-decidable follow-up — the same "mechanism now,
  rendering surface later" discipline The Residue already established for
  this project.
- **No change to `packs.rs`'s color ladder** — confirmed structurally
  unsuited (§2), not touched.
- **Per-species granularity only** — the registry's own "ultimately
  per-creature" is explicitly a later refinement, not V1's job.

## 7. Registry and decisions

- LANG-44 flips `raw` → `shipped` at close, repointing **Where** at this
  campaign's chronicle; the row's own prose is checked at close for the
  same "capability, not typology" framing risk flagged in §3.1.
- LANG-47 (the two missing rungs + the general `min()` formula for
  bidirectional variation) added to the idea registry during this
  brainstorm.
- Full reasoning trail — including the matrix organon that cut scope from
  5 rungs to 3, and the substitution pass that derived the
  channel-capacity `min()` formula validating the fixed-speaker
  simplification — is in the campaign's decision ledger, summarized at G3.

## 8. Flagged for G3

1. **Save-format addition (leads, per the autopilot policy on
   determinism-contract calls):** one new permanent stream, a pure
   addition, no existing consumption order touched. Confirm.
2. **The 5→3 rung scope cut (ledger #1):** your call on whether shipping
   only Subitizing/FullCounting/Decimals — deferring BodyTally and
   Fractions — is the right size, or whether the full 5-rung ladder is
   worth building now.
3. **The fixed-speaker-at-Decimals scope cut (ledger #2):** confirm you're
   comfortable that only the LISTENER's rung varies in V1 (matching The
   Echo's own G3-banked framing precisely), with full bidirectional
   variation banked as LANG-47.
4. **No live wiring into the vessel session:** confirm the mechanism +
   property-test-only scope (no `write`/`consult` behavior change) is the
   right size for this campaign, matching The Residue's own precedent.
