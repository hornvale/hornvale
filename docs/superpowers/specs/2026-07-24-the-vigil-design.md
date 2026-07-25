# The Vigil — dragon perception

**Campaign**: The Vigil (Dragons program, campaign 5)
**Date**: 2026-07-24
**Status**: spec, awaiting G3 review

## 1. Summary

The three chromatic dragons gain an authored `PerceptionVector` — the deferred
capacity that completes the mind → perceive → speak triad UNI-31 names. With
it, four things that have been true only in comments become true in the code:

1. Draconic gets its own content. A dragon's vocabulary is almost entirely
   perceptual, so until now "Draconic" has been *goblin perception wearing
   draconic phonology*.
2. The capability lattice's **chain** (`speech ⊆ perception ⊆ mind`) becomes
   true, and is then enforced at load — so every dragon added later is safe by
   construction.
3. The `NON_PERCEIVING_SPEAKER_BASELINE` stopgap is deleted in favour of a loud
   error, matching the sibling that already does this in `chorus`.
4. Species-registry fact emission stops being gated on sedentism and is gated
   per fact family on the component that produces it — completing decision
   0068's correction across all four families.

Additive: no epoch, no new predicate, no stream draw, no census. Worlds are
**not** byte-identical (see §7) — this is a deliberate, owner-ratified regen.

## 2. Context

The Dragons program has shipped: The Assay (potency = CR/30, 0064), The Eremite
(`SocialForm` + the nested-capacity lattice, 0065), The Solitary Tongue
(drift = f(sociality × lifespan), frozen Draconic, 0066), The Cloister
(`PsychVector` split into `MindVector` + `SocietyVector`, 0067) and its
corrective (society gates on sociality, not sedentism, 0068).

Perception was deferred at each step. Today `perception_registry()` carries
exactly the four settling peoples, and every all-minded pass that needs a
dragon's perception resolves a hardcoded goblin baseline in
`windows/worldgen/src/lib.rs`'s `exposure_of_impl`.

That stopgap is not neutral. It is printed, verbatim, into a published
artifact: every dragon section of `book/src/reference/dictionary-generated.md`
currently carries gap reasons reading *"hue rank 5 exceeds depth 4 from
night-vision 0.5"* — the goblin's number, asserted about a dragon.

### 2.1 The lattice is a fork in the code and a chain in the prose

Decision 0065 §2 enforces `perception ⊆ psyche` and `articulation ⊆ psyche`:
two independent branches under mind. `check_integrity`'s own comment says so —
*"speech ⊆ mind, perception ⊆ mind"*. The UNI-31 registry row describes the
same design as `speech ⊆ perception ⊆ mind`: a chain.

Those are different shapes, and the difference was unoccupied until The
Solitary Tongue gave dragons speech without perception — permitted by the fork,
forbidden by the chain. The stopgap is what let that ship without a panic.

This campaign makes the chain true and then enforces it (§4.4). The payoff is
roster growth, which Nathan has stated is coming (metallic dragons, further
chromatics, exotic types): with the chain enforced, a speaking kind authored
without a perception row fails loudly at load instead of silently speaking with
goblin eyes.

## 3. Design principle: clade eye, ecological schedule

`PerceptionVector`'s three dimensions sit at three different levels — an organ
(`night_vision`), a schedule (`activity`), and an attention allocation
(`sky_attention`) — and only one of them reaches language:

| dimension | consumers | reaches language? |
|---|---|---|
| `night_vision` | `pack_depths` (hue ladder + luminance switch), `perceptual_reason`, `perception_lens.night_sky`, `chorus::sky_capability` | **yes** — sole input to the hue ladder |
| `sky_attention` | `perception_lens` (`day_sky`, `night_sky`, `ambient`), `sky_capability`, `sky_first` | no |
| `activity` | `observation_time`, `sky_capability` | no |

So:

- **`night_vision` is a clade organ** — one named draconic constant, identical
  across every dragon. Phylogenetically conserved, and mechanically necessary:
  per-dragon night vision gives the dragons different hue inventories and guts
  the Draconic cognates section, which only admits concepts rooted in *all*
  daughters. A shared frozen tongue whose speakers cannot share color words is
  incoherent.
- **`activity` and `sky_attention` are ecology** — authored per dragon, and
  provably language-inert, so differentiation there cannot produce dialects.

This is the decoupling audit The Cloister asked for, landing clean:
differentiation is confined to the dimensions that cannot re-couple into the
language axis.

## 4. The design

### 4.1 The authored rows

`domains/species/src/lib.rs` gains a named constant and three rows:

```rust
/// The draconic clade's night-sky acuity. Authored once for the whole clade,
/// not per kind: `night_vision` is the only perception dimension that reaches
/// language (it alone drives `pack_depths`), so a per-dragon value would give
/// each dragon its own hue inventory and fragment the shared Draconic tongue
/// (spec: The Vigil §3). A future dragon inherits this by construction; a
/// deliberately divergent-eyed dragon must override it, which is exactly when
/// someone should have to decide whether the shared tongue still holds.
pub const DRACONIC_NIGHT_VISION: f64 = 0.9;
```

| kind | authored `insolation` optimum | `activity` | `night_vision` | `sky_attention` |
|---|---|---|---|---|
| `red-dragon` | 0.20 — "open volcanic terrain — high sun" | `Diurnal` | 0.9 | 0.25 |
| `black-dragon` | 0.10 — shaded swamp, lowland ambush | `Nocturnal` | 0.9 | 0.15 |
| `white-dragon` | 0.05 — polar, twilight-dominated light | `Crepuscular` | 0.9 | 0.30 |

**The activity rule** (stated so a future dragon authors itself): read the
kind's activity off its already-authored `ConditionNiche.insolation` optimum —
the light environment the kind was authored to live in. High sun → `Diurnal`;
shaded/low → `Nocturnal`; polar twilight → `Crepuscular`. This is existing
authored state constraining new authored state ("coarse constrains fine"), not
a fresh judgment call, and it is documented as the rule rather than computed —
perception rows stay authored constants (`PerceptionVector` doc), and deriving
one registry from another would couple them.

**`sky_attention` is low across the clade.** The dimension means *celestial vs
terrestrial* attention, not aerialness; `perception_lens.ambient = 1.5 −
sky_attention`, so a sky-rapt creature stops noticing the ground. A dragon on
the wing looks **down**, and this campaign's payoff (BIO-35 Stage 2, the
landscape of fear) is a ground-scanning predator. All three sit well under the
`sky_first` threshold of 0.6; the swamp ambusher ends up the most
ground-attentive kind in the roster (`ambient` 1.35). Flight is a *vantage*
fact, and the vector has no vantage dimension — encoding it as celestial
attention would put it in the wrong slot (captured as a registry row instead).

### 4.2 The stopgap deleted, not promoted

`NON_PERCEIVING_SPEAKER_BASELINE` in `exposure_of_impl` is **removed**. The
lookup fails loudly, matching the sibling that already does this for the same
component in `chorus::account_params_from`:

```
BuildError::MalformedKind("'{species}' carries no perception component
                          (not a peopled kind)")
```

`SocietyVector::baseline()` exists because a `Solitary` kind genuinely has no
society — a permanent hole a mixed consumer must resolve. After this campaign
no *speaker* lacks perception (§4.4 makes that an invariant, not a
coincidence), so a `PerceptionVector::baseline()` would cover a hole that no
longer exists. `exposure_of` is public and `resolve_kind` accepts any biosphere
kind, so the path stays reachable by a caller passing `owlbear` — precisely a
case that should error rather than silently answer as though a bear saw like a
goblin.

The same class of fix applies to `observe_with_sources`, whose
`.expect("peopled pass over a fauna kind")` means the REPL's `phenomena --as
owlbear` **panics today**. Verified, not inferred — run against a seed-42
world on this branch's base commit:

```
$ echo "phenomena --as owlbear" | hornvale repl --world /tmp/hv-vigil.json
hornvale repl — world of seed 42 ('help' for commands)
thread 'main' panicked at windows/worldgen/src/lib.rs:2895:10:
peopled pass over a fauna kind
```

It becomes the same loud `MalformedKind`. After this, `phenomena --as
red-dragon` works — the sky through a dragon's eyes, and the campaign's legible
payoff.

### 4.3 Fact emission re-gated by class

Owner decision (ledger #3). Today one gate wraps every species-registry fact:

```rust
if let Some(p) = wc.psyche.get(kind).filter(|_| settled) { /* 16 commits */ }
```

`settled` is `SocialForm::Settled` — sedentism, the exact shape decision 0068
corrected for `SocietyVector`. Each fact family is re-gated on the component
that produces it:

| fact family | predicates | gated on | dragons emit? |
|---|---|---|---|
| mind | `threat-response`, `deliberation-latency`, `time-horizon` | `psyche` present | **yes** |
| society | `in-group-radius`, `sociality-mode`, `status-basis` | `society` present | **no** — 0068's semantics, now visible in the ledger |
| perception | `activity-cycle`, `night-vision`, `sky-attention` | `perception` present | **yes** (this campaign) |
| articulation | `labiality`, `vowel-space`, `voicing`, `sibilance`, `voice-loudness`, `tonality`, `exotic-manner` | `articulation` present | **yes** |

**Emission order is preserved verbatim.** The existing sequence interleaves
society between mind facts (`threat-response`, `deliberation-latency`,
`in-group-radius`, `time-horizon`, `sociality-mode`, `status-basis`, then
perception, then articulation). The implementation changes each commit's *gate*
and nothing else — it does **not** regroup the commits by family. Consequence:
for the four peoples the emitted sequence is byte-identical, so the only new
facts in any world are the dragons' own.

Scope note: doing this for perception alone would leave mind, society, and
speech welded to `Settled` — reproducing 0068's defect one field over. This is
a scope expansion beyond the question Nathan was asked; it is flagged at G3.

### 4.4 The chain enforced

`check_integrity` (in `windows/worldgen/src/components.rs`) gains one rule:

```
for k in articulation.ids():
    if !perception.contains(k):
        MalformedKind("speaking kind {k:?} has no perception
                       (speech ⊆ perception ⊆ mind)")
```

Enforced lattice after this campaign: `articulation.ids == lexicon.ids`,
`articulation ⊆ perception ⊆ psyche ⊆ biosphere`, plus the existing `Settled ⟹
full peopled cluster` and 0068's `society ⟺ minded ∧ social`.

The direction matters and is deliberate. `perception ⊆ articulation` (today's
test assertion) would forbid a future non-speaking perceiver — an owl with
eyes and no words. `articulation ⊆ perception` forbids only the incoherent
case, a speaker with no senses, and is what makes §4.2's deletion permanently
safe rather than true-by-coincidence-of-roster.

## 5. Preregistered predictions

Frozen before any regeneration (per the preregister-on-named-axes discipline).
`hue = 2 + round((1 − night_vision) × 3)` → depth 2 at 0.9; luminance switches
to 3 above 0.6. Color-pack Berlin–Kay ranks: 1 `dark`/`light`, 2 `red`, 3
`green`/`yellow`, 4 `blue`, 5 `brown`.

**P1.** Each dragon section of `dictionary-generated.md` loses `green`,
`yellow`, and `blue` (they become `GapReason::Perceptual`); `brown` remains a
gap with re-worded reason text.

**P2.** Each dragon section gains `shadow` and `starlit` (luminance 1 → 3).

**P3.** Every dragon gap reason re-words from `night-vision 0.5` to
`night-vision 0.9`.

**P4.** The **Draconic cognates** section loses its `blue`/`green`/`yellow`
rows and gains `shadow`/`starlit`.

**P5.** Draconic's entire hue inventory becomes `dark`, `light`, `red` — the
three colors the three chromatic dragons are named in.

**P6.** The four peoples' dictionary sections are byte-identical.

**P7.** `dictionary-generated.md` is the only committed artifact under the CI
drift check that changes.

**P8.** The seed-42 world grows from **3514 facts to 3553** — three dragons ×
13 (mind 3 + perception 3 + articulation 7), and no others. Baseline measured
on this branch's base commit: `hornvale new --seed 42` reports *"world of seed
42 written to /tmp/hv-vigil.json (3514 facts; village: Qvooshtvoagootao)"*. The
village name is part of the prediction: it must not change, since no draw moves.

Honest caveat: kobold is already authored at 0.9, so hue depth 2 is not a new
mechanism — P5 is a new *instantiation* and a narrative payoff, not evidence
about the ladder. It is preregistered so it is a verified consequence rather
than a discovered coincidence.

## 6. Blast radius, enumerated by class

Spec-time consumer enumeration, complete by class (the discipline that worked
for The Cloister).

| class | sites | effect |
|---|---|---|
| Roster-size assertions | `windows/worldgen/tests/dissolve_equivalence.rs` (`perception.len() == 4`, "perception is the four peoples"; `perception ⊆ articulation`); `domains/species/src/lib.rs` unit tests | Re-pin to 7 by **named kinds**; invert the subset assertion to `articulation ⊆ perception` |
| Language derivation | `pack_depths`, `perceptual_reason` → `exposure_of_impl` → `lexicon_of` | The §5 predictions |
| Observation | `perception_lens`, `observation_time`, `observe_with_sources` | Dragons become observable-as; `expect` → loud error |
| Fact emission | `windows/worldgen/src/lib.rs` species-registry block | §4.3 |
| Integrity | `components.rs::check_integrity` | §4.4; new load-time failure mode |
| Religion | `chorus::sky_capability`, `sky_first`, `account_params_from` | **Unreached.** Dragons are never placed and still carry no `SocietyVector`, so the error moves one line down (perception → society). No behavior change |
| Vessel | `windows/vessel/src/agent.rs::mint_flagship` | **Unreached** — resolves a *settlement's* species. Its comment "Only peopled kinds carry a perception row" goes stale → fix |
| Census / Lab | `ALL_DAUGHTERS`, `hue_depth`, `studies/*.json`, census goldens | **No change.** Verified: zero `dragon` hits across `studies/` and every census golden; the only perception-derived metrics are `hue-depth-goblin` and `hue-depth-kobold`; no metric reads fact counts or species-registry predicates |
| Synthetic rosters | every `from_stores` caller: `roster.rs` (`goblin_derived`, `serpent_tonal_solo`, `awakened_owlbear`), `lib.rs` 6106/6313/6410/7928, `components.rs` tests, `beta_calibration_freeze.rs`, `branches_identity.rs` | **Verified safe.** Each either supplies perception alongside articulation, clones the canonical stores, passes both registries whole, or is fauna-only with every peopled store empty. None carries articulation without perception, so §4.4 breaks nothing |
| Artifacts | `book/src/reference/dictionary-generated.md` | P1–P4. Phonology and audio derive from articulation + society, not perception |
| Stale prose | `cli/src/repl.rs:302` ("the minded solitaries (dragons) never speak" — false since 0066) | Fix while adjacent |

## 7. Determinism and save format

- **No epoch.** No seed-derivation label changes, no stream consumption order
  changes, no hash/noise constants touched.
- **No new predicates.** All sixteen already exist and are registered; §4.3
  changes only which entities receive them.
- **No stream draws.** Fact commits consume no randomness, and `mint_entity`
  ordering is untouched, so every **drawn** value in every world is unchanged.
- **Worlds are not byte-identical** (owner-ratified, ledger #3). Three dragons
  × 13 facts (mind 3 + perception 3 + articulation 7) are appended at each
  dragon's position in the ascending-`KindId` entity loop, which shifts fact
  positions for every kind sorted after `black-dragon`. Position shifts are
  semantically inert — the Concordance's postings are derived views rebuilt
  from commit order, and every consumer reads by `(subject, predicate)`.
- **No census regeneration** (§6), so the census carve-out is not triggered.
- The four peoples' emitted facts are unchanged in value **and order** (§4.3).

## 8. Testing

1. **Registry** — the three rows exist with the authored values; all three
   share `DRACONIC_NIGHT_VISION`.
2. **Lattice** — `check_integrity` rejects a roster with articulation and no
   perception (anti-vacuity negative, not just a green path); the canonical
   roster passes; `perception.len() == 7` pinned by named kinds.
3. **Depths** — `pack_depths` at the draconic value yields `hue: 2, luminance:
   3`; asserted against the concept ids that enter and leave the ladder, not
   just the integers.
4. **Loud failure** — `exposure_of(world, "owlbear")` and
   `observed_phenomena_as(world, "owlbear")` return `MalformedKind`, not a
   panic and not a silent baseline answer.
5. **Observation** — `observed_phenomena_as(world, "red-dragon")` succeeds; the
   three dragons' salience orders differ from each other by exactly their
   `activity`/`sky_attention` distance (the `sky_first` flag is false for all
   three).
6. **Emission** — the four peoples' fact sequences are unchanged (value and
   order); each dragon carries mind + perception + articulation facts and
   **no** society facts.
7. **Predictions** — P1–P7 checked against the regenerated artifacts.

## 9. Non-goals

- **Widening `PerceptionVector`** (vantage, thermoception). Reserved to its own
  campaign by the type's own doc.
- **Placement.** BIO-35 Stage 2 / the `ANIMAL_PREY` prey field and the
  landscape-of-fear cascade is the next campaign, and it is census-regenerating.
- **Any mentorship / transmission mechanism.** The stemmatic model, divergence
  ∝ mentorship distance, the alignment register, and taught-beyond-perceived
  vocabulary are captured as registry rows; all of them need placed dragons.
- **Language-family topology.** Whether the goblinoid triad should collapse to
  one tongue with dialects is captured, not touched.
- **Re-opening 0066's drift rate.** Innate-vs-vertical-vs-mentorship is a
  *mechanism* question behind an already-shipped *rate*.
- **Giving non-dragon fauna perception.** The lattice change permits it; this
  campaign does not do it.

## 10. Decisions

Promoted from `.superpowers/sdd/decision-ledger.md`; a new `docs/decisions/`
entry (0069) is drafted at close covering §4.3 and §4.4 — the fact-emission
gate and the enforced chain — as the durable record.

## 11. Open question for G3

`DRACONIC_NIGHT_VISION = 0.9` was adopted without an owner pick (ledger #2).
Nathan was asked and answered with the shared-tongue argument, which supports
clade uniformity — satisfied here — but not any particular value. 0.75 (hue
depth 3, keeps green/yellow, no keystone) remains the live alternative.
