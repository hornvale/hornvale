# HANDOFF — The Tilth / The Tense, next session

Written 2026-08-05. **Every claim below is either measured (command given) or
explicitly marked as inferred.** That convention is inherited and worth keeping;
it is what caught the previous handoff's own wrong hypothesis about §1.

## Situation

Branch `campaign/the-tilth`, HEAD `cdf4b40b`, **36 commits ahead of main**
(`f73059d5`), tree clean, main fully absorbed. **The suite is RED: 50 failures /
2981** (was 52; §1 fixed two). Do not merge.

## 1. DONE — the two lib tests, fixed on the mechanism (`aa865df1`)

The previous handoff's hypothesis was **wrong**, and the mechanism is more
interesting than the guess. It supposed both were consequences of eviction
moving from `eff == 0.0` to pressure. Neither is; eviction is not on either
code path.

Measured: `history_bake.rs` reads `EraClimate.habitable` in **zero** places.
`Bake::factor` gates on ice alone and `era.ice` is identically empty. Both
fixtures built their unusable ground out of that mask, so the mask went inert
and each **silently stopped exercising its own rule** — the vassal's road led
somewhere (a successful flight, not a death), and the roller stopped in ring 1
instead of walking to ring 3.

Repaired by changing the fixtures' *language*, not their claims: dead ground is
now said in **capacity**, per-people, which is what `vacant_for` actually reads.
Every assertion untouched. Both pass.

> The trap that hid them, restated: `--test history_bake` is the *integration*
> binary. These are **lib** unit tests. Run `cargo nextest run -p
> hornvale-worldgen --lib`, or `make gate`.

## 2. DONE — campaign close (`cdf4b40b`)

- `docs/decisions/0105-habitability-is-a-relation-not-a-constant.md` (minted
  against `git log origin/main -- docs/decisions/`; 0104 is this branch's, so
  0105 was the first free number and no other worktree had passed 0103).
- `book/src/chronicle/the-tilth.md`, `book/src/chronicle/the-tense.md`, both in
  `SUMMARY.md`.
- `docs/retrospectives/the-tilth.md` — covers both campaigns, with the reason
  stated.
- Confidence Gradient re-scored **sideways** in `book/src/open-questions.md`.
- Verified: `mdbook build book`, and all 22 `cli/tests/docs_consistency` checks.

## 3. The triage of the remaining 50

Full log: rerun `HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast`.
The 52-failure baseline this table was built from took **720 s**.

### 3a. THE HEADLINE, measured — the dynamic range compressed

From `git show 655b63ca -- book/src/gallery/almanac-seed-42.md`:

```
  seed 42:  209 settlements -> 122
            bugbear seat 88 souls -> 67   goblin 82 -> 41   human 77 -> 36
  seed 1234 (settled finding, §5): 0 survivors -> 36 alive, 70 sites
```

A gate produces all-or-nothing worlds; a continuous squeeze produces middling
ones. **Dead worlds live and rich worlds thin.** Most of the table below is
downstream of this one fact.

*(An earlier draft of this section inferred "more, smaller settlements" from the
failure messages alone and was wrong in the direction of the count — the
artifact diff is what corrected it. Check the artifact, not the assertions.)*

### 3b. "The world legitimately moved — re-pin" (~34)

**One root cause, eight tests: seed 1's planet endonym `Pao` -> `Xoaboa`.**
`folk_sections_are_byte_unchanged`, `goblin_section_speaks_and_margins_seed_1`,
`initiate_edition_supersets_the_committed_artifact`,
`planet_sentence_aggregates_moons_star_and_day_length`, `the_additivity_law`,
`the_null_volume_is_untouched`, `the_esoteric_law_mutation_verified`,
`tongue_probes_derive_from_committed_is_a_facts`. Every species line is
byte-identical on both sides in each; only the planet name moves. The endonym
comes from the flagship culture's lexicon, which moved with placement.

**One root cause, five tests: the seed-42 bugbear seat was renamed
`Goodogododaga` -> `Dadogogodaga`** (confirmed in the almanac diff above; the
test's own message guessed "probably renamed"). `the_first_mark` ×3,
`possession_moves` ×2. Re-read the pin from
`book/src/gallery/possession-seed-42.md`.

**Ordinary seed-content drift.** `census_of_peoples_metrics_extract_for_seed_42`
(biome text), `seed_42_name_syllables_are_pinned` (2.561 -> 2.333),
`seed_42_name_transparency` (0.399 -> 0.530),
`build_world_generates_settlements_and_no_vale` (peak pop 118 -> 68),
`genesis_observes_an_unoccluded_sky` (231 -> 177),
`build_world_produces_the_full_cascade` (1 -> 2),
`the_material_fourth_key_barely_moves_the_stratigraphy`,
`affect_trace_golden`, `the_purview map_out`, `the_blocking`.

**`solitary_tongue peoples_lexicons_...` — VERIFIED legitimate, and this one was
worth the check.** Its own message distinguishes a determinism **bug** (a root
becoming a *different* root ⇒ `cascade_regime_of` byte-identity broke) from
benign exposure drift. Regenerated with `REBASELINE=1` and classified all 88
changed lines: **zero** are word→different-word. Every one is a gap↔word
transition or a gap-reason change. The phonology did not move. The regeneration
was reverted pending §4 (see 3d).

### 3c. "This is telling us something" (~14)

| what | evidence | read |
|---|---|---|
| **Kobold traded hill for valley** | `hill` gapped by 5 peoples -> all 6; `valley` gapped by 6 -> 5 | kobold, the authored *highland* specialist, now gaps hill and roots valley. **Directly entangled with Nathan's kobold call.** |
| **Toponymic exposure shrank** | `spring` gapped by 3 -> all 6; `marsh` by 0 -> 1; seed-7 goblins root `[river, ford]` not `[river, ford, hill, valley, marsh, spring]`; seed-0 gnolls lost `island` | fewer, smaller settlements touch fewer biomes |
| **Organization moved BOTH ways** | seed 2 hobgoblin lost it (3 tests agree: `the_taught_contrast`, `the_ladder_law`, `the_reckoning`); seed 4 kobold lost it; seed 57 bugbear *gained* it | consistent with a threshold crossed in both directions by a compressed distribution — not a one-way loss |
| **The sky stopped mattering to the flagship** | `locked_rotation_changes_the_flagship_cascade`: `assert_ne` now *equal* — spinning and tide-locked give the identical cascade | **the most suspicious single failure.** Era-averaging may have diluted insolation's influence. Worth a probe before accepting. |
| **Lab/worldgen staple lists diverged** | `the_independent_reading_covers_every_staple_worldgen_can_steep`: "does not steep rice ... the duplicate is stale again" | a real duplication-drift bug, independent of pins |
| **Belief one lost `celestial-body`** | `why_explains_belief_one`, `repl_answers_sky_village_and_belief` | content, or a broken belief chain — unverified, look before re-pinning |
| **Three vacuity guards fired** | `id_shift_invariance` (witness seed invalidated a **3rd** time in two days: 42→7→1→none); REPL settlement listing (**122 distinct of 122** — no repeats left to qualify); vessel `session` (walker never reached water) | the guards working as designed. See the retrospective's follow-up: this class wants *constructing*, not a 4th hunt. |

**Pre-existing, not ours:** `demesne::k_biomass_gradient_grounding_is_unaffected_by_the_vector_supply`
(verified at the branch point by the prior session).
**Unverified whether pre-existing:** `demesne::settlements_and_dominants_diversify_on_seed_42`
(xorn still not clearing the dominance ruler). Same file, same campaign lineage,
reads like an aspiration that may never have passed — **check it at the branch
point before treating it as a regression.**

### 3d. Why nothing was re-pinned

**Every re-pin in 3b is downstream of the three open calls in §4.** Each of them
moves settlement placement, and placement is what all ~34 pins record. Re-pinning
now means doing it twice. The one regeneration performed (the frozen lexicon
golden) was reverted for the same reason after its *diagnostic* value was taken.

## 4. Nathan's three calls — nothing else can land first

1. **Kobold.** ~18% of settlements against a 10% cap (`branches_identity`), and
   now measurably *off the highlands* it is authored for (3c, row 1). Lore says
   highland specialist; Nathan says "kobolds are like rats, pretty much
   anywhere". Both the cap and the niche authoring are on the table.
2. **`migration_fires_at_volume`.** Seed 42 gives 4 events against a floor of 5.
   Migration now scales with how much a world's climate actually moves; seed 42's
   deep past is mild, seed 1234's harsh (311 events). The floor was written when
   a binary mask displaced everyone on every era flip — i.e. it encodes the
   model this campaign removed.
3. **The owed census.** Still a carve-out needing explicit authorization, on
   lefford. `make rebaseline` skips censuses by design; census fixtures currently
   sit at main's tip.

## 5. Findings that are settled — do not re-derive

- **`era.ice` is identically empty** on every production path; "make factor
  ice-only" is a no-op.
- **`devotion` is the response curve's PEAK, not its breadth.**
- **Mixing floored and unfloored axes under `min()` is unstable by
  construction.** Stages 6 and 7 are the same bug on two axes; both landed,
  measured, reverted (`511d1fa9`).
- **Moisture must NOT become a gate** — takes human to 100% excluded.
- **Insolation is ~100% of the capacity pipeline's cost**, era-invariant, so it
  hoists; a 4× byte-identical memoisation is available and unspent.
- **Seed 1234 carries 36 alive, 70 sites, 16-deep columns.**
- **The bake HAS a recolonisation path** (daughter founding).
- **`CAPACITY_V_MAX = 140.2` is correct and was re-derived.**
- **`tolerance_tiered` exists and NOTHING CALLS IT.**

## 6. Process traps — now written up as rules

All eight are in `docs/retrospectives/the-tilth.md`. The two that cost the most:

- **Never project a cost from the components you happen to have listed** — show
  the components sum to the whole first. ("1.1×" measured 3.7×.)
- **Never compare a single seed when the constant re-rolls the world.**
  (`GENESIS_TOP_CELLS` gave 433/483/558/**281** for 8/16/32/64.)

And the one this session added: **check the artifact, not the assertions**, when
inferring which way the world moved (3a).

## 7. Commands

```bash
cd /Users/nathan/Projects/hornvale/.claude/worktrees/the-tilth   # ALWAYS explicit
make doctor
cargo nextest run -p hornvale-worldgen --lib
HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast   # ~720 s
make gate
```

A stray `cd` resets the shell to the **main checkout** — it happened this
session (caught immediately, main verified clean). Prefix every command.
