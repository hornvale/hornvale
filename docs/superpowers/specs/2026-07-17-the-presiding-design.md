# The Presiding — a world has no religion; its peoples do

**Campaign:** The Presiding
**Row:** SKY-25 — presiding-belief selection is dominance-blind
**Date:** 2026-07-17
**Status:** spec, G3 ruled (2026-07-17) — census regen **authorized**; the
world belief is **retired**, not repaired.

## 1. What SKY-25 says, and what is actually true

SKY-25 says the presiding belief is chosen by species-registry alphabetical
order plus the founder-floor flagship guarantee, independent of dominance,
and frames it as the second of **two** gates on SKY-5's tide payoff: fix
habitability (SKY-24, shipped), then fix this, and locked worlds head the
tide again.

Measured on the merged tree (epoch v4 + ECS c3), that framing is wrong three
times over. The bug is real; its story and its promise are not.

### 1.1 The species is goblin, not bugbear — in three documents

SKY-25's row, `terminator_acceptance.rs`'s diagnosis, and
`a_frozen_sky_never_heads_a_cyclic_pantheon`'s comment all say the founder
floor guarantees **bugbear** a flagship on every seed, so bugbear commits
first. Bugbear places on **0 of 9** of the battery's own locked seeds
(8, 13, 70, 78, 80, 95, 116, 145, 183) and **0 of 30** in 1–30. The first
committer is **goblin**, everywhere. Registry-first is not placed-first — the
same inference The Named made, now found in three more documents.

### 1.2 The real mechanism is sharper than the row's

The founder floor places exactly **one** goblin, and that single soul's
pantheon speaks for the world:

```
seed | chief settlements (species:pop)  | first-minted belief is
   8 | goblin:1  hobgoblin:23 kobold:4  | goblin's
  13 | goblin:1  hobgoblin:19           | goblin's
  78 | goblin:1  hobgoblin:27           | goblin's
```

Across spinning seeds 1–24 the alphabetically-first species differs from the
most populous on **22 of 22** worlds, and goblin is population 1 on every
one. This is not an edge case; it is every world Hornvale generates.

### 1.3 Dominance-awareness would NOT restore the tide — measured

On all 9 locked seeds **hobgoblin's own pantheon also heads Eternal**. Making
selection dominance-aware swaps goblin → hobgoblin and the reading stays
`eternal`: **0/9 → 0/9**. Tide beliefs are still committed (locked seed 8
holds six) — they never head anything. The binding gate on SKY-5 is the
**ambient extinction** (`belief-kind` ambient 69 → 0 at the 2026-07-16
regen), already pinned honestly in `calibration.rs` and owned by rift-and-fit
ledger #14/#19. **This campaign does not promise the tide.**

### 1.4 The concept, not the rule, is the defect

`belief-kind`'s own doc reads *"The first belief's sentiment tag"* — honest.
Nothing in the code calls it presiding; the overreading is entirely in the
prose around it. There is **no in-world consumer**: the almanac renders
per-species pantheon blocks (The Named), and the REPL's `beliefs` seam reads
the flat list.

`belief-kind` is a **legacy singular** — minted when goblin was the only
people, so "the world's belief" was well-defined. The Branches made it four
peoples and the metric was never rethought. Its siblings already went
per-species (`cult-form-goblin`, `pantheon-size-kobold`).

So SKY-25 asks us to fix the *selection rule* for a concept whose *existence*
is the artifact. `beliefs_of(&world).first()` is a fact about a loop, not
about a world — the same shape as The Named's `i == 0`, one layer up, in the
metric instead of the renderer.

## 2. The design: dissolve, don't repair

**A world has no religion. Its peoples do.** SKY-25 is resolved by deleting
the question, not by answering it better.

### 2.1 Retire `belief-kind`

Delete the metric. No `presiding_belief` function is added: with the concept
retired there is nothing for it to compute, and a correct function nobody
calls is worse than none.

`beliefs_of` is **not** touched — it keeps returning ledger order, which is
honestly what it is.

### 2.2 Add `belief-kind-<species>`, for all four peoples

One metric per peopled kind — `belief-kind-goblin`, `belief-kind-hobgoblin`,
`belief-kind-kobold`, `belief-kind-bugbear` — each the sentiment of that
people's pantheon head, `Absent` when that people holds no pantheon
(bugbear will be Absent on essentially every seed; that is the honest
reading, and it is exactly the fact three documents got wrong).

Head = `beliefs_held_by(world, flagship).first()`: beliefs mint
salience-descending, so a people's first belief *is* its head.

Naming follows the religion convention (`<metric>-<species>`:
`pantheon-size-goblin`, `cult-form-goblin`). Note the existing per-species
metrics cover **only goblin and kobold** — themselves pre-Branches stale. This
campaign does not widen them (see §5), but it does not replicate the gap: all
four peoples get a `belief-kind-<species>`.

### 2.3 Strengthen the calibration instead of relocating it

`a_frozen_sky_never_heads_a_cyclic_pantheon` currently asserts over
`belief-kind` — i.e. over whichever people happened to sort first. Re-express
it **per species**: on a locked world, *no people's* pantheon head is Cyclic.

This is strictly better science. The invariant is physical (a frozen sky
offers no rising-and-setting body, so nothing can read cyclic), and physics
does not care which people sorted first. The old form tested the invariant on
one arbitrary people and called it the world.

Both pins — the locked split and `spinning_eternal_exceptions` — are
re-measured from the authorized regen (§4).

### 2.4 Move the two other consumers

- `windows/lab/tests/depth_ladder.rs` uses `belief-kind` as its exemplar of a
  `Full`-rung metric. Repoint to `belief-kind-goblin` (same rung, same
  rationale: it reads religion facts).
- `cli/src/main.rs`'s `lab list-metrics` test asserts the output contains
  `belief-kind`. It would still pass by substring against
  `belief-kind-goblin` — a false green. Assert the per-species name
  explicitly.

### 2.5 Correct the terminator battery, keep its measurement

`terminator_acceptance.rs`'s bugbear diagnosis is wrong; its **measured 0/9
pin is right** and stays. Reframe it from "the world's presiding belief" to
what it actually establishes and what §1.3 re-confirms: *no people heads
Ambient on any locked seed.* The falsification it honestly recorded is
untouched — only its explanation changes.

## 3. Blast radius

**No epoch.** Metrics are code (decision 0011); no seed derivation, stream
label, draw order, mint order, or ledger write moves. Worlds stay
byte-identical. Deriving-rather-than-reordering is what keeps it so:
reordering genesis minting would have been a save-format change, since entity
ids are sequential.

**Census schema changes: −1 column, +4.** The committed fixture stops matching
a live run. This does not redden `make gate` (`fixture_staleness` is
`#[ignore]`d to the heavy tier) but it **does** redden `make gate-full` and
CI's regenerate-and-diff until the regen lands.

## 4. The regen (authorized at G3, 2026-07-17)

Nathan authorized the AWS census regen. Sequence is load-bearing:

1. Land the metric change and the per-species calibration **with its pins
   left to be measured**.
2. Run the regen (`make regen-remote`, `scripts/aws-gate/regen-git.sh`) —
   never locally; `HV_CENSUS=1` is the AWS path's alone.
3. Re-pin the calibration from the regenerated census **in the same commit**
   that the regen produces (golden-pin discipline: re-pin in the drifting
   commit, never defer to the close).
4. `make census-check` before committing — the golden-pins.sql duplicate has
   missed every re-pin since it was created.

## 5. Out of scope

- **The ambient extinction** — the real gate on SKY-5; rift-and-fit #14/#19.
- **`pantheon-size` / `cult-form`** are *also* legacy singulars: both are
  documented as "the goblin flagship's" and duplicate `pantheon-size-goblin` /
  `cult-form-goblin` outright. Same disease; retiring them is more column
  churn on an authorized regen than the ruling asked for. Followup.
- **Widening the per-species metrics to hobgoblin/bugbear** (`pantheon-size-*`,
  `cult-form-*` stop at goblin+kobold). Followup.
- **Why goblin is population 1 on every seed.** The founder floor guarantees a
  flagship; nothing guarantees it grows. A demography question, not a religion
  one. Followup.

## 6. Testing

1. `belief-kind-<species>` reads each people's head; `Absent` without a
   pantheon. Bugbear is Absent on the measured seeds — pinned as the honest
   reading, not asserted as a floor (ADR 0016).
2. **Mutation-verified**: a metric that reads `beliefs_of().first()` instead
   of the species' own head must fail the battery. A test that passes under
   both has not tested the change (The Named shipped a vacuous invariant
   test; this is that lesson applied).
3. The per-species calibration fails if any locked world's *any* people heads
   Cyclic — verified by mutation against a forced Cyclic head.
4. `beliefs_of` order unchanged (byte-identity of the flat seam).
5. Determinism: same seed → same per-species readings across runs.

## 7. Definition of Done

- [ ] `make gate` green; regen run and calibration re-pinned in the drifting
      commit; `make census-check` clean; `make gate-full` green after the regen.
- [ ] Chronicle + retrospective; SKY-25 flipped to `shipped` (**dissolved, not
      repaired**), Where repointed.
- [ ] The three stale documents corrected (SKY-25 row, SKY-24 row's bugbear
      sentence, the calibration's bugbear + stale "37 tide-headed" prose).
- [ ] Confidence Gradient: no bet moves — no re-score (0030).
