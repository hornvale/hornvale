Continue Hornvale work. Two things are in flight; the second is the one to start.

## STATE

```
  origin/main                 f73059d5
  the-deep-realm              c60f9d89   8 ahead, 0 behind, PUSHED
  worktrees                   main + .claude/worktrees/the-deep-realm
  decisions                   next free number is 0104
```

Merged earlier today: **The Tolerance** (C2t — a species gained an interior)
and **The Scatter** (the heavy tier went 6134s -> 2773s). Both closed properly.

## 1. C2a THE DEEP REALM — paused by its own gate, NOT abandoned

Branch `the-deep-realm` holds a **G3-approved spec**, a **10-task plan**, and
an **11-entry decision ledger**. All of it stands and is correct. Only Task 0's
*premise* failed.

- spec: `docs/superpowers/specs/2026-08-05-the-deep-realm-design.md`
- plan: `docs/superpowers/plans/2026-08-05-the-deep-realm.md`
- ledger: `.superpowers/sdd/decision-ledger.md` (git-ignored; **dies with the
  worktree** — read it before any teardown)
- Task 0 test: `windows/worldgen/tests/deep_realm_substrate.rs` (committed)

Tasks 1-9 were never started. Do **not** restart C2a until item 2 below ships.

The design in one line: the underworld is **a graph of addressed chambers in a
fixed lattice**, content derived per address from the seed, nothing stored;
entrances and depth budget come from terrain's existing `cave_at`.

## 2. THE NEXT CAMPAIGN — fix the cave model (terrain). START HERE.

**Nathan's ruling (2026-08-05): fix caves first, as its own campaign.**

C2a's Task 0 measured, over 30 seeds / 469,122 land cells:

```
  land cells with a cave    0.26%  (1198)    3 of 30 worlds had ZERO
  depth_reach_bands = 2      100%            bands 1, 3, 4 never occur
  CaveKind = Karst           100%            LavaTube, Fracture never occur
  clustering                96.7%
```

**The cause is structural, verified in code, not inferred from the numbers:**

```rust
cave_proneness = carbonate * porosity * (0.85..1.0)   // lithology.rs:425
exists iff noise < proneness * (0.58..1.0)             // provider.rs:264
cave_kind:  carbonate > 0.5 -> Karst                   // features.rs:36
            silica    < 0.3 -> LavaTube    <- needs carbonate LOW
            else            -> Fracture    <- needs carbonate LOW
depth_reach_bands = 1 + (proneness * 3.0) as u32       // provider.rs:279
```

Existence needs carbonate **high**; both non-Karst branches need it **low**. So
a cave can only exist where the Karst test has already passed — two of three
branches are dead code, and have been since The Lode. Same cause compresses
depth: proneness is a product of two `[0,1]` ratios and never reaches the 2/3
band 3 requires, while low proneness means no cave at all.

**A gotcha for whoever recalibrates it:** `1 + (proneness * 3.0) as u32` cannot
return 4 without `proneness == 1.0` exactly. Even a well-calibrated proneness
will not produce band 4 through that expression — **the mapping needs changing,
not just its input.**

**Why now:** nothing consumes a cave, so fixing the model moves **no artifact
and no golden**. The moment C2a lands, it becomes a world-identity change. This
is the cheapest this fix will ever be, and that asymmetry is the whole reason
it is its own campaign.

Captured durably as `MAP-cave-model-miscalibrated` in the idea registry.

**Shape it as a campaign:** brainstorm -> spec -> plan -> execute, autopilot on
(G3 and G6 are hard stops). It should be small. It wants a preregistered
readout — the same four numbers above, re-measured — so the fix is proven to
have moved the distribution rather than asserted to have.

## PROCESS NOTES THAT COST TIME TODAY

- **Subagent-driven development with a real review per task.** Reviews caught,
  in one campaign: a vacuous test, a mutation proof with a hole, a miscount
  inside the lesson about miscounting, and a report that softened a
  *confirmation*. Do not skip them.
- **A mutation proves only what it perturbs.** Perturbing a derivation shows
  the function reads its argument, never that the pipeline passes the authored
  value. Ask: what would still pass if the wiring were wrong?
- **Sweep on the invariant, not the wording.** One defect family took three
  sweeps, each phrasing unreachable from the last.
- **A guard a comment can satisfy is not a guard.** Strip comments before
  matching; mutate by *deleting the real thing while leaving the prose*.
- **The heavy tier now exits non-zero BY DESIGN** while
  `disposition_calibration` is deliberately red (The Tolerance destroyed its
  premise and declined to retune a preregistered bound). **Read the failure
  list, not the exit code.**
- **`hostname -s` flaps** between `MacBookPro` and `Greyjoy`. Nathan's ruling:
  do not chase it. Consequence: `make ci`'s baseline forks, and the first run
  under whichever name is current records silently and cannot alarm.
- Registry Idea cells are capped at **600 chars** and the waiver list only
  shrinks. Count before writing — it bit twice today.

## WHAT I WOULD DO FIRST

1. `make doctor` in the main checkout.
2. Read `MAP-cave-model-miscalibrated` and C2a's spec §1 (what already exists —
   the seam is far better provisioned than it looks).
3. Brainstorm the cave-model campaign. The interesting question is not "raise
   the numbers" but **what should a cave's kind and depth actually derive
   from**, given that lithology is meant to differentiate them and currently
   cannot.
