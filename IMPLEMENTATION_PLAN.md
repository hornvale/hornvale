# HANDOFF — The Tilth / The Tense

Written 2026-08-06. **Every claim below is either measured (command given) or
explicitly marked as inferred.**

## Situation

Branch `campaign/the-tilth`, **pushed** to `origin`, 57 commits ahead of main
(`f73059d5`), tree clean.

**The suite is GREEN: 2982 tests, 0 failures, 84 skipped.** `make gate` passes
(787 s, `cpu_ratio` 5.93). `make rebaseline` moves nothing but its own timing
ledger, which confirms the whole 49-failure sweep touched test code and fixtures
exclusively and never a generation path — the artifacts accepted in `655b63ca`
still stand byte-for-byte.

## What remains, and it needs Nathan

Two items, both blocked on the same thing: **this box cannot reach lefford.**

```
$ ssh -o BatchMode=yes lefford 'echo OK'
nathan@lefford: Permission denied (publickey,password).
```

1. **`make heavy-remote REF=1a1ca8b3c1e3376e2dd031dd9881c69ac38e7490`** — the
   branch is pushed, so the SHA is fetchable; the command just needs a session
   that can authenticate. (Run it as `! make heavy-remote REF=…` to put the
   output in the conversation.)
2. **The owed census.** Authorized by Nathan, unspent. `scripts/census-run.sh`
   hard-refuses anywhere but `lefford` (this box is `ambrose`) and there is no
   remote census target — `regen-remote` is abandoned, and `heavy-remote` runs
   the heavy tier, not a census. Decision 0063 is why the guard fails closed:
   the boxes disagree on ~0.1% of discrete-count metrics, and a wrong-host run
   commits wrong values that then drift-check green forever.

Then merge. Nothing else is outstanding.

## The campaign, in one paragraph

Capacity gained a **species** index (The Tilth) and then an **era** index (The
Tense), collapsing three mutually inconsistent oracles for the word "habitable"
into one: capacity. Decision 0106 records it — habitability is a relation
between a people, a cell and an era, not a property of ground. The measured
cost is that this **compresses the variance between worlds**: seed 1234 went
from permanently dead (0 survivors) to 36 communities across 70 sites, while
seed 42 fell from 209 settlements to 122 with chief populations down a third to
a half. A gate produces all-or-nothing worlds; a continuous squeeze produces
middling ones. Dead worlds live and rich worlds thin.

## What the 49 failures turned out to be

Roughly three quarters were the world legitimately moving. The quarter that
wasn't is the part worth keeping:

- **Two bake fixtures had gone VACUOUS, not wrong.** Both built their unusable
  ground from `EraClimate.habitable`, which the bake reads in zero places now.
  Green while proving nothing.
- **`the_lintel`'s walker was not moving.** `go n` twelve times, "No way n from
  here." every time — the mesh is triangular and the parity of the starting
  cell flipped. A search that had quietly stopped searching.
- **`the_water_column` looped until `look` MENTIONED water**, then dived from
  dry land and read the failure as a column. Its lateral-refusal check also
  hardcoded a direction the cell need not offer, so it could go green on a
  refusal it was not testing for. It now asks the verb.
- **The affect golden narrowed silently** — `Lost` and `Frustrated` vanished
  with the two gnolls the re-placement removed, and they were the only
  negative-valence affect in the fixture. A byte pin cannot tell "values moved"
  from "half the state space stopped being exercised". Now ratcheted.
- **`k_biomass_gradient` is not a gradient.** `raw_pole_mean = 0.004508` sits
  below the floor, so `ratio ≡ 100 × trop_mean` exactly. Re-labelled as a
  productivity tripwire, explicitly not evidence for the biomass-by-latitude
  claim — decision 0105 names `capacity-by-abs-latitude` as its circular cell.
- **Two preregistered predictions FALSIFIED and recorded, not rescued:** xorn no
  longer clears the dominance ruler (both the demesne and menagerie guards), and
  it is not erased — The Vacancy's viability guard is green.

## Three corrections to what the inherited handoff and I believed

1. **`k_biomass` was NOT pre-existing.** The handoff said it "fails identically
   at the branch point (verified by stashing)". Main is green on all 7 demesne
   tests, verified by running them in the main checkout. The stashing check was
   empty because the campaign's work was already committed.
2. **Kobold was never over the cap.** "~18% against a 10% cap" is false on this
   tree: 39 of 525 stack settlements, **7.4%**, and `branches_identity` was
   never failing.
3. **Exposure did not shrink.** I filed it that way; total gaps across the four
   seed-42 toponymic concepts went **17 → 15**, slightly MORE exposure on 42%
   fewer settlements. What actually happened is that kobold traded `hill` for
   `valley` exactly.

## The one thing left open on the physics

**Kobold's niche cannot be sensibly re-authored while elevation is an unfloored
axis under a flat `min()`.** Measured, both directions:

| setting | kobold dominance, seed 42 | verdict |
|---|---|---|
| `opt 3000, w 1100, dev 0.95` (shipped) | 39/525 = 7.4% | highland specialist |
| `opt 3000, w 2400, dev 0.95` | **492/525** | dominates the whole world |
| `opt 3000, w 2400, dev 0.50` | passes the cap | gaps **both** hill and valley |

The trap: kobold's curve looked right evaluated alone (lowland fit 0.023 →
0.435, peak unmoved). Dominance is **comparative** — widening one people's
unfloored axis while three stay narrow makes it best everywhere.

And the shipped niche *already* gapped `hill` before any edit, so this is
upstream of the authoring: era-varying capacity punishes high-elevation niches,
because elevation correlates with cold and the era minimum binds hardest there.
The fix is spec §3.3's two-tier gate/modifier split. `tolerance_tiered` exists
and is wired to nothing.

## Witness tests are now a rate problem

Several tests lost their witness this campaign, one of them for the fourth time
in three days (`id_shift_invariance`: 42 → 7 → 1 → 6). Each was re-found by its
own file's stated rule, and each now records the recurrence. The pattern is in
the retrospective as an open follow-up: `id_shift_invariance` in particular
wants **constructing** rather than a fifth hunt, and the header's rejection of a
synthetic *id shift* is not an argument against constructing two records with
genuinely equal material cores.

Also scarcer: the staple-coverage witness fell from seven qualifying (seed,
species) pairs in 0..150 to **three**, with none at all in 0..40.

## Commands

```bash
cd /Users/nathan/Projects/hornvale/.claude/worktrees/the-tilth   # ALWAYS explicit
make gate                                        # green, ~13 min
HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast
```

A stray `cd` resets the shell to the **main checkout**; it happened twice this
session and main was verified clean both times. Prefix every command.
