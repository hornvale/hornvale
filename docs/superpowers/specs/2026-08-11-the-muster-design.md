# The Muster — design

**Campaign:** C2e, The Muster. **Base:** `main` @ `d8f9bb0d` (The Radiation
merged, decision 0120).

A muster is a roll-call: every individual named, counted, and distinguishable
from every other. This campaign does that twice — once for a quantity nothing
was watching, and once for a person whose identity could silently be someone
else's.

---

## §1 Why this campaign exists

The Radiation (C2d) derived the biome-affinity ladder's **level** — decision
0120, `factor = floor + (1 − floor) · preference` with
`floor = sovereignty_floor(mass, potency)` — after discovering that its previous
value, `0.25`, had never been derived at all. It reached the codebase from
illustrative test-fixture code in a plan document.

That campaign closed with two debts and one open question. The open question has
since been **measured and answered**; the two debts are this campaign.

### §1.1 The open question, and its answer

Registry row `BIO-affinity-level-is-two-quantities` asserted that the level does
two jobs — how a kind *ranks* cells, and how *productive* it is on them — and
should therefore be split in two.

A preregistered sweep (`docs/superpowers/specs/` has no home for a throwaway
spike; the freeze, both amendments and both result sets are promoted verbatim
into §6 of this spec) tested that claim against seven readouts with published
bands. **The claim is refuted, by its own instrument.**

- A **global** λ sweep returns SPLIT — but that verdict is an artifact. A uniform
  level collapses the contrast between a stronghold (always `1.00`) and the
  elsewhere level, and at λ = 1.0 a uniform level **is** the no-affinity arm.
  The opposition between consumers was manufactured by the parameterisation.
- A **per-kind scaled** arm (`level_k = λ · floor_k`, ordering preserved, and
  λ = 1.0 reproducing the shipped world **byte-identically** across five seeds)
  satisfies every band at λ ∈ {0.25, 0.50, 1.00, 1.20}, with the shipped
  configuration **interior** to that set.

**Verdict: LEAVE.** The level is one quantity; the consumers were never in
conflict; `sovereignty_floor` is now defensible on evidence rather than on
parsimony. The registry row is flipped to `rejected` with the evidence attached.

**So this campaign contains no redesign of the affinity level.** What it contains
is the guard that should have existed before anyone could have asked the
question.

### §1.2 Debt one — the level is guarded by nothing

`windows/worldgen/tests/beta_calibration_freeze.rs::beta_yields_realistic_coexistence`
is the commit-gate guard for the coexistence behaviour the affinity level drives.
It builds its component set with `ComponentStore::new()` for `biome_affinity` —
**zero rows**. It therefore *cannot redden* on any affinity-level change,
whatever that change is.

This is the campaign-arc's signature defect in its purest form: a guard that
reads as though it protects a quantity, one level away from the quantity it
protects. The affinity level has now completed two full campaigns — authored,
copied, made load-bearing, breached four fidelity floors, and been re-derived —
with no test phase anywhere in that cycle.

### §1.3 Debt two — a founder's identity can be someone else's

`domains/history/src/flesh.rs::founder_handle` mixes people, site, founded,
ended, peak population and a role tag, and **deliberately excludes the entity
id** so that a handle is reproducible from material facts alone. Two occupations
identical in all of those therefore collide **by construction**.

> **ERRATUM, 2026-08-11 — this paragraph cited the wrong guard, and the
> correction is scope-defining.** It named
> `windows/worldgen/tests/descent_graph.rs::founder_handles_are_free_of_the_entity_id`
> as the pin on `flesh::founder_handle`. That test does not touch
> `flesh::founder_handle` at all: it exercises `descent::founder_of`
> (`windows/worldgen/src/descent.rs`), which is a **second, different** founder
> handle — `RoleHandle(founding_key_from(own, parent) ^ seed.rotate_left(17))` —
> consumed by `windows/lab/src/metrics.rs`'s name-prefix census metrics (The
> Namesake). The actual guard on `flesh::founder_handle` is
> `domains/history/tests/flesh.rs`.
>
> **So the world carries TWO founder identities derived from TWO keys.** This
> campaign's epoch moves the **promotion** key — the one that decides a person's
> name in the ledger — and **not** the descent key. Any movement under
> `book/src/laboratory/` during Part B is therefore a **STOP**, not an expected
> regeneration: it would mean the descent key moved too, and the epoch's blast
> radius is larger than this spec scoped.
>
> Found by the plan-writer reading the tree rather than trusting the spec, which
> is the mitigation The Radiation's retrospective prescribed. Left in place
> rather than rewritten, because a spec that quietly corrects itself teaches the
> next reader nothing.

Measured over seeds 0–2999: **1904 handle-sharing record pairs**, five reaching
the promoted cast. `main` panics on seed 2793; The Radiation panicked on 283 and
705, both inside the census range, which is how it was found.

The Radiation shipped an **authorized fidelity cut** (Nathan, 2026-08-10): the
colliding duplicate is dropped rather than fatal, ~2 worlds per 1000 losing one
remembered founder. The real fix — widening the key — renames every founder in
every world and is therefore an **epoch**. That is this campaign
(`MEM-founder-handle-epoch`).

---

## §2 Non-goals

Stated explicitly, because each is a live temptation and each has been decided.

1. **Not splitting the affinity level.** Refuted (§1.1). Do not re-open without
   new information; the sweep and its decision rule are in §6.
2. **Not re-sweeping β.** Nathan's scope call. If a result implies β must move,
   that is a *finding*, not an action.
3. **Not repairing `BIO-40`'s diversity failure.** Per-cell diversity measures
   1.333 against a preregistered band of [1.5, 3.0] — but the sweep established
   that diversity moves **0.039 (2.7%)** across the *entire* λ range on both
   arms and never crosses the band at any λ. **The diversity debt is not an
   affinity-level problem**, and it stays where it is (The Waterline / `BIO-40`).
   This campaign makes the guard *able* to see; it does not make the band pass.
4. **Not re-authoring any preference row.** Shapes are untouched throughout.

---

## §3 Part A — the guard that could not see

### §3.1 The repair

`beta_yields_realistic_coexistence` must build its component set from the **live**
registry, so that an affinity-level change reaches it.

### §3.2 The complication, and it is the substance of Part A

A live component store is necessary and **not sufficient**, because the band's
verdict is decided by *which roster is counted*, not by the physics:

| roster counted | measured diversity | verdict against [1.5, 3.0] |
|---|---|---|
| the 39-row biosphere | 1.42 – 1.46 | fails at **every** λ, both arms |
| the 18 peopled kinds | 2.19 – 3.00 | passes at **every** λ, both arms |

Same worlds, same β, opposite conclusions. `BIO-40`'s published `1.333` does not
say which instrument produced it.

So the repair owes a **named roster instrument**: the guard must state, in its
own assertion, which population it counts and why that is the population the band
was written about. A guard that can fire but cannot say what it counted has moved
the defect rather than fixed it.

### §3.3 Prediction A1 — the repaired guard discriminates

**Prediction:** with the live component store wired, a mutation to the affinity
level moves the guard's measured diversity.

**Falsifier:** it does not — meaning the guard is blind for a *second* reason
beyond the empty store, and that reason is the finding.

**This is a positive control and it is mandatory.** A guard that is repaired and
still green proves nothing; the campaign has to show it can go red. Name the
property the mutation must demonstrate — the implementer finds the mutation.

### §3.4 Prediction A2 — the roster choice is the verdict

**Prediction:** holding worlds and β fixed, the guard's pass/fail flips on the
roster counted alone, reproducing the table in §3.2.

**Falsifier:** it does not flip — the two rosters agree, `BIO-40`'s band is
roster-independent, and §3.2's table is wrong. Report it; it would be a finding
about the sweep, not about the guard.

---

## §4 Part B — the founder-handle epoch (`/v2`)

> **ERRATUM, 2026-08-12 — this section was not built by this campaign.
> `campaign/the-ell` shipped it.** Part B was held here on the reasoning that
> The Ell retypes `Fact.day` (decision 0126, superseding 0014) and moves the
> history bake from years to days — the units of the very fields
> `founder_handle` keys on — so landing an identity epoch first would have
> renamed every founder in every world and let The Ell rename them all again.
> The Ell then did the widening itself rather than handing the tree back,
> riding an epoch it was already paying for.
>
> **What it shipped, against what this section specifies.** The constraint §4.3
> made non-negotiable is honoured: **the drop backstop stays.** The Ell's own
> plan called for restoring the fatal assert, and building the residual seeds
> showed what that would cost, so the drop is retained as the honest handling of
> a residual. §4.4's prediction B1 is confirmed over the census range — **0
> colliding worlds and 0 dropped founders over seeds 0–999**, against 2 and 2
> before — and the residual over 0–2999 is exactly {2634, 2898}, the twin-parent
> case §4.1's own table predicted no one-hop widening would remove.
>
> **What it corrected in this section.** §4.1's scoring is the diagnosis this
> spec inherited, and The Ell measured the narrower founding-side-only key it
> implies: that key collides in **732 of 1000** worlds and costs **1582**
> dropped founders. Decision 0127 records the resolution — an *identity* key and
> a *discrimination* key are different kinds of object, and `founder_handle` is
> the second wearing the first's name. §4.5's rename-only property (B2) and the
> `/v2` naming discipline of §4.6 are subsumed by that decision's own
> byte-neutrality evidence.
>
> **Consequently §7's "one census refresh at the close" does not apply to what
> this campaign shipped.** The census was owed by the epoch, and the epoch
> landed elsewhere with its own regeneration. Part A's only production-crate
> change is doc-only; no world moved. See the chronicle's closing section for
> the evidence.
>
> Left in place rather than rewritten, for the same reason §1.3's erratum is: a
> spec that quietly corrects itself teaches the next reader nothing.

### §4.1 What changes

`founder_handle` widens to fold its referents by their **material** keys. The
Radiation's diagnosis scored the candidates over 3000 seeds:

| key | handle-sharing pairs | pairs reaching the cast | seeds still failing |
|---|---|---|---|
| baseline (shipped) | 1834 | 5 | 283, 705, 2403, 2634, 2898 |
| + parent `FoundingCoords` | 398 | 2 | 2634, 2898 |
| + parent material key | 153 | 1 | 2898 |
| + ender material key | 27 | 0 | — |
| + parent **and** ender | 2 | 0 | — |

**The obvious one-hop widening is insufficient** — at 2634 and 2898 the parents
are themselves twins, so the lineage hop ties. `ended_by` discriminates far
better than `founded_from`.

### §4.2 The correction the docstring already carries

`flesh.rs` previously justified excluding `ended_by`/`founded_from` as
"`EntityId`-valued fields decision 0051 already forbids keying on". The Radiation
established that this is a **category error** — 0051 forbids keying on an id *as
a value*, while the repo already folds referents by their material facts in
`FoundingCoords`, `founding_key_from` and `layer_key`. That correction is in the
tree, dated; this campaign acts on it.

### §4.3 No key is total, so the backstop stays

Even `+ parent and ender` leaves **2 residual pairs in 3000 worlds**. A world
generator must not panic on a legal seed, so The Radiation's deterministic drop
**remains** as the backstop beneath the widened key. The epoch reduces the
fidelity cut; it does not remove the need for one.

**This is a constraint on the design, not a caveat on it:** any proposal that
removes the drop because "the key is now unique" is rejected in advance by this
table.

### §4.4 Prediction B1 — the widened key clears the observed collisions

**Prediction:** over seeds 0–2999, zero pairs reach the promoted cast, and the
five failing seeds fall to none.

**Falsifier:** any seed still drops a founder — the widening is insufficient and
the residual is the finding. Report the rate; do not widen further to chase it
without saying so.

### §4.5 Prediction B2 — the epoch is a rename and nothing else

**Prediction:** the widened key changes **every** founder handle, and changes
**nothing else** — placement, settlement counts, population and the history bake
are byte-identical before and after, on a named seed panel.

**Falsifier:** a world's physics moves. That would mean `founder_handle` reaches
something upstream of promotion, and the epoch is not a rename. **This is the
prediction that decides whether the campaign is safe**, and it needs the
positive-control arm that would have shown movement if there were any.

### §4.6 Epoch discipline

Per the Constitution, deliberate regeneration uses an **epoch suffix**, never a
rename. Every committed artifact carrying a founder identity regenerates in the
commit that changes the key. The Signet (`docs/retrospectives/the-signet.md`) is
the precedent for what a renumbering costs and the discipline it demands.

---

## §5 Part C — record the level's three consumers

The LEAVE verdict's deliverable is documentation and evidence, not code.

`domains/species/src/lib.rs::biome_affinity_registry` currently documents the
derivation. It does not say that the level is read by **three** consumers with
different sensitivities:

1. `per_species_capacity` — multiplies headcount, hence population, hence the
   history bake's volume.
2. `coexist::pack` — each kind's share of a cell is `K^β` normalized **across**
   kinds, so a per-kind rescale moves every share.
3. `coexist::pack` — the cell's total capacity is a plain **sum** across kinds.

Only *within-kind ranking* is level-invariant, which is why "the level is gauge"
was believable for two campaigns: it is true of exactly one consumer and was
applied to all of them.

Record this, with the sweep's evidence, next to the derivation.

---

## §6 The preregistered sweep, promoted

The freeze, Amendment 1, and both result sets live in throwaway scratch and must
be promoted into the chronicle before that scratch dies. The load-bearing items:

- **The decision rule**, frozen before measurement: *is there a single λ interval
  satisfying every band simultaneously?* YES → one quantity (in-interval →
  LEAVE, out → RE-DERIVE); NO → the consumers want different values → SPLIT.
- **The rule was answered on the wrong arm first.** Sweep 1 varied λ *globally*,
  substituting along the **kind** axis, while the hypothesis was a substitution
  along the **consumer** axis. Its SPLIT verdict was an artifact.
- **The control that made this legible, and it generalises:** for any one-scalar
  sweep over a per-kind quantity, **does the shipped configuration reproduce
  byte-identically somewhere on the grid?** Sweep 1 could not express that check;
  sweep 2 passed it three ways. If a sweep cannot reproduce the shipped world, it
  is not interpolating it.
- **Diversity is not an opposed party** — refuting the failure mode the
  preregistration named in advance.
- **Known fragility, carried forward:** the majority-of-seeds reading condemns
  the shipped configuration via **sea-elf**, which places 2–3 settlements a seed
  and ties its baseline exactly on 3 of 5 seeds. Widen the panel before spending
  any decision on a kind that thin.

---

## §7 Definition of Done

- Part A's guard is repaired, **names its roster**, and is mutation-proven to
  redden on an affinity-level change.
- Part B's key is widened, the drop backstop is retained, and B2's rename-only
  property is measured with a positive control.
- Part C's three consumers are documented beside the derivation.
- Registry: `BIO-affinity-level-is-two-quantities` → `rejected` (done in this
  campaign's first commit); `MEM-founder-handle-epoch` → `shipped`; `BIO-40`
  repointed to say the diversity debt is **not** affinity-level.
- Chronicle, retrospective (decision 0020), book freshness sweep (0013),
  Confidence Gradient re-score if a bet moved (0030).
- One census refresh at the close, on lefford, authorization-gated.
- The sweep's freeze and both results promoted out of scratch (§6).

## §8 Risks

- **The epoch's blast radius is the campaign's main cost.** Every committed
  artifact carrying a founder identity moves. Sequence Part A before Part B so
  the guard work is measured against a stable identity space.
- **A census refresh is ~15 minutes and the re-pin sweep is the expensive half**
  — The Radiation's took a full agent and cleared 47 reds. Budget it.
- **`make rebaseline` exits 2 while a schema mismatch is outstanding**, and
  `set -e` kills five downstream steps. If a metric is added, expect it.
