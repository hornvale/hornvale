# The Reagent — Design

**Date:** 2026-07-26
**Status:** Shipped 2026-07-26. **Amended at close (§2a, §8a): the shipped
confound reaches appearance but not behaviour** — owner decision at G6.
**Parent spec:** `2026-07-26-the-crucible-metaplan-design.md` (§7.1)
**Worktree:** `the-crucible` (branch `the-crucible`), off `main` at `bd7314a0`
**Autopilot:** engaged (G3/G6 hard stops; ledger at
`.superpowers/sdd/decision-ledger.md`)
**Registry:** `ALCH-1`…`ALCH-3` (this campaign); `ALCH-4`…`ALCH-6` reserved

---

## 1. What this is

Campaign 1 of The Crucible: **the material ground truth, and nothing that
believes anything about it.** A substance is a bundle of latent qualities; a
production transforms bundles under a process; conservation says which
productions are well-formed. No practitioner, no doctrine, no observation.

The campaign's value is that it fixes the contract every later campaign
inherits, and it is deliberately the smallest thing that can do so.

## 2. Amendment to the metaplan (§9 risk 1)

The metaplan's mitigation for the taste-gated inventory said every quality must
be "observable in principle by a practitioner." **That is backwards and is
corrected here.**

If qualities were directly observable there would be no provenance confound at
all — a practitioner would simply read a substance's composition off it, and
the entire program's subject matter would vanish. The correct constraint:

> **Qualities are latent. Every quality must be *inferable* — it must affect at
> least one observable — but none may be directly readable.** What a
> practitioner perceives are **signs** (weak, partly misleading functions of
> the qualities) and **reaction outcomes**.

This is the campaign's most important structural commitment, and it is what
makes the doctrine of signatures *wrong in a mechanized way* rather than
wrong by authorial fiat.

## 2a. Amendment at close — the confound reaches appearance, not behaviour

Measured on the shipped code, not inferred. The causal chain this campaign
actually delivers is:

```
  ore grade  ->  causticity  ->  hue        (appearance)
  ore grade  ->  causticity  ->  nothing    (behaviour)
```

**No production in `PRODUCTIONS` requires `Causticity`.** Its only consumer
anywhere in the tree is `hue`. Sweeping all nine commodities across grade
0.00→1.00 in 0.01 steps leaves the admitted-production set invariant for every
commodity. So the drawn quantity that was supposed to be the confound's physical
mechanism **cannot make a recipe fail**.

What shipped is coherent and still worth having: poor ore *looks* different and
*behaves* identically, so a practitioner sorting reagents by colour learns
nothing. That is a real doctrine-of-signatures error, mechanized. But it is not
§2's "a recipe works in one valley and fails in the next" — that claim describes
a behavioural coupling this campaign does not build.

**Owner decision at G6 (2026-07-26):** ship the appearance half; do not touch
the authored inventory now. **Coupling grade to behaviour — a causticity-gated
production, so provenance determines whether a recipe works — is campaign 2's
first task**, ahead of any practitioner work, because The Signature's whole
subject is a doctrine that fails on provenance and there is nothing yet for such
a doctrine to be wrong about.

## 3. The three inventories (authored, and small)

### 3.1 Latent qualities — the `QualityVector`

Eight axes, each `f64` in [0,1], following the project's existing
`MindVector` / `PerceptionVector` / `SocietyVector` naming convention:

| Quality | Meaning |
| --- | --- |
| `fixity` | survives fire unchanged |
| `volatility` | passes into air on heating |
| `combustibility` | feeds fire |
| `solubility` | yields to water |
| `malleability` | deforms without breaking |
| `density` | mass per bulk |
| `causticity` | attacks other matter |
| `vitality` | of living or once-living origin |

`fixity` and `volatility` are deliberately *not* forced complements — a
substance may be both low-fixity and low-volatility (it decomposes rather than
survives or flies off), and that region is where the interesting productions
live.

### 3.2 Manifest signs — what a practitioner can actually perceive

Five, each a **partial and partly misleading** function of the quality vector:

| Sign | Function of | Faithfulness |
| --- | --- | --- |
| `heft` | `density` | near-faithful |
| `grain` | `fixity`, `malleability` | good |
| `lustre` | `malleability`, `fixity` | moderate |
| `odour` | `volatility`, `vitality` | weak |
| `hue` | `causticity`, `vitality` | **weak and misleading** |

`hue`'s weakness is the design's point of contact with the historical doctrine
of signatures ("the root is red, so it treats blood"). A practitioner reasoning
from hue will be wrong in a specific, derivable, reproducible way — and will
not be wrong because anyone authored a superstition.

### 3.3 Processes

Six authored operations: `grind`, `calcine` (fire), `dissolve` (water),
`distil`, `ferment` (requires `vitality`), `amalgamate` (combines two inputs).

## 4. The production notation

A production is written as preconditions, a process, effects, a conservation
witness, and an emitted sign. The notation is the campaign's authoring surface
and its test-fixture language:

```
  calcine:  [ volatility >= 0.4, fixity < 0.5 ]
            --fire-->
            [ volatility -= 0.4, fixity += 0.3 ]
            ! mass-balance
            ~ fume(acrid)
```

Reading the required slots off the notation is what forced §2's correction and
the sign inventory: the `~` slot cannot be filled by a quality, only by a sign,
because it is what an observer *gets*.

## 5. What is universal, what is derived, and what is drawn

This is the campaign's central determinism claim, and it is a strong one:

- **Universal (identical in every world):** the quality inventory, the sign
  inventory, the processes, the production set, and the quality→sign
  projection. Chemistry is the same everywhere; this is the mundane tier
  (metaplan §5.4).
- **Derived (a pure function of already-drawn state):** which substances a
  world actually *has*, and where — read off terrain's `Deposit` (with its
  drawn `grade` and `tonnage`), `RockClass`, `SoilOrder`, and species'
  `BiosphereTraits`.
- **Drawn:** **nothing.**

> **The Reagent draws nothing.** It introduces no `streams.rs`, no seed-derivation
> label, no new save-format contract, and no epoch risk. It is a pure
> projection over state other domains already drew.

Per-world variation still arrives in full, through *availability*: two worlds
with identical chemistry differ in which productions are reachable, because
they differ in what they are made of. This is the frontier essay's own
observation that a small delta in primitives is a combinatorial delta in what
is reachable — obtained here for free.

## 6. Architecture

Placement is forced by the layering law (metaplan §4.1), and the template is
exact rather than analogical — `domains/language/src/phonology.rs` defines
`Envelope` as "language's OWN copy of the articulation dimensions — populated
later by the composition root from the species `ArticulationVector`; this
domain never imports `hornvale-species`," and `windows/worldgen/src/lib.rs:3257`
carries it across with `envelope_of`.

- **`domains/alchemy`** (new crate, kernel-only). Owns `QualityVector`, the
  sign projection, the process set, the productions, and `permits`. Defines
  its own `Substrate` struct — alchemy's copy of the material dimensions it
  needs — and **never imports `hornvale-terrain` or `hornvale-species`.**
- **`windows/worldgen`** — a pure carry function `substrate_of(...)` mapping a
  located `Deposit` / `RockClass` / `BiosphereTraits` into a `QualityVector`,
  in the same shape and the same file as `envelope_of`.
- **Nothing in `windows/vessel`.** The agent fold is campaign 2.

## 7. Conservation: what `permits` enforces

`permits(production) -> bool` admits a production only if mass balances across
it: the summed bulk of the outputs (including fume and residue outputs) equals
the summed bulk of the inputs. This is the mundane tier's single invariant, and
it is deliberately the one the shipped carve battery already reasons about, so
the campaign borrows an existing discipline rather than inventing a law.

Opening UNI-2 later relaxes exactly this predicate and nothing else.

## 8. Evidence

1. **Property battery** (`domains/alchemy/tests/production_properties.rs`):
   every production in the table satisfies `permits`; no production produces a
   quality vector outside [0,1]; the sign projection is total.
2. **Determinism:** the same seed yields the same substance set for the same
   world, byte-identically. Trivially true given §5, and asserted anyway
   because §5's claim is the thing most likely to be broken by a later change.
3. **Divergence:** two seeds with materially different geology yield materially
   different *reachable* production sets. This is the campaign's one
   substantive claim about the world rather than about the code, and the number
   it produces is the baseline campaign 2's hypothesis is measured against.
4. **Layering:** `cli/tests/architecture.rs` continues to pass — i.e. the new
   crate genuinely depends on the kernel alone.

No lab metric is registered by this campaign, so no census regeneration is
owed. (Campaign 2 registers the accuracy metric and *will* owe one.)

## 8a. Amendment at close — item 3's real scope

Item 3 above claims "two seeds with materially different geology yield
materially different *reachable* production sets… and the number it produces is
the baseline campaign 2's hypothesis is measured against." **As written this
overstates what is true.** Measured:

- **At default terrain pins every seed reaches all seven productions.** Four
  ubiquitous source categories cover the whole table — life alone unlocks four
  of seven, and any silicate rock adds a fifth — so a default globe saturates
  it regardless of seed. The baseline is a constant, not a variable.
- Genuine production-set divergence appears only at deliberately sparse pins
  (`ocean_fraction=0.95, plates=2, continents=1, globe_level=4`), where roughly
  half of seed pairs differ — and the difference is **one production wide**
  (`dissolve-salt`).
- The substance-set half of the divergence assertion passes at default pins
  only because thousands of per-cell ore grades are distinct floats. Those
  floats provably cannot change a production set, so that assertion is true but
  weak as evidence.

**Reachable-production count is therefore a weak discriminator by
construction**, and the shipped test says so in its own doc comment rather than
hiding it.

**Consequence for campaign 2, to settle before it preregisters.** The
metaplan's hypothesis — doctrine accuracy varies with the terrain heterogeneity
an alchemist can reach — is untestable if exposure is "the world's reachable
production set," because between-world variance is zero at default pins. Two
changes rescue it, and both should be written into The Signature's spec:

1. **Measure reach locally, not globally.** An alchemist reaches a settlement
   catchment, not a globe. Globe-scale saturation says nothing about local
   variation, and at ~130 land cells the endowment already varies.
2. **Use a finer measure than production count** — distinct material categories
   reachable, or ore-grade dispersion. Grade dispersion is the most promising,
   since grade → causticity is the confound's actual mechanism and is exactly
   what a production-set measure discards.

## 9. In / out

**In:** the three inventories; the production table and its notation;
`permits`; the `Substrate` carry at the composition root; the property battery.

**Out:** any practitioner, observation, belief, or doctrine (campaign 2);
transmission, schools, lost arts (campaign 3); the violation tier (UNI-2, §5.4
of the metaplan); dose, purity, and temperature as modifiers — the *unmodified*
rung of the accuracy scale is reserved, not shipped; potions, inventory, and
identification; economy and trade.

## 10. Risks

1. **The inventory is the taste surface** and is the reason this spec puts the
   full proposed contents in §3 rather than deferring them to implementation.
   It is the one thing here that cannot be derived and the one thing worth
   arguing about now.
2. **An implementer builds the crate with a sibling dependency.**
   `architecture.rs` catches it, but late; §6 states the constraint and names
   the exact file to copy.
3. **Sign faithfulness is a tuning surface with no data behind it.** The
   `hue` weakness in particular is a judgement call, not a measurement. It is
   also cheap to retune, and campaign 2's accuracy metric is what will
   eventually put a number on whether it was set sensibly.

## 11. Flagged for G3

1. **The quality inventory (§3.1) and the sign table (§3.2)** — the taste
   surface the metaplan promised to bring to you before campaign 1 rather than
   during it. Eight qualities, five signs, six processes.
2. **The §2 amendment** — qualities are latent, not observable. This reverses a
   line in the metaplan you approved this morning; it is a correction, and the
   metaplan has been amended in the same commit.
3. **"The Reagent draws nothing" (§5)** — worth confirming, because it is the
   claim that makes this campaign low-risk, and because a later reviewer
   finding a draw hidden in the substance derivation would mean a save-format
   contract nobody registered.

## 12. Provenance

Brainstormed 2026-07-26 under `campaign-autopilot`. Ideonomy: one pass —
cross-domain re-instantiation + substitution (organon: notation; prompts:
visibility, discovery-vs-invention, materiality). The visibility prompt
produced §2's correction; the notation organon's required slots produced the
sign inventory; re-instantiation into metallurgy produced the **wootz steel**
anchor — a lost art whose recipe depended on trace vanadium in one ore body,
which died when that body was exhausted and has never been reconstructed. That
is the provenance confound producing a lost art, historically, and it is the
case campaign 3 should be measured against.

Full decision trail: `.superpowers/sdd/decision-ledger.md`.
