# The Crucible: An Alchemy Program Metaplan — Design

**Date:** 2026-07-26
**Status:** Approved at G3 2026-07-26. **Amended same day (Amendment 1, §9
risk 1): qualities are latent, not observable** — see
`2026-07-26-the-reagent-design.md` §2.
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution;
UNI-1's "one mechanic" thesis and UNI-2's metaphysics gate)
**Worktree:** `the-crucible` (branch `the-crucible`), off `main` at `bd7314a0`
**Autopilot:** engaged (G3/G6 hard stops; ledger at
`.superpowers/sdd/decision-ledger.md`)
**Frontier:** `book/src/frontier/frontier.md` §"Magic as a graph grammar — the
debt economy"; new registry rows `ALCH-1`…`ALCH-6`

---

## 1. What this is

A **metaplan**, not a single campaign: the arc from a world that knows what it
is made of but has nobody reasoning about it, to a world in which practitioners
derive **materially wrong doctrines** about matter — and are wrong for reasons
the simulation generates rather than reasons an author wrote down.

Two framings were tested during brainstorm and discarded, and their discarding
is why this is a metaplan rather than a campaign:

- **"An alchemy system."** Misleading. The player-facing potion-and-
  identification loop is a downstream consumer, not the spine. Naming it that
  would put the smallest, most game-first slice in front of the engine that
  makes it worth having.
- **"One campaign."** False. A doctrine cannot be wrong about a material layer
  that does not exist yet, and culture-scale transmission cannot inherit an
  agent fold that has not shipped. The dependency is a chain, not a T.

This metaplan fixes the thesis, the load-bearing laws, the architecture under
the layering constitution, the sequencing, and the evidence. Each campaign in
§7 gets its own spec → plan → execution cycle.

## 2. The thesis: the provenance confound

Strip the domain vocabulary from "an alchemist grinds herbs and minerals,
observes what happens, and forms a doctrine about which substances transform
into which," and what remains is:

> An agent induces a general rule from a **spatially biased sample** of a
> function whose true arguments are **partly imperceptible to the sampler**.

That is the whole design, and it yields the program's central claim:

> **A recipe works in one valley and fails in the next because the reagent's
> hidden property differs — and the doctrine names the wrong cause.
> The world is heterogeneous; the alchemist is local. That is the entire
> error term.**

Call it the **provenance confound**. Its consequence is the thing that makes
this program cheap relative to its payoff: **no falsehood is ever authored.**
Author what is true, and locality generates the error for free. This is the
same move the frontier essay makes when it derives the violation catalog off
the property batteries instead of off imagination — and, like that move, it
makes the content *grow with the simulation's fidelity* rather than with an
author's word count.

The shape is not new here. It has already shipped three times: The Surmise
(belief as a fold over what an agent happened to perceive), LANG-5's
Neogrammarian comparative reconstruction (proto-forms induced from surviving
daughters), and MEM's lost-arts cluster. The Crucible is a fourth site, not a
new mechanism.

## 3. What is read off shipped machinery, and what is authored

The honest accounting, verified against the tree at `bd7314a0`:

**Already shipped, read directly:**

- `domains/terrain/src/features.rs` — a `Commodity` enum (Gold, Iron, Salt,
  Coal, Gems, Tin, Bauxite), `DepositProcess`, and located `Deposit`s carrying
  lognormal `grade` and relative `tonnage`, banded by depth. The Lode's work.
- `domains/terrain/src/lithology.rs` — `RockClass`, `SoilOrder`, and the
  `MaterialBuffer`'s `metamorphic_grade` / `carbonate` terms.
- `domains/species` — `BiosphereTraits`, `MetabolicClass`, `ConditionNiche`.
- `windows/vessel/src/liveness.rs` — belief as a **pure fold over committed
  facts**, with no stored belief state. The doctrine layer is this, pointed at
  a different subject.

**Authored, and deliberately small:**

- A **quality inventory** (§5.5) — the substance feature set. Target 8–12
  entries. Taste-gated, and the only place in the program where taste is the
  binding constraint.
- An **explanation-schema inventory** — the ways a doctrine may name a cause:
  signature, sympathy, contagion, provenance, dose. Frazer's two laws taken as
  *cognitive* primitives, per the frontier essay's re-grounding (Rozin and
  Nemeroff, not *The Golden Bough*'s discredited ladder).

Four of the six architectural cells in §4 are reads; two are authored.

## 4. The architecture: the phonology triple × two layers

The generative shape this project has now re-instantiated twice — one authored
inventory, a **derived** selection from it, and an admissibility predicate
(`domains/language/src/phonology.rs`, transposed verbatim by
`windows/vessel/src/interior/pattern.rs`) — crossed against the two layers
gives the program's full surface:

```
                | MATERIAL LAYER (truth)          | DOCTRINE LAYER (belief)
----------------+---------------------------------+--------------------------------
AUTHORED        | Qualities: a small authored set  | Explanation schemas: the ways a
INVENTORY       | of substance features. A         | doctrine may name a cause --
                | material is a BUNDLE of them,    | signature, sympathy, contagion,
                | as a phoneme is a feature        | provenance, dose. Authored as
                | bundle. ~8-12 entries.           | cognitive primitives.
----------------+---------------------------------+--------------------------------
DERIVED         | Which substances THIS world      | Which explanation THIS agent or
SELECTION       | has -- read off lithology,       | school adopts, folded from what
                | deposits + grade, biosphere      | it has actually observed. Two
                | traits. Nothing new authored;    | alchemists in different valleys
                | the world already knows what     | derive different doctrines from
                | it is made of.                   | identical authored schemas.
----------------+---------------------------------+--------------------------------
ADMISSIBILITY   | Which productions are well-      | Whether a doctrine ACCOUNTS FOR
VALIDATOR       | formed. Conservation is the      | the observations it has seen --
                | predicate, so mass balance       | NOT whether it is true. The
                | comes from the shipped carve     | load-bearing distinction in the
                | battery, not a new law.          | whole program (§5.3).
```

### 4.1 Where the code lives (forced by the layering law)

`domains/CLAUDE.md` forbids a domain crate from depending on a sibling. The
material layer must read terrain's `Deposit`/`RockClass` and species'
`BiosphereTraits`, so it **cannot** be a self-contained alchemy domain that
reaches sideways. The constitution already supplies the resolution:

- **`domains/alchemy`** — kernel-only. Owns the quality inventory, the
  production grammar, and `permits`. Knows nothing about rocks or creatures;
  it knows about *qualities*.
- **`windows/worldgen`** — the composition root, and the only place the
  binding happens: located materials (a deposit here, a plant there) are
  projected into quality bundles and handed to the domain.
- **`windows/vessel`** — the agent fold: an alchemist's doctrine over its own
  committed observation facts.
- **`windows/lab`** — the doctrine-accuracy metric (§6), because metrics are
  code (decision 0011).

This is the single most likely thing to be got wrong by an implementer who
starts from "alchemy is a domain," and it leads the G3 flagged section.

## 5. Core commitments

### 5.1 Truth is universal; doctrine is local

The material rule-set is a property of the world. A doctrine is a property of
a **reach** — the set of places an agent or school has sampled. Every divergence
in the program traces to this asymmetry, and it is why the scope dimension is
the one that generates content rather than the complexity dimension.

### 5.2 A doctrine is a fold, never stored state

Re-derived from committed facts on demand, exactly as `liveness.rs` computes
believed water and believed hazard today. No doctrine is serialized; the ledger
is the only store. This is not a performance choice — it is what keeps the
layer byte-identical by construction and immune to the save-format contracts
that bite hardest in domains.

### 5.3 Admissibility is not truth

The doctrine validator asks *does this doctrine account for the observations
this practitioner has actually made?* — never *is this doctrine correct?* A
doctrine that is wrong about the world but consistent with everything its
holder has seen is **well-formed**, and must be. Collapsing these two
predicates would destroy the program's entire subject matter in one line, and
it is the review check every doctrine-layer campaign must carry.

### 5.4 The metaphysics gate is one predicate

UNI-2 asks whether magic is a second physics. This program **does not open that
gate**, and does not need to: per the frontier essay, superstition is the
universal and magic is the special case where superstition happens to be right.
A world with an *empty* violation catalog still derives correspondence-thinking,
false causes, rival schools, and lost arts.

The structural claim that makes this safe: **a production is a production
whether or not it violates an enforced invariant.** Opening the gate later
changes `permits` — one admissibility predicate — and nothing else in the
architecture. Shipping the mundane tier now therefore costs the magical tier
nothing.

### 5.5 Substances are quality bundles, not a catalogue

A material is a bundle of authored qualities, as a phoneme is a feature bundle.
An atomic substance list would be a lookup table, i.e. exactly the
catalogue-not-a-language failure that `CLIENT-language-not-catalogue` and The
Hearth's pattern discipline exist to prevent. Bundles buy a combinatorial space
from a small authored inventory; that is the whole reason `phonology.rs` is the
right template rather than a loose analogy.

### 5.6 Alchemy is religion's fallible sibling

`domains/religion` consumes phenomena without learning which system produced
them. Alchemy occupies the *inference* branch: a practitioner who can be wrong.
Revelation — knowledge that cannot be wrong because it did not come from
sampling — stays religion's. The existing phenomena seam is unchanged by this
program, and no campaign here may weaken it.

## 6. The evidence: doctrine accuracy as an ordinal metric

The program's falsifiable claim needs a number. The axis is *how much of the
true rule a doctrine captures*, and it is a lab metric (decision 0011):

```
  0%  | SIGNATURE        | Names a cause with zero predictive power.
      |                  | "The root is red, so it treats blood."
 ~20% | CONFOUNDED       | A real correlate mistaken for the cause. Works in
      |                  | the valley; the cause is the soil, the doctrine
      |                  | says the moon.  <-- the provenance confound
 ~50% | OVERREACHED      | Right cause, wrong SCOPE. A true local rule
      |                  | generalized past its domain.
 ~80% | UNMODIFIED       | Right cause, right scope, missing a modifier --
      |                  | dose, purity, temperature.
 100% | TRUE PRODUCTION  | Indistinguishable from the sim's own rule.
      |                  | This position is called "science."
 >100%| ACCRETED         | Names MORE conditions than the truth has.
      |                  | Precision without accuracy; ritual accretion.
      |                  | A doctrine can be worse than wrong by being
      |                  | over-specified.
```

Two features of this scale are load-bearing. First, it has a region **past its
own maximum**: over-specification is a distinct failure from error, and it is
the mechanism behind ritual accretion. Second, it makes the program
preregisterable:

> **Hypothesis (for campaign 2's preregistration):** doctrine accuracy varies
> inversely with the terrain heterogeneity an alchemist can reach. A flat,
> homogeneous world should converge doctrines toward truth; a fractured,
> mountainous world should hold them at CONFOUNDED and proliferate schools.

Measurable against the existing census, which regenerates locally in ~7 min
(decision 0063).

## 7. The campaign carve

Sequenced by what is inherited. Each is its own spec → plan → execution cycle.

### 7.1 Campaign 1 — The Reagent (the material layer)

The quality inventory; per-world substance derivation read off lithology,
deposits, and biosphere traits at the composition root; the production grammar;
conservation as `permits`. **Ground truth only — no practitioner, no doctrine,
nothing believed.** Evidence: substances and productions differ across seeds
and are byte-identical within one; conservation holds over the property
battery. Small, and it fixes the contract every later campaign inherits.

### 7.2 Campaign 2 — The Signature (the agent fold)

The thesis campaign. An alchemist folds its own committed observations into a
doctrine over the authored explanation schemas; the accuracy metric; the
preregistered heterogeneity hypothesis in §6. Two alchemists in different
valleys must derive different doctrines from identical machinery — that
divergence, demonstrated and measured, is the program's whole point and its
first falsifiable result.

### 7.3 Campaign 3 — The School (culture scale)

Transmission: apprenticeship inherits a doctrine, then divergence acts on it.
Orthodoxy, heresy, and lost arts fall out. This is where MEM's transmission
channels and MEM-9's suppression-as-craft attach, and where LANG-5's shipped
comparative-reconstruction cascade can be pointed at recipes rather than words.

### 7.4 Campaign 4 — reserved, UNI-2 gated

The violation tier. Changes `permits` and nothing else (§5.4). **Not scheduled
by this metaplan**; it is named here only to record that the architecture
already admits it.

**Later rungs of the wrongness ladder, reserved:** staleness (a changing truth)
and deception (a lying informant), in that order. These are not invented here —
The Surmise's registry row already reserves exactly these two as "later, harder
instances," with locality as the one it demonstrated.

## 8. In / out boundary

**In, across the program:** the material layer; the doctrine fold at both
scales; the accuracy metric; the explanation schemas.

**Out, explicitly:**

- **Potions, inventory, and the identification minigame.** UNI-1's roguelike-ID
  claim is real and MEM-8 describes it well, but it needs an item layer this
  program does not build. A downstream consumer.
- **Any UNI-2 metaphysics.** §5.4.
- **Economy and trade in reagents.** Adjacent and tempting; a separate arc.
- **Combat, poison-as-weapon, medicine-as-healing mechanics.** These are
  consumers of a dose axis that campaign 1 reserves but does not ship.

## 9. Risks

1. **The quality inventory is taste-gated.** It is the one authored artifact
   whose contents cannot be derived, and a bad inventory yields a generative
   space that is combinatorially large and semantically empty. Mitigation: keep
   it small, and require every quality to be *inferable in principle* — it must
   affect at least one observable, or it is dead weight.

   > **Amendment 1 (2026-07-26).** This mitigation originally read "observable
   > in principle by a practitioner." That was backwards: if qualities were
   > directly observable there would be no provenance confound at all, and the
   > program would have no subject matter. **Qualities are latent; what a
   > practitioner perceives are signs — weak, partly misleading functions of
   > the qualities — and reaction outcomes.** Corrected in
   > `2026-07-26-the-reagent-design.md` §2, which is binding.
2. **Collapsing admissibility into truth** (§5.3). A single reviewer check per
   doctrine campaign; called out here because it is a one-line mistake that
   silently deletes the subject matter.
3. **An implementer builds a self-contained alchemy domain** that reaches
   sideways into terrain, violating the layering law. §4.1 exists to pre-empt
   this; `cli/tests/architecture.rs` will catch it, but late.
4. **Campaign 2's hypothesis may fail.** Doctrines may converge regardless of
   terrain, if the reach of a settled agent is small enough that every world
   looks locally homogeneous. That would be a real result and would re-scope
   campaign 3, not invalidate the program — but it must be preregistered before
   the measurement, per the branch-absorption rule about not absorbing main
   mid-measurement.

## 10. Flagged for G3 — owner decisions, not autopilot's

1. **The layering resolution (§4.1)** — `domains/alchemy` kernel-only, binding
   at `windows/worldgen`, fold in `windows/vessel`. Forced by the constitution
   rather than chosen, but it is an architecture call and a new domain crate,
   so it should be seen and not merely inferred.
2. **The metaphysics gate stays shut (§5.4)** — this program ships a
   culture-and-knowledge engine with no magic in it, on the frontier essay's
   own argument. If the intent behind "alchemy" was the magical tier, this is
   the decision to reverse, and it reverses cheaply now and expensively later.
3. **The quality inventory is the taste surface (§3, risk 1)** — the one place
   authorship rather than derivation binds, and worth Nathan's eye before
   campaign 1 rather than during it.
4. **The program is three campaigns, not one** — §7, following #3 in the
   ledger.

## 11. Decisions to record in `docs/decisions/`

- **Alchemy's error term is derived, never authored** (the provenance
  confound). Durable because it will be re-proposed as an authored table by
  anyone who has not read §2.
- **Admissibility is not truth** for any belief-layer validator in the project.
  Broader than alchemy; states a rule the vessel's existing folds already obey
  implicitly.
- **A domain that must read siblings binds at the composition root** — arguably
  already implied by the constitution, but this is the first case where the
  need is intrinsic to the domain's subject rather than incidental.

## 12. Definition of done (per campaign, per decisions 0013 and 0020)

Each campaign in §7 closes with: the commit gate green; a chronicle entry in
`book/src/chronicle/`; a freshness sweep of stale chapters; a Confidence
Gradient re-score if it moves a bet (decision 0030); a one-page retrospective
in `docs/retrospectives/`; registry status flips for the `ALCH-*` rows it
resolves; and, where it adds a lab metric, a census regeneration
(`HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`) — because the-census is
"all registered metrics" and a new metric expands its schema.

## 13. Provenance

Brainstormed 2026-07-26 under `campaign-autopilot`. Two ideonomy passes:
abstraction-lift + combination (organon: scale; prompts: hierarchicalness,
polarity, scope), which produced the provenance confound and overturned the
starting assumption that wrongness had to be authored; and negation (organon:
chart; prompts: decomposability, complexity), which settled the metaphysics
gate, reproduced The Surmise's reserved ladder independently, and surfaced the
religion boundary in §5.6 that nobody had asked for.

Full decision trail: `.superpowers/sdd/decision-ledger.md`.
