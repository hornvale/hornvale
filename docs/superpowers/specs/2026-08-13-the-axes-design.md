# The Axes — a place-type is a point, and the corpus says which axes

**Status:** DRAFT, at G3 · **Campaign:** the-axes (`campaign/the-axes`) ·
**Program:** [The Chorography](2026-08-12-the-chorography-metaplan.md), campaign 1
· **Depends on:** campaign 0 ([The Fathom](2026-08-12-the-fathom-design.md)),
merged `6daabcd7`

Campaign 0 gave every realm a depth coordinate. This campaign decomposes
`Formation` from a flat enum of whole-community names into independently-valued
axes, and unifies `ConditionNiche` with `BiomeAffinity` into one response vector
over those axes.

The metaplan's §7 forbids this campaign opening until its spec carries a
reconstruction test. §4.4 and §6 are that test — restructured, because §7
described one procedure where there are two with different epistemic status, and
stated a criterion that cannot fail.

## 1. What this campaign produces

1. **A feature matrix** — the axis table, with a declared *valence*, *grounding*
   and *grain* per axis, plus **redundancy rules** in the phonological sense
   (which axis values are predictable from which others). Not a flat list of six
   names.
2. **An executable preregistration** — the compression bounds, the named
   resisters, and the per-arm stop rules as `#[test]`, not as prose.
3. **An additive unification** of `ConditionNiche` and `BiomeAffinity` that
   leaves `tolerance_liebig`'s flooring structure untouched.
4. **A measured verdict on factoring versus fidelity**, stated at the strength
   the measurement supports and no higher.

## 2. Non-goals

- **Grain.** Room-scale objects stay with The Grain (metaplan §5). This campaign
  supplies vocabulary; it does not place mangrove fringes.
- **The `Biome` enum → open-registry migration.** The atoms survive as a legacy
  projection. 189 references are not touched.
- **Composing the eight parallel place-describing enums.** They stay
  hand-mirrored per decision 0094.
- **Reshaping which axes `tolerance_liebig` floors.** See §4.3 — this is a
  deliberate non-goal, not an oversight, and it is what keeps The Holdfast's
  fast path sound.
- **Naming axis values.** Axis values do not become registered concepts. See
  §5.2.
- **Any world-side measurement before The Glasshouse lands or is declined.** See
  §5.5.
- **Building The Tense's §3.3 two-tier successor.** `tolerance_tiered` stays in
  shadow. Reshaping the evaluation is that campaign's stage, not this one's.

## 3. The findings, verified

All read in the tree at `57e21e59`, by command, not inferred.

### 3.1 The corpus

**21 `Formation`s** — 12 land, 6 marine, 3 cave — and **53 `Variant`s** across
65 weighted pool entries, every variant reachable from some pool. The metaplan's
"21 and ~50" is exact at 21 and 53. Corpus size for the compression budget in
§6.1 is therefore **74 names**.

### 3.2 The land half of the collinearity question is settled by reading, not
measurement

`classify_land` (`domains/climate/src/biome.rs:295`) takes
`(temp_c, moisture, elevation_m, sea_level_m, latitude_deg)` and is a pure
decision tree: an ice cut on temperature, an alpine gate on
`elevation - sea_level > tree_line(latitude)`, then a two-dimensional cut on
temperature × moisture producing the remaining ten land formations. Thresholds
are literals — freeze 0 °C, taiga 7 °C, temperate 20 °C, and moisture cuts at
0.35 / 0.30 / 0.25 / 0.40 / 0.75 / 0.20 / 0.45.

Therefore any axis vector **assigned per land formation** is a deterministic
function of exactly the inputs `classify_land` already reads. For the land
corpus the decomposition is **necessarily a factoring**; it cannot be fidelity,
by construction, for all six proposed axes and not merely §7's two suspects.

The metaplan §7 says "physiognomy and water regime are plausibly pure functions
of temperature and moisture" and asks the campaign to measure which it is. On
land the answer is derivable and the measurement would confirm an identity. This
spec corrects §7 rather than repeating it.

**The distinction that makes fidelity available at all:** an *assignment map*
(name → vector) reads no world state and can add nothing. A *derivation* (world
state → vector, with the name as a projection) can. Fidelity is only reachable
from inputs `classify_land` does not read, and three exist:

- **substrate** — `GroundKind` (5 values, `variants.rs:334`) and `SoilOrder`
  (9 values, `lithology.rs:692`). Note `classify_soil` itself branches on
  moisture and temperature for five of its nine orders, so the substrate axis
  inherits partial collinearity; what survives comes from `RockClass`,
  `slope_m` and `SoilDepth`.
- **light/depth** — The Fathom's column, shipped and unconsumed by the biome
  path.
- **disturbance** — no producer exists anywhere in the workspace.

### 3.3 `variant_pool`'s signature is already an axis vector

`variant_pool(formation, stratum, ground)` (`variants.rs:349`) is a live
three-axis cross-product shipping today. The campaign's job is to **unbundle its
first argument**, not to invent a vector — which makes a byte-identity check
available as the primary acceptance test (§7.1).

### 3.4 The affinity ladder is a hand-authored metric that already encodes a
two-axis decomposition

`domains/species/src/lib.rs` defines `AFFINITY_NEAR` (0.70) as "**one band out**
in the classifier's lookup table" and `AFFINITY_MARGINAL` (0.45) as "**two bands
out, _or the right climate in the wrong form_**".

That second clause states that two distinct moves land on the same rung: two
steps along climate, or one step along **form**. It is a two-axis space with a
stated exchange rate, authored by someone not attempting a decomposition, in a
domain that cannot see `Formation`. It is the only hand-authored metric on this
space that exists, and §6.4 uses it as independent validation of axis geometry.

### 3.5 Ten `Variant`s are phases, not states

Verified from their own doc comments. Post-event succession: `ForestGap` ("a
break in the canopy where light reaches the ground"), `MossyDeadfall`, `Burn`
("ground recovering from fire"), `FireScrub`, `ReefRubble`, `UrchinBarren`
("seabed grazed bare of kelp"). Seasonal phases of sea ice: `PressureRidge`,
`IceLead`, `RaftedFloe`, `MeltPond`.

A phase is not a point in a state space. §6.2 freezes these ten as the predicted
resisters.

### 3.6 `tolerance_liebig` acquired a fast path whose precondition nothing checks

The Holdfast (merged in the 72 commits absorbed at branch open) added to
`windows/worldgen/src/lib.rs`:

```rust
let elevation = cn.elevation.eval(s.height_asl_m.get(), 0.0);
if elevation <= floor_buf { return elevation; }
```

Its soundness argument is that the three floored axes each return at least
`floor_buf` while elevation is passed `0.0` and may return less. Two
`debug_assert!`s guard it, and they check only that `floor_buf ∈ [0,1]` and that
three devotions are non-negative. **Neither checks the load-bearing precondition
— that exactly one axis is unfloored, and that it is the one that may undercut.**

Renaming or re-shaping `ConditionNiche`'s fields is caught by the compiler.
*Reinterpreting* a field while keeping its name is not: it compiles, both asserts
pass, and the minimum comes back wrong. §4.3 is the response.

The same comment records a measured number this campaign should cite rather than
re-derive: with elevation unfloored, **elevation binds on 100% of land for
goblin, gnoll and human** — direct support for metaplan §3.5's claim that one
axis is doing all the work.

**The minimum is implemented at least three times:** the private original, a
hand-inlined mirror at `windows/worldgen/tests/warren_gate.rs:220` ("private, so
it is inlined here"), and `binding_axis` in `delver_readout.rs`, described as
"the mirror of the private `tolerance_liebig` — verbatim". Ten-plus test files
assert against its behaviour in doc comments. This is The Fathom retrospective
§1's lesson — *grep the consumers, not just the producer* — arriving before the
spec rather than mid-implementation.

### 3.7 The periodic grid: 12 of 21 formations occupy 1 of 11 legal cells

`Realm::strata()` gives `AirOverRock` one rung and water and rock five each, so
realm × stratum has 11 legal cells. `OVERWORLD/Surface` holds twelve formations;
`UNDERDARK` holds **zero placements across five rungs**, despite three legal
`Formation` values (`KarstCave`, `LavaTube`, `FractureCave`).

Two known follow-ups are visible as grid shape rather than as prose: F-3
(canopy, cliff face and air are **unaddressable**, not merely unnamed, because
`strata()` hands `AirOverRock` a one-element ladder) and F-4 (`strata()` keys on
`medium`, not realm, so an underground sea is handed the pelagic ladder).
Neither is this campaign's to fix; both are recorded because the grid makes them
countable.

### 3.8 A correction to retrospective F-2

F-2 reads "`SoilOrder` is derived and unread by the biome path … already
computed and unused." The *biome path* half is accurate. "Unused" is not:
`SoilOrder` has two live consumers, `windows/worldgen/src/alchemy.rs:81`
(`substrate_of_soil`) and `windows/lab/src/metrics.rs:1913`. Consuming it in the
biome path adds a **third** consumer to a live derivation, and whatever the
biome path does with it must not perturb the two that exist.

## 4. Design

### 4.1 The feature matrix, with the slots §7 left empty

§7 takes its provenance from distinctive-feature phonology and then writes a
flat list of six names. Phonology's notation has required slots, and the empty
ones are where this campaign's questions live. The axis table declares, per
axis:

```
FEATURE        VALENCE      GROUNDING (world input)            GRAIN
-----------------------------------------------------------------------
physiognomy    ordinal      f(T, M)                            cell, room
energy         scalar       insolation                         cell
water          scalar       moisture; the Rill's budget+alloc  cell, room
substrate      nominal(5)   GroundKind / SoilOrder             cell, room
light          scalar       land: f(insol, aspect, canopy)     cell, room
                            sea:  == Stratum  (see 4.1.1)
                            cave: constant 0
disturbance    RATE         (no producer exists)               room
```

**Valence is not homogeneous, and that is a finding rather than an
inconvenience.** Five axes are state values; disturbance is a *rate*. Speed a
rate up and it becomes a state (everything always burning is a stable
community); slow it down and it vanishes into the climax. A `Formation` is a
state, so no disturbance value is assignable to any of the 21 — which is why
§6.2's predicted resisters are all `Variant`s.

**Redundancy rules are the formalism for collinearity**, and phonology — §7's
own stated provenance — already solved this problem. A feature matrix plus
redundancy rules distinguishes *contrastive* features from *predictable* ones.
The campaign's real measurement is therefore a count: **contrastive features
remaining once redundancy rules are applied.** On land that count is zero beyond
`classify_land`'s inputs (§3.2, re-derived inside the notation rather than
beside it). Marine and cave are open, and are what §6.3 measures.

#### 4.1.1 Light is not a sixth axis in the sea; it is `Stratum`

The pelagic ladder *is* a light ladder, and the corpus names it: `Mesopelagic`
is "twilight water", `Bathypelagic` is "lightless water", and the variants are
literally `TwilightWater` and `LightlessWater`. Light therefore has three
groundings and one identity, and in the sea The Fathom already shipped the
machinery. The axis table consumes the column rather than deriving a parallel
light value.

#### 4.1.2 Two proposed axes are not axes of state

Disturbance, and the anthropogenic case F-6 names, both say the *generating
process* changed rather than naming a value some cell carries. They cannot be
read off a cell the way temperature can. The matrix records them with their
valence declared, and the campaign does not pretend they are the same kind of
thing as substrate.

### 4.2 The vocabulary lives in the kernel — **ruled at G3**

`BiomeAffinity` is keyed by biome **name string**, and its doc states why: *"a
domain crate may not depend on a sibling domain … resolution against the live
`Biome` value happens at the composition root."* A vector shared between
`domains/climate` (producer) and `domains/species` (consumer) has exactly that
problem, and the metaplan never says where it lives. The layering rule admitted
two answers — `hornvale-kernel`, or hand-mirrored per 0094 with resolution at
`worldgen`. **Nathan ruled: the kernel.**

The ruling is stronger than it looked when it was asked, because the pattern is
not merely *permitted* there — it is **already implemented there, twice, with
its hazards worked out.** `kernel/src/ecology.rs` carries:

```rust
pub struct ResourceAxis { .. }                      // a named axis
pub const PHOTOSYNTHATE: ResourceAxis = ..;         // six axis constants
pub fn v1_basis() -> &'static [ResourceAxis];       // a VERSIONED basis
pub struct ResourceVector(BTreeMap<u16, f64>);      // sparse, validating ctor
```

So the axis table is not a new kind of object in this codebase. It is a second
basis alongside `v1_basis()` — different semantics (that one is trophic supply,
this one is environmental state) and the same form. Three things transfer, and
this campaign adopts all three rather than re-deriving them:

1. **The basis is a versioned `&'static [Axis]`**, named with its version, so a
   later refinement is a `v2` epoch rather than an edit in place.
2. **The response vector is sparse with a validating constructor**, rejecting
   non-finite weights, with the zero vector legal and meaningful.
3. **Order is append-only, pinned by a guard test.** `v1_basis`'s own doc states
   the hazard exactly: *"Insert an axis at or before an existing one and you
   change which axis wins those ties … prepending `MARINE_FORAGE` would make a
   zero-weight niche resolve marine-dominant instead of photosynthate-dominant."*
   It is pinned by `the_basis_ids_are_append_only`, "which is what makes this a
   rule rather than a hope."

**The tie-break hazard transfers and must be designed against, not inherited.**
Any argmax or dominant-axis read over the environmental basis resolves total
ties to index 0. This campaign's basis will be read that way (a "which axis
binds here" query is the natural consumer, and §3.6 shows the codebase already
does exactly this for `ConditionNiche`). §5.5 makes the ordering contract
explicit rather than leaving it to be discovered.

This is the **third** time this program has found the thing it was about to
build already sitting one crate over — after The Fathom's `water_column_at` and
`variant_pool`'s three-axis signature (§3.3). The Fathom retrospective §1's rule
(*grep the consumers of the raw material, not just its producer*) is now
this program's most reliably load-bearing lesson.

### 4.3 Additive unification — the flooring structure does not move

Campaign 1's brief is to unify `ConditionNiche` with `BiomeAffinity` into one
response vector. The unification is **additive**:

- `ConditionNiche`'s four fields stay untouched, so `tolerance_liebig` compiles
  and behaves identically.
- New axes join as additional **floored** responses.
- Elevation remains the sole unfloored undercutter.

The Liebig minimum extends by taking more terms, and The Holdfast's fast-path
precondition holds **by construction** rather than by review vigilance. This is
expand-contract, the shape metaplan §3.6 blesses and
`windows/locale/src/grammar.rs` has executed here once already.

The invariant §3.6 found unguarded is then stated positively and made
executable: *exactly one axis is unfloored, and it is the one that may undercut
the others.*

### 4.4 The reconstruction test is two procedures, not one

§7 describes a **fitting procedure** — it declares the axis set "the OUTPUT of
that test, not its input", i.e. it licenses revising the axis list in response
to the result. A fitting procedure has nothing to preregister; preregistration
binds *confirmation*. Decision 0016 already carries the carve-out in as many
words: *"Exploratory sweeps remain legitimate but are labeled as such and are
never promoted to confirmations after the fact."*

So:

| stage | corpus | epistemic status | revision allowed |
|---|---|---|---|
| **Fit** | 21 `Formation`s | exploratory, labelled | yes — this is the point |
| **Test** | §6's four held-out arms | confirmatory, frozen | **no** |

The freeze precedes the fit, and §6 establishes that this is *feasible* rather
than merely desirable: the bounds derive from corpus size, which is already
known, not from the fit's outcome.

## 5. Save-format and determinism

### 5.1 No draw moves

This campaign consumes existing draws and adds none. `variant_pool`'s dispatch
is asserted byte-identical (§7.1). Grounding an axis that currently draws noise
must **keep its draw** and spend it as variation about the grounded value — The
Rill's rule, and F-8's, restated because removing a draw is a save-format break
rather than a style choice.

### 5.2 Axis values do not become registered concepts

`Variant::concept_name()` feeds the concept roster
(`windows/worldgen/src/lib.rs:5978`), and `domains/language/src/accession.rs`
sorts concepts by accession epoch under the rule that *"a concept that changed
epoch would re-sort"* — so new concepts must land in a new cohort, a deliberate
act.

Following the codebase's own idiom — everything in the taxonomy gets a
`concept_name()` — would therefore make the axis table a **permanent,
epoch-stamped, named vocabulary on the day it lands**, which is precisely the
naming free-for-all metaplan §3.7 says the brake exists to prevent. Axis values
stay internal and unnamed; only the projected `Formation`/`Variant` names remain
registry-bearing, and the axis table stays revisable.

Stated explicitly because the default implementation does the opposite.

### 5.3 The three copies of the Liebig minimum

Any change to the response vector's shape must be applied to, or proven
irrelevant to, all three sites in §3.6. The campaign's own design (§4.3) is that
none of them changes; the acceptance criteria assert that rather than assume it.

### 5.4 The basis order is a save-format contract from the first commit

Following §4.2's ruling, the environmental basis lands in `hornvale-kernel` —
the crate whose own `CLAUDE.md` opens with *"the three save-format contracts in
this crate … change any of these and the same seed produces a different world."*
The basis is not one of those three, but it acquires the same character the
moment anything reads it by index, exactly as `v1_basis()` did.

Therefore, in the **same commit** that introduces the basis:

- the ordering is declared append-only in the basis function's own doc comment,
  stating the tie-break consequence in the concrete terms `v1_basis` uses;
- an append-only guard test exists, in the idiom of
  `the_basis_ids_are_append_only`, and is **proven to fire** by mutation —
  reorder two axes, watch the specific assertion go red, revert;
- a refinement is a `v2` basis, never an edit in place, per the kernel's epoch
  rule.

Writing the guard later is not equivalent: between introduction and guard, a
reordering is invisible, and this campaign's whole currency is that no world
byte moves.

### 5.5 The Glasshouse sequencing

`campaign/the-glasshouse` has posted a `hold-off` naming `biome.rs
classify_land` and intends an **epoch**: a new astronomy seed label plus
stream-order slot, changes to `temperature.rs`'s latitude profile, and terrain
hypsometry, to move census median land temperature off −11.99 °C.

The corpus is a frozen hand-authored enum and is immune. Every measurement over
**real cells** is not: a world at −11.99 °C samples a cold corner of the
Whittaker diagram, so a collinearity or occupancy figure taken now is taken on a
badly-sampled population and would be re-derived post-epoch anyway. World-side
arms are preregistered here and **run after** The Glasshouse lands or is
declined.

Answered on the board at `17959dd1`, which also passed The Glasshouse the
finding that §3.2's ten moisture and three temperature thresholds are literals
authored against the cold world: re-centring without re-deriving them classifies
the new climate with the old world's tree.

## 6. Preregistered predictions

**Written as `#[test]`, not as prose.** This project's standing discipline is
that a guard must be proven to fire; prose cannot fire. A preregistration over
the committed axis table runs every gate forever and reddens the day a later
campaign quietly adds a seventh axis.

### 6.1 P-1 — the decomposition is compressive (a ceiling, not just a floor)

§7's stated criterion is that no two names collide on one vector and no name
resists assignment. **Both are satisfied perfectly by one axis with 21 values —
which is the enum.** Any corpus is trivially reconstructible by a sufficiently
fine axis set, so "it reconstructs" is not evidence for a decomposition. The
criterion as §7 wrote it cannot fail.

Bounds, derived from **corpus size (74), not from the fit's outcome**, and
therefore auditable as un-tuned:

| clause | requirement | derivation |
|---|---|---|
| coverage | product of axis cardinalities **≥ 500** | ≥ 74 is necessary to distinguish the corpus; 500 is ≈7× headroom, so the space is *generative* rather than exactly-enumerating |
| compression | sum of introduced axis cardinalities **≤ 30** | the enum costs 74 symbols; 30 is under 41% of that |
| collisions | zero distinct names share a vector | §7's original floor, kept |

`Stratum` is excluded from the compression sum: The Fathom shipped it and this
campaign consumes rather than mints it. It is included in the coverage product,
because the generativity claim is about the whole space.

**Stop rule.** P-1 and coverage may be **jointly unsatisfiable**. That is a
*finding*, not a failure: it would say the 21 names really are 21 atoms and the
corpus is not compositional. It ships as the headline, in this project's own
form (The Tumult's falsified power law, The Mire's double falsification). It is
explicitly **not** grounds for loosening a bound after unblinding.

### 6.2 P-2 — the resisters are the ten phase-`Variant`s, named in advance

"A name resists assignment" is otherwise a subjective call by an unblinded
author. Two things make it objective:

**Operational definition.** A name resists iff assigning it requires an axis
value no other name uses — i.e. it forces a cardinality increase that buys
exactly one name.

**Named prediction, frozen before the fit.** The resisters will be the ten
phase-`Variant`s of §3.5: `ForestGap`, `MossyDeadfall`, `Burn`, `FireScrub`,
`ReefRubble`, `UrchinBarren`, `PressureRidge`, `IceLead`, `RaftedFloe`,
`MeltPond`. If a materially different set resists, the model of "phases are not
points in a state space" is wrong and the campaign says so.

### 6.3 P-3 — factoring on land, open elsewhere

Stated at the strength the measurement supports:

- **Land: factoring, derived.** §3.2. No measurement is run; running one would
  confirm an identity. The spec states this as derived and does not claim the
  stronger result.
- **Marine and cave: measured**, as the count of contrastive features surviving
  redundancy rules. World-side, so gated on §5.5.

### 6.4 P-4 — axis-space distance reproduces the affinity ladder

§3.4's ladder is an independent, already-committed, hand-authored metric on this
space: one band out ≈ 0.70, two bands out *or one step of form* ≈ 0.45. If the
axes are right, that band structure should be recoverable as a distance in axis
space. If it is not, either the axes are wrong or the hand-authored ladder is,
and the campaign reports which it concludes and why.

### 6.5 The four held-out arms, with differentiated stop rules

The Fathom retrospective §2's lesson applied ahead of time: clauses that would
mean different things if falsified need different stop rules.

| arm | corpus | failure means | stop rule |
|---|---|---|---|
| 1 | the 53 `Variant`s | axes too coarse at fine grain | carry forward, refine; **not** a halt |
| 2 | marine formations | the keystone is false — not one space across realms | **HALT** |
| 3 | `MicroField`'s four room axes | keystone false across grain | HALT, or re-scope to cell-only |
| 4 | campaign 2's underworld communities | unevaluable until campaign 2 runs | recorded forward prediction, not a gate |

Arms 2 and 3 are the program's own keystone — *"the same space at every grain
and in every realm"* — measured rather than asserted. Nothing in the metaplan
proposed measuring it. Arm 3 is nearly free: F-8 records three of `MicroField`'s
four axes as ungrounded noise draws.

## 7. Acceptance criteria

1. **`variant_pool` dispatch is byte-identical** under the unbundled vector,
   across every `(formation, stratum, ground)` triple its pools cover. This is
   the expand-contract before-arm and the primary check.
2. **`tolerance_liebig` is unchanged**, and the unfloored-axis invariant (§4.3)
   is asserted by a test that has been **proven to fire** by mutation, not
   merely observed to pass.
3. **All three copies** of the Liebig minimum (§3.6) are shown unaffected.
4. The preregistration of §6 exists as tests, and each is proven to fire.
5. `make gate` green; `make rebaseline` produces an empty diff across the paths
   declared in **`docs/generated-paths.txt`** (the single source of truth since
   The Sexton — never a restated list). `docs/audits/` is among them and is the
   common miss: the type-audit report drifts on any pub-boundary change, and
   this campaign adds `pub` items to the kernel, so it is regenerated in the
   same commit that introduces them.
6. No new registered concept (§5.2), verified by an unchanged concept roster.
7. **The basis's append-only guard exists in the same commit as the basis**
   (§5.4), in `the_basis_ids_are_append_only`'s idiom, and is proven to fire by
   reordering two axes and watching the specific assertion go red.

## 8. Risks

- **The fit deadlocks against P-1.** Mitigated by §6.1's stop rule making that
  a publishable finding rather than a blocked campaign.
- **The Glasshouse lands mid-campaign.** Mitigated by §5.5 — world-side arms are
  not run until it settles, so nothing has to be re-derived.
- **Scope creep into The Tense.** `tolerance_tiered` is in shadow and its §3.3
  successor is specified-and-unbuilt; §4.3's additive design is what keeps this
  campaign out of it.
- **The three-copy drift** (§3.6) — a change believed local is not. Mitigated by
  acceptance criterion 3.
- **Arm 3 pulls the campaign into The Grain's territory.** Mitigated by keeping
  it a *measurement* of `MicroField`'s existing axes, not a grounding of them.

## 9. Definition of done

Implementation, plus this project's standing close: a chronicle entry
(`book/src/chronicle/the-axes.md`), a freshness sweep of stale chapters,
re-scoring any Confidence Gradient bet this campaign moves (decision 0030), and
a one-page retrospective (decision 0020).

**The Fathom's F-12 is discharged by The Sexton and needs no step here.** F-12
asked for a `make ci` re-record whenever a campaign adds or removes tests,
noting that nothing in the gate or the close routes you to it. `make ci` is now
an **alias for `make gate`** (`Makefile:195`), and the timing alarm and baseline
recorder run inside the gate itself — so the re-record happens on every gate
run rather than needing its own step. Recorded here rather than silently
dropped, because this spec's first draft carried F-12's step and it would have
read as a missing action to whoever ran the close.

Corrections this campaign owes to documents it does not otherwise touch, per
The Fathom retrospective §0b — *grep for the claim, not for the file*:

- metaplan §7's collinearity sentence (§3.2 here supersedes it)
- retrospective F-2's "already computed and unused" (§3.8 here)

## 10. Decisions promoted from the ledger

Seventeen entries in `.superpowers/sdd/decision-ledger.md`, five ideonomy
passes, two overturns (pass 1 reframed the gate question; pass 2 reversed the
`tolerance_liebig` decision from re-key to freeze). The load-bearing ones are
promoted above: §4.4 (#1, #7), §6.1 (#2), §3.2 (#3), §5.5 (#4), §4.3 (#6),
§6.5 (#8), §4.1 (#9), §4.1.1 (#10), §6.2 (#11), §7.1 (#12), §6.4 (#13),
§4.2 (#14), §3.7 (#15), §5.2 (#16), §6.1 stop rule (#17).
