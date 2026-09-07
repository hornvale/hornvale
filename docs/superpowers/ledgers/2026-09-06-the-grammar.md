# The Grammar — decision ledger

Campaign: **The Grammar** — BIO-3 reproductive architecture and the SOC-2
social derivations it enables. Branch `campaign/the-grammar`. Autopilot is
engaged; G3 spec review and G6 close remain hard stops.

#1 [G1] — **What shape should BIO-3/SOC-2 take?** · **Decision:** use an
operations-first grammar. Detailed body plans expose a narrow reproductive-
affordance interface; BIO-3 composes those affordances into reproductive
profiles and population outputs; SOC-2 consumes those outputs and derives
social arrangements. · **Why:** this preserves ordinary biology as the
baseline while supporting hybrids, sequential roles, communal reproduction,
manufactured life, host conversion, and future magic without species-specific
branches. · **Alternatives discarded:** species-first profiles risk a list of
exceptions; population-first modelling hides the causal body-level mechanism.
· **Ideonomy passes / overturns:** two passes (cross-domain re-instantiation /
abstraction-lift and tree-finding / graph); no recommendation overturn, with
the material sharpening that compatibility should be a graph relation and
social meaning must not be attached to biological type. · **Capture:** the
affordance, grammar, compatibility, substrate/projection, and probe rules are
in the G3 spec.

#2 [G2] — **Where does the campaign stop?** · **Decision:** specify the
BIO-3 substrate and the stable SOC-2 handoff, but leave the full household and
institution implementation to a successor stage. Magical sex/reproductive-
state transition is a future supported seam, not an implementation target. ·
**Why:** The Murrain's two-tier population design makes the substrate boundary
load-bearing, while household work is explicitly downstream of BIO-3/SOC-2.
· **Alternatives discarded:** implement households now (couples the campaign
to unresolved reproductive semantics); implement magic now (imports an
unsettled operation vocabulary and access model). · **Ideonomy passes /
overturns:** covered by #1's two passes; no separate pass, no overturn. ·
**Capture:** scope, non-goals, transition seam, and staged implementation
sequence are in the G3 spec.

## Follow-ups

- Choose the exact probe data shape during G3 review; do not assign new
  reproductive canon to existing peoples merely to populate the panel.
- Keep hybrid compatibility as a relation and test it before deciding whether
  the world needs a genus taxonomy.
- Revisit the future magic transition contract only when the magic campaign
  begins.

## Task 5 — BIO-3 probe panel frozen; SOC-2 handoff finalized

**Decision.** Freeze the eight synthetic cases exactly as Pairborn, Turning,
Broodweave, Budded, Forged, Guestborn, Crossing, and the non-reproducing
control, with the expected operations, development site, support topology,
transition prerequisite, and directional compatibility asserted in
`domains/species/tests/suite/reproduction.rs`. These remain test-only names:
the test proves that none is a row in `biosphere_registry`. No existing
species profile, society vector, prejudice default, or household rule was
authored to make the panel pass.

**Exact public BIO-3 types.** `hornvale_species` exposes
`ReproductiveOperation`, `DevelopmentSite`, `SupportMode`, `ReproductiveRole`,
`TransitionCapability`, `ReproductiveAffordances`, `GuardStatus`,
`DevelopmentalTiming`, `AssistanceCapability`, `MaterialCompatibility`,
`CompatibilityRule`, `CompatibilityContext`, `ReproductivePathway`,
`ReproductiveProfile<T>`, `CompatibilityOutcome`,
`DirectionalCompatibility`, and `CompatibilityRelation`, evaluated by
`possible_pathways` and `compatibility`. Possibility, typicality, and
realization remain separate: the species grammar evaluates possibility,
`ReproductiveProfile<T>` carries uninterpreted caller-owned typicality, and no
realization type or random draw exists in this domain.

**Exact public population-handoff types.** `hornvale_demography` exposes
`OffspringDistribution`, `IndependenceOutcome`, `SurvivalDistribution`,
`CareBurdenDistribution`, its own species-independent `ReproductiveRole`,
`RoleAvailabilityDistribution`, `HybridOutcome`,
`HybridOutcomeDistribution`, `ReproductivePossibility`,
`ReproductiveTypicality`, `PopulationPersistenceInputs`,
`ReproductivePopulationInput`, `ReproductivePopulationSummary`, and
`SocialSubstrateInput`, with `summarize_reproduction` and
`social_substrate_input` as the pure handoff operations. `hornvale_worldgen`
owns the sibling-domain conversion through `HybridPartnerConfig`,
`ReproductivePopulationConfig`, `ReproductiveSubstrate`, and
`reproductive_substrate_from`; neither domain imports the other.

**Measurement ruling.** The frozen population labels are birth-intensity
inputs, generation length, offspring distribution, survival to independence,
dependency duration, care burden, care topology, role distribution, hybrid
outcome, persistence, and handoff stability. Care topology remains the
species pathway's structural `SupportMode`; the present demography summary
measures care burden and does not flatten topology into a scalar. A cross-seed
test deliberately varies selection keys while calling the seedless pure
summary API, proving structural stability without adding realization
randomness.

**Deliberate plan boundary and SOC-2 successor questions.** The current
`SocialSubstrateInput` contains the aggregate reproductive summary only. The
successor must decide the public shapes for offspring pathway, dependency
profile, care topology, descent relation, compatibility relation, and
transition history rather than smuggling them into household defaults. It
must also decide which of descent and transition history are aggregate
relations versus realized cohort records, how lifecycle states transmit and
decay historically, and where contact combines with subsistence, property,
mobility, population pressure, authority, and religion. Its operation
vocabulary may include `recognize`, `associate`, `bind`, `care`, `assign`,
`inherit`, `adopt`, `exchange`, `exclude`, and `dissolve`; none may turn BIO-3
possibility into typicality, realized biography, prejudice, or social destiny.
No deviation from the approved scope added household, institution, magic, or
existing-species behavior.

## Close backfill — post-G3 rulings and deferred findings

The close sweep found the following six post-G3 rulings and findings only in
the git-ignored campaign `progress.md`. They should have been written to this
committed ledger when they occurred. That is a contemporaneous-ledger
discipline miss; these entries backfill the durable record without erasing or
rewriting the entries above. No separate ideonomy pass was run for these
execution-time rulings.

#3 [G4, backfill] — **Where should the demography handoff tests live?** ·
**Decision:** create the repository's missing consolidated integration-test
crate explicitly rather than treating it as optional. · **Why:** the plan
named `domains/demography/tests/suite.rs`, but no such suite existed; making
the layout explicit was a plan clarification, not a scope change. ·
**Alternative discarded:** retain an optional test location and leave the
implementer to invent the command shape. · **Outcome / location:** the
corrected layout is recorded in
`docs/superpowers/plans/2026-09-06-the-grammar.md`, and shipped as
`domains/demography/tests/suite.rs` plus
`domains/demography/tests/suite/reproductive.rs`.

#4 [G4, backfill] — **How should the plan expose bounded work to the SDD
extractor?** · **Decision:** rename all five `Stage N` headings to `Task N`. ·
**Why:** the task-brief extractor keys on task headings; the change affects
process labels only. · **Alternative discarded:** keep the stage labels and
require manual brief boundaries. · **Outcome / location:** the five task
headings are durable in
`docs/superpowers/plans/2026-09-06-the-grammar.md`; no implementation scope
changed.

#5 [G5, backfill] — **Did Task 2's remaining coverage permutations block the
grammar?** · **Decision:** defer independent missing-role guard cases and
additional successful/assisted combinations for mixed assistance precedence;
the required behavior was covered and the reviewer approved it. · **Why:** the
remaining cases add coverage breadth without closing a known behavioral gap. ·
**Alternative discarded:** expand the permutation matrix in this campaign. ·
**Outcome / location:** accepted as-is at close. Existing coverage remains in
`missing_body_roles_cannot_be_supplied_by_a_context_flag` and
`assistance_is_required_observable_and_never_repairs_missing_body_operations`
in `domains/species/tests/suite/reproduction.rs`; the outcome is recorded in
`docs/retrospectives/the-grammar.md` under “Deferred minors and follow-up
outcomes.”

#6 [G5, backfill] — **How long does Task 4's zero-drift witness remain
sufficient?** · **Decision:** accept the independent adapter/non-interference
proof while the adapter is inert, and require a stronger witness when a future
bake option consumes the substrate. · **Why:** today's test proves today's
boundary but cannot prove non-interference after activation. · **Alternative
discarded:** claim the inert-adapter test covers future live integration. ·
**Outcome / location:** the present witness is
`explicit_reproductive_resolution_is_inert_for_current_world_builds` in
`windows/worldgen/tests/suite/reproductive.rs`; the activation condition is
carried forward in the `BIO-3` row of
`book/src/frontier/idea-registry.md` and recorded in the retrospective's
deferred-outcomes section.

#7 [G5, backfill] — **What does Task 5's repeatability test actually prove?**
· **Decision:** name the result seedless structural repeatability and reserve
real seed variation for the future realization boundary. · **Why:** the pure
summary API consumes no seed and performs no draw. · **Alternative discarded:**
retain the stronger “cross-seed” claim based on seed-like metadata around the
call. · **Outcome / location:** the shipped test is
`seedless_structural_handoff_repeats_without_realization_draws` in
`domains/demography/tests/suite/reproductive.rs`; future seed variation remains
in the `BIO-3` row of `book/src/frontier/idea-registry.md`, with the correction
explained in `docs/retrospectives/the-grammar.md`.

#8 [G5, backfill] — **May ordinary reproduction require a hybrid-outcome
measurement?** · **Decision:** no; hybrid applicability is independent from
ordinary reproductive possibility. The branch could not enter the stage gate
or merge with the regression present. · **Why:** the whole-branch fix wave's
universal hybrid-distribution requirement rejected an ordinary reproductive
input with no hybrid partners. · **Alternative discarded:** infer hybrid
applicability from any nonzero pathway count. · **Outcome / location:** fixed
in `61401a616` by the explicit `hybrid_applicable` fact, with
`ordinary_reproduction_without_hybrid_partners_summarizes_successfully` in
`windows/worldgen/tests/suite/reproductive.rs` and the corresponding
non-reproducing/inapplicable coverage in
`domains/demography/tests/suite/reproductive.rs`; the retrospective records
the defect and correction as its lead lesson.
