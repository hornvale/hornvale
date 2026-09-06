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
