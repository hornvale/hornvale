# The Wanderers — Decision Ledger

Campaign branch: `campaign/the-wanderers`

## #1 [G1] — What does “flesh out our solar systems” mean?

**Question.** Should this campaign center on the physical simulation, the
orrery instrument, or an observational/cultural solar-system layer?

**Decision.** Combine physical system expansion with observational meaning.
The campaign will add a deterministic planetary roster around the existing
anchor world, then make the first consumer-facing payoff the things a culture
can discover: wandering planets, conjunctions, retrograde loops, calendars,
and knowledge gradients. The anchor world remains the only fully simulated
surface.

**Why.** The Campaign 2 Sky design established anchor-first generation and
the rule that everything else decorates around the habitable anchor. The raw
`ORRERY-sibling-planets` idea already identifies sibling worlds as the missing
system-level extension, while the project constitution keeps the simulation
authoritative and clients as lenses. This combines those precedents instead
of choosing between a richer sim and a meaningful observation layer.

**Alternatives discarded.** A client-only orrery campaign leaves sibling
worlds physically absent. A full-world-first campaign multiplies terrain,
climate, settlement, language, and culture scope before the system can first
be observed. Both remain possible later, but neither is the opening shape.

**Ideonomy passes / overturns.** One convergence pass using tree-finding,
organon-construction, and dimension-identification over a graph and
periodic-grid; 0 overturns. It enriched the initial A+C choice with the
system-to-observation chain and exposed the missing middle layer: a
deterministic planetary roster.

**Capture actions.** The existing `ORRERY-sibling-planets` registry row is
the campaign seed. New speculative extensions will be captured in the idea
registry; actionable follow-ups will be added below as they arise.

## #2 [Q] — How much worldhood does a sibling body receive?

**Question.** Should every sibling planet become a complete Hornvale world,
or should siblings remain astronomy-first bodies in this campaign?

**Decision.** Siblings are astronomy-first bodies: physically coherent
objects with system-level descriptors and orbital behavior, but no
independent terrain, climate, settlement, language, or culture generation.
The data model must leave a clean promotion path for a later campaign.

**Why.** This preserves the anchor-first contract and keeps the first
observational payoff in scope. The existing `scene/system/v1` is already a
semantic orbital contract, and the sibling-planet idea asks for bodies and
orbits rather than a second worldgen tree. Nathan explicitly approved this
fidelity boundary.

**Alternatives discarded.** Full sibling worlds were rejected for this
campaign because the cost is not merely more planets: it creates parallel
climate, geography, settlement, language, and culture substrates and would
turn one campaign into several coupled campaigns.

**Ideonomy passes / overturns.** One pass using combination and cycle with
complexity and hierarchicalness; 0 overturns. It confirmed the useful
composite is “physical roster × observational cycle,” not “many complete
worlds,” and made the promotion boundary an explicit phase transition.

**Capture actions.** Add a follow-up for a later full-sibling-world campaign
and a promotion contract so astronomy-first bodies do not become a dead-end
schema.

## Follow-ups

- Define the minimum astronomy-first sibling descriptor and its promotion
  seam to a future full-world identity.
- Decide which observed phenomena belong in the first campaign versus later
  client-only work, keeping the sim/client boundary explicit.
- Determine whether a widened `scene/system` document is additive or an
  epoch-bearing contract change after measuring all consumers.

## #3 [G2] — What is the first complete delivery?

**Question.** Where should the campaign spend its first implementation
surface now that sibling-body genesis already exists?

**Decision.** Complete the producer-side solar-system instrument in three
stages: phase-aware wanderer ephemerides; derived observational events and
almanac vocabulary; then the additive scene/world-wasm contract and client
handoff documentation. Positions remain evaluated from elements and time,
never sampled into a trajectory artifact.

**Why.** `StarSystem::wanderers` already owns the physical bodies and the
Night Sky spec explicitly deferred orrery/scene rendering. `scene/system/v1`
explicitly permits appended fields, while the client boundary requires the
native scene and wasm paths to share one contract. This completes the missing
layer without reopening genesis or inventing a second client-side physics
source.

**Alternatives discarded.** A client-first implementation is blocked by the
external orrery checkout not being present and would invert the repository's
producer-contract discipline. An orbit-physics-first rewrite would expand
the campaign into eccentricity, inclination, and N-body fidelity before the
existing bodies are even observable through the contract.

**Ideonomy passes / overturns.** One convergence pass using combination and
cycle with complexity and hierarchicalness; 0 overturns. It identified the
useful cycle as genesis → ephemeris → observation → client lens, with the
scene contract as the bridge rather than the endpoint.

**Capture actions.** The spec records elliptical/inclined orbits,
transits/occultations, full sibling-world promotion, and per-species sky
catalogs as deferred branches. Existing registry rows `ORRERY-ellipse-truth`,
`SKY-wanderer-calendar`, `SKY-transits`, and `SKY-figures-per-species` remain
the durable capture points.

## #4 [Q] — Should the campaign generalize the stellar root first?

**Question.** Should solar-system work begin from a singular host star, or
should it establish the basic stellar architecture first, including close and
wide binaries?

**Decision.** Generalize the stellar root first. The campaign will model
single-star systems, wide binaries with a circumprimary anchor, and bounded
close binaries with a circumbinary anchor. Wanderers remain a planetary-layer
feature beneath that topology. Arbitrary N-body systems and close
circumsecondary planets are deferred.

**Why.** A binary is not an extra ornament on `StarSystem::star`; it changes
the gravitational center, insolation source, sky, calendar, and orbit
admission. Treating it as a neighbor-star add-on would bake the wrong root
abstraction into every downstream feature. Nathan approved making this
topology stage part of the campaign.

**Alternatives discarded.** Keeping the singular-star root and adding a
companion later would make close binaries a retrofit. Supporting arbitrary
multiple-star/N-body systems now would turn the campaign into a general
orbital-dynamics project instead of a deterministic world-simulation layer.

**Ideonomy passes / overturns.** One pass using organon-construction with a
scale over system complexity and observability; 0 overturns. It exposed the
missing scale between a visible companion and a true gravitationally coupled
stellar architecture.

**Capture actions.** Add binary topology, S-type/P-type orbit placement, and
multi-source illumination to the spec. Record arbitrary N-body,
close-circumsecondary systems, and full binary cultural consequences as
deferred branches rather than allowing them to disappear into the first
implementation plan.

## G3 review

Nathan approved the revised spec after the binary-topology expansion. The
campaign may proceed to implementation planning; the four flagged items remain
explicit plan and execution checkpoints.

## G4 plan boundary

- **Ruling:** The four-stage plan is ready for execution.
- **Basis:** It covers stellar topology, phase-aware ephemerides, observational events, scene/wasm emission, and closeout artifacts from the approved specification; the self-review found no unresolved placeholder or interface mismatch.
- **Ideonomy passes / overturns:** one plan-coverage pass / 0 overturns.
- **Cost if wrong:** implementation rework at the first task boundary; the task-level review loop remains the correction point.
