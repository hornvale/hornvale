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
