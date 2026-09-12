# Astronomy Deepening — Decision Ledger

## #1 [G1] — Physical astronomy direction

Question: what should make Hornvale's astronomy the best version of itself?

Decision: prioritize a coherent physical substrate for human historical time,
with observation and culture as downstream layers. Use a thin hierarchical
Keplerian model; do not pursue N-body simulation.

Why: the project needs causal reuse across bodies, events, observers, and
cultural readouts, while N-body work is outside the simulation's needs.

Alternatives discarded: independent phenomenon-specific formulas (cheap but
contradictory); unrestricted N-body simulation (cost without relevant payoff).

Ideonomy passes / overturns: 1 / 0.

Capture actions: formal design spec; future idea-registry follow-ups for
deferred secular dynamics and eclipsing binaries.

## #2 [Q] — What small-body family should come first?

Question: event-only phenomena or persistent small bodies?

Decision: persistent comets with derived activity and debris streams; exclude
asteroids and impacts from the initial family.

Why: persistent identity supports returns, memory, meteor showers, and causal
history without requiring mutable world state.

Alternatives discarded: event-only apparitions (weaker identity); asteroid
population (no current naked-eye or impact requirement).

Ideonomy passes / overturns: 1 / 0.

Capture actions: comet section of the design spec; deferred outburst and
fragmentation follow-ups.

## #3 [Q] — How should stars and constellations be modeled?

Question: universal reference figures or culturally derived naked-eye skies?

Decision: physically model a modest catalog, generate the background lazily,
filter through species traits and observer geometry, and let cultures create
constellations.

Why: this preserves the domain boundary and makes pattern-matching and naming
the interesting cultural behavior.

Alternatives discarded: universal constellation truth; telescope-only
constellations; storing thousands of distant stars.

Ideonomy passes / overturns: 1 / 0.

Capture actions: catalog and observer sections of the design spec; figure
member-ID and brightness recalibration are sequencing prerequisites.

## #4 [Task 5] — Model the catalog without prematurely expanding figure input

The spec requires figure identity/brightness migration before catalog
expansion reaches that reader; Task 6 owns that migration and is explicitly
outside this task. `StarSystem::neighbor_catalog` therefore stores 24–40
physical `CatalogStar` records in identity-assignment order, while `neighbors`
remains the original 2–5-star compatibility projection, brightest first.
The old `Neighbor` struct shape, descriptions, fact predicates and ledger
lineages remain compatible. The catalog IDs are distinct from ledger entities
whose historical ordinals follow brightness rank.

The legacy draws consume exactly their previous streams. Their luminosity
anchors condition inverse physical tracks, preserving the original observed
values, including their last bits. Additional stars draw continuous mass and
age on dedicated catalog streams. Both populations derive class/color from
physical phase and temperature. The six legacy class names remain coarse:
hot main-sequence stars project to `BlueGiant`, while their physical stage
remains explicit. The host `Star` and its generation are unchanged.

The neighbor track is a declared approximation: the existing main-sequence
scalings, a short giant phase, and cooling white dwarfs. Massive-star terminal
eligibility derives from birth mass and age inside a requested historical
window; no transient execution, visibility, hazard, or host evolution is added.
Generated artifacts affected here are the stream manifest and audit reports.
