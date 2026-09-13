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

## #5 [Task 6] — Lazy physical star candidates and figure identities

Background queries generate only requested equal-area cells on the dedicated
`starfield/cells/v2` stream. The original label is retained. Identities carry
the astronomy seed, fixed cell, and draw ordinal before filtering. Reordered,
overlapping, repeated, and differently filtered queries preserve the same
stars without a stored roster or mutable cache.

Figures now consume modeled catalog IDs and background IDs with one shared
inclusive apparent-magnitude cut. The physical reference limit remains four;
observer queries cap at six and may apply an equatorial zenith/horizon.
Catalog flux uses the declared first-slice bolometric-as-visual approximation
with solar magnitude 4.83 at ten parsecs. Existing `FieldStar`, full-sky
`starfield`, and index-based `NightSky` readers remain source-compatible;
`starfield` retains the original sequential stream and byte-identical
`scene/neighbors/v1` output (including seed 42's 148 background stars), while
figures use the new physical queries. The two neighbor-observation regressions
exposed and now guard this compatibility boundary; their fixtures are unchanged.
`catalog_stars_at` exposes modeled IDs at an explicit epoch. Species and
culture behavior remain outside this task.

The population change is intentional, not a byte-identity claim. The red
regression printed seed 42's former two figures (three members each), unchanged
when modeled stars were made invisible. A temporary astronomy example after
the repair measured four figures with 6, 3, 3, 3 members: a loose northern
knot, tight equatorial knot, tight southern knot, and loose northern chain.
The probe was removed after recording its output. Census regeneration remains
the campaign-close canonical-box step, not a local Task 6 operation.

Verification: seven new behavioral regressions were observed red before their
implementations. `cargo test -p hornvale-astronomy` passes 339 unit and 56
integration tests, including the pre-retrofit genesis golden and pin isolation.
Astronomy clippy, formatting, type/placement/plumb checks pass; the stream
manifest and affected audit reports were regenerated.

## #6 [Task 8] — Culture-owned constellation seam

Culture owns a small deterministic `SkyCandidate` input and
`ConstellationCulture` grouping/name/meaning output. It has no astronomy
dependency: the worldgen composition root adapts astronomy's filtered
`SkyStar` records into opaque stable IDs plus position and brightness. Empty
candidate sets are valid, and two grouping rules demonstrate that cultures can
interpret the same physical sky differently. The four follow-ups—eclipsing
binaries, transient stellar events, dense meteor-stream clumps, and terminal
stellar evolution—are documented as deferred and are not inputs here.

Verification: culture, worldgen, and astronomy integration tests pass;
workspace clippy and formatting pass. The commit gate remains to be run after
the final staged change.
