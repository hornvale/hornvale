# The Anchor Orbit

**September 2026 · outcome: qualified — one explicit-time physical anchor
state now feeds the calendar, insolation, eclipse, and scene projections**

## The problem

Hornvale's astronomy already carried an analytic orbit, but several consumers
reconstructed pieces of it independently. The calendar could speak in one
longitude, illumination in another radius, and eclipse and scene geometry in
mean-orbit terms. Each answer was locally plausible; the system as a whole
had no single answer to “where is the anchor at this instant?”

The campaign made that question explicit. `anchor_state_at` evaluates the
existing analytic orbit at a caller-supplied typed instant and returns a
semantic state or a descriptive `OrbitalError`. The solver remains private;
consumers receive position, radius, velocity, frame, and true longitude, then
project those quantities into their own meanings.

## One state, several readings

Calendar seasons and daylight now use the physical true longitude. Instantaneous
illumination uses the same evaluated radius and position. Eclipse alignment and
angular size use that state at the event instant. The scene window converts its
explicit `WorldTime` once and projects the same state into body and moon
geometry. The older mean-phase calendar projection remains only where its
compatibility meaning is still intentional; it is no longer an accidental
second physical orbit.

The boundary is honest about failure. Unsupported or degenerate physical
evaluation returns a typed error, while a valid interval with no eclipse stays
an ordinary empty event result. Negative instants, locked worlds, retrograde
worlds, and angular wraps are all explicit test regimes rather than hidden
special cases.

## Qualification

The closing battery reads the calendar, illumination, eclipse, and scene
surfaces together across eccentric and spin-regime fixtures, compares them to
independent state projections, reverses query order, and perturbs one eclipse
projection to prove the checks can detect semantic drift. Astronomy passed
76/76 tests and scene passed 23/23. The intentional seed-42 artifact movement
is limited to eclipse event geometry and the four almanac lines that describe
those events; unrelated output from an overly broad regeneration was rejected
and reverted.

The anchor slice is now a stable foundation for a later body-family
generalization. Moons, wanderers, comets, binary companions, and deep-time
forcing remain separate decisions rather than being smuggled into this one.
