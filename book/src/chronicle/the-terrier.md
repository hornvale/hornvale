# The Terrier

*A terrier is an estate's register of who holds which land — walked once,
consulted many times. Hornvale keeps one too: every occupation the history
bake ever committed, grouped by the vertex it stands on. The vessel had been
re-surveying the whole estate every time it asked who held one plot.*

## The wrong noun

[The Rack](./the-rack.md) had made a chamber turn's per-turn sighting memo
legible enough to measure, and what it measured was a difference: a chamber
snapshot cost 8.4 ms after `look`, where the sighting memo already had an
answer to share, and 16.3–16.8 ms after `map` or a chamber `go`, where it did
not and had to derive one. Eight milliseconds, attributed to the thing that
difference seemed to name — one shadowcast — and written into a chronicle, a
retrospective, and a registry row as exactly that.

It was the whole derivation, not one step of it, and the step it was named
after was not the expensive one.

Ten scratch `Instant` prints inside `derive_sighting`, `chamber_plan`,
`describe_chamber_here`, `enter` and `brief_of`, taken on the same release
build, the same seed-42 flagship, and a visibly contended box (`uptime` load
averages 28.07/25.58/22.66 on the first run, 51.01/34.28/26.41 on the second
— every figure below is an upper bound, and the *split* between them is the
finding, not their absolute size), decomposed the difference completely:

```text
step inside derive_sighting                       per call
  chamber_interior_here  (= one brief_here)     8.7 – 12.6 ms
  anchor_cells                                  0.086 – 0.091 ms
  shadowcast (SIGHT_RADIUS = 4, ≤ 81 cells)     0.011 – 0.013 ms
  occupancy seat + interior_of                  0.005 – 0.006 ms
  placement loop + furnishings                  0.009 – 0.013 ms

step inside chamber_plan
  fabric_here                                   0.003 – 0.004 ms
  chamber_sources        (= one brief_here)     8.8 – 17.6 ms
  light_field                                   0.060 – 0.896 ms
  plan_of (+ ambient)                           0.119 – 1.023 ms

inside brief_of                     n = 42 calls in the run
  is_built                                      mean 0.000 ms
  is_cold                                       mean 0.003 ms
  containing_vertex                             mean 0.003 ms
  occupations_by_vertex (452 vertices)          mean 11.421 ms, max 26.048 ms
```

The shadowcast is a hundredth of a millisecond. Every other row worth naming
is `brief_here`, and every `brief_here` bottoms out in
`hornvale_worldgen::occupations_by_vertex(world)` — a function that
reconstructs **every occupation the history bake ever committed**, from a
`find(IS_OCCUPATION)` scan of the ledger plus a `value_of` read and a string
parse per record, then throws away every entry but the one vertex the caller
asked about. `Session::brief_here` has five static call sites, one of them
(`chamber_interior_here`) with seventeen callers of its own; dynamically, an
`enter` from out of doors makes four calls to it in `handle` and one more in
`snapshot`, a chamber `look` makes two and one, and a chamber `map` or `go`
makes zero and two. The function's own comment had prescribed the fix since
the day it was written, 2026-07-27, `4569d883d`: *"If a profile shows it
mattering, hoist the map to the caller… do NOT memoize inside this
function."* A profile finally showed it mattering, five weeks later, wearing
the wrong name.

## The register, kept once

The fix is the hoist the comment already asked for, placed where every other
world-scoped, derived-once, session-shared read already lives:
`WorldContext` (The Quire) gains one field — the occupation register,
`BTreeMap<Vertex, Vec<OccupationRecord>>` — built once at the end of
`WorldContext::build`, after the five seeded derivations that precede it, so
a reader who does not already know cannot mistake a ledger read for a sixth
stream-consuming one. `brief::brief_of` stops taking `&World` and starts
taking `&BTreeMap<Vertex, Vec<OccupationRecord>>`; its one production caller
passes `&session.wctx.occupations`. The whole-world scan moves from
*every call* to *once per world* — roughly nine to twenty-six milliseconds
against a multi-second build, paid exactly once.

Nothing about the *shape* of the fix was in doubt once the decomposition
existed. The find was that the campaign the brief opened — attack the
shadowcast — would have shipped a real, green, and almost entirely useless
0.01 ms improvement. See [decision 0636](../../../docs/decisions/0636-a-world-scoped-derivation-lives-on-worldcontext.md)
for the rule this generalizes to, and [the frontier's idea
registry](../frontier/idea-registry.md) — corrected here rather than
replaced — for the number that misled and the one that replaced it.
