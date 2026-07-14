# The Self-Describing Sky

**July 2026 · outcome: merged — the sky's ledger now carries every number
behind its own derivation, neighbors become queryable entities, and a new
`explain` verb proves the ledger can answer its own "why"**

## What was attempted

The world has always recomputed the numbers behind its sky — a star's mass,
its luminosity, the habitable zone it draws, a moon's distance, a
neighbor's brightness — and thrown them away the moment the almanac's
prose was rendered. Grounding the registry's long-standing "the ledger
drops derived facts" row against the current code found it half stale:
moon period, tide, and inclination already shipped as facts, and climate
already read insolation through `worldgen` rather than recomputing it
blind. What remained was not completeness for its own sake — it was
**interrogability**. Nothing could ask the ledger "why is this world warm?"
because the star's mass and orbit, the very numbers the answer depends on,
were never committed.

This campaign closes that gap and builds the verb that proves it closed.

## The fact surface, finished

Seven new facts land on the world entity: a star's mass and luminosity, its
habitable-zone bounds, the anchor world's mass and orbit, and
`insolation-rel` — the top-of-atmosphere flux at the anchor's orbit,
relative to Earth's, a global annual-mean scalar deliberately silent about
the seasonal and deep-time variation the forcing parameters already model.
Three more land per moon, alongside the shipped period/tide/inclination
triad: mass, distance, and apparent angular size. All of it is additive —
no existing fact moves, no epoch — because every new value is a pure read
of a quantity the world was already computing.

## One insolation, not two

The fact and the climate model used to compute `L / a²` independently, in
two different crates, with nothing stopping the copies from drifting.
`insolation_rel(star, anchor)` now lives once, in the astronomy domain — the
layer that owns the physics — and both the committed fact and
`windows/worldgen`'s climate input call it. A DRY refactor, not a
re-wiring: the function draws nothing, so the composition root's stream
order is untouched, and the two readings can no longer silently diverge.

## Neighbors become entities

The one place the ledger was not merely incomplete but opaque: a single
`notable-neighbor` `Text` blob — "a warm yellow star at 12 light-years" —
with nothing in it a query could reach. Neighbors are redesigned from a
clean slate, mirroring how settlements already work: each neighbor mints
its own entity, carrying an `is-neighbor` flag alongside its class,
distance, brightness, declination, and right ascension as first-class
facts, discovered the same way settlements are — by scanning for the flag.
`notable-neighbor` is retired outright, per the save-format contract's
no-rename rule; nothing rendered drifts, because every existing consumer
(the almanac, the star chart, the orrery, the lab metrics) already read
neighbors from the in-memory `system.neighbors` struct, never from the
fact. What changes is `world.json`'s content, not any prose a reader sees.

This is also the campaign's entity-hood precedent, recorded as a decision:
collections of a kind become entities, singletons stay flat on the world
entity. One star and one anchor stay flat facts; many neighbors become
entities. Moons are grandfathered flat this campaign — a second epoch on
already-shipped facts would have cost more than the asymmetry it leaves
behind, and the debt is named rather than silently accepted.

## `explain sky` — the ledger answers its own question

A new window, `windows/explain`, and a CLI verb: `hornvale explain --world
w.json sky`. Its design turns on one framing — **explain is a derivation
DAG joined against ledger values.** The ledger stores numbers; it does not
store whether a number was rolled, forced by a formula, or pinned by a
scenario. That classification is structural knowledge the narrator alone
holds: `mass → luminosity → zone`, `luminosity, orbit → insolation`, and so
on. `explain` walks that fixed topology, reads each node's value from the
ledger — and nowhere else, no in-memory `System`, no re-derivation — and
prints the join as prose, each figure tagged rolled, derived, or pinned. A
world whose committed facts are missing a node says so by name rather than
quietly recomputing it. That the narrator can be built at all, reading
nothing but committed facts, is the proof the ledger set out this campaign
to earn.

The verb currently narrates the sky only; a wider trace-replay tier —
capturing the individual stream draws down to a tie-breaking roll, not
just their settled values — is future work this campaign seeds but does
not deliver.

## An unplanned epoch: names were coupled to mint order

Minting one entity per neighbor bumps the ledger's monotonic entity-id
counter, and — this was not foreseen when the neighbor epoch was scoped —
deity-name generation in `windows/worldgen` was seeding itself from that
same counter, by passing a belief entity's id straight into the name
generator as salt. Astronomy genesis runs before religion genesis, so five
new neighbor entities shifted every belief id, and every generated deity's
name changed for every seed. The world stayed fully deterministic; the
fragility was that a *name* depended on global mint *order*, a coupling no
one had asked for and the spec had not anticipated.

The belief id was doing two jobs that needed to be pulled apart: keying
the committed name-gloss fact to its belief (correct — that's how
`recount` and `explain` find it later) and seeding the generated name
itself (the bug). The fix re-sources the name seed from the belief's
semantic identity — species, the phenomenon it names, and its rank among
that call's members — never from an entity id, while the gloss fact keeps
keying off the belief id exactly as before. Deity names change once more,
for every seed, under the `religion/deity/v2` epoch label; nothing else
about a world's physics moves. The general principle earned its own
decision record: a procedural name must be salted by something stable
about what it names, never by a global mint counter that a future, wholly
unrelated change can perturb.

## Two ideonomy passes

A first pass (substitution crossed with organon-construction, charting the
fact set by naturalness against ledger shape) is what surfaced the missing
anchor-mass and habitable-zone facts, and the DAG-joined-with-values
framing `explain` was built around. A second pass, later, ran a
periodic-grid method (salt source crossed with invariance) over the
deity-naming bug once it surfaced during execution, and it is what
confirmed a global counter cannot be made invariant by any reordering —
only re-sourcing the salt closes the coupling for good.

## The Confidence Gradient

No bet in the Confidence Gradient chapter names the sky's fact surface or
the `explain` verb, so nothing there was re-scored; this campaign's work is
recorded here in the chronicle instead.

## What this leaves open

The trace-replay tier of `explain` — draws, not just settled values — is
named and deferred, not built. Moons stay flat facts, an explicit debt a
future campaign may promote to entities under its own epoch. Neither
changes what shipped: a world's sky now carries every number its own
almanac prose depends on, and a verb exists that reads nothing but the
ledger to prove it.
