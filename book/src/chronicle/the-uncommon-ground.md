# The Uncommon Ground

**July 2026 · outcome: merged — a room stops being only its climate biome:
`windows/locale` gets a natural-tier strangeness overlay, the first slice of
the room-scale variety arc**

## What was attempted

The Locale Window gave a room a real biome and a set of blended continuous
fields, and left one deliberate placeholder in its wake: `SubCellTexture`, a
handful of biome-keyed phrases and a bounded jitter, explicitly capped at
four entries so it would not pre-empt the larger room-scale variety design.
This campaign fills that placeholder with the first working slice of the
biome bestiary the room-scale corpus sketched — not the whole ~70-entry
bestiary, but its engine, proven over the natural strangeness rungs only
(0–30 on a 0–100 scale, everything in this slice **steady**, meaning a pure
function of address with no clock and no state).

The goal was to defeat what the design brief called "1,000 bowls of
oatmeal" — an Earth-sized world where the categorical biome a room inherits
is honest but monotonous, the same "temperate forest" tag standing over
mile after mile of genuinely different ground. The answer is an **overlay**,
never a replacement: a room keeps the biome climate already gave it, and
additionally carries a `Regime` — a small, coherent bundle of departures from
that biome's defaults, a rendered descriptor, and a derived strangeness
magnitude that sets the prose register. Layering stays intact (decision
0038): biome is still mesh-bound categorical truth, inherited at maximum
weight, never re-quantized by this new layer.

## The negation-slot engine

The room-scale corpus's design catalogued dozens of ways a room could be
strange — different substrate, different energy source, different dominant
kingdom of life, isolated biota — and the naive move is a flat catalogue: enumerate
every combination as its own bestiary entry. That does not compose, and it
does not stay coherent (nothing stops a generator from drawing "sand" and
"glass" for the same sea). The design instead treats strangeness as a
**negation vector over exclusion slots**: one draw per slot, so composites
are coherent by construction.

```rust
struct Negations {
    substrate: Substrate,   // pick-one: Ordinary | Sand | Evaporite | Basaltic | Ashen
    energy:    EnergySource,// pick-one: Sunlit | Chemosynthetic | Geothermal
    kingdom:   Kingdom,     // pick-one: PlantAnimal | Fungal | Crystalline | Microbial
    endemic:   bool,        // isolation modifier, gated on a derivable signal
}
```

Each slot's mundane value is its identity element — a `Negations` where every
slot holds its default *is* strangeness zero, not a special case checked
separately. `strangeness` itself is not stored as an independent number; it
is **derived**, the maximum departure magnitude across the vector (plus a
small capped bonus for `endemic`), so a basaltic substrate next to a
chemosynthetic energy source reads as exotic — the ceiling of its most
extreme slot — rather than as the sum of two smaller departures stacking
into a rung the natural tier was never meant to reach. Two rooms at the same
strangeness magnitude can be strange in genuinely different ways, which is
more honest variety than a single scalar dial would give.

The one new workspace-wide contract this needed sits at the kernel, not in
`windows/locale`: `Stream::weighted_index(weights: &[f64]) -> Option<usize>`,
a cumulative-weight draw beside the existing uniform `pick`. Its
draw-semantics — weights consumed in slice order, a single `next_f64` draw
against the total, first cumulative bucket that exceeds the threshold wins —
are now a frozen save-format contract, the sole piece of this campaign that
lives below the window layer.

## Derived tier — free, and the answer to "miles and miles of forest"

Rung 15 costs nothing: it is conditioned purely on fields a room already
inherits (biome, temperature, moisture, elevation, relief) plus one addition,
a **substrate proxy** standing in for the lithosphere domain the project has
not built yet. The proxy is deliberately conservative — it infers
only the distinctions the existing signals can honestly defend (aridity plus
low relief reads as evaporite; tectonic or volcanic proximity reads as
basaltic or ashen; arid and coastal reads as sand; everything else is
ordinary) and defers everything it cannot earn — true rock petrology,
crystal, bone — to whenever a real lithosphere domain lands, behind a single
`substrate_at` function no consumer needs to know about when that swap
happens.

But fields and a substrate proxy alone still classify at cell granularity,
and most of a large world *is* cell-homogeneous — the actual "oatmeal"
problem is not thin biome coverage, it is that a hundred adjacent rooms
inside one biome cell have nothing to distinguish them. The campaign's
answer is a small **`MicroField`**: four grounded, continuous axes — relief,
aspect (slope facing / insolation), wetness, and canopy openness — sampled
from a room's own address noise, steady and quantized at emit like every
other room-scale value. The descriptor grammar reads this field before
anything else, so two adjacent rooms in the same forest cell differ by a
damp north-facing hollow versus a sun-warmed open rise rather than by a
random phrase pulled from a pool. This is the in-campaign lever for
within-biome interest, and it is explicitly not the last word on it — the
deeper levers (a room's history, halos radiating from a placed exotic,
successional change over time) stay named and deferred.

## Placed tier — the rarity budget and the founder floor

Rung 30 is the genuinely exotic end — chemosynthetic vents, geothermal
seeps, non-plant kingdoms, endemic isolates — and it is *not* free: minting
it everywhere would make the exotic wallpaper, so it is placed by a bounded
`StrangenessBudget` genesis pass over the canonical grid, computed once
per world and cached.

The pass is a seeded blue-noise placement: walk a Fisher–Yates permutation of
eligible land cells, accept a cell as a strange site if it clears an integer
graph-distance from every already-accepted site and the budget's remaining
mass (a target of roughly 1% of land cells) allows it, then draw its exotic
slot-values by a field-weighted `weighted_index` call so a chemosynthetic
negation is likelier near a tectonic boundary — strangeness clusters where
the geology actually warrants it, reading as earned rather than sprinkled.
Spacing is measured the same integer way as everything else in this
campaign's placement path: a new kernel `Geosphere::hops_between(a, b, max)`,
a bounded breadth-first search over the mesh's own neighbour graph, so no
transcendental ever enters a placement accept/reject decision (the class of
comparison that split Linux from macOS before the libm fix — decision 0041 —
made this an explicit both-platform guard here).

Pure probabilistic dart-throwing has one honest failure mode: a world with a
strongly volcanic province could, by unlucky draw, mint *zero* geothermal
vents, even though its geology all but demands one. Before the competitive
placement runs, the budget reserves **each strongly-warranted regime's
single most-eligible cell** — a founder floor, the same concept already
proven for settling peoples onto their best land before competitive
placement squeezes them out. It turns "the exotic feels earned by the land"
from a probability into a guarantee: a world that geologically implies a
vent will mint one, every time, on every platform.

Placement is deliberately **repulsive** — mutual spacing between sites, no
contiguous multi-cell provinces. That asymmetry is named on purpose: a
future negative wing (curse, blight, undeath) will need the opposite,
*excitatory* spread from a source, and building repulsion as the only shape
here would have made that later addition an unwind rather than an addition.
The `endemic` modifier is not a free toggle either — it only turns on where
a derivable isolation signal (the terrain domain's existing `endorheic`
field, an enclosed drainage basin) says the ground is actually cut off.

## Findability without a save-format cost

The budget's real byproduct is not just per-room prose — it is a map of
where the strange things are, which is exactly the kind of thing a future
scout or quest hook wants to query. Per the project's derive-don't-store
discipline, that map is not written to the save at all: `LocaleContext`
exposes `strange_sites() -> Vec<StrangeSite>`, a query that re-derives from
the seed on every call. A `hornvale locale --sample N` CLI mode renders N
rooms spread across biomes and both tiers as a readout, backing the
drift-checked `book/src/reference/locale-seed-42.json` artifact — the
schema bump from `locale/room/v1` to `locale/room/v2` (the emitted shape
changed, `SubCellTexture` replaced by the real `Regime`) is deliberate
regeneration, not a silent rename.

## What this slice deliberately is not

Everything the room-scale corpus named and this campaign did not touch has
a home already picked, not a gap. A room's history — scars, blooms, an
ecosystem recovering out of equilibrium — is a *time* property and belongs
to a future deep-time palimpsest campaign, not a steady overlay. Ephemeral
and periodic variety waits on a temporal phase clock (a future campaign of
its own). The engineered rung needs an actual maintainer — a civilization
or a wizard — and belongs beside settlement and demography. The aetheric and
faerie rungs, and the curse/blight/undeath negative wing, stay behind a
metaphysics pin that does not exist yet; building them without that gate
would mean magic leaking into the natural tier by accident. The radial
core-to-margin gradient and contagious spread around a placed site are the
halo primitive, reserved for a future campaign built on the excitatory
placement this campaign's repulsive budget is explicitly labeled against.
True lithology — crystal, bone, real rock petrology — stays with a future
lithosphere domain, never faked from a room's address. Naming these seams
now is
what lets the next campaign that picks one up inherit it cleanly instead of
re-deriving where it belongs.

## The road ahead

The room-scale variety arc now has a proven engine — the negation-slot
model, the derived/placed split, the founder floor, and the micro-field —
exercised over real terrain and climate data rather than left as a design
sketch. The natural tier's ~70-entry bestiary is still mostly unbuilt; what
exists is the machine it will run on, validated to compose coherently, place
deterministically across platforms, and stay honest about what it does and
does not yet know how to infer. The next campaigns that extend it —
palimpsest, halos, the metaphysics-gated wings — inherit a working substrate
instead of an open question.
