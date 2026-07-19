# The Freshet

**July 2026 · outcome: merged — the world can finally say which water is
drinkable, at zero save-format cost**

## What was attempted

The tectonic globe has computed rivers and interior basins since Placement &
Drainage: an upstream flow-accumulation field (`drainage`) that carves
waterfalls and condenses settlements, and an endorheic mask (`endorheic`)
marking land whose downhill path never reaches the sea. Both fields fed the
land's shape. Neither answered a question every living thing needs answered
first: *is the water here salt or fresh?* "Where's water?" had meant "below
sea level" — the salt ocean — because nothing else was exposed. That
omission was not cosmetic; it is what stranded the parked Surmise's thirsty
agents beside a sea they could see but not drink, with no way to ask for
anything else.

The Freshet closes that gap the same way The Ground closed the rock-and-soil
gap a few campaigns back: a **pure derived classification** over fields the
world already computes, with no new physics, no new stored state, and no
epoch. A `WaterKind` per cell — ocean, salt basin, river, or dry land — with
an `is_fresh` query, materialized once at genesis into the tectonic globe
and exposed to room-scale consumers through `LocaleFields`.

## The correctness heart: potability, not geometry

The classification's only hard decision is what to do with endorheic land —
the interior basins that never drain to the sea. The tempting shortcut is
"all endorheic water is salt," since that is where the ocean's mental model
already lived: closed basins evaporate, and evaporation concentrates salt.
That shortcut is wrong, and wrong in the specific way this campaign exists
to fix. The Jordan is an endorheic river — it flows toward the Dead Sea, not
the ocean — and it is fresh the entire way. Only the *terminal* point, the
evaporative sink where the water finally stops and concentrates, is salt.

So the classification asks two different questions of endorheic land: is
this cell a feeder, or the sink itself? A feeder is any endorheic cell that
still has somewhere downhill to go — flowing water, hence fresh, hence
`River` if its drainage accumulation clears the threshold. The sink is the
one cell (or handful of cells) at the bottom, where the downhill graph
terminates and there is nowhere left to flow: `SaltBasin`, the Dead Sea, the
Great Salt Lake. Ocean water is salt outright — below sea level, full stop.
Everything else that carries enough accumulated runoff is a fresh `River`,
including every endorheic feeder on its way to a terminal lake; everything
short of that threshold is plain `DryLand`. Four classes, one boolean query
(`is_fresh`, true only for `River`), and the one property that had to hold
for the whole campaign to mean anything: an endorheic feeder reads fresh,
and only the terminal sink reads salt.

## Where it lives, and what it cost

Terrain already owned `drainage` and `endorheic`; it now owns `water_kind`
too, materialized right alongside them in the same genesis pass, as a
`CellMap` that is recomputed every load and never serialized — exactly the
shape `drainage` and `endorheic` already had. No consumer threads the
geosphere at query time to ask "is this fresh"; they read a map, the same
way they already read elevation and biome.

The one place this touches the save format is `LocaleFields`, the room-scale
seam every consumer (the walk, the almanac, eventually the Surmise) reads
terrain through. It gains a **discrete** field, `water`, sampled at a room's
max-weight cell exactly as biome and substrate already are — never blended,
per the canonical-grid rule. Discrete fields changing what they carry means
the committed artifacts that render `LocaleFields` re-baseline (a
drift-check event, not a save-format one); the one such artifact
(`book/src/reference/locale-seed-42.json`) picked up exactly one new line
and nothing else moved. No ledger fact was added, no seed stream was drawn,
and genesis itself stayed byte-identical — the same shape The Ground used
for its rock-and-soil layer, applied one substrate deeper: fresh water at
zero epoch cost.

`WaterKind` lives in the terrain domain, which — like every domain crate —
depends on the kernel and nothing else (decision 0002). `LocaleFields`
derives `Serialize`, and the honest way to expose a domain-only enum through
a serializing window turned out to be a `serialize_with` shim at the locale
boundary rather than threading `serde` into terrain itself; the layering
rule held even where it made the plumbing one step less direct.

## Surfacing it

The almanac gains a short **"The Waters"** section, mirroring The Ground's
own headline treatment: one line reporting the fresh-water share of a
world's land — seed 42 comes in at roughly 6% under a generated sky, a clear
minority landform, exactly as a world where dry land and ocean dominate
should read. A water map lens (a fourth PPM alongside elevation and
lithology) was scoped as optional surfacing for this slice and deliberately
deferred — it is more rendering code than this close warrants, and nothing
downstream needs it yet.

## What this deliberately is not

This slice exposes a *truth*, not an *infrastructure*. Named rivers and
lakes as entities, settlements sited by fresh-water access, toponymy and
trade following a river's course — the whole people-infrastructure layer
hydrology could become — stays future work: it is a genesis change, needing
its own census regen and its own epoch, and nothing in this campaign's
scope touches it. Closed-basin lake physics (precipitation-routed filling
that would tell a *wet* interior basin from an *arid* salt playa, flat
multi-cell salt lakes, brackish river-mouth estuaries) is its own
domain-physics campaign, banked for later. The `hydrology` crate split —
pulling rivers, lakes, watersheds, and the
water table out of `terrain/drainage.rs` into their own crate — waits until
hydrology has grown into that people-infrastructure layer; a pure land
projection does not yet justify its own crate, the same restraint The
Ground exercised for its own material buffer. And no census metric reads
`water_kind` yet — a fresh-water-land-fraction metric across the standing
1,000-seed census is a natural next addition, but it needs an AWS regen this
campaign did not request.

## The road ahead

The parked Surmise inherits exactly the seam it was missing: its `is_water`
check becomes `LocaleFields.water.is_fresh()`, and its thirsty agents can
finally be pointed at the actual drinkable class instead of "anything below
sea level." Life and settlement inherit the same query — the
carrying-capacity model behind population placement already carries a
freshwater term that has been reading a proxy; it can now read the real
thing. And whenever hydrology
becomes worth its own crate, this campaign's classification is the floor
that crate builds on, not a rewrite it has to absorb.
