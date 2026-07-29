# The Formations

`HadalTrench` was never a biome.

It sat in the biome enum for years, between `Upwelling` and `Epipelagic`, and
nothing about it is a community of living things. It is a *depth* — the water
below six thousand metres, whatever happens to live there. The enum had been
carrying two different taxonomies in one list: communities like savanna and
kelp forest, and depth strata like bathypelagic and abyssal, as though they
were the same kind of answer to the same kind of question.

The symptom was visible in the classifier, for anyone who read it as a
statement rather than as code:

```rust
if feature == SeafloorFeature::Trench && depth_m > 6000.0 { return HadalTrench }
if feature == SeafloorFeature::Ridge { return HydrothermalVent }
```

A hydrothermal vent **is** abyssal. There is no world in which a cell must
choose between being a vent and being deep, and yet the code had to choose,
because there was one return slot and two things to say. The precedence chain
was not a modelling decision. It was the shape of a missing dimension.

## Saying two things

The repair is to say both. A cell's biome becomes a faceted expression —
`realm : formation : stratum` — where the formation is the community and the
stratum is the depth, and neither has to win.

```
waterworld : vent       : abyssal      <- what the vent actually is
waterworld : open-water : hadal        <- what HadalTrench actually was
overworld  : savanna    : surface
```

Realm is the third facet and the one with a future in it. It is deliberately
**not** an enum of worlds but a triple — a medium, the way in, and the column
of strata it holds:

```
Overworld   = (air-over-rock, default, [surface])
Waterworld  = (water,         dive,    [epipelagic … hadal])
```

Two values today. The point of the shape is that a sky realm, or an underworld,
or eventually somewhere reached only by ritual, is a new *value* rather than a
new axis. The discriminator between the world's own column and a plane turns
out not to be materiality — an elemental plane is perfectly material — but
**access**: whether you get there by continuous movement through a change of
medium, or by transit. The engine already distinguishes those; walking is one
and stepping inside a building is the other.

## The projection

None of this was allowed to move a single byte of a single world.

The technique: keep the old type, and make it a **projection** of the new one.
`Biome` did not go away; it became what a `BiomeExpr` looks like when you
flatten it back down. Every consumer in nine crates still calls `biome_at()`
and still receives exactly what it received before, because the projection
reproduces the old answer for every input. The disentangling happened
underneath a surface that never changed.

`HadalTrench` is where you can see the projection doing its work. It has no
formation of its own; it falls out of the open-water arm at hadal depth:

```rust
Formation::OpenWater => match self.stratum {
    Stratum::Hadal => Biome::HadalTrench,
    Stratum::Abyssal => Biome::Abyssal,
    ...
```

The legacy enum's oddity is now a derived consequence rather than a special
case, and reading that arm tells you what the old taxonomy had been hiding.

## Keeping the old behaviour on purpose

Two arms of the classifier look like defects and were preserved exactly:

- A deep trench is tested **before** a ridge, so a cell that is both is hadal
  open water rather than a vent.
- The shallow band matches reef above 20 °C and kelp below 12 °C, which leaves
  everything between them matching neither, falling through to the depth
  bands beneath.

Both are the world as it stands. A refactor that "tidied" either would have
changed which biome thousands of cells report, which would have changed
settlement placement, which would have changed names — and the whole campaign's
claim to have moved nothing would have been false.

The guard is a sweep of more than six hundred cases comparing the new path
against a **verbatim transcription** of the old classifier, kept in the test
module. That copy is not redundancy; it is the only way the claim is testable
at all. Delegating to the shipped function would have compared it with itself.

Three independent checks agree that nothing moved: the sweep, the seed-42 world
fixture already standing in the gate, and a byte comparison against a binary
built before the campaign began. Regenerating every artifact in the book
produces no diff.

## What it cost, and what it bought

One new module, two delegating functions, and a mapping table. Generation runs
at 1.74 seconds against the 1.9 it ran at before.

What it bought is that the next three campaigns are cheap. Marine prose has a
formation to hang authored variety on. The placed-exotic overlay has a tier to
be commensurable within. And the water column has a stratum to *be* — the same
construct the underworld's geological layers will use, rather than a second,
parallel one invented later.

The taxonomy also stops fragmenting. `biome_class`, the fertility mapping at
the composition root, was a formation-group facet hand-rolled because no
principled tier existed; it now keys off the tier. And the enum's own doc
comment, which claimed for years to describe "a biome class — terrestrial or
marine", finally describes what the enum is: the pre-facet taxonomy, kept as a
projection.
