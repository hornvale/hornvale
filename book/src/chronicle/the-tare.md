# The Tare

A tare is the weight you subtract to get a true reading. This campaign is about
a scale that had never been zeroed.

The world places about a hundred exotic sites — rare places where the ordinary
rules of a biome are negated, warranted by the ground itself. There are three
things to be, and a fourth that was never reachable. Across four seeds, the
placed sites came out **92 to 98 per cent fungal**. A design space of kingdoms
and energy sources was shipping as one repeated line of prose.

## A comment that was never code

Placement draws from a table of candidates, each scoring how strongly a cell
warrants it, and the fungal candidate's comment has said the same thing since
it was written:

> Fungal kingdoms: warranted by damp, low-relief, non-volcanic ground.

The score underneath computed one of those three clauses:

```rust
if relief > 0.0 && unrest < 0.4 { 1.0 - unrest }
```

Non-volcanic, and nothing else. `1.0 - unrest` is at least 0.6 on any quiet
land cell, which is most of a continent — so this was not a warrant at all but
a near-constant default wearing a warrant's comment. Against the vent
candidates, which score raw `unrest` and are therefore high only within a few
cells of a spreading ridge, it won the weighted draw nearly everywhere it was
offered.

The three scores had never been on a common footing. Nothing was wrong with the
drawing mechanism; it was faithfully sampling from numbers that meant different
things.

The repair is to implement the sentence. `drainage` is flow accumulation and is
zero on dry land — it is exactly the "damp" the comment always claimed — and
saturating it, `d / (d + k)`, keeps the term bounded without needing to know
any world's maximum in advance.

That alone moved fungal from 92–98% down to 78–91%. Better, and not enough,
which is how the second defect came to light.

## A kingdom that could not happen

There are four kingdoms an exotic site can belong to. `PlantAnimal`, `Fungal`,
`Microbial` — and `Crystalline`, mineral flora, which has existed as a type for
as long as the others and has had its prose authored for just as long:

> grown with mineral crystal

It appeared in no candidate. Not mis-weighted, not rare: **absent**. There was
no arrangement of terrain, no seed, no world in which a walker could ever have
encountered one. The type existed, the sentence describing it existed, and the
thing itself could not occur.

That is the same defect The Shoal found in the sea — something modelled and
authored and then never reachable — and it is worth naming as a class, because
neither instance was findable by any test. A test can only fail on behaviour
that happens.

Crystalline now has a warrant of its own, and a real one: endorheic basins,
where water has nowhere to leave and salts concentrate, on hard indurated
rock. Closed basins are rare, so the score is zero almost everywhere. That is
what a warrant is supposed to look like — the property the fungal score had
quietly lost.

## What a world looks like now

| seed | fungal before | fungal after | crystalline |
|---|---|---|---|
| 42 | 98% | 61% | 20 |
| 1 | 96% | 73% | 26 |
| 7 | 92% | 65% | 32 |
| 99 | 93% | 72% | 27 |

Fungal is still the most common exotic, and should be: fungal ground is common
and hydrothermal vents are not. What has changed is that it is no longer the
*only* one. Seed 42's page in the book now lists twenty places grown with
mineral crystal, in a world where last week there were none and could be none.

## The risk that had to be measured first

Exotic placement is not purely cosmetic. A placed site's strangeness becomes
the **uncanny** signal, and the uncanny is a drive the creature layer responds
to — so moving the sites moves what the world's inhabitants feel, and the
population-health battery reads exactly that.

The Stratum flagged this in advance as an open question and said any campaign
touching the overlay owes a health run as its *first* step, not its last. So
the battery was run before a line was changed, and again afterwards. It is
green both times: the null control still reads no false alarm across the seed
sweep.

World bytes never moved, and could not have — placed sites are derived on
demand and never stored in a save.
