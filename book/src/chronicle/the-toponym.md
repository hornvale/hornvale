# The Toponym

A toponym is a place name that means the place. Until this campaign, Hornvale's
settlements were named for their biome and nothing finer: a village in a
temperate forest was named for the temperate forest, whether it stood in old
growth, in a damp fern-choked hollow, or in a gap where a fallen tree had let
the light down.

The world knew the difference. It had known it since The Shoal, which gave
every formation a pool of authored prose — *old-growth timber*, *a mossy
hollow*, *a windthrow gap*. What it lacked was a way for those to be *things*
rather than phrasings, and a way for a language to have words for them.

## The cycle that decided the design

The vocabulary lived in `windows/locale`, which renders room prose. Settlement
naming lives in `windows/worldgen`, the composition root. And `locale` already
depends on `worldgen` — so the edge naming would have needed is a cycle Cargo
would reject outright.

That constraint turned out to be pointing at the right answer. Realm,
formation and stratum are all `domains/climate`'s already; variant is a facet
of the same expression and belongs beside them. The move is not a workaround
for a build error but the correction the build error revealed.

Fifty-three named variants now sit over the sixty-five prose entries. Several
entries share a variant — *a mossy hollow* and *a fern-choked draw* are both a
damp hollow — and the table keeps the order and weights the prose pool always
had, so the room draw is untouched and every descriptor renders exactly as it
did. Regenerating every artifact after the move produced no diff at all, which
is what a move is supposed to look like.

## Two mechanisms the vocabulary needed

**A settlement occupies a cell.** A room is one of some four thousand within
it, so "the variant at a settlement" is otherwise undefined. The name is drawn
from a cell-scale variant on its own stream label — additive, perturbing
nothing that existed. A town named for its cerrado can still sit beside a
wooded draw, which is how regions actually work.

**A word must be earned.** A concept only reaches a name if the species is
*exposed* to it, and exposure is not declared but derived: a people is steeped
in the biome of every cell it settled. The variant now joins it by exactly the
same rule. A people that has lived in a grass sward has a word for a grass
sward, for the same reason it has a word for the savanna that sward is a kind
of.

Seed 42's glosses now read `temperate-forest-old-growth`,
`forest-gap-temperate-forest`, `temperate-forest-damp-hollow`. Its flagship,
which was *Qvooshtvoagootao*, is now *Vngoashshngaoshshngoogootao* — a name
that means the ground it stands on rather than merely the forest around it.

## What the epoch proved about an older campaign

Fifty-three concepts appended to a registry of a hundred and four is a
substantial expansion, and the obvious fear is churn: a new concept landing
mid-alphabet takes a proto-root some later concept would have drawn, and every
word derived from it moves.

The Accession exists to prevent exactly that, by sorting on accession epoch
before name so a new cohort lands strictly last. This campaign is the largest
test it has had, and it held precisely:

- the proto-root table grew from 104 to 157 entries with **zero** pre-existing
  roots moved;
- every settled people's lexicon grew by exactly those 53 with **zero**
  existing words changed.

Additive by construction, as promised, and now demonstrated at a scale that
would have been unmistakable had it failed.

## Five witnesses

The campaign's real difficulty was not the vocabulary. It was that this
project deliberately re-derives a name-gloss's truthfulness in **five
independent places**: worldgen's own test, the lab's `name-gloss-true` metric,
the lab's independent exposure re-derivation, the calibration battery, and a
DuckDB tripwire that cross-checks the Rust pins against the census fixture.

None of them calls the others. That is the point — each is a witness that
would have to be independently deceived.

Two of the five caught real correctness bugs rather than drift. The gloss
metric found that settlements were being named for a concept the checker could
not re-derive, so the names were *unverifiable* rather than merely different.
The exposure re-derivation found the same gap one layer down. Both would have
shipped silently under a single-witness design, and the world would have
carried names nothing could confirm were honest.

The census was regenerated twice for the same reason: the first run predated
those fixes and measured the old derivation. A metric change invalidates the
census that measured it, which is obvious in hindsight and was not obvious at
the time.

The preregistered hypotheses are unmoved — bugbear ≥ goblin ≥ hobgoblin still
holds at 860/718/909, and bugbear's homophony stays highest by 3.95× against a
guarded 3×. Only the drift witnesses moved, and they moved because every name
in every world was redrawn, which is what an epoch is.
