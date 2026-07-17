# The Named

*Campaign: The Named — attribution is a property, not a position.*

## The accident that looked like a rule

For as long as the almanac has had more than one pantheon to print, its
Gods section chose each block's opening line by counting:

```rust
if i == 0 { /* anonymous lead */ } else { /* species-qualified lead */ }
```

Block zero got the old, unattributed sentence — *An organized priesthood
tends a pantheon:* — and every block after it named the people who held it.
The intent was conservation. Before the world had species, the almanac had
exactly one pantheon and exactly one way to introduce it, and that page is
pinned: a reader of a pre-species save must see the bytes they always saw.
Pinning the old sentence to the first block was the cheapest way to keep
that promise while letting later blocks say something new.

It worked, and it was never a rule. It was a coincidence with a plausible
face. `i == 0` says nothing about who holds a pantheon; it says only that
the loop has not yet advanced. The sentence it selects is *about*
attribution, but the condition it selects on is about iteration. That gap
is invisible while a single fact holds it closed — that block zero is
always goblin's.

## How the fact expired

The species registry is a `BTreeMap`, so its key order is alphabetical, and
the builder pushes one block per species in exactly that order. When the
roster was goblin and kobold, alphabetical order put goblin first and the
coincidence held. The roster is now four peopled species — bugbear, goblin,
hobgoblin, kobold — and **bugbear sorts ahead of goblin**. Block zero
stopped meaning "goblin" and started meaning "whichever species is
alphabetically first among those that happen to hold beliefs."

Nothing announced this. Bugbear rarely places, so block zero usually still
landed on goblin, and the sentence still read plausibly. What changed is
that goblin's pantheon was now the *only* one in a multi-people world
rendered without its village and its name, sitting directly above
neighbours that had both. On seed 2:

```
An organized priesthood tends a pantheon:                              <- goblin
In the legion of **Woogwaoweobwoaljeovzeof**, ...                      <- hobgoblin
In the warren of **Qshashngdashksashngkoshqsho**, ...                  <- kobold
```

Roughly one seed in six placed three peoples and printed this. The
Campaign Y2-2 retrospective had predicted the mechanism exactly — *"No
pinned seed exercises this path, so it shipped undetected"* — and filed it
as a debt for whichever campaign next touched the renderer. The debt was
not called in by a failing test. It was called in by someone asking whether
the ticket was still worth keeping.

## What replaced it

The discriminator the renderer needed already existed in the data and was
being thrown away. The builder knew perfectly well which kind of block it
had constructed — a species' pantheon, or the legacy fallback for a save
with no `peopled-by` facts — and encoded that knowledge as three
empty-string sentinels the renderer was free to ignore. It ignored them.

So the sentinels became a type:

```rust
pub struct PantheonBlock {
    pub attribution: Option<PantheonAttribution>,
    pub cult_form: Option<String>,
    pub beliefs: Vec<BeliefLine>,
}
```

and the renderer became homogeneous. No index, no first block, no special
case — every block renders from its own content, and the `enumerate()`
disappeared along with the bug. The policy moved up to the composition
root, where the knowledge already lived.

## Naming as a function of ambiguity

The subtler question was not *where* the decision belongs but *what it
decides on*. The obvious answer — reserve the anonymous lead for genuinely
legacy worlds, and attribute every pantheon that has a species — is wrong,
and the almanac itself says why.

The People section had already solved this problem, by cardinality:

```rust
let multi_species = flagships.len() > 1;
```

One people, and the chief line reads *The chief settlement, X*. Two or
more, and it reads *The chief goblin settlement, X*. The almanac's voice is
already: **name the species only when there is more than one to
distinguish.** Attributing every pantheon regardless would have made the
Gods section name goblin two paragraphs below a People section that
pointedly did not — one document, two voices.

The prose settles it independently. The attributed lead is contrastive:
*the village of X keeps **its own** folk pantheon* presupposes some other
pantheon for it to be distinct from. In a world with one people, "its own"
contrasts with nothing.

So the Gods section now reads the same predicate the People section reads —
literally the same function, not a second copy that agrees for now. A world
names its peoples in both sections or in neither.

## The shape of the error

This is the second time in consecutive campaigns that Hornvale has found a
privileged base case that had no right to be privileged. The Lens demoted
the `natural` render from a base case to a peer in a registry of lenses, on
the ground that a picture which merely *looks* like a photograph has no
special claim to truth. The Named demotes block zero from a base case to a
peer among pantheons, on the ground that being first in a loop is not a
fact about the world.

The pattern is worth naming, because it does not look like a bug while it
is being written. It looks like conservation — the old thing keeps working
because it is still the default path, and the new thing is handled as a
generalization layered around it. The default is load-bearing and
undeclared. It survives exactly as long as the coincidence that made it
true, and it fails silently, because the base case still renders something
plausible. The question that finds it is not *does this work?* but *what is
this condition actually about?* — and for `i == 0`, the honest answer was:
nothing.
