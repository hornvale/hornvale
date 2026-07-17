# The Dissolution

Every creature in Hornvale was, for a time, a single index card. One
`SpeciesDef` struct carried everything a kind was: its body (mass, metabolic
class, resource niche, climate tolerance, magical potency), its mind
(psychology, perception), and its speech (phonology, a social vocabulary, an
ancestral proto-tongue). The four goblinoids wore the card comfortably. The
menagerie tore it: a dragon, a fungus, a walking mound of ore — each was forced
to carry a psychology it would never think with and words for a chieftain it
would never elect. This is the inheritance-versus-composition problem in its
oldest clothes, and it is the problem entity-component systems exist to solve.

This campaign dissolved the card. A kind is no longer a record. It is a **set of
components**, each keyed by the kind's stable label (`KindId`), each authored
and owned by the one domain that presents it, and joined into a whole only at
the composition root. `species` authors the body and the mind; `language`
authors the speech; `worldgen` assembles a kind from whatever components carry
its key. The change is deliberately invisible from outside: same seed, the same
1043-fact world, every committed artifact bit-for-bit unchanged. What moved was
*where* traits live, never *what* they compute.

## A kind is a join, not a record

The load-bearing idea is a change of ontology, and it answers a question the old
struct made natural to ask: *who authors the goblin?* No one does. `species`
authors the goblin's biosphere; `species` authors its psyche and perception;
`language` authors its articulation and lexicon; the label `KindId("goblin")` is
the only thing the four registries share. "The goblin" exists only as the view
`worldgen` assembles by looking that label up in every registry — the
orchestral score the substrate's design promised, each domain a staff, each kind
a measure, silence where a component is absent. A dragon has a biosphere row and
no mind row; that absence *is* its dragon-ness, expressed rather than declared.

Two consequences fall out. There is no roster struct and no central list of
kinds: at this point in the program the universal biosphere registry — the
trunk every kind hangs from — is still the whole of it, and "which kinds
exist" is answered by iterating it alone. (Campaign 5, *The Individuation*,
generalizes this once kinds without a body — deity, culture, raw material —
enter the substrate: the canonical roster becomes the union of every
component store's key-set, biosphere included but no longer exclusive.) And the
old struct's `Option<PeopledTraits>`, which made "all the peopled traits or none"
true by the shape of the type, becomes a **referential-integrity check** at the
composition root: the peopled cluster must share one key-set, every peopled kind
must have a biosphere row, every multi-member family must have a proto. What the
type once guaranteed by construction, a load-time invariant now guarantees by
inspection — and fails loudly, with the kind's name, when a future author forgets
a component.

## The substrate and the seam

Underneath sits one new kernel primitive: `ComponentStore<K, C>`, the
generalization of the geosphere's `CellMap` from a fixed cell key to any ordered
identity — a `BTreeMap`-backed typed store with deterministic ascending
iteration, and an *open* `Component` marker (unsealed, so any domain may declare
a component without editing a central enum). Every domain publishes its own
registry over that store; `worldgen` holds the struct-of-registries and is the
only crate that names two domains' component types at once. That last fact is the
campaign's quiet victory: the constitutional layering — a domain depends on the
kernel and no other domain — is no longer a convention a reviewer must guard. It
is enforced by construction. `species` *cannot* read `language`'s articulation,
because it cannot depend on `language`; only `worldgen`, which depends on both,
composes across them. Moving the speech cluster (the articulation vector, its
exotic-manner enum, the proto table, the lexicon) bodily out of `species` and
into `language` was the first time the ownership matrix stopped being a diagram
and became a compile error.

## The strangler-fig and its one surprise

The migration was a textbook strangler-fig: ten commits, each byte-identical,
each independently reviewed, the new registries grown alongside the god-struct
until the last commit could delete it. The design's one genuine surprise arrived
mid-execution. The plan assumed `worldgen` would reassemble a kind by looking its
components up in the *canonical* registries. But the laboratory builds synthetic
creatures — a goblin cloned under a fresh name, a "serpent" with its tonality
forced to 1.0 — that are deliberately absent from the canonical set and carry
values no registry holds. A canonical lookup would either fail or return the
wrong creature. The correction was not a patch but a clarification of the thesis:
**the roster itself is a component-set.** `worldgen` builds its component stores
*from the roster it is handed*, canonical or synthetic, and reads only from that.
The synthetic serpent is now, honestly, what it always was — a custom set of
components composed from the registries and overridden where it differs — and the
model is more faithful for having been forced to say so.

Genesis, too, moved. The fact stream that records each species entity — its name,
its psychology, its perception, its phonology — had lived in `species`; it now
lives at the composition root, reading the assembled components, committing the
same facts in the same order under the same provenance. The ledger is unchanged
to the byte, but the code that writes it now sits where the composition happens.

## What this leaves

The god-struct is gone. `SpeciesDef`, `PeopledTraits`, the monolithic
`registry()` — all deleted; `species` authors its four registries directly, and
the workspace holds no reference to the old shape. The entity-component program
has three of its seven campaigns behind it: the split (The Menagerie), the
identity (The True Name), and now the components themselves. Ahead lie the query
engine that will make "every kind with component X" a first-class question, the
instance-versus-kind join that will let a beast awaken or a corpse cool, the
schedule, and — only when measured scale demands it — the spatial partition. But
the shape they will build on is now real: an entity is its component-set, and the
sentence is no longer a description of the data model. It is the data model.
