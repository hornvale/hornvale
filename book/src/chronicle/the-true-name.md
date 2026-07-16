# The True Name

A kind was known by a string. The label `"kobold"` did three jobs at once — it
keyed the species registry, it was the value of `SpeciesDef.name`, and it was
the text a settlement's `species-name` fact carried into the ledger — and
nothing at the type level said that these were the same thing, or that this
string was the identity a kind would be remembered by. Shadowing it was a second
identity nobody had chosen: a positional integer the packer minted by
enumerating the registry, correct only for as long as it never escaped a single
build. Two identities, both implicit, one of them fragile.

The Menagerie had already leaned on this quietly — when it added twelve
creatures the positional integers all renumbered, and it was only safe because
the labels, not the integers, are what the ledger records. The True Name makes
that arrangement explicit and typed, so the next campaign can build on it.

## The label becomes a type

`KindId` is the stable identity of a kind — its authored label, wrapped in a
newtype and placed in the kernel beside `EntityId`, the two identities of the
entity-component substrate sitting side by side: the minted handle of an
*instance*, and the authored name of a *kind*. The species registry is now keyed
by `KindId`; `SpeciesDef.name` still carries the same label beside it, the way a
predicate's name duplicates its own registry key — the redundancy is deliberate,
because the name *is* the identity, not a lookup into something else.

`KindId` is build-state, and its type says so: it wraps a `&'static str` and
does not serialize. A kind is authored, never loaded; its registry is re-derived
from the source each run, never read from a save. When a kind is *referenced* in
the ledger it goes in as ordinary text — a `species-name` fact is still
`Value::Text("kobold")` — and comes back out resolved against the registry by
label. So the type that names a kind and the value that records a reference to
one are, correctly, two different things.

## The integer demoted

The positional `u32` did not go away — the packer still wants a dense index for
its array math, and that is a real and honest use. What changed is its status.
It is now documented, in one authoritative place, as a *build-local dense
index*: derived by enumerating the `KindId`-ordered registry, valid only within
one build, and never serialized. It is the ephemeral half of a durable-label /
ephemeral-index split, the same shape as an `EntityId` against an array offset —
identity is the label; the integer is a temporary convenience that must never be
mistaken for it.

A regression pins the distinction so it cannot quietly rot. Prepend a kind whose
name sorts first and every real kind's build-local index shifts by one — proof
that position is not identity — while every real kind's `species-name` fact,
minted through the true genesis path into an independent world, stays
byte-for-byte the same. The integer moves; the name does not.

## What did not change

Nothing a world contains. The label text was always the serialized identity, so
making it a type and demoting the integer alters no committed byte on any seed —
every world, every artifact, bit-for-bit identical across the whole campaign.
This was a change to the type system and to a contract, not to the simulation.
The contract is worth stating plainly, because a later campaign will lean on it:
a kind's serialized identity is its label, and a deliberate change to a kind that
must not be mistaken for the old one takes an epoch suffix — `red-dragon/v2` —
never a rename.

That is the whole of it. A kind now has a true name: a typed, stable identity
with a save-format contract, distinct from the array index that merely counts
it. The god-struct is split, its kinds are named, and the domain-owned component
registries the next campaign will key by `KindId` have a key that will hold.
