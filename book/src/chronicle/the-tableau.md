# The Tableau

*A tableau vivant is a scene held still so it can be looked at. The word
carries the whole argument: the situation is arranged, not found, and nobody
pretends otherwise.*

## The blocker was a decision, not the world

The Company closed by reporting that its founding scene — a goblin with an
orange, a drow who wants it — was blocked by `SOC-one-creature-per-settlement`,
the world's rule that a settlement holds exactly one simulated creature.

That was wrong twice over, and the campaign exists because of it.

The binding constraint was not the world. It was The Company's own spec, which
said **scenes are found, never staged** — written on one good argument (a
staged pass is evidence about the staging, not about Hornvale) and then applied
universally. Two products had been forced into one instrument: a scene as a
*capability audit*, which must be found, and a scene as *a drama you want to
watch*, which must be stageable. The audit's requirement won a fight the drama
was never allowed to enter.

The second error is systemic and outlives this campaign. The decision log is
append-only and supersedes rather than edits, which is right for correctness
and misleading in tone: a record written to be durable *reads* as settled
whether or not it was. `SOC-one-creature-per-settlement` is a scale-management
compromise — it exists so the world does not carry a billion creatures — and it
was read as a statement about what the world may contain. Nothing in the format
distinguishes a ruling from a stopgap.

## A diff, not a world

A tableau overrides named layers of an otherwise-derived session. It does not
build a world from nothing, and the reason is cost: every expensive layer —
sculpted terrain, fitted climate, the species roster, the demography fit —
lives in the `WorldContext` a session rides, and `Session::start_in` already
promises to pay none of it. Build that once; a hundred tableaux after it each
cost a session start.

The alternative was to synthesize a `LocaleContext` from stipulated values.
That was rejected: its fields are private and typed to real terrain and
climate, and a second way to make a locale is two paths that can disagree about
what a place is.

## Unspecified means empty

The rule that makes a tableau reproducible is the one about what it does *not*
say:

> Layers you could have written down default to **empty**. Layers you could
> not — terrain, climate, the species roster, the sky — **inherit**.

An unspecified cast stages nobody. It does not fall back to the world's own
inhabitants, because a tableau that stipulated a room and inherited a cast
would depend, silently, on whatever the seed happened to place — the *mystery
guest* every fixture library eventually learns to fear, and the exact
complaint this campaign answers.

One corollary from the spec did not survive contact: the requirement that
*absent* and *empty* stay distinguishable is vacuous under the rule above.
Both mean empty for every statable layer, so there is no third state to differ
into, and a distinction with no consequence is a field nothing reads.

## What it took, and what was already there

Almost nothing had to be invented.

`Body` is a plain struct whose `species` is a `String`, so a drow needed only
constructing. `derive_wild_npcs` already built village-less bodies from
`(species, position)` pairs — which is precisely a staged body's shape, so the
staged path shares that derivation rather than adding a second. `Facet` already
answers `centroid()`, so a cast placed at one point lands in one room.
`thing::promote` and `located_in_holder_fact` already put a thing in a hand.

The single genuine gap was that `Session::start` derived its roster internally
and `bodies()` was read-only. One seam, and a drow could stand in a world that
has none.

## The scene

```text
> !whoami
A drow of the wilds (agent 9630022852472602624), day 0, room 896860167.
> !npcs
1 NPC(s) derived this session:
  [1] goblin
> examine goblin
goblin — a goblin of this world, alive and moving. It is holding a key.
```

From four lines of JSON naming two species and one prop. No seed hunted; no
world that happened to contain it.

The last inch was not the staging. **Custody was on the wire and no verb said
it**: The Company put `carrying` on the presence-gated channel, and nothing
rendered it for a creature you were looking at, so a player could not see the
key without reading snapshot JSON. Teaching `examine` alone would have missed
the chart marks and the focalized nouns, which is how *"examine has two
matchers"* became a known defect twice before; all three now read one fold.

## The measurement, and the case it makes

A staged fixture that asserts on an arrangement it chose costs **3.35 s** — one
world build.

The seed hunt it stands against is `world_where_an_unsensed_creature_arrives`,
which sweeps sixty-four worlds looking for one that exhibits a situation. Its
own recorded cost is **233.72 s**, and it currently **fails on every seed in
the range**. The consequence is written into its ignore reason: `!wait`'s
arrival narration has no witness of any kind.

Seventy times the cost, and the expensive one does not work. That is the case
for staging, made by the codebase about itself before this campaign began — and
that test's reason names the remedy exactly: *a day-parameterised placement
seam able to pre-stage a same-tick position change*. A tableau is most of that
seam. What it still lacks is the day parameter, which is the next campaign's
business rather than this one's.

## What was deferred, and why it is not cowardice

The chamber seam was specified against the wrong object. The plan said inject
an `Interior` — a room's furniture, a hearth and a doorway — while shadowcasting
runs on the `Lattice`, the grid of cells that a `Structure` and a seed are
embedded into. Supplying an `Interior` would let an author say *there is a
hearth here* and never *the wall is here, so the light falls there*, which is
the whole of a lighting test.

Correcting it is not a rename. It opens a fork the campaign had not earned an
answer to: whether a tableau supplies a lattice wholesale — total control,
including rooms the world's own rules would never produce — or supplies
constraints the embedder honours. That is the same permissive-versus-safe
question settled for creatures, and it deserves the same deliberate answer
rather than a late one.
