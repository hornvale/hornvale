# The Individuation

A kind is a label with defaults. Three campaigns of this program taught
Hornvale to say what a goblin *is* — a set of components, each owned by one
domain, joined at a stable `KindId`. What they never let it say is what a
*particular* goblin is, once it stops being interchangeable with every other
goblin of its kind: the one that grew unusually large, the beast that woke up
and started speaking. Every entity in the world was still, in effect, an
anonymous instance of its species — present in the ledger only as facts about
things it had done, never as a record of what set it apart from its kind.
This campaign gives an entity its own individuality: an effective trait that
is its kind's authored default, *unless* the entity itself has said
otherwise.

That rule — an instance's own fact overrides its kind's default, and nothing
else does — is prototype inheritance, the same idea JavaScript objects, Self,
and a database column default all share. It is also, conveniently, exactly
what "individuation" names in the psychological sense the chronicle borrows
its title from: a particular thing differentiating itself from the
undifferentiated collective it came from. The awakened owlbear that carries
this campaign's demonstrations is individuation staged twice over — once as
data (an override fact distinguishing one entity from its kind's row) and
once as fiction (a beast that wakes into personhood).

## instance-of: a state machine made of facts

The ledger already had a native shape for "this became true on this day, for
this reason" — the Fact, with its day and provenance fields. Campaign 4 gave
that shape a fast index. What it did not yet carry was a canonical answer to
the most basic question an entity-component substrate can be asked: *what
kind of thing is this entity?* This campaign adds it as `instance-of`, a new
kernel-core predicate beside `NAME`, and a mint operation,
`Ledger::mint_instance`, that is that predicate's sole writer — the
single-writer discipline the ledger has held since campaign 2, applied here
by construction rather than convention.

The one wrinkle is temporal. Most predicates in Hornvale are functional: a
subject gets one value, and a second commit is rejected as a contradiction.
`instance-of` cannot work that way, because a kind is allowed to *change* —
an owlbear can be awakened, and the arc this unblocks (corpse, lich, and
their kin) depends on kind changes being ordinary, legal events rather than
errors. So `instance-of` is non-functional, and the entity's current kind is
read by a new kernel primitive, `kind_of`, which returns the *last*
committed `instance-of` fact rather than the first. Commit order is
deterministic time order, so this reading is total and unambiguous: an
entity's full kind history — every state it has occupied and every
day-stamped, provenanced transition between them — is a state machine
written entirely in facts, inspectable today with nothing more than
`facts_about` and a filter.

## The lens: override, else default

The join this campaign exists to build sits between two things that were
already real — a kind's authored components, and an instance's own facts —
and had never been made to answer one question together. The answer is a
lens, `instance_biosphere`, materialized fresh on every call: look up the
instance's current kind via `kind_of`, take that kind's authored
`BiosphereTraits` as the starting point, and then apply the instance's own
override facts (mass, potency) on top, if it has committed any. No cache, no
serialized shadow copy — a derived read over the ledger and the registry,
the same posture the query engine's indexes hold. When the metaplan's
per-tick cache eventually arrives, it will belong to the campaign that also
builds the tick, because the tick is the only thing that can say when a
cached lens goes stale.

The override facts are deliberately per-instance rather than
per-(instance, kind): they survive a kind change. This is stated as a
contract, not left to accident, because it is the detail the demonstration
turns on. The unusually large owlbear that gets minted, then given a
900 kg mass override, then awakened into `awakened-owlbear`, stays a
900 kg awakened owlbear afterward — the new kind supplies a new potency, not
a fresh mass. Its individuality outlives the transition that individuated
it further.

## Four shapes of "what a kind is made of"

Three new kinds enter the substrate to prove the join is not owlbear-shaped
by accident: `deity` (a `manifest` flag and nothing else — mind-adjacent,
no body at all), `culture` (a transmission mode, oral or written — a social
aggregate with neither body nor psyche), and a pair of material kinds,
granite and limestone (hardness and a sedimentary flag — body-stuff with no
agency whatsoever). Each is a distinct point in the space of possible
component-sets an entity-component substrate is supposed to make cheap, and
each lives in its owning domain's new component store, added to the
canonical `WorldComponents` at the one legal place domains are allowed to
meet.

One kind does not join them there. `awakened-owlbear` needs biosphere *and*
the full peopled cluster — psyche, perception, articulation, lexicon, family
— because check_integrity's invariant from campaign 3 is not relaxed for
"nascent": if a kind speaks, it carries everything speaking requires, no
half-measures for narrative flavor. But genesis mints a species entity for
every row in the canonical biosphere registry, and the placement engine
walks that same roster — so a canonical `awakened-owlbear` row would make
every shipped world place a mighty, speaking owlbear as ordinary fauna. It
lives instead in the test/lab roster, composed via the same `from_stores`
path the laboratory already used for its synthetic goblin clones and
tonality-forced serpents. This forced a small but real correction to how
"which kinds exist" is answered: not by asking the biosphere registry alone,
but by taking the union of every component store's key-set —
`WorldComponents::kinds()`. Biosphere is still where genesis and placement
look; it is simply no longer the whole of the kind roster, because kinds like
deity and culture were never going to have a body.

## Shadow, not cutover

Every one of this campaign's mechanisms — the predicate, the mint API, the
lens, the four new kinds — is exercised only by tests. No shipped world
mints a single instance. This is deliberate: the strangler-fig discipline
the whole program runs under says the mechanism proves itself before it is
allowed to write production data, and a genesis retrofit right now would
burn artifact drift and a census regen with no consumer yet able to *render*
what it created. The proof that nothing changed is not a claim; it is a
pinned test asserting a freshly generated world contains zero `instance-of`
facts, plus every committed artifact — three seed-42 almanacs, the
elevation map, the census fixtures — coming back byte-identical. The only
serialized drift anywhere in this campaign is the concept registry gaining
rows for the new predicates, which is exactly the kind of change the save
format has always allowed without an epoch. The first campaign whose
consumer actually looks at an instance is the one that gets to mint them
in a real world, and that campaign owns the artifact-drift call this one
declined to make.

## The keystone

The program has carried one demonstration query since its metaplan: *mighty
things in the cold north*. It names exactly the join this campaign built —
filter entities by a fact about their own state (their place, `located-in`,
answered by the query engine's OSP index) crossed with a fact about their
*kind* (potency, read through the lens). The keystone test mints three
instances — a mighty owlbear and a mundane one in the north, a mighty one in
the south — places each with an ordinary ledger fact, and asks for
everything in the north whose current kind carries positive potency. The
answer is exactly one entity: the mighty northerner, and no other. Two
mundane instances share its home; a third mighty instance shares its kind;
only the intersection of "here" and "mighty" is what the query returns. This
is the join the program was named for, running for the first time, over
indexes the query engine spent the previous campaign building for exactly
this purpose.

## What is left

The lens has no cache, kind changes carry no transition guards, and overrides
apply to numeric scalars only — mass and potency, not yet a structured trait.
Each of these is a deliberate boundary, not an oversight: the cache waits
for the tick that will own its invalidation; guards wait for the capability
schema that will know which systems are allowed to write which transitions;
structured overrides wait for a Value-envelope story nothing yet demands.
The program has one substrate campaign left in this arc after this one —
systems and the schedule, campaign 6 — before entities finally do something
without a person driving them by hand.
