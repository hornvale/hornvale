# The Correspondence

A world can model a thing without having a word for it, a word without a way
to perceive the thing, a perception no mind can hold. Hornvale had drifted into
exactly this state, quietly, across a dozen campaigns. The climate carried a
temperature field — isotherms, seasonal swing, the Orrery's living lens — and
the world had no *concept* of temperature, no word any people could say, no
phenomenon anyone could observe. Nothing was wrong; nothing failed. The gap was
simply invisible, because every guard the project owned checked one ledger's
internal coherence and none checked the correspondence *between* them. This
campaign makes "modeled but unnamed, unspoken, unperceived, unthought" a state
the compiler refuses to accept.

## Five ledgers, and the conservation law between them

A phenomenon, to be fully real in Hornvale, must appear in five ledgers. It is
**computed** (a field, a vector, a bearing). It is a registered **concept** (a
name the fact graph can predicate over). It is a **lexeme** (a word a people
can utter). It is a **percept** (a salience-ranked observation a mind can
receive). And it is **cognizable** (something a mind can hold and act upon).
These are not layers stacked in privilege; they are parallel books, and the
question this campaign asks of every modeled quantity is the question a
bookkeeper asks of every transaction: *does it post to every ledger, or is its
absence from one explicitly, accountably recorded?*

Framed that way the design writes itself, because the shape is old. It is the
`NOT NULL` foreign key that forbids a row without its referent; it is the
central dogma's *silenced* gene, absent by annotation rather than by
accident; it is Saussure's sign, where a signified without a signifier is by
definition unspeakable. Hornvale already enforced this posture twice — the
`Biome` catalog's exhaustive-match tripwire, which will not compile until a new
variant is handled everywhere, and the type-audit's default-deny verdict, which
fails until every boundary primitive carries a judgment. Each guards a single
registry. The Correspondence lifts that same discipline from one book to the
correspondence across all five.

## The manifest, and the void that must name itself

The instrument is a single record. To register a concept, a domain now supplies
a `Manifest`: the concept anchor, and one edge each for the lexeme, percept, and
cognition ledgers. Every edge is either `Present`, carrying its realization, or
`Absent`, carrying a **`Void`** — and `Void` is a closed vocabulary of reasons
(`Unnamed`, `Gap`, `Imperceptible`, `Uncognized`), each of which must state
*why*. The record has no default and no partial constructor. There is no way to
write down a manifest that omits an edge, because the incomplete value does not
exist to be written — the type admits no representable-but-excluded state. This
is a **construction** proof rather than a recognition proof: it does not check
that coverage is complete, it makes incompleteness unrepresentable. The repo's
first `compile_fail` doctest stands beside it as the anti-vacuity witness,
exhibiting that a manifest missing an edge does not compile, so the guard is
known to bite rather than trusted to.

The old registration primitive, which took a concept and quietly returned, is
gone. `register_manifest` is the only public way to name a concept, and the
migration ran through all nine domains and some seventy concepts without moving
a single serialized byte — the manifest's edges live in a field the save format
skips, so a world's identity is untouched while its author is now forced to
account for how each concept manifests. That byte-identity was not hoped for; it
was scored, the world regenerating every committed artifact unchanged, the way
this project scores every claim that something changed nothing.

## Where enforcement lives, and where reconciliation does

Not every edge is forced at the same place, and discovering why was the
campaign's one real design turn. Three of the four edges flow *outward* from the
concept's owner: the owner computes the quantity, registers the concept, and is
the natural author of whether it is perceptible or cognizable. Those edges are
compile-forced at the registration site. But the **lexeme** flows the other
way. A word is not the owner's to give; it is realized by the language domain,
per people, per era, long after and far from the concept's registration. To
demand the owner supply the word would recouple a seam the architecture keeps
deliberately open. So the lexeme edge is a *declaration* — the owner promises
the concept is `Expected` to be lexicalized, or records a reasoned `Gap` — and a
separate reconciliation checks that promise against the language ledger it
actually depends on.

That reconciliation has teeth, and they closed on real slack. Seven concepts —
`snow`, `rain`, `ice`, `god`, `spirit`, `home`, `hearth` — had been declared
`Expected` on optimism, but no lexicon root or compound recipe named them. The
honest resolution was not to loosen the check but to correct the declaration:
each became a stated `Gap`. The world now carries a coverage report,
drift-checked like any other artifact, whose trial balance foots — for every
ledger, the concepts covered plus the concepts explicitly void equal every
concept that exists. A phenomenon that appears in no column is a failure the
build surfaces.

## The negative space is the plan

The report's most valuable column is the emptiest. Every concept's cognition
edge is `Uncognized`, deferred to a named future wave — because at the moment
this campaign landed, no concept had a cognitive handle at all. That is not a
weakness of the design; it is its yield. The list of voids is not an admission,
it is a backlog: a compiler-generated, drift-checked enumeration of exactly what
the world models but cannot yet think about, name, or perceive. `wind` sits in
the percept-gap column, a word with no phenomenon behind it, waiting to be wired
to the air the climate already moves. Temperature waits for its concept and its
name. The map of what is missing is now a document the world maintains about
itself, rather than a thing a person must remember to look for.

There is a fitting rhyme in the timing. The Correspondence merged alongside
[The Foresight](./the-foresight.md), which gave the world its first planner —
the cognitive machinery that will one day consume the `Uncognized` column and
begin, concept by concept, to turn its voids into handles. One campaign built
the ledger that demands cognition; the other began building the cognition the
ledger will demand. The negative space now has something reaching toward it from
the other side.
