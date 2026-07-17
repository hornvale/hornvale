# The Concordance

The ledger was an honest list. Facts went in the back and stayed in the order
they arrived; to answer a question you walked the whole thing. That is fine for
a village and ruinous for a world, because the cost is hidden in the one
operation nobody watches: committing a fact first asks whether the ledger
already holds it, and whether it contradicts a value the subject already has —
two linear scans. Every commit reads every prior fact, so building a world of
*n* facts costs on the order of *n²*, and the census, which builds a thousand
worlds, would feel it as a wall. The metaplan named this the prerequisite: the
indexes must land before anything runs at census scale.

A concordance is the old answer to the same problem — an index of every term and
every place it occurs, so a reader never rereads the book to find a word. This
campaign builds one for the fact ledger, and builds it as a *derived view*: a
structure the world is never saved with and never loaded from, only rebuilt from
the facts themselves. The world does not change. The speed does.

## Three rotations of one triple

A fact is a triple — subject, predicate, object — which is the shape a triple
store has indexed for decades. The proven form is three ordered maps, one for
each rotation of the triple: **SPO**, **PSO**, **OSP**. Each answers one family
of question in logarithmic time — everything about a subject, everything with a
predicate, everything pointing at an object — because in each map the known part
of the query is the prefix of the key. Three rotations are a symmetry group over
the three positions, and together they cover every "two known, one unknown"
pattern the ledger is ever asked.

The values in each map are not facts but *positions* — ascending indices into
the one true `Vec` of facts. This is the load-bearing detail. A query gathers
positions, sorts them, and reads the facts back in commit order — the exact
order the linear scan produced — so every consumer that renders a query result
sees byte-for-byte what it saw before. The index made the answer faster without
moving a comma of the output.

The commit path, the actual bottleneck, is fixed twice over: the "have I seen
this fact" scan and the "does this contradict" scan both become a single lookup
into the subject-predicate prefix. Building a world drops from *n²* to
*n* log *n*.

## The predicate, interned

A predicate is a string, repeated across thousands of facts and compared
character by character in every hot loop. The compiler's answer is a symbol
table: intern each distinct predicate once to a small integer, and compare
integers thereafter. The intern table is build-state — the predicates come from
a fixed registry — and, like the indexes, it is never serialized; the integers
are an in-memory convenience whose values no saved byte depends on. The fact on
disk still carries its predicate as text. Only the index keys are interned.

## The index that is provably the scan

The whole speed argument rests on one claim: that the index returns *exactly*
what the linear scan returned, always. So that claim is the keystone test.
Generate random ledgers from a seed, and for every query shape assert the
indexed answer equals the naive scan's answer, fact for fact, order for order.
The naive scans are kept, not deleted — they are the oracle the index is
measured against, and the baseline the benchmark times against.

The keystone earned its keep. It held for subjects, predicates, and object
references, but the first version quietly failed on a single value: signed zero.
A quantized `-0.0` and `+0.0` are equal under the arithmetic the scan uses, but
the index ordered them apart, so the two paths disagreed about whether a fact
was a duplicate — a disagreement no live world triggered, and none should, but
one the keystone must forbid absolutely, because the entire edifice is the
promise that index and scan are the same function. The fix canonicalizes the
sign of zero inside the index key alone, where the ordering lives; the saved
byte is untouched. The property test grew a numeric case so the hole cannot
reopen unseen.

## What did not change

Nothing a world contains. The indexes and the intern table are `#[serde(skip)]`
— absent from every save, rebuilt on demand, incrementally as facts commit and
from scratch after a load, the two routes provably agreeing because an ordered
map does not care in what order it was filled. Every world, every almanac, the
census, every committed artifact: bit-for-bit identical across the whole
campaign. This was a change to the cost of a question, not to any answer.

That is the whole of it. The ledger keeps its honest list and gains a
concordance over it — three rotations of the triple, a symbol table for the
predicates, and a keystone that holds the index to the letter of the scan. The
commit is logarithmic, the world-build is *n* log *n*, and the census-scale work
the next campaigns need now has a substrate that will not buckle under it.
