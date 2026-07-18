# The Ordination

Somewhere in `windows/worldgen/src/lib.rs` sits a function called `build_to`,
and it has always run genesis in the same order: sky, then terrain, then
settlement, then culture, then religion, then species, then a deep-time
paleoclimate pass. That order is correct — settlement really does need
terrain's elevation before it can place a town, religion really does need
culture's castes before it can size a cult — but nothing in the code *says*
so. The order is correct because a person wrote it correctly, the same way a
hand-copied manuscript is correct because the scribe was careful. Every
domain this program has touched, it has moved from a person's care to a
declared fact the system checks for itself. This campaign does that for
order: to *ordain*, in the word's oldest sense, is to put in order — and the
claim this campaign makes is that genesis's order was never decreed, it was
always waiting to be *derived*.

## A schedule is a topological sort

Each genesis stage gets a `System`: a stable label plus two sets of predicate
names, the ones it reads and the ones it writes. Nothing more — a
declaration, not a behavior, the same declarative posture every ECS campaign
in this program has taken toward its subject. A capability schema is a
collection of these declarations, and a schedule is what falls out of them
mechanically: draw an edge from writer to reader wherever one system's writes
intersect another's reads, and the schedule is a topological sort of the
resulting graph — Kahn's algorithm, ties broken by ascending label rather
than the order anyone happened to list the systems in (the same
positional-tag bug this program has refused at every layer below this one).
Feed it the real eight declarations — `world-entity`, `sky`, `terrain`,
`settlement`, `culture`, `religion`, `species`, `paleoclimate` — and it
returns one fixed, drift-checkable sequence, pinned now as a test: `culture,
sky, species, terrain, world-entity, paleoclimate, settlement, religion`.

That is not the hand-order, and it should not be. The label tie-break has no
reason to reproduce a sequence a person chose for readability; it only has to
respect the *dependencies* a person happened to get right. Five stages —
`culture`, `sky`, `species`, `terrain`, `world-entity` — read nothing from
the ledger and so start the graph with no incoming edges at all; the
scheduler places them alphabetically because nothing else constrains them.
The genuine dependency chain — `world-entity` and `terrain` feed
`settlement` and `paleoclimate`; `culture` and `settlement` both feed
`religion` — is what actually shapes the rest. Two orders, over the same
graph, both valid. The interesting claim was never "these orders match." It
was "the hand-order respects every real dependency, and the schedule can
prove it."

## The keystone, and its negative

**SCHEDULE ≡ HAND-ORDER** is this program's fourth keystone (after
INDEX≡SCAN, JOIN≡SCAN, and the kind ⋈ instance query), and it is a subtler
claim than its predecessors, because equality here does not mean *identical
sequence* — it means *the hand-order is a valid linearization of the same
DAG the schedule derives*. The positive half is a single assertion:
`is_valid_order(GENESIS_HAND_ORDER)` walks every declared edge and confirms
the pipeline that ships today never runs a reader before its writer.

A test that only checks the positive half is a test that would pass on
declarations with half their edges missing — an under-specified `reads` set
looks, to a topological sort, exactly like independence. So the keystone
ships with its anti-vacuity negative: swap `terrain` and `settlement` in the
hand-order — settlement reads terrain's elevation, so this is a genuine
dependency violation — and `is_valid_order` must reject it. If that negative
test ever passed, it would mean the `settlement` declaration had quietly
stopped claiming `terrain`'s outputs as a read, and the positive test would
have been proving nothing. A construction check without its recognition
check is not a check; this is the same lesson the ledger's own contradiction
logic has always leaned on, applied here to a graph instead of a value.

## Genesis is tick zero

The deeper question this campaign had to answer before writing any code was
what "the bulk-synchronous tick made concrete" could possibly mean when no
running simulation and no sim-dynamic system yet exist to schedule. The
answer that dissolved the question: **genesis already *is* a tick.** A
running world would rotate a cycle — read a frozen snapshot of tick *N*,
let every system compute its writes against that unchanging view, commit
tick *N*+1 in one deterministic pass — and genesis is exactly that cycle's
first half-turn, run once, from a seed, with nothing yet to read. The *age*
of a system (build-static, run once at world creation, versus sim-dynamic,
run every tick going forward) turned out to be orthogonal to the *mechanism*
that runs it; genesis is simply the mechanism's age-zero instance.

So the campaign builds the machinery in earnest — a `TickSystem` trait
(`step(&frozen_ledger) -> Vec<Fact>`) and a `tick` function that runs every
system against one frozen ledger and commits their combined writes into the
next, in schedule order, then by stable label — and proves it with a toy.
Two systems each write a fact recording how many facts they saw in the
frozen ledger; both report zero, because neither sees the other's write
until the *next* tick. That one assertion is the whole simultaneity
guarantee an agent-based model needs to be deterministic rather than a race:
every system's-eye view of the world is frozen for the duration of one
tick, no matter what order its commits land in afterward. No real domain
implements `TickSystem` this campaign — genesis keeps running through
`build_to`'s hand-written calls, not through `tick` — but the mechanism it
would eventually run on now exists, tested, and waiting.

## A check that was run, not assumed

The metaplan's seventh section has always named a rule genesis was supposed
to obey: no *functional* predicate — one where a subject may hold only one
value — should have two systems both declaring it in their writes, because
two same-tick writers of a fact that can only be one thing is exactly the
kind of race this whole program exists to make unrepresentable. Nothing had
ever checked whether genesis actually followed that rule. It would have been
easy to write the check, run it once by eye against the declarations,
declare it satisfied, and move on — an assumed-green check is a check with
no evidence behind it, and this program has learned, more than once, that
the gap between "should be true" and "is true" is exactly where the real
bugs live.

So the check was built to mirror the load-time referential-integrity check
worldgen already runs (fails loudly, at load, before a bad world can be
built), and it was run for real against the genuine eight declarations. It
found something: `name-gloss` — a functional predicate, 72 facts in a
seed-42 world — declared as a write by *both* `settlement` and `religion`.
A real violation, on paper. A settlement gets a name-gloss; so does a deity.
Both commits are genuine; neither declaration is wrong. But they write
through one shared composition-root helper onto two entirely disjoint kinds
of subject — a town id never collides with a deity id — so the predicate
that looked doubly-written was never actually contested for any one
subject. The check had done exactly its job: surfaced a real finding,
rather than confirming an assumption nobody had tested.

The resolution, Nathan's ruling, sharpens the program's ownership model
rather than special-casing around the finding. A predicate is *defined* in
exactly one place — the registry already enforces this, an error on
re-declaration — and `name-gloss` is cross-cutting enough (any domain that
names something might need to gloss it) that its one true home is the
kernel, beside `name` and `instance-of`, not any one domain. Once it moved
there, the single-writer contract itself gained its needed shape: a
*domain-owned* predicate keeps its one writer, but *kernel-core* predicates
are shared infrastructure, legitimately written by more than one system as
long as their subjects stay disjoint — a fact this coarse, per-predicate
check cannot see, but does not need to, because a finer invariant already
covers it.

## Why the exemption is safe, not merely convenient

Two things came out of pushing on this finding harder than the ruling
strictly required, and both belong in the record because they explain *why*
the exemption is sound rather than just declaring that it is.

The first: the single-writer contract, as written, is a check over
*predicates*. The real invariant — the one that actually prevents a
same-tick write conflict — is over *(subject, predicate)* pairs, and the
ledger has enforced that finer invariant unconditionally since this
program's second campaign, as an ordinary part of committing a fact: two
writes to the same subject's same functional predicate are a contradiction,
full stop, no matter which system produced them. The schedule's
per-predicate contract is therefore a *coarse static proxy* for an invariant
already guaranteed, precisely and dynamically, at commit time. `name-gloss`
trips the proxy without ever coming near the real thing it stands in for —
which is what makes exempting it a correction to an over-eager
approximation, not a hole poked in the actual guarantee.

The second: `name`, `instance-of`, and now `name-gloss` share a property
that is not incidental. They are *hubs* — predicates every domain might
plausibly want to write, because naming and identity are cross-cutting
concerns rather than any one domain's private business. The single-writer
contract, by contrast, is a rule shaped for *domain-owned leaves* — a
predicate like `species-name` or `deity-name`, which belongs to exactly one
domain by construction and should never see a second writer. A
high-connectivity hub predicate does not fit that shape by nature, not by
exception; kernel-core status is the honest name for a category the
contract was always going to need.

## The shadow this campaign keeps

Genesis does not run through the derived schedule. It cannot yet — the
seven real stages take seven different shapes of input, and a uniform
`step(&frozen_ledger)` signature is the running tick's job, not this
campaign's. So the schedule *validates* `build_to`'s order; it does not
replace it, the same shadow posture every campaign in this program has held
before cutting anything real over. Nothing this campaign adds is
serialized: no new fact is committed, no new predicate is registered that
was not already registered somewhere (`name-gloss` moved, from worldgen to
the kernel — the same string, the same functional flag, defined exactly
once either way), and every world, almanac, and census fixture this
campaign touches comes back byte-identical to the commit before it started.

What remains is named, not hidden. The running liveness tick — the actual
GOAP and belief and social systems that would generate new sim-time facts,
turn by turn, without a person driving them — needs those systems to exist
first, and a consumer able to render what they produce; both are future
arcs. Genesis executing *through* the derived schedule needs the uniform
`step()` the running tick will define. And the kernel-core exemption's own
edge, worth stating plainly: it is safe at genesis because settlement and
deity subjects are disjoint by construction, but a *running* tick where two
systems might someday rename the same subject in the same tick would need a
finer, per-subject check the static schema cannot express — the ledger's
commit-time guard would still catch it, but only after the fact, not before
the tick runs. This program has one substrate campaign left after this one,
spatial partition, before the schedule and the mechanism this one built
have anything real to run.
