# The Hearsay

Decision 0100 ratified three registers for world data. *Fact* is committed and
contradiction-checked. *Phenomenon* is derived and coherent by construction.
*Myth* is derived, free, evictable, and — the clause the record spends most of
its argument on — **not required to be coherent, with or without fact or other
myth**. Then, in a sentence the campaign is named for: *"Myth is new, and has
no channel today."*

This campaign built the channel, and discovered in the process that one half of
what it set out to measure was not measurable at all.

## What a claim is

A **claim** is what somebody holds to be true: a holder, a subject, a
predicate, an object, a provenance grade, and a hop count. It is derived and
never serialized. That is not an implementation convenience — 0100 rule 5
forbids committing a balance, and a stored belief is a balance with no
invalidation story. A claim set is recomputed from committed facts every time
it is wanted, and costs nothing when it is not.

The holder is non-optional, which is 0100 rule 2 expressed in the type system:
*"the dwarves say X, the goblins say Y"* is texture, while a bare `X ∧ ¬X` is a
defect, and the difference between them is entirely attribution. There is no
way to construct a myth here that does not carry someone to hold it.

**Provenance** is the epistemic grade — witnessed, taught, inferred — and it
moves in one direction only. Being told that someone saw a thing is not seeing
it, so transmission maps `Witnessed → Taught` and nothing maps back. That
single anti-symmetry is what makes a rumour decay across retellings rather than
strengthen, and it is deliberately distinct from `domains/language`'s
`Evidential`, which asks the different question of whether a tongue can
*grammatically mark* the distinction. A tongue with no evidential category
leaves provenance linguistically invisible without making it any less true.

## The world already had the tree

Nothing needed generating. A community founded from another is linked in the
ledger by `occ-founded-from`, and on seed 42 those links describe 704
occupations — 46 founded at a bare site, 658 descended from a parent — reaching
21 generations deep with a median of 8.

The predicate is a sum type, and reading it as a plain parent pointer is wrong
in a way that flatters the data: a `Number` is a *site*, meaning genesis with no
ancestor, and only an `Entity` is a real link. An early draft of this campaign's
spec read all 704 as descendants and reported a tree that does not exist.

Witnessing follows the same committed record. When a village is raided it is
destroyed and its survivors refound elsewhere — the bake drives the loser off on
every raid — so the parties present at an ending are the village itself, every
child founded on exactly the day it ended, and the attacker. On seed 42, 477 of
562 child foundings sit at precisely their parent's ending day, with all three
gap quartiles at zero. The survivors and the ordinary daughter colonies separate
cleanly, with no threshold to tune.

## The measurement, and its null

Nothing in this campaign forgets. Every descendant inherits, always. So the
distribution of hop counts is not a description of memory but a **ceiling** on
it — the shape myth takes when nothing at all opposes its spread.

Two predictions were frozen before the code that would move them. Both were
wrong.

Myth was predicted to stay near its source, at a median of two hops or fewer;
the median is **four**. The tail was predicted to be thin, under a tenth of
pairs beyond ten hops; it is **15.4%**, and claims reach the tree's full depth
of 21 undiminished.

The shape is the part worth keeping. Pair counts decline **almost linearly**
with distance, where any process with a per-hop survival probability produces a
geometric curve instead. A straight line is the fingerprint of nothing being
lost. The baseline is therefore diagnostic and not merely prior: a later
campaign's forgetting will be legible as a *bend*, and its presence detectable
before its rate is estimated.

## What could not be measured, and why

The campaign also set out to measure independent corroboration — whether the
communities holding a story about an event are separate sources or one source
wearing many mouths. It failed three times, and the failures are the more
useful result.

The first measure could not vary: with claims reaching holders by a single
route, every event had exactly one origin, and the ratio was a constant dressed
as a finding. The second and third were topological repairs, each plausible,
each scoring the motivating scenario — a village raided, its survivors founding
two successors whose lines diverge forever — at zero, for different reasons.

The cause is one level above all three. Corroboration is **semantic**: it needs
accounts that *could* differ. Content here is carried unchanged by
construction, distortion being a later campaign's, so no two accounts can
disagree, agreement is constant-true, and every structural measure was standing
in for a property with no variance. Sensor fusion states the same thing in a
line — two sensors are independent to the extent their *errors* are
uncorrelated, and at zero error the correlation is undefined. What the three
measures were computing, accurately and uselessly, was redundancy.

Underneath sits a type error worth naming. Corroboration is symmetric; ancestry
is a partial order and therefore antisymmetric. Filtering a witness set by
ancestry cannot express a symmetric property in either direction. The symmetric
relation available inside a partial order is *incomparability*, and a set of
pairwise-incomparable witnesses is an **antichain** — which for a raided village
and its two survivors is exactly the pair the scenario is about, and which none
of the three attempts computed.

## What the world turned out to be like

One finding arrived unlooked-for, from checking whether attacker and victim
lineages were disjoint.

**They are almost never disjoint.** On seed 42, 210 of 234 attributed raids —
89.7% — fall between communities that share an ancestor, and 217 of 234 — 92.7%
— between communities of the *same people*, in a world holding fifteen distinct
peoples.

Hornvale's violence is intramural twice over. Communities fission from common
stock and spread locally, so a neighbour is usually a cousin, and raiding is
overwhelmingly a family affair. The world has fifteen peoples and no
meta-ethnic frontier anywhere in its conflict record — which means the
precondition that persecution concentrates at aligned cleavages, and that race
is manufactured to legitimate extraction, is simply absent. Under decision 0021
no ideology ranking peoples may ever be an input, only a generated output; this
measurement says the world does not currently generate the conditions that
would produce one. Contact must precede prejudice, and contact is what the
world is short of.
