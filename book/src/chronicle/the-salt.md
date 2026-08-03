# The Salt

**August 2026 · outcome: merged — derived prose stops reading a serial number,
and the instrument that was supposed to prove it was blind to the largest
case**

## What was attempted

Everything in a world is minted with a number: 1, 2, 3, in the order it was
made. That number is an identity, and nothing more. But four derivations were
reading it as though it described something — a ruin's rubble, a founder's
name, which layer of a buried settlement lies deeper, which of two victims a
conqueror drove off. Add ninety people to a world and every number after them
shifts, and a ruin three continents away fills with different potsherds. Nothing
about that ruin changed. Only its serial number did.

The Salt is the middle campaign of three. The Scaffold split the bake's private
handle from the ledger's permanent one; The Signet will change how the numbers
are handed out. The Salt exists between them for a reason that is about
reviewability rather than correctness: The Signet renumbers everything, and if
prose still keys on numbers, its diff carries both kinds of change at once and
no reviewer can tell "a number moved" from "a ruin's contents moved because a
number did."

## How the work was scoped, and why it was not by grep

The channel population was established by an **id-shift rehearsal**: a throwaway
probe burned a thousand extra mints at the top of the build, shifting every
entity id in the world, after which the whole artifact set was regenerated and
diffed against a verified-clean baseline. Whatever moved was coupled to id
derivation by construction. This was chosen over a grep because The Scaffold's
own retrospective had just recorded three of four grep-and-trace artifact
predictions coming back wrong.

Three artifacts moved. One was prose — a single line of `history-seed-42.md`,
*a hut and a granary* becoming *two huts and a granary*. Two were the possession
transcripts, where the numbers are printed as numbers: `settlement/7/name`
became `settlement/1007/name` while the name itself, *Godogododaga*, stayed put.
Those two are genuinely id-valued output and belong in The Signet's diff, not
this one. Everything else held: three almanacs, the book, `explain`, every
reference dump, the chorus study, the type-audit report. Settlement names are
salted by cell and deity names were re-seeded from phenomenon identity by an
earlier campaign; both were already free.

**And the rehearsal was blind to the largest channel in the campaign.** Every
person name in the world is drawn off a founder handle that mixed the
occupation's entity id into the world seed. That channel surfaces only in The
Namesake's name-prefix metrics, which live in the **census fixture** — and
`regenerate-artifacts.sh` excludes censuses by design. Measured directly
instead, with the published metric code: **18 of 20 world-rows moved** under the
same shift the artifact sweep had just called clean.

That is the campaign's first finding, and it generalises past this campaign. A
measurement instrument has a population, and "the committed artifacts" is not the
same population as "everything this change can move." The census fixtures are
committed, drift-checked, and invisible to the sweep that exists to catch drift.

## The Scaffold's deferral did not work

The second finding was that this campaign's scope had been drawn one item too
small — by the campaign before it, in a doc comment that said so plainly.

The Scaffold replaced the stratigraphy comparator's mint-order tie-break with
material facts, but its *fourth* key ordered by the predecessor occupation's
`EntityId`, and its own documentation called that "a compromise, not a material
fact … itself a mint-order artifact," adding that "a future encoding that gave a
founding its own material identity (a 'signet') would close this gap." The gap
was handed forward to The Signet.

Handing it forward defeats the sequence. A campaign that renumbers everything
while the stratigraphy comparator still reads a number produces exactly the
unreadable diff the three-campaign split was built to avoid. The fix belonged
one campaign earlier, and it cost almost nothing: keying the tail on the
predecessor's own founding coordinates instead of its id changes the rendered
layer order at **one site across seeds 42, 7 and 1000**.

## What shipped

**Two keys, split by causal horizon.** The type the campaign needed already
existed: `Occupation` — the core The Scaffold carved out as "the facts both
sides agree on" — carries people, site, founding day, ending day, peak
population, tech, function, deity, tongue, cause and notability, and **no
`EntityId` at all**. The three id-bearing fields live on the wrapper. So the
work was two derivations over a type that was already clean.

`material_key` mixes the whole core and feeds the flesh: a ruin reflects the
size a place reached and the way it ended, not only its founding.
`founding_key` mixes only `(people, site, founded)` plus one hop of ancestry —
the predecessor's same triple — and feeds the founder handle behind every person
name. The split is not fastidiousness. Keying a founder on the whole core would
make a person's name a function of how their community later died, which is
causally backwards, and the ancestry hop is what recovers the discrimination
that excluding the ending costs: **8.4% / 3.3% / 3.6%** stem collisions across
the three seeds, against **27.7% / 14.8% / 16.2%** for the founding triple
alone.

**Every public signature stayed put.** `founder_of`, `flesh_seed`,
`forebear_of`, `clan_root_of` and the lab's name renderer all still take an
`EntityId`; it is simply a lookup key now, used to read material facts back off
the ledger and never read for its value. That rule — *an `EntityId` may be
stored, compared and looked up; it may not be read for its value* — is the
campaign's durable statement, and a source scan enforces it over the four
derived-prose files.

**Collisions are the output, not a defect.** Two occupations identical in every
material fact now leave identical remains. Under the id they left *different*
potsherds, and the difference came from nothing but mint order — entropy
fabricated where the world had none. The measured cost is smaller than the
principle: **1.0% / 0.2% / 0.3%** of occupations sit in a colliding
material-core group, and **0.0%** of the layers that actually render flesh
collide at any of the three seeds.

**A tie-break that had never fired.** `conquest_victim` chose among candidate
victims by lowest entity id, under a doc comment asserting a conqueror can have
at most one. Verified rather than trusted: **1718 candidate calls across the
three seeds, maximum candidate-set size one**. The tie-break survives, keyed on
the victim's own site and founding day, so that if a second candidate ever does
appear the choice is a property of the world rather than of commit order.

## The result, and the campaign it complicates

The Namesake shipped a falsification: its shortest-prefix render rule, which
spends extra name elements only when a name would otherwise be ambiguous, was
found to be "priced for a collision rate the name generator declines to
produce." Person stems were near-unique, so the rule almost never fired.

Re-keying founder handles raises that collision rate to 8.4%, which was
**chosen with the numbers in hand** rather than discovered afterwards — the
alternative keys and their costs went to Nathan as a ruling before the code was
written, and this consequence was preregistered as a prediction to report.

The prediction holds, and the magnitude is the interesting part. The share of
founders whose name renders in exactly one element against their own site falls
from **0.9956 to 0.9586**, and the share spending every element they carry rises
from **0.311 to 0.343**. So the rule now fires for about one founder in
twenty-five where before it fired for about one in two hundred and fifty — a
tenfold increase off a near-zero base. Every one of The Namesake's preregistered
targets keeps the same verdict: settlement scope still passes, region full-stack
still passes, region-scope median still fails at 1.1 exactly as it did before.

The honest summary is that The Namesake's falsification is **softened, not
reversed**. The rule is doing more work than it was and still very little.

## One artifact that did not move, and why that needed checking

`history-seed-42.md` was predicted to move and did not. The Scaffold's
retrospective had just finished arguing that a predicted artifact move is itself
a claim wanting verification, so this one was traced rather than accepted. The
seed for cell 1400's rendered layer genuinely changed —
`Seed(11388647889657673426)` under the id key, `Seed(10641468697408252209)`
under the material key — and `structures_of` returns `[Hut, Granary]` under
both. The re-key took effect; the dwelling-count draw simply lands the same
either way. A coin flip, checkable in one probe, and not evidence of anything
having failed.

## What it leaves reserved

**The Signet is next**, and its diff should now contain only the two possession
transcripts plus whatever the renumbering itself touches. Anything else that
moves is a channel this campaign missed.

`layer_key` is **no longer a total order**, deliberately. Two occupations
matching on every material fact including their predecessor's founding
coordinates now tie — measured at five such pairs across the three seeds — and
the three decoders agree on a tie only because `sort_by_key` is stable over one
shared ledger iteration order. That order is *commit* order, which no change of
id derivation moves, which is precisely why a key that ties is safe under The
Signet. It is also an invariant that two campaigns have now flagged and neither
has enforced, and it carries a registry row of its own.

One open question is recorded rather than answered: decision 0084 declined an
epoch for a derivation whose renderer "commits nothing," which describes the
flesh seed exactly, yet this campaign bumped `history/flesh` to `/v2` all the
same. The difference — 0084's measurement came back byte-identical, and this one
did not — is written into the epoch roster beside the row, for whichever
campaign next moves a derivation the ledger never sees.
