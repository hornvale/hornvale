# The Nettle

Grasp a nettle firmly and it does not sting; brush it and it does. The five
small problems this campaign inherited were nettles in that exact sense — each
one cheap to fix and expensive to leave, and each one recorded, months earlier,
by someone who had brushed against it and written it down.

Four of the five records turned out to be wrong.

Not carelessly wrong. Each was written at the moment of maximum context, by a
project that had just measured the thing it was describing, in language more
careful than most engineering prose. And still: one described a defect that had
already been repaired three days before the note was filed. Another asserted
that nothing in the project pinned a particular class of derived output, when
ten committed artifacts pinned exactly that. A third quoted a cost that was
real but measured on a different command than the one it was attached to. The
fourth understated the size of its own problem by a factor of five.

This campaign fixed the five problems. What it learned is about the records.

## Two kinds of claim, one filing cabinet

A project that writes things down about itself is keeping two species of claim,
and they behave nothing alike.

The first is a claim about a **particular thing**: this file writes to that
path; this function is called from these places; this value is 0.3. Its truth
lives in a location. It can be checked by looking, and it changes only when
someone changes that location. If it goes stale, it goes stale *because
somebody edited the thing it describes* — which means the edit and the staling
are the same event, performed by someone who had the claim's subject open in
front of them.

The second is a claim about **the whole**: nothing pins this output; no test
covers this case; there is no mechanism for that. Its truth is a property of
everything at once. It is falsified not by editing its subject but by editing
*anywhere* — by some unrelated effort adding the pin, writing the test,
building the mechanism. Nobody who falsifies such a claim is looking at it.
Nobody knows they have.

Both get filed the same way, in the same register, in the same tone, with the
same confidence marking. Nothing in the filing distinguishes them, and so
nothing warns the next reader that one of these two has a shelf life and the
other does not.

The two false records this campaign inherited were both of the second kind.
The one that held up perfectly was of the first kind. That is a sample of
three, which proves nothing — but the mechanism it illustrates is not
statistical, it is structural, and once seen it is hard to unsee.

## Confidence does not decay with content

The sharper half is that the *stated* certainty of a record is fixed at
writing. A claim's content can rot for a month while its confidence marking
sits at "high", because the marking was an honest description of the evidence
on the day it was made and nothing revisits it.

So the reader does not merely inherit a wrong fact. The reader inherits a
wrong fact *wearing a badge that says it was checked*. This is worse than an
unmarked guess, which at least invites suspicion. The register held two such
records, both marked high, and both were acted upon: one produced a repair to
something already repaired, the other a plan to close a gap that was mostly
closed.

The remedy is not more caution when writing. It is cheaper than that. A record
of the second kind should carry the state it was observed against, and the one
command that re-decides it — so that unparking costs *running something*
rather than *re-deriving everything*. Today the register carries five columns:
what the idea is, what its status is, how confident it is, and where to read
more. There is no column for when, and no column for how to check. That
absence is not an oversight in any one record; it is the shape of the cabinet.

## The instruments were the problem more often than the code

Five guards were built or repaired here. In four of the five, the defect that
mattered was not in the thing being guarded but in the *checking* of it, and
that pattern was consistent enough to be the campaign's real subject.

One guard refused work for *mentioning* a forbidden command rather than
running it — matching text where it meant to match action. Its repair then
introduced the mirror-image fault: a rule for stripping quoted text treated
any two apostrophes on a line as a matched pair, so an ordinary English
contraction inside one phrase could pair with a contraction inside another and
silently swallow everything between them. A genuine forbidden command,
sandwiched between two harmless remarks, became invisible. The guard reported
approval, in exactly the tone it uses when it has checked something.

Another guard was built with two halves — one to catch a new violation, one to
catch a stale exemption — and only the first half was ever made to fire. Its
test passed honestly. The second half worked, as it happened, but nobody knew
that, because the probe written to exercise it could not reach it: the probe
tripped the first assertion, which sits earlier, and the run went red for the
right reason at the wrong place. A red result had been mistaken for evidence
about the arm it never touched.

A third pattern recurred so often it stopped being surprising: a check whose
inputs did not contain the defect's precondition. Every test case for the
quote-stripping rule had at most one quoted phrase per line, and the fault
needed two. The cases were reasonable, the coverage looked broad, and the bug
lived in a shape no case had.

## The gap between two working guards

The finding that no single review could have produced came at the end, from
reading the whole of the work at once rather than each piece against its own
brief.

Two of the repairs interacted. One made a certain class of change trigger a
set of prose-checking tests that had previously been skipped. Another
introduced a guard whose list of accepted exceptions lives in a plain text
file. That file is prose by every classification the project has — and so a
change to it took the newly-built prose path, and the new guard was not among
the tests that path runs.

The consequence: deleting an entry from the guard's own exception list would
pass every local check and fail only later, on the shared machine, after that
machine had been claimed. Which is precisely the failure the first repair
existed to prevent, manufactured by the second, in the same body of work.
Each piece was correct. Each piece was reviewed. The defect was in the join,
and a join is not any one piece's subject.

## What a project can know about itself

The useful generalisation is not "be careful", and it is not "check twice".
Fourteen claims failed checking in the course of this work — the inherited
records, and then the campaign's own prose, plans, and repairs, half of them
written by whoever was coordinating. Every single one was caught by somebody
**re-deriving** it. Not one was caught by somebody re-reading it.

Re-reading verifies that a document is consistent with itself. It cannot
verify that a document is consistent with the world, because the document does
not contain the world. Only running something does that. This is why the
project's guards are executable and its conventions are not merely written
down — a rule in a program is a law and a rule in a document is advice, as one
of those programs says about itself in its own opening lines.

The corollary is uncomfortable and worth stating plainly. A project's
self-knowledge is not a store of facts. It is a store of claims with
different half-lives, and the substrate that holds them records neither the
half-life nor the date. Everything filed there is, strictly speaking, a claim
about a past state of the world, indexed by nothing.

What this campaign did about that is small: it fixed five things, corrected
two records that were false, and wrote down the distinction between the two
kinds of claim so the next reader has a name for it. Building the missing
columns — the observed-against state, the re-deciding command — is left
undone, deliberately, because a mechanism for keeping records honest is a
larger thing than five nettles and deserves its own grasp.
