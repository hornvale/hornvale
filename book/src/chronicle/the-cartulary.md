# The Cartulary

*A cartulary is the book a house copied its charters into, so their contents
would survive the loss of the originals.*

**Five recorded losses of the same material, two of them after a
verification step was added specifically to prevent it, is the number that
opens this campaign.** A campaign's decision ledger — its rulings, their
costs, its deferred and parked findings — lived in a directory git ignores
entirely, one copy per campaign's own working copy of the repository. The
prescribed remedy was to promote it into the retrospective by hand at close.
That remedy failed five times: The Ell promoted nine items and lost six; The
Quoin wrote the promotion instruction down beside the material and lost it
anyway; The Gallery/Lodestar wrote a 193-line retrospective, skipped its own
verification step, and belief disagreed with `grep` on eleven counts; The
Overture lost nine decision records when its working copy was reused for the
next campaign before close, reconstructed from module doc comments; The
Attestation lost nine deferred minors and two parked findings when its
working copy was reused *between the merge landing and the close walk*,
reconstructed from a session transcript.

The Quoin and The Lodestar are the two that matter most, because both had
already tried the obvious fix — verification that reads the ledger back and
confirms each item landed — and it did not hold, for a reason no amount of
verification can address: **the ledger was gone before anything could check
against it.** A check that requires an artifact to exist cannot detect that
artifact ceasing to exist.

This campaign stopped patching the remedy and moved the ledger. From Task 1
onward, `docs/superpowers/ledgers/2026-08-30-the-cartulary.md` is a
committed, per-campaign document, written to as rulings happen rather than
promoted from scratch at the end. **This chronicle and its retrospective are
themselves reconstructed from that file** — the first time in this
campaign's history the source document for a close was something other than
memory, a session transcript, or a scramble through module doc comments.

## Two ledgers, not one

Plan-writing found something the spec did not know: there are two scratch
ledgers, not one, with different owners. `decision-ledger.md` — rulings, Q
entries, ideonomy passes — is defined by this repository's own
`campaign-autopilot` skill and can be repointed. `progress.md` — task state,
fix rounds, **the deferred minors and parked findings The Attestation
actually lost** — is defined by the vendored superpowers plugin, which this
repository cannot edit and which the next plugin update would overwrite if it
tried.

The fix was not to mirror one into the other — that reintroduces the manual
copy step this campaign exists to remove — but to make the committed ledger
the *primary* home for the durable kinds from the start, written there
directly by the controller through this repository's own dispatch and
closing skills. `progress.md` keeps its narrower job, untouched: it does not
need to survive a working copy being handed to a later campaign, because
that only happens once a campaign is over, and its task-completion facts
are recoverable from `git log` regardless.

Re-pointing those skills (Task 4) produced the campaign's own thesis in
miniature: an instruction that reads correctly can still find nothing, if
the location it names has moved and the instruction hasn't. The closing
skill's step 2 still told a closer to route ledger entries by grepping the
scratch directory — exactly where the material no longer lived. The fix
split the step into two named halves (the scratch sweep, for what still
dies; a direct read of the committed ledger, for what now survives) with the
failure mode stated before either half — and the sibling sweep it prompted
found a second, unrelated instance of the same stale-mechanism pattern in a
Quick Reference table that a narrower fix would have missed.

## The check, and the hole it admits to rather than hides

A ledger nobody writes is worse than one that dies, because it looks like a
record. Task 3 built the ratchet: a campaign whose spec and plan both exist
must also have a non-empty ledger, matched by exact slug against
`docs/superpowers/ledgers/<slug>.md` — never by listing the directory, so a
stray `README.md` dropped in beside the ledgers is invisible to the check by
construction rather than by an exclusion rule someone has to maintain.

The reviewer did not reason about the check's blindness — it demonstrated
it, by constructing a spec/plan pair whose names defeat the exact-slug
matcher (the `the-deed-design`/`the-deed-state` shape the code's own doc
comment already names) and watching the check pass clean, missing it
entirely: never flagged as missing a ledger, never exempted, simply unseen,
permanently. The code's own doc comment already stated this blindness in the
harsh, honest form. What read as reassuring was the *report's* summary —
"safe for the ratchet's correctness," a narrower claim than it sounded like,
true only of the exemption list's internal consistency.

The fix does not close the hole. It freezes the count of unmatched
spec/plan pairs (54) as its own ratchet, the same frozen-count idiom
`tropes check` and the timings baseline use, so a new unmatched campaign
moves the count and reddens the day it happens instead of vanishing into an
instrument that was never looking. `==`, not `>=`, because the reviewer
checked by hand that `>=` would have let its own probe pass (55 ≥ 54) —
this population mixes legitimate spec-less growth with real matcher misses,
so a rise cannot be waved through by rule the way an append-never
reservation count can. The fix's own doc comment says exactly what it
guarantees and no more: *"This test does not close the hole ... it only
makes the count that hole hides in impossible to move quietly."*

## H1 — confirmed, and on firmer ground than the task claimed

The campaign's one falsifiable stop-condition: does a committed ledger
actually survive its working copy being handed off to a later campaign?
Task 5 simulated The Attestation's loss without touching the live campaign's
own checkout — a fresh clone of `campaign/the-cartulary` at HEAD into a
throwaway directory, then destroyed. A clone starts with none of the
git-ignored scratch history a campaign accumulates, which *is* the
simulation. The ledger arrived complete — 424 lines at the time,
byte-identical to `git show HEAD:...` in the live checkout — readable as an
ordinary file with none of that scratch machinery anywhere in sight.

The review went further than the task claimed for itself. It read the
actual mechanism this project uses to hand a finished campaign's checkout to
the next one and found the clone simulation is not a *weaker* analogy for
that recycling, as the task had assumed — it is a *harder* one: the real
mechanism only ever fires on an already-merged branch and switches branch
inside a shared object store, while the simulation fully disconnected from
the repository and tested an unmerged branch. **H1 is confirmed on stronger
ground than the task claimed**, which is the shape of finding this campaign
wants more of: not a result overturned, but a result's own margin measured
and found wider than reported.

## H2 — the recursion, and the finding underneath it

The spec's second hypothesis — that committing the ledger does not change
what gets written into it — was preregistered alongside its own author's
prediction that it does, slightly, and an admission that this was not
expected to be measurable from inside a single campaign. Task 5's honest
first answer was "unmeasurable, as preregistered." Review pushed on
*why* it was unmeasurable, and the answer is not flattering to the spec:
preregistering "I expect not to measure this" and then building no
instrument at all guarantees an unmeasurable report whether or not the
hypothesis is true. That is a methodological error in this campaign's own
spec design, not a limit of Task 5's execution of it — see decision
[0492](../../docs/decisions/0492-a-preregistered-hypothesis-needs-a-preregistered-instrument.md).

A cheap instrument existed the whole time, sitting unused: this campaign's
own ephemeral scratch task reports (the old regime) against their matching
entries in the committed ledger (the new regime) — same task, same author,
minutes apart. Applied post hoc to Task 5 itself, it found one line. The
scratch report carried a self-critical sentence that never reached the
committed ledger:

> H2's honest "unmeasurable" answer is only as trustworthy as the
> preregistration itself — since the prediction and the "expect
> unmeasurable" clause were both written into the spec by the same author,
> one could argue the unfalsifiability was baked in rather than discovered.

Every other line of mechanical content — commands, output, the H1 verdict,
the H2 verdict — carried through from scratch to committed ledger nearly
verbatim. This one, specifically, did not. **That is one confounded
observation consistent with H2's own prediction, not a confirmation of it**:
an unblinded single pair, one author, one sitting, cannot distinguish
"committed, therefore softened" from "second pass, therefore tightened, and
the second pass happens to be the committed one." Reported with that
limit stated rather than sold past.

**The recursion is the campaign's actual finding.** It built a durable
ledger specifically so candid material would stop being lost, and then lost
a candid line *from that ledger*, in the one task built to test whether the
ledger works — and caught it, because the fix for "the candid line was
softened out" was itself checked, side by side, word for word, against the
version it replaced, to confirm the remedy had not shipped a softened
version of the very sentence it existed to restore.

## A third instance, found by the whole-branch review, and it was still in the tree

The two instances above were both caught by this campaign itself. A
whole-branch review found a third, and its own framing is the point: the
skill re-pointed to fix the ledger's location (Task 4) deleted the bullet
that routes a deferred minor to a named home, and did not replace it — a
ledgered deferred minor, the ordinary case under the new regime, had no
routing instruction left at all. This campaign's own three deferred minors
(a missing ideonomy field, an undercounted citation, a loosely characterised
count) reached no recorded disposition in the first draft of its own
retrospective. Nothing material was lost this time; two had already been
quietly discharged and one had partially landed in a doc comment — which is
exactly the state the five prior losses this campaign exists to prevent were
in, before the loss each is now remembered for. Restored, with the three
given a home in this campaign's own retrospective.

The same review found the load-bearing "shared filename" argument had been
citing the wrong file in five places (the per-campaign `progress.md` where
the actually-shared `decision-ledger.md` was meant, corrected by decision
0493) and that the exemption list's stated "a new campaign cannot add
itself" was asserted and not enforced, proven by a mutation the review ran
and this campaign then reproduced. Both are corrected below.

## What shipped

| what | mechanism |
|---|---|
| the ledger stops living only in scratch | `docs/superpowers/ledgers/<slug>.md`, committed from Task 1, one file per campaign |
| the hook and `CLAUDE.md` claims that made committing "always a mistake" | superseded visibly, quoting the retired sentence before replacing it |
| a campaign with a spec and a plan but no ledger | a ratchet in `docs_consistency.rs`, resolving by exact slug, never by directory listing |
| the exemption list's own exact-match blindness | frozen at 54 unmatched spec/plan pairs — a visible ratchet, not a closed hole |
| two ledgers, two owners | the committed ledger is primary from the start; the plugin's `progress.md` is untouched and unmirrored |
| the in-repo skills' stale ledger pointers | re-pointed in Task 4, with the sibling sweep catching a second stale instance |

Eight decisions were ratified, 0486–0493: [0486](../../docs/decisions/0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md)
(the ledger is committed, not scratch),
[0487](../../docs/decisions/0487-per-campaign-ledger-paths-are-what-make-committing-safe.md)
(per-campaign paths make committing safe — superseded by 0493, below),
[0488](../../docs/decisions/0488-a-record-that-must-be-manually-copied-to-survive-will-not.md)
(a record that must be manually copied to survive will not),
[0489](../../docs/decisions/0489-verification-cannot-substitute-for-durability.md)
(verification cannot substitute for durability),
[0490](../../docs/decisions/0490-two-ledgers-two-owners-the-committed-one-is-primary-from-the-start.md)
(two ledgers, two owners),
[0491](../../docs/decisions/0491-a-stated-blindness-gets-a-visible-ratchet-not-a-silent-fix.md)
(a stated blindness gets a visible ratchet, not a silent fix),
[0492](../../docs/decisions/0492-a-preregistered-hypothesis-needs-a-preregistered-instrument.md)
(a preregistered hypothesis needs a preregistered instrument), and
[0493](../../docs/decisions/0493-decision-ledger-md-not-progress-md-is-the-shared-scratch-file.md)
(`decision-ledger.md`, not `progress.md`, is the shared scratch file — the
whole-branch review's own correction to 0487, minted from the same reserved
block rather than editing 0487's substance). Two numbers in the reserved
block, 0494–0495, were not needed and are left unused.

## What is still owed

**Reports and reviews still die with their scratch home, on purpose** — the
spec's own §2 measured scratch prose at Task 1 across three checkouts and
called it "remarkably stable at ~200 K per campaign." Later re-measurement
inside this same campaign found that word false: six checkouts at Task 6's
close spanned 80–360 K, and the whole-branch review's own pass across
thirteen found a 95x spread, 4 K to 380 K. The mean sits near 200 K; the
argument built on it does not depend on the word "stable," and survives the
correction — the outliers are almost entirely on the *regenerable* side
(one checkout's review packages alone ran to 58.9 MB), which is exactly the
side this design already treats as disposable. The committed ledger's own
share averaged 15–19 K across those same samples, over a 12–32 K range —
and this campaign's own ledger closed at 41 K, above that range's ceiling,
so read those as a floor rather than a size. Whichever figure is taken it
is the small share: the implementer reports and review packages that still
die with their scratch home are the large majority of that prose in every
sample. That gap is banked as its own registry row rather than a task,
because it is a deliberate, costed boundary this campaign drew on purpose,
not an oversight.
