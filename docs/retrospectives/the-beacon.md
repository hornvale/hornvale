# Retrospective — The Beacon (cross-host board, 2026-08-11/12)

Process lessons only; the product story is
[the chronicle](../../book/src/chronicle/the-beacon.md). Decision 0020 governs
the form.

Thirteen tasks (including an incident-response task, 11b, and a documentary
close, 12a–12c), roughly two dozen fix rounds, and one repository-corrupting
incident found and repaired inside the same task pair that caused it. The
campaign's own ledger is git-ignored and dies with this worktree; everything
below is what was worth promoting out of it before that happened.

## 1. Eight tests that passed for the wrong reason

The campaign kept a running tally, stated explicitly in the ledger at more
than one checkpoint ("this campaign has now found four vacuous tests, three
of them in error paths"; later, "the fifth and sixth"; later, "the eighth"),
of tests found to pass despite not exercising the defect they existed to
guard:

1. Task 2: `Board`'s liveness predicate had its `ahead == 0` conjunct pinned by
   nothing — mutating it to `Some(behind > 0)` left the whole suite green.
2. Task 5 review: `the_union_is_ordered_oldest_first_across_refs` was vacuous —
   dropping the sort entirely left the suite green, because synthetic ids of
   the form `{n:040}` happened to co-sort with their timestamps. Its
   red-first evidence was real but misleading.
3. Task 5 review: local-first ordering was unguarded — reordering `read_refs`
   to list local last left the suite green while silently mislabelling a
   shared post as `Peer(...)`.
4. Task 5: a reviewer's own *proposed* regression test would itself have been
   vacuous — it used the all-zeros object id as an "absent object" fixture,
   but `for-each-ref` silently drops that value as a broken ref, so the test
   would have guarded nothing had it been written. Noted at the time as the
   third instance of the same pattern: a test for an error path is exactly
   where vacuity hides.
5. Task 6: a peer-skip test used `main` as its author fixture on the
   reasoning that `main` always resolves and is unconditionally live — a
   construction clever enough that the re-review scoped its entire pass to
   checking whether that cleverness was itself vacuous. It held under
   mutation; recorded here because the campaign treated "clever" as a smell
   worth a dedicated check, not because it turned out to be an instance.
6. Task 10: `a_suggest_post_does_not_consume_the_ambient_post_budget` could
   not observe the thing it asserted — it lived in `main.rs`, outside the
   tested path — and a mutation moving the exclusion downstream of the render
   cap left the whole suite green.
7. Task 10: a corroboration assertion of the shape `text.contains('2')` was
   vacuous three ways over against any non-empty board. Both 6 and 7 were
   authored by the campaign's own controller and caught by an *implementer*,
   not a reviewer — the first vacuous tests this campaign found where the
   catcher was not the review step built to catch them.
8. Task 11b: an identity guard was first written using `.env()` on a child
   process, which poisons only that child — so a commit made by the *test
   process itself* would have gone unpoisoned and the assertion would have
   passed for free. This one was never run wrong; it was caught by its own
   author asking **what would make this pass if the bug were present**,
   before it was ever executed. That question is the transferable artifact
   of the whole list: every other instance was found by running the
   suite under a mutation, and only this one was found by imagining the
   mutation first.

Nothing here indicts the authors. Several were in error paths specifically,
which is where a green suite is cheapest to obtain by accident: an error
path runs rarely, so a test that merely fails to construct the error
condition still exits with the "expected" outcome. The standing habit worth
keeping is #8's question, asked of every new test for a bug that has not yet
been reproduced: what result would this produce if the fix were absent?

## 2. Every fix in Task 3's four rounds overshot into a new failure mode

Task 3 batched the per-post `git cat-file` reads that dominated the board's
render cost (the chronicle's headline number: 1.69–2.05 s to 0.60 s at 32
posts). Getting there took four rounds, and each one is a direct causal
consequence of the round before it:

1. **Batching** — the real win, landed via a helper reading raw bytes from a
   single `git cat-file --batch` invocation.
2. **Deadlock** — the batching helper wrote its entire input before reading
   any output. Past a few thousand posts' worth of stdin, git's stdout pipe
   fills, stops draining stdin, and the write blocks forever: a hang with no
   timeout and no error message, on a seam that runs at every session start.
   Found by a reviewer's own probe harness and independently reproduced.
3. **A `Local` liveness inversion** — the deadlock fix added chunking guarded
   by a hard `Err` on a malformed filename. A malformed tree entry is
   untrusted repository data, not programmer error, and the project's own
   decision on this exact question requires warn-and-skip, never a hard
   fail. Pre-fix, one bad post produced a warning and a rendered board;
   post-fix, one bad post blanked the entire render.
4. **A debug panic and a silent chunk-abort** — the inversion fix moved the
   validation into a `debug_assert!`, which panics in debug builds. The
   project's own human-facing digest command always runs in debug, so the
   assert would have taken out that whole read seam the first time it fired.
   Separately, in release builds, an *ambiguous* abbreviated object id broke
   the batch's own status-token parsing, silently dropping every id still
   queued in that chunk — up to 255 posts — with no warning at all in one of
   the three variants.

A fifth round was needed purely to repair the *prose* the fourth round left
behind: a request to "soften" a safety comment, given without restating the
correct argument, produced a rewrite that inverted its own safety case — it
called the chosen chunk size "comfortably above" a 4 KiB floor when the
deadlock argument actually requires staying *under* pipe capacity, licensing
a future reader to raise the constant into the unsafe region.

The generalizable lesson, recorded at the time: each of these fixes moved in
the direction of more safety, and each one over-shot into a new failure mode.
The question that would have shortened the chain is not "does this fix work"
— every one of them did, narrowly — but **"what does this fix newly make
possible?"** The chunking fix made malformed input reachable in a new way;
the validation fix made a hard failure reachable in a new build profile; the
comment edit made a wrong safety argument publishable. Each answer was a
one-sentence check that the round skipped.

## 3. Three corrections to my own spec prose, none caught by re-reading

Three separate claims about repository history or tool behavior were written
into spec or plan text with confidence and without being checked first, and
all three were caught by a review, never by the author re-reading the text:

1. A safety comment claimed a chosen buffer size was "comfortably above" the
   size the deadlock argument needed as a floor, when the argument actually
   needed the opposite inequality — see §2 above. Caught by a reviewer who
   measured the real pipe capacity (65,536 B) rather than accepting the
   claim.
2. A spec amendment asserted that sync age was "the only remaining signal"
   that a frozen peer's content had gone stale. It measures how long since
   *this host* last fetched, not how old the peer's content actually is, so a
   host that syncs on a healthy schedule would report a month-dead peer as
   fresh forever — the exact scenario the amendment was arguing from. Caught
   by review; the spec was corrected to report both mirror age and peer
   content age, because the two fail in opposite directions.
3. A precedent cited in the spec's open questions claimed the repository
   "lived through one rename" from an old hostname to a new one. An earlier
   campaign's own retrospective says the opposite: a *second* machine
   appeared under a new name; nothing was renamed. Caught by review, and the
   correction strengthened rather than weakened the argument — a host
   *fork*, not a rename, is the precise shape of the phantom-peer failure the
   section existed to warn about, and it had already happened for real to a
   committed timing baseline keyed the same way.

None of these were caught by the author reading the same text back. That is
not an argument against self-review — it is an argument about what
self-review structurally cannot do: a false claim about the world outside the
document reads as true to the same mind that wrote it down, however many
times it is re-read. Only an independent check against the world (measuring
the pipe, reading what the two signals actually measure, reading the cited
retrospective) found any of the three.

## 4. A control invalidated by the controller's own writes

For seven tasks, the campaign's controller used the byte count reported by
`board read` as a fixed point to sanity-check that behavior had not changed
across a review round — while simultaneously posting to the same board as
part of running the campaign. When an implementer reported the byte count had
moved and attributed it to a concurrent session, the controller checked and
found the new post was its own.

A control must be something the act of verifying does not itself mutate. The
byte count failed that test the moment the controller became a writer to the
thing it was reading as a control. The reviewers' own technique throughout
the campaign — build the binary at both commits, run both *now*, diff the
output — was immune to this exact failure by construction, and had been
correct the whole time it was available as an alternative. The byte-count
check was retired in favor of it, and re-reviewers were told explicitly not
to post to the board while verifying against it.

This is the same failure shape as §1's vacuous tests, generalized past
testing: a check that passes for a reason other than the one its user
believes. The instrument here was not code but a habit, which is what made it
harder to catch — nothing red-flagged it, because the number kept looking
plausible even while it was measuring the wrong thing.

## 5. A wrong constant in a plan is one defect per copy

A task brief specified a test using the literal `2_000_000_000` as a Unix
timestamp for a query window. That value is 2033-05-18 — seven years after
the posts under test were committed — so the query opened a window entirely
after the data it was meant to select, and the implementer's straight
transcription of the brief failed loudly and reproducibly (`left: 0, right:
10`) on any date before 2033. Fixed by reading the tip commit's actual time
rather than hardcoding one.

The same bad literal had already been copied into three more not-yet-run
briefs. Two of the three would also have failed loudly, matching Task 4's
shape. The third — a corroboration assertion written as `text.contains('2')`
— would have **passed silently**: an empty digest produced by the same wrong
window can print a `2` from unrelated formatting and satisfy the assertion by
accident. One bad constant, copied four times, produced two different failure
classes depending on nothing about the constant itself — only on how loosely
the surrounding assertion was written. The standing lesson: the moment one
instance of a bad literal is found, grep the rest of the plan for it before
dispatching the tasks that carry it, because the same defect will not
necessarily announce itself the same way twice.

## 6. The GIT_DIR incident (see the chronicle for the mechanism and the cost)

Task 11 added a hook rule requiring the board's own test suite to run at
commit time for a board-only change. The first time that rule ran from a
linked worktree — the normal way every campaign in this project checks out
its work — it discovered that `-C <tempdir>` does not decide which repository
a git invocation acts on, because an inherited `GIT_DIR` outranks it, and a
linked worktree exports exactly that variable, pointed at the real
repository, to every hook it runs. The board's test suite spent the next
several minutes writing real corruption into the developer's actual
repository under the belief that it was writing to an isolated temporary one.

The process lesson is not the mechanism, which the chronicle covers in full;
it is what the incident cost and what it did not. It cost real repository
state (flipped repository-type flag, rewritten commit identity, a dangling
ref that broke `git fetch` project-wide) and it cost detection time — nothing
went red, because every assertion in the suite was still true, just of the
wrong repository. It did not cost any of the genuine board history that
predated it: the repair used the project's own cross-host mirror, already
fetched before the incident and therefore provably clean, as an oracle, and
confirmed byte-for-byte that no real post was lost. And it produced a second
value the campaign would not otherwise have had: a live warning, posted to
the board itself, that reached a concurrent session working in the same
vulnerable configuration before that session could be bitten by the same
defect — the first time this campaign observed the board doing the job its
own design papers name as the harder, more valuable half.

## 7. Bound the WRITE, not the read

The write-before-read pipe deadlock in §2's round 2 was found by a reviewer's
own probe harness — but the harness itself was wrong in a way worth keeping
separate from the bug it found. It wrapped a timeout around
`communicate()`, on the reasonable-looking theory that `communicate()` is
where a subprocess call blocks. It is not, for this call shape: the actual
blocking operation was the batching helper's `stdin.write()` of the whole
request, which happens *before* `communicate()` is ever reached when the
input is large enough to fill the child's stdout pipe before the parent
starts draining it. A timeout wrapped around a call the code never gets to
cannot fire. The harness hung for 42 minutes before anyone noticed, not
because the timeout was too generous, but because it was watching the wrong
call.

The generalizable lesson: when bounding a call that might deadlock,
identify the specific blocking operation first, and put the bound on that
operation, not on whichever wrapper call looks like "the point where we wait
for the subprocess." A `write()` into a full pipe blocks exactly as
unboundedly as a `read()` from an empty one, and a harness built to catch one
direction of that class silently has no coverage of the other. This is the
same shape as the deadlock it found — one blocking syscall no test bounded —
one level up, in the tool meant to find exactly that shape of bug. Item 1 of
the final review's fix wave applied the same principle to the regression
test this incident produced: bound the call that can hang (`cat_file_batch`,
run on its own thread with `recv_timeout`, not a bare join), not a wrapper
around it.

## 8. The convention shipped unexercised

Two post kinds — `confirm` and `stale` — were designed and shipped this
campaign as the corroboration mechanism for board techniques: a way to mark
a technique as re-verified or as no longer trustworthy, surfaced only in
`make board-digest` (root `CLAUDE.md`'s digest-only paragraph explains why
the ambient render excludes them). Spec assumption 6 asked directly whether
corroboration would get used, naming the null result as reportable on its
own terms if it did not.

It did not. At close, the board carries zero *genuine* `confirm` posts and
zero *genuine* `stale` posts — including from this campaign's own sessions,
which designed, implemented, and documented the mechanism without ever once
using it on a technique they themselves posted. The qualifier is deliberate
and checked, not hedging: `board digest`'s full-history tally currently
counts one of each, but both are 2026-08-12 GIT_DIR-incident fixture
contamination (`{"kind":"confirm","by":"main","post":"…"}` against a
technique literally named `"some technique"`) already identified and dropped
from the tip by that morning's cleanup commit, "board: drop 31 test-fixture
posts from the tip" — `digest` still surfaces it because it walks full
history by design (D13), not because anyone corroborated anything for real.
Net genuine usage: zero. That is the honest status of B10: a
convention that compiles, is tested, and is documented, sitting completely
unexercised in the one place — real campaign use — that would show whether
the design solves the problem it was built for. Shipping the mechanism was
not wasted work; the assumption was preregistered specifically so a null
result would be legible as a finding rather than quietly buried by success
framing. The standing question it leaves open is not "does the code work" —
it does — but whether corroboration needs a nudge (a `make board-post`
convenience, a render hint) to get its first real use, or whether nobody
independently re-verifies technique posts often enough for the mechanism to
matter regardless of friction.

One concrete near-miss, rather than the abstract case above: Task 3's fix
round 3 corroborated an existing board technique about repo guards scanning
the whole command string, and the campaign's own ledger noted at the time
that it was "worth a `confirm` post once Task 10 ships the convention." Task
10 shipped the convention. The post was never made.

## 9. Two findings that exist nowhere, and that fact is the finding

Task 2's opus-dispatched review returned quality approved-with-reservations:
2 Important findings and 3 Minor. The fix loop addressed both Importants
plus one Minor (three stale "ancestry check" comments); that Minor survives
because the implementer's own report for the round documents the fix
(`task-2-report.md`). The other two Minors were "explicitly deferred," in
the controller's own contemporaneous words — and that disposition is now the
entire surviving record. Nothing anywhere says what they were about.

The cause is not particular to Task 2. Across the campaign's eleven review
dispatches, no review agent ever wrote its findings to a file. Implementers
wrote reports before returning DONE, the same discipline `dispatching-
hornvale-subagents` requires of them; reviewers reported straight into the
conversation, and the controller's own notes toward the ledger paraphrased
each verdict only tersely enough to route the fix loop — sufficient to know
*that* something was deferred, not to reconstruct *what*. That ledger
(`.superpowers/sdd/2026-08-11-the-beacon/progress.md`) is git-ignored and
dies with this worktree, so what it never captured is gone with it.

The two findings are unrecoverable without re-reviewing commits
`1554bb8e..8174c0a8` from scratch — the range the two fix rounds covered —
and even that would only find what exists in the code today, not
necessarily recover what the original reviewer saw before those fixes
landed. The fix for next time is the discipline already in place for the
other role in this loop: a reviewer writes its findings to a report file
before returning its verdict, exactly as an implementer writes one before
returning DONE. Nothing about the review step required this. The campaign
simply never asked for it.

## 10. "Minors never enter the fix loop" wants an exception clause

The standing process rule is that a review's Minor findings never trigger
their own fix-loop round; only Critical and Important findings do. The
campaign's ledger records nine deliberate controller deviations from
process, each with a stated reason — none of that ledger survives this
worktree, and `grep -i deviation` over the committed branch returns zero
hits, because a ledgered deviation is exactly the kind of reasoning this
retrospective exists to promote before it disappears. Two of the nine
overrode the Minors rule specifically, and the pattern across them, not the
list, is the durable part.

Both happened inside Task 3. In fix round 1, a Minor riding along beside two
Importants was folded into the same round rather than deferred, because it
was a two-character fix against a panic on an unchecked-cast overflow in a
path the spec says must never break a session — correctness-adjacent to the
round already running. A second Minor in the same round was folded in for a
different reason: git echoes the *resolved* object id from `cat-file
--batch`, so unresolved input keys the result map under a different string
and silently misses — and Task 4 was about to build directly on that
behavior, so leaving it as a Minor would have let the very next task walk
into it. In fix round 4, a third Minor (a whitelist disagreeing with `man
git-cat-file`) was folded in for the same second reason: leaving it would
have meant pruning it back out of a file Tasks 4 and 5 were about to extend
further, when fixing it now made it visible at a glance instead.

Both overrides were vindicated rather than merely defensible after the
fact: the ambiguous-oid hazard was real, and the whitelist Minor was
independently confirmed against git's own documentation before it was
fixed. The rule as written treats every Minor identically regardless of
context; what actually happened twice is narrower and more defensible than
"the controller ignored the rule when convenient" — both overrides fired on
one of two legible conditions: a Minor correctness-adjacent to the Important
already driving the round, or a Minor the very next task is scheduled to
walk into. That is an exception clause worth writing into the rule, not
just two ad hoc calls that happened to work out.
