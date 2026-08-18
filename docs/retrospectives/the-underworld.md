# The Underworld — retrospective

**Merged:** 2026-08-18

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-underworld.md): two independent
ladders, a cave depth budget in metres, a water table, chamber conditions that
vary, twenty-two underworld communities in The Axes' basis, and a preregistered
gate that closed against the campaign's own headline.

## 1. Fifteen instruments that looked like they were measuring and were not

This is the campaign's subject, and it is a process finding before it is a
product one. **Fifteen findings across fourteen distinct places**, and the
arithmetic is stated rather than rounded because the campaign is about counts
that outrun their evidence: rows 1–12 are twelve different mechanisms, no two
alike, and rows 13 and 15 are the *same assertion in the same file* counted
twice, because they are two different failures of it — the first is what let a
committed table go stale unremarked, the second is that the fix wave closing
the whole-branch review's fourteen findings re-pinned that table and left the
guard standing. An earlier draft of this section headlined twelve, having
stopped counting at the point the campaign's own ledger did not:

| # | where | what it was |
|---|---|---|
| 1 | a retired estimator | a criterion satisfied by its own three-armed `match` — three kinds at one identical depth still spelled three bands |
| 2 | a `compile_fail` doctest | rustdoc does not verify a pinned error code on stable; the annotation reads as an assertion and is documentation |
| 3 | the replacement for #2 | a differential pair cannot catch a mutation on the one line the two blocks exist to differ on |
| 4, 5 | two mutation runs | `cargo fmt` reflow made the search text match nothing; the unchanged file read as a surviving assertion |
| 6 | a shell watcher | `grep -v grep` deleted every hit, because the hunted process is ri**pgrep**-universal |
| 7 | a probe | nothing in 614 tests asserted the live path supplied a depth at all |
| 8 | an implementer's own test | vacuous, found by its own mutation sweep |
| 9 | a test selector in the plan text | matched nothing and reported `0 passed` |
| 10 | a distinguishability claim | a genus filter can match rows and still not be the one in use — no control |
| 11 | five prose figures | estimates presented as measurements, two of them uncaught until a later round. All five were an implementer's; a **sixth** instance of the same shape, counted separately below, was in the campaign's own plan text |
| 12 | a genus join | `"karst"` compared against `"karst-cave"`; one of three agreed by coincidence |
| 13 | an eleven-seed comparison table | every cell of `id_shift_invariance`'s committed table went stale across the genus repair and nothing reddened — the test asserted only `!colliding.is_empty()` |
| 14 | a calibration probe's own output | `Undercroft 100/100/100 %` printed as the first row of a before/after table beside four real measurements, and cited as evidence for a shipped rule. It is an **identity** — rank 0's top is 0 m, the table is floored at 0 and `is_phreatic` is strict — so it cannot come back any other way, in any world |
| 15 | row 13's assertion, still | the wave that fixed the other fourteen re-pinned the stale table, added a loud note asking a human to watch it, and left the guard at `!colliding.is_empty()`. Now `colliding.len() >= 2`, which is the file's own standing selection rule, so the rule and the check are one statement |

The best statement of the pattern came from the implementer that had authored
five of the eleventh kind, and the closing sentence is **theirs, not mine**:
*"numbers written while explaining, when the arithmetic felt too small to run.
The three that were caught were caught by running something; the two that
weren't were the two where I ran nothing. The countermeasure that works is
mechanical, not attentional."*

An earlier draft of this section lifted that last clause out of the quotation
and asserted it in my own voice. In a campaign whose subject is attribution,
that is the wrong direction to move a sentence, and the chronicle — which keeps
it inside the quote — had it right.

Every remedy this campaign shipped for one of these is of the same shape: make
the measurement re-runnable from the tree rather than correct in a report. The
1 K re-bin, the dryness-gain sweep, the arity histogram and the band histogram
are all committed tests now, and each of them steered a design decision that
would otherwise have rested on a transcription.

The durable artefact is `scripts/mutate.py`, which refuses to write unless its
target text is found **and unique**, leaving the file byte-unchanged on either
refusal. It ended a failure that had bitten twice in this campaign and at least
once before it. **Worth a decision record if Nathan agrees**: that a mutation
demonstration is applied by that tool rather than by an ad-hoc `sed`, because
the failure mode is silent and the green it produces is indistinguishable from a
robust implementation.

Number twelve is the one that matters most: repairing it flipped the campaign's
verdict from pass to fail. It was found because eleven others had been, not
because anything in the gate objected.

Number fifteen is the one that says most about the method. A campaign that had
catalogued fourteen instruments which could not see what they were pointed at
wrote a fifteenth finding into the *fix* for the other fourteen, and only caught
it because a re-review asked for a strengthening the wave had declined. Vigilance
scaled to the subject still missed it; the mechanical remedy — making the check
say the same thing the prose rule says — is what closed it.

## 2. Every defect in the design came from the plan text, and every implementer correction was right

The strongest evidence for that title arrived *before a line of code was
written*. The pre-flight conflict scan read every task's `Consumes`/`Produces`
block against the signatures it named, and every defect it found was in the plan
text, because nothing else existed yet: Task 3's `Consumes` claimed a
`DelveRung` its own produced signatures never take; Task 9's omitted
`environment_fit`, which it needs; and **four tasks — 5, 6, 7 and 8 — specified
fourteen `#[test]` blocks with comment-only bodies**, which would have compiled
and passed vacuously. Nine of the eleven task-pair checks came back clean, so
the scan was discriminating rather than uniformly pessimistic.

That last finding is the whole reason Ruling 1 exists — *every such test must be
implemented with real assertions expressing what its comment describes; a test
that compiles with no assertion is a task failure, not a completed step* — and it
was carried verbatim into every dispatch for those four tasks. A campaign whose
subject turned out to be instruments that do not measure began by writing four
of them into its own plan, and caught them only because something read the plan
adversarially before dispatching it.

Three times an implementer disagreed with a controller ruling. Three times the
implementer was right.

| ruling | what I had said | what was actually true |
|---|---|---|
| 8 | Task 1b left worlds with "materially fewer chambers" | counts were fine (−2.0 / +1.4 / −5.9%); what broke was **variance** — `deepest_band` collapsed onto one value for 97.3–99.0% of caves. A worse defect, invisible to a count check |
| 9 | "the missing stress-concentration factor can wait" | true, with an untrue rider attached: the docs next to it asserted `S/(ρg)` *is* the closure depth. **A deferral is only honest if the doc next to it stops claiming the thing that was deferred** |
| 14 | a differential pair is a sufficient replacement proof | it is not — the mutation lands on the line the pair differs on. The fix needed a third test pinning that line's shape |

Ruling 8's provenance is the general lesson: a reviewer asserted a diagnosis, I
promoted it into a ruling and an acceptance criterion **without measuring it**,
and it was wrong. An inherited diagnosis is a hypothesis. The same shape produced
the campaign's one overstatement to Nathan — a report's "provably cannot be
traded" relayed without checking its scope, when the proof covered two magnitudes
and not the parameter that had actually moved.

## 3. Pre-committing a rule, and honouring it when it binds

Ruling 11 declined to escalate a fix loop to fresh eyes, and said in terms: *"if
round 4 also comes back partial, that IS a stuck loop and round 5 goes to fresh
eyes without further argument."* It did, so it did — even though a good argument
existed that the purpose did not fit, since the previous implementer had fixed
the mechanism correctly and got only a *fact about rustdoc* wrong.

Declining to honour it there would have been the same move as retuning a
criterion after unblinding: relitigating a rule at the exact moment it costs
something. The value of the pre-commitment is that it is not renegotiated when it
binds, and the round-5 implementer then found that Ruling 14's design was
insufficient — which the arguing party would not have found.

The same discipline appeared unprompted from implementers three times: a
constant refused because tuning it after seeing its downstream effect is
disqualifying; a boundary rule applied to exactly the one flagged edge and no
other; a marginal 6-vs-5 assertion pooled over eight seeds rather than shipped,
on the grounds that *"a one-coin-flip pass would have been the same defect I was
sent to fix."*

## 4. Measurement before authoring is what kept the null honest

The plan's shape — measure the ladder, then the communities, and author only if
a frozen criterion holds — is the discipline The Delvers lacked, and it paid
three times over:

- **Task 1 falsified the spec's central design in the first task**, before
  anything was built on it. The repair cost a scope change; discovering it at
  Task 9 would have cost the campaign.
- **Task 3 found that no `Underdeep` or `Sunless` chamber is dry in any world**,
  four tasks before a criterion would have failed *by construction* rather than
  by measurement.
- **Task 8 measured the founding circularity rather than reasoning about it.**
  The first design ranked rungs by fit × drainage cost and measured **zero**
  communities founding below the water table on any seed, because the shallowest
  rung is dry in every column — a rule unreachable in exactly the case it was
  written for. Reasoned about, it would have shipped.

  **Two corrections to that measurement, made in the closing review and worth
  more than the original.** First, "the shallowest rung is dry in 100% of
  columns" was cited as a probe reading and is an **identity**: a rung is judged
  at its top, `Undercroft`'s top is 0 m, the water table is floored at zero and
  `is_phreatic` is strict, so `works` is `false` at rank 0 in every possible
  world. A figure that cannot come back any other way is not evidence, and it
  was disclosed as evidence for a campaign. Second, the figure itself — 0 of 23
  — was taken **before** Task 9 repaired the genus join, and so describes a tree
  that no longer exists. Re-run 2026-08-18: the counterfactual is now **0 of
  81** and the shipped rule fires on **3 of 93**, having moved off seed 42 and
  onto seeds 7 and 1234. The design conclusion survives both corrections and is
  better evidenced by the second, which is the outcome you hope for and cannot
  assume.

The one place this discipline was applied *to itself* is worth recording: when
the review asked whether a failing criterion should be restated under decision
0138, the answer was no, and the reasoning was better than the instinct. 0138's
three-part test is *unsatisfiable* when the subjects do not exist yet — the
property cannot be independently verified, the estimator defect can only be
inferred from a proxy, and no injection can re-prove a restatement against kinds
not yet authored. The spec was **strengthened** instead, pre-unblinding, with a
clause that could not be satisfied by choosing niches. That clause is the one
that earned its place in the readout.

## 5. Ten task boundaries, zero absorptions, and the board caught it

The plan's own Global Constraints say to absorb main at every plan-stage
boundary. This campaign went **ten boundaries and absorbed zero times**, and
what surfaced it was not any gate or any process step — it was a peer campaign's
board post warning that *it* had gone seven boundaries without absorbing, and
that the cost is not conflicts but premises going stale.

Measured at that point: **119 commits behind, 60 ahead.** The damage assessment
is the useful part, because it explains why nothing had complained:

- main's 119 commits touched **nothing** in `kernel/`, `domains/` or
  `windows/worldgen/` — an empty `git diff --stat` over those paths — so the
  census that had just been run was not invalidated. That is the expensive
  outcome that did not happen, and it happened by luck.
- The three merge conflicts were all in **generated session fixtures**, which are
  resolved by regeneration and never by hand-merge.
- **`windows/vessel/src/session.rs` auto-merged cleanly**, and main had added
  159 lines to it — the file holding the function whose hardcoded band and empty
  override map are the entire premise of a disclosure this campaign committed. *A
  clean auto-merge is not evidence the premise survived.* Re-verifying it took a
  structural argument (the underground refusal sits at the dispatch match
  upstream of the general movement arm, so a rewrite of the latter is unreachable
  from that band), not an inspection.
- **The 3,863-test workspace suite is structurally blind to `clients/`**, which
  is outside the cargo workspace. The two client check targets had to be run by
  hand. A green workspace suite is not evidence about a directory the workspace
  does not contain.

The correction is not "absorb more often" as a resolution; it is that nothing in
the current process notices. The stage gate exists and was not used; the board
post that caught it was addressed to nobody in particular.

## 6. Decision numbering: the board claim is advisory, the gap check is not

The plan for the close said to number from 0143, because a peer campaign had
claimed 0142 on the board at brainstorm. `docs_consistency`'s
`no_gaps_in_the_decision_log` refuses a hole, and 0142 does not exist on main —
so numbering from 0143 fails the gate that runs on every commit.

The records were written as 0142–0144, contiguous from main's 0141. A collision
with the peer is resolved by renumbering at merge, which is routine; a gap is a
red gate and cannot merge at all. The test's own failure message names "shifting
too far to resolve a collision" as the usual cause of a hole, which is exactly
the manoeuvre the plan text prescribed.

**A board claim tells you a number is taken. It cannot tell you to leave a hole.**

### The renumber, and what only the merge product could see

The peer — The Illumination — closed first and landed 0142 on main, so by the
repo's own precedent from the 0132/0133 collision (first to merge keeps the
number) these records became **0143–0145** at the final absorption. Every
citation moved with them: the three records' front matter and cross-links,
`docs/decisions/README.md`, two idea-registry rows carrying the number in both a
status cell and a Where link, the spec, and three test files whose doc comments
cite the one-community-per-place record.

**The order was load-bearing, and a guard proved it.** The renumber was done
against the merged tree, not before it, because only the merge product holds
both 0142s. Absorbing main first produced a tree with two files beginning 0142 —
different slugs, so `git merge` raised nothing — and it was `docs_consistency`'s
`decision_numbers_are_unique` that refused the merge commit. That is the useful
half of the ordering: the collision is invisible to the merge machinery and
visible only to a check running on the merged tree, which is the same shape as
this campaign's other findings and the same shape as the argument for gating the
merge product rather than a branch tip. Renumbering first would instead have
left 0142 unoccupied here until the peer's record arrived, and
`no_gaps_in_the_decision_log` is in the sub-floor roster — red on a branch whose
merge product is perfectly contiguous.

**Three things about the citation sweep are worth keeping.** First,
`git grep -E '\b0142\b'` returns *nothing* — POSIX ERE has no `\b`, so the whole
search is silently empty, and an empty result reads exactly like "no citations to
fix". It needs `-P`. A plain `git grep -l 0142` found 36 files. That was caught
only by running the search against a number known to be present and watching the
control come back empty too. **Run a positive control on a search before trusting
its emptiness** is this campaign's subject, and it nearly bit at the last step.
Second, the inverse: a bare-number sweep across the whole tree hits numeric
data — `-0.0142` in a disposition doc table, `1.8150142892666530` repeated in a
chronicle, a plan and a calibration freeze, and four-digit runs inside census
CSVs, terrain fixtures and a geojson — so the sweep must be scoped to prose and
every hit eyeballed. Third, a prepared patch is
not an enumeration: the one written for this renumber predated a commit that
added a fourth citation site (`windows/hearsay/tests/parley_readout.rs`), and
applying it without re-running the sweep would have left that one behind.

## 7. Two pre-commit guards fired on the ledger prose describing them

Writing the hook-bypass flag as a literal string inside a paragraph *praising*
its non-use tripped the guard that matches that text; a sentence about counting
tool invocations tripped another. Both are false positives, both are correct
behaviour for a text-matching guard, and the second fired on a sentence
describing the first.

Not a defect. Recorded because a blunt guard is the right trade and the cost is
worth knowing in advance: prose about a forbidden construct is indistinguishable
from the construct.

## 8. What held up well

**The spec was amended, never rewritten.** Four corrections (§4.0, §4.2.1, §4.3,
and §5's strengthening) sit next to the reasoning they replaced, deliberately, so
that falsified premises stay readable. A spec that reads as though it were always
right teaches nothing, and three of this one's corrections are individually more
instructive than the claims they replaced.

**The null was audited harder than it was asked to be.** The review verified that
the criterion's threshold was untouched, that `git diff` over the spec directory
since the readout's first commit is empty, that all three seeds were reported and
none dropped — and that the headline uses the *generous* overlap statistic
(7.3 / 16.4 / 16.5%) rather than the pessimistic Jaccard (3.8 / 8.9 / 9.0%), with
the quartile taken over cave-bearing cells only, which inflates overlap. The
failure is conservative in every direction it could have been flattered.

**Incremental regeneration meant the close moved nothing.** `make rebaseline` and
`make rebaseline-goldens` at Task 10 moved zero bytes, because the tasks that
drifted an artifact regenerated it in the same commit.

**A committed witness that fired and did not change its verdict.** The
falsified-recall statistic moved to land exactly on its own preregistered bar,
its four-site re-read was performed as its pin demands, and the arithmetic
(three readings within one standard error of the bar, across three census
epochs) says the verdict remains *cannot tell*. It was not escalated because
nothing changed status — and a witness that has now fired twice and corroborated
its own prior re-read is worth more than one that never moves.

The transferable half is the escalation rule, and it is not "a fired witness is
the controller's to settle". **The trigger for escalation is a changed status,
not a moved number.** This reading left the registry row neither `refuted` nor
`shipped`, so the controller read it, re-stated it across the four sites the pin
demands, and closed it. Had the reading crossed the bar meaningfully it would
have been Nathan's call, as it was at The Gnomon's close. A witness firing is a
controller's to *read*; a witness changing a status is his to *rule on*.

## Follow-ups

**U-1 — the third seam is open, not closed, and is priced.** `ChamberOrigin` has
a correct writer with no shipped call site. Closing it needs a descent verb,
chamber state that tracks an address rather than one chamber, prose that tells a
cut hall from a found void, and a home for the overrides — a client campaign, not
a capacity task. The disclosure in the writer's own doc records the current
measurement and has already gone stale once; re-measure the band histogram rather
than trusting the sentence.

**U-2 — the disposition draw key carries no rung.** Two alive communities in one
column share a key and draw one mind vector. Accepted unrepaired by decision
0145; the repair needs an epoch *and* a way for the ledger-side wrapper to
resolve a rung, which it cannot do today.

**U-3 — the deep may be dry for a modelling reason rather than a physical one.**
The water table treats matrix porosity as a conductivity proxy, and the columns
that reach the deepest rung are fracture caves in metamorphic rock, where
fracture permeability rules. "Deep caves sit in rock that cannot shed water" may
be an artifact. Captured in the registry.

**U-4 — the habitable ceiling is the least well-placed edge in the ladder and
could not move.** It is an authored constant frozen before the fit. A campaign
that wants to move it must do so *before* measuring anything that depends on it.

**U-5 — two live witnesses read zero after the node re-key**, so their
per-instance assertions execute zero times: green, asserting nothing. Nothing
separates "at zero and we know" from "at zero for six months, unlooked-at".
Captured; the fit is a three-valued ratchet.

**U-6 — `scripts/mutate.py` should probably be mandatory.** See §1. It is
available to every future campaign and required by none.

**U-7 — the underworld corpus is authored and unvalidated against a generated
world**, and its physiognomy axis is regrounded relative to the surface corpus
in a way that is uncontrolled. Both concerns are stated in the corpus's own
tests rather than only here.
