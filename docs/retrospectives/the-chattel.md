# The Chattel — retrospective

**Campaign:** The Chattel (arc IV.c) · 14 tasks, 13 implementing · five
decisions (0396–0400) · three stage gates · five absorptions of main.

**Process, not product.** The chronicle carries what shipped.

---

## 1. The headline: two controller failures with one shape

This campaign's two worst defects were both the controller's, both in the same
place in the pipeline, and the second happened *after* the first had been
diagnosed and written into memory. They are recorded by name because the
lesson is not "be careful" — it is that **nothing in this project's pipeline
compares a ledger entry against the tree.**

### Failure 1 — the Task 1 census test that was cited as a mitigation and never written

Task 1 measured the invariant that licenses `ordinal: 0` — that no production
room composes two anchors of one kind — across all 60 gate combinations. The
plan's own branch table said to write that invariant as a permanent test. The
controller's ledger then used that test as the **mitigation** justifying a
decision:

> Ruling: **no task reviewer dispatched for Task 1.** It produced zero
> commits, so a diff-based task review has nothing to gate. … Cost if wrong:
> Task 4 hardcodes ordinal 0 on an unearned census; **mitigated by the
> invariant test Task 4 writes over the same census shape, which would
> redden.**

Nobody wrote it. The census harness was scratch and was deleted; the only
surviving record of the measurement was a git-ignored report that dies with
the worktree. It surfaced two tasks later, as a Critical in **Task 4's**
review. The controller's own post-mortem named the shape correctly at the
time — *an unimplemented followup is an unverified claim* — and the campaign
produced the memory note `a-cited-mitigation-must-be-verified-to-exist`.

### Failure 2 — `#D-t12-1`, ruled at length and never dispatched

After Task 12 the controller ruled that a second key must be placed in a role
a strongbox never occupies, because the only key in the world was inside the
box it opened. The ruling was written out in full, with its reasoning, its
census re-run and its reachability re-run. It was then **omitted from the
fix-round dispatch**, which covered four other items, and never built.

The ledger's own closing words:

> I wrote the ruling, recorded the reasoning at length, and then wrote a Task
> 12 fix-round dispatch covering A, B, C and D — and silently dropped it. …
> **Writing the decision felt like doing the thing. Nothing checks a ledger
> entry against the tree.**

It surfaced only because **Task 13's reviewer found `take` reaching through a
closed, locked lid** — and that bypass was load-bearing precisely because the
second key did not exist. Closing the bypass alone would have made the
strongbox unopenable and reverted Nathan's own 0398 reachability ruling;
adding the key alone would leave a lock any player defeats in one move. They
were one change, which is exactly the coupling the dropped ruling existed to
prevent.

The campaign came within one review of publishing a gallery page — its only
public evidence of its own thesis — whose opening beat is a player defeating
the only lock in the game by reaching through its closed lid.

### What would have caught them

Not re-reading; both artifacts read as complete, because they *were* complete
as prose. Not a gate; no gate has an opinion about whether a written ruling
corresponds to a commit. The two candidate mechanisms, neither of which
exists:

1. **A ledger-to-tree reconciliation at every fix-round dispatch.** Every
   `#D-*` entry that ruled a *code* change carries a grep-able consequence;
   listing them and grepping for each before closing a round is a five-minute
   check that would have caught both. Failure 1's mitigation was "a test
   exists" (`grep` the test name); failure 2's was "a pattern exists"
   (`grep 'name: "the-key'`, which is literally the command that later found
   it missing).
2. **A dispatch that is generated from the ruling list rather than written
   beside it.** Both failures are a hand-written dispatch losing an item a
   hand-written ledger held. Nothing reconciled the two documents.

The generalisable form, which is worth more than either: **a remedy that is
written down instead of built, and then relied upon, is indistinguishable from
a remedy that exists — to its author, at the moment they need it most.** Both
failures were committed by the person who had just done the analysis, which is
exactly when writing feels like building.

---

## 2. Defects by origin

Four campaigns running have reported the same distribution. The interesting
number remains the split.

| Origin | Count | Notes |
|---|---|---|
| **Controller prose** (plan text, task briefs, dispatch instructions, ledger rulings) | **43** | 36 in plan/brief text, 7 verification or ruling errors |
| **Implementer code** (review + fix-round findings) | **≈68** | A floor: three tasks' reviews were never triaged in the ledger |
| **Reviewer / other** | **3** | All *inherited* from prior campaigns. **Zero reviewer errors recorded.** |

**Read the denominators before the ratio.** The controller count is
*complete* — every brief was verified before dispatch, so its defects were
enumerated exhaustively. The implementer count is a *floor* — Tasks 10, 12 and
13 have no review-triage section in the ledger at all, so their findings
survive only as passing references. The 43:68 ratio is therefore an **upper
bound on the controller share**, and it should not be quoted as though the two
sides were measured the same way. This is `right-measurement-wrong-attribution`
arriving as a reporting risk rather than as a defect.

### The sharper statistic

The pre-dispatch brief check **found a defect in every one of the ten task
briefs it ran against.** Ten for ten. Every one of them was in controller
prose; none was in implementer code; and none was findable by re-reading,
because re-reading checks a claim against the model that produced it. Each
died to one command run against the tree.

The three most transferable:

- **F4** — a signature specified with no possible caller. The brief's rule was
  a conjunction and `is_latent`'s signature could implement only the second
  half: no `Terrain`, no seed, no world. Written from what the *concept* has,
  not what the *call site holds at that line*.
- **F7** — a task that moves the keystone byte-golden with a step that cannot
  see it. The golden's guard is not in the sub-floor roster (`grep -c` = 0)
  and its directory is deliberately not in `docs/generated-paths.txt`, so the
  result would have been a green gate, a clean drift check, and a stale
  golden.
- **F25** — the STOP row firing correctly. The chamber band never builds
  `Noun`s, corroborated three ways, so the field Task 13 was told to add would
  have been empty for every entry. *The third artifact in this arc that would
  have read as delivered while doing nothing.*

### Defects found inside a correction

At least **six by the ledger's own reckoning**, eleven by enumeration. The
ledger flagged the fourth as it happened: *"the 0398 commit introduced a false
claim into the very doc correcting a false claim."* Others: a paragraph
written to *prevent* a phantom instruction contained one; the repair to a
narrowed guard was itself narrow; a warning about a stale citation was itself
a stale citation; a doc claiming to state the honest shape of a mutation
carried a filtered citation.

This is `a-correction-is-unaudited-text`, and its recurrence rate here (roughly
one in four of the controller's own defects) matches the prior campaigns'.
**Nobody audits the audit**, and writing a fix is precisely the moment that
feels like the careful act.

---

## 3. What worked

**Measurement first.** Task 1 produced zero commits and existed only to
falsify the design before anything rested on it. It answered three questions
the spec had flagged as its likeliest real cost, and the answers moved
nothing — which is a good outcome, not a wasted task. Its per-read figure
(~90 ns) also *corrected an inherited adjective*: decision 0366's "a read
costs a scan" was neither "a scan" nor "free" here, and inheriting either
direction would have been wrong.

**Reviewers corrected the controller twice, and were never wrong.** Task 3's
reviewer showed the trope ratchet is a *byte* ratchet, not a numeric one — a
structural argument the controller should have made instead of asking for a
direction. Task 4's reviewer found failure 1. Zero reviewer findings are
recorded as mistaken.

**Nathan's model correction dissolved a problem four remedies were trying to
patch.** Presented with a soft-lock and four options, he took none of them:
closed and locked are different states, and a key in the lock is not in the
container. That reframing dissolved one open Major and *changed the answer to*
the other. Worth noting as a pattern: when four remedies all feel like
patches, the framing is the thing to reject.

**Positive controls before believing a null.** The `passage-cleared` sweep ran
a known-positive grep before accepting an empty result, and then found the
first instrument had been narrowed by an `--include` flag — 0 hits became 14
tracked files. This is the campaign doing `an-empty-diff-needs-a-positive-control`
correctly and then still finding the narrower-question failure one layer up.

---

## 4. What to change

1. **Reconcile the ledger against the tree before closing any fix round.**
   Every `#D-*` entry that ruled a code change gets a one-line grep-able
   consequence, and the round does not close until each has been run. This is
   the single practice that would have caught both headline failures.
2. **A deleted scratch harness is a deleted measurement.** Task 1's census was
   real, correct, and is now unreproducible except from a decision record's
   recipe. Either the harness lands as a test or the *numbers* land in a
   durable document in the same commit — not in git-ignored scratch.
3. **`followups.md` is not a place.** Two deliberate non-builds (`#D-t4-2`,
   `#D-t13-1`) were "carried to followups", which is per-worktree scratch that
   dies with the campaign. A deferral belongs in the idea registry, a decision
   record, or a doc comment on the code it defers.
4. **Stage-gate verdicts belong in the ledger.** One stage gate's submission is
   recorded; its result is not, anywhere. A submission without a recorded
   verdict is the same shape as a ruling without a commit.
5. **Do not quote the 43:68 split without its denominators.** See §2.

---

## 5. Open at the close

- `PLAY-closed-container-conceals-nothing` — the room's prose renders from the
  grammar with no ledger and no latency filter, so a shut chest still narrates
  its contents and a taken key is still listed on the floor. Priced by 0398.
- **The knowledge gate's residual** (0397): it denies a passage, but cannot
  deny through a *live* session, because knowledge absorption is
  unconditional. Named, not closed.
- **`NounEntry.affordances`** — refused twice, both times correctly (the
  chamber band builds no `Noun`s). The named prerequisite is the chamber band
  learning to.
- **Five deferred minors** from Tasks 2–3, listed in the spec's §8.
- **`MAP-if-world-conformance` is still `raw`.** This campaign was its named
  first customer and moved the *subject* — the object model exists — without
  building the *instrument*: no corpus was created, frozen, or resolved
  against. Said plainly in the row rather than implied by a status change.
