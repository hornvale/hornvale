# The Attestation — an absence has no row

**Branch:** `campaign/the-attestation`, from `origin/main` @ `26db4df99` ·
**Decision block:** 0456–0465 · **Drafted:** 2026-08-29 ·
**Status:** G3 package pending.

*A muster roll exists to find out who did not answer.*

---

## 0. What this is

Three checks in this repository verify a **state** and are structurally blind
to an **action that did not happen**. Each is green today. Each was green
while the thing it appears to guarantee was false.

This campaign does not add a record — the record already exists and is
committed. It adds the **statement of what should be in it**, without which an
absence is invisible.

## 1. The three defects, each measured rather than argued

### 1.1 The phase roster is restated, and the guard between the copies is one-sided

`scripts/lane-sets.tsv` is declared in its own header to be "THE SINGLE SOURCE
OF TRUTH for what a set is", and it carries a `rung` column that already
answers which phases belong to which job kind:

```
  gate        stage        artifacts   stage
  outboard    stage        clients     stage
  heavy       merge        census      campaign
```

`scripts/sluice-run.sh:394-395` restates that as two literals:

```sh
merge_phases="artifacts outboard gate clients heavy"
stage_phases="artifacts outboard gate clients"
```

`cli/tests/suite/lane_sets.rs` reads those two lists and asserts **each token
has a roster row**. It does not assert that the lists agree with the `rung`
column. So moving `heavy` between the lists — which decision 0148 did in one
direction and 0426 did in the other — passes the test in both positions.

That file is otherwise exemplary: it names the direction each of its checks
enforces and what each is blind to. This is the direction it does not yet have.

### 1.2 The ledger records every phase that ran, and nothing diffs it

`docs/timings.md` is tracked, committed, and present in every checkout. It
carries one row per phase per chamber job. The question two sessions spent an
hour reconstructing on 2026-08-29 is one `awk` away and always was:

```
  the-errata's merge          The Overture's merge
    sluice:artifacts            sluice:artifacts
    sluice:outboard             sluice:outboard
    sluice:gate                 sluice:gate
    sluice:clients              sluice:clients
                                sluice:heavy
```

The same file answers the staleness question. `sluice:heavy` rows by date:
**2 on 08-16, 6 on 08-17, 6 on 08-18, 7 on 08-19, then nothing until 1 on
08-29** — the ten-day silence, already committed, never read.

**Why nothing noticed.** The durable record of a green job is
`scripts/sluice-run.sh`'s `record()`, which writes `phase_failed` and not the
phase list — so a green run stores no phase information. The resolved list *is*
written, at `sluice-run.sh:569`, into the claim file — which the EXIT trap
deletes. And `scripts/sluice-drain.sh` composes the queue note as
`"all merge phases rc=0 in ${ELAPSED}s"`, which names the list **by reference**
and was therefore a true statement for a four-phase run and a five-phase run
alike.

### 1.3 A declared path's writer check is evaluated at the wrong granularity

`docs/generated-paths.txt` declares ten paths. The drift check is
`git diff --exit-code` over them, run after `make rebaseline`.

Measured 2026-08-29 on `26db4df99` by touching a marker, running
`make rebaseline` (rc=0), and counting tracked files newer than the marker:

```
  path                                        written / tracked
  book/src/gallery/                              45 / 56
  book/src/reference/                             9 / 21
  book/src/laboratory/                           11 / 825
  docs/audits/                                   11 / 16
  docs/digest/                                    2 / 3
  book/src/domesday/                             14 / 14
  clients/game/core/tests/fixtures/               3 / 3
  docs/audits/underworld-lattice-seed-panel.md    1 / 1
  docs/audits/the-confidant-report.md             1 / 1
  docs/audits/the-reticence-report.md             1 / 1
  ------------------------------------------------------
  TOTAL                                          98 / 941
```

**`make rebaseline` writes 98 of the 941 files the drift check covers.** For
the other 843 the check compares a file against itself and reports "no drift",
which reads as "current" and means only "the author I ran did not move it".

Of `book/src/laboratory/`'s 825 files, **229 are census output** — legitimately
authored by a different, deliberately-excluded path. That leaves **585 whose
author is not yet established**, and establishing them is Task 1 rather than an
assumption of this spec. A sample already shows at least two distinct kinds
among them: `generated/the-history/` is authored by the **heavy tier**, and
`census-of-coasts-ii.md` is a **hand-written prose chapter** that no generator
has ever produced and that should probably not be declared at all — checked, not
inferred from its name: it opens in narrative voice, carries no generated-file
banner, and was not rewritten by the measured run.

**This defect was demonstrated accidentally and is the campaign's strongest
evidence.** During The Overture's close, `make rebaseline` ran, the drift check
over this exact list came back **clean**, and `book/src/laboratory/generated/
the-history/` was at that moment **ten days stale** — as the chamber's own heavy
phase proved forty minutes later by rewriting it. A green check and a stale
artifact, from one run, an hour apart.

## 2. The thesis

**An absence has no row.** A check on state cannot detect an action that did
not happen, and neither can a complete ledger, because the signal is a row that
is not there. The remedy is not more recording. It is a **declaration of what
should be present**, against which absence becomes a diff.

All three defects are the same shape and take the same remedy.

## 3. The phase lists are derived, not restated

**Derivation is REJECTED, on correctness rather than on cost, and this
section's first draft had it backwards.** The plan-writing pass measured the
two orders:

```
  roster order   style subfloor gate artifacts outboard clients heavy census …
  phase order    artifacts outboard gate clients heavy
```

`gate` and `artifacts`/`outboard` are **transposed**. Phase order is
load-bearing — `artifacts` regenerates and commits, so running `gate` first
would gate a pre-regeneration tree — and the roster is ordered by rung, not by
sequence. A task that derived the lists from the roster would have silently
reordered every merge. `sluice-run.sh`'s own comment says the literals are
deliberate for exactly this reason; the first draft of this section read that
comment as inertia.

**So the literals stay, and the agreement test is the whole deliverable.**
`lane_sets.rs` gains the direction it lacks: the chamber's two lists and the
roster's `rung` column must agree **as SETS, in both directions** — every
`stage`-rung set appears in `stage_phases`, every `stage`-or-`merge`-rung set
appears in `merge_phases`, and neither list contains a set the rungs do not
imply. Order is deliberately not asserted: the script owns sequence, the roster
owns membership, and the test asserts only what both actually claim.

This is a better outcome than derivation. Deleting one copy would have made
order implicit in a file that does not encode it; asserting agreement leaves
each file authoritative for the thing it is actually authoritative for.

`integration` is itself a `merge`-rung row naming `sluice-run.sh`, so the
agreement test must exclude the row describing the chamber itself. That
exclusion is a real edge and gets a test, not a comment.

`lane_sets.rs` gains the direction it lacks: whatever the chamber resolves must
equal what the roster's rungs imply, **checked in both directions**, so neither
a phase added to the script nor a rung changed in the roster can pass alone.

**Constraint:** `/bin/sh` on the canonical box is dash and must parse the TSV
without a tool (decision 0004 admits no parser). The existing file is already
read this way; this is an extension of that, not a new mechanism.

## 4. Every declared generated path names its author

`docs/generated-paths.txt` gains a second column naming the author that writes
it. **The column names an INVOCATION, not a program**, and that distinction was
found during spec self-review rather than assumed: `scripts/regenerate-
artifacts.sh` authors both the ordinary artifacts *and* the census goldens, and
which it writes depends on whether the `HV_CENSUS` flag is set. So the
values are of the shape `rebaseline`, `rebaseline+census`, `heavy` — the command
a person would have to run to make that file current. A column naming the
*script* would put the census files and the ordinary ones under one value and
lose exactly the distinction the campaign exists to draw.

(This also makes the 585 subtraction in §1.3 sound: a plain `make rebaseline`
leaves `HV_CENSUS` unset, printed `censuses SKIPPED` on the measured run,
and therefore wrote none of the 229 census-named files. The 11 it did write
under `book/src/laboratory/` are disjoint from them.)

Three assertions, each with its direction stated in its own doc comment:

1. **Every declared path names an author.** Blind to a generated path nobody
   declared.
2. **Every tracked file under a declared path is written by the author it
   names** — and this REPLACES A PROXY rather than adding a check, which was
   discovered while planning and is a correction to this section's first draft.

   `cli/tests/suite/generated_paths.rs:243` already asserts "every declared path
   is one `regenerate-artifacts.sh` writes", and `docs/generated-paths.txt`'s own
   header already states that criterion correctly. The check is real and the
   criterion is right. **Its implementation matches the declared path as a
   literal substring of the script**, so `book/src/laboratory/` satisfies it
   because the string occurs somewhere in the script — while 814 files beneath
   it are never written by it. The criterion is about files; the granularity is
   the directory.

   The test's own header already names this blindness, in the other direction
   (a path written through a shell variable would read as undeclared). This is
   the same weakness read the other way, and it fails *unsafe*: a false negative,
   not the false positive that header anticipated.

   **Where the measurement runs.** Not in a unit test — the positive control
   costs a full regeneration (measured 128.720 s). It runs in the chamber's
   **`artifacts` phase, which already executes the author**, so capturing which
   declared files that run wrote is a byproduct of work already paid for. The
   result lands in the ledger; §5's reader diffs it. The static test keeps the
   substring proxy but applies it **per declared author** rather than assuming
   `rebaseline`.
3. **A path with no author is a declaration error.** Not a tolerated case, not a
   waiver — a declared generated path that nothing generates is a false claim
   about the repository, and the fix is to stop declaring it.

**The expected outcome of assertion 3 is deletions, not new generators**, and
that is flagged for Nathan rather than assumed (§9).

## 5. The freshness reader

One command diffing `docs/timings.md` against the two declarations above:

- **per job kind** — phases owed by the roster's rungs, versus rows recorded.
- **per author** — when it last ran, from its own rows, and how many declared
  files depend on it.

**It diffs in BOTH directions, and the first draft of this section did not.**
Owed-but-missing is the absence that motivated the campaign; *recorded-but-not-
owed* is its mirror — a phase that ran when nothing asked for it, an author that
ran outside any declaration. A reader that only reports absences would be a
one-sided consistency check, which is precisely the defect §1.1 identifies in
`lane_sets.rs`. Shipping the campaign's own instrument with that shape would be
the campaign refuting itself. (Found by an ideonomy negation pass at G3: the
design's definitional property was *absence is the signal*, and negating it
gives *presence is the signal*.)

It reports; it does not gate (§6). Both questions of 2026-08-29 become one
invocation.

## 5a. The fourth cell: a decision record inside its own block

Same shape, same remedy, and it has already cost this project twice.

A campaign reserves a decision block (`make decision-block`), which is a
**declaration** living on the canonical box. It then authors records into
`docs/decisions/`, which is the **record**. Nothing diffs them:
`docs_consistency.rs::decision_numbers_are_unique` asserts uniqueness and
`decision_cites_in_sources_resolve` asserts links resolve, but no check asks
whether a record's number falls inside the block its author reserved.

Both known instances were caught by a human noticing:

- **The Overture** (2026-08-29) drafted its spec claiming 0357–0366, which
  nothing had reserved and which by close straddled two other campaigns' live
  blocks. Caught at the closing task, before any record was written.
- **campaign/the-stride** minted `0160` inside `campaign/the-burr`'s reserved
  0156–0165. Caught by the queue operator by hand, at the mouth, and recorded
  on the board — with the note that "a number inside someone else's reserved
  block is invisible to every mechanical check until the block's owner mints
  the same number, and by then both are committed."

This is in scope because it is the same defect, not because it is nearby: a
declaration exists, a record exists, and the diff between them is performed by
whichever human happens to look. The check is cheap. What it needs is the
block ledger to be readable from a checkout, which is the one design question
this section carries into the plan rather than settling here.

## 6. What this does NOT do

- **It does not gate.** Nathan chose observability over prevention: a gate
  prevents one known failure and leaves the record silent about the rest, and a
  gate that reds for a benign reason trains people to ignore it — the disease
  0426 diagnosed in the heavy tier itself. A later campaign may add one on
  evidence from the reader.
- **It does not schedule anything.** Making an author run on a trigger is a
  separate question from being able to see that it did not.
- **It does not touch the determinism contract.** No seed label, no stream
  order, no quantized value.
- **It does not re-author any stale artifact.** If the reader finds one, moving
  it is a deliberate act with its own review.
- **It does not give seam-guard a record.** The same grid that produced §5a
  shows seam guards are worse off than anything this campaign touches: they have
  a declaration (`docs/audits/seam-guard-roster.md`) and **no record of what
  happened at all** — the artifact states outright that it lists registrations
  and "never the verdicts". Ordered by reversibility that is the severe case, and
  it is severe in a way this campaign cannot cheaply fix: a missing phase can be
  re-run and a stale artifact re-authored, but a seam that stopped being guarded
  leaves no trace of *when* it stopped, so the history is unrecoverable rather
  than merely absent. It gets an idea-registry row, not a task.

## 7. What is unverified, and how each is settled

| claim | status | settled by |
|---|---|---|
| the roster's `rung` column can express both lists | **verified** — read at spec time | `scripts/lane-sets.tsv` |
| `lane_sets.rs` does not check list-vs-rung agreement | **verified** — read at spec time | the file's own header |
| `make rebaseline` writes 98 of 941 declared files | **verified** — measured | marker + mtime, 2026-08-29 |
| the 585 unaccounted files resolve into a small number of authors | **hypothesis (H1)** | Task 1 classifies every one |
| ~~deriving the phase lists in dash costs nothing measurable~~ | **WITHDRAWN before test** — derivation loses phase order, which is load-bearing | measured at plan time: roster and phase order are transposed |
| the reader finds at least one absence nobody knew about | **hypothesis (H3)** | run it against the committed ledger |

## 8. Preregistered measurement

- **H1** — the 585 fall into few authors. **Null is a result:** if they fall
  into many, or into "no author" in bulk, then §4's assertion 3 is a much larger
  deletion than this spec assumes and the campaign should stop and re-present.
- **H2 is withdrawn, not tested.** It asked whether derivation was fast enough,
  which was the wrong question: derivation is wrong at any speed, because the
  roster does not encode phase order and the order is load-bearing. The
  hypothesis is left here rather than deleted because the campaign's own thesis
  applies to it — a withdrawn hypothesis that leaves no row reads exactly like
  one that was never proposed.
- **H3** — the reader surfaces an absence not already known. **This one has a
  real chance of failing**, because the two absences we know about are the two
  that motivated the campaign. A null means the instrument is correct and the
  repository is currently clean, which is worth reporting as such and is not a
  reason to withhold it.

## 9. Flagged for Nathan at G3

1. **§4 assertion 3 will most likely produce deletions from
   `docs/generated-paths.txt`, not new generators.** Removing a declaration
   narrows what the drift check covers — it is the correct response to a false
   declaration and it also, mechanically, reduces coverage. Nathan raised no
   objection when this was put to him during design; it is recorded here so the
   decision is his explicitly rather than by silence.
2. **§3 touches `sluice-run.sh`'s dispatch path**, which board convention marks
   campaign-grade risk and off the board lane. The change is confined to how two
   variables are resolved, but the file is the one every merge runs.
3. **A fix to `sluice-run.sh` cannot be verified by dispatching**, because the
   chamber runs the dispatcher's own checkout copy. The first run that exercises
   §3 is the first merge after this campaign lands. That is stated here so no
   task claims a dispatched run verified it.
4. **The 941/98 measurement is one run on one machine on one day.** It is
   load-bearing for §4 and should be re-taken by whoever next depends on it.

## 10. Decisions to promote (0456–0465)

- **0456** — a rule stated in two places needs an agreement test in both
  directions; a one-sided check is an echo.
- **0457** — a declared generated path names its author, and a path with no
  author is a declaration error rather than a tolerated case.
- **0458** — the drift check reports identity, never freshness: it is blind by
  construction to an author that did not run.
- **0459** — what a job actually ran is recorded against what it owed, and the
  owed list has exactly one source.

## 11. Task outline

1. Classify all 585 unaccounted files by author (settles H1 before any code).
2. The two-way set-agreement test between the roster's rungs and the chamber's
   lists — written first, and shown failing on a tree where they disagree.
   (Task 3 of the first draft, deriving the lists, is deleted: see §3.)
4. The author column in `docs/generated-paths.txt`, and assertions 1 and 2.
5. Assertion 3, and the deletions it implies — the G3-flagged item.
6. The freshness reader, diffing both directions (H3).
7. The decision-block check (§5a), which needs the block ledger readable from a
   checkout before the check can be written.
8. Artifacts, book, chronicle, retrospective, decisions, registry.
