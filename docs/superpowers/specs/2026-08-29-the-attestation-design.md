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

### 1.3 A declared generated path's drift check is vacuous for any file its author does not write

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

`sluice-run.sh` resolves `merge_phases` and `stage_phases` from
`lane-sets.tsv`'s `rung` column instead of restating them: the `stage` rung is
the stage list, and the `stage` rung plus the `merge` rung is the merge list.
The copy stops existing rather than being tested against.

`integration` is itself a `merge`-rung row naming `sluice-run.sh`, so the
derivation must exclude the row describing the chamber itself. That exclusion
is a real edge and gets a test, not a comment.

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
   names.** This is §1.3's positive control, mechanised: mark, run the author,
   compare. Blind to a file written by an author it does not name.
3. **A path with no author is a declaration error.** Not a tolerated case, not a
   waiver — a declared generated path that nothing generates is a false claim
   about the repository, and the fix is to stop declaring it.

**The expected outcome of assertion 3 is deletions, not new generators**, and
that is flagged for Nathan rather than assumed (§9).

## 5. The freshness reader

One command diffing `docs/timings.md` against the two declarations above:

- **per job kind** — phases owed by the roster's rungs, versus rows recorded;
  a job missing an owed phase is named.
- **per author** — when it last ran, from its own rows, and how many declared
  files depend on it.

It reports; it does not gate (§6). Both questions of 2026-08-29 become one
invocation.

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

## 7. What is unverified, and how each is settled

| claim | status | settled by |
|---|---|---|
| the roster's `rung` column can express both lists | **verified** — read at spec time | `scripts/lane-sets.tsv` |
| `lane_sets.rs` does not check list-vs-rung agreement | **verified** — read at spec time | the file's own header |
| `make rebaseline` writes 98 of 941 declared files | **verified** — measured | marker + mtime, 2026-08-29 |
| the 585 unaccounted files resolve into a small number of authors | **hypothesis (H1)** | Task 1 classifies every one |
| deriving the phase lists in dash costs nothing measurable | **hypothesis (H2)** | timed against the current chamber |
| the reader finds at least one absence nobody knew about | **hypothesis (H3)** | run it against the committed ledger |

## 8. Preregistered measurement

- **H1** — the 585 fall into few authors. **Null is a result:** if they fall
  into many, or into "no author" in bulk, then §4's assertion 3 is a much larger
  deletion than this spec assumes and the campaign should stop and re-present.
- **H2** — derivation is not measurably slower than the literals. Null: keep
  the literals and add the two-way test instead. The test is the load-bearing
  half; derivation is the tidier half.
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
2. The two-way agreement test between the roster's rungs and the chamber's
   lists — written first, and shown failing on today's tree if it can be.
3. Derive the phase lists from the roster (H2), or keep the literals if null.
4. The author column in `docs/generated-paths.txt`, and assertions 1 and 2.
5. Assertion 3, and the deletions it implies — the G3-flagged item.
6. The freshness reader (H3).
7. Artifacts, book, chronicle, retrospective, decisions, registry.
