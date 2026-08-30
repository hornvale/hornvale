# The Attestation

*A muster roll exists to find out who did not answer.*

This campaign named three committed checks that verify a **state** and are
structurally blind to an **action that did not happen**. Each was green the
whole time the thing it appeared to guarantee was false. It did not add a
record to any of the three — the records already existed and were already
committed — it added the statement of what should be in each one, without
which an absence is invisible.

## The headline number was wrong, in the same measurement that confirmed the hypothesis it motivated

Task 1 set out to classify 585 files `make rebaseline` leaves untouched under
declared generated paths — the spec's own number, offered as evidence that
the residual would resolve into a small number of authors (H1). It does, but
585 was arithmetic that missed a second directory gated behind the same
census conditional, on the adjacent line of the same block in
`scripts/regenerate-artifacts.sh`. The real residual is **132**: 2 files from
an active heavy-tier author, 3 from a heavy test demoted to hand-run only,
110 from nine frozen one-off study runs (a tenth, `census-of-skies`, had an
automated author too — a CI workflow decision 0125 later deleted, leaving
only git history as its trace), and 17 hand-written prose files sitting
directly under a declared directory.

**H1 is confirmed, and its own motivating figure is falsified in the same
breath.** A hypothesis can be right about the shape of an answer and wrong
about the number used to justify asking the question.

## The payoff hypothesis came back NULL, and it took two tries to get there honestly

The reader built to settle H3 — does diffing `docs/timings.md` against what
the roster and the generated-paths declarations say a job owed surface an
absence nobody already knew about — reported five confident "owed but
absent: `clients`" jobs on its first run against the real ledger, and called
the hypothesis confirmed.

It was wrong. All five were legitimate: `scripts/sluice-phases.sh`
deliberately drops `clients` (and `heavy`, `seam-guard`) from a candidate
whose every changed path is hand-written prose, and the review traced each of
the five to exactly that case in the queue's own logs. The reader's "owed"
set had been computed from the roster's rungs alone, with no model of the
chamber's own conditional narrowing — a confident false positive, on the
first real run, from the instrument built specifically to find this shape of
defect.

Corrected, what remains is already-known history: eighteen "present but
unowed: `seam-guard`" rows from before decision 0148 split that tool onto its
own dispatch path, and `census`'s standing, structural inability to produce a
`sluice:census` ledger row (it bypasses the chamber's phase loop by design).
**H3 is NULL.** The instrument is correct; the repository, checked honestly,
is currently clean of any absence not already known. A null was
preregistered as a legitimate, publishable result, and this is the campaign
that shipped one as its headline rather than its footnote.

## Five times, the campaign wrote its own defect while fixing someone else's

1. Task 1's own classification file was drafted for `docs/audits/
   generated-path-authors.md` — a directory declared as generated. A
   hand-authored classification of generated paths, sitting inside one,
   would have been a fresh instance of the exact thing Task 5 exists to
   remove. Moved to `docs/generated-path-authors.md` before landing.
2. Task 3's own migration silently broke `scripts/hooks/post-merge` — a
   *live* git hook — by adding the author column its whole-line parser could
   not survive. `git diff --exit-code` over a pathspec that matches nothing
   exits 0, so the hook's drift notice would have been dead, on every merge,
   forever, with nothing testing it. Caught by review, fixed, and given its
   first test (`scripts/test-post-merge.sh`).
3. Task 4's self-row — the artifacts phase's own line in its new write-count
   ledger — reads as measured the same way every other row does, and the
   claim is false: a bash compound-command redirect stamps the target file's
   mtime at redirect-open time, before the body runs, so the row cannot come
   back false while the phase runs at all. The row states a true fact; the
   file's comment claiming uniform measurement did not, and was corrected.
4. Task 5's fix round found the precedence rule its own new checks depend on
   — the more specific of two overlapping declarations wins — had no test at
   all; a factually wrong, more-specific row passed every check clean.
5. Task 6's freshness reader is the instance above: a confident false
   positive, reported as the campaign's confirmed payoff finding, produced by
   the very tool built to catch exactly that shape of overclaim.

Every one was caught before landing — by a pre-dispatch check, a review, or a
fix round — which is the discipline working as designed. None of them would
have been caught by re-reading the plan a second time; each died to a command
someone ran.

## A real collision, found live, by the substitute for a check that could not be built

Spec §5a asked for a check that a decision record's number falls inside the
block its own campaign reserved. That ledger — `scripts/decision-block.sh`'s
`blocks.tsv` — lives outside the repository entirely, per machine, reachable
only by ssh, which a workspace test must not do. Task 7 confirmed this rather
than assuming it, and reported the honest stop: not writable as specced.

The narrower, repo-only substitute — pairwise disjointness of the
`Decision block: NNNN-MMMM` declarations already committed in every spec
header — reddened on its very first run against the real tree. The Scarf and
The Quadrat both still commit `Decision block: 0286–0295`; not stale text,
but the genuine double reservation `docs/retrospectives/the-quadrat.md`
already describes. Waived, not fixed — both headers are historical fact, and
editing either would misstate what each campaign actually reserved.

## What shipped

| defect | check | direction it now enforces |
|---|---|---|
| the chamber's two phase lists restate the roster's `rung` column, one-sided | `the_phase_lists_and_the_roster_rungs_agree_both_ways` | both — a rung the lists omit, and a phase the lists carry that no rung implies |
| a declared generated path names no author, or the wrong one | `docs/generated-paths.txt`'s author column, `every_declared_path_names_a_known_author` | every declared path attributes to a roster set name or a reasoned `none(<reason>)` |
| nothing diffs what a job actually ran against what it owed | `cli/src/attest.rs` (`hornvale attest`) | both — owed-but-absent, and present-but-unowed; `none(...)` authors and conditionally-droppable phases reported honestly as their own categories, never as a confident absence |
| a decision record can land inside another campaign's reserved block | `decision_blocks_do_not_overlap_across_campaigns` | committed declarations checked against each other (cannot see a block never declared at all) |

Five decisions were ratified: [0456](../../docs/decisions/0456-a-rule-stated-in-two-places-needs-a-bidirectional-agreement-test.md)
(a rule stated twice needs a two-way agreement test),
[0457](../../docs/decisions/0457-a-generated-paths-author-absence-is-declared-not-deleted.md)
(an author absence is declared, not deleted),
[0458](../../docs/decisions/0458-tracked-ness-proves-coverage-never-freshness.md)
(tracked-ness proves coverage, never freshness),
[0459](../../docs/decisions/0459-the-freshness-reader-observes-both-directions-and-never-gates.md)
(the freshness reader observes, it does not gate), and
[0460](../../docs/decisions/0460-a-committed-declaration-is-checked-against-its-siblings-without-the-remote-ledger.md)
(a committed declaration is checked against its siblings without the remote
ledger).

## What is still owed

Spec §6 named seam-guard as worse off than anything this campaign touches —
a declaration (`docs/audits/seam-guard-roster.md`) and no record of what
happened at all, ever, with no schedule behind it since decision 0148. That
gets an idea-registry row (`TOOL-seam-guard-no-verdict-record`), not a task:
a missing phase can be re-run and a stale artifact re-authored, but a seam
that stopped being guarded leaves no trace of when. Two more rows are banked
for later campaigns: consolidating the three hand-copied shell parsers of
`docs/generated-paths.txt` behind one `scripts/lib/generated-paths.sh`
(`TOOL-generated-paths-shell-parser-duplication`), and the ledger-reading
decision-block check itself, still owed if the block ledger is ever made
committable (`PROC-decision-block-ledger-not-committable`).
