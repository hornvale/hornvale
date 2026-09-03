# Campaign The Plumb — retrospective

**Merged:** 2026-09-02 · **Ledger:** `docs/superpowers/ledgers/2026-09-02-the-plumb.md` (39 entries)

Process lessons only. The product — `tools/plumb`, decisions 0586 and 0587,
one worked conversion and 26 published findings — is in the chronicle.

Cost, stated first because it prices everything below: **six review rounds
across three of five tasks**, and **two of the four lessons here were found by
reviewers rather than by the campaign**.

## 1. A verdict vocabulary invites answers in a neighbouring vocabulary that sounds like it

Four of the first ninety `universal` verdicts gave reasons of this shape:

- *"a calibration knob, not a physical constant"*
- *"a game-design coefficient, not a tuned physical constant"*
- *"a fixed climate threshold …, not a species property"*

Every one answers **where the number came from** — authored, derived, measured.
The rung asks **along what axis it varies**. A confident answer to the first is
indistinguishable from an answer to the second, which is what makes this the
swept verdict the campaign was chartered to prevent, wearing a tag saying
somebody decided.

The tell is the word *not*: all four define themselves against a category
rather than naming an axis. The sharpest — *"not a species property"* — sits
directly above a doc reading *"a room's **people** build around a fire"*, which
names the correct rung in the sentence the tagger was reading.

**Nothing mechanical detects this.** A tag format that accepts free-text
reasons cannot, and a stricter grammar would only move the guessing. The only
instrument is reading the reason against the question, which is what the
reviewer did. Generalizes past this campaign: any scheme with a free-text
justification attracts justifications from the adjacent question.

## 2. A check's silence is a claim about the check's REACH, not about the tree

The branch was red for two commits while the controller ruled on a
blast-radius comparison, because a committed byte-golden had moved — 89 of
about 410 lines. Both instruments consulted were structurally incapable of
seeing it:

| instrument | why it could not speak |
|---|---|
| `make rebaseline` + the generated-paths drift check | the fixture is not a declared generated path, and the script does not write it (`REBASELINE=1` does) |
| `make gate-commit` | `grep -c 'affect_trace' docs/timings/subfloor-roster.tsv` is **0**, against 386 `hornvale-lab` entries |

Both were green and both were honest about what they ran. The ruling that
depended on them said *"nothing else declared in `docs/generated-paths.txt`
moved; no census touched"* — literally true, materially wrong, and the two
load-bearing words are *declared* and *census*.

**Two checks agreeing is worth nothing when they share a blind spot**, and
these two shared exactly one. Two memory entries already covered the halves
(`check-the-mechanism-a-discharge-names`, `an-empty-diff-needs-a-positive-control`);
neither fired, because the campaign had correctly asked the implementer to
*run* the artifact branch table rather than predict it, got an honest silence,
and never asked whether the instrument could speak. Running the right command
is not the same as running a command that can answer.

The remedy shipped is a procedure, not a check: after any behaviour change, run
byte-golden tests explicitly (`cargo nextest run -p hornvale-lab -p hornvale-vessel`)
rather than inferring their state from the artifact diff.

## 3. A COUNT of findings is not PUBLISHED findings

For most of the campaign the committed artifact — `docs/audits/plumb-roster.md`,
drift-checked, regenerated, reviewed — carried `per-species | 19` and per-crate
totals, and **did not name a single constant or reason**. The actual list, which
is the campaign's entire product for the project owner, existed only in
`.superpowers/sdd/`, which is git-ignored and dies with the worktree.

A count satisfies every drift check, reads as complete, and is the half that
carries no information a reader can act on. Nothing would have complained. The
closing sweep's own rule — *name the committed file and line each promoted item
landed in* — run against this campaign an hour later would have found nothing
to name.

Worse, this was found by pulling a thread about something else: the re-review's
actual finding was that a report claimed two edits it never made. The
countless artifact was collateral.

**A tool that reports a count of findings has not published its findings.** The
fix was in the artifact (a Fidelity findings table of 27 rows with file, line,
rung and reason), not in prose moved somewhere safer.

Its sibling, one absorption later: **a default-deny gate defends the PRESENCE
of a verdict, never its CONTENT.** Taking main's side of three files during the
325-commit absorption was provably lossless for the code — our changes there
were purely additive — and would have deleted two `per-individual` verdicts,
with re-tagging them `pending(wave-1)` satisfying every check. When an
absorption discards your side of a file, enumerate what your side **asserted**,
not just what it changed. The diff answers the second question; only the
campaign knows the first.

## 4. A correction is unaudited text — ~35 instances, several inside the entry recording the previous one

The ledger catalogues roughly thirty-five claims that outran their evidence.
The distribution is the finding: a large share were written by the controller,
in rulings and plan text, and several sit inside the paragraph correcting the
last one.

The sharpest is #34 → #36. A ruling argued that converting `REST_BOUT` alone
inverts `REST_BOUT < SLEEP_BOUT` above `L = 1.6` — reasoning entirely from the
two constants' definitions and an assertion's text. `SLEEP_BOUT` is not the
sleep span; it is a floor that usually does not bind, and its consumer says so
200 lines away. The inversion was real, the mechanism was wrong, and the wrong
mechanism was named by reading a constant instead of its consumer. It was
written three hours after the ledger entry about doing exactly this.

Second sharpest, #20: entry #18 claimed a tense correction had been applied. It
had not. The substitution, the paragraph claiming it, and the verification went
into **one shell command**; the substitution raised an `AssertionError`, the
`cat >>` appended the claim anyway, and the failure printed afterwards. Same
mechanism as the predecessor campaign's instance 25 — which produced the memory
entry `never-chain-a-correction-to-its-own-assertion`, **written in this same
session and not applied in it**.

That is the second data point for a conclusion worth more than either instance:
**a written rule did not change the behaviour.** What changes it is a
mechanical habit with no judgement in it — *an edit and a claim about the edit
never go in the same command*. Apply, print, stop, read, then write the claim in
a separate call.

## 5. Two smaller ones, both about the verify-the-brief step

**Verifying the brief's SUBJECT is not verifying its DELIVERABLE.** The spec
asked for a default-deny accession gate. That gate had shipped thirteen days
earlier (The Accession), `cli/CLAUDE.md` indexed it in one sentence, and the
predecessor campaign's own ledger described it firing. The pre-dispatch check
examined the subject thoroughly — the registry assembly path, the cohort sizes,
the neighbouring test — and never asked whether the thing being specified
already existed. **Read the directory `CLAUDE.md` before specifying an
enforcement test.** The task became a real strengthening rather than a
duplicate (anti-vacuity floor `>= 76` → `>= 253`, which is the finding: the old
floor would have passed a sweep reaching 76 of 253 concepts), but only because
the implementer declined to build what was asked.

**The step running one task ahead earns its cost, repeatedly.** It caught two
plan-text defects in Global Constraints alone, one of which would have failed
in this campaign's own worktree: the plan cited `type-audit` as a std-only
precedent (it depends on `syn` and `proc-macro2`, because it parses Rust rather
than scanning lines), and omitted that a `tools/` manifest needs an empty
`[workspace]` table or cargo binds to the outer workspace — *"e.g. a worktree
under `.claude/worktrees/`"*, in `type-audit`'s own comment, which is precisely
where this campaign ran.

## 6. Process discipline that lapsed

**`grep -in "deferred minor\|parked"` over the ledger returned nothing across
38 entries**, while four review rounds raised minors throughout. Every *ruling*
was ledgered contemporaneously and no *minor* was. The asymmetry is worth
naming: a ruling feels like a decision and a minor feels like a note, so only
one of them triggered the habit. They were backfilled as entry #39 at the
close, which is exactly the promotion-at-close practice The Cartulary exists to
remove.

The absorption cadence held in one direction and not the other: main was
**325 commits ahead** at the point of absorption. That was deliberate rather
than neglected — the ratchet made absorbing before Task 5 the right sequencing,
and Task 5 edits a file main had been moving — but a 325-commit gap is a
stage-boundary cadence that did not run, and it produced eight conflicts.

## Deferred minors, and where each landed

`closing-a-campaign` step 2B: *"it's in the ledger" is not a location.* All five
minors this campaign recorded (ledger #39), with outcome and location:

| Minor | Finding | Outcome |
|---|---|---|
| M5 | `REST_BOUT > WAKE_SCAN_STEP` now pins the `L = 1` anchor only, while production varies with `L` — the same "test pins `L = 1`, production doesn't" shape Task 5 exists to fix, one assertion over | **Doc, not assertion.** A paragraph on `a_bouts_length_is_a_property_of_the_act_not_of_the_next_scan_step` naming the measured limit: repayment is `L`-invariant at ~0.125 against a 0.1 floor across 4–100 h, and only a 4.0–4.8 h world puts a rest under one scan step. `windows/vessel/src/liveness.rs` |
| M6 | The `#[cfg(test)]` text-scan guard splits on a **doc comment about the attribute** (`liveness.rs:6251`) rather than the module (`:8166`), leaving ~1,900 lines of production unscanned | **Idea-registry row**, `TOOL-a-text-scan-guard-splits-on-prose-about-its-needle`. Not fixed: it is pre-existing, carries its own design question, and this campaign had no business rewriting it at close |
| M7 | `#[allow(dead_code)]` on `REST_BOUT` is a permanent suppression | **Accepted as-is.** Correct *given* M6 — a `#[cfg(test)]` there would truncate the guard's window and re-break it — and the comment says so. Moot when M6 is fixed |
| M8 | A comment said a terrain reduces to the flat span when `day_ticks() == None`; the real convention is `day.filter(\|d\| d.ticks() > 0)`, so zero and negative also fall back | **Fixed**, both sites (`REST_BOUT`'s doc and `act_span`'s `Action::Rest` comment). `windows/vessel/src/liveness.rs` |
| M9 | *"`WAKE_SCAN_STEP`, 7.2 minutes"* is wrong by 10x — the constant is `TICKS_PER_STD_DAY / 20` = 0.05 std days = **72 minutes**, as its own doc says | **Fixed.** The ledger recorded **two** sites; `grep` at close found **four** (`liveness.rs:2668`, `:2731`, `:10145`, `:17355`), all corrected. A count of instances is itself a claim — this campaign's own subject, one last time |

## Carry forward

- **An edit and a claim about the edit never go in the same command.** Apply,
  print, stop, read, then write the claim separately. A recited rule is not a
  followed rule; only the shell's inability to express the mistake is.
- **Before trusting "nothing moved", confirm the thing that would have moved is
  inside what the check can see.** Silence is a fact about reach.
- **A tool that publishes a count has not published its findings.** Ask what a
  reader could act on from the committed artifact alone.
- **Verify the DELIVERABLE, not only the subject.** Grep for the described
  mechanism and read the directory `CLAUDE.md` before specifying a new gate.
- **Ledger a minor when it occurs.** A note that does not feel like a decision
  is the one that gets promoted from memory at close, which is the practice
  already known to fail.
- **A free-text justification attracts answers from the adjacent question.**
  Read the reason against the question, not against the format.
