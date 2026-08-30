# The Cartulary — the ledger outlives its worktree

**Branch:** `campaign/the-cartulary`, from `origin/main` @ `cd6547134` ·
**Decision block:** 0486–0495 · **Drafted:** 2026-08-30 ·
**Status:** G3 package pending.

*A cartulary is the book a house copied its charters into, so their contents
would survive the loss of the originals.*

---

## 0. What this is

A campaign's decision ledger — its rulings, their costs, its deferred and
parked findings — lives in `.superpowers/sdd/`, which is git-ignored and
per-worktree. It dies when the worktree is recycled. The prescribed remedy is
to **promote material into the retrospective at close**, by hand.

That remedy has now failed **five recorded times**, and the last two failures
happened *after* campaigns added checks to prevent it.

This campaign stops relying on a manual copy and makes the ledger a committed
document.

## 1. The failure, as this repository already records it

| campaign | what happened |
|---|---|
| The Ell | promoted nine items, lost six |
| The Quoin | *"the material was written down, the instruction to promote it was written down beside it, and it was still lost"* (`docs/retrospectives/the-quoin.md:219`) |
| The Gallery / Lodestar | wrote a 193-line retrospective, skipped the verification; belief and `grep` **disagreed on eleven counts** (`docs/retrospectives/the-lodestar.md:92`) |
| The Overture | worktree recycled before close; nine decision records reconstructed from module doc comments |
| The Attestation | worktree recycled **between the merge landing and the close walk**; nine deferred minors and two parked findings lost, reconstructed from a session transcript |

**The Quoin's reading is the important one and this spec adopts it:** the
second data point matters more than the first, because one loss reads as a
careless close and two do not. The check is not a backstop for sloppiness; it
catches the ordinary case.

**And the two most recent failures already tried the obvious fix.** The Quoin
and The Lodestar both responded by adding *verification* — a step that reads
the ledger and confirms each item landed. That is a reasonable remedy and it
did not hold tonight, for a reason verification cannot address: **the ledger
was gone before anything could verify against it.** A check that requires an
artifact to exist cannot detect that artifact ceasing to exist.

**The Attestation's own sequence is worth stating exactly, because the
reasoning error is the interesting part.** Its controller read
`closing-a-campaign`'s warning, wrote the retrospective, and then deliberately
kept the worktree "until it lands" — reasoning that the merge landing was the
safe point. **Landing is what makes a worktree eligible for recycling:**
`worktree-take` selects a pool member whose branch is already merged. The
guard was inverted. The moment it was judged safe to keep was the moment it
became unsafe to keep.

## 2. What becomes durable, and what deliberately does not

Measured across three live campaign worktrees (2026-08-30):

```
                regenerable (.diff)     durable prose
  the-chattel        3,412 K            208 K  (10 files)
  the-stile            964 K            204 K  (21 files)
  the-winze             16 K            196 K  (15 files)
```

The prose is remarkably stable at ~200 K per campaign; the bulk is review
packages, which vary by two orders of magnitude.

**Only the ledger moves** — roughly 15 K per campaign. Reports, reviews and
review packages stay in `.superpowers/` and still die with the worktree.

The split is principled rather than budgetary. A **review package is
`git diff` output**, regenerable by construction from two SHAs. An
**implementer report is evidence for** a ruling the ledger already records.
The ledger is the only artifact in that directory that is nobody else's
derivative — lose a report and you lose the working, lose the ledger and you
lose the decision.

**Accepted cost:** The Overture reconstructed nine decision records from
module doc comments because its reports were gone. Under this design that
would happen again. The judgment is that a lost report costs a campaign's
effort and a lost ledger costs the project's memory.

## 3. Where it lives, and the cadence

`docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md` — the third sibling to
`specs/` and `plans/`, same naming convention, same tree.

**Per-campaign paths are what make committing safe**, and this is the load-
bearing point. `CLAUDE.md` warns that a committed ledger "silently clobbers
every parallel session's on absorption, raising no conflict." That hazard is
real and it is a property of the **shared filename**: every campaign writes
`.superpowers/sdd/progress.md`, so two of them editing it merge to one side
silently. A path keyed by campaign is touched by exactly one campaign, ever.
An absorption sees a clean add. There is no collision to have.

**The controller commits at each task boundary and each ruling** — small
docs-only commits that skip `gate-commit` because no Rust path is staged.
Roughly 8–12 per campaign. The ledger's git history becomes the campaign's
decision history, readable with `git log -p` over one file.

**Committing only at close would not fix the observed failure.** Both
worktree-recycling losses happened before or during the close walk; a ledger
that first reaches git at close is exposed for the whole campaign.

## 4. The supersession, which is the substantive work

Two committed statements become false. They are **superseded visibly, not
edited**, per the project's own idiom and this spec's own subject.

**`scripts/hooks/pre-commit`:**

> "The ledger is explicitly NOT the durable record — material entries are
> promoted into the spec at close — so committing it is always a mistake,
> never a judgment call."

The premise is "promotion at close works." §1 records five failures. What was
"never a judgment call" becomes a judgment that was made and has been
falsified by measurement.

**`CLAUDE.md`:**

> "never force-add it: a committed ledger silently clobbers every parallel
> session's on absorption, raising no conflict"

Dissolved by per-campaign paths (§3), not overruled. The sentence stays true
of `.superpowers/sdd/progress.md` and stops applying to the new path.

**The hook's guard itself is unchanged.** It keeps refusing `.superpowers/`,
which remains correct for the up-to-3.4 MB of regenerable diffs that stay
there. Only its stated *rationale* narrows.

## 4a. TWO ledgers, two owners — found at plan time, and it reshapes §3

The plan-writing pass established something this spec did not know: there are
**two** ledgers, with different owners and different editability.

```
  decision-ledger.md   rulings, Q entries, ideonomy passes
                       defined by .claude/skills/campaign-autopilot/ -- IN THIS REPO

  progress.md          task state, fix rounds, deferred minors, parked findings
                       defined by the VENDORED superpowers plugin at
                       ~/.claude/plugins/cache/.../superpowers/6.3.0/ -- NOT in this repo
```

**A campaign cannot change where the plugin writes**, and a local edit to a
versioned plugin path would be overwritten by the next plugin update. So §11's
"update `subagent-driven-development`'s workspace resolution" is not a task
this repository can perform, and the material lost in The Attestation — the
deferred minors and parked findings — lived in exactly that file.

**The resolution is better than the original framing, and does not involve
mirroring.** Copying from scratch to durable at intervals would reintroduce the
manual copy this whole campaign exists to remove.

Instead: **the committed ledger is the PRIMARY home for the durable kinds, from
the start.** Rulings, deferred minors and parked findings are written to
`docs/superpowers/ledgers/<slug>.md` when they occur, by the controller, per
the in-repo skills that this campaign *can* edit (`campaign-autopilot`,
`closing-a-campaign`, `dispatching-hornvale-subagents`).

The plugin's `progress.md` keeps its own separate job — resume-after-
compaction and task state — and stays scratch. That is correct rather than a
compromise: **its purpose does not need to survive worktree recycling**, because
a recycled worktree means the campaign is over, and its task-completion lines
are recoverable from `git log` regardless.

So nothing is mirrored and nothing is copied. Two artifacts, two jobs, one of
them durable.

## 5. The check, and what it cannot see

A ledger nobody writes is worse than one that dies, because it looks like a
record. So: a campaign whose spec and plan both exist must also have a
non-empty ledger.

**It must be a ratchet, not retroactive.** There are 311 specs and 293 plans
in the tree and zero ledgers; a check applied to history would redden for
every campaign ever run and be deleted within a day. It applies to campaigns
starting after this one lands, by a mechanism the plan determines — an
append-never exemption list of existing slugs is the obvious shape and is what
`registry-length-waivers` and the seam-guard declarations already do.

**Stated blindness:** it can see that a ledger exists and is non-empty. It
cannot see whether the contents are honest, complete, or written contemporan-
eously rather than backfilled at close. Those are exactly the properties that
matter and none of them is mechanically checkable — which is the same
three-valued honesty `tropes check` and type-audit's `waiver(...)` carry.

## 6. What this does NOT do

- **It does not make reports or reviews durable.** They stay scratch (§2).
- **It does not add a gate to campaign commits.** A hook enforcing "no commit
  without a ledger entry" was considered and rejected: a gate that fires for
  benign reasons trains people to ignore it, which is the disease decision
  0426 diagnosed in the heavy tier.
- **It does not touch the determinism contract**, any seed label, or any
  generated artifact.
- **It does not retrofit ledgers for past campaigns.** Their material is gone
  and inventing it would be worse than recording that it is gone.

## 7. What is unverified, and how each is settled

| claim | status | settled by |
|---|---|---|
| the shared filename, not committing, is what causes the absorption clobber | **verified** — read at spec time from `CLAUDE.md`'s own wording | the hazard is stated of one path |
| ~200 K prose / up to 3.4 MB diffs per campaign | **verified** — measured 2026-08-30 across three worktrees | `du` over three live scratch dirs |
| promotion-at-close has failed five times | **verified** — four in committed retrospectives, one in this session | `docs/retrospectives/{the-quoin,the-lodestar}.md` and the two campaigns' own records |
| a committed ledger survives worktree recycling | **hypothesis (H1)** | recycle a worktree mid-campaign, read the ledger from git |
| committing does not change what gets written | **hypothesis (H2)** | see §8 — probably unfalsifiable from inside |

## 8. Preregistered measurement

- **H1 — the ledger survives recycling.** Falsifiable by construction: take a
  worktree mid-campaign, recycle it, read the ledger back from git. If this
  fails the campaign has no purpose and should stop.
- **H2 — committing does not change what gets written into the ledger.** **I
  predict it DOES, slightly**, and I expect not to be able to measure it from
  inside. Recorded so the prediction is on the record before the fact rather
  than rationalised after. The honest report at close is likely "unmeasurable
  from here"; a campaign that claims to have confirmed H2 should be
  disbelieved unless it names its instrument.

## 9. Flagged for Nathan at G3

1. **This reverses a documented position** — a hook comment stating that
   committing the ledger "is always a mistake, never a judgment call." The
   reversal is evidence-backed (§1) but it is a reversal, and the record
   should show it was made deliberately.
2. **A committed ledger may change candour.** §8's H2. The entry most worth
   keeping — "I do not know why this works" — is the one most likely to be
   softened when written for the record. This spec accepts that cost and
   cannot measure it.
3. **The check's exemption list is append-never and starts at ~293 entries.**
   That is a large frozen list; the plan must decide whether it is enumerated
   or expressed as a date/threshold, and either choice has a failure mode.

## 10. Decisions to promote (0486–0495)

- **0486** — a campaign's decision ledger is a committed document, not scratch.
- **0487** — per-campaign paths are what make a shared record safe to commit;
  the absorption hazard is a property of the filename, not of committing.
- **0488** — a record that must be manually copied to survive will not
  survive; five instances.
- **0489** — verification cannot substitute for durability: a check that reads
  an artifact cannot detect that artifact's absence.

## 11. Task outline

1. Create the `ledgers/` tree and move this campaign's own ledger into it —
   the campaign uses its own deliverable from task 1 onward.
2. Supersede the hook comment and `CLAUDE.md`'s sentence, visibly.
3. The ratchet check, with its exemption mechanism and its stated blindness.
4. Update the IN-REPO skills (`campaign-autopilot`, `closing-a-campaign`,
   `dispatching-hornvale-subagents`) so the durable kinds are written to the
   committed ledger when they occur. The vendored plugin is not editable and
   is not touched — see §4a.
5. H1: recycle a worktree mid-campaign and read the ledger back.
6. Artifacts, book, chronicle, retrospective, decisions, registry.
