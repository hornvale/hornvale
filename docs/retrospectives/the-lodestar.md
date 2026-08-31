# The Lodestar — retrospective

A follow-up branch closing findings from [The Gallery](./the-gallery.md)'s
whole-branch review, which ran *after* that campaign had merged. Process
lessons only; the product account is in
[the chronicle](../../book/src/chronicle/the-lodestar.md).

## What this campaign is evidence for

**Twelve per-task reviews passed and a thirteenth, whole-branch review found
three Criticals.** Not through negligence — several of those per-task reviews
were unusually good, recomputing numbers from source rather than reading
reports, and one of them refuted a plausible finding by building a scratch
crate to check whether an attribute suppressed a compile error. They passed
because each was scoped to a task, and three of the four defects lived in the
space *between* two tasks:

- a renderer written one task before the producer that would fill it, saying
  so in its own doc, and never reopened;
- a specification clause (`rank ≤ 5`) whose wrong reading propagated
  faithfully into five documents;
- a wire schema with two client mirrors, of which the campaign enumerated one.

**A join has no owner.** The task that wrote the renderer was correct at the
time. The task that filled the field was correct, and its reviewer verified
the producer. Neither was wrong; the pair was.

The cheap instrument is not a better per-task review. It is one question asked
once at the end: *which of the things this campaign said it would do can a
player actually observe?* The Gallery's spec had nine numbered acceptance
criteria. Checking them against shipped code took one reviewer one pass, and
criterion 6 — "meets something living in it" — was not met.

## The failure that recurred twelve times

Prose that a diff makes false. The Gallery hit it eleven times; this branch
found the twelfth, in the renderer's own doc.

The remedy took most of the campaign to find, and each version failed in a
way that taught the next:

1. *"Re-read comments adjacent to your change."* Fails structurally — the
   stale comment is adjacent to the **thing the change was about**, often in a
   file the fix never opened.
2. *"Grep the crate for prose describing what you changed."* Better, and still
   fails: one sweep searched `per keypress|per redraw` and missed a line
   saying "re-derived"; another searched `no creature is placed` and missed one
   saying "no creature arm at all". **You grep the words you would write, not
   the words the stale prose used.**
3. *"Take the search terms from the phrases the SPEC uses to name the
   before-state."* This one works. A spec that quotes the old behaviour is
   telling you exactly which strings to hunt, and it found both remaining
   instances — including one it correctly declined to touch, because the match
   was another campaign's accurate historical record about a different
   subsystem.

Knowing which matches to **leave** is as much of the skill as finding them.

## Tests that pass while proving nothing

Three of the four fixes were assertions green for reasons unrelated to what
they guarded: a quantization check whose needle was a substring of the raw
value it was meant to reject; a test naming a branch its fixture never
executed; a monotonicity check trivially satisfied by a set that never
shrinks.

**Reading cannot distinguish these from working tests**, because reading tells
you what a test says and not what it can detect. So every fix here carried a
demonstration: remove the code the test protects, run it, record the failure,
restore. A reviewer then reproduced all four independently rather than reading
the transcript — a claim that a test catches something is itself a claim, and
checking it costs a minute.

One of the three had been copied verbatim from an older test with the same
defect. A weak assertion propagates exactly like a strong one.

## The close that lost eleven items

**The Gallery's own close promoted its follow-ups and its process lessons into
its retrospective — and lost all eleven of its per-task deferred minors.** Not
one of `CellGrid`'s untested degenerate path, `shadowcast_with`'s missing
re-export, the dead `Underground::step`, the duplicated transparency predicate,
or the rest appears anywhere in the committed record. They survived only
because that campaign's worktree had not yet been recycled when this branch
went looking.

The `closing-a-campaign` skill names this exact check — *for each item, name
the committed file and line it landed in* — and cites a prior campaign that
promoted nine items and still lost six. The Gallery ran the close, wrote a
193-line retrospective, and skipped that verification. **"It's in the
retrospective" is a belief; `grep` is a check**, and the two disagreed on
eleven counts.

They are recorded below, which is the point of writing this down.

## Rescued from The Gallery's ledger

Each is small; the value is that they exist somewhere `git` can see.

- `CellGrid::new`'s `saturating_mul`/`.max(0)` degenerate-extent path is
  untested — harmless robustness, no action needed.
- `sight.rs`'s doc still cites `kind_of` as the conceptual authority after the
  wrapper inlined the equivalent `.get`; proven equivalent, cosmetic.
- `Underground::step`, the all-in-one form, is `#[allow(dead_code)]` with no
  production caller since the peek/commit split. Dead weight; deletable.
- `take_stairs` and `sight_reach` shipped narrower than their briefs said
  (`pub(crate)` and private). Both defensible — the narrower visibility keeps
  them off the type-audit boundary and matches their siblings.
- The transparency predicate is written verbatim twice, in `underground_level`
  and `mark_underground_seen`. Identical today; a future impassable cell kind
  would silently desync fog from the pane.
- The comment that replaced Task 6's stale one **reintroduces the coupling
  class it fixed** — it asserts as present-tense fact what another file does.
  Fixing an instance without noticing you rebuilt the mechanism is how a class
  of bug becomes permanent.
- Two lexicon-inventory characterisations in reports were imprecise (an
  area-sense claim for a markdown-table-cell token; unrelated rows that only
  decrease). The guard counts, never classifies, so neither affects
  correctness.
- Plan defect: Task 1's file list named `underworld_level/region.rs`, which
  contains no relevant reference and was correctly left untouched.

## Controller errors worth recording

The session running this made four of the same class it was catching:

- **Asserted a mechanism from how code looked rather than by running it** —
  the spec claimed a new enum variant would be a compile error in the client.
  It was not; the client has its own mirror enum. That claim was then repeated
  into a task dispatch, telling an implementer something untrue.
- **Got the viewport arithmetic wrong**, and it propagated to five documents
  before a reviewer re-derived it.
- **Put a raw `|` inside a registry cell** — a column separator in a markdown
  table — and wrote the row twice. The docs gate caught both immediately.
- **Ran a generator bare so it printed to stdout**, leaving the artifact
  unwritten while the drift check read clean. `CLAUDE.md` warns about this
  exact hazard; checking `git diff --stat` rather than the exit code caught it.

The pattern in all four: a check that *looks* like verification but answers a
neighbouring question. It is the same failure the campaign spent twelve tasks
cataloguing, which is worth stating plainly rather than filing as irony.

## Process notes

- **A merge landed without authorization.** The Gallery's final task ran
  `make sluice` and pushed to `main`. Its brief's closing section named the
  target and the `closing-a-campaign` skill covers merging, so the instruction
  was ambiguous enough to read as in-scope. The controller had excluded
  censuses and out-of-worktree deletions explicitly and did not exclude the
  merge. Consequence: the whole-branch review ran *after* the merge, so its
  three Criticals landed on `main` first.
- **The stage-gate cadence was missed** for the whole of The Gallery — noted in
  its own retrospective, and worth repeating because this branch's stage gate
  came back green in 1145 s and would have been cheap at any point.
- **This branch absorbed 180 commits of `main` across five campaigns** and the
  merge was textually clean. The semantic check — did another campaign change
  the same idea — mattered more: `the-errata` is a corrections campaign landing
  in the same territory, and confirming it had touched none of the same files
  is what made the absorption safe. A clean `merge-tree` would not have told
  anyone that.
