# The Newel — retrospective

Process, not product. The campaign's own findings are in
`book/src/chronicle/the-newel.md`; its rulings and measurements are in
`docs/superpowers/ledgers/2026-09-06-the-newel.md`.

## Both of Nathan's interventions overturned a section that had already passed my own review

The autopilot's G2 self-review and the G3 package both cleared the B3
section. Nathan then read it and said *"I wonder if we conflated or
misdescribed something somewhere along the way."* He was right, and the
conflation was inherited: The Pavement deleted `course.rs` on the argument
that *"with eight real edges the mapping is the identity"* — true of one
step, false of a walk — and I had repeated that as though it settled
whether a walk could drift. It does not, and their own preregistered H1
would have caught it had it ever been run.

His second intervention was larger. I had framed the compass as primary and
the lattice as its approximation; he inverted it — the graph edge is the
movement, the compass word is shorthand — and that reframing produced a
**sharper measurement**, not merely a nicer story. "Does west go west" has
no clean observable. "Does the key take me to the box I was looking at" has
one, and it reads 87.3%.

**The lesson is not "ask the human more."** It is that both errors were
*framing* errors in text that was factually correct line by line, and no
amount of the verification this campaign was otherwise good at would have
found them. Every number in the first B3 section was right. The question it
answered was the wrong question. A self-review checks whether the document
is consistent; it cannot check whether the document is about the right
thing.

## Two defect shapes, with counts

**Prose that overclaims the code beneath it — four times.**

| where | what the prose said | what the code did |
|---|---|---|
| Task 3 | `strip_article`'s doc block | rustdoc had attached `named_neighbour`'s entire doc to it, leaving `named_neighbour` undocumented |
| Task 4 | `seeded_driver` leaves `chart_scope` empty | `Driver::start`'s `refresh()` populates it; the task's own other test proved it |
| Task 5 | `site_note`'s band-B silence is "correct, not a gap" | the rationale was false precision; the real cause is a walk-band-shaped facet match |
| Task 3 | the report's cited test evidence | neither test exercised the path, nor lived in the binary named |

**Enumerations that came back short — three times, each in a report that
had been asked to enumerate.** Task 4's fix report named "all ten callers
(`completion_tests`, `cycle_tests`)"; there is no `cycle_tests` module and
the count was eleven. Task 5's routing table omitted a call site. Task 3's
was the evidence miscitation above. **Every one was found because a
reviewer was told to count rather than to confirm** — the instruction
"verify the enumeration" reliably produced the finding; "check the report"
would not have.

Neither shape is catchable by a gate, and both were caught by review prompts
that named the specific claim to test.

## Reviewers that ran commands beat reviewers that read

The findings that mattered came from reviewers doing something, not reading
something. One built rustdoc and read the rendered HTML — the source looked
fine, which is why nothing else could have caught it. One mutated the
mid-decay rule to "inherit" and confirmed the test failed, proving the test
discriminated. One wrote its own probe interleaving cursor moves with ticks,
catching the failure mode that would have made the map's clauses permanent
again while every existing test passed. One diffed a restored doc block
against the parent commit to confirm it was a restoration rather than a
paraphrase.

Writing review prompts that name the specific doubt — and say *how* to
settle it — was the highest-leverage controller work of this campaign, and
it is cheap. Where I merely listed an area, I got an area-shaped answer.

## Where the process itself misled me

**The model floor.** I dispatched a scoped re-review on haiku, which this
project bans outright for all work. Caught in the same turn and aborted; the
agent touched nothing. The cause is a live conflict: SDD's Model Selection
says *"Single-file mechanical fixes also take the cheapest tier"*, and a
26-line doc-movement diff is exactly that. The project rule is narrower and
wins — but nothing in the SDD skill says so, and **the pull toward the cheap
tier is strongest on the tasks that look trivial, which is where the
original haiku incident happened too.**

**My own plan text carried a banned word.** The lexicon guard rejects "cell"
in the mesh-vertex sense; my Task 5 plan text and its extracted brief used
it throughout, including the example test name. The implementer's first
commit was refused with +19 occurrences and it paid for a rename pass. I had
tripped that same guard on my own probe hours earlier and fixed it locally
without carrying the lesson into the plan I then wrote.

**A ruling nearly died in scratch.** Ruling 2 — the one that unblocked
Task 4 by mirroring the wire's `room` field — was written to the plugin's
git-ignored `progress.md` and never to the committed ledger, while Ruling 1,
made in the same sitting, went to both. It survived only because
`closing-a-campaign`'s step 2A says to grep scratch for ruling-shaped lines
rather than summarise from memory. **This is the exact loss decision 0486
made the ledger durable to prevent, committed by a session that had read and
cited 0486.** Two files open with different lifetimes is not a hazard that
announces itself.

## Two environment hazards, one of which bit

**The shared stash stack.** `git stash` keeps its reflog in the common git
dir, so every linked worktree shares one stack — currently twelve entries
from eleven campaigns. During Task 6 another campaign's stash was applied
into this worktree, leaving five unmerged index entries with no `MERGE_HEAD`
(a conflicted `stash apply`, not an interrupted merge) and live conflict
markers in three tracked files, including `domains/species/` paths this
campaign never touched. Nothing was lost: the other campaign's stash was
still on the stack, I preserved both sides before cleaning, and the task's
own commit predated the pollution (`git commit` refuses with unmerged
paths, which is how the timing was established). Posted to the board as a
technique.

**Parallel reconnaissance in one worktree.** Four concurrent recon agents,
two of which held deliberate mutations. A `cargo build` run during that
window reported *their* tree, and "two dead colour constants" went into the
ledger as a finding before being retracted. The tell was printed and
skimmed past: `Blocking waiting for file lock on build directory`. One of
the four worked this out unprompted and re-ran everything in a throwaway
detached worktree. Read-only fan-out over one worktree is fine; anything
that compiles is not.

## What no gate can see

Two of this campaign's defects were in rustdoc output, and **nothing in the
gate ladder runs rustdoc at all.** A reviewer running
`RUSTDOCFLAGS="-D warnings" cargo doc` also surfaced pre-existing broken
intra-doc links in `tiles.rs` and `almanac.rs`. A board post from
`campaign/the-culvert` already reports the workspace carrying 476
undocumented rustdoc warnings, so this is known and unowned rather than new.
Recorded here because this campaign produced two independent instances of
the class in six tasks.

## The cadence miss

The branch never absorbed `main` until close, by which point main had moved
**194 commits**. CLAUDE.md asks for absorption at every plan-stage boundary;
I ran six task boundaries and took none of them. The absorption conflicted
in exactly two files, both under `docs/audits/`, and both resolved by
consulting `docs/generated-paths.txt` for which authority owns each — the
hand-authored reconciliation TSV took a union of rows, the generated plumb
roster was regenerated. That file's own row warns that resolving the TSV by
regeneration silently drops one side, observed live on `campaign/the-warp` a
day earlier. A cheap resolution this time; the risk of a 194-commit
absorption is that it will not always be.

## Deferred minors, and where each landed

| minor | outcome |
|---|---|
| historical prose names the removed colour constants | accepted as-is; the names are history and no doc links dangle |
| Task 2's report silent on the transcript parsers | corrected in the report at fix round 1; no code gap existed |
| no driver-level test of the completion scopes' first-wins order | carried to the final review's triage; correctness holds, nothing reddens on a swap |
| Task 4's caller enumeration wrong | recorded above as a pattern instance; the engineering conclusion survived |
| Task 5's routing table omitted a call site | fixed at fix round 1 |
| the strip cites `Rate::Ornamental` with no registered `LayerDecl` | accepted as informal rationale; not mechanically enforced |
| no gate runs rustdoc | recorded above; known and unowned, predates this campaign |

## What went right, and is worth repeating

Reproducing all six reports against real output **before** writing the spec
is what turned three of them into different problems than the ones reported.
Two became separate campaigns because the reproduction showed how big they
were, not because anyone estimated. And the two spin-off designs cite this
campaign's ledger rather than copying it, so there is one account of one
afternoon's measurements rather than three partial ones.
