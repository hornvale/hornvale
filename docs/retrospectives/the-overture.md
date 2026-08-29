# The Overture — retrospective

**Merged:** 2026-08-29

## A decision block written into a spec is not a reservation

The spec's header read **Decision block: 0357–0366**, drafted 2026-08-28. When
the campaign reached its closing task, both halves of that range belonged to
other campaigns: `the-upkeep` held 0356–0365 and `the-latch` held 0366–0375,
and `docs/decisions/0366-…` already existed on `main`.

Nothing had gone wrong at merge time. The range was never reserved. It was
written into the spec header as the next free number, which is exactly the
`max+1` failure the project has already named — the ceiling on the `main` you
branched from is not the ceiling on the `main` you will merge into, and the
reservation ledger lives on the canonical box, not in the tree. `make
decision-block NAME=the-overture` was run at close and returned **0436–0445**,
nine records behind it.

The cost this time was ten minutes and a renumber before anything was written.
The cost when it is discovered *after* the records exist is every citation of
every number, in prose that has already been reviewed.

**The transferable form:** a spec header naming a decision block is a claim
about a shared resource on another machine. Either run the reservation in the
same act as writing the header, or write no number there at all and reserve at
the moment the first record is authored. A range in a document, unreserved,
reads to every later session exactly like a range that was reserved.

## The campaign absorbed main once, then not for 186 commits

The standing rule is absorption at **every plan-stage boundary**. This branch
absorbed once, before Task 5, and then ran Tasks 5 through 8 without another —
finishing 186 commits behind `main`.

The absorption at close was, by luck, cheap: one conflict. But the shape of
that conflict is the argument for the rule rather than against it.
`docs/audits/lexicon-inventory.tsv` is a generated **aggregate**, and an
aggregate must never be text-merged: both sides were plausible and the merge
product was neither. `main` had *lowered* two counts by deleting the word;
the branch had *added* rows for files `main` had never seen. Taking either
side wholesale would have been silently wrong in one direction, and a
three-way text merge produced a conflict precisely because both edits landed in
one contiguous block.

What made the resolution trustworthy was not care in reading it. It was
**running the guard against the resolved file** — five tests, all passing —
rather than inspecting the union and finding it plausible. A merged aggregate
that looks right is the failure mode; the only instrument that distinguishes it
from one that *is* right is the thing that generated it.

Had the branch absorbed at Task 6, 7 and 8 as the rule says, each of those
absorptions would have been a handful of commits and this reasoning would not
have been needed at all.

## The scratch ledger died with its worktree, and the module docs saved the close

`.superpowers/sdd/` is per-worktree and git-ignored. The Overture's worktree had
already been recycled back into the pool when this session resumed the campaign,
so the decision ledger and the followup register — the things a G6 digest is
assembled from — were simply gone.

The close was still writable, and the reason is worth naming because it was not
an accident of this campaign's luck. **Every task had written its reasoning into
the module doc comment of the code it shipped**, not only into a task report:
why the sky view reads the ledger rather than the almanac and what two things
were checked before settling it; why `render` takes `&mut self` and what a
`OnceLock` would have cost; why `WorldContext` cannot cross a thread and why
that is a bound rather than an oversight; the measured clone cost with its
table, "recorded here so a later campaign that changes the ledger's size can
tell whether the conclusion still holds".

Nine decision records and a chronicle were derived from those comments and the
commit messages. The scratch would have been faster. The module docs were
**durable**, and they are the artifact a reader six months out will actually
find.

**The rule this supports:** promote findings out of scratch before teardown is
the stated discipline, and it failed here. The compensating practice — write the
*why* at the site, in the code, as you go — is the one that survived a worktree
being handed to someone else.

## A brief's API sketch is a hypothesis about the tree

Task 8's brief sketched a cache API in two shapes that did not survive contact
with the source, and the implementer corrected both rather than building to the
sketch:

- `Cache::write(dir, world)` cannot derive the pins a world was built under.
  Pins are not committed as facts, and `World::derived_under` is a different
  thing — stream-label metadata keyed by label. Verified by grep before the
  module was written, not assumed.
- `load_if_valid` cannot be a method. The cache exists to survive a process
  restart, and after one there is no live `Cache` value for a method to hang
  off.

This is the plan-text failure class the campaign's own plan warned about in its
"what this campaign keeps re-learning" preamble, and the preamble worked: the
implementer's report says how it knows, and the module doc says it permanently.
Task 4 and Task 7 report the same shape against their own briefs. The practice
that catches these is the one the plan mandated — **every step demanding
executable proof, and implementers empowered to override the plan and say so.**

## The two nulls were reported as nulls

Three hypotheses were preregistered. Two came back the way the spec hoped and
one did not, and none of the three was rescued after unblinding.

**H1** — *every view renders at every rung* — is false for two of four views,
and the preregistration had already written down what to do about it: declare
the rung, be skipped, never render blank. The null *was* the design, which is
the cleanest possible case and worth noticing as a template: a hypothesis whose
null has a named response costs nothing to falsify.

**H2** — *a cached start under 1 s* — measured ~1.2–1.4 s. The cache shipped
anyway, and the record says why in a form that can be re-checked: the residual
is independent of genesis's own cost, so the saving grows as genesis grows. That
is a conditional argument with a named predicate, not a rationalisation, and a
later campaign can falsify it by measuring whether the tail scales with world
size.

**H3** — the tripwire — was flagged in the spec as *likely to fail*, explicitly
so that a null would not be misread as the cache being unsafe. It held. Flagging
the expected direction in advance cost one sentence and would have been worth it
either way.

## Small things

- The plan's step checkboxes were never ticked. The commit log carried the
  progress instead, and reconstructing task state from it was unambiguous —
  but it works only because every task commit named its task.
- The absorption's gate ran at load average 62 on a box shared with several
  other campaign sessions. `cpu_ratio=5.92` on that row is the tell; the wall
  time is not comparable with anything.
