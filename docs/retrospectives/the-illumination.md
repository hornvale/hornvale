# Retrospective — The Illumination

**Campaign:** colour, glyph, and the chart at world scale.
**Decision:** [0142](../decisions/0142-a-channel-carries-one-axis-and-a-lost-axis-is-declared.md).
**Chronicle:** [The Illumination](../../book/src/chronicle/the-illumination.md).

Process lessons, not product. The product argument is in the chronicle.

## The tally, and where it came from

**Twenty-two numbered findings. Essentially all of them originated in the
controller's own plan and dispatch text; none originated in implementer
code.** That is not a new observation for this project — it is now the
modal shape of a Hornvale campaign — but the magnitude is worth stating
plainly, because the corrective effort keeps being spent on the wrong half
of the pipeline. Reviews of implementer diffs found real things. They found
them at a rate far below the rate at which reading the *brief* against the
tree found things, and every serious finding this campaign produced came
from verifying a plan sentence one task before it was dispatched.

Concretely, and each of these was a sentence written with confidence:

- A probe placed in a crate that structurally cannot build the type it
  probes (a layering inversion that does not compile).
- A cross-task handoff through a Cargo `examples/` binary, which is not
  importable as a library — not cross-crate, not even from tests in its own
  crate.
- A hypothesis whose baseline had no capture point anywhere in the plan; by
  the time it was compared against, the pre-change value was unrecoverable.
- A geometry test specified at depth 4 that needs depth 27 to converge.
- A ruling that enumerated three requirements and then dismissed the third
  as already satisfied, having confused "public in the kernel" with "on the
  wire."
- A task whose steps named one regeneration path when the campaign's own
  spec documented three, leaving two committed fixtures stale for two whole
  tasks.
- A grep published with a count but not its criterion — case-sensitive
  where the codebase is not — which missed a real site the implementer then
  found.

**The practice that worked, and it is cheap.** Verifying each task's brief
against the tree *one task ahead of dispatch* caught seven of these before
any code was written. That is the single highest-yield activity in the
campaign and it costs a few greps. It should be a standing step, not a
habit that happens to have been followed here.

**The practice that did not scale.** Reviewing an implementer's diff cannot
find a defect in the instruction the diff faithfully executed. Several
times the implementer did exactly what was asked and the answer was wrong;
in each case the finding is against the controller and the review seat was
the wrong instrument to have deployed.

## Twelve checks that read as protection and were not pointed at what they claimed

This is the campaign's densest single pattern and the one worth carrying
forward. In every case the check was *correct as written*. Reading it
harder would not have found any of them.

1. **A ceiling that passed on the exact regression it was written for.**
   The guard bounded distinct colours at 20 to catch a rejected continuous
   design. Asked to *demonstrate* rather than assert that it would fire, the
   rejected design measured **18**. Now 9, with the red-on-continuous and
   green-on-tiered runs both recorded.
2. **A test whose comment claimed it pinned a field it never checked.** Its
   own docstring said it existed so "a future change cannot add either by
   accident." It asserted nothing about the field. The predicted red never
   came, and the implementer reported the absence rather than implying
   otherwise — which is how it was caught.
3. **A guard whose fixture could not reach the code it guarded.** The
   escape-free lens test rendered a scene containing no cell of the state
   whose handling changed. It stayed green while precisely that path was
   rewritten.
4. **A positive control that discriminated nothing.** An absolute value was
   taken to prove a sign was irrelevant; removing it left the test green,
   because at the fixture's magnitudes both signs rounded into the same
   band. Found only because a review brief instructed the reviewer to assume
   another instance existed.
5. **A wire declaration that had become false.** The document announced that
   colour cannot vary below grid resolution — published by the campaign
   whose purpose was making it vary, while the sim's own new test asserted
   the opposite.
6. **A field with no guard in its owning crate.** Mutating it to a constant
   left the entire crate green, goldens included, because that crate's
   byte-goldens are uncoloured and carry the key at all. Its only guard was
   a golden two crates away, rebaselined in the same commit.
7. **Two committed fixtures stale for two tasks**, invisible to three
   independent instruments: not on the drift-checked path list, not written
   by the everyday regeneration command, and not in the commit gate's test
   roster. Surfaced by luck, during unrelated due diligence.
8. **A control whose emptiness was read as a result.** A regeneration diff
   came back empty and had to be *proved* correct rather than assumed —
   which it was, by establishing that no committed artifact path can reach
   the colour rendering at all.
9. **A count published without its criterion** (case-sensitive grep against
   a mixed-case corpus), in the campaign whose recurring lesson is exactly
   that.
10. **A stale legend table** describing a ladder as pure relief, a full
    campaign after it stopped being one.
11. **A memory-substitution twin in the browser client** that survived the
    sweep which deleted its sibling, because the sweep was scoped to one
    language rather than to the rule.
12. **A decision-number claim that no tree-based search could see.** A
    number announced but not yet written is invisible to every mechanical
    check; "no collision in the tree" and "no collision" are different
    propositions, and the check that eventually catches a duplicate only
    fires after both are merged, when renumbering is most expensive.

**The remedy that consistently worked was making the check fail on
command** — twelve mutations applied and reverted in one task alone, five
positive controls in another, a real `NaN` reproduced by removing a clamp.
**The remedy that consistently failed was reading the check.**

**And one clause to add to the standing practice.** This project already
asks that a check be made to fail on command, and that it be answerable to
an enumeration. Finding 4 adds a third: **a positive control must be shown
to discriminate, not merely to exist.** A control that cannot separate the
two hypotheses it names is a cannot-fire check with better manners.

## The decision number went round a loop, and the loop was already filed

This campaign claimed 0142, released it on discovering that a campaign which
did not exist at claim time held 0142–0144 committed on its branch, took 0145,
and was then **unable to commit at all** — `no_gaps_in_the_decision_log` is in
the commit gate's roster, and a branch spanning `0001..0145` with three holes
fails it. The number came back to 0142 only because the other campaign's merge
was itself held, making the wait unbounded, and because repo precedent from the
0132/0133 collision is that the first to merge keeps the number.

**The underlying conflict is not ours to claim as a discovery.**
[The Sluice's retrospective](the-sluice.md) filed it first, under the heading
*"A decision number wants assigning at merge time"*, and stated it better:

> The no-gaps and no-collision invariants on `docs/decisions/` are mutually
> exclusive under parallel campaigns, and the gap check pushes an author into
> the collision the uniqueness check exists to catch. The queue is the only
> mechanism that could assign at merge.

Both guards shipped anyway, each catching what the other creates. This campaign
is simply the instance where the trap closed completely: every available move
was refused by one guard or the other, and the resolution had to come from a
human ruling rather than from anything in the tree.

**Outcome, recorded rather than performed:** Nathan approved dropping
`no_gaps` and moving number assignment to merge time. That is not this
campaign's work and was not done here — it is the disposition of the blocker,
noted so the next reader knows the trap has an exit and does not re-derive it.

**One process note that is ours.** Two judgement calls at the close were load
bearing, and both were refusals. The pre-commit hook has a sanctioned docs-only
fast path that would have let the prose through by splitting the commit; it was
refused, because that path exists on the premise that docs cannot change the
verdict, and here the verdict *was* about the docs. And a fail-fast gate run
reporting one failure had executed 111 of 1456 tests; re-running with
`--no-fail-fast` (2935 of 2936 passing) is what turned "something is broken"
into "exactly one thing is broken, and it is external." A partial run and a
complete one are different statements — the same lesson this campaign learned
from a queue run earlier, arriving a second time at its own close.

## Two epistemic lessons that are not about checks

**A null taken at eight points is a statement about the sampling.** A
seasonal signal was declared absent on eight readings that declined
monotonically across most of a year — which reads as clean evidence, and is
arithmetically impossible for a periodic function unless the minimum sits in
the unsampled tail. The cell's own reported annual mean contradicted all
eight readings, and that contradiction was the tell. A denser resample found
the minimum, and the mechanism was a roughly 23-day oscillation **aliasing**
against a 46-day sampling grid. Ask of any periodic measurement whether the
sampling interval shares a period with anything in the system.

**"Green" over a partial run and "green over everything" are different
statements.** The queue run immediately before this campaign's was also
green, fail-fast: 2,256 of 3,807 tests ran, one failed, 1,551 never
executed, and a whole phase never started. Read the executed count against
the total. This campaign's own stage gate reported **3,818 of 3,818**.

## Two operational notes

**An implementer ended its turn waiting on a background job.** Its children
die with its turn, so nothing was running and nothing would ever notify it.
The prohibition against backgrounding is already written down; the
prohibition alone does not hold, and the *recovery recipe* has to ship
beside it. Recovery here was to inspect the tree, find the uncommitted
correct work, and resume the agent with the remaining steps — rather than
killing it and restarting from a clean commit, which is safe but discards a
correct shape already found.

**Absorbing main at the close was load-bearing, not hygiene.** The branch
was 44 commits behind. The absorption merged cleanly, and it contained a
sibling campaign's chronicle entry appended at exactly the point this
campaign's own entry needed to go — a guaranteed conflict at the queue's
mouth, avoided for the cost of one merge. The general rule stands: absorb at
the boundary, and the cost of skipping is not conflicts you will notice, it
is conflicts someone else notices later.

## What was deliberately not fixed

Recorded so a future campaign does not mistake these for oversights. Each
has a registry row.

- **Three renderers now evaluate one projection through three different
  `cos`/`sin` implementations** — the portable crate, the platform libm, and
  V8 — two of them on the client side of the determinism boundary. Probably
  legitimate; nothing in the repo can currently answer it, because the
  cross-renderer agreement test runs on one platform. A rushed mitigation at
  the close of a campaign is the wrong instrument for a question that
  touches two ratified decisions.
- **The colour work is invisible where the game starts you**, and no
  committed artifact reaches the colour rendering. A real finding about
  settlement siting, not a defect, and the remedies belong to a campaign
  that can choose between them.
- **The ordinal claim is confirmed for one of the three renderers** the
  campaign preregistered. The narrowing is stated rather than assumed.
- **One of the two cross-renderer references became a mechanism; the other
  is still a hand-maintained discipline.** Converting both was out of scope
  on the highest-risk stage.
- **The dim-over-tint composition is exercised by no committed test**,
  because no committed chart carries a tinted cell at all — which is the
  same finding as the invisibility one, arriving from the coverage side.
