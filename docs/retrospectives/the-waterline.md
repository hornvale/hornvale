# The Waterline — retrospective

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-waterline.md).

This campaign shipped a guard and two findings, and discarded its own headline
feature. Most of what it cost was avoidable, and the avoidable parts have a
common shape.

## The headline: I measured with an assumed constant while the real one was published

The probe classified ocean as `elevation < 0.0`. Sea level on seed 42 is
**−2,936 m**, and `terrain.is_ocean` — `elevation < globe.sea_level` — had
existed all along, in the same file I read to write the gate.

So I used the codebase's real predicate in the **fix** and my own assumption in
the **measurement**. That is exactly backwards: the measurement is what
everything else is built on, and it is the place where an assumption compounds.
Four rounds of conclusions went to the owner before anything forced the two
into contact — and what finally forced it was implementing the fix and noticing
the numbers did not reconcile.

**The rule:** when a probe needs a predicate the domain already exposes, use
the domain's. If you find yourself writing `< 0.0` for a threshold, ask what
the codebase calls that threshold. The probe now keeps an explicit assertion
that the two tests disagree on 8,162 cells, so the trap cannot return quietly.

The related lesson is about *how* it was caught. Not by review, not by a test —
by building the thing and finding two numbers that could not both be true.
**A measurement nobody has cross-checked against a second derivation is a
hypothesis.**

## The diagnosis shrank twice, and the campaign's justification did not survive

- First draft: two independent defects; 78% of prey production oceanic;
  therefore a **prerequisite** for the prey field.
- After the sea-level correction: **one** defect; 92% of prey production is
  terrestrial; therefore **not** a prerequisite at all.
- After The Tumult merged: the defect was already fixed, better.

Only the third of those was outside my control. The first two were mine, and
both inflated the campaign's importance in the direction that made it worth
doing. That is the direction motivated error always runs, which is a reason to
check hardest exactly when a finding makes your work look necessary.

## Parallel campaigns: `make preflight` cannot see a semantic collision

The Tumult fixed the same defect while this was in flight. `make preflight`
reported no decision, chronicle, retrospective, study, or registry-row
collisions — correctly, because there were none. Two campaigns independently
diagnosing and fixing the same defect is invisible to every mechanical check
the preflight runs, and it surfaced only when the merged tree reddened a test.

The absorption cadence would have caught it earlier. CLAUDE.md requires
absorbing main at every plan-stage boundary; this branch absorbed twice, both
times late, and by the second absorption main had moved 78 commits and had
already shipped the fix. **The cadence is not bureaucracy — it is the only
mechanism that surfaces a semantic collision before it becomes wasted work.**

I do not think the *design* discussion was wasted: The Tumult's supply-frame
argument is better than my medium-axis one, and comparing them is what made
that clear. But the implementation, its tests, its census run, and a review
pass all went into work that was deleted.

## When you are superseded, argue the design, not the ownership

The removal decision was made on the merits and it is worth recording *which*
merits, because "they got there first" was the weakest available argument and
would have been the easiest to reach for.

The strong arguments: `ResourceVector` is already a Hutchinsonian
resource-hypervolume niche, so habitat is its shadow and a declared medium is a
Grinnellian concept bolted on top; two mechanisms can *contradict* (an aquatic
diet with a terrestrial medium is expressible and incoherent, where
supply-masking makes that unwritable); and promoting a derived attribute to a
primitive is precisely the error the ECS program exists to undo.

An ideonomy pass produced all three, on a question I had already decided by
instinct in the other direction. **The pass earns most when you think you know
the answer.**

## A census run spent on a state that no longer exists

I regenerated the census — correctly, on the canonical box, verified
column-wise — for a change that was deleted three hours later. The merge then
conflicted on exactly those goldens, because main had regenerated them too.

The sequencing rule this suggests: **regenerate the census after the final
absorption, not before.** It is the most expensive artifact in the project and
the one most likely to be invalidated by anything that lands meanwhile.

## Two mechanical near-misses worth keeping

**`git add -A` swept an uncommitted feature change into a docs commit.** Caught
by reading `git show --stat` before moving on, and split back out. The habit
that saved it was checking what a commit actually contained rather than what I
intended it to contain.

**The census nearly ran on the wrong machine**, and the guard that now prevents
it is this campaign's main deliverable. The near-miss is instructive in itself:
the documentation was not wrong, it was *indexical*. "This box is the single
canonical platform" is true when read on the canonical box and false everywhere
else, and every copy of that sentence had been written there. Prose that
depends on where it is read cannot enforce anything. See decision 0079.

## What the reviewer caught that the process did not

The whole-branch review found one Critical and six Important issues, including
two tests that **passed whether or not the code was correct** — one of which I
had specified in the plan. Both were mutation-verified by the reviewer, not
argued about.

This is the second campaign running where the final whole-branch review found
the most important defect, and both times it was a test asserting nothing. The
per-task reviews did not find them because a test that passes looks identical
to a test that works, unless someone deliberately breaks the code underneath
it. **Mutation-verify any test whose failure would be the only thing standing
between a wrong value and a green suite.**
