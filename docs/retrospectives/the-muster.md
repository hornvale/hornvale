# Retrospective — The Muster

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-muster.md): a registry row refuted
by its own instrument, a commit-gate guard that could not see the quantity it
named, a falsified prediction and a null shipped as headlines, and a second half
given away to the campaign that was already paying for its epoch.

## The count: six defects, every one in controller-authored text

Two implementer tasks, four review passes, two fix rounds, one absorption, one
re-verification. **Six defects were found in text the controller wrote** — the
spec, the plan, and the two sweep briefs:

1. **The spec cited the wrong guard**, and the correction was scope-defining.
   It named a descent-graph test as the pin on the founder handle. That test
   exercises a *second, different* founder key. The world carries two founder
   identities derived from two keys, which the spec did not know and which
   decided the blast radius of the whole second half. Found by the plan-writer
   reading the tree instead of trusting the spec.
2. **The spec named one empty store where two were empty.** `biome_affinity`
   was named exactly; `habitat_realm` sits three lines above it on the same
   traced path and was not mentioned at all. The repair had to handle both, and
   the decomposition that followed is what showed a third of the headline effect
   was interaction between them.
3. **The roster-flip prediction was falsified twice over** — numerically (the
   biosphere arm reads 1.5063, not the asserted 1.42–1.46) and structurally
   (the band's ceiling is derived from the peopled count, identical in both
   arms, so both rosters derive the same band and only the floor could separate
   them). The spec's table did not reproduce on this tree, and the instrument
   as built could not have produced the flip.
4. **The first sweep's brief substituted along the wrong axis.** See below; it
   is the campaign's whole shape.
5. **"Only within-kind ranking is level-invariant"** was written into the plan's
   own step text as the sentence to record, and it is false. It commits the
   exact error the paragraph exists to name.
6. **A branch-table row prescribed the wrong reading.** The plan offered
   "level-plus-reach reddens the guard" as the qualified-confirmation branch.
   The common-shape arms falsify it: reach is necessary and nowhere near
   sufficient. The implementer took the override and said so.

The distribution is the same as the last three campaigns' and the diagnosis has
not changed: a controller writes prose that *asserts things* and dispatches it
to agents who execute against it, so a controller's mistake meets nothing until
somebody checks the premise. What is different here is the tail. This campaign
also found defects in **implementer-authored** prose — a recorded control that
did not reproduce from its own recipe, a three-point span quoted across a
non-monotone response, the word "distinct" describing an assignment with two
collisions in it — which the previous campaign did not. Doc comments recording
a measurement are now a defect surface of their own, and they are not covered by
any gate.

## 1. A campaign can be right about a problem and wrong about its prescription

The registry row that started this campaign was **correct about the problem**:
one number serves consumers with materially different sensitivities, and the
justification the project had attached to it was true of one consumer and silent
about the rest. It was **wrong about the prescription**: those consumers do not
want different values, and a split would have shipped two constants where the
evidence supports one.

The distance between those two statements is the whole campaign. The row had
been sitting in the registry asserting a redesign, and the redesign would have
been executed on its authority — it is a `raw` row's job to be executed
eventually — had nobody asked it for evidence first.

**The instrument that settled it was cheaper than the redesign it prevented.**
Two throwaway sweeps, run in a scratch branch, no committed code, roughly a
minute of wall clock per full run. Against that: a split constant, a save-format
epoch, every affinity row re-authored, and a chronicle explaining a distinction
that does not exist. The asymmetry is not close, and it is the argument for
running the instrument on *any* registry row whose status is `raw` and whose
prescription is expensive.

Corollary worth stating on its own: **a preregistered rule earns its keep when
it comes back and forbids what you were going to do.** The rule was written so
that YES/LEAVE was a reachable outcome, and it fired. That is a win for
preregistration, not wasted work, and it should be recorded as one so the next
campaign is not tempted to write a rule that can only endorse.

## 2. The controller's own instrument was confounded, and an ideonomy pass caught it before it reached a spec

This is the finding worth the most.

The first sweep's brief said *"sweep λ with the shape held fixed"*. It never
said **preserve per-kind ordering**, so "the level" was reasonably read as one
number — and the arm that got built collapsed the shipped per-kind spread to a
single scalar. The hypothesis under test was a substitution along the
**consumer** axis; the arm substituted along the **kind** axis. It could not
discriminate the hypothesis, and it returned SPLIT on a 0.0067 hairline through
a single kind.

**That verdict was one honest agent report away from becoming the campaign's
founding premise.** Nothing was wrong with the execution. The numbers were
right, the bands were scored correctly, the report was accurate. The defect was
one clause absent from a brief, and no gate, review or test in this project can
see an absent clause.

What caught it was an ideonomy pass run before the spec was drafted, and the
specific prompt that did the work is worth naming: **hierarchicalness**. Laying
the candidate parameterisations out as rungs — flat-global, flat-per-kind,
scaled-per-kind, two-scalars, full matrix — made it immediately visible that the
sweep and the hypothesis were sitting on different rungs. The same pass produced
the campaign's second half independently, through the cycle organon: the
authored-constant lifecycle has an author phase, a copy phase and a
load-bearing phase, and **no test phase**, which is the blind guard.

Two operational rules follow.

- **Run the expansion pass on the instrument, not only on the design.** The
  routine use of ideonomy here is generating alternatives to a *plan*. Its
  highest-value use in this campaign was generating alternatives to a
  *measurement arm*, before the measurement was trusted.
- **Amend the arm, never the rule, and prove which you did.** The amendment
  passed the project's own correction-versus-rescue test by construction: the
  new arm **adds** two constraints (must reproduce shipped byte-identically at
  λ = 1; preserves per-kind ordering) and removes none, with the decision rule,
  the bands, the seed panel and the frozen competition temperature all
  unchanged verbatim. Write the amendment down, dated, *before* the second run,
  and state the test it passes — otherwise an amendment made after seeing
  results is indistinguishable from metric-chasing, because in form it is
  identical to it.

## 3. "Do not fix the deferred Minors" is hard to honour, and both implementers hit it

Four Minor findings were deliberately deferred rather than repaired. **Both
implementers nearly closed some of them by accident**, because the required
rewrite of an adjacent Important ran straight through their lines. Both caught
themselves, reverted, and flagged it; the re-verification pass later checked all
four in the diff rather than from memory and confirmed they survive verbatim.

The lesson is not "be careful". It is that **a deferral is a claim about a
region of a file, and a rewrite is a claim about a region of a file, and nothing
tells you when the two overlap.** The mitigation that worked was mechanical: the
implementer verified its own change was doc-only by counting changed lines
(184 of 184 changed Rust lines were `///` or `//!`), and the reviewer re-read
the deferred sentences in the final text rather than in the diff hunks. Both
halves were needed — a diff shows what moved, not what was supposed to stay
still.

A second, gentler form of the same hazard: when appending to a paragraph that
contains a deferred Minor, **append after the sentence, never into it**. That
was done deliberately at the re-verification and it is the reason the ambiguity
about which average a percentage refers to survived intact for a future
campaign to close.

## 4. A reviewer's finding can have a false premise, and the adjudication is the best work

The strongest single piece of work in the campaign went **against** a reviewer,
and the shape is worth keeping.

A review raised, as an Important, that a recorded arm had been run at negative
levels — outside the constructor's documented contract, undisclosed. The
implementer's adjudication showed the premise was false, and it did it by
finding the discriminator rather than by arguing. On the arm in question,
clamped and unclamped are **bit-identical**: a row whose only rung is preference
1.0 has factor 1.0 at any level, and the packer filters a non-positive capacity
as absence, so zero and negative are the same input. Reproducing that arm
therefore proved nothing at all about which implementation produced it. The
discriminating arm is a different one, where the two do diverge — and the figure
nobody could place turned out to be the clamped value there.

The transferable rule: **a value recorded before anyone raised a question is
positive evidence about which code produced it.** The reviewer used a number
recorded *before* the clamp was ever discussed as evidence that the clamp was
absent; that number could not have been chosen to support either answer, and it
supports the opposite one.

And the general posture: a review finding is a hypothesis with a premise
attached. Check the premise before implementing the fix. Two rounds of this
campaign's fix work were spent well precisely because the implementer refused a
finding it could show was wrong, and spent the effort on the two it could not.

## 5. Holding half a campaign was the right call, and it needs saying out loud

The second half was held rather than built, because a parallel campaign was
retyping the exact fields it would key on. Building first would have renamed
every founder in every world, and then the other campaign would have renamed
them all again — two identity epochs back to back, the second silently
invalidating the first's regeneration.

The parallel campaign then **shipped the second half itself**, riding an epoch
it was already paying for. That is the outcome the hold was reasoning toward,
reached by a route the hold did not predict.

Three process notes fall out of it.

- **The correct response to a scope transfer is to mark it, not to delete it.**
  The plan's superseded tasks stay in place, marked, pointing at the campaign
  that shipped them. A deleted task leaves no record that the work was
  considered, sequenced and re-homed; a marked one is the archaeology.
- **A held half's diagnosis can still be load-bearing.** This campaign's
  scoring predicted that no key would clear the twin-parent case. The shipping
  campaign's residual is exactly that case, and the prediction is what let it
  be recognised as a design property rather than a bug to chase.
- **Re-verify a held campaign's numbers, do not restate them.** Part A's
  figures live in doc comments, not assertions, and a doc comment cannot go
  red — the guard passing after the absorption said nothing about whether the
  tables it documents still reproduce. An independent probe rebuilt every arm
  from scratch and every figure held bit-identically. **Ask for the structural
  reason as well as the result**: knowing *which future change would* move
  those tables (terrain, climate, the affinity or realm stores, or the packer)
  is what makes the next absorption cheap to reason about.

## 6. Small notes

- **A campaign that correctly declines an expensive canonical run should say
  how it knew.** This one owes no census refresh: its only production-crate
  change carries zero non-comment changed lines, the other file is a test, and
  the seven-path drift check is clean after a full regeneration. Writing that
  down with its evidence is what distinguishes a considered decline from an
  omission, and the next reader cannot tell the difference otherwise.
- **A blind guard and a wrong prescription came from the same reading pass.**
  Both halves of this campaign were found *while measuring something else* —
  the sweep noticed the empty component store on its way past. Budget the
  incidental findings of a measurement pass; they were worth more here than the
  measurement's own verdict.
- **The scratch ledger dies with its checkout.** Everything above was promoted
  from an ignored directory, along with the preregistration, its amendment and
  both result sets. The result sets in particular lived in a temporary
  directory outside the repository, which is one cleanup away from gone. Promote
  a freeze the day it is written, not at the close.
