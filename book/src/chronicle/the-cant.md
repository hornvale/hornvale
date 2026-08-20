# The Cant

*Myth, campaign 8.* The first campaign of the **evaluative-beliefs**
program: can a believable, relational *snap-judgment predisposition* between
peoples be **derived** from what each people materially is, rather than
authored?

## The lean before the cant

A cant is a regime's self-justifying moral talk — but before a regime can
cant, a creature must already *lean*. [The Cupel](the-cupel.md) closed on the
finding that the myth engine's transmitted content is behaviourally decorative
because belief is coarse-bucketed **and only ever factual**: a remembered date
can be garbled, but two peoples cannot hold it in genuine conflict. The
evaluative-beliefs program attacks the second half — *contestable claims with
no true value*, a people admirable or contemptible, a neighbour to be feared,
envied, pitied. The Cant delivers only its first layer: the **baseline
predisposition**, a discovered true distance, world-invariant, before any
perturbation or feedback makes worlds diverge.

Decision 0021 forbids the engine an *authored* preference between peoples. The
Cant's whole architecture is a single equation obeying that prohibition:

```
  v(A -> B) = Σ over axes of [ w_A(axis) · distance(A, B, axis) ]
```

Eight axes, each a normalized attribute distance in `[0,1]` derived from the
authored species catalogue — habitat, diet and predation, condition-niche,
sociality, activity-cycle, reproductive tempo, speech articulation, and
size-threat. The distances are symmetric strangeness except for the two
directional ones (predation and size), where *I hunt your kind* differs from
*you hunt mine*. The only thing that **ranks** is `w_A`, the judging people's
own weight-vector — an insular people (small in-group radius) weights every
axis up; a stand-your-ground people weights the threat axes; a rigidly ranked
people weights social disorder. No constant anywhere encodes a preference
between two *named* peoples; a valuation is always *someone's*. The weighted
distances are projected onto a **warmth × competence** plane — the Stereotype
Content Model — and the quadrant names an emotion: admiration, envy, pity,
contempt.

Because every axis reads an authored model card and no axis reads a built
world, the derivation needs no worldgen: it is a pure function over the
fifteen peoples' catalogue rows, and its readout is fast, not a census.

## The measurement

The believability of a derived prejudice is the one thing no assertion can
settle, so the deliverable is a **readout**, not a pass/fail. Following the
Cupel discipline, the criteria were frozen as *structural* floors — never
"human must dislike drow", which would smuggle the authored ranking back in —
and the specific pairs were **reported** for a human to judge, not asserted.
Five floors: the landscape is non-degenerate; it produces asymmetry; similar
peoples land warm; peoples have distinct personalities; and — the one that
would matter most — **it likes as well as loathes**, at least one pair reaching
admiration.

Over the 210 cross-people judgments among the fifteen peoples, four floors
held and one broke:

| floor | result |
|---|---|
| non-degenerate | three distinct emotions appear |
| asymmetry | all 105 unordered pairs judge differently in each direction |
| similarity → warmth | Pearson `r(distance, warmth) = -0.896` |
| distinct personalities | the fifteen weight-vectors are not all equal |
| **it likes** | **falsified — 0 of 210 pairs reach admiration** |

The emotion landscape is 124 contempt, 80 envy, 6 pity, and **zero
admiration**. The system fears, envies, and disdains; with the derivable axes
alone it never *likes*. The closest it comes is two desert peoples —
desert-elf regarding desert-dwarf — landing at warmth 0.452, missing the
admiration boundary by 0.048.

## Why nothing is admired

The null is not an accident of tuning; it follows from the model's shape, and
the shape is a faithful reading of the spec. Every axis's signature on the
plane pushes warmth **down** from its maximum — strangeness on any axis is a
reason for coldness, and no axis is a reason for warmth. Warmth therefore
starts full and can only erode; a pair reaches the warm half of the plane only
when it is close on *every* warmth-lowering axis at once, and no two of the
fifteen are. Admiration additionally needs high competence, which the pair
does clear — the desert pair sits at competence 0.81 — so the failure is
entirely on the warmth channel. There is no *positive-warmth channel* in the
derivable axes: nothing that says *this creature is beautiful, or healthy, or
kin, and I warm to it on sight*.

This was reported, not rescued. No weight, baseline, threshold, or axis
signature was retuned after the null appeared; the falsified floor was left as
a printed measurement while the other four remained hard assertions, and the
diagonal — a people judging itself, which is admiration by construction — was
excluded so the finding could not be trivially satisfied.

## What it means, and the two readings it admits

The null carries two explanations, and they are not exclusive.

The first is about the **substrate**. The vivid cues of real prejudice —
appearance, the disgust of the unclean, the pull of the familiar — are exactly
the axes the campaign deferred for want of authored data. A world with only
*off-putting* traits to notice will only ever produce coldness. The lever is a
future one: build the positive-warmth substrate (appearance, a behavioural
immune / purity axis) that gives a creature something to warm to.

The second is about **calibration**, and its evidence is the 0.048. The warmest
pair sits a hair under an author-chosen line, and the warmth baseline and the
classification threshold were free parameters — frozen *before* the
measurement, so the discipline held, but not derived from anything. A model
that recentres the neutral point, or adds a small positive term, could put the
desert pair over the line without touching the axis substrate at all.

The deeper constraint the null surfaces is a design one, and it reframes the
whole program's target. A world whose peoples are *universal xenophobes* — where
an elven merchant sneers at a human customer and commerce, travel, and cultural
exchange simply do not function — is not the goal and is a worse simulation than
one where views are held, are often wrong, and yet leave room to trade and
travel. The aim is not neutrality and not maximal prejudice but *situated,
survivable error*. So the program's later layers are charged not merely with
transmitting prejudice but with keeping the world livable while they do: a
perturbation whose distribution recentres on the neutral point rather than the
negative; a **baseline consideration** sapient creatures extend to other
sapient creatures — the way traditions long in conflict may still regard one
another as people of the book; concentric circles of in-group, weighted so that
a stranger is not, by default, an enemy.

The Cant measured a null and, as the Cupel did before it, found in measuring it
exactly where the levers are: not in the equation, which is correctly shaped and
correctly derived, but in the poverty of what the derivable axes can be *warm
about*, and in a neutral point set where warmth had nowhere to go but down.
