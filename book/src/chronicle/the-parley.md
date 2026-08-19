# The Parley

A parley is the moment two hostile parties stop and speak to each other. Every
transmission model this world has built walks parent to child down the founding
tree and nothing else, so an account has never been able to leave the people
that witnessed the event. The raid is the one moment the ledger already records
two peoples in the same place at the same time. This campaign makes that moment
an edge.

Three layers ship together, in dependency order, each with its own before/after
measurement so that none is confounded with another: a **clock**, so a community
that has already ended stops holding later news; the **stance geometry as two
co-equal arms**, so the asymmetry that decides whether guilt inherits can be
seen rather than assumed; and the **contact edge** itself. The readout is
preregistered, asserts only substrate controls, and reports every hypothesis —
including the two it falsified, which are the more interesting half.

## The dead were holding later news, and the damage was mostly indirect

The shipped model had no notion that a community can stop existing. Measured
before anything was frozen: **1,959 holders — 1.19%** — held a claim about an
event that postdated their own end, at hops one and two, some of them
250 to 500 years dead. The cause is a total function: the amplitude takes
`|founding(hearer) − founding(teller)|`, which is silent about ordering by
construction.

The rule is derived and has nothing to tune. A community may hold a claim about
an event at day `e` only if its own ending is absent or later than `e`, compared
exactly, matching the day discipline already in the witness rule. A step whose
hearer fails that test is refused, and refusing it also breaks the chain beyond
it.

That second clause is the whole prediction: the clock should remove **strictly
more** than the 1.19% it can see directly, because a refused step orphans
everything below it. Measured over forty worlds:

| | holders |
|---|---|
| no clock | 413,216 |
| clock applied | 383,277 |
| **removed** | **29,939 (7.25%)** |
| of which directly dead | 5,815 (1.41%) |

**5.15× the directly-dead share.** Five sixths of what the clock removes is not
a dead holder at all — it is a living line whose only teller was dead. The
change is strictly removing on the descent graph, which is what makes it
separately measurable from the edge below, which is strictly adding: the two
cannot cancel.

## The asymmetry nobody chose

A claim is damaged where teller and hearer stand differently toward the event.
The label for the attacking community, `Perpetrator`, is attached to the exact
entity the ending names — one community, no descendants. The label for the
destroyed community is closed under descent: the subject *or any of its
offspring*. So the step *attacker → attacker's own child* crosses a boundary and
costs a rung, while *victim → victim's own child* costs nothing.

| first retelling step | lossy |
|---|---|
| victim line (subject → child) | **124 of 3,056 (4.06%)** |
| raider line (attacker → child) | **3,694 of 3,694 (100.00%)** |

Both figures survived a wrong explanation. A first reading said the raider's
child is forced into the bystander label because it can never descend from the
subject — which is false wherever the attacker itself descends from the subject,
and the measurement says that happens: the raider's child lands on bystander
3,118 times, on the victim line 576 times, and on `Perpetrator` **zero**. The
correct statement is weaker and still sufficient: a child of the attacker can
never *be* the attacker, so some non-`Perpetrator` label is forced, and every
one of them differs from `Perpetrator`. Hence 3,694 of 3,694, for a reason that
is structural rather than incidental.

The victim line's residual 4.06% has a confirmed cause rather than an inferred
one. The number of victim children that **are** the named attacker is 124 —
exactly the 124 lossy victim steps. Those are foundings where a community's own
offshoot is what destroyed it.

**What that costs, measured on the consequence rather than the mechanism.** Over
548 cross-people endings holding on both sides:

| geometry | median rung, victim line | median rung, raider line | median gap |
|---|---|---|---|
| singleton (ships today) | 0.00 | 1.00 | **+1.0000** |
| inherited | 0.00 | 0.00 | **0.0000** |

Exactly one rung, and never more: the per-event gap is +1 on 542 of the 548 and
0 on the remaining 6, so the ceiling a predecessor measured — a retained claim
crosses stance at most once — binds here precisely. Closing the perpetrator
label under descent takes the gap to zero on every one of the 548.

Both geometries ship, both are reported, **neither is nominated and there is
deliberately no default.** The substrate for both was measured before either was
written down, so choosing a favourite afterwards would be selection on data
already in hand. The choice is not merely technical: the singleton says guilt
does not inherit and a raider's grandchildren are bystanders to their
grandparent's raid; the inherited arm says the deed stays the line's own. Both
are defensible readings of a world with no alignment axis, and today a
data-structure decision — one label being a set of one — makes that reading for
the world. A design that requires a moral valence be *derived* cannot accept
this provenance, which is why it is now measured and visible rather than
implicit.

## An edge along the raid, and what it does to path uniqueness

The transmission graph gains a **horizontal, undirected** edge for every ending
that names an attacking entity: between the victim and that attacker, stamped
with the ending's day. A claim about an event at day `e` may traverse an edge
stamped `c` when `e ≤ c` — a meeting cannot carry news of something that has not
happened — and when the receiving party passes the clock above. A crossing costs
the same as a descent step under the campaign's accumulation rule; charging it
extra would be a second free parameter added in the same breath as the edge, and
would make the whole readout unattributable.

Undirected is a freeze, not a discovery. The ledger records one event both
parties attended; asserting news flows only one way across it would be
authoring. Undirected is also the *ceiling*, so any directed variant is a
restriction measurable against these numbers later.

**Corrected by [The Undertow](the-undertow.md), which ran that restriction.**
Both directed arms were simulated on this graph and neither restores
disagreement: divergence falls under *every* arm on *every* rule, and under the
multiplicative rule the victim→raider arm collapses it further than the
undirected edge does. Pooling tracks the seam's volume, not its symmetry. The
freeze was the right call and it was not the cause; the follow-up this paragraph
holds open is closed rather than pending.

The edge makes the transmission graph **cyclic**, and every previous model
assumed it was not — each recovered *the* path by slicing an ancestry walk,
unique because the founding tree has a single parent per node. The rule survives
and the enumeration does not: a holder still keeps the least-corrupted telling,
but the walk becomes a best-first relaxation over the augmented graph, ordered
by the key already in use (smallest accumulated width, then fewest hops, then
smallest witness identifier).

**Termination needs both halves of that key, and saying "hops rise on every
edge" is not enough.** The key is compared lexicographically with width first,
so if width ever fell, the primary component would drop and the key would shrink
however many hops had accumulated. Width is non-decreasing under every
accumulation rule, which is what actually closes the argument. Measured on a
deliberately cyclic case: **5 relaxations** under the shipped rule against
**1,078** under a rule that halves the width, the latter halting only when a
float underflows to zero and the width component finally ties. That is
floating-point exhaustion, not termination.

**Termination is the argument this section makes at length; correctness got
an oracle instead, twice, and neither oracle survives.** The first ran the
old path enumeration and the new best-first relaxation side by side on the
real seed-42 substrate under the shipped policy — 1,755 (rule, event) pairs,
30,936 claims, 26,562 of them retold — and found **0 mismatches**. That is
the direct evidence that the rewrite left the descent arm's answers alone.
The second asks the harder question, because agreeing with the old walk only
shows the descent case is unchanged and says nothing about the cyclic one a
tree could never present: a brute-force enumerator over forty synthetic
tangled worlds checked that the relaxation returns the genuine minimum for
every holder — 3,936 cases across four clock × contact policies and three
accumulation rules, again **0 mismatches**. It carries its own positive
control, without which the zero would be unreadable: a worst-first mutant of
the frontier order, run through the same harness, produces 102 mismatches.
Both oracles were reporting instruments rather than committed tests, and the
second one — which pins a property no shipped test covers — should probably
have been kept.

**Corrected by [The Undertow](the-undertow.md): the relaxation minimises the
first component of that key and not the whole of it.** An exhaustive route
enumeration over the real substrate — 13,569,981 routes across 4,388 holders —
finds **36 of 13,164 contact holders (0.27%)** holding a telling with the same
accumulated width bits and the same remembered day as an available route, but
one hop more. The width minimum above is genuine; "the genuine minimum for every
holder" is not, because the key is lexicographic and the hop component is where
it fails. Neither oracle could have seen it: the first compared against the old
tree walk, which has no choices to make, and the second's synthetic worlds never
presented the degeneracy. The descent arm is clean at 0 of 310,215. The defect
is reproduced and deliberately not repaired — its cause is that the additive
rule's width telescopes and is therefore blind to hop count, which makes the
question a re-expansion-policy one rather than a typo.

The campaign's one real correctness risk gets its own test, and it is the one
the campaign is named for: an account that leaves its lineage, crosses to
another people, and comes home to a descendant of its own witness — by a route
no tree admits, arriving damaged. The tree route has to be *blocked* for that
test to mean anything, because a route home through contact always costs more
hops than the tree route and would otherwise lose on the key; a first version of
the fixture could not have detected a wrong route at all.

## Accounts that no tree can produce

| walk | endings | reach 2+ peoples | reach 3+ | most peoples | holders |
|---|---|---|---|---|---|
| descent — ships today | 23,594 | 548 (2.32%) | **0** | 2 | 413,216 |
| contact | 23,594 | 4,112 (17.43%) | **554 (2.35%)** | **6** | 1,413,515 |

The floor set in advance was 1% of endings reaching three or more peoples;
measured 2.35%. The distribution under contact runs 19,482 accounts reaching one
people, 3,558 reaching two, 443 three, 107 four, 2 five and 2 six. Under
descent, three peoples is reached on **exactly zero of 23,594** endings — not
rare, structurally impossible, which is the shape of an account that only ever
travels down a tree.

## A control that was falsified because its premise was false

The sharper half of that prediction was a single world. One of the forty has
**zero** endings whose attacker is of another people, and therefore zero
cross-people accounts under descent. The prediction was that under contact it
would carry more than zero — a world where the mechanism is absent at hop zero
but present as a graph being the cleanest available discriminator between
"contact works" and "contact re-describes co-witnessing".

It carries **zero**, and the prediction is recorded as falsified. The reason
travels with the number, because it is not a null about contact. A contact edge
joins a victim to its *own named attacker*, so an edge crosses a people boundary
**only where that attacker is foreign**. A world with no foreign attacker has no
cross-people contact edge, and no chain of edges can cross a boundary either. So
the graph is not present-where-hop-zero-is-absent: **the graph's edges *are* the
hop-zero seam.** The state the control assumed is one no world can be in.

The seam is demonstrably live on that world anyway — it widens the reach of its
accounts from 5,345 holders to 11,747, a factor of 2.20 — it simply has no
boundary to cross. The discriminator the control wanted would be a world *with*
a foreign attacker and no three-people account under descent; this was not one.
Nothing was edited to rescue it.

## Contact makes the two sides agree, and that is the campaign's surprise

The baseline first. A first instrument compared the two peoples' *sets* of
remembered days and found 111 of 138 cross-people endings unequal. Set
inequality is a weak question. The sharper one — does **each** side hold a day
the other holds nowhere? — gives a very different picture over the same 138:

| | events |
|---|---|
| mutually exclusive (genuine two-sided divergence) | **19 (13.8%)** |
| one-sided (one set strictly contains the other) | 92 (66.7%) |
| identical day sets | 27 (19.6%) |

Median holders per side is 4.0 on both, so the one-sided majority is not an
artifact of one line being larger. **The baseline for cross-people disagreement
is 19 events per twelve worlds, not 111** — a figure three campaigns had quoted
in its inflated form.

The prediction was that contact raises that count by more than 3×, because a
derivative account arrives already damaged rather than starting from an
eyewitness's finest rung. Measured over the 548-ending panel:

| accumulation rule | descent | contact | ratio | predicted |
|---|---|---|---|---|
| additive | 37 | 22 | 0.59× | > 3× |
| quadrature | 33 | 17 | 0.52× | > 3× |
| multiplicative | 69 | 53 | 0.77× | > 3× |

**Falsified on every rule, and in the opposite direction.** Where it went, over
the same population:

| rule | mutually exclusive | one-sided | identical |
|---|---|---|---|
| additive | 37 → 22 | 269 → 245 | 242 → **281** |
| quadrature | 33 → 17 | 237 → 220 | 278 → **311** |
| multiplicative | 69 → 53 | 365 → 371 | 114 → **124** |

Identical day sets rise on every rule. The mechanism is the edge's own symmetry:
**a seam is a channel in both directions**, so each side receives the other's
tellings and each keeps whichever it can reach least corrupted. The accounts
**pool rather than diverge**. The prediction assumed a damaged arrival would push
the two sides apart; it instead hands each side a route to the other's telling.

One reading is offered beside that and cannot discharge the prediction, because
it was not frozen in advance. The frozen measure compares the victim's people
against the raider's people **and no other pair** — and contact's whole effect
is to create *new* pairs of peoples that share an account, every one of which
that measure is blind to by definition. Over the endings that reach two or more
peoples, the share carrying *some* mutually-exclusive pair rises:

| rule | descent | contact | rate change |
|---|---|---|---|
| additive | 37/548 = 6.75% | 720/4,112 = 17.51% | **2.59×** |
| quadrature | 33/548 = 6.02% | 1,105/4,112 = 26.87% | **4.46×** |
| multiplicative | 69/548 = 12.59% | 1,072/4,112 = 26.07% | **2.07×** |

**2.1× to 4.5×, quoted as a rate and never as a count.** The eligible population
grows 7.50× by construction under contact — 548 endings to 4,112 — because
contact only ever adds holders, so a bare ratio of counts would be mostly the
denominator moving, and would overstate the effect roughly fourfold. The frozen
verdict is unaffected either way: its own population is pinned at 548 on both
arms and all three rules.

## The null was named in advance, and it fires on one rule of three

The live alternative, written down before any of this was measured: contact
chains are longer and damage only accumulates, so everything saturates to the
coarsest rung, accounts that are all equally vague become identical again, and
divergence falls as reach rises.

| rule | saturated, descent | saturated, contact |
|---|---|---|
| additive | 0.00% | 0.00% |
| quadrature | 0.00% | 0.00% |
| multiplicative | 21.26% | **30.46%** |

Under the multiplicative rule saturation rises while divergence falls, which is
exactly the named null. Under the other two, saturation is zero on **both** arms
and divergence still falls — so the null cannot explain the inversion there, and
the pooling mechanism is what is left. That is what a null detector is for: it
rules itself out on two rules of three.

Additive and quadrature never saturating is a predecessor's frozen unit
erratum reproducing unchanged — a width seeded in standard days and incremented
by a dimensionless count of generations, so those two rules stay near the day
rung and cannot reach a people's coarsest ones. It is deliberately left
untouched here; repairing it inside the campaign that adds the edge would make
neither measurable.

## What this leaves

The descent arm is byte-identical to what shipped before, which is what licenses
every comparison above.

The contact edge was the last named precondition for **corroboration** — agreement
between accounts that could have differed — and supplying it produced the
opposite of what corroboration would need. On the frozen measure, contact is
*homogenising*: two peoples joined by a seam converge on the same telling of an
event rather than drifting apart. Whether that survives a directed edge is now
the sharpest open question the model has, and it is cheap to ask, because
victim→raider and raider→victim are both restrictions of the undirected ceiling
measured here and need nothing re-derived.

**Corrected by [The Undertow](the-undertow.md): it was cheap, it was asked, and
it was the wrong question.** Pooling survives both directed restrictions, so the
undirected freeze is exonerated rather than implicated. More usefully, the axis
itself turns out to be the minority case — **about 70% of cross-people holders
cross the seam more than once**, out to depths of 13, 19 and 9 hops, and only
about 4% are the hop-zero co-witness line. Victim-versus-raider is a story about
a single crossing. The successor's own subject moved twice for this reason: it
also exonerated the *selection rule*, and found that under descent all 103,405
holders receive exactly one telling — so half of the ratio above was never at
stake for any ordering rule to move.

Two freezes held while the edge varied and are now load-bearing in a way they
were not before: the amplitude reads the *teller's* generation length, and the
rung a claim is reported at is resolved against the *originating witness's*
ladder, fixed once per path. Both were free of consequence while transmission
never left a people. It leaves now. Whose ladder resolves a claim once it has
crossed — the witness who first saw the event, or whoever is holding it — is a
question the world can finally distinguish, and this campaign deliberately did
not answer it.
