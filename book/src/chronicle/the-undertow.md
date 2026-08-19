# The Undertow

An undertow is a current running the other way beneath the surface one. This
campaign went looking for one: a seam between two peoples that is a channel in
both directions on its face, and might carry something asymmetric underneath.

It found the asymmetry, gave it a magnitude derived from the world's own
history, and then measured that the aggregate the whole thread has been
reporting cannot see it. That is the campaign's result, and it is three
statements rather than one.

Two other things happened first. **The subject of this campaign was falsified
twice, by probes, before a line of its specification existed** — both times the
falsified thing was a premise its own controller had written down and
recommended. Those eliminations are the campaign's inheritance and they are why
the design is what it is.

## Symmetry is not what pooled the accounts

The predecessor added a horizontal edge along every raid and found the opposite
of what it predicted: two peoples joined by a seam **agree more**, not less.
Two-sided divergence fell to 0.59× / 0.52× / 0.77× of the descent baseline on
the three accumulation rules, and identical remembered-day sets rose on all
three. That chapter closed by naming the obvious suspect. The edge had been
frozen **undirected**, because asserting a direction is authoring; a seam that
carries in both directions is by construction a homogenising device, so perhaps
the pooling was the freeze's doing rather than the world's.

The test is cheap, because victim→raider and raider→victim are both
*restrictions* of the undirected ceiling and need nothing re-derived. Simulated
on the shipped graph over twelve worlds:

| arm | additive | quadrature | multiplicative |
|---|---|---|---|
| both (undirected) | 0.60× | 0.58× | 0.84× |
| victim→raider only | 0.70× | 0.67× | **0.68×** |
| raider→victim only | 0.80× | 0.75× | 0.84× |

**Divergence falls under every arm on every rule**, and under the multiplicative
rule the one-way arm collapses it *further* than the undirected edge does.
Restricting the channel does not restore disagreement. Pooling tracks the seam's
**volume**, not its symmetry.

**And the axis was the wrong axis anyway**, which is the more useful half. The
victim-versus-raider story is a story about a *single* crossing, and single
crossings are the minority case: **~70% of cross-people holders cross the seam
more than once**, with crossing depths running out to 13, 19 and 9 *crossings*
— the instrument counts boundary crossings on the winning path, not hops; only
about 4% are the hop-zero co-witness line; and among the holders that do cross
exactly once, the split between the two directions is near-balanced
(41.6/58.4, 37.5/62.5, 42.4/57.6). A campaign frozen on direction would have
measured a real number against an axis the substrate does not sit on.

## Nor is the tie-break — and half the ratio was never at stake

The second candidate was sharper, and it was the one the campaign expected to
carry it. The shipped walk keeps, for every community, the *least-corrupted*
account it can reach, ordered by accumulated width and then by hop count. That
ordering does not consult who is speaking. We had built perfect cosmopolitans:
hand a community a better-sourced account from the people who burned its
village last spring and it adopts it over its own grandmother's.

If pooling is an artifact of that rule, swapping the rule should move it. Four
people-blind alternatives — primacy, frequency, frequency-weighted, recency —
were run against the shipped least-damage rule over the same worlds. Of the
**8** cells where the baseline pools and an alternative could disagree, **0**
did. The largest gap anywhere was **2 events of 124**.

**A structural fact stands behind that table and is stronger than it.** The
ratio's own denominator is the descent arm, and under descent all **103,405**
holders receive exactly **one** telling. The founding tree is a forest, witnesses
are never re-entered, and there is therefore no choice to make anywhere in it.
**Half of the ratio a selection rule was supposed to move was never at stake.**
No ordering rule could have touched it.

That also closes the elegant repair before it was proposed. The one people-blind
rule that would produce ingroup preference as an *output* — count how many
concordant tellings reached you, since your own line supplies more of them — is
what `frequency` and `frequency-weighted` already measure. They pooled.

## What the probes did find, and it is this campaign's chief hazard

Under contact, **51.3%** of holders carry two or more distinct remembered days,
and the five selection rules disagree about which one is held at up to **45.7%**
of holders. Swapping the rule rewrites nearly half the world's held beliefs and
moves the divergence aggregate by **≤2 events of 124**.

That is not a null. It is a **dissociation**: the frozen measure is nearly blind
to a change that touches half the population. It was written into the
preregistration as a named live alternative, because it is the most plausible way
a campaign of this shape produces a real number that means nothing.

It is what happened.

## A seam costs what the peoples across it are strangers

The mechanism that shipped is one line of arithmetic. A transmission step from
teller to hearer whose peoples differ adds, to the accumulating width, a penalty
on top of the step's ordinary generational damage:

```
crossing_penalty(a, b) = span(FINEST) / (1 + contact_edges(a, b))
```

`contact_edges(a, b)` counts the raid edges in the contact graph running between
any occupation of people `a` and any of people `b`; `span(FINEST)` is the width
the accumulator already seeds itself with, so the penalty is commensurate with
what it joins and introduces no new scale. Read it as: **a crossing costs one
finest rung, discounted by how well the two peoples know each other.** A
same-people step pays nothing, which is what makes ingroup preference an
**output** rather than a rule.

The tally is derived once, at walk construction — one pass over the contact
edges reading each endpoint's people into a canonically-ordered pair key — rather
than re-scanned at every crossing, which is the read-path pattern a predecessor
campaign had already deleted one level down.

### Why a derived magnitude and not a constant

This is the campaign's one judgement call and it is worth stating carefully,
because the project's standing constraint forbids any ideology that *ranks*
peoples from ever being an input; such a thing may only ever be a generated
output.

An earlier reading of that constraint — this campaign's own — took it to forbid
any people-aware rule at all. That is too strong. A community preferring its own
line's account is a preference over **sources**, not over **worth**, and nothing
in it ranks either people. What the constraint requires is that the *strength* of
the preference be manufactured from material conditions rather than declared, and
`contact_edges` is read from the same ledger facts the seam itself is built from.
Two peoples who have raided each other for centuries know each other's accounts;
two who met once do not.

The line that must not be crossed, stated so a later campaign can check it: **no
constant may encode a preference between two peoples.** A magnitude that cannot
be traced to a ledger fact is authored.

That reading was ratified, and the ratification sharpened the constraint's intent
past what its text states. The thing to avoid is **the game** assuming kobolds
are stupid and evil and elves good and pure; the player and the creatures are
*expected* to hold very strong opinions on these questions. **So the target was
never an absence of prejudice. It is a question of whose.** A world where nobody
holds a view about anybody is not the goal and is a worse simulation than one
where views are held and are wrong. What is forbidden is the *engine* holding
the view — a species carrying a valence, a lookup table deciding conduct. What
this thread builds toward is the opposite: creatures with strong, situated,
mistaken opinions about each other, arrived at because of how their accounts of a
shared history actually travelled.

### The worked reading of that formula was wrong, and the correction is measured

The design's own gloss said "peoples with a single recorded contact pay the full
rung". **They cannot.** The graph builds its peer list and its people-pair tally
in the same pass over the same record, so any edge that makes two occupations
peers has already incremented its own pair's count. The denominator is therefore
at least 2 at every reachable crossing, and **the ceiling is half a finest rung,
not a whole one**. Measured over **67,765** winning-path crossings: a zero
denominator occurred **0 times**, realized penalties ran from one twenty-sixth of
a finest rung to exactly one half, and every crossing was carried by a seam edge
— **zero by descent**. The full rung remains reachable only by a descent step
across a people boundary, which is open in the code and never walked by the bake:
a measurement, not an invariant.

The probe was commissioned expecting to find the mechanism **inert**, and
intending to correct the numerator. Two of the commissioning inputs were wrong.

- **The rung gap is not the 41.7× the controller quoted.** The finest rung is a
  world's *day* — the next one up is its first moon, or its year on the one
  moonless seed — and the step between them ranges
  **2.23× to 530.85×, median 12.56×** across the panel's 180 ladders. One seed
  steps 1.5507 d → 3.4606 d, where a half-rung penalty is **40.6% of the gap**.
  Another is moonless and steps 1.0010 d → 531.39 d, where the same penalty is
  0.094%. Same twelve worlds; four orders of magnitude between them; one seed's
  number had been quoted as the world's.
- **Width is cumulative**, so "one crossing against one gap" is the wrong
  comparison entirely. What matters is how many holders sit within one penalty of
  a rung boundary after a whole path — a density, small and non-zero.

**477 holder-rungs move** between the two arms (additive 50, quadrature 10,
multiplicative 417) against 58,618 holder-widths. A re-walk at multipliers 1× to
256× finds the mechanism already reaching the ladder at **k = 1** on every rule,
so no correction is warranted; it also shows the width-moved column is *constant*
in k — **a bigger penalty buys resolution, never reach** — and that the response
saturates, 256× buying only about 20× the rungs under the rule most sensitive to
it.

That constancy has a countable cause, and it is worth stating because it is the
kind of ceiling a magnitude argument cannot argue past: **7,066** of the
multiplicative rule's 13,410 width-moved holders already sit at their ladder's
**coarsest** rung, where no penalty of any size can push them further, there
being no coarser rung to reach. (Under additive and quadrature that count is
zero — their accumulated widths never run that far up the ladder.) A penalty is
a width, and a width past the top of a ladder is a width the ladder cannot
report.

**The analytic estimate would have said the opposite.** Median headroom by
calculation is 71× to 32,129×, which reads as *inert*. The 477 are the **tail**,
not the median, and a ratio of medians cannot see a tail. The probe re-walked the
panel instead of dividing two numbers, which is the only reason the formula was
kept rather than rescaled.

## The readout

Preregistered before the mechanism existed; every hypothesis reported, only
substrate controls asserted. Forty worlds, 2.84 s each.

### The penalty does not break pooling

The prediction was that the mutually-exclusive count under contact would rise
above its descent count on at least one rule — that the 0.59×/0.52×/0.77× ratio
would clear 1.0. Over the 548 cross-people endings held on both sides:

| rule | descent | contact | ratio | ratio before the penalty |
|---|---|---|---|---|
| additive | 37/548 | 22/548 | **0.59×** | 0.59× |
| quadrature | 33/548 | 17/548 | **0.52×** | 0.52× |
| multiplicative | 69/548 | 53/548 | **0.77×** | 0.77× |

**Falsified, and unmoved to two decimal places.** The penalty moves the
mutually-exclusive count by *exactly zero* on every rule — 22→22, 17→17, 53→53 —
so the ratio is bit-for-bit the pre-campaign one.

It is not a dead counter. The composition *inside* the unchanged total does move:
one-sided containment goes 245→247 under the additive rule and 371→374 under the
multiplicative, with identical day sets moving the other way by the same amounts.
Accounts are being reshuffled between "one side's set contains the other's" and
"the two agree" without ever reaching mutual exclusion.

The eligible population is 548 on both arms and all six cells, and it equals the
foreign-ending count exactly: the attacker an ending names is always a witness,
so both sides hold the account by construction and the population is saturated at
100%. That makes this ratio a pure numerator comparison — the opposite of the
denominator inflation the predecessor had to disclaim.

### Ingroup preference rises, but not where the derivation says it should

The second prediction had two clauses. The share of cross-people holders keeping
a telling that reached them *without* crossing a seam should rise under the
penalty; and the rise should be strictly larger for people-pairs in the **bottom**
tercile of contact — near-strangers — than in the top. The second clause is the
one that matters, because a uniform rise shows only that a penalty penalises,
while the tercile ordering is what would demonstrate the *magnitude* is reading
the world's history.

Terciles were cut on **people-pairs**, not on crossings: 48% of all crossings on
the panel sit at a single pair, so a per-crossing tercile would report one pair's
behaviour as the world's. That gives 172 seamed pairs, cut at ≤1 edge (73 pairs),
2–3 (53), and >3 (46).

| rule | bottom | middle | top |
|---|---|---|---|
| additive | 486→486 (**+0.00 pp**) | 1618→1623 (+0.06) | 5137→5169 (+0.08) |
| quadrature | 471→471 (**+0.00 pp**) | 1620→1625 (+0.06) | 5566→5617 (+0.12) |
| multiplicative | 486→486 (**+0.00 pp**) | 1631→1639 (+0.10) | 5260→5298 (+0.09) |

Clause 1 holds, barely: a rise of +0.06 to +0.10 percentage points, 37 to 56
holders out of 57,204. **Clause 2 is falsified outright on all three rules.** The
bottom tercile's rise is exactly zero, on 7,621 holders, on every rule, while the
top is positive — so *bottom greater than top* fails however anyone argues about
exposure.

The direction that observation seems to point in is deliberately **not** promoted
here. The counts behind it are 0/5/32, 0/5/51 and 0/8/38 flips, and whether that
is a signal depends entirely on which denominator one considers eligible: uniform
over all holders it is improbable, uniform over *ingroup-holding* holders it is
not. The falsification is robust; the story about why is not, and is left as an
observation.

Two facts qualify the tercile axis itself, and both are now printed beside the
result rather than buried in a report. The buckets are keyed on the contact
between the holder's people and the ending's subject's people — but penalties are
levied at whatever boundaries the route actually crosses, and **1,882 of the
bottom tercile's 7,621 holders (24.7%) have no direct edge at all between those
two peoples.** They were reached through a chain, and every penalty they paid was
priced by some *other* pair.

### The mechanism reached them. Exactly.

An exactly-zero rise is two different findings wearing one number — *the penalty
never reached this population* and *the penalty reached it and changed nothing* —
and the aggregate cannot tell them apart. The battery therefore carries a control
that can, and it is an identity rather than a sample.

Every cross-people holder whose route crossed a seam under the free arm should
have had its width move under the penalised one. Counted in a separate pass:

| rule | crossed under the free arm | widths moved |
|---|---|---|
| additive | 49,963 | 49,963 |
| quadrature | 49,547 | 49,547 |
| multiplicative | 49,827 | 49,827 |

**To the unit, on all three rules.** That also proves no ladder on the panel
returned a zero finest span, since a zero unit would have left some crossing
holder untouched. Broken out by tercile, it says the thing the verdict needs:
**7,135 of the bottom tercile's 7,621 holders — 94% — demonstrably paid a
penalty, and that tercile's flip rise is +0.00.**

So the bottom tercile's zero is a **clean null**, not an absent one. The
mechanism arrived, was charged, and changed nothing about which account those
communities kept.

### The named null fires, and it is the headline

Both levels, side by side, over 1,413,515 contact holders:

| rule | widths moved | held telling changed | mutually-exclusive change |
|---|---|---|---|
| additive | 63,730 (4.51%) | 4,636 (0.328%) | 22 → 22 (**+0**) |
| quadrature | 66,186 (4.68%) | 2,162 (0.153%) | 17 → 17 (**+0**) |
| multiplicative | 63,983 (4.53%) | 3,104 (0.220%) | 53 → 53 (**+0**) |

The mechanism fires on about 4.5% of all holders, survives to change what is
actually held at 0.15–0.33%, and moves the aggregate the campaign is about by
**zero**. Thousands of communities changed their minds about which account of a
war they carry, and the number this thread has been reporting for two campaigns
is bit-for-bit unmoved.

The battery distinguishes three outcomes explicitly, because two of them print
the same headline: *inert at both levels* (no widths move), *invisible at emit*
(widths move, nothing held changes), and *the named null* (holdings change, the
aggregate does not). Only the third occurred.

About a fifth to a quarter of the mechanism's reach — 13,767 / 16,639 / 14,156 of
the ~64,000 width changes — lands on holders of the ending subject's **own**
people: communities that never look foreign at all, paying a crossing because
the least-damaged route out of their own history went through someone else's and
came back. That zigzag is the predecessor's open question priced.

## The three statements, and the third is a limit on what may be claimed

**One. The mechanism works.** 477 holder-rungs move; 94% of the near-stranger
tercile demonstrably paid; the reach identity closes to the unit on all three
rules; the direction the arithmetic forbids — a holder gaining a crossed telling
it did not have — occurs 0 times out of 57,204, reported rather than asserted so
that a separate known defect could not be mistaken for it.

**Two. The preregistered measure cannot see it.** The aggregate moves by +0
everywhere. This is the third time this thread has found the same dissociation:
a change that rewrites which account a large fraction of the world holds, and an
aggregate that does not register it. At some point that stops being a surprising
result about a mechanism and becomes a statement about the instrument.

**Three. Whether the derived magnitude beats a constant one is untested, and
this campaign must not claim otherwise.** An early draft of the readout's own
decision table said that falsifying the tercile clause would mean the contact
term is inert and the derivation decorative. It does not, and that gloss was
withdrawn before it shipped. The table had been written for a *uniform* rise;
what happened was an *inverted* one, and an inverted ordering is equally
consistent with a **constant** penalty — 48% of crossings sit at one pair, so
flips concentrate wherever crossings are, whatever the magnitude rule is. There
is no arm anywhere in this campaign separating a derived magnitude from a
constant one; the k-multiplier sweep varied a *global* scalar, which cannot
reorder pairs relative to each other. Unit tests prove the formula responds to
edge count. Nothing on this panel proves that responsiveness matters on real
data more than a constant would.

**So the derivation — the campaign's entire licence — is neither confirmed nor
refuted here.** That is a limitation of the instrument, not a finding against the
model, and the arm that would settle it is nameable: the penalised walk against a
constant-denominator control at matched mean penalty.

## A defect in merged code, and the mechanism behind it

The shipped walk is **not always its own argmin**. Enumerated exhaustively —
13,569,981 routes over 4,388 holders, which is the only instrument that could see
it, since any reimplemented relaxation inherits the behaviour rather than
detecting it — **36 of 13,164 holder-rule cells (0.27%)** hold a telling with the
same width bits and the same remembered day as an available route, but one hop
more. Descent is clean: 0 of 310,215. The denominator is a **(holder, rule)
cell** and not a holder — 4,388 holders scored under each of three accumulation
rules — which is why it exceeds the holder count it is built from.

Under the crossing penalty it roughly **doubles, to 90 of 13,164 (0.68%)**, and
the increase is almost entirely on one rule: the additive rule goes 32 → 88,
quadrature *shrinks* 4 → 2, and the multiplicative rule sits at exactly **0**
under both arms. Both figures are floors rather than point estimates — about 10%
of foreign endings are excluded by a size cap before any comparison, and they are
by construction the densest ones, where the defect is likeliest. Both arms drop
the same ones, so the ratio between them is like-for-like.

**The mechanism explains all three columns at once, and it is structural.** The
generational span between two communities is `|founded_h − founded_t| / g`. Along
a run of steps within one people, with a locally monotone founding order, those
spans **telescope**: the sum collapses to the endpoints. So under the *additive*
rule — which simply sums them — the accumulated width of a route depends only on
where it starts and ends, and is **blind to how many hops it took**. Two routes of
different length between the same endpoints carry bit-identical width. The
defect's signature is precisely "same width bits, one more hop", so it is an
additive-only phenomenon by construction. The other two rules compose
non-linearly and do not telescope. Two independent synthetic checks over 100,000
random monotone chains agree on the structure — roughly two-thirds of
different-length additive routes bit-identical, zero for the other two rules — and
disagree measurably on the rate, for a reason neither identified.

The defect is reproduced and **not fixed**. No one-line repair is defensible: the
question is whether the ordering key's monotonicity survives the additive rule's
width degeneracy, which is a re-expansion-policy question rather than a typo.

## What this leaves

The descent arm is byte-identical to what shipped before, which is what licenses
every comparison above.

The directed edge is **closed**, not deferred: it was measured, it does not
explain pooling, and the axis it sits on is the minority case. The tie-break is
**exonerated** — and half of the ratio it was supposed to move turns out never to
have been at stake, because a tree offers no choices.

What stands open is sharper for both eliminations. Ingroup preference now exists
as a derived output and is demonstrably paid by the communities it was designed
for, and the aggregate this thread reports is blind to it — so the next instrument
is not another mechanism but a **measure that can see one**.

One candidate is already measured rather than merely proposed. The scale probe's
**day-moved** column runs *above* its rung-moved one under the multiplicative
rule — **468 against 417** — because a changed winning route re-floors the
remembered day even where the rung that day is reported at does not move. A
measure keyed on the remembered day is therefore strictly more sensitive than
one keyed on the rung, on the same walk, at no extra cost. It is not the whole
answer — a day that moves is not yet a disagreement between two accounts — but a
thread that needs an instrument should start from the observable that already
registers more.

A second question this campaign asked and answered belongs here rather than
buried in a probe, because a reader of the census-versus-panel argument would
never think to open a hearsay probe to find it. The merge queue left an
unexplained **1.89×** residual on `endings`, the 12-seed control reading high
against the census. It is **not a seed-count artifact**: the 12-seed prefix reads
**low** against a 100-seed panel (0.792× on `endings`, 0.744/0.744/0.726 on
foreign, compared and mutually-exclusive), so correcting for seed count makes the
residual *larger*, around 2.4×. All four z-scores sit within ~1.2 standard errors
of the wide mean, so this bounds the sampling question rather than settling it in
the other direction — but it moves the residual's explanation into what the
census does *differently*: its build depth, its pin sets, or the denominator it
divides by.

And the one question
this campaign was built to answer about its own derivation, it did not ask:
whether a magnitude read from the world's contact history does anything a
well-chosen constant would not. That arm is cheap, it is named, and until it runs,
the derivation is a design commitment rather than a measured one.
