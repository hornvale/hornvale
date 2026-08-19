# The Undertow — a seam costs what the peoples across it are strangers

**Campaign 5 of the Myth thread.** Predecessors: The Hearsay (claims held at a
distance), The Retelling (content varies by path), The Palimpsest (memory gets
a unit), The Parley (an account crosses a people boundary).

**This campaign's subject changed twice before a line of it was written, both
times because a probe falsified the premise.** §3 carries both eliminations;
they are the campaign's inheritance and they are why §5 is what it is.

---

## 1. What this campaign produces

One change to how a claim is chosen, and a readout that says whether it did
what it was built for:

**A seam crossing costs damage, and the cost is derived from how much contact
the two peoples actually have.** A claim crossing between peoples who raid each
other constantly arrives barely marked; one crossing between near-strangers
arrives heavily marked, and the receiving people keeps its own account instead.

The mechanism is **people-aware**. The magnitude is **derived from the ledger**.
That distinction is the whole design and §5.4 defends it against decision 0021.

## 2. Non-goals

- **Not a trust model.** No community holds an opinion about another people. The
  penalty is a function of the contact graph, evaluated at emit; nothing is
  stored on a holder and nothing accumulates as an attitude.
- **Not corroboration.** A stateful notion of belief — one that changes on
  confirmation — remains unbuilt and remains
  `KNOW-corroboration-needs-variation`'s blocker. §7 records why this campaign
  does not reach for it.
- **Not the unit fix.** The Palimpsest's §6.6 erratum stays frozen for a session
  that has not read its §6.7. Nothing here touches `gen_span`'s return or
  `accumulate.rs`'s `step`/`precision_at`.
- **Not a directed seam.** `KNOW-directed-contact` is measured-and-shelved by
  §3.2, not deferred for lack of time.
- **Not a new lab metric.** Nine studies declare `"metrics": "all"` with no
  opt-out. The readout is a heavy battery, as in campaigns 2, 3 and 4.

## 3. Substrate, measured before the model was frozen

> ### ERRATUM 0 — THE SUBSTRATE MOVED UNDER THIS CAMPAIGN AT ITS CLOSE, AND EVERY FIGURE BELOW IS RE-DERIVED
>
> This branch ran 137 commits without absorbing `main` once. **The Underworld
> changed settlement placement**, and `parley_readout.rs`'s own controls were
> re-pinned to match at `44ea8d5a`. The merge was textually clean — which is
> what made it dangerous, because nothing conflicted and every number in this
> document silently described a world the merge product does not contain:
>
> | seeds 0–11 | as measured | re-derived |
> |---|---|---|
> | endings | 5,913 | **4,975** (−15.9%) |
> | foreign attacker | 138 | **102** |
> | mutually exclusive (descent × multiplicative) | 19 | **15** |
>
> **The governing principle is that a campaign measures against the merge
> product it actually lands.** Every battery and probe was therefore re-run
> against this tree and every figure in §3 and §5.5 below re-derived from that
> run — not transcribed from `main`. Five independent instruments re-derived
> the three counts above and all five agree with `parley_readout.rs` exactly.
> §6's preregistration is **frozen and untouched**: no hypothesis, decision
> table or threshold was edited to match a re-derived number.
>
> **Two conclusions in §3 changed, and they are marked in place** — §3.3's
> tie-break exoneration is now one broken cell rather than none, and §3.5's
> defect is no longer moved by the campaign's own penalty at all. Both are
> stated where they occur rather than only here. The §6 *verdicts* are
> unchanged (H1 falsified, H2 falsified on clause 2) with one exception, also
> preregistered as a report rather than a verdict: §6.3's named null now fires
> on two of the three accumulation rules instead of three.
>
> Where a figure is a **citation of another campaign's published result**, it
> is left as that campaign published it and labelled pre-Underworld; where it
> is a **pilot measurement of a rejected design**, it is kept as the record of
> what was tried and labelled likewise. Nothing else below is pre-absorption.

Two probes, both committed before this spec existed:
`windows/hearsay/tests/probe_seam_direction.rs` (`8eb88fbf`) and
`probe_tiebreak_rules.rs` (`95cbdd33`). Twelve-seed panel, census seeds 0–11.
Every assertion in both is a positive control on the substrate; none is about an
outcome. Both re-derive `parley_readout.rs`'s committed constants — 4,975
endings, 102 foreign, 15 mutually-exclusive — and both hold a reimplemented
relaxation to the shipped `variants_about_accumulating` with **0 holders
differing** before reporting anything.

### 3.1 What The Parley left behind

Contact made the two sides of a raid **disagree less**: the divergence ratio
fell to **0.52× / 0.55× / 0.90×**
(additive / quadrature / multiplicative) — re-derived on the merge product both
by `parley_readout.rs`'s own H4 rows and, independently, by this campaign's
readout under `Crossing::Free`, which is the pre-campaign behaviour. The Parley
published 0.59× / 0.52× / 0.77× on the pre-Underworld substrate. Three campaigns
had assumed contact was divergence's precondition. On the frozen measure it is
the opposite, and the direction of that result survives the re-derivation on
every rule.

**Two qualifications the re-derivation adds and the original did not carry.**
First, the ordering across rules is not stable across substrates — quadrature is
now the middle rather than the strongest fall, and multiplicative the smallest
rather than the weakest. The sign is stable; the magnitudes are not, and no
argument below should rest on which rule pools hardest. Second, and larger:
**identical remembered-day sets rise only under quadrature now** (242 → 247);
under additive they fall 219 → 210 and under multiplicative 110 → 95, with the
lost mutual exclusion going into *one-sided containment* instead (181 → 200 and
263 → 283). "Contact makes the two sides agree" was a uniform statement as
executed and is a per-rule one on the merge product. Divergence falls on every
rule either way, which is the inherited finding this campaign is built on.

### 3.2 Symmetry is not the cause — `KNOW-directed-contact` is closed, not deferred

The Parley froze the seam **undirected** and named a directed variant as the
obvious follow-up. Simulating direction on the shipped graph:

| arm | additive | quadrature | multiplicative |
|---|---|---|---|
| both (undirected) | 0.80× | 0.86× | 0.67× |
| victim→raider only | 0.80× | 0.71× | **0.60×** |
| raider→victim only | 1.00× | 1.00× | 0.93× |

Divergence **never rises** under either restriction on any rule, and under
multiplicative the victim→raider arm collapses it *further* than the undirected
edge does. Pooling tracks seam **volume**, not symmetry.

**The re-derivation weakened the first clause and the weaker form is the one
stated.** As published this table read "divergence falls under *every* arm on
*every* rule"; on the merge product the raider→victim arm sits at exactly 1.00×
under additive and quadrature — level, not falling. Restricting the channel
still never *restores* disagreement, which is the claim the closure rests on,
but "falls everywhere" is no longer true and is not asserted. **These are small
integers on a 102-ending population** (descent mutex 5 / 7 / 15), so a single
event moves a ratio by 0.07–0.20; the table is a direction, never a magnitude.

**And the axis was wrong anyway.** ~55% of cross-people holders cross the seam
**more than once** (54.3 / 58.7 / 53.4%, depth histograms out to 10 / 13 / 8);
only ~6% are the hop-0 co-witness line (6.6 / 5.7 / 7.4%); the single-crossing
split runs 34.7/65.3, 34.5/65.5, 34.7/65.3 — leaning to raider→victim on all
three rules where it had been near-balanced. Victim-versus-raider is a story
about a *single* crossing, and single crossings are the minority case (39% of
cross-people holders under additive). A campaign frozen on it would have
preregistered on an axis the substrate does not sit on.

### 3.3 The argmin is not the cause either — but the re-derived table is one cell weaker

**THIS SECTION'S HEADLINE CHANGED ON RE-DERIVATION AND IS NOT REWRITTEN TO
MATCH.** As published it read: of the **8** cells where the baseline pools and
an alternative could disagree, **0** did, largest gap **2 events of 124**. On
the merge product the eligible matrix is smaller and one cell breaks: of the
**4** cells where the baseline pools and an alternative could therefore
disagree, **1** did — `recency` under multiplicative, whose identical-day count
falls (23 → 19) where the baseline's rises (23 → 29). The largest gap between
any rule's mutually-exclusive count and the baseline's is **4 events of 100**.

Four people-blind selection rules — least-damage (shipped), primacy, frequency,
frequency-weighted, recency — over the same worlds. So the honest statement is
now: *the tie-break is implicated at one cell, on the one rule that is not
shippable as written.* `recency` maximises hops, which is unbounded around a
seam cycle; it is defined in that probe only because the enumeration was
restricted to simple paths. Of the rules that could ship, `primacy` and
`frequency-w` pool exactly as the baseline does and `frequency` pools with a
4-event gap.

**A structural fact stands behind that table and is stronger than it — and it
is untouched by the re-derivation.** Under `Contact::Descent` — the ratio's own
**denominator** — all **82,209** holders receive exactly **one** telling. The
founding tree is a forest and witnesses are never re-entered, so there is no
choice to make anywhere. **Half of The Parley's ratio was never at stake.** No
selection rule could have moved it, and that is a fact about the shape of a
tree rather than about any substrate's numbers.

This also closes the elegant people-blind repair before it was proposed: a
count of concordant tellings is what `frequency`/`frequency-w` already measure,
and they pooled.

### 3.4 THE DISSOCIATION — and it is this campaign's chief hazard

Under contact, **50.5%** of holders carry two or more distinct remembered days,
and the five selection rules disagree about which is held at up to **41.9%** of
holders. Swapping the rule **rewrites nearly half the world's held beliefs** and
moves §6.5's aggregate by **≤4 events of 100**.

That is a dissociation, not a null. **The Parley's divergence measure is nearly
blind to a change that touches half the population.** §6 preregisters against
this at both levels, because it is the most plausible way this campaign produces
a real number that means nothing.

### 3.5 A defect in merged code, found by exhaustive enumeration

**The shipped walk is not always its own argmin under the seam** — 49 of 9,531
contact holder-rule cells (**0.51%**) hold a telling with the same width bits
and the same remembered day but one hop more than an available route. The
denominator is a **(holder, rule) cell**, not a holder: 3,177 holders scored
under each of three accumulation rules. Descent is clean: 0 of 246,627.

Neither earlier probe could have seen it: each reimplemented a *relaxation*, so
each inherited the behaviour rather than detecting it. It took a full route
enumerator (10,589,340 simple routes over 3,177 holders).

**RE-DERIVED, AND THE SECOND HALF OF THIS FINDING INVERTED.** As executed, the
campaign measured the defect **roughly doubling** under its own penalty — 36 →
90 of 13,164, almost all on `additive` (32 → 88), quadrature *shrinking* 4 → 2,
multiplicative 0 under both — and that differential is why every additive column
in the readout carried a CONFOUNDED annotation. On the merge product the two
arms are **identical at 49**: additive 49 → 49, quadrature 0 → 0, multiplicative
0 → 0. `Crossing::ContactWeighted` does not move this defect at all here.

That is the outcome the *structural* explanation predicted and the executed
numbers muddied. The mechanism is `gen_span`'s telescoping within a same-people
segment along a locally monotone founding-day run, which makes additive width
endpoint-determined and hop-blind **by construction** — a property of
`amplitude.rs` and `accumulate.rs` with nothing to do with `Crossing`. A defect
that is structurally additive-only should not respond to an arm that only
changes cross-people step costs, and on this substrate it does not. The defect
is still real, still unfixed, and still entirely on `additive` (1.54% of its
cells against 0% for the other two rules).

Both counts remain **floors**: the enumeration's size cap drops 2 of 102 foreign
endings from every cell on both arms, and a capped ending is by construction the
densest one — where a route-count-dependent defect is likeliest. Both arms drop
the same ones, so the arm-to-arm comparison stays like-for-like.

### 3.6 What the least-damaged route actually does

A seam-crossing cap at 2 was tried and **rejected by its own control**: winners
piled at the ceiling (`0:13670 1:16491 2:11512`) and 21,476 holders lost their
shipped answer. Those three numbers are a **pre-Underworld pilot measurement of
a rejected design** and are not re-derivable — the capped arm was deleted with
the bound it rejected — so they are kept as the record of what was tried.

The conclusion they support is re-derived and unchanged: **the least-damaged
route zigzags across the seam**, with crossings on the shipped winner's route
still running out to **8** on the merge product (`0:1375 1:853 2:466 3:264
4:122 5:56 6:30 7:9 8:2` under multiplicative). Any design that assumes a claim
crosses once is wrong about this substrate.

## 4. Where the code lives

| file | role |
|---|---|
| `windows/hearsay/src/derive.rs` | `tellable`, the relaxation, the ordering key |
| `windows/hearsay/src/contact.rs` | `ContactGraph` — §5.2 reads its edge multiset |
| `windows/hearsay/src/transmission.rs` | `Transmission`, `Walk`, `AS_SHIPPED` |
| `windows/hearsay/src/amplitude.rs`, `accumulate.rs` | **read, never changed** (§2) |

`windows/hearsay` is a window: it reads a committed ledger and derives. Nothing
draws, no stream label, no domain dependency, no save-format contract moves.

## 5. The derivation

### 5.1 The crossing penalty

A step from `teller` to `hearer` whose peoples differ adds, to the accumulating
width, a penalty in addition to the step's ordinary `gen_span`:

```
crossing_penalty(a, b) = span(FINEST) / (1 + contact_edges(a, b))
```

where `contact_edges(a, b)` is the number of raid edges in `ContactGraph`
between any occupation of people `a` and any of people `b`, and `span(FINEST)`
is the width the accumulator already seeds itself with
(`windows/hearsay/src/derive.rs:449`).

**`contact_edges` must be DERIVED ONCE, and today's API cannot answer it.**
Verified against the code rather than assumed: `ContactGraph` exposes exactly
`peers_of(occ) -> &[(EntityId, f64)]` and `edges() -> usize`
(`windows/hearsay/src/contact.rs:55,64`). Neither answers "how many raid edges
lie between people `a` and people `b`" — `peers_of` is keyed by *occupation*,
and `edges()` is a bare total. So the campaign owes a people-pair tally built
once, at `Walk` construction, alongside the graph: one pass over the contact
edges reading `occ-people` at both endpoints into a
`BTreeMap<(String, String), usize>` with the pair ordered canonically so
`(a,b)` and `(b,a)` are one key.

Deriving it per step instead would re-scan the graph at every crossing, which
is the exact pattern `lineage.rs`'s own header records The Begat deleting from
the read path and warns against reintroducing one level up. This is the
project's derive-once discipline (`kernel/CLAUDE.md`'s `Fbm` pattern) applied
to a read-side structure.

Read it as: **a crossing costs one finest rung, discounted by how well the two
peoples know each other.** Peoples with a single recorded contact pay the full
rung; peoples with nineteen pay a twentieth of it. A same-people step pays
nothing, which is what makes ingroup preference an **output**.

### 5.2 Why this magnitude and not a constant

A constant — "a crossing costs 0.7" — would be an authored ingroup preference
and is the thing §5.4 forbids. `contact_edges` is read from the same ledger
facts the seam itself is built from, so the preference's *strength* is a
property of the world's history rather than of this campaign's taste. Two
peoples who have raided each other for centuries trust each other's accounts;
two who met once do not. Nothing about that ranks either people.

`span(FINEST)` is the natural unit because the accumulator is already seeded
with it (`derive.rs`), so the penalty is commensurate with the width it joins
and introduces no new scale.

### 5.3 What is deliberately NOT changed

- **The ordering key stays `(width, hops, witness)`.** The penalty enters
  through `width`, reusing machinery §3.3 exonerated, rather than adding a key
  component that would need its own justification.
- **`gen_span` and the accumulation rules are untouched** (§2).
- **The seam stays undirected** — §3.2 measured the alternative and it changes
  nothing.
- **The emit ladder stays the originating witness's**
  (`KNOW-teller-ladder-at-emit`, still carried forward).
- **`Transmission::AS_SHIPPED` must remain behaviour-identical.** Every number
  below is a difference from it.

### 5.4 Decision 0021, and why this is admissible

0021 forbids "any ideology **ranking** species" as an input, and names *race —
the doctrine that some species are inferior* as something that must be a
generated output. **An earlier draft of this campaign's own reasoning read that
as forbidding any people-aware rule at all. That is too strong, and the
overreach is recorded rather than quietly corrected.**

A community preferring its own line's account is not asserting that the raiders
are inferior. It is a preference over **sources**, not over **worth** — and its
strength here is derived from contact history, not declared. What 0021 requires
is that the *ideology* be manufactured on top of material conditions, which is
exactly the position this leaves the model in: the transmission asymmetry
exists, and any doctrine built on it remains unbuilt and unauthored.

The line that must not be crossed, stated so a later campaign can check it: **no
constant may encode a preference between two peoples.** If a magnitude cannot be
traced to a ledger fact, it is authored.

**Nathan's ratification of §5.4 at G3 sharpens 0021's intent past what its text
states, and it is recorded here because it governs every future campaign in
this thread:**

> The thing to avoid is **the game** assuming kobolds are stupid and evil and
> elves are good and pure. The player, and the creatures within the world, are
> likely to have very strong opinions on these questions.

So the target was never *an absence of prejudice*. It is a question of **whose**
prejudice it is. A world where nobody holds a view about anybody is not the goal
and would be a worse simulation than one where views are held and are wrong.
What 0021 forbids is the **engine** holding the view — a species carrying a
valence, a lookup table deciding conduct. What this thread is building toward is
the opposite: creatures with strong, situated, mistaken opinions about each
other, arrived at because of how their accounts of shared history actually
travelled.

That reframing is why §5.1 is admissible and why a *constant* would not be. A
derived crossing penalty gives communities a reason to keep their own account;
whatever they come to believe about the people across the seam is then theirs,
not ours.

### 5.5 ERRATUM — §5.1's worked reading is wrong, corrected before any readout

**Measured, not argued** (`windows/hearsay/tests/probe_crossing_scale.rs`,
committed `05b1565f`, before Task 4 existed and before any hypothesis was read
out).

§5.1 says "peoples with a single recorded contact pay the full rung". **They
cannot.** `contact_of` builds the peers list and the people-pair tally in the
same loop from the same ending record, so any edge that makes two occupations
peers has already incremented its own pair's count. Therefore
`edges_between ≥ 1` at every reachable seam crossing, the denominator is ≥ 2,
and **the ceiling is half a finest rung, not a whole one.** Re-derived over
**26,798 winning-path crossings** on the merge product: `edges_between == 0`
occurred **0 times**, the realized penalty ran from `span(FINEST)/12` to exactly
`span(FINEST)/2`, and **all 26,798 crossings were carried by a seam edge — zero
by descent.** (As executed, on the pre-Underworld substrate: 67,765 crossings,
`span(FINEST)/26` to `span(FINEST)/2`, same two zeros. The *ceiling* — half a
finest rung — is an argument about `contact_of`'s construction and does not
depend on either measurement; only the *floor* moved, because the panel's
densest people-pair now carries 11 edges rather than 25.)

The full rung is reachable only by a descent step across a people boundary,
which is **open in code and never walked by the bake** — a measurement (campaign
2's own, zero of 780 typed edges; and re-derived here, zero descent-carried
crossings of 26,798), not an invariant. Task 2's reviewer is owed that
distinction.

**THE FORMULA IS NOT CORRECTED, and the reason is measurement rather than
reluctance.** The controller commissioned the probe expecting the mechanism to
be inert — a half-rung penalty against what it believed was a 41.7× rung gap —
and intended a numerator correction. Two of its inputs were wrong:

- **The gap is not 41.7×.** Rung 1 is *this world's first moon*, and
  next-over-finest ranges **2.23× … 530.85×, median 12.56×** across the panel's
  180 ladders. Seed 1 steps 1.5507 d → 3.4606 d, where a half-rung penalty is
  **40.6% of the gap**. Seed 11 is moonless and steps 1.0010 d → 531.39 d, where
  it is 0.094%. Same twelve seeds. **This bullet is the one figure in §5.5 that
  the re-derivation left untouched, to the last digit** — precision ladders are
  built from moons and years, and The Underworld moved settlements, not the
  sky.
- **Width is cumulative**, so "one crossing against one gap" is the wrong
  comparison. What matters is how many holders sit within one penalty of a
  boundary after a whole path — a density, small but non-zero.

**282 holder-rungs move** between `Free` and `ContactWeighted` (additive 39,
quadrature 29, multiplicative 214) against 22,681 holder-widths. The mechanism
reaches the ladder at **k = 1** for every rule; a re-walk at multipliers 1×–256×
finds no threshold above it, and shows the width-moved column is **constant in
k** — a bigger penalty buys resolution, never reach. (As executed: 477 rungs
— additive 50, quadrature 10, multiplicative 417 — against 58,618 holder-widths.
The *conclusion* is unchanged on every rule; the population is ~2.6× smaller and
the rung count fell with it, and quadrature is now the rule the penalty reaches
least unevenly rather than the one it barely reaches at all.)

**And the analytic estimate would have been wrong.** Median headroom by
calculation is 38.4×–1,201.1× (multiplicative 38.4×, additive 163.2×, quadrature
1,201.1×), which reads as "inert". The 282 are the **tail**, not the median. The
probe re-walked the panel instead of dividing two numbers, which is the only
reason this erratum says *keep the formula* rather than *change it* — and the
re-derivation makes that point harder, not softer: the headroom medians fell by
roughly an order of magnitude while the rung count fell too, so nothing about
the median predicted the tail on either substrate.

## 6. Preregistration

Frozen before the code in §5 exists. A falsified prediction is a finding.

### 6.1 H1 — the penalty breaks pooling

**Prediction: under contact, the count of mutually-exclusive cross-people
endings rises above its descent count under at least one accumulation rule** —
i.e. the ratio §3.1 measured at 0.59×/0.52×/0.77× exceeds **1.0**.

Reported as counts with both populations named, never as a bare ratio: the
eligible population differs between arms by construction, and The Parley shipped
a near-miss where exactly that inflated an effect fourfold.

Falsified if every rule stays below 1.0 — which would mean pooling is a property
of seam volume that selection cannot reach, and the thread needs a different
instrument entirely.

### 6.2 H2 — ingroup preference appears as an output, and tracks contact

**Prediction: the share of cross-people holders keeping a telling that reached
them without crossing a seam rises under the penalty; and the rise is strictly
larger for people-pairs in the bottom tercile of `contact_edges` than the top
tercile.**

The second clause is the one that matters. A uniform rise would show only that a
penalty penalises. The tercile split is what demonstrates the magnitude is doing
work — that the model is reading the world's contact history rather than
expressing a constant.

Falsified if the rise is uniform across terciles, which would mean
`contact_edges` is inert and §5.2's derivation is decorative.

### 6.3 THE NAMED NULL — the dissociation repeating one level up

§3.4 measured a selection change that rewrote 45.7% of holders and moved the
aggregate by ≤2 events of 124. **The live null is that §5.1 does the same: it
changes which account communities hold, enormously, and leaves whether the two
peoples agree exactly where it was.**

**Both levels are reported side by side, always:** the share of holders whose
held telling changes, against the change in the mutually-exclusive count. A
campaign that reports only the second cannot tell a working mechanism from an
inert one, and this substrate has already produced that shape once.

### 6.4 What is asserted versus reported

The battery asserts only substrate controls: the panel built; held claims exist;
no claim reports a rung its own ladder lacks; `AS_SHIPPED` reproduces the
pre-campaign walk; the penalty never *removes* a holder; §3's published counts
re-derive. **Every hypothesis is REPORTED.** This file and that battery must
never be edited to rescue a prediction.

## 7. Carried forward

- **`KNOW-corroboration-needs-variation`** — still blocked on a stateful notion
  of belief. §3.3 shows why this campaign cannot supply it cheaply: under
  descent every holder receives exactly one telling, so a confirmation count is
  1 everywhere the seam is absent, and a belief model would be exercised only at
  the seam.
- **`KNOW-teller-ladder-at-emit`**, **`KNOW-signed-amplitude`**,
  **`KNOW-lectio-difficilior`**, **`KNOW-misattribution-drift`** — unchanged.
- **`KNOW-directed-contact` — CLOSED by §3.2**, not deferred. Its row should be
  flipped to `refuted (measured)` with the numbers.
- **The 0.51% non-argmin holder-rule cells (§3.5)** — a defect in merged Parley
  code. Task 1 reproduces it against the shipped walk and decides; it may vanish
  under §5.1's changed width. **It does not**: re-derived, the count is
  identical under both `Crossing` arms.
- **The zigzag (§3.6)** — the least-damaged route crosses the seam up to 8
  times. Whether that is physical or an artifact of a cost model with no
  distance term is open.

## 8. Definition of done

- §5.1 implemented; the `descent` arm byte-identical to today.
- The preregistered readout (§6) as a heavy battery over the 40-seed panel
  campaigns 3 and 4 used, reporting both levels of §6.3 side by side.
- The §3.5 defect reproduced and dispositioned.
- Chronicle, retrospective, registry flips (including `KNOW-directed-contact`),
  Confidence Gradient re-score if a bet moves, book freshness sweep.
- `make sluice-stage` at each plan-stage boundary; `make sluice` to merge.

## 9. Open for Nathan at G3

1. **§5.4 is the campaign's one judgement call.** It reverses this session's own
   earlier reading of 0021 — that any people-aware rule was forbidden. I believe
   the earlier reading was too strong and say why, but 0021 is yours.
2. **§6.3 may make this campaign a null**, and the substrate says that outcome is
   live rather than rhetorical. Worth knowing before it costs a campaign.
3. **Scope.** §5.1 is one formula. The defect in §3.5 and the zigzag in §3.6 are
   each defensible campaigns of their own and are carried, not bundled.
