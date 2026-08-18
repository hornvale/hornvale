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

Two probes, both committed before this spec existed:
`windows/hearsay/tests/probe_seam_direction.rs` (`8eb88fbf`) and
`probe_tiebreak_rules.rs` (`95cbdd33`). Twelve-seed panel, census seeds 0–11.
Every assertion in both is a positive control on the substrate; none is about an
outcome. Both re-derive `parley_readout.rs`'s committed constants — 5,913
endings, 138 foreign, 19 mutually-exclusive — and both hold a reimplemented
relaxation to the shipped `variants_about_accumulating` with **0 holders
differing** before reporting anything.

### 3.1 What The Parley left behind

Contact made the two sides of a raid **agree more**: identical remembered-day
sets rose on every accumulation rule, and the divergence ratio fell to
0.59× / 0.52× / 0.77×. Three campaigns had assumed contact was divergence's
precondition. On the frozen measure it is the opposite.

### 3.2 Symmetry is not the cause — `KNOW-directed-contact` is closed, not deferred

The Parley froze the seam **undirected** and named a directed variant as the
obvious follow-up. Simulating direction on the shipped graph:

| arm | additive | quadrature | multiplicative |
|---|---|---|---|
| both (undirected) | 0.60× | 0.58× | 0.84× |
| victim→raider only | 0.70× | 0.67× | **0.68×** |
| raider→victim only | 0.80× | 0.75× | 0.84× |

Divergence falls under **every** arm on **every** rule, and under multiplicative
the one-way arm collapses it *further* than the undirected edge. Pooling tracks
seam **volume**, not symmetry.

**And the axis was wrong anyway.** ~70% of cross-people holders cross the seam
**more than once** (depth histograms out to 13 / 19 / 9); only ~4% are the hop-0
co-witness line; the single-crossing split is near-balanced (41.6/58.4,
37.5/62.5, 42.4/57.6). Victim-versus-raider is a story about a *single*
crossing, and single crossings are the minority case. A campaign frozen on it
would have preregistered on an axis the substrate does not sit on.

### 3.3 The argmin is not the cause either

Four people-blind selection rules — least-damage (shipped), primacy, frequency,
frequency-weighted, recency — over the same worlds. Of the **8** cells where the
baseline pools and an alternative could disagree, **0** did; the largest gap was
**2 events of 124**.

**A structural fact stands behind that table and is stronger than it.** Under
`Contact::Descent` — the ratio's own **denominator** — all **103,405** holders
receive exactly **one** telling. The founding tree is a forest and witnesses are
never re-entered, so there is no choice to make anywhere. **Half of The Parley's
ratio was never at stake.** No selection rule could have moved it.

This also closes the elegant people-blind repair before it was proposed: a
count of concordant tellings is what `frequency`/`frequency-w` already measure,
and they pooled.

### 3.4 THE DISSOCIATION — and it is this campaign's chief hazard

Under contact, **51.3%** of holders carry two or more distinct remembered days,
and the five selection rules disagree about which is held at up to **45.7%** of
holders. Swapping the rule **rewrites nearly half the world's held beliefs** and
moves §6.5's aggregate by **≤2 events of 124**.

That is a dissociation, not a null. **The Parley's divergence measure is nearly
blind to a change that touches half the population.** §6 preregisters against
this at both levels, because it is the most plausible way this campaign produces
a real number that means nothing.

### 3.5 A defect in merged code, found by exhaustive enumeration

**The shipped walk is not always its own argmin under the seam** — 36 of 13,164
contact holders (**0.27%**) hold a telling with the same width bits and the same
remembered day but one hop more than an available route. Descent is clean: 0 of
310,215.

Neither earlier probe could have seen it: each reimplemented a *relaxation*, so
each inherited the behaviour rather than detecting it. It took a full route
enumerator (13,569,981 routes over 4,388 holders).

### 3.6 What the least-damaged route actually does

A seam-crossing cap at 2 was tried and **rejected by its own control**: winners
piled at the ceiling (`0:13670 1:16491 2:11512`) and 21,476 holders lost their
shipped answer. **The least-damaged route zigzags across the seam**, with
crossings running out to 8. Any design that assumes a claim crosses once is
wrong about this substrate.

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
- **The 0.27% non-argmin holders (§3.5)** — a defect in merged Parley code. Task
  1 reproduces it against the shipped walk and decides; it may vanish under
  §5.1's changed width.
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
