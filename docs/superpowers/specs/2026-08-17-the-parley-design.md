# The Parley — an account crosses a people boundary, and arrives damaged

**Campaign 4 of the Myth thread.** Predecessors: The Hearsay (claims held at a
distance), The Retelling (content varies by path), The Palimpsest (memory gets
a unit, and the unit varies by who remembers).

**This spec was frozen by a controller contaminated on The Palimpsest's
exploratory readout and clean on everything here.** See §6.1.

---

## 1. What this campaign produces

Three layers of the transmission model, in dependency order, each shipped with
its own before/after measurement so that none is confounded with another:

1. **A clock.** A community that had already ended cannot hold a claim about a
   later event. Today 1.19% of holders do.
2. **The stance geometry, as two co-equal arms.** `Perpetrator` is a singleton
   label and `VictimLine` is closed under descent, so the raider's line loses a
   precision rung on its very first retelling 100.00% of the time against the
   victim line's 4.06%. Nobody chose that. Both geometries are implemented,
   both are reported, neither is nominated.
3. **A cross-people contact edge.** Transmission gains a horizontal step along
   `occ-ended-by`, so an account can leave the people that witnessed it. Today
   97.67% of accounts reach exactly one people and **none has ever reached
   three**.

The deliverable is the model plus a preregistered readout (§6) reporting all of
it side by side.

## 2. Non-goals

- **Not corroboration.** It needs a notion of belief that changes on
  confirmation, which does not exist. Contact is its precondition, not its
  blocker — this campaign supplies the precondition and stops.
- **Not the unit fix.** The Palimpsest's §6.6 erratum (the amplitude adds a
  dimensionless generation count to a std-days width) is **deliberately left
  frozen and untouched** for a session that has not seen §6.7. Nothing here
  changes `accumulate::step`, `precision_at`, or `gen_span`'s return.
- **Not the signed amplitude.** `KNOW-signed-amplitude` stays carried forward:
  varying the graph's topology and the amplitude's sign at once measures
  neither. This campaign's §5.4 makes the *existing* teller pin load-bearing,
  which is the prerequisite that row was waiting on.
- **Not a new lab metric.** Nine studies declare `"metrics": "all"` with no
  opt-out (`windows/lab/CLAUDE.md`). The readout is a heavy battery, as in
  campaigns 2 and 3. Cost accepted: the readout is not tracked in the census.
- **Not blind reconstruction.** Contact is stemmatic *contamination*, and the
  literature's claim is that contamination is what breaks tree reconstruction.
  That makes `KNOW-lectio-difficilior` more interesting and strictly harder;
  it is carried forward, not attempted.

## 3. Substrate, measured before the model was frozen

Instrument: `windows/hearsay/tests/probe_contact_substrate.rs` (committed at
`cd8221be`, before any hypothesis below was written). Twelve seeds, census
seeds 0–11 — a strict prefix of The Palimpsest's 40-seed readout panel — plus
seed 42 for the published-count control. Every assertion in the probe is a
positive control proving it reached the population it reports on; none is
about an outcome.

### 3.1 The raid seam is thin at hop zero and dense as a graph

| quantity | value |
|---|---|
| endings, 12 seeds | 5,913 |
| endings **naming an attacker** | **2,804 (47.4%)** |
| endings whose attacker is of another people | 138 (2.33%) |
| per-seed range of that share | 0.0% (seed 2) – 4.7% (seed 11) |
| seed 42 | 16 of 585 |

Campaign 2 published **17 of 474** for seed 42; this measures **16 of 585** —
the count is stable, the denominator is not, because main has moved. The probe
therefore asserts only that the mechanism is live, never the exact figure.

**The 2.33% is the misleading number and it is the one three campaigns quoted.**
It counts boundary crossings at hop zero only. Nearly half of all endings name
an attacker, so the *contact graph* is dense; a claim crossing a people
boundary two contacts down a chain is invisible to that figure.

### 3.2 The far side has a line to tell it to

Of 154 foreign attackers, **4** have no descendants. Median descendant count 3,
maximum 101. An edge to the raider is not an edge to a leaf.

### 3.3 Reach today, and the ceiling under contact

| walk | reaches 2+ peoples | reaches 3+ | max peoples | holders |
|---|---|---|---|---|
| `descent` — ships today | 138 (2.33%) | **0** | 2 | 103,405 |
| `contact` — undirected, ungated | 953 (16.12%) | 152 (2.57%) | 4 | 342,670 |
| `gated` — clock applied | 732 (12.38%) | 104 (1.76%) | 4 | 252,972 |

Control: the `descent` walk reaches three peoples on exactly **0** of 5,913
endings, reproducing the shipped model — so the probe walks the graph the model
walks. Endings with no foreign attacker that reach 2+ peoples today: **0 of
5,775.** Contact today is a co-witnessing coincidence, never a channel.

**A contact edge multiplies cross-people reach 5.3× even with a clock, and
creates 3- and 4-people accounts that are structurally impossible today.**

### 3.4 The disagreement that exists is smaller than it first looked

A first instrument compared the two peoples' *sets* of remembered days and
found 111 of 138 unequal. Set inequality is weak. The sharper question — does
**each** side hold a day the other holds nowhere? — gives:

| | events |
|---|---|
| mutually exclusive (genuine two-sided divergence) | **19** (13.8%) |
| one-sided (one set strictly contains the other) | 92 (66.7%) |
| identical day sets | 27 (19.6%) |

Median holders per side: 4.0 victim, 4.0 raider, so the one-sided majority is
not an artifact of one line being larger. **The baseline for cross-people
disagreement is 19 events per 12 seeds, not 111.** This spec reports 19.

### 3.5 The clock defect

| quantity | value |
|---|---|
| founding steps where the child predates the parent | **0** of 8,662 |
| holders founded after the event they hold | 132,214 (80.2%) — normal; descendants inherit |
| holders that **ended before** the event they hold | **1,959 (1.19%)** |

Confirmed on named instances in the shipped model's own output, not only in a
structural walk: holders 91,312 / 109,575 / 182,625 days dead (≈250–500 years)
holding claims at hops 1–2 about events postdating their end. `gen_span` takes
`(fh - ft).abs()`, total by construction and therefore silent about ordering.

### 3.6 The asymmetry nobody chose

`stance::stance_of` labels **only the exact `occ-ended-by` entity**
`Perpetrator`, while `VictimLine` is `subject` **or any descendant**. So the
step *attacker → attacker's own child* is `Perpetrator → Bystander` — a stance
crossing, and therefore a lost rung — while *victim → victim's own child* is
`VictimLine → VictimLine` and costs nothing.

| first retelling step | lossy |
|---|---|
| victim line (`subject → child`) | 124 of 3,056 (**4.06%**) |
| raider line (`attacker → child`) | 3,694 of 3,694 (**100.00%**) |

The 100.00% is structural, not a probe artifact — but **not for the reason a
first draft of this section gave.** That draft said `Bystander` is forced
because a child of the attacker never descends from the subject. That is false
where the attacker *itself* descends from the subject, and the measurement says
it happens: the raider's child lands on **Bystander 3,118, VictimLine 576,
Perpetrator 0**. The correct statement is weaker and still sufficient: a child
of the attacker can never be the attacker, so *some* non-`Perpetrator` label is
forced, and every one of them differs from `Perpetrator`. Hence 3,694 of 3,694.

The victim line's residual 4.06% is a real population and its cause is
confirmed rather than inferred: the count of victim children that **are** the
named attacker is **124**, exactly the 124 lossy victim steps. Those are
foundings where a community's own offshoot is what destroyed it.

**Under decision 0021 this matters beyond tidiness.** An asymmetry that makes
perpetrator lines forget their own violence faster than victim lines remember
it is precisely the kind of moral valence 0021 requires be *derived* rather
than handed out — and this one falls out of a data-structure choice, which is
the one provenance 0021 forbids.

## 4. Where the code lives

| file | role |
|---|---|
| `windows/hearsay/src/lineage.rs` | the founding tree; `descendants_with_hops`, `ancestry`, `is_ancestor` |
| `windows/hearsay/src/derive.rs` | `witnesses_of`, `claims_about`, `variants_about`, `variants_about_accumulating` |
| `windows/hearsay/src/stance.rs` | `Stance`, `stance_of`, `is_lossy` — §5.2's subject |
| `windows/hearsay/src/amplitude.rs` | `gen_span` — read, never changed (§2) |
| `windows/hearsay/src/accumulate.rs` | `Accumulation`, `precision_at` — read, never changed (§2) |
| `windows/hearsay/tests/probe_contact_substrate.rs` | §3, already committed |

`windows/hearsay` is a **window**: it reads a committed ledger and derives.
Nothing in this campaign draws, mutates world state, or adds a domain
dependency. The library's runtime deps stay `hornvale-kernel`,
`hornvale-history`, `hornvale-astronomy`.

## 5. The derivation

### 5.1 The clock

A community may hold a claim about an event at day `e` only if it had not
already ended: `occ-ended` absent, or `occ-ended > e`. A transmission step is
refused when the hearer fails that test, which also breaks the chain beyond it.

Derived, with nothing to tune: the two quantities are `occ-ended` on the holder
and the event day already read by `witnesses_of`. Exact comparison, matching
the existing exact-equality day discipline in `witnesses_of`.

**This is a strictly-removing change on the descent graph** — it can only
delete holders, never add them, which makes it separately measurable from §5.3
(strictly adding) with no possibility of the two cancelling.

### 5.2 The stance geometry, as two arms

```rust
pub enum Perpetration {
    /// Ships today: only the `occ-ended-by` entity itself.
    Singleton,
    /// `VictimLine`'s mirror: the attacker or any of its descendants.
    Inherited,
}
```

`stance_of` takes a `Perpetration`. `Singleton` reproduces today's behaviour
byte for byte; `Inherited` closes `Perpetrator` under descent exactly as
`VictimLine` is closed.

**Neither is nominated, and there is deliberately no `Default` impl** — the
same posture `Accumulation` takes and for the same reason (§6.4 of The
Palimpsest): the substrate for both was measured before either was chosen, so
picking a favourite now would be selection on data already in hand. Both are
reported. Adopting one is a separate dated decision citing this campaign's
numbers.

The choice is not merely technical. `Singleton` says guilt does not inherit and
a raider's grandchildren are bystanders to their grandparent's raid;
`Inherited` says the deed stays the line's own. Both are defensible readings of
a world without an alignment axis, which is exactly why this campaign measures
rather than asserts.

### 5.3 The contact edge

The transmission graph gains a **horizontal** edge, undirected, for every
ending that names an `Entity`-valued `occ-ended-by`: between the victim and its
named attacker, stamped with the ending's day.

A claim about an event at day `e` may traverse a contact edge stamped `c` when:

1. `e <= c` — a meeting cannot carry news of something that has not happened;
2. the receiving party satisfies §5.1's aliveness test.

Both conditions are read from the ledger; neither has a threshold.

**Undirected, and that is a freeze, not a discovery.** The ledger records one
event both parties attended; asserting that news flows only one way across it
would be authoring. Undirected is also the *ceiling*, so a directed variant is
a restriction measurable against this campaign's numbers later. Directed
variants are carried forward (§7).

**Cost of a crossing is the same rule as a descent step** — `gen_span(teller,
hearer)` under the campaign's accumulation rule, unchanged. Charging a contact
step extra would be a second free parameter introduced in the same campaign as
the edge, and would make the readout unattributable.

### 5.4 Two choices that were free and are now load-bearing

`variants_about_accumulating`'s own doc comment predicted this campaign:

> That is equivalent to "the teller's people's ladder" only for as long as a
> transmission path never crosses a people boundary, which is exactly the
> invariant §7 of the design expects a later campaign to break.

Both are **frozen at their current values**, deliberately, so the edge is the
only thing that varies:

- **The amplitude reads the TELLER's generation length** (`amplitude.rs`). The
  Palimpsest pinned this with an artificial test precisely for this moment.
- **The emit ladder is the ORIGINATING WITNESS's, fixed once per path.** The
  alternative — the current teller's ladder — is carried forward (§7).

Changing either while adding the edge would confound the campaign's headline.

### 5.5 What a cross-people path does to path uniqueness

A contact edge makes the transmission graph **cyclic**, and every shipped model
assumes it is not: `variants_about` recovers *the* path by slicing
`ancestry(d)[..=pos]`, unique because the founding tree is single-parent.

The existing multi-path tie-break already states the semantics — a holder keeps
the least-corrupted telling — so the *rule* survives; the *enumeration* does
not. The walk becomes a best-first search over the augmented graph, relaxing
each node by the same ordering key already in use (smallest width, then fewest
hops, then smallest witness `EntityId`). Cycles terminate because the key's
first component is non-decreasing along any path (`Accumulation::step` is
non-decreasing for every rule and non-negative span, which
`accumulate.rs` already documents and tests).

**This is the campaign's one real correctness risk** and it gets an explicit
test: an account that leaves its lineage, crosses to another people, and
returns to a descendant of its own witness — arriving damaged, by a route no
tree admits.

## 6. Preregistration

Frozen before the code in §5 exists. A falsified prediction is a finding, not a
failure.

### 6.1 Disclosure

The controller of this campaign **read The Palimpsest's exploratory readout**
(the corrected-units column) while mapping the chronicle's headings, before any
hypothesis here was written, and disclosed it immediately. That contamination
bears on the *unit fix*, which §2 therefore excludes entirely and leaves frozen
for a fresh session. **It does not bear on anything in this spec:** nothing in
that table speaks to the transmission graph's topology, to stance geometry, or
to the clock. The substrate in §3 was measured by this campaign from scratch.

How the leak happened is recorded because the mechanism will recur: the freeze
named *spec sections*, but the payload also sat in an unmarked table inside the
*chronicle*, a file the handoff instructs you to read. A freeze that requires
the reader to know where the boundary is before reading anything cannot survive
an orientation pass. **Future freezes must put the frozen content in a separate
file that is not opened.**

### 6.2 H1 — the clock's blast radius exceeds its direct count

§3.5 found 1.19% of holders already dead. Refusing a step also orphans
everything below it.

**Prediction: the clock removes strictly MORE than 1.19% of holders.**
Confirmed if the measured removal exceeds 1.19%; falsified if it is at or below
— which would mean dead holders are overwhelmingly leaves, itself a finding
about the shape of the founding tree.

### 6.3 H2 — the raider's line retains coarser precision than the victim's

§3.6 measured the *mechanism* (100.00% vs 4.06% of first steps lossy). H2 is
about the *observable consequence*, which has never been measured.

**Prediction: under `Singleton`, for the same event, the median retained
precision rung of raider-people holders is strictly coarser than that of
victim-people holders; under `Inherited` the gap narrows.** Reported as the
median rung gap per arm.

Falsified if the gap is absent or reversed under `Singleton` — which would mean
the 100% lossy first step is absorbed downstream and the geometry is cosmetic.

### 6.4 H3 — contact produces accounts that no tree can produce

**Prediction: with contact edges, at least 1% of endings carry an account held
by three or more peoples** (§3.3's gated walk says 1.76%; the model adds
filters the raw walk lacks, so this is a prediction, not a restatement).

**Seed-level positive control, which is the sharper half.** Seed 2 has **zero**
cross-people attackers and therefore exactly **zero** cross-people accounts
under descent. Prediction: **under contact, seed 2 carries more than zero.** A
seed where the mechanism is absent at hop 0 but present as a graph is the
cleanest available discriminator between "contact works" and "contact
re-describes co-witnessing".

### 6.5 H4 — contact raises genuine two-sided disagreement

Baseline, §3.4: **19** mutually-exclusive events per 12 seeds.

**Prediction: contact raises that count by more than 3×**, because a derivative
account arrives already damaged rather than starting from an eyewitness's
finest rung.

### 6.6 The null detector, named in advance

Contact chains are longer, and damage only accumulates. **The live null is that
everything saturates to the coarsest rung, and accounts that are all equally
vague become identical again — so divergence FALLS as reach rises.** The
Palimpsest predicted total saturation and two of its three rules did not show
it, so this is not a rhetorical hedge.

**Saturated fraction is reported for descent and contact side by side, per
accumulation rule.** If H4 fails this way, the campaign reports the mechanism
working and the phenomenon vanishing, and does not retune anything to rescue it.

### 6.7 What is asserted versus reported

The heavy battery **asserts only substrate controls**: that the panel built,
that held claims exist, that no claim reports a rung its own ladder lacks, that
the `descent` arm reproduces today's numbers, and that a strictly-removing
change never adds a holder. **Every hypothesis above is REPORTED.** This file
and that battery must never be edited to rescue a prediction.

## 7. Carried forward

- **Directed contact** — victim→raider and raider→victim as restrictions of
  §5.3's undirected edge, measurable against this campaign's numbers.
- **The current teller's ladder** at emit, against §5.4's originating-witness
  freeze.
- **`KNOW-signed-amplitude`** — unblocked by §5.4 making the teller pin
  load-bearing, still held back from this campaign.
- **`KNOW-lectio-difficilior`** — contact is stemmatic contamination, which the
  literature says breaks tree reconstruction. Harder now, and more interesting.
- **`KNOW-misattribution-drift`** — across a people boundary, subject drift is
  *racecraft*: the raid we remember happening to us happened to someone else.
  Decision 0021 names that gap as the epistemic layer's most serious use.
- **Non-monotone distortion** — the clock in §5.1 establishes the second
  world-time clock campaign 2 wrongly said did not exist.
- **The unit fix** (The Palimpsest §6.6) — frozen, for a session that has not
  read §6.7.

## 8. Definition of done

- §5.1–§5.5 implemented, with the `descent` arm byte-identical to today's.
- The preregistered readout (§6) as a heavy battery over the 40-seed panel
  campaign 3 used, reporting every quantity per arm with no arm nominated.
- The §5.5 round-trip test: an account that leaves its lineage, crosses a
  people boundary, and returns damaged.
- Chronicle entry, `book/src/chronicle/the-parley.md`, and a Confidence
  Gradient re-score if this moves a bet (decision 0030).
- Retrospective, `docs/retrospectives/the-parley.md` (decision 0020).
- Idea-registry updates: `KNOW-contact-is-an-edge` resolved or amended;
  `KNOW-mismatch-needs-contact` amended again with §3.4's corrected baseline.
- `make sluice-stage` at each plan-stage boundary; `make sluice` to merge.

## 9. Open for Nathan at G3

1. **§5.2 is the one with a value judgement in it.** `Singleton` vs `Inherited`
   is "does guilt inherit". This spec measures both and nominates neither,
   which is the project's own discipline — but if you want the campaign to
   *adopt* one, say so now, because adopting after the readout is selection on
   data already seen.
2. **Scope.** Three layers is a heavyweight campaign. The clock (§5.1) alone is
   a defensible small one, and §5.2 alone is nearly free. Cutting is your call.
3. **§3.6 is arguably the campaign's real finding and it was free.** If you
   would rather ship it alone and immediately — as a defect fix with a
   two-arm measurement — the contact edge keeps.
