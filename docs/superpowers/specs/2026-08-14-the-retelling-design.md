# The Retelling — content learns to vary, and divergence gets a measure

**Status:** DRAFT, at G3 · **Campaign:** the-retelling (`campaign/the-retelling`) ·
**Program:** Myth (campaign 2 of 4) · **Depends on:** The Hearsay, merged
([spec](2026-08-13-the-hearsay-design.md),
[retrospective](https://github.com/hornvale/hornvale/blob/main/docs/retrospectives/the-hearsay.md))

Campaign 1 built myth's channel and then failed, three times, to measure
corroboration. The cause it eventually named is the premise of this campaign:
**corroboration is semantic and campaign 1 was structural.** It needs accounts
that *could* differ, and content was carried unchanged by construction — so
agreement was constant-true and every topological measure was standing in for
a property with no variance.

This campaign makes content vary. It does not measure corroboration.

## 1. What this campaign produces

1. **A transmission chain with two filters** — the communication model, not a
   single per-hop mutation: `producer -> productive filter -> receptive filter
   -> receiver`. What a teller *encodes* and what a hearer *decodes* are
   separate functions with separate keys.
2. **Filter keys derived from committed facts** — a teller's *incentive*
   (has this community ever raided?) and a hearer's *formation* (was it born
   of a catastrophe?). Both ledger-readable, both deterministic.
3. **A correct maximum-antichain measure of divergent structure** (§5.3),
   named *divergent structure* and never *corroboration*, per
   `KNOW-divergence-antichain`.
4. **A preregistered measurement (§6)** comparing recognisable reach against
   campaign 1's no-decay baseline, and variant multiplicity against the
   antichain that is supposed to predict it.

## 2. Non-goals

- **Corroboration as an event.** Someone from B meeting someone from C and the
  tale holding up needs contact and a notion of belief strength. Neither
  exists. This campaign ships the *precondition* and measures it; the event is
  campaign 3 at the earliest, and probably needs contact first.
- **Prejudice, ingroup/outgroup, meta-ethnic frontiers.** `SOC-intramural-
  violence` measured the world not to generate the cause (92.7% of raids
  within one people). Decision 0021 requires derivation, never authoring. The
  campaign that unlocks it is **contact**, and §3.1 below now supplies the
  mechanism for why: divergence needs mismatched filters, mismatch needs
  boundary-crossing, and boundary-crossing is contact.
- **Diffusion — a claim arriving by two routes.** Still campaign 3. This
  campaign varies *content along the existing tree*; it adds no edges.
- **Species-keyed filters.** Falsified before design; see §3.2.
- **Persons as holders.** Persons exist (`domains/person`), and §7 keeps the
  row, but individual-level filters are the axis campaign 3 varies.
- **Any new seeded draw.** Both filters are deterministic functions of
  committed facts. No `streams.rs` label, no save-format contract, no epoch,
  and `windows/hearsay` stays a window rather than becoming a domain.
- **Any change to `Fact`, the ledger, or the concept registry.** Myth reads
  fact; fact never reads myth (0100 rule 1).

## 3. Substrate, measured before anything was designed

Reported as substrate, not outcome — the precedent is The Hearsay spec §6.1.
Two throwaway probes on seed 42, each reproducing campaign 1's published
counts as positive controls, because **a zero needs a control**.

### 3.1 Fission never crosses a people boundary

```
INHERITANCE  edges=658  typed=658  crossing=0   (0.0000 of typed)
WITNESS      endings=474  with_foreign_witness=17  (0.0359)
             non-subject witness pairs=711  crossing=17
PEOPLES      distinct=15  (hobgoblin 283, gnoll 171, kobold 139 = 84% of 704)
```

Positive controls: `edges=658` reproduces the chronicle's "658 descended from
a parent"; `endings=474` reproduces spec §6.1's ending count. The zero is a
real zero.

**The consequence beyond this campaign.** Accounts can only diverge where the
two filters are *mismatched*; filters are mismatched where a claim crosses a
group boundary; boundary-crossing is contact. So campaign 1's unmeasurable
corroboration and the blocked prejudice campaign are **one scarcity, not
two** — the world is short of contact, and both symptoms follow. That is a
tighter statement of "contact must precede prejudice" than the chronicle
carries, and it arrives with a mechanism. Recorded as
`KNOW-mismatch-needs-contact`.

### 3.2 Which is why the filters may not key on species

A filter keyed on `occ-people` is **inert on 100% of inheritance edges** and
fires only at the attacker seam, on 17 of 474 endings. That is a measurement
requiring a mechanism the campaign has excluded — the scope error The Hearsay
spec §5 generalised, in a new costume. Seventeen events on one seed is also
far too thin a denominator to carry a preregistered fraction.

### 3.3 Role and history do vary

```
RAIDERS             230 of 704 (0.3267); peoples holding BOTH raiders and
                    non-raiders = 8 of 15
SURVIVORS           477 of 704 (0.6776); edges with an ended parent = 562,
                    parent never ended = 96
DIVERGENCE-CAPABLE  361 of 408 qualifying endings (0.8848) have >= 2 distinct
                    witness keys | histogram {1 key: 47, 2: 308, 3: 53}
```

Positive controls: `477` and `562` reproduce spec §6.1 exactly. The raider key
varies *within* a people in 8 of 15 peoples — the thing species-keying
structurally cannot do.

**Disclosure, and it binds §6.** `0.8848` was computed on seed 42 *before*
any hypothesis was frozen. It is therefore substrate, and **no hypothesis in
this campaign may set a threshold against it** — that is exactly H5's defect
in campaign 1, where a seed-42 exploratory pass supplied the 0.50 bar it was
then tested against. Divergence-capability is consequently *unavailable* as
this campaign's headline.

### 3.4 The heavy-tier cost claim is stale by two orders of magnitude

`hop_depth_seed42.rs` is `#[ignore]`d as a "live-worldgen battery (minutes)"
and `windows/hearsay/tests/common/mod.rs` says a Settlements-depth build "is
minutes". Measured on the real subject at `c9fb7701`: **4.31 s** (7.34 s wall
including the build), reproducing every published number. So campaign 1's
preregistered readouts are deferred out of the commit gate for a cost that
does not exist, and a regression in them is currently invisible. §8 moves
them.

**And the staleness is enforced, not merely un-updated.**
`cli/tests/heavy_tier.rs:123` asserts every heavy-tier ignore reason is
*verbatim* `"heavy: live-worldgen battery (minutes); deferred from the commit
gate to make gate-full"`. A new heavy test therefore cannot state its real
cost without first changing the canonical string — this campaign's own second
probe went red for saying something true, and is committed carrying the false
clause with a comment saying so. A ratchet that freezes a measurement is
working exactly as designed and pointed at the wrong thing.

**And the merge of The Staff made it worse rather than fixing it.** Decisions
0132/0133 turned `make gate-full` into a refusing signpost that exits non-zero,
but did not sweep `cli/tests/heavy_tier.rs:64`. So the canonical reason string
every heavy test must match *verbatim* now carries TWO falsehoods: a cost claim
wrong by two orders of magnitude, and a make target that refuses to run. The
enforcement test is still green, because every heavy test repeats the same
wrong string consistently — which is precisely what a verbatim ratchet
guarantees and precisely why it cannot notice. §8 fixes both clauses at once.

## 4. Where the code lives

**`windows/hearsay`, entirely.** The filters read committed facts and draw
nothing, so the window contract holds unchanged and no `streams.rs` label is
owed. Nothing in the kernel changes except one added method on `Claim`
(§5.2) — `Claim` is derived and never serialized, so it remains outside the
save-format surface and triggers no epoch.

This is the load-bearing reason the model is **deterministic**. A stochastic
per-hop rule would be a *draw*; `windows/CLAUDE.md` is explicit that a window
that draws "has quietly become a domain with no registry entry and no
pin-isolation test," and it would owe a label, a consumption-order contract,
and an epoch. Architecture decides this before realism does.

## 5. The derivation

### 5.1 The two filters, and what they key on

A retelling passes `producer -> productive filter -> receptive filter ->
receiver`. What a teller encodes and what a hearer decodes are separate
functions — but they must be **two values of ONE axis**, not two unlike
booleans, and getting that wrong is the error this section records.

**The axis is stance: where a community stands relative to the event the
claim is about.**

```
stance(who, claim):
    PERPETRATOR   who is the claim subject's occ-ended-by
    VICTIM-LINE   who IS the subject, or descends from it
    BYSTANDER     neither

is_lossy(teller, hearer, claim) = stance(teller, claim) != stance(hearer, claim)
```

**The rejected draft, and why it was wrong.** An earlier version keyed the
productive filter on *"has this community ever raided"* and the receptive
filter on *"was this community born of a catastrophe"*, then compared them with
`!=`. Two defects, one structural and one measured:

1. **It compared incommensurable predicates.** Those are not two readings of a
   single axis, so `!=` between them means nothing — the same species of type
   error as campaign 1's, where a *symmetric* concept (corroboration) was
   pushed through an *antisymmetric* relation (ancestry).
2. **It ignored the claim entirely.** Being a raider was a standing property of
   a community, so a raider distorted *every* account he ever passed on —
   foundings, weather, anything — not merely accounts of raids he committed.

Measured on seed 42 over all 658 inheritance edges, that predicate marked the
largest quadrant in the world **frictionless**: teller-raids x
hearer-born-of-catastrophe is 254 edges (38.6%), the perpetrator telling the
community that fled him, and `!=` excluded exactly it.

**Stance fixes both.** It is one axis, so the comparison is meaningful; it
takes the claim, so a community's stance is relative to *this* event; and it
fires on a peaceful world, because a founding still separates the founders from
everyone else.

**Measured distribution**, over the population the predicate actually applies
to — edges that carry the claim, i.e. inside the witness's subtree:

```
5,650 carrying (edge x event) pairs; lossy = 698 (12.4%)
  Perpetrator -> VictimLine  :    46
  VictimLine  -> Perpetrator :    12
  VictimLine  -> VictimLine  : 4,952   (matched: the overwhelming case)
  Bystander   -> Perpetrator :   211
  Bystander   -> VictimLine  :   429
```

**A denominator warning, because the first run of this probe got it wrong.**
Counting every edge against every event gives 0.3% lossy and reads as a dead
metric. That population is wrong: an edge outside the witness subtree carries
nothing, so 305,974 of those pairs are transmissions that never happen. The
correct denominator is carrying pairs, and the same mistake in a different
costume killed the species-keyed draft.

**Consequence to expect, stated before the readout.** 12.4% is well below the
rejected rule's 38.6%, so claims coarsen more slowly and H1's bend will be
gentler than campaign 1's baseline might suggest. That is not a weakness of
stance — the higher rate came partly from firing on edges with no relation to
the event — but it is said here so a shallow curve is not later mistaken for a
null.

**Cost, measured in node visits rather than seconds** (`Instant` is banned by
the wall-clock rule, and a step count is deterministic where a duration is
not):

```
naive ancestry per (holder, event) : 2,946,813 visits  (8.8 per pair)
memoised per EVENT                 :   333,696         (  8.8x)
memoised per NODE                  :     6,221         (473.7x)
```

Per-node memoisation is what ships: each occupation's ancestor set built once
in `Lineage`, after which a stance read is an `O(log n)` membership test. At
6,221 visits for a whole world the cost is not worth managing — it is worth
removing.

### 5.2 What distortion does to content

`Claim.object` on the measured predicate `occ-ended` is a `Value::Number` — a
day. Distortion coarsens it one rung per lossy retelling, never reversing.

**The rungs are the world's own cycles, not a calendar.** They are read from
committed astronomy facts — `day-length-std`, `moon-period-std` (once per
moon), `year-length-std` — and sorted by their actual spans, so the ORDER is
world-derived: whether a moon sits coarser or finer than a season depends on
that world's sky. A moonless world has no lunar rung; a tidally-locked world
has no day rung; a ladder is allowed to be short, and an empty one loses no
precision at all. **Both moons of a two-mooned world are rungs**, because two
irreconcilable lunar reckonings are the phenomenon rather than noise to be
averaged away.

**The rungs deliberately do not nest, and this is the campaign's sharpest
choice.** Real cycles are incommensurable — a synodic month does not divide a
year, which is why intercalation exists — and forcing them to nest would erase
exactly the thing worth simulating: a sky that disregards the best-laid
theories of its inhabitants. Two consequences follow, both wanted:

1. Each teller re-rounds an **already-rounded** day, so error compounds and a
   claim can name an interval that no longer contains the event it describes.
   Traced and pinned in `kernel/src/precision.rs`'s tests: a witness to day 745
   tells it at the greater-moon rung (708.9); the hearer re-rounds *that* to
   the year rung (372.4), an interval `[372.4, 744.8)` excluding day 745, where
   rounding the truth directly would have given `[744.8, 1117.2)`. **A rumour
   becomes false**, not merely vague — which is what makes the harder-reading
   question worth asking at all.
2. The invariant is therefore **precision-rank monotonicity** — the rung index
   only ever rises — and *not* any statement about error. An earlier draft
   demanded that coarsening never reduce error, which treats a claim as a point
   that moves. It is an **interval that widens**: "sometime that year" is
   strictly less informative than "on that day" even when its representative
   value lands nearer the truth.

**Where the code lives, and why the split is not cosmetic.** `Precision` is a
bare rung *index* in the kernel; the ladder, its spans and all day arithmetic
live in `windows/hearsay`. A duration at a `pub` boundary wants the typed
quantity `StdDays` (design principle 5), `StdDays` lives in
`domains/astronomy`, and the kernel may not depend on a domain — so the kernel
cannot hold spans without either lying about units or breaking layering.
`type-audit` is what surfaced this, by refusing a bare `f64` day.

`Claim::inherited_by` is left untouched so campaign 1's tests keep their
meaning, and gains two siblings: a frictionless retelling (pinned equal to it
by test) and a lossy one taking an already-coarsened value, so the kernel never
performs day arithmetic.

**The rung set is derived and the lossy RULE is authored, and that split is the
point.** A model's functional form is always authored; what decision 0021's
discipline governs is its *inputs*. Nothing here reads a parameter the world
was handed — the rungs are that world's astronomy and both filter keys are
consequences of what a community did.

**Deliberately not built here:** the residual itself — how badly a world's
cycles fail to close — as a driver of ritual and eschatology. Recorded as
`SOC-incommensurable-sky`, and see §7.

### 5.3 Divergent structure is a maximum antichain

Campaign 1 computed the wrong extremum. For witnesses `{A, B, C}` with `B` and
`C` survivors of `A`, the *minimal elements* are `{A}` — size 1, scoring the
motivating scenario zero — while the **maximum antichain** is `{B, C}`.

In a forest poset the maximum antichain of a witness set `S` is exactly **the
elements of `S` with no strict `S`-descendant**. It is an antichain (if `x`
were an ancestor of `y`, `y` would be an `S`-descendant of `x`); and it is
maximum, because any antichain `A ⊆ S` maps injectively into it by sending
each `a` to a deepest `S`-descendant of `a` — injectively because incomparable
elements of a tree have disjoint descendant sets. That is `O(|S|^2)` and
needs no matching algorithm.

## 6. Preregistration

Frozen here, before the code that would move it (decision 0016). Study JSON
carries no hypothesis field, so this section is the freeze. Population: every
(event, holder) pair the derivation produces, across the census seed set.

**Known at the time of writing:** everything in §3, plus campaign 1's full
baseline — `pairs=7778 median=4 tail_ge_10=0.1535 max=21`, echo `0.2727` over
408 endings, divergent `0.4632`. **Not computed at the time of writing:**
every quantity below.

### H1 — the no-decay line bends

Campaign 1's pair counts decline **almost linearly** with hops, which is the
fingerprint of nothing being lost. With lossy retellings, counts of *claims
still at the finest precision* should decline **geometrically**.

Measure: the ratio of successive per-hop pair counts, `n(k+1)/n(k)`, over
`k` in `1..=8`, and its variance.

```
  ratio roughly constant across k (variance < 0.02)
        -> H1 CONFIRMED. Geometric decay; report the per-hop survival rate,
           which is the first number campaign 3's contact model must move.
  ratio rising with k (later hops lose LESS)
        -> H1 REFUTED, and this is the headline: matched filters cluster
           down-tree, so a story that survives five retellings is SAFER than
           one that survived two. Say so; it inverts the intuition the
           baseline was built on.
  ratio falling with k
        -> H1 REFUTED; decay accelerates, and the precision ladder is exhausting
           before the tree does. Report the exhaustion hop.
  fewer than 500 pairs at the finest precision -> NO VERDICT; report the count.
```

**Honest bound.** That *some* bend exists is close to analytic once a lossy
step exists at all. What is not analytic, and what H1 actually tests, is the
**shape** — whether the per-hop rate is constant, and where it bites. The
ceiling is stated so the floor means something: the ladder has five rungs, so
no event can show more than 5 distinct precisions, and a result at exactly 5
means the ladder saturated and the measure is reporting my constant, not the
world.

### H2 — distortion manufactures multiplicity

Distinct surviving `(precision, value)` variants per qualifying ending (>= 3
holders), median over the seed set. **Prediction: median >= 2.**

The null is that the tree's homogeneity swallows the filters — most events
end with one variant because most paths are frictionless — which would mean
distortion alone does not produce the divergence corroboration needs, and
campaign 3 must bring contact rather than refine the filters.

```
  median >= 3   -> H2 CONFIRMED strongly. Accounts routinely differ; the
                   precondition is a background condition of the world.
  median == 2   -> H2 CONFIRMED at the bar. Report the full distribution.
  median == 1   -> H2 REFUTED, and this is the headline: content varies in
                   principle and not in practice. Contact, not filters, is
                   the missing mechanism, and that is the campaign-3 finding.
  fewer than 100 qualifying endings -> NO VERDICT.
```

### H3 — does divergent structure predict semantic divergence?

`KNOW-divergence-antichain` asserts the maximum antichain is the *right*
precondition for corroboration. That is a testable claim and nobody has
tested it. Measure Spearman's rho between an ending's maximum-antichain width
(§5.3) and its distinct-variant count (H2), over qualifying endings.

```
  rho >= 0.5   -> H3 CONFIRMED. The antichain earns its name: structure
                  predicts semantics, and campaign 3 may use the cheap
                  structural measure as a proxy for the expensive one.
  0.2 <= rho   -> H3 REFUTED but informative; the antichain is a weak proxy.
       < 0.5     Report both and recommend the direct measure.
  rho < 0.2    -> H3 REFUTED, and this is the headline: the structural
                  precondition does NOT predict whether accounts differ, so
                  KNOW-divergence-antichain is measuring the wrong object —
                  the fourth failed operationalisation of corroboration, and
                  the most useful, because it fails against ground truth
                  rather than against a scenario.
  fewer than 100 qualifying endings -> NO VERDICT.
```

**Do not retune the precision ladder to move any of these.** If a result is
uncomfortable, the result is the finding.

## 7. Carried forward

- **Blind reconstruction — `KNOW-lectio-difficilior` re-operationalised.** Not
  *do variants get simpler* (analytic, since this campaign authors the rule),
  but *does preferring the harder reading recover the archetype better than
  majority rule*, scored against the committed fact the reconstructor never
  sees. `LANG-8`'s shape, with a null campaign 1 sharpened: majority voting
  misleads when copies are related, which is why *eliminatio codicum
  descriptorum* exists. Needs H2 to confirm first — with one variant per event
  there is nothing to reconstruct.
- **Misattribution — drift in `subject`, not `object`.** `Claim` has six
  fields and every discussion so far has drifted only `object`. A claim whose
  *subject* drifts is *the raid we remember happened to our neighbours*:
  richer, exactly scoreable against ground truth, and needing no numeric noise
  scale. Recorded as `KNOW-misattribution-drift`.
- **Filter width — species, incentive, individual.** The axis campaign 3
  varies, against the curve this campaign freezes.
- **Oscillating distortion** — regularise, then re-embellish. Unreachable:
  `Claim` carries no time, so `hops` is the only clock and distortion is
  necessarily monotone-on-retelling.

## 8. Definition of done

- The two filters and `Claim::retold_by` in `windows/hearsay` / the kernel,
  `#![warn(missing_docs)]`-clean, `type-audit:` tags on every pub-boundary
  primitive, and the type-audit report regenerated **in the same commit**.
- Maximum-antichain divergent structure, with a test that builds the
  `{A, B, C}` scenario by hand and asserts the answer is `{B, C}` and not
  `{A}` — the case campaign 1 got backwards.
- The §6 readouts, reported against their decision tables including NO VERDICT.
- **Campaign 1's battery moved into the commit gate** (§3.4), its ignore
  reason deleted rather than reworded, plus the two probes promoted or
  discarded on the same evidence.
- Chronicle entry, freshness sweep, Confidence Gradient re-score if moved,
  retrospective, registry rows landed with resolving **Where** cells.
- Census refresh on lefford at the pre-merge close if a metric is registered.

## 9. Open for Nathan at G3

**The scope of the filters is a fidelity call, and therefore not
auto-resolved.** The chain model invites keys on species, incentive, *and*
individuals. This spec ships the two narrowest keys and defers the rest. The
argument for narrow: campaign 1's value came from freezing a clean baseline,
and a campaign that varies three filter families at once measures none of
them. The argument against: it leaves the model visibly thinner than the
diagram that motivated it. Recommendation is narrow; the call is yours.
