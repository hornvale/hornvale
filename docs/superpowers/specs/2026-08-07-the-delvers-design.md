# The Delvers — C2c design

Dwarves ×5: Hill, Mountain, Duergar, Gully, Desert. The first people to be
authored into the Deep Realm, and the first occupant of the lifespan channel
The Long Age shipped empty.

Programme context: `2026-08-03-the-peoples-program-design.md` §4 (C2c), §5.
Immediate predecessors: `2026-08-06-the-warren-design.md` (the realm gate this
campaign uses), `2026-08-06-the-long-age-design.md` (the schedule it occupies).

---

## 1. What was measured before anything was designed

The Warren's retrospective states the habit this campaign owes it: *"before
refining the details of a specified campaign, check that its central noun is
expressible."* C2c has four central nouns — *underground*, *deeper*, *arid*,
*high* — and they do not all survive the check.

### 1.1 Three of the four condition axes are authored and silent

`tolerance_liebig` (`windows/worldgen/src/lib.rs:1051`) is the whole tolerance
model:

```rust
cn.temperature.eval(s.temperature_c, floor_buf)
  .min(cn.moisture.eval(s.moisture, floor_buf))
  .min(cn.insolation.eval(s.insolation, floor_buf))
  .min(cn.elevation.eval(s.height_asl_m.get(), 0.0))
```

Temperature, moisture and insolation are floored by
`sovereignty_floor(mass, potency)`; elevation is passed a literal `0.0`. The
function's own doc records the consequence and The Tilth's measurement of it:
*"a wide low-`devotion` elevation curve evaluates to a near-constant below every
other axis's floor, so **elevation binds on 100% of land for goblin, gnoll and
human**."*

Dwarves sit in those kinds' mass class at `potency = 0.0`, so the same result is
expected to follow. **This is an inference from a neighbouring measurement, not
a measurement**, and Task 1 exists to make it one before any trait value is
authored.

### 1.2 A chamber is at the elevation of the ground above it

`subterranean_substrate` (`windows/worldgen/src/lib.rs:2189`) swaps `moisture`
to `SUBTERRANEAN_MOISTURE` and `insolation` to `0.0`. `temperature_c` and
`height_asl_m` pass through unchanged — The Deep Realm's deliberate choice,
recorded there as out of its scope because a real depth coordinate needs a
coordinate the world does not have.

Combined with §1.1: going underground moves only axes that cannot bind. This is
The Warren's shipped finding, pinned by two tripwires, and this campaign does
not touch it.

### 1.3 The chamber graph's edges have never been walked

Verified by grep at `765fca29`, not inherited:

```
passages_from        windows/worldgen/src/chamber.rs:394
  non-test consumers: 0   (only its own doc and deep_realm_chamber.rs)
ChamberOverrides     windows/worldgen/src/chamber.rs:283
  consumers: 2, both `ChamberOverrides::new()` — an always-empty map.
  No writer exists anywhere in the workspace.
```

And the claim inherited from the session brief is confirmed: **no `(cell,
chamber)` or `(cell, stratum)` keying exists anywhere.** `CellMap<T>`
(`kernel/src/geosphere.rs:47`) is a dense `Vec` indexed by `CellId` with no
generic key parameter, so nothing *can* be keyed by a sub-cell address without a
new type. `CarryingInput` (`domains/demography/src/carrying_capacity.rs:14`)
carries six scalars — `is_land`, `temperature_c`, `precip_mm_yr`, `freshwater`,
`coastal`, `hostility` — and no depth, chamber, stratum or realm.

### 1.4 The census cost in the programme spec is wrong by a factor of thirty

The metaplan (§5, §8) costs a roster campaign at *"31 rows in `the-census` and 3
in `census-of-the-meeting`"*. Measured from the actual regen commits:

```
  regen commit          the-census    census-of-the-meeting
  36824412  (gnoll)     1000 / 1000            34
  74a7827d  (human)     1000 / 1000             2
  4731b926  (latest)    1000 / 1000     (file untouched)
```

A new settling people re-decides settlement placement on **every** seed, so
`the-census` refreshes wholesale. `census-of-the-meeting` is near-immune
*structurally*, not incidentally: its rosters are `goblin-solo` and
`goblin-twin-solo` (`windows/lab/src/roster.rs:96,102`), so a new kind never
competes in it. Its handful of moved rows are collateral from shared
concept-registry movement.

The wrong figure has been copied into three further documents
(`2026-08-03-the-generalist-design.md:339`, `2026-08-03-the-generalist.md:725`,
`2026-08-06-the-long-age-design.md:352`). Correcting the metaplan is in scope
here; the two closed campaigns' documents are left as they shipped.

---

## 2. What this is not

**This campaign does not build chamber occupancy, and that is a decision rather
than an omission.** The metaplan's C2a paragraph promised population keyed
`(cell, stratum)`; decision 0105 replaced bands with an addressed chamber graph
and the keying never arrived. C2c is the campaign that inherits the bill and
**declines it, explicitly**, for three reasons:

1. **The roster does not need it.** The cave gate at
   `windows/worldgen/src/lib.rs:1256-1272` already confines a subterranean kind
   to the ~12% of land with caves. Nothing about authoring five dwarves is
   blocked by the absence of chamber-keyed population.
2. **Occupancy cannot be built without traversal** (§1.3). `passages_from` has
   no shipped consumer, so a campaign that places a people in a chamber is also
   the campaign that first walks the graph's edges. That is The Deep Realm's
   unshipped second half — a mechanism campaign.
3. **It would confound the roster's census.** Programme spec §6 refuses to
   bundle goblin's re-characterisation into a roster epoch precisely because
   *"moving goblin's vectors inside a roster epoch makes every census movement
   unattributable, and attribution is this program's entire product."* The same
   argument binds here.

So: **"a dwarf lives underground" means its cell holds a cave and its capacity
is gated on that.** It does not mean it occupies a chamber. The chapter says so
in those words. `chamber.rs:304` currently names C2c as "the digging campaign" —
that comment is corrected to point at the successor.

Also out of scope, each for a stated reason:

- **The two refused repairs.** Flooring the elevation axis (The Tilth stage 6.1)
  and switching on `tolerance_tiered` (The Tense §3.3, shadow mode) both
  relitigate a calibration two campaigns just made, and both would rescue a
  prediction after unblinding. Neither is touched.
- **Tuning cave prevalence or clustering.** `MAP-underworld-reachability` and
  `MAP-cave-depth-weld` are terrain's levers, calibrated by The Hollow against
  five preregistered criteria. Untouched.
- **The full MAP-69 relationship** — over/under commerce, autarky, chthonic
  emergence. That row's own campaign.
- **Half-dwarves.** `BIO-17`; a data-model campaign in a roster costume.

---

## 3. The design

### 3.1 The differentiation budget is not where the roster's names put it

Every authored slot on a kind falls into one of three states. Naming them is the
design:

```
  slot          state     consumer that reads it                    binds?
  ------------  --------  ----------------------------------------  ---------
  realm         speaking  per_species_suitability, cave gate        HARD 0/1
  elevation     speaking  tolerance_liebig, unfloored -> the min    YES
  resource vec  speaking  axis_supply / mineral|forage|prey fields  DOMINANT
  mass          speaking  allometry, sovereignty_floor              YES
  potency       speaking  sovereignty_floor                         YES (meta)
  schedule      speaking  lifespan, maturity, tempo, cascade regime YES
  social form   speaking  society, culture rungs, language envelope YES
  temperature   prepared  tolerance_liebig, floored                 NO
  moisture      prepared  tolerance_liebig, floored                 NO
  insolation    prepared  tolerance_liebig, floored                 NO
  depth         absent    nothing                                   n/a
```

*Prepared* is the organ-builder's word for a stop-knob that is engraved,
installed, and connected to no rank. It is the honest description of what an
authored moisture curve is at dwarf mass and potency.

The roster's five names land unevenly on this table. **Gully, Hill and Mountain
are elevation names** — the one condition axis that binds — so the roster is,
almost by accident, an elevation ladder that this model can express. **Desert is
a climate name**, and climate is prepared. **Duergar's differentiator is depth**,
and depth is absent.

The budget that remains, and where the real differentiation goes:
`ResourceVector` is the loudest channel in the model. `BIO-supply-drowns-niche`
records that supply magnitude spans orders of magnitude while tolerance is
bounded in `[0,1]` — read as a defect that is the row's complaint; read as a
lever it is this campaign's answer. A mining dwarf, a foraging dwarf and a
scavenging dwarf differ by orders of magnitude where a hot dwarf and a cold
dwarf differ by nothing.

### 3.2 The five, and what makes each one different

Trait values are authored in the plan against Task 1's measurement, not fixed
here — the metaplan §7 explains why a spec that pins numbers against a model it
has not yet measured goes stale. What is fixed here is **which axis carries each
kind's identity**, because that is the design and the rest is calibration.

```
  hill      Surface       mid elevation      forage/prey    Allometric
  mountain  Subterranean  high elevation     mineral        Paced
  duergar   Subterranean  high elevation     mineral        Paced
  gully     Surface       low elevation      detritus       Allometric
  desert    Surface       (no binding axis)  --             Allometric
```

Mountain and Duergar are separated in psyche, society and perception — that is
real and reaches language, culture and demography — but **not in capacity**.
Desert is separated in nothing that binds. Both facts are preregistered in §5
rather than discovered.

### 3.3 Dwarves are the first occupant of `LifeSchedule::Paced`

`LifeSchedule::Paced { factor }` (`domains/species/src/lib.rs:1748`) ships with
no occupant; `domains/species/tests/coverage.rs:295` pins it at
`Rung::Declared` with an empty witness list and names C2c as the campaign that
must edit it.

Mountain and Duergar take the factor. Two constraints govern the value:

- **`pace_of_life` and `reproductive_tempo` saturate at 1.0** once
  `factor × raw × pace_multiplier` exceeds `MAX_PACE_MULTIPLIER = 1.5`
  (`domains/species/src/allometry.rs:21`). Saturation is deliberate and stated
  (The Long Age §3.5), but it means those two channels are uninformative for a
  strongly-paced kind. `lifespan`, `age_at_maturity` and `generation_length`
  stay linear and unbounded.
- **`LIFESPAN_THRESHOLD_YEARS = 120.0`** (`windows/worldgen/src/lib.rs:5129`)
  is where `cascade_regime_of` switches a Settled people to the slow
  language-drift regime. A dwarf below it is long-lived in the almanac and
  ordinary in the model.

So the factor must clear 120 years to be *read*, and the campaign should say
plainly which side of the saturation ceiling it lands on.

**This closes The Long Age's one open code-reading gap for free.**
`generation_length_of` (`windows/worldgen/src/descent.rs:140`) resolves its
species by name from the canonical registry, so no mutation could observe it
while every row was `Allometric` — `descent_graph.rs:324-338` says so
explicitly. A `Paced` row in the canonical registry makes that consumer
observable for the first time, at no cost.

### 3.4 A dwarf family, and the first generalisation of the proto seam

`family_of` gains five rows all labelled `dwarf`. The moment the second lands,
`check_integrity` (`windows/worldgen/src/components.rs:322-331`) requires a
`family_proto` entry — today only goblinoid, draconic and plant have one.

That is the mandatory half. The half worth flagging is that
**`cli/src/proto.rs:25` hardcodes `FAMILY = "goblinoid"`**, and its own doc says:
*"the campaign's only multi-member family today… A second family would need
either a second page or this function generalized to take a family argument —
deferred until that need is real."* Dwarf makes it real. This campaign
generalises it and adds the second page.

`cli/src/dictionary.rs:104` already generalises — it loops `family_proto().ids()`
— so the dictionary gains a Dwarf cognates section for free on regeneration.
`windows/lab/src/metrics.rs:5920,5925` (`GOBLINOID_DAUGHTERS`, `ALL_DAUGHTERS`)
do **not** generalise, and the family-monophyly / inventory-closure / homophony
metrics will silently not measure dwarf unless extended. Extending them is in
scope; silently not measuring is the failure this programme exists to avoid.

### 3.5 The authoring cost is eight registries, not six

`BIO-kind-authoring-seam` says six registries plus a validation in a seventh.
That count predates The Tolerance and The Long Age. Measured against the `human`
and `gnoll` commits, a minded kind today needs:

```
  domains/species/src/lib.rs      biosphere, psyche, dispersion, society,
                                  perception, family_of, KIND_CONCEPTS,
                                  habitat_realm (sparse), + a condition-niche fn
  domains/language/src/lib.rs     articulation, lexicon, family_proto
  domains/language/src/accession.rs   an appended epoch cohort
  windows/worldgen/src/components.rs  check_integrity validates, authors nothing
```

The accession cohort is the one most easily missed and the one with teeth:
`cli/tests/accession.rs:37` reddens if it is skipped, and commit `ee4e6a00`
records that omitting it also *changed which proto-root the concept draws* —
ordering is load-bearing, not documentation. Append a cohort; never edit one.

**Landing order is not free.** Commit `6fef04fc` proves it: landing a biosphere
row alone makes `assemble()` hard-fail workspace-wide ("a Settled kind is
missing a peopled component"), so species and language must land in one commit.
The pre-commit hook runs `make quick` workspace-wide regardless of staged paths,
so this is not something a plan can split.

The row's count is corrected as part of this campaign's bookkeeping.

---

## 4. Blast radius

Beyond the authoring seam, adding five kinds moves a large set of pinned counts
and rosters. Enumerated from the `human` and `gnoll` diffs so the plan can
sequence them rather than discover them:

**Hard counts** — `domains/species/src/lib.rs:3060` (`bio.len() == 30`), `:3078`
(`psy.len() == 9`), `:3168` (the full 30-name lexicographic roster);
`domains/species/tests/coverage.rs:435` (`reg.len() == 30`), `:451`
(`habitat_realm_registry().len() == 2`);
`windows/worldgen/tests/dissolve_equivalence.rs:18,28,30,73-83`;
`windows/worldgen/tests/demesne.rs:344`.

**Authored coverage tables** — `coverage.rs` metabolic class, status basis,
activity cycle, social form, life schedule; all five move.

**Roster constants** — `social_form.rs:48,78`; `worldgen/src/lib.rs:8784-8796`;
`generalist_baseline.rs:121,127`; `tolerance_baseline.rs:129`;
`tolerance_mutation.rs:186`; `solitary_tongue.rs:438`; `metrics.rs:5920,5925`.

**Seed-42 pinned tables** re-measured when human landed and expected to move
again — `deep_grammar.rs:181-202`, `diachronic.rs` (six `LADDER_TABLE` blocks),
`descent_graph.rs:76`, `exposure.rs`, `history_emit.rs`, pantheon and name-gloss
counts in `worldgen/src/lib.rs`, three `metrics.rs` name-syllable pins,
`the_dial.rs`, five literal peoples-line assertions in `windows/book/src/lib.rs`,
and the vessel fixtures' `GRIEVANCE_NPC`.

**Byte goldens** (`REBASELINE=1`) — `cli/tests/fixtures/world-seed-42.json`,
`affect-trace-seed-42.txt`, the scene tiles/surrounds fixtures,
`session-seed-42.json`, `proto-goblinoid-root-table-seed-42.txt`,
`solitary-tongue-peoples-lexicons-seed-42.txt`, `occupancy.csv`.

**The ghost check** — `windows/worldgen/tests/non_void_roster.rs` requires each
new kind to reach `K >= hornvale_demography::FLOOR` on at least one cell of
every tested seed, **with no allowlist entry**. This is the `BIO-39` kobold
failure's guard and it is the sharpest risk for the two subterranean dwarves,
whose habitat is 12% of land before any tolerance applies.

---

## 5. Preregistration

Frozen before any trait value is authored. Each names its axis.

**P1 — Climate is silent for a dwarf.** Over the five authored kinds, on seed
42 and at least 25 further seeds, the Liebig-binding axis is `elevation` on
≥ 99% of land cells for every one of the five. *Predicted: confirmed.* If it is
refuted, §3.1's whole design premise is wrong and the trait values must be
re-authored. Measured by extending `niche_breadth_probe.rs`.

**P2 — Mountain and Duergar are one rank.** Their per-cell capacity fields are
identical to within `1e-12` on every land cell of every tested seed.
*Predicted: confirmed — this is a null, stated in advance.* The two kinds differ
only in an absent axis, so the model cannot tell them apart. If it is refuted,
something differentiates them that this campaign did not intend, and that is a
finding to chase before merge.

**P3 — Desert is indistinguishable from its nearest elevation-neighbour.**
Desert's capacity field correlates with Hill's above 0.99 across the tested
seeds, despite an authored arid niche. *Predicted: confirmed.* This is the
control Nathan chose: it converts `BIO-gnoll-desert` from a single anecdote into
a second, preregistered witness.

**P4 — The remaining three are genuinely distinct.** Gully, Hill and
Mountain produce pairwise capacity fields correlating **below** 0.95. *Predicted:
confirmed.* This is P2 and P3's control in the other direction — without it, a
distinctness probe that reports "these are the same" proves nothing, because it
has not been shown able to report "these are different."

**P5 — A paced dwarf is read, not merely stored.** `cascade_regime_of` returns
the slow regime for Mountain and Duergar and the settled regime for the other
three, and `generation_length_of` returns a longer value for the paced pair than
mass alone predicts. *Predicted: confirmed.*

**P6 — World identity moves.** Unlike The Warren, whose fauna re-scoring could
not reach the ledger, dwarves are *peopled*, and settlement genesis packs
peopled species. Seed 42's committed world is expected to differ. **The magnitude
is deliberately not predicted** — The Warren's retrospective records that
refusing to guess a magnitude is what kept a falsified prediction from acquiring
a number to defend.

P1 and P4 are the ones that would invalidate the design. P2 and P3 are nulls the
campaign expects and ships.

---

## 6. The mutation this campaign owes

The programme's shared acceptance criterion (metaplan §3): a green test proves
the code ran; only a mutation proves the axis is visible.

**M1 — the realm gate.** Flip Mountain's `habitat_realm_registry` row to
`Surface` and its capacity must become non-zero on cave-free land. Reddens
`non_void_roster` land counts.

**M2 — the schedule.** Revert Mountain's row to `LifeSchedule::Allometric` and
`cascade_regime_of` must return the fast regime. This is the mutation The Long
Age could not run, because no registry row was `Paced`.

**M3 — the distinctness probe must discriminate.** Give Duergar a materially
different elevation curve and P2's identity assertion must break. Without M3 the
probe could be reporting "identical" because it computes nothing — the vacuous-
guard failure The Benchmark recorded, where a check sampled the one cell in
which the bug was invisible.

M3 is the one that matters most and the one easiest to skip.

---

## 7. Definition of done

- All five kinds authored across the eight registries plus the accession cohort;
  `check_integrity` green; `non_void_roster` green with no allowlist entry.
- P1–P6 measured and reported, including the nulls.
- M1–M3 demonstrated.
- `cli/src/proto.rs` generalised beyond `goblinoid`; the second proto page added;
  `metrics.rs`'s daughter constants extended to dwarf.
- Epoch declared only if a derivation actually moved (0084).
- Census regen: **one run, on lefford, at the merged SHA** — authorized by
  Nathan 2026-08-06. Both fixtures refreshed; expect ~1000 rows in `the-census`
  and near-zero in `census-of-the-meeting` (§1.4).
- The Warren's two tripwires (`warren_readout.rs:310`,
  `deep_realm_rehome.rs:301`) still green; `warren_gate.rs`'s mirror unmoved.
- Book chapter + chronicle + freshness sweep (including the "six settling
  peoples" prose in `book/src/domains/species.md:38,45,267,271,412`) +
  retrospective + Confidence Gradient re-score.
- Metaplan corrections landed: the census cost (§1.4), the eight-registry count
  (§3.5), and `chamber.rs:304`'s stale pointer at C2c.

---

## 8. Flagged for review

**1. `subterranean_substrate` should carry a real elevation, and this campaign
does not do it.** Without it Mountain and Duergar are the same kind (P2). The
remedy — deriving `height_asl_m` from `Cave::deepest_band` instead of inheriting
the surface's — is **neither of the two refused repairs**: it does not floor an
axis and does not change the tolerance operator. It supplies missing information
to the axis that already binds, and it is a *fidelity correction*, since a
chamber under a 2000 m peak genuinely is not at 2000 m. It would redden both
Warren tripwires for a reason they did not anticipate (a substrate change, not a
tolerance change), and it would move world identity and the census. Recommended
as its own campaign, with P2's number as the evidence. **Nathan's call.**

**2. The programme's probe-validity ladder has no rung for Desert dwarf.** Its
moisture curve is expressible, read, and correctly coupled — it is *inaudible
against a louder rank*. Rungs 1–2 are authoring-side, rung 3 is
consumption-side; masking is neither. Same phenomenon as
`BIO-supply-drowns-niche`. Captured as a registry row rather than amending a
programme-level instrument from inside one of its campaigns.

**3. Five kinds in one epoch means no per-kind attribution.** Accepted at
Nathan's direction (one regen at close). Worth stating because programme spec §6
uses the opposite argument to refuse bundling goblin's characterisation.

**4. `non_void_roster` is the campaign's real risk.** Two kinds confined to 12%
of land must still clear `FLOOR` on every tested seed with no allowlist. If they
do not, the roster is authoring the `BIO-39` failure and the trait values need
re-authoring, not an allowlist entry.

---

## 9. Frontier bookkeeping

- `BIO-kind-authoring-seam` — correct the count to eight registries plus the
  accession cohort; name `dispersion_registry`, `LifeSchedule` and
  `habitat_realm_registry` as the three that arrived after the row was written.
- `BIO-three-probes` — the dwarf cell moves from *inexpressible* to measured;
  record which of its differentiators bound and which did not.
- `BIO-supply-drowns-niche` — third witness, and the first where the row is used
  as a lever rather than reported as a defect.
- `BIO-gnoll-desert` — second witness, preregistered (P3).
- `LANG-53` — a `dwarf` family with five members and a `family_proto` is the
  second multi-member family topology; repoint per the metaplan §9.
- **New row** — the missing ladder rung (§8.2). Mint a slug per decision 0026;
  scan the registry before generating.
- **New row** — chamber occupancy and traversal as the successor campaign
  (§2), naming `passages_from`'s zero consumers and `ChamberOverrides`'
  absent writer.
- `MAP-underworld-reachability` — record that C2c placed a people underground
  without resolving reachability, so "can a creature live there" and "can a
  walker get there" are now genuinely separable questions with a people in
  between.

---

## 10. Amendment — two G3 decisions reversed by Task 1's measurement

Task 1 ran before any trait value was authored, which is what made both of these
catchable. Both reverse a call Nathan made at G3 on a premise the measurement
falsified. Recorded as an amendment rather than an edit: the original reasoning
is part of the record.

### 10.1 What Task 1 actually measured

The closed form in §1.1 is **confirmed exactly**: every kind whose authored
`devotion_elev` sits below its `sovereignty_floor` is elevation-bound on
**100.00%** of land, on every seed. But the generalisation this spec drew from
it — that dwarves inherit the result because they share human's mass class — is
**refuted**:

```
  kind        mass    floor  dev_el  below?    s42       s7    s1234
  kobold      13.6   0.3078    0.95     no   43.72%   41.55%   51.45%
  goblin      18.1   0.3347    0.35     no  100.00%  100.00%   97.04%
  hobgoblin   74.8   0.4527    0.70     no   74.77%   77.32%   69.26%
  bugbear    132.0   0.4933    0.70     no   72.89%   78.12%   71.40%
  gnoll      136.1   0.4954    0.40    YES  100.00%  100.00%  100.00%
  human       70.0   0.4477    0.30    YES  100.00%  100.00%  100.00%
```

Hobgoblin is 74.8 kg — human's mass class — and is elevation-bound on 74.77% of
seed 42's land, not 100%. **Mass sets the floor; the authored devotion decides
the bind.** The Tilth measured three kinds and this spec extended the result to a
roster it never covered.

Goblin is the instructive boundary: devotion 0.35 clears floor 0.3347 by 0.0153
and still drops to 97.04% on seed 1234. The theorem is exact; the margin is what
makes it visible.

### 10.2 Desert dwarf is buildable, and will be built (reverses §5 P3)

Aridity is **not** a prepared stop. Authoring `devotion_elev` above the kind's
sovereignty floor (~0.44 at 66 kg) makes its temperature and moisture curves
bind, exactly as kobold's and hobgoblin's already do in the shipped roster.

The §3.1 table's *prepared* rows are therefore conditional on an authoring
choice, not on the model. **Nathan's decision, 2026-08-07: make Desert actually
work.** It becomes the first people in the roster whose climate niche selects.

**P3 is withdrawn and replaced.** The old P3 predicted Desert would be
indistinguishable from Hill. The replacement:

> **P3′ — Desert's climate curves bind.** With `devotion_elev` authored above
> its sovereignty floor, the Liebig-binding axis for desert-dwarf is
> `temperature` or `moisture` on ≥ 20% of land cells, and its capacity field
> correlates with Hill's **below 0.95**. *Predicted: confirmed.*

This makes `BIO-gnoll-desert` a sharper row, not a satisfied one: gnoll's
documented desert stronghold selects zero settleable cells **because gnoll's
`devotion_elev` of 0.40 sits below its floor of 0.4954**, so its authored
moisture curve never binds. That is a diagnosis the row did not have. Gnoll is
**not** re-authored here — moving an existing people's capacity inside a roster
epoch is the attribution-destroying bundling §6 of the programme spec refuses.
The diagnosis is recorded on the row for its own campaign.

### 10.3 The depth coordinate is folded in (reverses §8 item 1)

**Nathan's decision, 2026-08-07: build it in C2c.** Without it Mountain and
Duergar are one kind, and no authoring choice can separate them — depth is
*absent*, not merely prepared, and that distinction is what §3.1's trichotomy
was for.

**The coordinate already exists and is not being invented.**
`domains/terrain/src/strata.rs:107` defines `BandSample.top_depth_m` — *"depth
to the top of this band, metres below the surface"* — and `Cave.deepest_band`
(`features.rs:29`) names which band the void reaches. So a chamber's elevation
is a **read** over two committed derivations:

```
  height_asl_m(chamber) = height_asl_m(surface) - top_depth_m(cave.deepest_band)
```

No new field, no new tuning, no change to cave prevalence, clustering or
`cave_depth` itself — `MAP-cave-depth-weld` and `MAP-underworld-reachability`
stay untouched. `top_depth_m` varies per cell with column thickness, so the
underground elevation field has real spatial structure rather than being a
constant offset.

`subterranean_substrate` (`windows/worldgen/src/lib.rs:2189`) takes
`fn(Substrate) -> Substrate` and so cannot see the column; it gains a depth
argument, supplied by its one caller at `lib.rs:1232`, which already holds the
terrain and the cell.

**Why this is not either of the two refused repairs.** It does not floor an
axis and it does not change the tolerance operator. It supplies missing
information to the axis that already binds. It is a fidelity correction: a
chamber under a 2000 m peak is genuinely not at 2000 m, and the previous
behaviour was an approximation The Deep Realm recorded as out of its scope.

**P2 is withdrawn and inverted.** The old P2 predicted Mountain ≡ Duergar. The
replacement:

> **P2′ — Mountain and Duergar are distinguishable, and depth is what
> distinguishes them.** Authored with different elevation optima — Mountain
> shallow, Duergar deep — their capacity fields correlate **below 0.95**. And
> with the depth coordinate reverted, the same two kinds correlate **above
> 0.999**. *Predicted: confirmed.* The second half is the mutation: it proves
> the separation comes from depth and not from some other authored difference.

### 10.4 The Warren's tripwires now redden by design, and must be re-measured

This is the part that must not be done casually. `warren_readout.rs:310` and
`deep_realm_rehome.rs:301` both assert `ratio == 1.000` and both say, in their
own text, that a change means *"the tolerance model changed and the spec's §10.3
and the chronicle need re-measuring rather than this assertion needing a nudge."*

**They anticipated a tolerance-model change. This is a substrate change.** The
instruction still applies and the campaign owes the re-measurement:

1. Re-run The Warren's P1 with the depth coordinate live. Rust monster and xorn
   are subterranean and will re-score.
2. Update the assertions to the newly measured ratios, with a comment naming
   **this** campaign as the cause and stating that the masking they pinned has
   been lifted by supplying elevation rather than by changing the minimum.
3. Update `2026-08-06-the-warren-design.md` §10.3 and
   `book/src/chronicle/the-warren.md`'s "The minimum that cannot see the
   improvement" section — its central claim (*a non-lethal preference cannot
   matter while an unfloored axis is scarcer*) remains true as a statement about
   the tolerance model, but it is no longer what The Warren's own mechanism is
   limited by.
4. `warren_gate.rs`'s mirror should **not** move: it exercises a *Surface* kind,
   and a Surface kind's arithmetic is untouched. If it reddens, the depth change
   has leaked into the surface path and that is a bug.

The Warren's chronicle keeps its finding. What changes is that the finding
acquired a remedy one campaign later, which is the outcome its own closing
section asked for.

### 10.5 Consequences for scope, cost and the other predictions

- **P1 stands but is re-stated.** It is no longer "climate is silent for a
  dwarf"; it is the theorem: a kind is elevation-bound everywhere iff its
  authored `devotion_elev` is below its sovereignty floor. Which side each dwarf
  lands on is now an authoring decision recorded per kind.
- **P6 (world identity moves) is strengthened.** Peopled dwarves reach the
  ledger, and the depth coordinate re-scores the two existing subterranean
  fauna. Magnitude still deliberately unpredicted.
- **The census will move more than a roster-only campaign would.** Still one
  regen at the close, per Nathan's authorization.
- **Still out of scope, unchanged:** chamber occupancy and traversal (§2),
  `tolerance_tiered`, flooring elevation, cave prevalence/clustering tuning, and
  re-authoring gnoll.

### 10.6 The depth measurement, and what it does to P2′

`delver_depth_probe.rs` measured the cave-depth distribution **before** Task 3b
was built, applying the autopilot rule's *"this invariant holds → measure it
against real data"* class to §10.3's own claim. Seeds 42 / 7 / 1234:

```
  band       share of cave cells        depth p50          depth max
  Cover    17.5%  43.9%   26.3%             0.0 m          9 - 10 m
  Basement 54.5%  27.2%   40.4%             0.0 m       645 - 1807 m
  Roots    28.0%  28.9%   33.4%    13714 - 14774 m     16607 - 21561 m
  underground height_asl_m p50:  +1045 / +1482 / +368 m
```

**The read is safe and is built** (Nathan, 2026-08-07). Two thirds of caves sit
between 0 and 1800 m; `non_void_roster` has ample habitat. The `Roots` third,
at 14–21 km, becomes uninhabitable — which is correct rather than broken, since
`BandKind::Roots` is documented as *"deep crust: hot, high-pressure."* That
exclusion is a genuine selection effect and is worth having on its own.

**But it probably will not separate Mountain from Duergar**, which is why §10.3
folded it in. Among the habitable shallow caves the median depth is **0.0 m on
every seed** — the variation is a thin tail. Two kinds differing only in an
authored elevation optimum will therefore score alike across most of their
shared habitat.

**P2′ is therefore restated as a genuinely open question, not a confident
prediction.** The campaign ships whichever answer it gets:

> **P2″ — does depth carry enough variance to separate two kinds?** Mountain
> and Duergar are authored with different elevation optima. Their capacity
> fields are measured, and the correlation is **reported, not asserted**. The
> preregistered *interpretation* is fixed in advance so the result cannot be
> read after the fact:
> - `r < 0.95` — depth carries enough spatial variance to separate two
>   subterranean kinds. The degeneracy is resolved.
> - `r >= 0.95` — it does not, and the reason is now measured rather than
>   assumed: **the depth field is nearly constant over habitable caves.** That
>   is a sharper finding than the original "depth is inexpressible", because it
>   names the quantity that is missing (variance, not the coordinate) and it
>   would not have been reachable without building the coordinate first.
>
> The mutation stands either way: with the depth reverted, the two must
> correlate above 0.999. That is what makes the number mean anything.

**This is the campaign's second preregistered null-tolerant result**, and it is
deliberate. The Long Age shipped its null as the headline; the same discipline
applies here. Under no circumstance is a threshold moved after unblinding to
make one branch true.

---

## 11. Amendment — the roster is cut to three, and Task 3b is withdrawn

**Nathan's decision, 2026-08-07.** C2c ships **Hill, Gully and Desert**.
**Mountain and Duergar are deferred** to a successor campaign that makes the
underworld a place.

### 11.1 Why: the fake this campaign existed to avoid, committed by this campaign

The Warren's retrospective states C2c's founding hazard exactly: authoring
Mountain and Duergar under the old model *"would have meant authoring them with
a low-insolation surface curve, which is precisely the fake the previous
campaign spent itself removing."*

This campaign avoided the low-**insolation** version and then committed the
low-**elevation** version of the identical error. Duergar was authored at an
elevation optimum of 300 m to mean *deep*, on the strength of a depth
subtraction (Task 3b) that had not landed. **Depth-below-surface and
elevation-above-sea-level are different quantities**: a deep chamber under a
mountain is at *high* ASL; a shallow cave in a marsh is at *low* ASL. So the
authored curve selected lowlands, and the measured result — duergar rooting
`marsh`, `spring` and `valley`, more toponyms than any other people at seed 42 —
was read at first as an emergent finding about a fungal food web wanting damp
ground. **It was not.** It was the elevation curve doing exactly what it says.

Task 3b would not have rescued it. `delver_depth_probe.rs` measured the median
depth over habitable caves at **0.0 m**, so the subtraction mostly does not move
the value the curve reads.

**A kind whose defining trait the model cannot express is rung 2 — the trap this
programme's ladder exists to name.** Hill, Gully and Desert are expressible
today. Mountain and Duergar are not, and shipping them would be the programme
declining its own acceptance criterion in the campaign that discovered the gap.

### 11.2 What replaces it: the underworld becomes a place

Registered as `BIO-kinds-declare-biomes` (Nathan, 2026-08-07). **The sea already
solved this and the land never inherited the solution.** `Epipelagic`,
`Mesopelagic`, `Bathypelagic` and `Abyssal` are depth-named biomes carrying a
decaying supply multiplier (`.45 / .15 / .05 / .02`,
`windows/worldgen/src/lib.rs:961`), so in the ocean depth and darkness are a
**place type**, not a coordinate pushed through a tolerance curve. Decision 0105
gave rock an addressed chamber graph and nobody ever gave it biomes.

An underworld biome is near-free: `f(CaveKind, Cave::deepest_band)`, both
already committed, no new draws. Species-declares-its-biomes generalizes
`HabitatRealm`, which The Warren measured to be the one mechanism in this model
that can **select** rather than merely modulate. Travel stays unconstrained —
`traversal.rs` already gates movement on biome; habitat would gate settlement.

### 11.3 Consequences

- **Task 3b is withdrawn.** Its justification was separating Mountain from
  Duergar. With both deferred it would redden both Warren tripwires and move the
  census for the benefit of two fauna kinds, inside a roster campaign. The
  biome design supersedes it: a biome expresses depth better than an ASL
  subtraction, and §10.6 already measured the subtraction as nearly flat.
  **The two tripwires therefore stay green, and this campaign does not touch
  `subterranean_substrate` at all.**
- **`habitat_realm_registry` returns to exactly xorn and rust-monster.**
  `the_subterranean_roster_is_exactly_the_two_rehomed_kinds` stays green
  **unchanged** — the deliberate edit Task 5 planned there no longer happens.
- **`family_proto` is still required.** Three kinds still carry the `dwarf`
  family label, so the multi-member-family invariant still binds and
  `cli/src/proto.rs`'s generalisation beyond `goblinoid` is still in scope.
- **`LifeSchedule::Paced` still gets its first occupants** — all three surface
  dwarves are `Paced(4.0)`. The coverage table still moves `Declared` →
  `Witnessed`, and `generation_length_of` still closes The Long Age's open
  code-reading gap.
- **Roster arithmetic:** biosphere 35 → **33**; psyche/perception/dispersion
  14 → **12**; settling peoples 11 → **9**.

### 11.4 The preregistrations, restated

- **P1** stands unchanged (the bind theorem).
- **P2″ is withdrawn entirely.** There are no two subterranean kinds to compare.
  The question it asked — does depth carry enough variance to separate two kinds
  — passes to the biome campaign, with §10.6's measurement as its input.
- **P3′ stands.** Desert is the campaign's climate demonstrator and is
  unaffected.
- **P4 becomes Gully / Hill / Desert** pairwise below 0.95 — still the control
  that proves the distinctness probe discriminates.
- **P5 stands** (three Paced kinds are read).
- **P6 stands**, magnitude still unpredicted.

### 11.5 What C2c's headline becomes

Not "the first people in the Deep Realm." That claim is withdrawn and it was
never true: settlements are cell-keyed, so a subterranean kind lives on the
surface of a cell that has a cave in it. The headline is the discovery itself —
**the realm gate places a kind at a cave mouth, not underground, and the model
has no vocabulary for the inside of the world because the sea got biomes and the
rock got a graph.**
