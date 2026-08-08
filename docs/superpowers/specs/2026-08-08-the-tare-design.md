# The Tare — design

*The tare is the weight you subtract to make a scale read true.*

Campaign: **The Tare**. Clears the five red heavy-tier tests standing at main
before C2d (The Radiation) begins. Not a feature campaign: every item is a
**measurement instrument**, and no product behaviour changes anywhere in it.

Sequencing rationale, adopted unchanged from the session brief: starting a
roster campaign on top of five unexplained heavy-tier failures makes every
future heavy failure ambiguous about its cause. Attribution under simultaneous
change is the trap the last two campaigns kept falling into
(`docs/retrospectives/the-delvers.md`).

---

## §1 The reds, measured

Source: lefford heavy run `heavy-20260808T163452Z-442429.log`, ref `9c96e45f`.
The two commits between that ref and main are docs-only
(`git diff --stat 9c96e45f..155b0901` touches four files under `book/` and
`docs/`), so the log is a current picture of main.

```
FAIL  hornvale::history_battery              history_gates_full_world_and_cross_seed
FAIL  hornvale::scene_cost                   scene_api_cost_is_bounded_on_seed_42
FAIL  hornvale::session_cost                 a_possessed_turn_stays_within_its_ceilings
FAIL  hornvale-lab::disposition_calibration  non_raiding_peoples_hold_their_genesis_flagship…
FAIL  hornvale-worldgen::occupancy_readout   occupancy_readout_is_current
```

**Five, not the three the brief lists.** `disposition_calibration` is named in
no campaign document; it is, however, already known to the idea registry as the
reason the heavy tier's exit code is a constant
(`TOOL-heavy-tier-red-allowlist`).

### §1.1 The unifying diagnosis, and where it stops

All five are instruments whose **stated cause is not their measured cause** —
The Confusion's lesson recurring five times. But the cure does **not**
generalise, and the natural generalisation ("move it onto the census") is wrong
for four of the five. Two defect classes:

| class | defect | cure | instances |
|---|---|---|---|
| **A** | too few reads of a wide distribution | the census | `history_battery` |
| **B** | a literal that was always a function of something that moved | re-derive the rule, or adjudicate | `disposition_calibration`, the two cost gates |
| — | a stale witness | re-check, then re-pin | `occupancy_readout` |
| — | a criterion written only in prose | mechanize it, after correcting it | the two cost gates |

The discriminating measurement: `disposition_calibration` **already samples 60
worlds** and still breaks. A thousand would not save it, because its bound
encodes a *pre-Tolerance physics*, not a sample size. Sample size is orthogonal
to that defect.

---

## §2 Item 1 — the displacement gate becomes a census column (Class A)

### §2.1 What is actually true

`cli/tests/history_battery.rs:190` asserts `mig42 > 0` and fails with *"seed-42
displacement does not fire at all at Full depth: the gates do not survive the
cascade"*.

**The message's explanation is false.** Measured on this tree:

```
seed 42 @ Full        migration_events 0   collapses 7   occupations 420   tribute relations 49
seed 42 @ Settlements migration_events 0   collapses 7   occupations 420   tribute relations 49
```

Zero at *both* depths, on a thoroughly alive world. The cascade has nothing to
do with it. That sentence was written when the guard was built, describing the
failure its author expected — a prediction read at failure time as a finding,
which is the exact shape `docs/retrospectives/the-confusion.md` is about.

### §2.2 Why no single-seed gate can work here

48-seed probe, `BuildDepth::Settlements`:

```
migration events   deciles [0, 0, 3, 5, 111, 291, 578]   zeros  6/48 (12.5%)
tribute relations  deciles [2, 4, 52, 71, 102, 131, 166] zeros  0/48
collapses                                                zeros 20/48

spearman  migration vs tribute relations  -0.357
          migration vs collapses          +0.565
          tribute   vs occupations        +0.611
```

Bimodal, spanning nearly three orders of magnitude, and **zero on one world in
eight**. Any single-seed "does displacement fire" assertion therefore has a
~12.5% failure rate by construction. The nine-seed sweep inside the same test
asserts `r.migration > 0` *per seed*: at 12.5% per world that is roughly a 70%
chance of at least one red on an arbitrary nine-seed panel. **It passes today
because its nine seeds are lucky, not because it is robust.**

### §2.3 What ships

**One new census metric**, following the `raid-victim-rate` /
`hydro-variant-coverage` precedent exactly (the doc names the battery it
replaces):

- `climate-displacement-events` — `migration_events` per world, read off the
  ledger at the Settlement rung (`OCC_CAUSE == "migrated"`, excluding
  conquest-relocations, unchanged from the existing function). `Absent` on a
  world with no occupation records. Numeric summary, bucket edges chosen to
  straddle the measured bimodality.

**One calibration test** in `windows/lab/tests/calibration.rs`, reading the
committed fixture, asserting what a thousand worlds can support and nothing
more:

- the column is present and non-empty (anti-vacuity);
- displacement is **not inert across the census** — the pooled count clears a
  floor set well under the measurement;
- every value is a non-negative finite count (a fold that broke would show
  here);
- the **zero share is reported, not asserted** — it is the number that
  justifies retiring the single-seed gates, and pinning it would re-create the
  defect one level up.

**In `cli/tests/history_battery.rs`:** delete the seed-42 `mig42 > 0`
assertion and the per-seed `r.migration > 0` inside the sweep. Both are the
same defect. **Keep** the pooled `SWEEP_MIGRATION_FLOOR` volume claim, the
territory-separation gate, the stratigraphy gate and the median
depth/capacity-correlation gate — none of those is the defect under
discussion, and removing them would be scope creep. **Keep migration in the
generated report artifact** (`book/src/laboratory/generated/the-history/`) as
a reported column, so the artifact's shape does not change.

### §2.4 Mutation proof

The metric's fold is exercised on hand-built records, not only on live worlds
— a column that reads the same number on every world proves nothing about the
check behind it. Two mutations, each of which must produce a **real assertion
red**, not a compile error, and each of which must first assert its target text
exists before substituting (a no-op mutation is worse than no mutation).

---

## §3 Item 2 — both panels retire, and the tribute one needs a new observable

Both 12-seed panels go. The Sundering panel is a clean supersession; the Tithe
panel needs a replacement observable, and choosing it was a measurement
exercise rather than a design one.

### §3.1 The Sundering migration panel — superseded exactly

`windows/worldgen/tests/history_sundering.rs`'s
`the_migration_distribution_is_reported_over_a_panel` reads `migration_events`
— the *same quantity* §2's census column measures, at 12 worlds instead of
1000. Delete it, its `FLOOR_PANEL` reporting and `MIN_MIGRATION_EVENTS`, and
point the module doc at the census column. The file's other gate
(`isolation_predicts_divergence`) is untouched.

### §3.2 The Tithe panel measures what the world does not keep

`the_tribute_accumulator_is_reported_over_a_panel` asserts on
`c.tribute_collected` — a FLOW integrated inside `History::tally`, which
`build_world_to` discards before any census view exists. The only committed
tribute fact is `PAYS_TRIBUTE_TO`, on the subordinate, carrying the patron,
**dated by the day the relation began**.

That asymmetry is a design statement, not an oversight: **the world remembers
who owes whom, not how much has been paid.** So the task is not to find a
faithful stand-in for the flow; it is to measure what the world keeps, and to
state honestly how well that tracks the flow.

### §3.3 The observable, chosen by measurement over three refuted predictions

Every ledger-visible candidate, scored against the bake's own
`tribute_collected` over 36 worlds:

```
  stock  (standing relation count)   spearman +0.9344   <- chosen
  relation_years  SUM(now - since)            +0.8909
  patrons (distinct)                          +0.8419
  top_share (largest patron)                  -0.7692
  oldest  MAX(now - since)                    +0.4571
```

Three predictions made during design were refuted here, and each is recorded
because the refutation is what chose the column:

1. **"Relation-years will win, because the flow accumulates and a stock is
   steady."** False. The plain count beats it. Decomposed: count-only 0.934,
   duration-only 0.457, count x duration 0.891 — the count carries the signal
   and the duration dilutes it.
2. **"`top_share` earns its own column at -0.769, since it is not a
   rescaling."** Substantially an arithmetic artifact: `top_share >= 1/stock`
   by construction, and excluding small worlds decays it -0.769 -> -0.727 ->
   -0.702 -> -0.608 -> -0.539 (at `stock > 60`, n = 28) while `stock` holds at
   +0.875. Declined.
3. **"A census column should be a normalized share, for cross-world
   comparability."** False, and by a wide margin: `stock / occupations` scores
   **+0.623** against the raw count's **+0.934**. Raw counts are precedented
   (`raid-attribution-unresolved` is one).

**What ships: one metric, `tribute-relations-standing`** — the count of
`PAYS_TRIBUTE_TO` facts on a world, Settlement rung, `Absent` on a world with
no occupation records. Its doc names the panel it replaces and states the
0.934 agreement as a measured witness, not as an equivalence.

### §3.4 A second column is declined, and the reason is not cost alone

An invariant column mirroring `raid-attribution-unresolved` — *does every
patron reference resolve to an occupation record* — is **declined as
structurally vacuous**. `history_emit.rs` already carries
`.expect("a tribute patron names a community minted in this history")` at the
commit site, so the property is true by construction and the column could
only ever fire on hand-built input. The Confusion's equivalent was justified
because its invariant had actually broken in production; this one has not and
cannot.

### §3.5 The retired panel was already near-vacuous, so the bar is not "match it"

**All four candidates, and `tribute_collected` itself, are `0/36` zeros** —
every sampled world collects tribute. The panel's only assertion
(`live * 2 >= n`, at least half the panel collected anything) therefore could
essentially never fire. The replacement must be *better* than the panel, not
merely wider.

The calibration test accordingly asserts:

- the column is present and non-empty (anti-vacuity);
- **the column spans a real range** across the census — a constant column is a
  broken fold, and this is the assertion with actual teeth;
- pooled non-inertness, with the floor set well under the measurement;
- the **zero share is reported, not asserted** — at 1000 worlds there may be
  zero-tribute worlds this 36-world probe cannot see, exactly as The Confusion
  found three no-raid worlds in a thousand that a 12-world probe had missed.
---

## §4 Item 3 — the occupancy readout: two causes, and a claim that was never met

### §4.1 The drift, attributed by measurement

`windows/worldgen/tests/fixtures/occupancy.csv`, regenerated by hand via
`regenerate_occupancy_readout` (deliberately not `heavy:`-ignored, so the
artifact cannot self-heal past its own drift check):

```
350 -> 386 rows
  +36 rows   desert-dwarf, gully-dwarf, hill-dwarf      (C2c, The Delvers)
   24 rows   rust-monster x12, xorn x12  CHANGED        (the realm gate, 643d3c68)
  326 rows   byte-identical
```

**Two causes, disjoint.** The brief attributes the drift to C2c's three kinds;
that is the +36 rows only. The 24 *changed* rows are exclusively the two
lithovores, and `643d3c68`'s own commit message names them: *"A sparse two-row
store. Its occupants are the two kinds The Deep Realm re-authored for darkness
and damp and then left being scored against sunlight."* `rust-monster,alpine`
falls 83081 → 5586 occupied cells.

This is the **third** under-checked attribution recorded against this one
file, and the file's own doc warns the next reader about exactly it.

### §4.2 The Vacancy's exit criterion 6, re-checked rather than re-pinned

```
region    doc's claim                 committed fixture        regenerated
desert    giant-scorpion 0.0177 MET   otyugh 0.0470 not met    otyugh 0.0470 not met
savanna   treant 0.0555     not met   treant 0.0822 not met    treant 0.0822 not met
taiga     treant 0.0273     not met   treant 0.0545 not met    treant 0.0545 not met
```

Three findings:

1. **The desert verdict is wrong.** `otyugh` tops desert; `giant-scorpion` is
   not the top occupant and is not even the best newly-authored kind there
   (`carrion-crawler`, rank 2 of 26). EC6 is met in **zero of three** regions,
   not one of three.
2. **The savanna and taiga numbers are wrong by roughly half**, though their
   verdicts hold.
3. **All three region rows are byte-identical before and after the
   regeneration.** The drift never touched EC6's subject, so this failure was
   always present, and the 2026-08-05 "witnesses refreshed, verdict unchanged"
   pass did not re-read the quantity the test computes.

**What ships:** the regenerated fixture; the doc table corrected to measured
values with the two-cause attribution stated; the verdict corrected to zero of
three. **The test's assertion and its `newly_authored` list are not touched** —
its failure is the record, per its own doc, and it stays `#[ignore]`d awaiting
`BIO-supply-drowns-niche`. The diagnosis it carries (a supply term spanning
orders of magnitude multiplied by a condition product bounded in [0,1], so the
niche can only modulate, never select) is **strengthened** by this reading, not
weakened: the top slot in all three regions belongs to a sessile autotroph or a
detritivore.

---

## §5 Item 4 — the cost gates: correct the discriminator, then mechanize it

### §5.1 The retrospective's stated gap does not exist

`docs/retrospectives/the-confusion.md` says `scene_cost` *"asserts on the first
budget it checks, so a contended run panics at genesis and never measures the
four metrics that would settle it."*

It does not. `cli/tests/scene_cost.rs` takes all five measurements (:249–:317),
prints all five (:319–:325), and asserts afterwards (:327–:349). All five are
in the very heavy-run log the retrospective describes. `session_cost.rs` has
the same shape.

### §5.2 The discriminator is not merely un-mechanized — as written it is wrong

```
                   quiet Mac   lefford basis   lefford heavy   ratio to basis
genesis               3947.9          6318.6         13187.1   2.09x
SceneContext::build    416.6          1308.0          1277.5   0.98x
tiles(512)+json       1174.6          4319.9          4207.4   0.97x
small docs+json          0.7             2.7             2.6   0.96x
region per tile         75.5           206.1           266.7   1.29x
```

Four of five reproduce lefford's own recorded basis to within 4%; only
`genesis` moved. The written rule — *"a real regression is LOCAL: one or two
metrics move and the rest hold. A uniform 3x across metrics is the machine"* —
therefore classifies this run as a **regression**. It is not one: the quiet box
builds the same world in 3948 ms against a 13000 ms ceiling.

The rule fails because the five metrics have **different resource profiles**.
`genesis` is the only one that sculpts terrain across a large grid; the other
four operate on an already-built world. A 40-way-concurrent runner starves the
bandwidth-bound phase and leaves the cache-resident ones alone. **Uniformity
was never the right test**, and the correct answer was reached last time only
by a quiet-box re-measure — i.e. by overruling the discriminator, not by using
it.

### §5.3 What ships

- Each budget gains a **named basis constant** beside it, carrying the measured
  value the ceiling was set from. Today those numbers live only in doc-comment
  prose, which is why nothing can compute a ratio.
- On failure, both gates print **ratio-to-basis per metric** and a **computed
  verdict line**, replacing the prose instruction to work it out by hand.
- The verdict is scoped by **resource class**, not by uniformity: `genesis` is
  the contention-sensitive metric and the four scene documents are the control
  set. A `genesis` breach with the controls within tolerance reads as the
  machine; **any control moving** reads as the code. `session_cost` gets the
  matching split (`Session::start` versus the per-turn metrics).
- **No ceiling is raised.** The ratchet rule stands; this campaign changes the
  diagnosis, not the budgets.

### §5.4 What the corrected discriminator found, on its first use

**Amendment, 2026-08-08.** §5.3's last bullet did not survive contact with the
instrument it specifies, and the way it failed is the strongest evidence the
change was worth making.

Run against `session_cost`, the new verdict reported
`1 control(s) moved: ["indoor snapshot+json"]` — 18.662 ms against an 8.910 ms
basis, **2.09x**, reproduced three times at loadavg 5.0-5.4 while the two
controls sat at 1.08x and 0.92x. Not contention.

A commit-by-commit bisect over the full range since the ceiling was set
attributes the entire step to **one** commit:

```
109f8422  the commit that SET the ceiling         indoor  8.85 / 8.80 ms
211e99ca  a built cell has a fabric                       8.92
bcf4a596  light is a derived view over shadowcaster       8.61
7f198ea5  a hearth is at a wall                           8.57 / 8.67
c25bb1d2  PaletteEntry.color fills                       17.71 / 17.30   <--
f962ee95  the lens                                       17.17
155b0901  main                                           18.72
```

**The hypothesis this spec's author offered was wrong.** Five candidate commits
were named, with the light and fabric work reasoned as the likely causes; four
of the five moved the metric by exactly zero. The cause is the palette-key
widening from `CellKind` to `(CellKind, Option<[u8;3]>)`, which replaced one
shared entry per wall material with per-cell `Observer::sense` + `to_srgb`
work on every indoor snapshot.

**It is a feature's price, not a defect.** Nathan's call: raise the ceiling to
40.0 (~2x the new measurement, the same method every other ceiling here uses)
and move the basis with it, both recorded at the constant. Moving the basis is
not bookkeeping — leaving it at 8.910 would make this metric report a moved
control on every future green run, and an alarm that always fires is an alarm
nobody reads.
- `docs/retrospectives/the-confusion.md`'s follow-up is **corrected in place**
  with the log evidence. An incorrect follow-up in a retrospective about
  inherited diagnoses is precisely the failure that retrospective names.

---

## §6 Item 5 — the disposition calibration, adjudicated (the headline)

### §6.1 The falsification is larger than the one failure reported

`windows/lab/tests/disposition_calibration.rs` predicted this red in advance:
*"if this battery reddens on the next heavy-tier run, that is a finding for The
Tolerance's readout to report, not a number to adjust."* Partitioning the nine
settling peoples by authored `threat_response >= 0.6`, off the heavy-run log
(seeds 1..=60):

```
RAIDERS (>=0.6)     rate        ABSTAINERS (<0.6)   rate
  kobold    0.80   0.729          human      0.50  0.333  BREACH
  bugbear   0.80   0.467          desert-dw  0.45  0.333  BREACH  <- only one reported
  gnoll     0.85   0.450          goblin     0.50  0.300  BREACH
  hobgoblin 0.70   0.600          gully-dw   0.20  0.050
  hill-dw   0.60   0.433

  RAIDER_MIN     0.30   weakest raider 0.433                HOLDS
  NONRAIDER_MAX  0.25   broken by 3 of 4 abstainers         FAILS
  SEPARATION     2.0    0.433 / 0.333 = 1.30 (was 2.55)     FAILS
  spearman(threat_response, rate) = 0.831, n = 9            ORDERING SURVIVES
```

**Three of four abstainers breach, not one.** The test asserts inside its loop
and stops at the first in `BTreeMap` order, so it reported `desert-dwarf` and
never reached `goblin` or `human` — and never reached the separation check at
all. This is the genuine instance of "the assertion destroys the evidence the
diagnosis needs", the defect §5.1 shows was misattributed to `scene_cost`.

### §6.2 The adjudication

**The ordering claim survives; the hard partition at 0.6 is dead.** That is
exactly what the file's own doc anticipated when The Tolerance replaced an
authored-mean comparison in `Bake::takes_the_initiative` with a
**per-settlement draw** around that mean. Under a draw, every people has
settlements on both sides of the gate, so a two-set partition is no longer a
partition of behaviour — but the rate remains a continuous increasing function
of the mean.

### §6.3 Three assertions, none of them fitted

Chosen by charting statistic-form against where-the-bound-comes-from and
reading the saturated grid's gaps. Every bound this file ever carried sat in
the *fitted* column; all three below sit in the *sign* column, which is why
they are invariant to roster growth where their predecessors were not.

1. **PRIMARY — `separation > 1.0`.** The weakest raider re-seats more often
   than the strongest abstainer. This is the original preregistered claim with
   the fitted magnitude stripped off and the direction kept. Measured
   **0.433 > 0.333** (margin 1.30x): it holds. **The factor is not set anywhere
   above 1.0** — thinness is the correct condition for a sign claim, and any
   higher value would be refitting.
2. **SECONDARY — `spearman(threat_response, rate) > 0`** across the whole
   roster. Catches a global loss of ordering that a min-versus-max comparison
   can miss. Measured 0.831, pinned as a **witness** and deliberately not used
   as the threshold.
3. **LOAD-BEARING — the span guard.** The rates must span a real range before
   the correlation is read.

`RAIDER_MIN` stays unchanged at 0.30 (it holds at 0.433). The 2.55 -> 1.30
collapse is recorded as a witness.

**Collect every breach and assert once at the end**, reporting the whole table.
An instrument must not destroy its own evidence — which is the defect this very
campaign found misattributed elsewhere.

### §6.4 Why the span guard is load-bearing, not decoration

The file carries two mutation controls: forcing `Bake::takes_the_initiative`
to `true` and to `false` must each redden the battery. Checking the proposed
assertions against them found a real defect in an earlier draft of this spec:

- Force everybody to raid, or nobody, and all nine rates move **together**.
  The rates collapse toward equal, and **the correlation becomes noise of
  arbitrary sign** — so `rho > 0` alone might or might not redden. It is not a
  reliable anti-vacuity guard.
- `separation > 1.0` reddens cleanly under both, because the ordering between
  the subsets collapses.
- The **span guard** reddens under both by construction, which is what makes
  the correlation safe to assert at all.

This is why the answer is all three and not the correlation alone. It is also
why "delete the bound and report the table" is rejected: with nothing asserted
both mutation controls pass, and a 400-second battery becomes a printout.

### §6.5 The honesty constraint

**This is a post-hoc re-derivation and is labelled as one in the test.** Every
bound above is set from the **mechanism** — a per-settlement draw around an
authored mean predicts monotonicity and predicts the *sign*, and nothing more.
No measured value is used as a threshold anywhere. That is the move The Delvers
made when it re-derived the diversity ceiling's rule instead of fitting its
value, and the move this repo forbids is the one not taken here: `NONRAIDER_MAX`
is **not** raised from 0.25 to clear 0.333.

The honest cost is stated at the test: **a sign claim is a weaker discriminator
than the ceiling it replaces.** It is chosen because it is the strongest claim
the shipped physics actually supports.
---

## §7 Sequencing, and the window in which the workspace is legitimately red

This campaign adds **two** metrics — `climate-displacement-events` (§2) and
`tribute-relations-standing` (§3). Each adds a column to **nine studies**:
every study carrying `"metrics": "all"` (`the-census`, `census-of-eyes`,
`census-of-lands`, `census-of-peoples`, `census-of-tongues`, `census-of-words`,
`census-of-the-meeting`, `census-of-faiths`, `the-pyx-probe`). That reddens the
census-fixture tests until a census regen, which `make rebaseline` **cannot**
perform. The Delvers measured **34** such reds for a comparable single-metric
addition; expect at least that. The implementer must diff the committed fixture
header against the live registry and confirm it differs by **exactly two
columns** and nothing else — which is what makes "these reds are expected" a
verified statement rather than a hope.

The order is therefore structural, not a preference:

1. Items 3, 4, 5 (occupancy, cost gates, disposition) — no census dependency.
2. Item 1's metric + calibration test, and item 2's panel retirement.
3. `make rebaseline` + goldens.
4. **Census regen on the canonical box** — `bash scripts/census-run.sh`.
5. Re-pin the metric-registry size assertion (**191 → 193**) and the
   calibration witnesses the regen moves. That line has caught three pairs of
   parallel campaigns reconciling to a wrong number; add a provenance comment
   rather than replacing the existing ones.
6. Final green gate, then the heavy tier via `make heavy-remote REF=<sha>`.

The gate cannot be green before step 4, the size of that red is knowable in
advance, and knowing it is the difference between waiting and debugging.

**The census regen is a carve-out requiring explicit authorization.**
Authorized by Nathan at G3, 2026-08-08.

---

## §8 Success criteria

1. **All five heavy-tier tests are green.** `occupancy_readout`'s sibling
   `each_target_region_gains_a_top_ranked_occupant` stays deliberately red as a
   preregistered record, but it carries a **non-`heavy:`** ignore reason and so
   is not in the tier at all — `make gate-full` never runs it.

   **Consequence worth stating: this makes the heavy tier's exit code mean
   something again.** `TOOL-heavy-tier-red-allowlist` exists because
   `disposition_calibration` is deliberately red and `gate-full-heavy.sh`
   propagates nextest's status unfiltered, so every otherwise-green run reports
   failure forever. Adjudicating that test removes the premise. The implementer
   must **verify** at the close that no `heavy:`-tagged test remains
   deliberately red, and amend that registry row with the result rather than
   assuming it.
2. No ceiling, floor or bound is raised to clear a measurement anywhere in this
   campaign. Every threshold that moves is **re-derived from a mechanism** and
   says so at its definition.
3. Every claim this spec makes about a generated artifact or a tool's behaviour
   is backed by a command and its output, recorded in the ledger.
4. The census column is mutation-proved before it is trusted, and each mutation
   asserts its target text existed before substituting.
5. `docs/retrospectives/the-confusion.md` carries a correction, not a quiet
   edit.
6. Book: a chronicle entry, a Confidence Gradient re-score if any bet moves,
   and a retrospective.

## §9 Non-goals

- No change to any product behaviour: no domain, no kernel, no provider.
- ~~No ceiling recalibration on the cost gates.~~ **AMENDED 2026-08-08, by
  Nathan, after measurement.** `scene_cost`'s ceilings are untouched as
  planned. `session_cost`'s `INDOOR_SNAPSHOT_BUDGET_MS` **was raised, 18.0 ->
  40.0**, with its basis 8.910 -> 18.720. The corrected discriminator found a
  genuine localised cost increase on its first real use, a bisect put the whole
  step on one commit (`c25bb1d2`, per-cell colour replacing per-material
  palette sharing — the four sibling Lantern commits moved it by zero and both
  controls stayed flat), and the ratchet rule provides for exactly this as an
  explicit reviewed act. The original non-goal assumed any red here would be
  contention; it was not.
- No `BakeCensus` commit / save-format change. The tribute FLOW stays
  unreachable from the census by design; §3 measures the stock instead and says
  so.
- No new species, and no touching C2d's surface.
- No retuning of `NONRAIDER_MAX` to clear 0.333.
