# The Trencher — decision ledger

**Campaign:** The Trencher. Rung 1 of the food-system program.
**Branch:** `campaign/the-trencher`, continuing `campaign/the-ceiling` rather
than merging it (Nathan's ruling). **Base:** `d84ac908a`.
**Decision block:** 0976-0985. **Started:** 2026-09-11.

Autopilot is engaged. The Ceiling's ledger
(`docs/superpowers/ledgers/2026-09-11-the-ceiling.md`, twenty entries) is this
campaign's evidence base and is not restated here.

---

## #1 [Q] — `CHEMOSYNTHATE` survives as an aggregate

**Question.** The campaign adds per-metabolite resource axes. Does the
existing `CHEMOSYNTHATE` axis (id 6) stay, or is it replaced by the finer
vocabulary?

**Decision: it stays, alongside the new axes.** A generalist eats the
aggregate; a specialist names a metabolite.

**Why, and this is decided from precedent rather than taste.** The Tidemark is
**mid-campaign** authoring a vent commensal that weights `CHEMOSYNTHATE`, with
a boundary this project negotiated in both directions today. Deprecating the
axis would break a live peer campaign for tidiness. CLAUDE.md's entire
preflight/board apparatus exists because "no gate has an opinion about whether
two campaigns changed the same idea in incompatible ways"; this is that case,
seen in advance.

**Alternatives discarded.** *Replace it* (cleaner vocabulary; breaks a live
campaign mid-flight). *Deprecate with a shim* (a shim nobody removes is a
second vocabulary forever).

**Ideonomy passes / overturns:** none run; a coordination question settled by
precedent and a live peer's state, not a design space.

**Capture actions:** spec §4.2; The Tidemark to be told on the wire.

---

## #2 [Q] — `Geothermal` and `DetritalImport` leave the food vocabulary

**Decision.** `DetritalImport` routes to the **existing** `DETRITUS` axis;
`Geothermal` becomes a **modifier** on the chemical supplies rather than a
food.

**Why.** Their own docs say so. `DetritalImport` is "surface-sourced organic
and mineral material", reads `drainage` rather than rock chemistry, and its
doc already states it is "not one of the row's six". `Geothermal` is "the
gradient itself … independent of local mineralogy" — a condition, not a
substance. The seven-way mean currently adds five chemical foods, one pile of
surface detritus and one thermal gradient together and calls the result
chemical food. **The resolution problem sits on top of a category error, and
the category error is the one worth fixing first.**

**Corroboration, not the reason:** the two category-outliers are also the two
smallest contributors in The Ceiling's measured histogram — `Geothermal` wins
0.3% of chambers, `DetritalImport` 1.6%.

**What is NOT decided:** the exact form `Geothermal`'s modifier takes. That is
the implementer's, argued in the code, per the rule that a plan author does
not know which values discriminate.

**Ideonomy passes / overturns:** one pass, at spec-drafting time — reading the
seven sources' own docs as a classification rather than a list, which is what
surfaced that three different kinds of thing were being averaged. That
reframing is this campaign's whole §2 and it overturned my own earlier framing
("the mean is too coarse") into a sharper one ("the mean crosses category
boundaries").

**Capture actions:** spec §2 and §4.2.

---

## Follow-ups

- **Ley-lines are now reachable but unbuilt.** Turning `thaumic` on makes
  `MAP-40`/`MAP-53` live — Sculpting's carve seam was written
  potential-agnostically on purpose, so thaumic flux over a thaumic potential
  reuses the erosion machinery whole. Named in spec §6 as out of scope so the
  successor knows the hook is open rather than rediscovering it.
- **The tolerance half of this boundary is still open.** Nathan ruled that
  `ConditionNiche` and `Substrate` unify into the kernel (The Ceiling ledger
  #7); this campaign does the *diet* half of the same seam. Doing both at once
  doubles a registry-wide edit, and the diet half is the one with measurements
  behind it.
- **`census-yellow-fix` carries four underworld peoples on a detached HEAD**
  with ~9,178 uncommitted lines, in the worktree pool nothing reaps. Not this
  campaign's to fix and not depended on (spec §4.6); recorded because a
  `reset --hard` there would take work this campaign's §4.5 assumes will
  eventually exist.

---

## #3 [G2] — The metaphysics gate does not exist, and T4 was half a control

Found at plan-writing time, one hour after the spec was approved. Both are my
defects in a document I wrote today.

**The gate does not exist.** Spec §4.4 said `thaumic` would be "gated on world
metaphysics", and a grep for `Metaphysics`/`metaphysics:` across `kernel/`,
`domains/`, `windows/` and `cli/` returns **no type, no field, no flag** —
only doc comments saying "the metaphysically-inert tier this campaign builds",
which describes the current tier as inert *by construction*. The Ground's
reservation, `UNI-2`, and my own §4.4 all speak of a gate as though one
existed. **Nothing selects metaphysics today.**

I inherited that assumption from The Ground's prose without checking, which is
the same error this campaign's predecessor made four separate times — reading
a document's framing as a description of the code.

**Ruling: the campaign builds the gate, as a default-off pin**, following
`TerrainPins` (`domains/terrain/src/pins.rs:9`), which is `Default` with every
field an `Option`. Default `None` means inert, so an unpinned world takes the
existing path unchanged. *Cost if wrong: a pin is part of a world's
generation inputs, so if pins turn out to carry a contract I have not found,
this needs re-siting before stage 3 commits.*

**And T4 was the vacuous half of a control.** With a default-off pin, "an
inert world is byte-identical" holds **because the new code path never runs**
— a guard the type system guarantees. T4 is now two-way: unpinned must be
byte-identical AND pinned must differ, with the readout naming which fields
moved at how many vertices. A pin that changes nothing is a gate wired to a
derivation that never fires, and the inert arm alone cannot see that.

**Ideonomy passes / overturns:** none; a verification that found a missing
prerequisite and a vacuous control.

**Capture actions:** spec §4.4 and §5 T4 corrected in place, with the
correction stated rather than the text quietly replaced — the spec is
pre-merge and this campaign's own, which is the only condition under which a
record is repaired rather than superseded.

---

## #4 [G1] — Where `Absent` lands: split the token

**Tuple** (`ideonomy-plain`): operators *cross-domain re-instantiation* +
*dimension-identification*; organon *list*; dimension-prompts *direction*,
*autonomy*, *materiality*. Run at Nathan's direction on the one question the
plan deliberately left open.

**The facts it operated on, measured first:** `TrophicMode::Absent` has
**zero** carriers. `ThermalStrategy::Absent` has **one** (`xorn`) and its doc
says "No metabolism at all (construct/undead analogue)" — while `xorn` is
`TrophicMode::Chemotrophic` with an authored `CHEMOSYNTHATE` weight of 0.35.
`ThermalStrategy::Unmodelled`, written *after* The Gossan's split, is careful
where `Absent` is not: "Has a metabolism; its thermal behaviour is not
modelled."

**Dimension-identification found three absences wearing one word**, and the
discriminator is *temporal direction*:

| kind of absence | what it claims | direction over time |
|---|---|---|
| ontological | the organism has none | steady — permanent |
| inapplicable | the axis does not apply | steady — structural |
| epistemic | nobody has decided yet | **decaying** — it is a debt |

The autonomy prompt sharpens it: an ontological absence is a fact about the
**world**; an epistemic one is a fact about **us**. A value whose carrier
count ought to shrink to zero and one that ought not are not the same value.

**Cross-domain re-instantiation found the form is among the most litigated
design errors there is**, and every domain fixed it the same way:

- **SQL's `NULL`** — Codd argued for two markers (missing-but-applicable,
  missing-and-inapplicable); SQL shipped one, and forty years of consensus
  calls that the error.
- **HL7/FHIR** — distinct `unknown` / `not-asked` / `not-applicable` /
  `not-performed` codes, because conflating "not tested" with "tested
  negative" is a safety issue.
- **Survey methodology** — "no answer", "N/A" and "refused" are separate
  missing-data codes; merging them biases every downstream statistic.
- **Accounting** — a zero balance is not the absence of an account.

**None of them resolved it by choosing which meaning wins. All of them split
the token.**

**RULING, adopted into plan Task 1 as a requirement rather than advice:**

1. `Absent` does **not** become a value on each of the three new axes —
   copying it triples the ambiguity.
2. **Ontological absence sits OUTSIDE the triple.** A construct has no energy
   source, no electron donor and no carbon source: one claim about the
   organism, not three coincidences. `TrophicMode::Absent`'s zero carriers
   mean nothing is displaced by moving it out.
3. **`Unmodelled` already is the epistemic case** and is careful about it. Do
   not reinvent it on the new axes; its population is *supposed* to shrink,
   which is a different lifecycle from the other two.

*Cost if wrong: a construct becomes slightly more awkward to author than a
per-axis `Absent` would have made it — against an ambiguity every comparable
system has had to unwind later.*

**A LIVE DEFECT THE PASS TURNED UP, now plan Task 0, ahead of Stage 1.**
`ThermalStrategy::Absent` has behavioural teeth, not merely a stale doc:
`allometry.rs:71` returns basal rate `0.0` for it and `:125` returns `None`
for lifespan. So **`xorn` has zero basal metabolic rate and no lifespan while
drawing chemosynthate capacity.**

It is authored deliberately — `coverage.rs:143` registers `xorn` as the
*tested witness* (`Rung::Witnessed`) for `ThermalStrategy::Absent` — so this
is not an oversight to correct unilaterally. Task 0 therefore **fixes only the
doc's gloss**, which is unambiguously imprecise on a thermal axis post-split,
and **refers the behavioural question to Nathan**: whether a kind with zero
basal rate and no lifespan should be drawing chemosynthate capacity is a
fidelity question about what a xorn *is*. Changing an authored, tested witness
to tidy a vocabulary is precisely what the decision log exists to prevent.

**Why Task 0 runs before Stage 1:** the split would otherwise propagate an
ambiguous token into three places, and fixing meaning once is cheaper than
fixing it three times.

**Ideonomy passes / overturns:** one pass, **one overturn** — I had been about
to answer "which axis does `Absent` belong to", and the pass established the
question was malformed: it belongs to none of them, and the shape of the
answer is a split rather than a placement.

**Capture actions:** plan Task 1 Step 1 carries the rule and the table as a
requirement; plan gains Stage 0 / Task 0; the `xorn` behavioural question is
recorded here for Nathan rather than resolved.

---

## #5 [Q] — Ametabolic life is a category error (decision 0976)

**Nathan's ruling**, on the question #4 referred to him rather than resolving:
ametabolic life does not make sense. If the world wants ametabolic things it
should have **ghosts** — or constructs, or undead — and those are not
creatures with a trophic mode.

**Minted as decision 0976** rather than kept in this ledger, because it binds
future campaigns: a kind needing `ThermalStrategy::Absent` is a signal that it
is not a creature.

**Consequences, scoped before writing the task rather than discovered in it:**

- **`xorn` moves `Absent` → `Unmodelled`.** It is alive, burrows through
  stone, and eats mineral and a chemical gradient. `Unmodelled` is literally
  "Has a metabolism; its thermal behaviour is not modelled."
- **`Ectothermic` was considered and refused.** A rock-dweller at cave
  temperature is plausibly ectothermic — which is exactly why assigning it
  here would be making a modelling call silently, inside a vocabulary
  refactor, on a plausible-sounding inference. `Unmodelled` exists to say the
  call was never made.
- **`ThermalStrategy::Absent` becomes uninhabited**, demoting `Witnessed` →
  `Declared`. Verified that this is a legal state: `coverage.rs`'s `Rung` enum
  is exactly two values and `Declared` is "The variant or branch exists; no
  kind carries it." **Reserved, not retired** — it is the right value for the
  first genuinely non-living kind.
- **Worlds move.** `xorn` gains `B0_ENDOTHERM` where it had `0.0`, and a
  lifespan where `life_history` nulled it. Rebaseline, not epoch: no stream
  label moves, no consumption order changes.
- **`Unmodelled` now carries two debts.** Its doc is written entirely about
  `BIO-autotroph-physics`; it now also holds "is a chemolithotroph thermally
  coupled to its rock?" The task updates the doc, because a reader meeting two
  cases under a one-case doc will read the second as an accident.

**The task's guard is written to fail in the useful direction.**
`no_living_kind_is_ametabolic` REDs if anyone gives `Absent` a carrier, and
its message says the fix is to ask whether the new kind is alive — **not to
delete the test.** An uninhabited variant is otherwise exactly the shape of
thing a later campaign "fills in" to improve coverage, which is the error 0976
names.

**One branch written into the task deliberately.** Step 7 says worlds *should*
move, and that **if nothing moves that is the finding** — it would mean a
kind's thermal strategy reaches no world number, which the campaign needs to
know before Stage 1 builds on the same machinery. Stated as a branch rather
than a prediction, per this project's own repeated lesson about imperatives
with outcomes hiding inside them.

**Ideonomy passes / overturns:** none for this entry; it is Nathan's ruling on
a question #4's pass surfaced and explicitly declined to answer.

**Capture actions:** decision 0976 with README row and regenerated digest;
plan Stage 0 / Task 0 rewritten from a doc fix into a real change with a
rebaseline and a guard.

---

## #6 [R] — Task 0 executed: `xorn` moved `Absent` → `Unmodelled`

**Files touched, beyond the brief's three.** The brief named
`domains/species/src/lib.rs`, `tests/suite/coverage.rs` and
`tests/suite/metabolic_pairs.rs`. Two more sites broke and needed fixing to
get `make gate-commit` green, neither anticipated by the brief or the ledger
above:

- **`domains/species/tests/fixtures/life-history-all-kinds.txt`** (byte
  golden) — `xorn`'s row moved from `0	-	-	-	-	0.38674726` to
  `68.667452	64.972103	12.994421	0.5801209	28.587725	0.38674726`: real BMR,
  lifespan, maturity, tempo and generation length where it had none; `pace`
  unchanged (both `Absent` and `Unmodelled` carry pace multiplier `1.0`).
  `domains/species/tests/suite/life_history_golden.rs`'s
  `the_life_history_table_is_not_vacuous` asserted the fixture must witness
  the ametabolic (`None`) branch — a requirement decision 0976 makes
  permanently unsatisfiable through `biosphere_registry` (a future non-living
  kind "will want their own treatment rather than a `BiosphereTraits` row
  with the life nulled out", per 0976's own consequences). Removed that
  clause; the branch itself is still guarded directly, by `allometry.rs`'s own
  unit tests at `life_history(.., Absent, ..)` (lines 246, 356) and by
  `is_ametabolic_is_true_only_for_the_absent_thermal_strategy` — neither needs
  a registry carrier.
- **`windows/worldgen/src/lib.rs`**: `tests::an_ametabolic_kind_is_never_asked_for_a_lifespan`
  asserted `xorn.thermal_strategy == Absent` directly off the registry and
  used that to exercise `cascade_regime_of`'s "read `life_history`'s honest
  `None`, never the bare `lifespan()`" discipline. Since no living kind can
  ever carry `Absent` again, the witness moved to a **directly-constructed**
  `BiosphereTraits` (cloned from `xorn`'s own row, mass 55 kg, `Solitary`,
  `thermal_strategy` forced back to `Absent`) rather than disappearing with
  its registry carrier. Added a second test, `xorn_is_alive_and_still_settled`,
  confirming `xorn` itself now carries `Unmodelled`, has a real lifespan
  (~64.97 yr), and still resolves to `CascadeRegime::SETTLED` — same numeric
  outcome as before, different reason it holds (its lifespan sits under
  `LIFESPAN_THRESHOLD_YEARS = 120.0`, not because it has none).
- **`windows/lab/tests/fixtures/affect-trace-seed-42.txt`** (byte golden,
  found only by running the full `--no-fail-fast` suite across
  species/worldgen/vessel/almanac/lab — `gate-commit`'s sub-floor tier does
  not reach it). `xorn`'s entire 40-tick block moved from a flatlined
  `Content arousal=0.0` (the ametabolic case: no drives ever fire) to real
  `Searching`/`Eager`/`Frustrated` cycling through `Danger`/`Fatigue`/`Social`
  objects — the same shape every other living creature's block already has.
  Confirmed by diff that **only** `xorn`'s block moved (one hunk); no other
  creature's trace changed. `affect_trace_golden.rs`'s own doc says a diff
  here "needs investigation before acceptance" because it is meant to catch
  semantic regressions from cache/memo work — investigated: this is `xorn`
  newly experiencing hunger/fatigue/danger because it is alive, exactly
  0976's claim, not a caching artifact. Rebaselined with
  `REBASELINE=1 cargo test -p hornvale-lab --test suite -- affect_trace_golden`.

**SANCTIONED pair count: 5 before, 5 after** (net unchanged — removed
`(Absent, Chemotrophic)`, added `(Unmodelled, Chemotrophic)`). Every other
`SANCTIONED`/`PINNED` assertion in `metabolic_pairs.rs` still passes for the
right reason, re-verified rather than assumed: `sanctioned_thermal_keys_are_pairwise_distinct`
still finds a duplicated thermal key (now at `Unmodelled`, was at `Absent`) so
its per-kind `PINNED` guard is still load-bearing, not vestigial.

**What moved, and it is real (Step 7's branch resolved: something moved, not
nothing).** Two byte goldens (life-history, affect-trace) plus the
`plumb-roster.md` line-number churn from doc edits and the ordinary
`docs/timings.md` ledger rows from `make rebaseline`/`REBASELINE=1` runs.
`make rebaseline`'s drift check against `docs/generated-paths.txt` shows only
`docs/audits/plumb-roster.md` (line numbers) — no almanac, elevation map, or
other committed seed-42 artifact moved, meaning `xorn`'s new BMR/lifespan
did not visibly shift capacity/occupancy/placement in the specific
already-committed seed-42 worlds this repo tracks (it may in other seeds; not
probed).

**Concerns for review, found but NOT fixed — out of this task's declared
scope, and each risks silently making a modelling call `xorn` never asked
for:**

1. **`fatigue_rise_registry()` and `sleep_grade_registry()`
   (`domains/species/src/lib.rs`) still hardcode `xorn` at the floor**
   (`RATE=0.0` / `NO_GAIN=1.0`), with doc/plumb rationale explicitly reading
   "`xorn` is ametabolic and can collect nothing" and "`ThermalStrategy::Absent`
   and `SocialForm::Sessile` are the two traits that reach this floor"
   (`lib.rs:4673` plumb tag; `coverage.rs`'s
   `the_sleep_grade_table_carries_a_real_ladder_not_one_repeated_number` test,
   `~L644-682`). Under the letter of that stated rule, `xorn` no longer
   qualifies for either floor (it is `Unmodelled`, not `Absent`, and
   `Solitary`, not `Sessile`) — yet the tables are untouched, since they are
   `KindId`-keyed data, not derived from `thermal_strategy`. The affect-trace
   golden confirms `xorn` **does** now accrue Fatigue/Danger/Social drives in
   the walk band, so the "collects nothing from a bed" framing is already
   half-stale in practice while the registry values are unchanged. Whether
   `xorn` should keep gaining nothing from an afforded site is a fidelity
   question this task did not have standing to answer (Task 0's own "do not
   make a silent modelling call" discipline, applied to a different pair of
   registries than the one 0976 named).
2. **Several `windows/vessel/src/liveness.rs` unit-test fixtures build a
   synthetic `Body` with `species: "xorn".to_string()` and
   `thermal_strategy: ThermalStrategy::Absent` by hand** (lines ~17365,
   19061, 19130 pre-edit), not read from the registry, so they are
   functionally unaffected by this change (confirmed: full
   `-p hornvale-vessel` run is green). They now describe a hypothetical
   creature sharing `xorn`'s name but not its real (post-0976) thermal
   strategy — cosmetic, not a defect, left alone.

**Verification run.** `make gate-commit`: green (rc=0, two consecutive runs).
Full `cargo nextest run --no-fail-fast` across
`-p hornvale-species -p hornvale-worldgen -p hornvale-vessel -p hornvale-almanac -p hornvale-lab`:
3160/3160 passed on the second pass (first pass found the two goldens above;
both rebaselined and re-verified green).

**Ideonomy passes / overturns:** none; execution of #5's ruling plus two
unplanned repairs to keep the build honest.

**Capture actions:** this entry; task-0-report.md in scratch per the
dispatch contract; concerns above flagged for the controller/review rather
than resolved unilaterally.

---

## #7 [G5] — Task 0 complete, and a guard whose premise Task 0 invalidated

**Task 0 approved** (commit `e65584918`), spec ✅, one Minor. The review traced
mechanisms rather than accepting the report's claims, and confirmed:

- **The vacuity guard's replacement is narrower, not newly vacuous.** The
  removed clause asserted the golden witnessed the ametabolic `None` branch
  *through a registry carrier* — impossible after 0976, so keeping it would
  have made the test permanently red rather than a guard. **Coverage moved
  rather than disappearing:** `allometry.rs`'s own
  `ametabolic_nulls_the_biological_traits` still exercises the `Absent` branch
  directly, off-registry.
- **`an_ametabolic_kind_is_never_asked_for_a_lifespan` still routes through
  production** (`cascade_regime_of` called on the fixture, not mocked), and
  that function reads only mass/thermal/schedule/social_form — never
  `trophic_mode`.
- **`SANCTIONED`'s count does not pass by coincidence.** Net-zero is exactly
  the case a count guard cannot see, but `every_kind_carries_a_sanctioned_pair`
  checks membership per kind and `PINNED` pins every kind's trophic mode by
  name, so a wrong substitution fails one of those.
- **The affect change's real cause:** `is_ametabolic` gates the **entire**
  drives vector — all six — not one drive. `xorn` had no drives at all.

**Minor, deferred:** the synthetic fixture encodes `(Absent, Chemotrophic)`,
the exact pair 0976 just closed, without a comment noting the mismatch. Inert
to the mechanism under test.

## THE FINDING: a zero that was inert and is now load-bearing

`fatigue_rise_registry()` pins `xorn` at `0.0`, explicitly, added in a prior
campaign's fix round with its own comment. **Its stated justification is now
false**, and Task 0 is what falsified it:

> `xorn` (`ThermalStrategy::Absent`) is EXPLICIT here, at `0.0` … the walk's
> ametabolic gate already excludes exactly that kind from the REST drive.

Both clauses fail. `xorn` is `Unmodelled`, and the gate excludes *all* drives
together rather than REST specifically — and excludes none of them for `xorn`
any more.

**Measured in the committed golden:** of `xorn`'s 40 ticks, **18 carry
`object=Some(Fatigue)`** — the plurality, against 11 `Danger` and 11 `Social`.
So the kind's most frequent affect object is a drive whose rise rate is pinned
at zero.

**The structure of the defect, which is the transferable part.** A constant
justified by *"a gate upstream already excludes this case"* is safe only while
the gate holds. It is not merely stale documentation — the value was **inert
under the old gate and became live under the new one**, and nothing marks the
transition. Grepping for stale docs would not find it; only a change to the
gate reveals it.

**NOT resolved here, and deliberately.** Whether a `Fatigue`-objected affect
on a zero-rise drive is wrong depends on what `arbitrate` does with a drive of
zero urgency — whether it is selected on serviceability or a tie-break rather
than magnitude. Neither the reviewer nor this session traced that to
certainty, and guessing would put a fidelity claim in the record on an
untraced mechanism. **Referred to Nathan with its numbers.**

**Not a blocker.** Stage 1 touches the metabolic vocabulary, not the drive
system; it proceeds.

**Ideonomy passes / overturns:** none; a review finding plus a measurement.

**Capture actions:** this entry; the question put to Nathan; a correction of
my own in-session claim that the reviewer's concern rested on a
misattribution — it did not, and the 18/40 count is what settles it.

---

## #8 [Q] — The frustration bug, root-caused; xorn stays sleepless

**Nathan's ruling:** fix the mechanism; **keep `xorn` genuinely sleepless at
`0.0`** — "that's good for another kind of test."

**That ruling is better than my recommendation, and for a reason I missed.** I
had recommended giving `xorn` the shared `0.3` rate. That would have left the
mechanism fix **with no carrier**: the guard would pass because nothing
exercises it — the exact vacuity pattern this campaign's predecessor found five
times. Keeping one genuinely sleepless kind means the fix has a live witness
that can go red.

**ROOT CAUSE, traced rather than guessed** (`systematic-debugging`, Phase 1-2).
`arbitrate`'s activation predicate, `windows/vessel/src/liveness.rs:~6186`:

```rust
if d.seek_while_asleep() {
    !awake || normally        // urgency is NEVER consulted on this arm
}
```

`Fatigue` is the only drive overriding `seek_while_asleep()` to `true`,
"because it is the drive that carries a creature INTO sleep, so the off-phase
is exactly when it engages." Sound intent; its **unstated assumption** is that
a creature entering the off-phase has accrued fatigue. At urgency `0.0` the
drive engages anyway, **nothing can reduce it below zero**, so the blocked
branch — "no candidate reduces the drive" — fires and labels the creature
`Frustrated` with a hardcoded `valence: -1.0`.

**A second finding, not a bug but a misreading trap.** Arousal is *the maximum
urgency across ALL drives*; `object` is *the pursued drive*. They have
different sources, so `arousal=1.0 object=Fatigue` never meant fatigue was
urgent — it meant something else was maximally urgent while fatigue was being
pursued. The trace's rendering invites exactly the wrong reading, and both the
reviewer and I made it.

**THE HYPOTHESIS THE FIX MUST TEST, and it is why Task 0b writes the general
case first.** This may never have been `xorn`-specific: a **normal** creature
that sleeps to full rest while the off-phase is still running has fatigue
`0.0` and should hit the identical path. If that reproduces, the defect was
always present and `xorn` merely made it *permanent* rather than *brief* —
which would mean a pre-existing bug in every creature's sleep, found by
changing one kind's metabolism.

**The risk the fix carries, written into the task rather than discovered in
it:** does a creature need the fatigue drive *active* to STAY asleep? If the
drive going inactive at full rest wakes it mid-off-phase, the fix trades one
defect for another. Task 0b Step 4 requires this checked before committing and
**escalates rather than improvising** if it is true.

**Ideonomy passes / overturns:** one pass (tree-finding + dimension-
identification; spectrum) preceded the trace and produced the region spectrum
— FULL / SATIATED / UNSATISFIABLE / FROZEN / GATED / UNMODELLED /
INAPPLICABLE. Its finding stands and named the target precisely: `xorn` was in
GATED, the intent was FROZEN, and it landed in UNSATISFIABLE because **nothing
in the system can express "present but never selected."** The pass also found
this is the split-the-token problem from #4 one layer down — a zero used to
mean "does not apply".

**Capture actions:** plan Task 0b (TDD, general case before the xorn case);
this entry; the arousal/object rendering trap recorded because it misled two
readers today.

---

## #9 [R] — Task 0b executed: the fix, and the general case DID reproduce

**Step 1's hypothesis confirmed: this was never xorn-specific.** The general
test (`a_fully_rested_creature_asleep_is_not_frustrated_about_fatigue`,
`windows/vessel/src/liveness.rs`) has no `xorn` in it — a synthetic
`Fatigue { awake: false }` with `Perceived.fatigue: 0.0` — and it went **RED**
before the fix, failing for exactly the traced reason
(`label: Frustrated, object: Some(Fatigue)`). The defect predates `xorn`
entirely; `xorn`'s zero rise rate only turned a one-tick misread into a
permanent one. The `xorn` test
(`xorn_asleep_is_never_frustrated_about_fatigue`) uses a REAL carrier —
`sleep_traits_of`/`fatigue_at` resolved through `hornvale_species::
fatigue_rise_registry`'s actual `xorn` row, asserted `== 0.0` inline rather
than assumed — and also went red for the identical reason before the fix.

**The fix** (`windows/vessel/src/liveness.rs`, the `seek_while_asleep` arm):

```rust
(!awake && u > 0.0) || normally
```

`u > 0.0` is the narrowest change that closes the gap: any nonzero debt still
engages Fatigue for the whole off-phase exactly as before (unchanged
behaviour for every creature that has not yet fully repaid); only the
fully-repaid instant (`u == 0.0`) stops claiming there is something left to
chase. Both tests go green; the whole `hornvale-vessel` suite (1234 tests)
stays green.

**Step 4's risk, checked and cleared — sleep is NOT sustained by continued
drive activity.** Traced both routes that put a body under
(`liveness.rs`'s `advance_one` `Intent::Do(Action::Sleep)` arm, and
`session.rs::sleep`): both call `act_span` **once**, at the decision instant,
which computes the WHOLE bout length via `next_awake_day` and then jumps
`st.day`/`wake_at` forward by that span in one step. Neither re-invokes
`arbitrate` before the span elapses, so the Fatigue drive going inactive at
`u == 0.0` **cannot** wake a creature mid-off-phase — there is no re-check
for it to fail. `renders_unconscious`/`act_span` are the only two call sites
in the crate that gate unconsciousness, and both are one-shot. No further
ruling needed; the fix does not trade one defect for another.

**Rebaseline moved more than xorn, and that is Step 1's finding landing in
production data too, not a new bug:**

- `windows/lab/tests/fixtures/affect-trace-seed-42.txt`: **2 of 10** sampled
  creatures moved, not one — `xorn` (28/41 lines) **and `rust-monster`**
  (28/41 lines). `rust-monster` carries the ORDINARY `0.3` rise rate
  (`fatigue_rise_registry`), so its movement is live confirmation, in the
  committed golden, that an ordinary creature really did hit `fatigue == 0.0`
  mid off-phase in this seed's simulation — exactly the general case Step 1
  predicted, not a synthetic-only concern. The coverage floor
  (`labels >= 4`, `species >= 6`) still clears. Later-tick object churn within
  each block (e.g. `Danger`↔`Social`↔`Hunger` swapping identity at ticks past
  the fix point) is hysteretic fallout, not a second defect: `arbitrate`
  carries an incumbent `Mode` between ticks, so no longer mis-pursuing a
  satisfied `Fatigue` at tick 4 changes which drive becomes incumbent there,
  which then persists forward under hysteresis. The other 8 sampled
  creatures' blocks are byte-identical.
- `clients/game/core/tests/fixtures/session-seed-14-carrying.json`: **all 58**
  `sensed.present[].felt` strings moved, uniformly, from `"looks lost, unsure
  where to turn"` to `"seems content"` — **`AffectLabel::Lost` → `Content`**,
  not `Frustrated`. Same root cause, the OTHER label the blocked branch can
  emit: `label = Frustrated if believed_water.is_some() else Lost`
  (`liveness.rs`, the blocked-branch arm). A creature with no known water
  source hits `Lost` instead of `Frustrated` on the identical spurious-active-
  drive path, so this fix's blast radius is wider than "Frustrated about
  Fatigue" specifically — it corrects both mislabelings the same blocked
  branch can produce. Not predicted in the traced root cause as written, and
  worth carrying forward: a fix framed around one label of a shared branch
  should check the branch's other exits before declaring scope.
- No other declared generated path moved (`docs/generated-paths.txt` diff is
  otherwise empty besides the ordinary `docs/timings.md` ledger rows from the
  `rebaseline`/`gate-commit` runs themselves).

**Verification run.** `cargo fmt --check`: clean. `make gate-commit`: green
(rc=0, 141.4s). Full `cargo nextest run` (`-p hornvale-vessel`, 1234 tests;
`-p hornvale-lab`, 543 tests, including `health_calibration`'s 19-test suite
and the affect-trace golden's own coverage-floor assertions) and
`cargo test --manifest-path clients/game/core/Cargo.toml` (fixture-consuming
tests, including `the_carrying_fixture_names_a_thing_in_hand`): all green
against the regenerated fixtures.

**Ideonomy passes / overturns:** none; TDD execution of #8's ruling.

**Capture actions:** this entry; task-0b-report.md in scratch per the
dispatch contract; the Lost/Frustrated dual-exit finding flagged for review
since it broadens the fix's stated scope.

---

## #10 [R] — Task 0b, fix round 1: a doc comment falsified by its own fix, and a traced arousal shift

Review came back APPROVED with one Important finding (fix) and one item to
explain, not fix.

**Finding 1 (Important), fixed.** `Fatigue::act()`'s doc a few dozen lines
from the predicate itself still read "keeps this drive engaged through the
whole off-phase whatever its urgency — which is a stronger guarantee than a
second threshold would have been, and it is why one is not authored here."
Both clauses were false after the fix: the predicate now DOES consult
urgency, and a threshold WAS authored at that exact site. Rewritten to keep
the history rather than erase it — the reasoning was sound under its own
unstated assumption (a creature entering the off-phase has fatigue to
repay), and the fix is stated as what changed and why, not as though the
threshold had always been there. **This is the same shape as the bug
itself**: `fatigue_rise_registry`'s `xorn` row was justified by "the walk's
ametabolic gate already excludes exactly that kind from the REST drive," an
invariant that later moved (decision 0976, xorn `Absent` → `Unmodelled`)
with nothing marking the move — this doc comment was a second instance of
exactly that pattern, caught one fix round later rather than a campaign
later. Worth carrying forward as a review habit: when a predicate changes,
grep for prose describing its OLD guarantee, not just its call sites.

**Finding 2, traced, not fixed — the answer is (a).** Coordinator asked why
`rust-monster`'s arousal at tick 2 moved (`0.34978789` → `0.34965155`, label/
valence/object unchanged) given arousal is a pure function of physical state
with no dependence on `active[]`/hysteresis, implying the two runs' physical
trajectories had already diverged by tick 1 or 2.

**CORRECTED 2026-09-11 by the fix round's re-review, which diffed the fixture
rather than trusting this paragraph.** An earlier draft of this entry said the
arousal shift was "ONE tick before its own first label divergence (tick 4)".
**Both numbers were wrong.** Diffing `befa0df45^..befa0df45` over the
`rust-monster` block:

```
tick 2  arousal 0.34978789 -> 0.34965155   label/valence/object unchanged
tick 3  arousal 0.43227372 -> 0.45         label/valence/object unchanged
tick 4  arousal 0.45       -> 0.5          label/valence/object unchanged
tick 5  Frustrated -> Eager, -1.0 -> 1.0   <- FIRST LABEL DIVERGENCE
```

The first label divergence is **tick 5**, not tick 4; and tick 2 precedes tick
4 by *two*, not one. **The conclusion is unaffected** — arousal moves at tick
2, three ticks ahead of any label change, which is exactly the evidence for
(a). Only the arithmetic narrating it was wrong.

**Worth recording as its own lesson**, because it is the day's most-repeated
shape in miniature: a count stated in prose about an artifact, not reconciled
against the artifact's own rows. The day↔tick mapping the same entry uses for
the *position* claim was checked and is correct (`day N` post-increment is
fixture `tick N-1`, so day 3 = tick 2) — so one index claim in this entry was
verified and the neighbouring one was not.

**Traced directly, not inferred.** Added temporary debug instrumentation
(never committed): (1) inside `arbitrate`'s `active` computation, an
`eprintln!` gated on `HV_DEBUG_FATIGUE` printing every drive's
`u`/`awake`/`normally`/`active` for one entity; (2) in
`windows/lab/src/health.rs`, a print of `rust-monster`'s entity id and its
committed `agent_position` at each simulated day. Ran the affect-trace test
once with the CURRENT (fixed) predicate, then temporarily reverted the one
line (`!awake || normally`, the pre-fix form) and re-ran, capturing the same
positions, then restored the fix immediately and re-verified `git diff`
showed only the intended predicate line. Direct comparison of `agent_position`
at each day:

```
day 1: OLD and NEW identical      path [...,1,0,2,2,1]
day 2: OLD and NEW identical      path [...,1,0,2,2,1]
day 3: OLD  path [...,1,2,1,0,2]
       NEW  path [...,1,2,1,2,0]   <- DIVERGES
```

Day 3 is tick 2 (the trace pushes after `day += 1.0`, so day 1 = tick 0).
**Position itself diverges starting exactly at tick 2** — confirming (a):
an earlier Hold-vs-move decision changed, and the body was in a different
place by tick 2, with the label/valence/object at that tick rendering
identically only because Danger was still the loudest/pursued drive in both
runs (the position shift moved the physical inputs Danger's own urgency
reads, producing the small arousal delta without changing which drive won).
The debug log also showed WHY a divergence was possible at all:
`rust-monster` (ordinary `0.3` rise rate, not `xorn`) hit `u == 0` while
asleep repeatedly across the 40-tick run, not only at genesis — direct,
logged confirmation that this is the GENERAL case reproducing in committed
production data, additional to what Task 0b's report already inferred from
the label-only diff.

**All temporary instrumentation removed before verification.** `git diff
windows/lab/src/health.rs` is empty; `git diff windows/vessel/src/liveness.rs`
contains only the doc-comment change (predicate line byte-identical to the
shipped fix, reconfirmed by re-running
`a_fully_rested_creature_asleep_is_not_frustrated_about_fatigue`,
`xorn_asleep_is_never_frustrated_about_fatigue`, and the affect-trace golden,
all green with the golden UNCHANGED, as expected for a doc-only diff).

**Verification run.** `cargo fmt --check`: clean. `docs/audits/plumb-roster.md`
regenerated (two line-number-only rows moved, from the doc comment growing) —
`make gate-commit` failed once on this drift, then passed green after
regenerating. Full `cargo nextest run -p hornvale-vessel` (1234 tests): green.

**Ideonomy passes / overturns:** none; review-response tracing.

**Capture actions:** this entry; fix-round report appended to
`.superpowers/sdd/2026-09-11-the-trencher/task-0b-report.md`.

---

## #11 [G5] — Task 0b complete

**Fix round 1 closed, both findings resolved**, commit `baa78cf4e`.

- **Finding 1 ADDRESSED.** `Fatigue::act()`'s doc rewritten to describe the
  predicate as it now stands, naming the old text and the unstated assumption
  that made it sound rather than erasing the history. The re-review checked
  specifically for a **third** instance of the invariant-citing pattern and
  found none.
- **Finding 2 ADDRESSED as an explanation**, and its narrating arithmetic
  corrected above by the re-review.
- **Instrumentation removal CONFIRMED CLEAN**, four independent ways: `git
  status`, `git diff HEAD` over `windows/vessel/src` and `windows/lab/src`, a
  grep for debug prints and flags, and the predicate line read back
  byte-identical to the shipped fix.

**That last check is why the round had a re-review at all.** The change was a
doc comment — the textbook "too small to review" case — but the investigation
behind it ran a **revert-and-compare**, temporarily restoring the buggy
predicate. A revert left in place would have been catastrophic and completely
silent: every test would pass, against goldens regenerated under the old
behaviour. "I removed my instrumentation" is a claim, and this is the round
that checks it.

**Task 0b: complete** (commits `0bdfbef3a..baa78cf4e`, review clean after one
fix round).

**What Task 0b actually found, for the chronicle.** The defect was never
`xorn`-specific. A synthetic fully-rested creature reproduced it with no
`xorn` involved; `rust-monster` — an ordinary `0.3`-rise-rate kind — moved in
the committed affect golden and was measured hitting `u == 0` while asleep
**repeatedly through the run**; and all **58** creatures in the committed
client session fixture moved from `Lost` to `Content`. Every sleeping creature
in the world was rendering to a player as *lost*, and had been.

It was found because Nathan ruled that a rock-eater ought to have a
metabolism.

---

## #12 [R] — Task 1 executed: `TrophicMode` factored into three axes

**The three types, for Tasks 2, 4 and 9-12 to consume verbatim — do not
guess these names.** All defined in `domains/species/src/lib.rs`, replacing
`TrophicMode` at what was line 2638 (Task 0 had already shifted it once):

```rust
pub enum EnergySource {
    Phototrophic,
    Chemotrophic,
}

pub enum ElectronDonor {
    Lithotrophic,
    Organotrophic,
}

pub enum CarbonSource {
    Autotrophic,
    Heterotrophic,
}
```

`BiosphereTraits`'s single `pub trophic_mode: TrophicMode` field became three:
`pub energy_source: EnergySource`, `pub electron_donor: ElectronDonor`,
`pub carbon_source: CarbonSource` — same names as the types, lowercased with
underscores, no abbreviation.

**The mapping from every kind's old `TrophicMode`, applied uniformly, per
Step 1's reading of the roster and its variants' own doc comments:**

| old `TrophicMode` | `energy_source` | `electron_donor` | `carbon_source` | carriers |
|---|---|---|---|---|
| `Heterotrophic` | `Chemotrophic` | `Organotrophic` | `Heterotrophic` | 35 kinds (every ordinary animal/people) |
| `Phototrophic` | `Phototrophic` | `Lithotrophic` | `Autotrophic` | `treant`, `twig-blight`, `shrieker` |
| `Chemotrophic` | `Chemotrophic` | `Lithotrophic` | `Autotrophic` | `xorn` alone |
| `Absent` | — not expressible as a triple, by design — | | | zero carriers (unaffected) |

No kind's mapping was ambiguous. Every one of the three live `TrophicMode`
values reads unambiguously off its own doc comment as a single point in the
microbiology trichotomy (energy × donor × carbon): `Heterotrophic` names an
ordinary consumer (chemoorganoheterotroph), `Phototrophic` names a
photosynthesizer whose own doc already said "plant-folk/fungal analogue"
pulling energy from light with no donor/carbon ambiguity (every real
phototroph in the shipped sense is a photolithoautotroph), and
`Chemotrophic`'s own doc named `xorn` explicitly as "a thing that burrows
through stone and eats mineral and a chemical gradient... a chemolithotroph"
— donor and carbon source both stated in the variant's own prose. `Absent`
had zero carriers, so no mapping question arose for it at all; it is simply
not one of the three sanctioned triples (see ledger #4/#5, already ruled).

**`SANCTIONED` decouples from `ThermalStrategy` entirely, and this is a
design decision beyond what ledger #4/#5 ruled, made reading the task
brief's own illustrative code.** The old two-axis `SANCTIONED` paired
`(ThermalStrategy, TrophicMode)` because `TrophicMode::Absent` needed to
pair with `ThermalStrategy::Absent` to close the ghost/construct/undead
corner — the one place the two axes were coupled. Decision 0976 already
removed that coupling (`TrophicMode::Absent` — now no axis's `Absent` at
all — sits outside the triple per ledger #4). With the coupling gone,
`SANCTIONED` is now `&[(EnergySource, ElectronDonor, CarbonSource)]` alone,
**3** entries (down from the old 5, which counted `(Absent, Absent)` as a
row that no longer exists in this shape):

```rust
const SANCTIONED: &[(E, D, C)] = &[
    (E::Chemotrophic, D::Organotrophic, C::Heterotrophic), // chemoorganoheterotrophy
    (E::Phototrophic, D::Lithotrophic, C::Autotrophic),    // photolithoautotrophy
    (E::Chemotrophic, D::Lithotrophic, C::Autotrophic),    // chemolithoautotrophy (xorn)
];
```

The count-assertion guard from `metabolic_pairs.rs` (the one that caught the
silent widening when `(Unmodelled, Chemotrophic)` was added and `treant`
flipped) is preserved in this three-axis form: `every_kind_is_pinned_to_its_
metabolic_triple`'s final assertion pins `SANCTIONED.len() == 3`.

**Tests, renamed for the new shape (no test name was mandated beyond the
brief's Step 2 illustration):**
- `every_kind_carries_a_sanctioned_combination` — brief's Step 2, verbatim
  shape.
- `chemolithoautotrophy_is_witnessed_by_xorn_alone` — replaces
  `chemotrophic_is_declared_and_unwitnessed`; same witness-count discipline,
  now naming the actual rare triple (xorn's) rather than the old
  `Chemotrophic` trophic value alone, which under the new axes is also
  carried by all 35 ordinary heterotrophs (an animal's energy source *is*
  chemotrophic — the axis alone no longer distinguishes it from xorn; only
  the full triple does).
- `every_kind_is_pinned_to_its_metabolic_triple` — replaces
  `sanctioned_thermal_keys_are_pairwise_distinct`; same exhaustive
  set-equality + per-kind pin discipline, over the triple instead of the old
  single `TrophicMode` value, plus the `SANCTIONED.len() == 3` guard.

**A discrepancy in the controller's own measured surface, found by the
compiler as instructed ("If cargo check --workspace --all-targets implies a
materially different surface, STOP and report").** The controller's
resolution said `windows/sentiment`'s only exposure was `#[cfg(test)]`
fixtures. `cargo check --workspace --all-targets` after this task's change
shows that is only PART of the story: `windows/sentiment/src/lib.rs` carries
a genuine **production** field, `PeopleTraits::trophic_mode: TrophicMode`
(struct at ~line 61, constructed at ~line 141 from `bio.trophic_mode`), and
`windows/sentiment/src/axes.rs:271` is the `#[cfg(test)]` site the
resolution named. `PeopleTraits`'s own doc already says "Nothing in this
crate reads it" (THE GOSSAN) — so it is dead weight carried through, not a
behavioural reader — but it is still a real struct field outside
`#[cfg(test)]`, and whichever task fixes `windows/sentiment` needs to touch
`src/lib.rs` as a fourth production file, not only its test fixtures. Flagged
here rather than fixed, since `windows/sentiment` is out of this task's
declared scope (`domains/species/src/lib.rs` and `metabolic_pairs.rs` only).

**`cargo check -p hornvale-species --all-targets`: clean, 0 errors.**
`cargo check --workspace --all-targets`: **6** `error[...]` diagnostics
(3 in `hornvale-sentiment`'s lib + lib-test builds: two `E0432` unresolved
imports at `windows/sentiment/src/lib.rs:31` and `axes.rs:271`, one `E0609`
missing field at `lib.rs:141`; 3 more in `hornvale-worldgen`'s lib +
lib-test builds, all the same site, `windows/worldgen/src/lib.rs:2737`,
`bio.trophic_mode != hornvale_species::TrophicMode::Phototrophic`). Every
crate depending on `hornvale-worldgen` (`windows/lot`, `windows/lab`,
`windows/vessel`-adjacent consumers, `cli/`) could not be attempted at all
in this run, because cargo cannot type-check a dependent of a crate that
fails to build — not because they carry additional undiscovered sites. This
matches the controller's own count (47 sites / 49 arms across
`domains/species/src/lib.rs`, `windows/lot/src/slots.rs`,
`windows/worldgen/src/lib.rs`, plus `windows/sentiment`), with the one
addition (`windows/sentiment/src/lib.rs`'s production field) noted above.

**Rebaseline (the empty-diff proof) is BLOCKED, not run, and not "empty" by
inference.** `make rebaseline` shells out through `scripts/regenerate-
artifacts.sh`'s `run()` helper, which is `cargo run -q -p hornvale ...` —
the CLI, which depends on `hornvale-worldgen`, which does not compile right
now (see above). The workspace-wide proof this task's brief asks for
literally cannot execute until whichever task fixes
`windows/worldgen/src/lib.rs:2737` lands. What COULD be checked, and was:
`domains/species`'s own suite is green end to end (79/79, including the
three tests above), and specifically `life_history_golden`'s two tests
(`every_kinds_life_history_is_frozen`, `the_life_history_table_is_not_
vacuous`) pass unchanged against the existing byte-golden fixture — that
golden is driven by `thermal_strategy` and `mass` alone (untouched by this
task) and reads no trophic field at all, so it cannot move from this
change and didn't. The exhaustive `every_kind_is_pinned_to_its_metabolic_
triple` test is the stronger, machine-checked version of the same claim the
brief wants a rebaseline diff to demonstrate: every one of the registry's 39
kinds maps to exactly the triple this ledger's table above says it should,
checked by name, not sampled. The workspace-level rebaseline (almanacs,
elevation maps, etc.) should be re-attempted once `windows/worldgen` and
`windows/lot` are fixed — nothing here predicts it will move anything (the
mapping is a pure bijection on the old `TrophicMode` value), but it has not
been observed, so it is not claimed.

**`make gate-commit` currently REDS, on `clippy`, at exactly the two files
named above — expected and by design, per the task brief's own Step 5
("Other tests referencing TrophicMode will fail to compile — that is Task
2's work... Do not hunt for sites by grep").** Everything else in the gate
that COULD run was run and is green: `cargo fmt --check` (workspace-wide),
`cargo clippy -p hornvale-species --all-targets -- -D warnings` (scoped,
clean), the three `check` tools (`type-audit`, `placement-audit`, `plumb` —
all 0 findings against this change), and all three report-freshness checks
(`type-audit-report`, `placement-audit-report` green with no drift;
`plumb-report` was stale on pure line-number churn from this task's edits —
regenerated and now green, `docs/audits/plumb-roster.md` included in this
commit). The sub-floor nextest tier was not attempted, since it also builds
`--workspace` and would fail for the identical reason.

**Files touched, beyond the brief's two.** `docs/audits/plumb-roster.md`
(regenerated, line-number-only diff) and this ledger entry, per the brief's
own Step 6 instruction to include the ledger in the commit.

**Ideonomy passes / overturns:** none; execution of Task 1 per the brief and
ledger #4/#5's prior rulings, plus one design decision (decoupling
`SANCTIONED` from `ThermalStrategy` entirely) made reading the brief's own
illustrative Step 2 code as the intended shape rather than guessing a
4-tuple was wanted.

**Capture actions:** this entry (type/variant names for Tasks 2, 4, 9-12);
the `windows/sentiment/src/lib.rs` production-field discrepancy flagged for
whichever task or review picks up `windows/sentiment`; the blocked
rebaseline flagged for a re-run once `windows/worldgen`/`windows/lot`
compile again.

**UPDATE, per ledger #13's ruling: Tasks 1 and 2 land in ONE commit, and
everything BLOCKED above is now done.** This entry's BLOCKED analysis stands
as written — it is what was true at the time and it is why the ruling
exists — but every paragraph above describing something as blocked, not
run, or a controller error to fix later is superseded by the work below,
completed in the same commit as Task 1.

**The four production files (three named plus the corrected fourth from
ledger #13) are migrated:**
- `windows/worldgen/src/lib.rs:2737` (`prey_pressure_from`) —
  `bio.trophic_mode != TrophicMode::Phototrophic` became
  `bio.energy_source != EnergySource::Phototrophic`. A pure rename: the
  three kinds this excludes from the prey base (treant, twig-blight,
  shrieker) are identical before and after, since `EnergySource::Phototrophic`
  is carried by exactly the same three kinds `TrophicMode::Phototrophic` was.
- `windows/lot/src/slots.rs`'s `diet()` — **the one site the ruling asked to
  be checked by hand, not approximated.** The old match had FOUR arms keyed
  on a single `TrophicMode` value, and one axis alone no longer distinguishes
  them: under the split, `EnergySource::Chemotrophic` is carried by every
  ordinary heterotroph too (an animal's energy source is chemical), not by
  `xorn` alone. So the new match keys on the FULL TRIPLE
  `(energy_source, electron_donor, carbon_source)`, with three explicit arms
  reproducing the exact old text for the exact old selected sets
  (chemoorganoheterotroph → "ate other living things…"; photolithoautotroph →
  "took its energy from light"; `xorn`'s chemolithoautotroph → "took its
  energy from chemical gradients in rock and water"), plus a wildcard arm.
  The wildcard is `unreachable!()`, not approximated text — there is no
  surviving state for the old `Absent` arm ("ate nothing at all") to
  describe, since `Absent` is unrepresentable on any of the three new axes
  (ledger #4/#5) and every kind `ctx.components.biosphere` can return is
  drawn from `hornvale_species::biosphere_registry()`, which
  `every_kind_carries_a_sanctioned_combination` already guards against ever
  producing an unsanctioned triple. This is the same shape the old code
  already used: `TrophicMode::Absent` had zero carriers too, so that arm was
  always dead in practice, just not spelled `unreachable!()`.
- `windows/sentiment/src/lib.rs` — `PeopleTraits::trophic_mode: TrophicMode`
  (the field ledger #13 found) became the same three fields
  `BiosphereTraits` got, migrated as a faithful pass-through per the ruling's
  instruction: "nothing in this crate reads it" stays true field-for-field,
  and whether the pass-through should exist at all stays a separate,
  undecided question.
- Plus the mechanical remainder found by the compiler, all doc-comment or
  synthetic-fixture updates with no selection-set question of their own:
  `windows/sentiment/src/axes.rs`, `windows/sentiment/tests/suite/axes.rs`,
  `windows/sentiment/tests/suite/judgment.rs` (all three: a `#[cfg(test)]`
  synthetic `PeopleTraits`, `Heterotrophic`'s triple substituted in),
  `windows/worldgen/tests/suite/underworld_separation.rs` (two synthetic
  `BiosphereTraits` fixtures, mountain-dwarf and duergar, both
  `Heterotrophic`'s triple), and one stale doc-comment cross-reference in
  `windows/worldgen/tests/suite/deep_realm_rehome.rs`.

**`cargo check --workspace --all-targets`: 0 errors** (was 6, all in the two
files above). **`cargo clippy --workspace --all-targets -- -D warnings`:
clean.** **`HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast`:
6070/6070 passed, 0 failed, 248 skipped** (1345.212 s). **`HV_TEST_OK=1
cargo test --workspace --doc`: green.** **`make gate-commit`: green, rc=0**
(460 tests via the sub-floor tier, 43.162 s wall).

**`make rebaseline`'s empty-diff proof: NOT literally empty, and here is
exactly what moved and why.** `git diff --exit-code` against
`docs/generated-paths.txt`'s paths shows exactly one file,
`book/src/gallery/generated/the-lot-seed-42.md`, 10 lines changed (one per
drawn lot). Every changed line is a bibliography-style source citation this
task's own edit to `slots.rs::diet()` renamed on purpose:
`- [N] derived: species::BiosphereTraits::trophic_mode (the people's
authored trophic mode)` became `- [N] derived: species::BiosphereTraits::
{energy_source,electron_donor,carbon_source} (the people's authored
metabolic triple)`. That field name is gone, so the old citation string
would now name a field that does not exist — leaving it unchanged would be
the actual defect. **Confirmed separately that no kind's DIET NARRATIVE
moved**: `grep -n "ate other living things" book/src/gallery/generated/
the-lot-seed-42.md` returns the identical ten lines, byte-for-byte, in both
the pre- and post-rebaseline trees (git did not flag them, which is the
positive control — a real text change on any of those lines would have shown
in the diff above and did not). So the branch this task's brief asked to
distinguish resolves as: the retyping was faithful (no kind's metabolism or
narrated meaning changed); the one thing that moved is the citation
mechanism's own self-description of which field it reads, which is required
to move by the same retyping and is not a finding about any kind.

**Ideonomy passes / overturns:** none for this update; execution of ledger
#13's ruling.

**Capture actions:** this update (supersedes the BLOCKED-era capture actions
above); `docs/superpowers/plans/2026-09-11-the-trencher.md`'s Task 2 section
already carries the ruling and the corrected four-file count (written
alongside ledger #13, not duplicated here).

---

## #13 [Ruling] — Tasks 1 and 2 share a commit; and my blast-radius measurement was wrong

**The implementer BLOCKED correctly**, and did three right things at once: it
did not bypass the pre-commit hook, it did not expand scope into another
task's work unasked, and it reported options rather than choosing one.

**RULING: Tasks 1 and 2 land in ONE commit.** The boundary was mis-drawn and
that is my defect. Removing `TrophicMode` breaks its consumers and
`make gate-commit` is workspace-wide, so **no green commit exists between
these two tasks.** The plan asserted one could. A task is supposed to be the
smallest unit that carries its own test cycle; "define the type" cannot carry
one when the type and its consumers are a single compile unit.

**Alternative discarded:** leave `TrophicMode` as a deprecated alias so Task 1
commits alone. It buys a commit boundary at the price of a temporary alias,
and a deprecated alias is precisely the kind of thing that outlives its
deprecation. *Cost if wrong: one larger commit to review instead of two
smaller ones.*

**AND MY MEASUREMENT WAS WRONG — four production files, not three.** I wrote,
in the spec and twice in the plan, that `windows/sentiment`'s five
`TrophicMode` mentions were "ALL inside `#[cfg(test)]` — fixtures, not
consumers." Verified now:

```
windows/sentiment/src/lib.rs:71    pub trophic_mode: TrophicMode,    <- PRODUCTION
windows/sentiment/src/lib.rs:141   trophic_mode: bio.trophic_mode,   <- PRODUCTION
windows/sentiment/src/lib.rs:175   #[cfg(test)]                      <- tests begin here
```

**How I got it wrong is the reusable part.** I read `axes.rs` carefully — saw
its `#[cfg(test)]` at 264 and its uses at 269 and 308, and correctly concluded
test-side. Then I reported its **sibling** `lib.rs` in the same sentence
without checking whether its three mentions fell before or after the
`cfg(test)` line I had *already printed to my own screen*. I had the number
and did not use it. **Verified one file, inferred the neighbour, reported
both with equal confidence.**

This is the same shape as the day's other text defects and the one I have now
committed most often: a claim about a set, where part of the set was checked
and the rest was assumed to resemble it.

**Consequence taken:** the plan now states the four-file surface, and Task 2's
dispatch tells the implementer to expect `PeopleTraits` to need the same
three-field treatment `BiosphereTraits` got. Its doc notes "nothing in this
crate reads it (THE GOSSAN)" — a pass-through whose reader lives in
`hornvale_worldgen` — so it is migrated faithfully rather than removed;
whether a pass-through field should exist is a separate question.

**Ideonomy passes / overturns:** none; a structural ruling plus a correction
of my own measurement.

---

## #14 [G5] — Tasks 1+2 complete: the trichotomy lands

**Approved** (commit `1c234398b`), spec ✅, no Critical or Important findings.
Workspace `cargo check` 6 → 0; 6070/6070 tests green.

**The review verified exhaustively rather than sampling**, which is what the
task needed: it extracted all 39 `(kind, TrophicMode)` pairs from the registry
at `53f8344c4` and all 39 triples at `1c234398b` and diffed them
programmatically — **0 mismatches**, independently reproducing the mapping and
matching `metabolic_pairs.rs`'s own `PINNED` table.

**The proof held, and its shape is worth recording.** The rebaseline was NOT
empty: `book/src/gallery/generated/the-lot-seed-42.md` moved 10 lines. Every
one is a derived-fact **citation string** naming the old field — a citation
pointing at a field that no longer exists would be the actual defect. The
review confirmed by reading the diff that no diet sentence moved at all.
**A non-empty diff that is entirely citations is a better outcome than a
literally empty one**, because it proves the citation mechanism is live rather
than decorative.

**`shrieker`'s known corpus error was PRESERVED, and that was the point.** It
remains a photolithoautotroph despite eating `DETRITUS`, with
`ThermalStrategy::Unmodelled`'s "a corpus error left standing on purpose,
because a data fix inside a structural rename hides both" untouched. A rename
that quietly corrected it would have hidden two changes in one diff.

**The split justified itself in `slots.rs`.** `diet()` used to key on
`TrophicMode::Chemotrophic` alone; it cannot any more, because chemo- is now
**shared between ordinary heterotrophs and `xorn`**. The match moved to the
full triple and selects identical sets. The old flattened value was conflating
"gets energy from chemistry" with "eats rock rather than creatures", and
nothing could see it until the axes came apart — which is the trichotomy's
whole thesis, demonstrated by the first consumer to meet it.

**The `unreachable!()` is a DISCIPLINE guarantee, not a type-level one**, and
the review traced it rather than accepting it: every `WorldComponents` path
routes through `assemble()`, which reads the live registry unfiltered, and
`every_kind_carries_a_sanctioned_combination` iterates that registry rather
than a pinned list, so later kinds are covered automatically. But
`BiosphereTraits`'s fields are `pub`, so nothing in the type system forbids a
hand-constructed unsanctioned triple outside the registry. Consistent with how
this repo enforces invariants generally (test ratchets, not types); recorded
as the soft spot rather than a defect.

## Carried into Stage 2: a name collision this campaign is about to worsen

`hornvale_species::EnergySource` (new — `Phototrophic`/`Chemotrophic`) shares
its name with the pre-existing `hornvale_worldgen::energy::EnergySource` (the
seven chemistries — `Serpentinization`, `Radiolysis`, `SulphideOxidation`, …).

Pre-existing and not Tasks 1+2's to fix. **But this campaign puts them in the
same room:** Stage 2 reworks the worldgen seven into metabolites, Stage 4
authors kinds on the species two, and someone will need both in one file.

**Which is misnamed is answerable, not a matter of taste.** The species one is
the **term of art** — microbiology's trichotomy is literally *energy source*
(photo/chemo) × *electron donor* × *carbon source*. Worldgen's seven are
energy-yielding **reactions**, not sources in that sense.

**Ruling: defer the rename into Stage 2**, where that vocabulary is being
redesigned anyway. Renaming during a rework costs a hunk; renaming standalone
costs a churn commit across a file Stage 2 is about to rewrite. *Cost if
wrong: Stage 2's implementer meets two `EnergySource`s and has to disambiguate
imports before the rename lands.*

**Task 1+2: complete** (commits `53f8344c4..1c234398b`, review clean).

---

## #15 [Q] — A faithful bijection is blind to the distinctions the new vocabulary exists for

**The Tidemark's finding, recorded with attribution because it is a defect
class rather than a note about their campaign**, and it lands on this one
harder than on theirs:

> A faithful bijection is faithful to the OLD vocabulary's distinctions, and
> is therefore exactly blind to the ones the new vocabulary was built to
> express.

**Their worked example is what makes it undeniable.** Tasks 1+2's migration
maps every old `Chemotrophic` to chemo/litho/auto — correct, because that is
what preserves rename behaviour. But:

- their **vent commensal** is a people that EATS chemosynthetic bacteria:
  chemo-**organo**-hetero;
- their **tube worm**, hosting chemoautotrophic symbionts, is the one that is
  genuinely chemo-**litho**-auto.

**The two kinds the bijection collapses into one triple are precisely the two
the new axes exist to distinguish.** And it would collapse them **silently** —
the bijection is behaviour-preserving by construction, so nothing goes red.
Applied mechanically, it would ship their commensal as an autotroph that eats
nothing.

**Taken as a binding constraint on this campaign's Stage 4**, which authors
8-20 organisms against these axes. Any authoring that reaches for the
migration table as a shortcut inherits exactly the blindness the split was
meant to remove. **The bijection is a MIGRATION table for existing rows, never
an AUTHORING table for new ones**, and Stage 4's tasks must say so where an
implementer would otherwise reach for it.

**A correction I owed them and have sent.** My migration notice said the
bijection let them "translate your slate table mechanically". Given the above
that sentence is wrong for exactly the kinds it matters for, and I have
retracted it to them in those terms.

**Their question, answered from the file rather than from my review's summary
of it:** `chemo-organo-hetero` **is** already sanctioned — it is the 35
ordinary kinds' triple. Their whole roster migrates without touching
`SANCTIONED`, and the count guard stays at 3. If they ever do need a fourth,
that is a real signal rather than paperwork.

**One thing worth keeping about method.** A mechanism's first genuine test
case turning out to be the one it gets wrong is worth more than a page of
reasoning about the mechanism. Tasks 1+2 shipped with 39/39 verified, a clean
review and an exhaustive programmatic check — and none of that could have
found this, because every one of those instruments asks whether the OLD
distinctions survived.

**Ideonomy passes / overturns:** none; a peer's finding, verified and adopted.

**Capture actions:** this entry; Stage 4's authoring constraint recorded above
for its tasks to carry; the retraction sent.

---

## #16 [Q] — SANCTIONED validates legality, never correctness

**The Tidemark raised a near-miss; verifying it closed that one and exposed
the unguarded neighbour.**

**Their concern:** `Unmodelled` and `Absent` are one word apart in prose and a
whole metabolic triple apart in the model. Their kelp tender's slate reads
"(unmodelled)" — written weeks before this split existed. Authored as
`Absent`, they reasoned, migration would silently erase a phototrophic
people's phototrophy.

**Verified, and it cannot happen — two independent reasons.**

1. **The three axes are MANDATORY fields**, not `Option`
   (`domains/species/src/lib.rs:3680`, `:3684`, `:3688`). A
   `BiosphereTraits` cannot be constructed without all three, whatever its
   thermal strategy. `metabolic_pairs.rs`'s header line "carries no metabolic
   triple at all" states ametabolism's **intent**, not the type's shape.
2. **It would not be silent regardless.** `no_living_kind_is_ametabolic`
   (`coverage.rs:178`) iterates the live registry and REDs on any carrier of
   `ThermalStrategy::Absent`, by name.

**A second-order effect worth recording:** that guard was written in Task 0
for an entirely different reason — decision 0976 ruling ametabolic life a
category error — and it now also protects another campaign's authoring
against a hazard nobody had in mind when it was written. **A guard that
encodes a real distinction protects uses its author never enumerated.**

## The neighbour, which has no net

**A kind mis-authored as `Absent` reds. A kind authored with the WRONG TRIPLE
does not.** Nothing checks that a phototrophic people is photo/litho/auto
rather than chemo/organo/hetero. `SANCTIONED` asks only whether a combination
is **legal**, and all three are legal.

**SANCTIONED validates legality, never correctness. A wrong-but-sanctioned
triple is exactly as green as a right one.**

This is #15's bijection finding in its sharpest form: the mechanical-migration
hazard and this one are the same defect, and **only the version with a wrong
*enum* is caught — the version with a wrong *value* is not.**

**Stage 4's constraint, in these words:** the only thing that catches a wrong
triple is someone asking, of each authored kind, whether the triple is **true
of the organism**. That is a reading, not a test, and no ratchet will do it.
Stage 4's tasks must require the reading explicitly rather than assuming
`SANCTIONED` covers it.

**One mechanical partial net exists and is worth considering in Stage 4:** a
test asserting that every kind whose niche carries a `PHOTOSYNTHATE` weight
also has `EnergySource::Phototrophic`, and the converse — **cross-checking two
independently authored fields against each other**. It catches the plant case
specifically. It does not generalize to all three axes, but the shape does:
wherever two authored fields imply each other, an agreement test turns a
reading into a ratchet. Raised to The Tidemark for their kelp tender as well.

**Ideonomy passes / overturns:** none; a peer's near-miss, verified, which
overturned its own premise and relocated the hazard.

---

## #17 [Q] — Load-bearingness is checkable only by ablation

**Two findings from The Tidemark, one of which corrects me and one of which
widens Stage 4.**

**THE CORRECTION, in their words:** *"a guarantee that exists somewhere is not
a guarantee that exists here."* I told them `no_living_kind_is_ametabolic`
protected their authoring. It does not — **it lives on `campaign/the-trencher`
and their tree returns 0 for it.** They acquire it at absorption, and their
Task 4 authors nine fauna before that. I verified the guard existed *in my own
tree* and asserted about theirs.

**Same root as my `sentiment` error earlier today** (#13): I read `axes.rs`
carefully, correctly concluded its uses were test-side, then reported its
sibling `lib.rs` in the same sentence without checking a line number already
on my screen. **Verified one thing, asserted about the adjacent one.** Twice in
a day, in both directions. The generalisation is theirs and it is better than
mine: *somewhere* is not *here*, and a tree is a "where".

## Their kelp tender, and why it is this campaign's own thesis inverted

```
lib.rs:5018   kelp-tender niche = (PHOTOSYNTHATE, 0.40), (MARINE_FORAGE, 0.60)
W5 measured   that PHOTOSYNTHATE weight moves capacity at 0 of 29,679 vertices
lib.rs:4986   PHOTOSYNTHATE is fed by `base_carrying`, which is TERRESTRIAL
```

**Coherently authored, legally sanctioned, passes the agreement cross-check I
proposed — and its photosynthesis does nothing.** Three green nets over an
inert weight.

**This is The Ceiling's finding inverted.** That campaign measured the
underworld as *richly derived and thinly read* — nine lithology fields, a
light ladder, and composition itself, all computed and reaching no consumer.
Theirs is a **consumer reaching no supply**. An axis with no producer and a
producer with no consumer are the same broken join seen from opposite ends,
and **neither end can see it alone**.

## The hierarchy, which I think is the day's durable form

```
  legality            checkable by a TABLE           (SANCTIONED)
  coherence           checkable by an AGREEMENT test (two authored fields implying each other)
  LOAD-BEARINGNESS    checkable ONLY by ABLATION     (remove it; did anything move?)
```

**Ablation is the only one that answers "does this do anything".** And the
sting survives: none of the three runs unless someone schedules it. Their kelp
tender is known decorative only because they instructed an implementer to
verify the weight moves the score rather than assume it. **Nothing would have
asked otherwise.**

**This campaign already built the instrument and did not notice it
generalises.** The Ceiling's M4 — forced into existence by The Staple D5B's
review, which found that my specified readout could not distinguish a working
`CHEMOSYNTHATE` weight from an ornamental one — is exactly per-weight
ablation, run for one kind.

**Consequence taken: plan Task 13's T2 now runs PER AUTHORED ORGANISM**, not
only for the chemotroph. If that proves too expensive at 8-20 organisms, the
task must **measure the cost and say so**, then take a sampled version
deliberately — not decide it is too expensive in advance, which is how a
measurement becomes an assumption.

**Offered and not absorbed:** whether a marine `PHOTOSYNTHATE` supply is
genuinely absent or merely unbuilt is a supply-side gap plausibly closer to
this campaign's Stage 2 than to their Task 4. Offered to carry it; not taking
it unasked.

**Ideonomy passes / overturns:** none; two peer findings, one correcting my
own claim.

---

## #18 [Q] — A missing JOIN is not a missing HALF, and this campaign was conflating them

**The Tidemark closed their own supply question by finding the producer was
never missing**, and the result corrects a sentence this campaign has been
repeating since The Ceiling:

```
waterworld.rs:584   plankton = light/(light+1), derived from attenuated light
readers             test assertions ONLY
marine_habitat.rs   zero references to plankton — it never crosses into scoring
```

**Producer present. Consumer present. Join absent.** Neither end is missing,
which is exactly why neither end looked broken.

**THE CORRECTION TO THIS CAMPAIGN'S OWN HEADLINE.** "The underworld is richly
derived and thinly read" has been applied to three findings as though they
were one defect. They are two kinds:

| finding | producer | consumer vocabulary | what is wrong |
|---|---|---|---|
| lithology's nine fields | exists | **absent** | a missing HALF |
| composition | exists | **absent** | a missing HALF |
| the light ladder | exists | **exists** (the kernel's `LIGHT` axis) | a missing **JOIN** |

**A missing half needs vocabulary built; a missing join needs a wire run.**
Different costs, different owners, different odds of being closed by accident.
Spec §2 corrected to split them; §6 already scopes the light join out, and now
says why it is a different animal rather than the same one deferred.

## The hierarchy, completed (their fourth line)

```
  legality          checkable by a TABLE
  coherence         checkable by an AGREEMENT test
  load-bearingness  checkable ONLY by ABLATION
  a MISSING JOIN    checkable by NONE of the three -- because both sides pass
```

Ablation tells you a weight is decorative. **It cannot tell you whether the
supply exists-but-unwired or is genuinely absent.** They found theirs by going
looking after the null — "curiosity, not method", in their words.

**A candidate fourth instrument, offered as a candidate and NOT built here.**
Their plankton's tell is stated in their own message: *readers — test
assertions only*. That is mechanically detectable. Not by `dead_code`, which
is satisfied because the field IS read; but a check of the form **"a derived
value whose only readers are `#[cfg(test)]` or `tests/`"** would have flagged
it, and would flag any future overlay stock that is computed, asserted about,
and never joined. It is the data-flow analogue of dead code, and this repo
already has the family — `seam-guard` asks whether a function's contribution
is pinned by any assertion, `type-audit` whether a boundary primitive carries
a verdict, both default-deny with declared waivers.

**Honest limits, which matter more than the idea:** it catches *some* missing
joins and never all (a value read once by production and then discarded would
pass); its false-positive rate is unknown and could make it useless; and it
needs a waiver mechanism on day one or it reds everywhere and gets ignored,
which this project has watched happen. **Written down as a candidate rather
than left as folklore; not built in this campaign, which is the food system.**

## What the whole exchange established about method

Between the two campaigns today: **five bounded-window errors, and not one was
caught by the side that made it.** Theirs were windows over text — a
`grep -A 18` concluding a field did not exist, a BSD `awk '/\bworld\b/'`
returning nothing because that regex engine has no `\b`, a roster-anchored
sweep reporting one kind's value for six. Mine were windows over **context** —
one file standing for its sibling (#13), one tree standing for another (#17).

**Same error, different substrate: a window mistaken for the thing.**

Every one was found by the other side re-running it. That is not a fact about
carelessness; it is a fact about what self-review structurally cannot do, and
it is the strongest argument this session has produced for two campaigns
talking rather than one campaign being thorough.

**Ideonomy passes / overturns:** none; a peer finding that overturned this
campaign's own framing of its headline.

---

## #19 [Q] — Two ordinary-tier tests have heavy-tier semantics, and nothing labels them

**Source:** The Tidemark (`campaign/the-tidemark`), unprompted, 2026-09-11.
Verified by them rather than assumed; not yet re-verified in this tree, and
that distinction is recorded deliberately — see #17, where asserting about a
peer's tree from my own cost me a correction.

Their six new peoples moved the census. Two tests now RED on the Mac and
**cannot be made green here**, because they compare against committed census
goldens that only lefford may author:

- `windows/lab/tests/suite/census_sentinel.rs`
- `windows/lab/tests/suite/tripwire.rs`

**The generalizable part.** CLAUDE.md's stated reason for keeping the heavy
tier OFF the stage-gate list is that a census-backed assertion "would red
predictably for the whole middle of any world-touching campaign." That
reasoning applies word for word to these two — and they are **not** heavy-tier
and carry **no `#[ignore]`**. So the property that earns a test its
heavy-tier placement is held by two tests that nothing marks, and the only
signal a reader gets is a red that looks exactly like a regression.

**What it costs this campaign.** Stage 4 authors 8-20+ organisms. That is a
larger census perturbation than six peoples, so these two will red here too,
harder, and earlier than I would have looked for a cause. Budgeting two known
reds through Stage 4 rather than discovering them at a gate.

**Ruling:** adopt their treatment — the reds stand until pre-merge close and
are **not** chased with a mid-campaign census refresh. A census refreshed in
the middle of Stage 4 is invalidated by the next organism authored hours
later, which is the same argument that keeps heavy off the stage list. The
refresh belongs at close, once, when the roster has stopped moving.

**The risk of that ruling, stated so it is not discovered later:** two
standing reds are two places a GENUINE regression can hide. Mitigation is to
record the expected red set by name now, before Stage 4 makes any, so a
**third** red is visibly novel rather than absorbed into "the census ones."

They are filing it as a `PROC-*` candidate at their close — either these
belong in `heavy:`, or the stage set needs a declared census-blocked
exemption. Not duplicating that row here, on the same reasoning as the
`TOOL-*` row in #18: the finder files it, I cite it.

**Ideonomy passes / overturns:** none; an adopted peer ruling with its risk
named.

---

## #20 [Ruling] — Task 3's brief is silent on the three things that make it land

Verification-before-dispatch (the `dispatching-hornvale-subagents` step 1),
run against `kernel/src/ecology.rs` before Task 3 went out. Budget ~3 minutes;
found four defects, three of which fail **silently**.

**A. `v1_basis()` is not optional, and the brief never names it.** The brief
says "add the axes" and its test checks only the consts' ids. A `pub const`
that is never appended to `v1_basis()` (`ecology.rs:146`) is invisible to
every consumer that iterates the basis — `dominant_axis`
(`domains/demography/src/niche.rs:77`), the herbivory fraction
(`coexist.rs:298`), `total_non_detritus` (`niche.rs:216`). The axes would
exist, compile, and be orphaned. **The brief's own test passes either way**:
it is blind to the omission it most needs to catch. This is the missing-JOIN
shape from #18, arriving one stage after I named it.

**B. `the_basis_ids_are_append_only` (`ecology.rs:686`) hard-pins
`vec![0, 1, 2, 3, 4, 5, 6]`.** Appending to the basis REDs it. That is the
guard working: it converts A's silent omission into a loud, deliberate edit.
But the brief mentions neither the test nor the literal, so the implementer
meets it as a surprise mid-task and may "fix" it by reverting the append —
which is precisely the wrong repair, and leaves the tree green and wrong.

**C. The brief's test sketch does not compile where it belongs.** It uses
`use hornvale_kernel::*;`, an external-crate path. `kernel/tests/suite/` would
accept that but needs a `#[path]` registration in `kernel/tests/suite.rs` the
brief does not mention. The right home is the **inline `mod tests`**
(`ecology.rs:602`) — beside the guard B says must be extended — which needs
`super::*` instead and costs no registration.

**D. The sketch's `>= 7` and dedup assertions are subsumed** by extending B's
dense-ascending literal, which is strictly stronger (it catches reorder and
insert, which a dedup cannot). The `CHEMOSYNTHATE.id == 6` assertion is
**not** subsumed and is kept: it is the peer-campaign guard from ledger #1.

**Ruling:** Task 3 extends `v1_basis()` and `the_basis_ids_are_append_only`'s
literal as part of the task, not as follow-up; the test lives inline with
`super::*`; the sketch's redundant assertions are dropped in favour of the
extended sequence pin plus the `CHEMOSYNTHATE` guard. Folded into the
dispatch as the controller's resolution of ambiguity.

**Cost if wrong:** low and visible — a mis-specified test home is a compile
error, and the sequence pin reds loudly. The expensive branch was A, and it
is the one nothing would have reported.

**One consequence the implementer must know, because appending is not inert.**
`coexist.rs:298` sums weights **over the basis** as a denominator. Widening
the basis changes that denominator for any niche carrying metabolite weight.
Harmless today (no niche carries one yet) and load-bearing from Task 4 on.

**Also noted, not fixed:** `windows/worldgen/src/lib.rs:1147` calls
`MARINE_FORAGE` "a sixth `v1_basis()` member". It is the sixth by position
(index 5) and the basis has held seven since `CHEMOSYNTHATE`. Ambiguous
rather than false, and about to be more so. Task 4 touches this file; fix the
wording there.

**Ideonomy passes / overturns:** none; a verification step with four findings.

---

## #21 [Ruling] — Stage 4's organisms must be an APPENDED accession cohort, and a live campaign already owns the peoples half

Found while submitting the Stage 1 boundary gate: `sluice-request` prints the
board's `hold-off` notices, and two of them are addressed to
`campaign/underworld-peoples` rather than to me. Read anyway — CLAUDE.md's
standing advice is that the half nothing mechanizes is two campaigns changing
the same idea, and reading a neighbour's chronicle is how that gets caught.

Three things bind Stage 4. None was in my spec.

### A. Adding a species kind re-deals every later name draw unless it is appended

`domains/language/src/accession.rs` holds `EPOCH_COHORTS`. A people's or a
creature's name is drawn from the lexicon, and **the proto-root walk consumes
draws in registration order**. Inserting a kind into an existing cohort shifts
every draw after it — so an addition does not append names, it re-deals the
hand from the insertion point, renaming existing kinds in every world ever
generated from this seed space. `the_additivity_law` and
`folk_sections_are_byte_unchanged` both catch it.

`campaign/underworld-peoples` hit exactly this: their duergars took `Dazha`,
the gully dwarfs' existing autonym, pushing the gully dwarfs to `Xabxat`.
Gate rc=2, five failures, one cause.

**This is not hypothetical for Stage 4 — it is the same operation.** Epoch 22
is `The Murrain, Task 1: the five pathogen species rows`, appended as a new
cohort for precisely this reason. Species rows go through accession. My 8-20+
organisms are species rows.

**Ruling:** Stage 4 appends ONE new cohort at the end of `EPOCH_COHORTS`,
with a comment in the house style of epochs 20-22 stating the campaign, the
task and why it is a fresh cohort rather than an edit to an earlier one. It
never edits an existing cohort. The plan's Stage 4 tasks do not currently
mention `accession.rs` at all; that is a plan defect to fix before Stage 4 is
dispatched, not during it.

**Epoch 23 is claimed** by `campaign/underworld-peoples` (unmerged, live).
Mine is 24 or later depending on merge order, which means the cohort index is
**not** something Stage 4 may hardcode from today's file — append, and let the
index fall where it falls.

### B. The cohort fix trades a rename for an autonym ambiguity

Injectivity in `assign_proto_roots` is per-CONCEPT within a family. Two
*species* sharing a root for the *same* concept — each people's own word for
"person", which is where an autonym comes from — is outside what it spans. So
an appended cohort can produce two peoples with one autonym in one world, and
nothing objects.

Smaller than a rename (it breaks a page, not a contract with every committed
world) and the right trade. But Stage 4 should expect it rather than discover
it, and `cli/src/repl.rs` already carries the render-time disambiguation
pattern for settlements.

### C. Census AFTER the last absorb, never before

From the notice to `campaign/the-wanderers`: a census measures the world at
the ref given, so running one while N commits behind main buys goldens that
may not describe the absorbed tree, and it is paid twice. `campaign/the-murrain`
paid that tax this morning.

Folds into #19's ruling and sharpens it: the close-time census refresh goes
**after** the final absorb of main, at that SHA, once.

### D. The scope collision, which is the part Nathan will want

Nathan's Stage 4 direction was "add settled peoples IMHO. We should already
have 4-5 underworld sentient species. Use 'em and/or add more," and he flagged
that dwarves were being readmitted elsewhere and "that might shrink the
campaign a bit."

Confirmed and more advanced than I assumed. `campaign/underworld-peoples` is
live and unmerged, and adds **four**: duergars, kuo-toas, mountain dwarfs,
svirfneblins. It is past its save-format fix and near merge.

**Ruling:** Stage 4 authors **no settled people that duplicates those four**.
It takes the "use 'em" half of Nathan's direction and spends its whole budget
on the biota — the fungi, the chemotrophs, the weird flora and fauna that are
this campaign's actual subject and that nobody else is building. If the
peoples land first, Stage 4 may give them metabolite-aware diets, which is a
JOIN between two campaigns' halves and strictly better than a fifth people.

**Cost if wrong:** low. Under-authoring peoples is recoverable by a successor;
authoring four that collide with a live branch is a merge conflict in a
save-format-contract file, which is not.

**Ideonomy passes / overturns:** none; three constraints read off a neighbour's
board notices plus a scope collision confirmed against the branch list.

---

## #22a [R] — Task 3 executed: the four metabolite resource axes

*(Numbering collision resolved at close: written as `#22`, which my own
acceptance below also took. Suffixed rather than renumbered so no citation
moves; content unchanged.)*

Ruling #20's corrections applied in full: `v1_basis()` extended, the
sequence pin in `the_basis_ids_are_append_only` widened to
`[0..10]`, the test written inline in `mod tests` with `super::*`, and the
sketch's redundant `>= 7`/dedup checks dropped in favour of the extended
sequence pin plus a new test asserting the join (each const is the axis
`v1_basis()` actually returns at its id, not merely a value with a matching
number) and the `CHEMOSYNTHATE.id == 6` guard.

**Four axes, ids 7-10, all `Field`.** Reaction→metabolite mapping taken
directly from spec §2's table, per the rule "two reactions both yielding H₂
feed one axis":

| id | const | label | kind | reactions |
|---|---|---|---|---|
| 7 | `HYDROGEN` | hydrogen | Field | Serpentinization + Radiolysis (both yield H₂) |
| 8 | `REDUCED_IRON` | reduced iron | Field | IronReduction |
| 9 | `REDUCED_SULPHUR` | reduced sulphur | Field | SulphideOxidation |
| 10 | `METHANE` | methane | Field | Methanogenesis |

`Geothermal` and `DetritalImport` are deliberately absent — spec §4.2 (ledger
#2) already routes them out of the food vocabulary, and Task 4 is where that
routing is wired, not Task 3.

**The `kind` argument, stated once because it is one argument repeated four
times.** All four are `Field`, not `Stock`, and the doc comment on each
argues it against the same counter-example: `MINERAL` (id 4) is also
rock-derived and is `Stock`, so "produced by rock chemistry" alone does not
settle it. What does: each of these four reactions runs *continuously*
wherever its rock/water precondition holds (per `energy.rs`'s own docs — a
stoichiometric `* moisture` multiply for Serpentinization/Methanogenesis, a
saturating trace-moisture gate for IronReduction/Radiolysis, a
connected-flow gate for SulphideOxidation), so what a consumer draws on is an
ongoing production *rate*, not an accumulating deposit `MINERAL` names.
Decisively: all four are a disaggregation of `CHEMOSYNTHATE` itself (the
aggregate ledger #1 keeps, `Field`), so consistency with the aggregate they
refine settles the question rather than merely arguing it by analogy.

**Re-exports.** `kernel/src/lib.rs`'s `ecology::{...}` list gained
`HYDROGEN, METHANE, REDUCED_IRON, REDUCED_SULPHUR`, re-sorted into the
existing ASCII-byte-order placement (case-sensitive: uppercase before
lowercase at the first differing byte) rather than appended out of order.

**The consequence ledger #20 flagged, re-confirmed rather than assumed.**
`coexist.rs:298` sums weights over `v1_basis()` as a denominator; widening
the basis from 7 to 11 members widens that denominator for any niche
carrying metabolite weight. No niche does yet (grepped — nothing in
`domains/species` weights ids 7-10), so this is inert today and becomes
load-bearing at Task 4/9-12, exactly as #20 predicted.

**What is NOT in this commit.** No organism, no supply, no wiring into
`windows/worldgen` — this task registers vocabulary only, per the brief's own
scope line. `windows/worldgen/src/lib.rs:1147`'s stale "sixth `v1_basis()`
member" wording (noted, not fixed, in #20) is untouched; it is Task 4's file.

**Verification.** `cargo nextest run -p hornvale-kernel -E 'test(basis) or
test(metabolite) or test(chemosynthate)'`: 4/4 passed, including the new join
test and the widened sequence pin. `make gate-commit`: reported in the
commit below.

**Ideonomy passes / overturns:** none; execution of #20's ruling plus a
mapping decision (which reactions feed which axis, and the axes' `kind`)
that #20 left to this task, resolved directly from spec §2's table and
`energy.rs`'s own reaction docs rather than invented.

**Capture actions:** this entry; task-3-report.md in scratch per the
dispatch contract.

---

## #22 [G5] — Task 3 complete: the metabolite axes, and ledger #20's join defect proved live

Commit `88373ff39`. Four axes registered, all four of #20's corrections
applied. `make gate-commit` rc=0; `hornvale-kernel` 383/383.

| name | id | kind |
|---|---|---|
| `HYDROGEN` | 7 | `Field` |
| `REDUCED_IRON` | 8 | `Field` |
| `REDUCED_SULPHUR` | 9 | `Field` |
| `METHANE` | 10 | `Field` |

`CHEMOSYNTHATE` keeps id 6.

**The `Field` argument is one argument, not four, and it is the right shape.**
The implementer did not reason "rock-derived, therefore ambient" — which would
have been wrong, since `MINERAL` is also rock-derived and is `Stock`. It
reasoned that all four are **a disaggregation of `CHEMOSYNTHATE` itself**,
which is already `Field`. A split of one axis into four inherits that axis's
kind unless something argues otherwise. Flagged by the implementer for a
second look, and I am leaving it flagged rather than resolving it here: Task 4
wires supply and is where a wrong `kind` would first show as a trophic cap
that does not bite.

**I VERIFIED THE JOIN TEST CAN RED, RATHER THAN READING IT.** #20.A's whole
point was that the brief's test passed whether or not the axes were appended
to `v1_basis()`, so a replacement test that merely *looked* stronger would
have been the same defect wearing better prose — and a guard authored
alongside its own fix is exactly the shape that asserts the data instead of
the effect.

Mutation: removed `HYDROGEN` from `v1_basis()` **only**, leaving the `pub
const` defined — the precise orphan #20.A describes.

```
FAIL  ecology::tests::the_metabolite_axes_are_registered_and_reachable_via_the_basis
FAIL  ecology::tests::the_basis_ids_are_append_only
        left: [0, 1, 2, 3, 4, 5, 6, 8, 9, 10]
       right: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

Both red; restored to an empty diff; both green again. So the join is pinned
by something that routes **through `v1_basis()`** and asserts the const *is*
the axis reachable at that id — not a const compared against itself.

**Ruling:** accept. Concern 2 (the `coexist.rs:298` denominator now spanning 11
members) is recorded as predicted-and-inert-today, load-bearing from Task 4 —
carried into Task 4's dispatch rather than closed.

**Ideonomy passes / overturns:** none; a task acceptance with one mutation.

---

## #23 [Ruling] — Task 4's brief names one edit site of four, and the term it moves was justified by a prediction that was then falsified

Verification before dispatch, against `windows/worldgen/src/lib.rs` and
`energy.rs`. Five findings. The first is the same shape as #20.A one task
later, which is itself worth noting: **a brief that names "the" site is
asserting a census.**

### A. There are FOUR `let per_axis = [` sites. The plan names one.

```
1975   capacity loop 1 — suitability form   (saturated = supply / (1 + supply))
2391   capacity loop 2 — dimensional form   (headcount = V_MAX * supply / (K_M + supply))
11256  the hoist-agreement test's fixture
11287  the ORDER PIN's own source-scan
```

The plan's Task 4 says "`score_at`'s `per_axis`, ~line 2390" — loop 2 only.
Adding the metabolites to loop 2 alone would leave loop 1 with a 7-entry
supply array against an 11-entry weight array.

**That particular miss fails loudly** — `axis_supply_with` asserts
`weights.len() == per_axis.len()` — so it is a panic, not a silent wrong
answer. Recording it anyway, because the *reason* it is loud is an assertion
someone wrote, not anything structural, and the brief should not rely on luck
it did not know it was relying on.

**The two loops are not copy-paste.** Loop 1 reads bare locals
(`*mineral.get(vertex)`); loop 2 reads a hoisted struct
(`*hoisted.mineral.get(vertex)`). Same order, different plumbing, different
output units. Thread the metabolites into each in its own idiom.

### B. `SUPPLY_AXIS_ORDER` is `[ResourceAxis; 7]` — a fixed-size type

`lib.rs:1537`. Adding four makes it `[_; 11]`, which changes the type and
breaks every use site until updated. Loud, and unmentioned by the plan.

### C. The order pin SOURCE-SCANS, and carries a hardcoded label table

`the_supply_axis_order_matches_both_capacity_loops` (`lib.rs:11283`) does
`include_str!("lib.rs")`, splits on `"let per_axis = ["`, and maps identifiers
to labels through a literal match:

```rust
"CHEMOSYNTHATE" => "chemosynthate",
other => other,
```

A new axis with no arm falls to `other => other`, yielding `"HYDROGEN"` where
`"hydrogen"` is expected — so it fails, loudly. **Four arms must be added.**

**And the scan's filter is the part to be careful with:** it skips any block
not containing `"(vertex)"`. `assert_eq!(found, 2)` catches both loops
vanishing, but **nothing catches a metabolite entry that does not read at
`vertex`** — that entry is simply invisible to the order check while the loop
still matches. Supply the metabolites as per-vertex reads, in the idiom of
their neighbours.

### D. The U-SHAPE IS ALREADY DEAD, AND THIS DE-RISKS THE `DetritalImport` MOVE

This is the finding worth the verification budget on its own.

`energy.rs`'s module doc (the "seventh term" section) records that
`DetritalImport` is **not** one of `BIO-subterranean-energy-sources`'s six —
it was added because, without an import term, *"the sum built in Task 5 would
have no shallow arm and could not produce the U that task measures."*

Task 5 then measured, and the test is named for the answer:
`derived_energy_is_monotone_not_a_trough`. Per-rung `ENERGY` medians,
`Undercroft` → `Nadir`:

```
0.168609  0.200822  0.265342  0.281421  0.281449   — strictly non-decreasing
```

**There is no U.** The term added to produce a shallow arm did not produce
one. Its own justifying prediction was falsified by the very measurement it
was added to enable, and nothing since has re-argued it.

**Consequence for Task 4:** routing `DetritalImport` to `DETRITUS` is not
removing a load-bearing shallow arm from the chemical supply — it is removing
a term whose stated purpose was never realized, from a vocabulary it never
belonged to (the module doc says so in its own words). The move gets *easier*,
not harder, and the implementer should know that rather than treading
carefully around a U that is not there.

**It also will not red the gate:** that probe is `#[ignore]`d (demoted by The
Governor, 2026-08-28), so it runs only by hand.

**What this does NOT license.** Expect the chemical sum to become *more*
steeply depth-weighted once the shallow detrital term leaves it. That is the
correct direction — chemical food should be deep — but it is a real change to
a measured shape, and Task 5's T1 reads a ceiling off this. Report the new
per-rung medians beside the five numbers above rather than only the ceiling.

### E. Ruling

Task 4 edits all four `per_axis` sites, `SUPPLY_AXIS_ORDER`, and the pin's
match arms, in one commit. Folded into the dispatch. The plan's file list is
amended in the same commit as this entry.

**Cost if wrong:** low. Every miss in A-C is a compile error or a failing
assert. D is the one that could have cost real time — a careful implementer
would have tried to preserve a U that does not exist.

**Ideonomy passes / overturns:** none; a verification step with five findings.

---

## #24 [Q] — The Staple's calibration ruling, adopted, with one refinement the code currently forbids

Nathan relayed The Staple D5B's take on Task 4's overshoot. **Adopted, and my
own lean was wrong.** I had leaned toward rescaling the corpus bands. That is
refuted by a structural fact I checked rather than argued:

```rust
// kernel/src/ecology.rs, EnvironmentVector::new
if !value.is_finite() || *value < 0.0 || *value > 1.0 {
    return Err(UnitError { reason: "must be finite and within [0, 1]", .. });
}
```

The `ENERGY` ruler is a **[0,1] contract enforced by the kernel type**, and
the authored underworld corpus goes through it. Moving the bands does not
merely cost cross-domain comparability, as The Staple said — it is not
expressible without breaking a kernel invariant. My option 3 was never on the
table and I did not check before proposing it.

### Why it shipped silently, which is its own finding

Nothing constructs an `EnvironmentVector` from the *derived* field, so the
overshoot panics nothing. The derived quantity escapes the type that contracts
its own ruler. That is the gap; the corpus is protected and the thing measured
against the corpus is not.

### THE HARM IS CONCRETE, NOT MERELY UNITS

`windows/vessel/src/underground.rs:949`:

```rust
condition_fit * (1.0 - chemo_weight) + energy.clamp(0.0, 1.0) * chemo_weight
```

With every rung's median ≥ 1.0, `energy.clamp(0.0, 1.0)` returns **exactly
1.0 almost everywhere**, so the chemotrophic term is a constant and
`inhabitant_fit` loses all energy-based discrimination. **The campaign's own
mechanism is destroyed by the campaign's own fix** — a stronger argument than
comparability, and the implementer found and documented it rather than hiding
it.

### The refinement: the ruler and the resource feed must stop being one value

The Staple's framing is *"separate how much chemistry exists from how much
normalized energy the legacy ruler reports."* The code cannot honour that
today, because they are literally the same function:

```rust
pub fn subterranean_energy(..) -> f64 {
    chemical_supply(..).chemosynthate     // the ruler IS the aggregate
}
```

So the fix is to **un-collapse them**, not to bound one shared value:

| | value | why |
|---|---|---|
| `ChemicalSupply::chemosynthate` | **stays the RAW sum** | a resource *magnitude*. Every other entry in `per_axis` is an unbounded magnitude that `score_at` saturates itself (`supply / (1.0 + supply)`). Bounding it here would saturate a generalist **twice** while a metabolite specialist saturates once — a modelling artifact dressed as a result. |
| `subterranean_energy` | **`raw / (1 + raw)`** | the `ENERGY` **ruler** readout: compared against authored corpus bands that live in a `[0,1]`-contracted `EnvironmentVector`, and consumed by `inhabitant_fit`'s clamp. |

Same Type-II transfer function the model already uses for
resource-to-suitability, so it is not an arbitrary rescale and not a
"divide by four."

### Projected ruler medians, and why this is a genuine T1 pass

`raw / (1 + raw)` on the measured medians:

```
1.004527 -> 0.501    1.316387 -> 0.568    1.993109 -> 0.666
2.094758 -> 0.677    2.094758 -> 0.677
```

All five land **between `E_FED` (0.5) and `E_RICH` (0.75)**, preserving
ordering and contrast. T1 predicted *"at least one rung realizes `fed` at
>= 25%, AND the realized maximum exceeds 0.5"* against a baseline of max
`0.424277` with `fed` never realized at any rung on any of twelve seeds. That
now passes **honestly**, rather than by an overshoot that would have to be
reported as a pass while meaning saturation.

### Also adopted

- **Metabolites stay raw, additive channels.** That is the new information.
- **`DETRITUS` stays additive**, not replacing. `DETRITUS_AMBIENT = 0.2`
  (`lib.rs:1201`) is a background floor; imported detritus is a separate
  depth/drainage-dependent source, and replacing would erase one of two
  sources rather than model their sum. It stays the largest single mover, so
  it gets its own T1 arm rather than a defence.
- **T1 becomes a four-arm calibration comparison** (Task 5): raw sum as
  control expected to fail the ruler; sum-then-saturate as primary;
  mean-of-four as diagnostic only; detritus add-vs-replace as a semantic
  sensitivity arm. Report band occupancy, maxima, depth shape, and
  xorn/specialist capacity for each.
- **The twelve reds wait behind this decision** — re-pinning them now would
  pin numbers this fix is about to move. **`survivorship_probe`'s is not a
  re-pin at any point**: another campaign's §5.2 claim has stopped holding
  (stratified z 1.371 vs pooled 6.464) and that is a finding to carry, not a
  number to update.

### One doc defect found on the way

`energy.rs:659` cites `crate::inhabitant_fit`. It is
`windows/vessel/src/underground.rs:937` — a different crate. Fix with the
projection.

**Ideonomy passes / overturns:** one overturn — my own band-rescaling lean,
overturned by a peer's recommendation plus the kernel invariant that settles
it.

---

## #25 [G5] — Task 4b accepted: the split works, and "0 of 12 closed" is the evidence it worked

Commit `1a73702ca`. `make gate-commit` green.

**Measured live, not projected.** Ruler medians `Undercroft` → `Nadir`:

```
0.501129  0.568293  0.665899  0.676873  0.676873
```

Matching #24's hand projection to six digits, all between `E_FED` (0.5) and
`E_RICH` (0.75). T1's prediction — one rung realizing `fed` at >= 25% and a
realized max above 0.5, against a baseline of max `0.424277` with `fed` never
realized on any of twelve seeds — now passes on a ruler that still means what
the corpus means.

**`inhabitant_fit` discriminates again, measured rather than asserted.** A
full-field scan over all three seeds and every rung found **zero vertices at
or above 1.0**, spread min ~0.25-0.47 / max ~0.61-0.79. Before: the *median*
was >= 1.0 everywhere, so `energy.clamp(0.0, 1.0)` returned a constant and the
chemotrophic term carried no information. The mechanism this campaign exists
to serve is live again.

### "0 of 12 closed" is the load-bearing result, and it is the GOOD outcome

I asked for that number expecting it to be informative either way, and it is
the strongest single piece of evidence the split landed correctly.

`per_axis` feeds `chem.chemosynthate` **directly** — not `subterranean_energy()`.
So changing the ruler cannot move placement, capacity, occupancy, names, or
fact counts, and none of the twelve content-driven reds could have closed.
Had any closed, the two values would still have been entangled and the split
would have been incomplete.

The same fact from the other side: `make rebaseline` moved **only
`docs/timings.md`**, and the generated-artifact drift check over
`docs/generated-paths.txt` is clean. A ruler change that touches no committed
artifact is exactly what "the ruler is not the magnitude" means in practice.

### The red list is FOURTEEN, not twelve

The implementer found two the Task 4 inventory missed —
`delve_seating::…seated_rung_and_nowhere_else` and
`plat_readout::…frozen_words` — and **verified by temporarily reverting
`energy.rs` to `HEAD~1`** that both fail identically without 4b. Pre-existing
Task 4 world-content drift, not caused by the calibration fix. Correctly left
untouched.

Worth recording as a method note rather than a scolding: an inventory
assembled from one full run is a **sample of the reds that run took**, not a
census of the reds that exist. The count moved 23 → 12 → 14 across three
passes today.

### Ruling: T1 RUNS BEFORE THE REDS ARE REPAIRED

The reds were parked behind the calibration decision (#24). That decision is
made, so the obvious next move is to repair them — and it is wrong, for the
reason the census ordering rule already teaches this project: **do the thing
that moves the world before pinning numbers to it, or pay twice.**

Task 5's T1 is now a four-arm comparison, and **two of its arms can move the
world**: `mean-of-four` and `detritus replace`. If either wins, placement moves
again and every literal repaired now is repaired to a dead value. The Murrain
paid that tax this morning with a census; there is no reason to re-pay it with
fourteen test literals.

So: **Task 5 first, reds after, repaired once against the arm that wins.**

`survivorship_probe` is exempt from that sequencing and from repair entirely —
it is not a number to update in any arm. Another campaign's §5.2 claim has
stopped holding (stratified z 1.371 against pooled 6.464), and a finding is
carried, not re-pinned.

**Ideonomy passes / overturns:** none; a task acceptance plus a sequencing
ruling taken from an existing precedent.

---

## #26 [G5] — T1 held; arm B ships unchanged; and this campaign has built the exact defect it spent the day naming

Commit `1964ab996`. `make gate-commit` green. 12 seeds, `BuildDepth::Terrain`,
**82,135** cave-bearing vertex-rung readings.

**Result: no shipped change.** Arm B (`raw/(1+raw)`, already shipped) wins;
arm C rejected; arm D confirms #24's additive ruling with a number.

| arm | best `fed` occupancy | realized max | ≥ `E_TEEMING` |
|---|---|---|---|
| A raw sum (control) | 100% | 3.774602 | **44-92% at every rung** |
| **B saturate (shipped)** | **92.0%** | **0.790558** | **0** |
| C mean-of-four | 51.4% | 0.943650 | 0 |
| D detritus replace | ruler identical to B by construction | | |

**Arm C rejected on boundedness, which is the argument that matters.** `raw/4`
breaches 1.0 at `raw > 4`, and the measured max raw is **3.774602** — so C's
realized max sits **within 5.7% of breaching the kernel's `[0,1]` ceiling**,
the invariant #24 established cannot be moved. It also destroys the shallow
end (Undercroft median 0.236692, *below* `E_LEAN`; `fed` at 0.0% on the two
shallowest rungs). B cannot breach at any input. C earned its keep as a
diagnostic by showing a linear rescale buys the same `rich` column while
giving up the one property that is non-negotiable.

**Arm D: keep additive, now with a cost.** Replacing costs the drow **39.01%**
of mean cave-vertex suitability — the one *peopled* subterranean kind — and
buys nothing on the ruler. The double-counting worry is answered by shape:
`detritus_supply_field` is a flat land mask with no depth term;
`DetritalImport` is depth- and drainage-gated. Different shapes sum correctly.

**The table cross-checks itself**, which is better than my asking for it would
have been: `raw/(1+raw) >= 0.5`, `raw/4 >= 0.25` and `raw >= 1` are the same
condition, so A's teeming column must equal B's fed column must equal C's lean
column — `44.0 / 87.1 / 92.0 / 92.0 / 92.0` — and they do, exactly. And the
capacity half carries a **zero-delta positive control**: 49,281 (kind, cave
vertex) pairs against `per_species_suitability`, worst |delta| `0.000e0`.

### MY FROZEN PREDICTION CARRIED A CLAUSE THAT COULD NOT FAIL

I added a third clause — *no arm-B vertex reaches `E_TEEMING`* — deliberately,
writing that it "is what distinguishes a calibrated ruler from the overshoot
arm A exhibits." **It distinguishes nothing.** `raw/(1+raw) < 1.0` for every
finite non-negative input, so the clause is guaranteed by algebra and holds in
any world, including one where the calibration is wrong.

This is a falsifier the mechanism cannot produce, and I have a written note on
exactly that shape. Writing the note did not protect the next thing I wrote —
which is itself the recorded lesson that a diagnosis is not a control.

The implementer caught it, stated it as a qualification rather than burying it
in a green, and **repaired it in the right direction**: the discriminating
ratchet now sits on **arm A**, where teeming is reachable and does fire, so a
red means the comparison lost its control rather than that something
regressed. The two clauses that *could* have failed both held with margin.

### THE FINDING NOBODY ASKED FOR, AND IT IS THE DAY'S SHARPEST

**No shipped kind weights any metabolite axis.** Verified independently by me
over `domains/species/src/lib.rs`:

```
HYDROGEN 0    REDUCED_IRON 0    REDUCED_SULPHUR 0    METHANE 0
CHEMOSYNTHATE 9   (xorn's 0.35, plus docs and tests)
```

So Stage 2 has built a **producer with no consumer** — a high-resolution
supply vocabulary that nothing in the world eats. That is precisely the
**missing HALF** this campaign spent Stage 1 characterizing (#18), reproduced
by the campaign itself, one stage later, in its own headline deliverable.

It is not a defect to fix now: **Stage 4 is the consumer**, and the plan has
always said so. But it changes what Stages 3-4 are for. Until an organism
names a metabolite, the disaggregation is a capability, not a behaviour — and
this campaign of all campaigns does not get to call an unfed producer a
result.

**It also qualifies #24's own justification.** I argued that leaving
`chemosynthate` raw avoids saturating a generalist twice while a specialist
saturates once. True prospectively, and today it **guards an empty set** —
there are no specialists. The design decision stands (it must be right before
Stage 4 creates them), but its stated reason is a forecast, not an observation,
and the ledger should not read as though it were measured.

### Ruling on sequencing, extended

#25 ruled T1 before repair, because two arms could move the world. Neither
did. But **main has moved 169 commits**, including four new underworld
peoples, and absorbing that will move the world again. So the same rule
applies once more, and the repair waits one more step:

**absorb main -> repair the fourteen reds ONCE against the absorbed world ->
stage gate.**

Repairing before absorbing would pin fourteen literals to a world that is
about to change. The measurement is committed, so absorbing is now safe —
"never absorb mid-measurement" no longer binds.

**Ideonomy passes / overturns:** none; a measurement acceptance, a self-caught
prediction defect, and a finding that relocates Stage 4.

---

## #27 [Q] — CORRECTION: the §5.2 claim holds on THIS tree. I carried "it has stopped holding" through three entries without re-checking it.

The Tidemark relayed decision 0959 and their power analysis. Checking it
against my own tree produced a correction to **my** record, not theirs.

**Entries #24, #25 and #26 each state that `survivorship_probe`'s §5.2 claim
"has stopped holding (stratified z 1.371 vs pooled 6.464)."** On this tree,
post-absorb, it **passes**:

```
PASS [ 136.560s] (6159/6161)
  hornvale-worldgen::suite survivorship_probe::the_separation_survives_conditioning_on_tenure
```

The z figures came from Task 4's report, measured on Task 4's *pre-absorb*
world. I repeated them three times, in three entries, each time as a live
fact, and never re-ran the test after the world moved twice underneath it.
**A measurement is a claim with a date**; I had the date and used the number
anyway. The instruction "never re-pin this" was the right call and stays —
but its stated reason was stale from the second repetition onward.

### THE INTERESTING PART: THE VERDICT IS COMPOSITION-DEPENDENT

Their tree reds; mine passes. The difference is roster composition:

| tree | added kinds | conditioned verdict |
|---|---|---|
| The Tidemark's | 6 marine + 4 subterranean (49 kinds) | RED |
| The Trencher's (here) | 4 subterranean | PASS |

Neither campaign alone moved it — they measured both tips green separately —
and the merged product reds. **That is the two-campaign interaction class
CLAUDE.md names as the half nothing mechanizes**, and it is the first instance
I have seen measured from both sides.

Their diagnosis is that the conditioned statistic is decided by its thinnest
cells: per stratum, breached n = 1, 3, 5, 4, 19; the two strata resting on 1
and 3 carry 20% of the pair mass and hold the two most extreme AUCs (0.144,
0.282). Pair-weighted AUC over strata with n >= 4 is 0.5635; over all five,
0.4976. So "holds" and "does not hold" are both within the noise this panel
can produce, and which one you see depends on which kinds happen to be in the
roster. **My green is not evidence the claim is true.**

### Their ruling, adopted as context for my close

Decision 0959 rules it **unevaluated, not refuted** — `refuted` (0131) would
assert a negative the data cannot carry. Their probe asserts the pooled
separation (unambiguous at n=32) and *reports* the conditioned readout behind
a **power gate that re-arms when every stratum reaches breached n >= 10**.
Nobody has to remember to switch it back on.

They also self-reported a flaw their own measurement exposed: pooled quintiles
are dominated by the ordinary group (148 of 180 ended), so they are
effectively ordinary-tenure quintiles and the breached pile into Q5. They
**declined to re-cut**, on the grounds that re-cutting after a null to obtain
a different null is fitting under another name. That restraint is the right
call and worth recording as precedent.

### WHAT THIS BINDS IN STAGE 4

Stage 4 authors 8-20+ organisms. If any settle or dig, they widen the roster
the same way the two peoples campaigns did, and this readout moves again —
from a base that is already unevaluable.

1. **Do not read a green here as support for the conditioned claim.** On their
   tree it is green only when declining to assert; on mine it is green on a
   panel too thin to decide. Neither is support.
2. **Report the breach yield per seed in the close.** Measured 2.67
   breaches/seed (32 across twelve). If my organisms push it UP, the power
   gate closes sooner; DOWN, it stays shut longer. Either is a line in the
   retrospective and the figure is cheap to read off the probe's output.
3. **If my Stage 4 flips this test red, that is not a regression to repair.**
   It is this campaign reproducing the interaction from the third side, and it
   gets recorded, not fixed.

**Extending the panel is The Winze's**, not theirs and not mine. The
arithmetic: measured yield 2.67 breaches/seed; ~10 per stratum x 5 needs ~19
seeds, ~20 needs ~38, ~30 needs ~56. The twelve-seed cap was right for the
pooled claim and was never sized for the conditioned one. Posted to the board
for them — no Winze session is on the wire.

**Ideonomy passes / overturns:** none; a correction to my own record plus an
adopted peer ruling.

---

## #28 [G5] — The repair pass: 23 of 25 repaired, no guard weakened, and a preregistration on `main` found re-authored to equal its own readout

Commits `ef1738867` + `8c1e56f99`. Full sweep: **6,160 of 6,162 pass**; the two
reds are both class 3 (census-backed), left untouched for the pre-merge
refresh on lefford. `make gate-commit` rc=0.

**No anti-vacuity guard was relaxed, weakened or deleted.** Every class-2
repair moved the *subject* — a new seed or a widened panel — which was the one
outcome I said would be worse than leaving a test red.

### The classification held, and class 4 was checked rather than assumed

`the_roll` was the strongest class-4 candidate: a *property* assertion failing
133 against 119. Rather than reason about it, the implementer instrumented it.
The 65 arrivals are **51 desert-dwarf (exactly the population, as the test
claims) plus one body each for 14 wild herds**. `refresh_roll_at` derives herds
*and* settlements by design; the test's filter only ever excluded settlements,
**so it had been holding by luck**. Shipped behaviour correct → class 2, and
the precondition was strengthened rather than the assertion loosened.

That is the right instinct: the difference between "the code is wrong" and
"the test was lucky" is not visible from the failure message.

### A masked assertion, which is why the count was 25 and not 26

`seed_42_name_syllables_are_pinned` carried a **second** assert on `kobold`
behind the failing `goblin` one. Short-circuiting meant the sweep only ever
reported the first. The repair moved both. **A failure list is a list of
tests, not of assertions** — a test with N asserts reports at most one per run,
so a red list systematically under-counts the work.

### THE FINDING: a preregistration re-authored to equal its readout

`windows/lab/tests/suite/reticence_calibration.rs`. **Verified by me against
both commits rather than relayed.**

At `a720d5cf2` (The Reticence, Task 7):

```rust
/// section 5). Frozen before the code: 15 peoples, organized 9, folk 6,
assert_eq!((god, spirit), (9, 6), "frozen arm counts over 15 peoples");
```

At `43208f575` (The Underworld Peoples) the doc line became *"Frozen before
the code: **19 peoples, organized 13, folk 6**"*.

**The precise defect, which is subtler than it first reads and is NOT bad
faith.** That campaign was *honest about the assertion*: the diff carries
`// THE UNDERWORLD re-pin: (10, 5) -> (13, 6). Adding the four peoples…`. What
it also did, in the same edit and without comment, was update the sentence
that says **"Frozen before the code"** — and that sentence is not prose about
the assert, it **is** the preregistration record.

So a prediction became equal to the readout it was supposed to be tested
against, and the claim "the arms are distributed as preregistered" became
**true by construction**. Decision 0016's whole content is that the freeze
precedes the code that moves it.

**The structural cause, which is the transferable half:** the preregistration
lived in a **doc comment adjacent to the assertion it constrains**, with no
separate or protected home. A routine, honestly-labelled re-pin swept it along
because it *looked like* prose sitting next to the number. Nothing could have
objected — there is no mechanism that distinguishes "a comment near an assert"
from "the frozen hypothesis."

This is the same family as #27: text adjacent to a fact drifts with the fact,
and the adjacency is what makes it invisible. Mine was a *reason* rotting
beside a correct instruction; this is a *prediction* rewritten beside a
re-pinned value.

**Disposition.** Restored verbatim, the rewrite named in the test's own doc,
the readout series kept separately — and the honest consequence recorded: the
restored prediction **is falsified**, because it was frozen over a roster of
15 and the roster now holds 19. Under 0016 a falsified prediction is a
finding, not a failure.

**This is a finding about `main`, not about this branch.** Both campaigns are
merged. Posted to the board so the owners see it; not minting a decision, as
it is their record to amend.

### Owed follow-up, discharged and verified

The implementer flagged that `docs/audits/the-reticence-report.md` and
possibly other `make rebaseline` artifacts had drifted across two world
movements. Ran it: **only `docs/timings.md` moved**, and the drift check over
every path `docs/generated-paths.txt` declares is **empty**. Already current.
Checked rather than carried forward.

**Ideonomy passes / overturns:** none; a task acceptance plus a verified
finding about merged work.

---

## #29 [Ruling] — Stage 3's surface is a fifth of what the plan says, its inert guard already exists, and Tasks 6 and 7 must not be split

Verification before dispatch, against `domains/terrain/`. Also: the stage gate
came back RED (`rc=11`, 1581 s) and its **two failures are exactly the two
census-backed reds I deliberately left open** — `census_sentinel` and
`tripwire`, 6,165 of 6,167 passing on lefford. Nothing new; the census refresh
is queued at `1172e1069b43`, submitted at 0 commits behind main per #21.C.

### A. NINE sites, not ten — and only TWO are production

The plan states *"Measured: 10 sites, every one hardcoded `0.0`, none via
`Default`."* The count is **9**, and the classification the plan never made is
the one that matters:

```
domains/terrain/src/lithology.rs:632    PRODUCTION
domains/terrain/src/globe.rs:532        PRODUCTION
domains/terrain/src/lithology.rs:969    test
domains/terrain/src/features.rs:718     test
domains/terrain/src/cave_depth.rs:247   test
domains/terrain/src/cave_depth.rs:263   test
windows/worldgen/src/energy.rs:985      test
windows/vessel/src/fabric.rs:292        test
windows/vessel/src/plan.rs:614          test
```

**Task 7's real surface is two sites.** Seven are fixtures that may stay
`0.0` — a fixture asserting an inert value is not a call site of the
derivation. The plan's "modify the 10 sites" would have had an implementer
threading a derivation through seven test helpers for nothing, across three
crates.

### B. Task 8's inert arm ALREADY EXISTS

`domains/terrain/src/lithology.rs:1126`,
`buffer_axes_are_bounded_and_thaumic_is_zero`:

```rust
let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
...
assert_eq!(b.thaumic, 0.0, "inert-tier thaumic must be identically zero");
```

Default pins, **every vertex**, seed 42. That is precisely Task 8 Step 1's
"artifacts under default pins must be byte-identical," in a stronger and far
cheaper form — identically zero rather than byte-identical, checked pointwise
rather than through a rendered artifact.

So **Task 8's genuinely new work is the PINNED arm**, which is the half the
plan itself calls the non-vacuous one. The inert arm is a guard to keep green,
not to build.

### C. Tasks 6 and 7 SHARE A COMMIT

The plan splits the pin (Task 6) from its derivation (Task 7). **A pin with no
derivation is a field nothing reads** — a producer with no consumer, which is
the exact defect #26 records this campaign committing one stage earlier. Task
6's own test (`pins.metaphysics.is_none()`) would pass on a field wired to
nothing, forever.

**Ruling:** one commit. The deliverable is a gate that demonstrably gates
something.

### D. What the plan got right, verified

- **No metaphysics pin exists.** Only prose mentions (`star.rs:38`,
  `features.rs:5`, `lithology.rs:104`). The gate is genuinely new.
- **`MaterialBuffer` does not derive `Default`** — `#[derive(Debug, Clone,
  Copy, PartialEq)]` only, so all nine sites are explicit and none is implied.
- **`TerrainPins` is all-`Option` with `Default`**, so `None` = inert = the
  unpinned world is byte-identical. The shape to copy is real.

### E. Noted, not in scope

`features.rs:5` already declares a sibling reservation — *"Mundane only —
magical ores are metaphysics-gated and stay reserved."* So the pin this stage
builds will eventually gate more than `thaumic`. Worth knowing when naming it:
call it for the *gate*, not for the one axis it first admits.

**Cost if wrong:** low. A and B save work rather than prevent damage; C is the
one that prevents a repeat of #26.

**Ideonomy passes / overturns:** none; a verification step with five findings.

---

## #30 [Ruling] — The census is a WORLD-MOVER, not only a fix; and two gate holes composed to hide a red branch

Tasks 6+7 came back **BLOCKED**, correctly. The implementation is complete and
verified; it cannot commit because `make gate-commit` is red at HEAD **for
reasons that predate it**. The implementer verified the same twelve failures
against a clean HEAD before concluding that, and refused `--no-verify`.

### A. THE ORDERING RULE I WROTE WAS RIGHT AND I APPLIED IT ONE MOVE SHORT

#25/#26 ruled: repair once, after the last thing that moves the world. I then
treated the **census** as the *fix* for two census-backed reds rather than as
*a mover* in its own right. It is both. The delivery moved **117 golden
files**; twelve `hornvale-lab` calibration pins read census output and were
falsified by it.

```
domesday::anomaly::evaluable_columns_… : evaluable count moved, 181 -> 179
calibration::name_collision_rate_…     : mean name-collision-rate drifted
                                         to 0.527216074640001
+ 10 more, all hornvale-lab, all sub-second reads of committed CSV
```

So the census **fixed two reds and broke twelve**. The refined rule:

> A census refresh is a world-moving event for every consumer that reads
> census output, not merely the remedy for the consumers that compare against
> it. Repair AFTER the census lands, not before — and expect the census
> itself to move pins that the pre-census repair could not have seen.

The repair pass (#28) was therefore correct in method and one step early in
sequence. Nothing it did was wasted; twelve more were simply not visible yet.

### B. TWO INDEPENDENT GATE HOLES COMPOSED, AND THE SECOND ONE IS MINE

The branch has been red since the census merge and **nothing caught it**:

1. **A `--no-ff` merge fires `pre-merge-commit` only, and `scripts/hooks/`
   holds no such hook.** CLAUDE.md documents this exactly — the *clean*
   auto-merge is the ungated shape. Both my merges (census delivery, Cadastre
   absorb) were true merge commits and both went ungated.
2. **Every commit I made after that was DOCS-ONLY**, and the pre-commit hook
   says so itself: *"no Rust-relevant paths staged — running the prose-subject
   tests instead of `make gate-commit`."* Four ledger commits, four prose-only
   runs.

Neither hole is a defect on its own — (1) is documented and (2) is a
deliberate cost saving. **Composed, they mean the subfloor tier had not run on
a commit since before the census**, and a campaign that ledgers diligently is
*more* exposed than one that does not, because every ledger commit is
docs-only.

**The generalizable form, which is worth more than this instance:** after any
merge that moves the world, the next commit that runs the code gate may be
arbitrarily far away, and a run of documentation commits guarantees it is.
**Run `make gate-commit` by hand after a world-moving merge** — the hook will
not do it for you, and the merge did not either.

### C. Ruling on the twelve

**They are class-1 literal drift** (#28's taxonomy), caused by the census
delivery `bdc59cc18` and the Cadastre absorption. Not invariant violations,
not vacated preconditions — pinned values whose world moved. I verified two
directly and the shape of the other ten from their sub-second runtimes.

**Re-pin all twelve, with the cause named in each**, plus `golden-pins.sql`.
Bare re-pins are refused, per #28: a number updated without its reason is what
makes the next person's breakage invisible.

This is the campaign's judgment call and the implementer was right to leave
it. Delegating the mechanical half with the ruling attached.

### D. Two smaller findings from the same report, both accepted

- **`windows/worldgen/tests/suite/artifacts.rs` was edited outside the
  brief's file list, and correctly.** Its `TerrainPins` literal is exhaustive
  with no `..default()`, so a new field forces it. Setting the new field to
  `Some(Metaphysics::Thaumic)` rather than `None` **extends that test's
  round-trip claim to the new pin** instead of merely satisfying the compiler.
  The forced edit was turned into coverage.
- **This session's `docs/timings.md` rows were discarded during staging.**
  Figures preserved in the report. Append-only and not regenerable, so the
  loss is real but bounded; noted rather than reconstructed.

**Ideonomy passes / overturns:** none; a blocked task correctly refused, plus
a refinement to my own sequencing rule.

## #31a [G5] — The twelve re-pinned and read rather than inferred: 26 drifted values behind 12 failures, and three corrections to #30

*(Numbering collision resolved at close: written as `#31`, which my
reaction-yields entry below also took, and which #32 and #33 cite meaning that
one. Suffixed rather than renumbered so those citations stay correct; content
unchanged.)*

#30's ruling executed. **All twelve are class-1 literal drift, and that is now
READ rather than inferred** — #30 classified two directly and ten from their
runtimes, and invited a better instrument.

### A. THE INSTRUMENT: SOFTEN THE PINS, KEEP THE INVARIANTS ARMED

Rather than re-run twelve times reading one failure per run, the **pin**
assertions alone were temporarily rewritten to non-fatal `eprintln!` (a
`soft!`/`soft_eq!` pair), leaving every invariant, precondition and structural
relation as a live `assert!`. One run then reported every drifted value at
once **and** proved the classification:

```
12 tests run: 12 passed, 0 failed        <- with ONLY the pins softened
```

Twelve green with the pins removed means every invariant in all twelve was
**reached and held**: the frozen-sky `panic!` arm, blind attribution's 0.75
floor AND its mooned-pair `assert_eq!`, the epithet detector's inner
`assert!`, the syllable/name-length per-row structural relation, homophony's
`mb > mg && mb > mh`, and the latitude baseline. None is an invariant
violation; none is a vacated precondition. The instrument was removed and the
files diffed back to their pristine hashes before the real re-pins were made.

### B. TWELVE FAILURES WERE HIDING TWENTY-SIX DRIFTED VALUES

The asserts are **sequential**, so each failure masked everything after it.
The measured surface is **26 drifted values across 24 literal sites**, plus
**20 literals in `golden-pins.sql`** — not twelve:

| masked behind | rows that never ran |
| --- | --- |
| frozen-sky split | `spinning_eternal` 14 -> 12 — **a 13th test-level pin nobody had named** |
| blind-attribution `correct` | `total` 982 -> 983 (the denominator moved) + the mooned-pair invariant |
| goblin flagship `coastal` | `inland` 817 -> 805 |
| name-length goblin arm | kobold present 982 -> 983 **and** its mean |
| name-syllables goblin arm | kobold present 982 -> 983 **and** its mean |
| homophony goblin | hobgoblin, bugbear, kobold — **all three** |
| transparency mean | the floor AND the ceiling |
| evaluable count | excluded count 51 -> 53 |

`branches_family_calibration.rs` carries a note from 2026-08-28 warning about
exactly this ("only goblin was [re-measured]... these asserts are sequential").
Reading one failure per run would have reproduced that mistake four-fold.

### C. THREE CORRECTIONS TO #30, all measured

1. **The delivery moved 116 files, not 117** (`git show --stat bdc59cc18`:
   `116 files changed, 2611 insertions(+), 2623 deletions(-)`).
2. **The Cadastre absorb is NOT a mover.** #30 attributes the drift to "the
   census delivery `bdc59cc18` and the Cadastre absorption".
   `git diff --name-only 6b7d05eab c23bae9fd -- 'book/src/laboratory/generated/**'`
   is **empty**: the absorb touched no census fixture. All four affected files
   read `book/src/laboratory/generated/the-census` (and
   `census-of-the-meeting` for the null control), both of which only
   `bdc59cc18` rewrote. **The census delivery is the sole mover**, and each
   re-pin says so.
3. **`spinning_eternal` makes thirteen test-level pins**, not twelve tests'
   worth of one each.

### D. TWO DEFECTS FOUND WHILE RE-PINNING, ONE OF THEM MINE

- **A pre-existing duplicate table row.** `name_length_distributions_…`
  iterates `[("goblin", …), ("goblin", …), ("kobold", …)]` — goblin appears
  **twice** with identical values, so its arm is asserted twice. Pre-existing,
  harmless to correctness, and **reported rather than silently deleted**; both
  arms were re-pinned together. Removing the duplicate is a separate decision.
- **I introduced, then caught, a half-updated pin.** Each integer pin in
  `golden-pins.sql` writes its value TWICE — `182.0 AS pinned` for the report
  and `= 182` for the verdict. My first pass updated only the comparison, so
  `census-check` went green while its report still PRINTED the old pinned
  value: the check passed and lied. Caught by reading the passing output
  instead of trusting the exit code — the row read
  `spinning-yet-eternal …,12.0,14.000…,true`. Eleven rows were affected and
  all eleven are fixed; computed and pinned now agree on every row.
  **The lesson is the one this file already teaches from the other side:** a
  duplicated value needs both halves moved, and a green check whose own output
  contradicts itself is worse than a red one.

### E. The latitude row's standing instruction fired, and is answered

`pop_weighted_abs_latitude_…` moved 17.6474 -> 18.1105, a **+0.4631-degree**
poleward step — the largest this row has recorded, and the **third consecutive
narrowing of the margin**, each step larger than the last (+0.2055, +0.2042,
+0.4631). That file carries a standing instruction that a further narrowing be
treated as a question about the floor rather than a quiet re-pin.

Answered in place: **it still says nothing about the floor.** The sole mover is
the census delivery, whose content is this campaign's trophic/metabolite/
subterranean-supply work plus an absorbed peoples roster — nothing with a
latitude term, and a roster's authored biome affinities are exactly the
authored-curve-read-as-finding that file has been caught on before. The
asserted claim is untouched: 18.1105 clears the 32.7 uniform-sphere baseline by
1.81x. **Raised here rather than left in a comment**, because three
accelerating narrowings is more than the single step the previous note
answered.

### F. Verification

```
subfloor tier: 4646 tests run: 4646 passed, 1767 skipped     (was 4634/12 red)
make census-check: ok                                        (20 SQL pins resynced)
```

Per #30.B this is **the first green code tier on this branch since the census
merge**. `census-check`'s recomputation is an independent second path from the
fixture, and its computed column agrees with every Rust re-pin.

**Ideonomy passes / overturns:** none; an execution entry with three
corrections and two defects.

---

## #31 [Q] — The seven reaction yields were never on a common scale. The mean hid it; the sum exposes it. And "methane is flat" was my imprecision.

Chasing my own reported finding — *methane flat at 0.025 while sulphur spans
0.053-0.556* — on the suspicion that an inert axis about to take a permanent
append-only id might be a defect rather than a fact. It is neither. It is a
pre-existing asymmetry this campaign's instrument made visible.

### What the code actually says

`EnergySource::yield_at` (`windows/worldgen/src/energy.rs`) treats its seven
sources three different ways:

| source | moisture term | shape term |
|---|---|---|
| `IronReduction` | `water_gate(m, 0.25)` — saturates | silica band |
| `Radiolysis` | `water_gate(m, 0.10)` — saturates | silica band |
| `SulphideOxidation` | `water_gate(m, 0.40)` — saturates | **normalized to peak exactly 1.0** (the `4r·ΔT/(r+ΔT)²` AM-GM construction) |
| `Geothermal` | `water_gate(m, 0.20)` — saturates | saturating ΔT |
| `Serpentinization` | **raw `* moisture`** — documented stoichiometric | silica band |
| `Methanogenesis` | **raw `* moisture`** — documented stoichiometric | **none, and unnormalized** |
| `DetritalImport` | — | depth falloff |

**`Methanogenesis` is `buffer.carbonate * buffer.porosity * moisture`: the only
source that is a bare product of THREE unnormalized `[0,1]` terms**, with no
saturating gate and no peak normalization. Its siblings either saturate their
water term to ~1.0 in ordinary conditions or normalize their shape term to
peak at 1.0. It does neither.

Also measured off the code, and it corrects a thing I assumed: **only three of
the seven read `depth_m` at all** — `SulphideOxidation`, `Geothermal`,
`DetritalImport`. `Serpentinization`, `IronReduction`, `Radiolysis` and
`Methanogenesis` have no depth term; their per-rung variation arrives entirely
through `moisture`.

### MY CLAIM WAS IMPRECISE AND I PROPAGATED IT TO TWO CAMPAIGNS

I reported *"three of the four axes discriminate spatially; one does not"* and
*"METHANE buys no discrimination."* Both overstate what I measured.

What I measured was **per-rung medians**. Methane carries the same raw-moisture
term Serpentinization does, so it plausibly varies *proportionally* as much as
`HYDROGEN` — its **absolute** range is compressed by the `carbonate × porosity`
factor, which is a product of two small numbers.

**The right word is DIM, not FLAT.** And dimness is what matters here, for a
reason that is not obvious from the word: `axis_supply_with` is a **SUM**, so a
consumer receives absolute magnitude, not proportional variation. `METHANE` at
~0.025 against `HYDROGEN` at ~0.35 is a **~14x weaker axis**. A kind weighting
`METHANE` at 1.0 receives a fourteenth of what the same weight on `HYDROGEN`
would pay.

Combined with The Tidemark's discount constraint, the consequence sharpens: a
methanotroph is not merely unviable, it is **dominated** — any kind that names
methane instead of hydrogen takes a 14x supply cut for naming a different food.

**The practical advice I gave both campaigns — do not build anything that
depends on methane varying — is unchanged and still correct. Its stated reason
was wrong.** That is precisely the shape I warned The Tidemark about and then
produced: a correct instruction resting on a justification that does not hold.
Correcting the reason on the board and to both campaigns.

### WHY THIS IS THE CAMPAIGN'S FINDING RATHER THAN ITS DEFECT

The mean-of-seven divided everything by seven and averaged the scale asymmetry
into a single scalar, where it was **invisible by construction**. No reader of
`subterranean_energy` could have seen that one of its seven inputs was on a
different scale from the others, because the output was one number.

Disaggregation makes each reaction's scale **directly observable as an axis
magnitude**. So the campaign's own instrument revealed a pre-existing
inconsistency in a reaction set that predates it by two campaigns. That is what
a better instrument is for, and it is worth saying in the chronicle: the
disaggregation's first finding is about the thing it disaggregated, not about
the world.

### Ruling: RECORD, DO NOT FIX

Normalizing `Methanogenesis` — a saturating gate, or a peak normalization
matching `SulphideOxidation`'s — would change world output, invalidate the
census delivered at `1172e1069b43`, and cost another ~2100 s run plus a repair
pass, inside a campaign Nathan has already cut to its supply half.

It is also a **modelling** question, not a mechanical one: is methane genuinely
rare because carbonate, porosity and water rarely co-occur, or is the triple
product the wrong encoding of "all three required"? The doc says water is
consumed by the reaction, which justifies the raw multiply; it does not justify
the absent normalization.

**Recorded as a finding with its cost named, for a successor.** `METHANE` keeps
id 10: an axis whose supply is dim is not an axis that is wrong, and the id is
append-only and cheap. What would be wrong is landing it while implying four
usable metabolites — so the spec says three discriminate usefully and one is
dim, with this entry as the reason.

**Ideonomy passes / overturns:** none; a suspicion chased into the code, which
corrected my own reported result.

---

## #32 [Q] — A claim repeated eleven times, asserted zero times, went false before anyone noticed

Stage 3's implementer escalated `pop_weighted_abs_latitude_…` rather than
re-pinning it quietly: `17.6474 -> 18.1105`, the largest single step that row
has recorded, and the third consecutive narrowing with each larger than the
last (`+0.2055, +0.2042, +0.4631`). That was the right call and the file's own
standing instruction demanded it.

Chasing it found something the step does not show.

**`windows/lab/tests/suite/gathering_calibration.rs` says the directional claim
"still clears the baseline by better than 2x" ELEVEN TIMES. It does not.**

```
mean 15.0340  ->  2.175x      the last reading that was true
mean 17.2377  ->  1.897x   |
mean 17.4432  ->  1.875x   |  the three narrowings the implementer tracked
mean 17.6474  ->  1.853x   |
mean 18.1105  ->  1.806x      The Trencher
```

**The crossing predates this campaign and predates all three narrowings** — it
sits between `15.0340` and `17.2377`, and belongs to neither.

**Nothing could have caught it.** The assertion pins the DIRECTION
(`mean < UNIFORM_SPHERE_BASELINE`) and nothing pins the RATIO. So a margin
claim, restated approvingly by eleven successive campaigns, lapsed in silence
while every run stayed green. This is the same shape as #27 and #31 arriving
from a third direction: **a claim carried in prose beside an assertion that
does not enforce it**. #27 was a reason rotting beside a correct instruction;
#31 was my own imprecise summary propagating; this is eleven authors
inheriting a margin figure nobody re-derived.

Worth naming the mechanism, because it is not carelessness: each of those
eleven entries was **true when written**, each author re-checked the
*directional* claim exactly as the file asks, and the ratio was a parenthetical
nobody was asked to verify. The file's discipline is real and it had a hole in
the shape of the one number it repeated most.

**What I did, and deliberately did not do.** Added a correction at the current
entry. **Did not rewrite the eleven historical lines** — they record what each
campaign correctly observed at its own time, and editing them would falsify the
record to make the file tidy. Append-only applies to prose that is a record.

**Did not add a ratio assertion**, though that is the repair that would stop
recurrence. This campaign is cut to its supply half, and adding a guard to
another campaign's calibration row is not its call. Recorded for that row's
owner: the cheap fix is to pin or print the ratio so the next lapse fires
rather than accrues.

**The implementer's own three corrections to #30, all verified by me:** the
delivery moved **116** files not 117; **the Cadastre absorb is not a mover**
(`git diff 6b7d05eab c23bae9fd -- book/src/laboratory/generated/**` is empty),
so #30 named a co-cause that does not exist; and it was **thirteen** pins, not
twelve — `spinning_eternal` (14 -> 12) was masked and nobody had named it.

**And the methodology that earned those:** rather than infer the
classification, it softened **only the pin assertions** to non-fatal prints,
left every invariant and precondition armed, and re-ran. All twelve then
**passed** — proving the invariants were reached and held, so all twelve are
class-1 literal drift. That is a positive control for a classification, which
is stronger than any amount of reading, and it is the technique to reuse.

It also found that **twelve failures were hiding twenty-six drifted values
across twenty-four sites** plus twenty in `golden-pins.sql`, because asserts
are sequential — including three of homophony's four species, the exact
mistake that file's own 2026-08-28 note records. Reading one failure per run
would have repeated a documented error fourfold.

**Its own defect, self-caught and worth more than the fix:** each integer pin
in `golden-pins.sql` writes its value twice — once for the report, once for
the verdict. Its first pass moved only the verdict, so **`census-check` went
green while its report printed the old value**. Caught by reading the passing
output rather than trusting the exit code.

**Ideonomy passes / overturns:** none; an escalation chased into a lapsed
claim.

---

## #33 [G5] — Task 8: the gate fires, the author's own claim survived an independent check, and a comment this campaign wrote claimed coverage its test cannot give

Commit `0e6052e2c`, plus `windows/worldgen/tests/suite/metaphysics_gate.rs` —
**two tests in the ORDINARY TIER, not `#[ignore]`d**, 0.481 s each. A two-way
control that only runs by hand is half-armed, and this one does not have that
problem.

### The table, measured from the composition root by a second implementer

Seed 42, level 6 (40,962 vertices), both arms through
`build_world_to_with_artifacts`:

```
thaumic            12512 moved   0.305454      (charged: 3479 on land, 24 saturated,
silica/grain/induration/carbonate/            max 1.0, mean 0.413285; inert: 0)
metamorphic_grade/porosity/margin/
soil_depth/basement/elevation/is_ocean/rock
                       0 moved   0.000000
```

**The claim I told them to distrust survived.** The gate's author measured "0
vertices moved on any other buffer axis"; the check was redone from a
different entry point (composition root rather than `assemble_material`) and
**widened past the buffer** to `elevation`, `is_ocean` and `rock`. It holds.

### The instrument was proven to go red — three mutations, each restored

| mutation | fires-and-moves-nothing-else | round trip |
|---|---|---|
| `+1e-12` leak into `silica` | **RED** (40962/40962) | green |
| charged branch of `thaumic_at` neutralised | **RED** | **RED** |
| `metaphysics` dropped from `pin_strings` | green *(correct — the build uses pins directly)* | **RED** (12,512 disagree) |

Mutation 1 earns the test its place on its own: **the pre-existing inert guard
could not have caught it**, because a `+1e-12` leak stays inside `[0,1]` and
that guard checks bounds and `thaumic == 0.0`, not sibling-axis stability.

### Two honest observations, correctly filed as NOT defects

- **72% of charged ground is seafloor** — the ley term keys off plate
  boundaries, which are mostly submarine. Nobody claimed otherwise; worth
  knowing before anyone builds on charged ground.
- **`thaumic` has zero production consumers**, so "moves nothing else" is
  **structurally guaranteed today** — there is exactly one production read of
  `metaphysics`. The implementer said so plainly rather than banking the
  result: the test is *a control against the first consumer landing*, not
  evidence about today's code.

That is the third appearance of this campaign's own signature defect —
`#26` (metabolite axes, no consumer), `#31` (methane, dim by construction),
and now the gate. **The Trencher's deliverable is a set of capabilities, and
every measurement that looks like a result is really a baseline for whoever
adds the first consumer.** The chronicle says that once, plainly.

### THE FINDING, AND IT IS AGAINST THIS CAMPAIGN'S OWN COMMIT

The gate commit (`1488148b0`) added to `artifacts.rs`:

> a charged world that re-derived as an inert one would be a lossy round trip
> of exactly the kind this test exists to catch.

**It cannot.** `projection` (`artifacts.rs:58`) returns
`(elevation_at, is_ocean)` — and the table above shows the gate moves **zero**
of both. A charged world re-deriving as inert is invisible to that test by
construction.

**Demonstrated, not argued:** with `metaphysics` dropped from `pin_strings`,
**all six `artifacts::` tests PASS** while the new
`a_charged_world_re_derives_from_its_ledger_as_charged` fails at 12,512
vertices.

This is `prose and its own exemplar disagree`, authored by this campaign three
commits ago, and it is the *dangerous* direction: a comment that tells the next
reader a guarantee exists where none does. **Corrected in place** — the comment
now states what the test cannot do, names the measurement, names the mutation
that proves it, and points at the test where the round-trip claim actually
lives. The pin stays, because keeping `TerrainPins` exhaustive is a real job.

**Ruling: accept.** The implementer did not touch the other campaign's test
code, closed the gap beside it, and reported the comment for correction rather
than editing it themselves — which is the right boundary.

**Ideonomy passes / overturns:** none; a task acceptance and a self-inflicted
prose defect caught by an independent reader.

---

## #34 [Ruling] — The close walk, and three things it found in this campaign's own record

### A. The ledger had DUPLICATE ENTRY NUMBERS — two `#22`, two `#31`

Found by reading the ledger end to end, which is step 2B's whole point: a
ledger is committed at write time but **nobody has read it just because git
has a copy**. Two entries were written `#22` and two `#31`, by different
authors appending at different moments.

This is the duplicate-`TOOL-24` shape that CLAUDE.md records travelling
through a spec, a plan, a study JSON and a decision before anyone noticed —
arriving here in the campaign's own ledger.

**Resolved by SUFFIX, not renumber**: `#22a` and `#31a`. `#32` and `#33` both
cite `#31` meaning the reaction-yields entry, so renumbering would have
silently repointed two live citations. Suffixing fixes uniqueness and moves
nothing. Each carries a one-line note saying what happened; content unchanged.

### B. #32's "three accelerating narrowings" is PARTLY UNWOUND by the absorb

`#32` recorded the latitude row's three consecutive narrowings — `+0.2055,
+0.2042, +0.4631` — as accelerating. **The largest step belonged to this
branch's own census delivery, which the absorb of `origin/main` did not
keep.** Post-merge the value is `17.6474`, not `18.1105`.

**What survives, and it is the part that mattered:** the ratio is `1.853x`,
still short of the "better than 2x" this file claims **eleven times**, and the
crossing still predates this campaign — `15.0340` read `2.175x`. The lapsed
claim is real; the acceleration was partly an artifact of a delivery that has
since been superseded.

Recorded as a correction rather than edited into `#32`, because `#32` was true
when written and the interesting fact is that a finding can be unwound by a
later merge.

### C. The Ceiling's deferred minors, routed before the worktree is recycled

`.superpowers/sdd/2026-09-11-the-ceiling/progress.md` is **git-ignored scratch
from the folded-in campaign** and dies when this worktree is next taken. Two
deferred minors sat in it, ledgered nowhere:

1. *"exact-value pin should be direction-only, matching
   `subterranean_energy_probe.rs`'s falsified-prediction style"* — both marked
   `deferred -> folded into Task 3`.
2. *"`MaxOfSeven`'s doc comment says 'composition-preserving extreme',
   inheriting the spec's inverted rationale."*

**Both are moot and that is their disposition, not an evasion.** The Ceiling's
`MaxOfSeven` arm does not exist in this tree — the campaign closed unmerged
and only its *measurements* were folded into The Trencher, not its code. There
is no doc comment to correct and no pin to re-style.

**The process lesson is the part worth keeping**, and it goes in the
retrospective: those two minors lived only in git-ignored scratch belonging to
a campaign that never merged, and would have evaporated silently. The
Cartulary's committed ledger fixed this for the campaign that *writes* it; a
folded-in campaign's scratch is still outside that guarantee.

**Ideonomy passes / overturns:** none; a close-walk sweep.

---

## #35 [Ruling] — The ratio finding bumped up: pinned in the row, and filed as a class

Nathan asked whether the ratio finding could be raised. It could, in two ways,
and the first is smaller than the guard I declined to build in `#32`.

### A. PINNING THE RATIO IS NOT A NEW GUARD — it is the file's own discipline

`#32` said the repair was "not this campaign's call" because adding a guard to
another campaign's calibration row changes what it checks. **That framing was
wrong.** `gathering_calibration.rs`'s entire culture is *pin the value, re-pin
it with a stated cause* — eleven entries do exactly that for the mean. The
ratio was **the one quantity in the file that was stated and never pinned**,
which is precisely why it drifted: it was outside the discipline every other
number obeys.

So pinning it adds no mechanism. It puts the last unpinned number under the
rule the file already runs on. The assertion says so in those words, and says
plainly that a new reading is a re-pin with a cause, **not** a failure — what
it refuses is the silent case.

**Proven to fire, not assumed.** Mutated the pin `1.8530 -> 1.9000`: **RED**.
Restored by python rewrite (which stamps mtime — the safe row of the
restore-verb table in `#33`'s sibling finding), re-ran: **PASS**, and the diff
shows only the 25 added lines. Verified by the named PASS line, not an exit
code.

### B. The class is filed, with its own remedy's limits attached

`PROC-prose-claims-no-assertion-checks`. Four instances measured in this one
campaign, which is what makes it a class rather than an anecdote:

1. `gathering_calibration.rs` — "better than 2x" stated **eleven** times,
   asserted **zero**, false since before this campaign (`#32`).
2. `reticence_calibration.rs` — a "Frozen before the code" line rewritten to
   equal its own readout, during an honest and labelled re-pin of the
   assertion beside it (`#28`).
3. `artifacts.rs` — a comment claiming coverage its test provably lacks,
   demonstrated by mutation (`#33`).
4. My own instruction, restated four times on a measurement that had died two
   world-moves earlier (`#27`).

**The mechanism is not carelessness and the row says so.** In every case the
author re-checked *what the assertion checks*, exactly as asked. The prose
claim was a parenthetical nobody was assigned.

**The candidate instrument is named WITH its limits rather than proposed.** A
narrow grep — calibration sources carrying a prose `better than Nx` with no
assertion naming N — is plausible and catches this family where margins are
claimed. Its false-positive rate is **unmeasured**, and the general form
("does this comment's claim follow from the assertions below it") is
intractable. Worth building only after someone counts how often the narrow
pattern occurs. Recorded that way so the next reader does not build first and
measure after — the failure `#18`'s candidate instrument was filed to avoid.

**The Idea cell overran the 600-character budget at 2,068 and again at 694
before it fit at 541.** Worth noting because the waiver list only shrinks, so
"grandfather it" was never available: the budget is a forcing function on
whether a row states a shape or retells a story. Three drafts to say it in one
sentence.

**Ideonomy passes / overturns:** one overturn — `#32`'s "not this campaign's
call," overturned by seeing that the repair was the file's existing rule rather
than a new one.

---

## #36 [G5] — Task 10: my candidate was wrong, the profile named a different one, and the fix is worth 65.9 s

Commit `f15744189`. Nathan ruled *profile and optimise first* rather than raise
the ceiling; this is what the profile found.

### MY HYPOTHESIS IS FALSIFIED, and I am glad I told it not to trust me

I said the likely cost was `SUPPLY_AXIS_ORDER` widening 7 -> 11, since no kind
weights a metabolite axis and those four terms are `0.0 * supply` for every
species in both capacity loops. **It costs essentially nothing.**

- The new per-rung derivation is **cheaper than what it replaced** —
  `energy::chemical_supply` 0.078% of study cycles against
  `energy::subterranean_energy` 0.096% at baseline. Net **−0.3%** of the delta.
  `subterranean_energy` is now a *projection* of `chemical_supply` over the
  same `EnergySource::ALL` loop, so the work did not double; it moved.
- The widened dot product, bounded above by the inner closure's absolute
  cycles, is **≤0.4% of the +341.192 s census delta** — and that bound also
  contains astronomy and every other main commit in the interval.

The `0.0 * inf` trap I warned about never applied, because `axis_supply_with`
was never touched. **A brief that names the author's own hypothesis AND tells
the implementer to distrust it is worth writing**; this is the second task this
campaign where doing so changed the answer.

### The three-point differential, which is why the attribution is credible

Instrument matched to the 2026-09-11 baseline exactly (perf 99 Hz,
frame-pointer, 1000 rows, 3,708,467 samples, zero lost) and **validated against
the recorded figures before use** — `build_row` 82.83%, extraction 49.61%
reproduced. Folded stacks were re-derived from the preserved `fa1223fd7` profile
and a 2026-09-12 main profile already on the box, making three points rather
than two.

Study cycles **+31.84%** A→C:

| | share of delta |
|---|---|
| ordinary main drift (296 commits, neither campaign) | **41.0%** |
| `astronomy::ephemeris::orbital_state_at` — 0% at baseline, 10.707% now | **44.3%** |
| everything else, including all of The Trencher | 14.7% |

### The optimisable share, and it is a FIFTH instance of `PROC-prose-claims-no-assertion-checks`

Swept every frame ≥0.1% for a duplicate-derivation signature. **One real hit,
nothing else above 0.25%:** `chorus::accounts_from`, 5.866% of study cycles,
split into **six shares inside a 0.2 pp band** — 16.76 / 16.69 / 16.68 / 16.67
/ 16.61 / 16.59.

`chorus_voices`'s own doc claimed the six metrics *"share one call site rather
than each re-deriving voices."* **True of the call site. False of the work.**

That is exactly the class I filed at `#35` — a prose claim no assertion
enforced — and it is the **first instance with a price on it**: 4.889% of study
cycles by profile, 6.53% of study wall realised, **3.86% of the whole census,
65.9 s of 1707.679 s**. The registry row now leads with it, because a row with
a number is worth several without.

### Bit-identity, proven four ways rather than asserted

A private `OnceCell` on `FullView`, the pattern already used for `lot`,
`band_transects` and the weft pools. Matched pair on lefford, full 1000-row
study, back to back under one claim:

```
957.58 s -> 895.02 s   (−62.56 s), user CPU −2,479 s, RSS flat
rows.csv SHA-256 IDENTICAL: 0fb652ea…
```

And that hash **is also the hash of the census goldens already staged** in
`hornvale-census-wt` — so the instrument reproduces the real census exactly,
and a re-run at the fixed ref delivers the goldens we already have. Plus:
`census_sentinel` emits an **identical 138-line** disagreement set before and
after, run as an explicit control; `make rebaseline` moves nothing; a Mac
hardware-counter cross-check agrees at −7.27% cycles.

### Ruling on the ceiling: DO NOT RAISE YET

Projected census **~1641.8 s** — under the 1650 ceiling by **8.2 s (0.50%)**.
The implementer said plainly that this is not comfortable, and cited the
ledger's own **2.5% wall spread at +0.03% of work**. That is the right kind of
honesty about a margin one coin-flip wide.

**Re-run first** — the recovery (65.9 s) exceeds the overage (57.7 s), the
goldens are provably the staged ones, and a re-run at the final ref is owed
anyway. Census queued at `06055072d639`.

**If it still refuses**, raise it as a deliberate recalibration with this
condition attached: **ratchet back to 1650 once `orbital_state_at` is addressed
and two consecutive censuses measure under 1600.** That names a measured target
— ~96 s of the new 895 s study, roughly 1.5x the lever just taken.

`orbital_state_at` was left alone **deliberately and correctly**: its shape is
per-item (200 lots, ~270 settlements each observing its own sky), not the
six-equal-shares case, so it needs its own task rather than a speculative edit
inside a closing campaign. It is also **astronomy's code, not ours**.

**Ideonomy passes / overturns:** one overturn — my own candidate, falsified by
the instrument I asked for.

---

## #37 [G5] — Round 2, and an implementer correctly refused to author my finding for me

Twelve reds fixed; my thirteenth blocked the commit. **The implementer
declined to write my yellow-log row**, and its reason is the best thing in the
report:

> writing the row myself would mean authoring a profiling finding I never
> measured into the one log whose stated purpose is that
> acknowledging-without-profiling is the failure it replaced.

It also enumerated and rejected every route around the guard —
`--no-verify` (forbidden), `HV_SUBFLOOR_EXCLUDE` (narrows the roster),
`HV_CENSUS_DELIVERY=1` (would misrepresent the commit *and* stand down the
column-count witness it had just re-pinned). **Refusing to satisfy a guard by
becoming its false witness is exactly right**, and it is the same instinct as
`#28`'s "never weaken an anti-vacuity guard," applied to a log rather than a
test.

The row is written now, from Task 10's real profile.

### The protocol did not fit, and saying so was the finding

`anomaly_injection`'s pin prescribes ablating column families to separate
instrument movement from world movement. I flagged that every worked example
in it describes a surface that GREW while ours SHRANK. The implementer
measured, and the method **misfires in both arms** on a shrink:

- **The null is unreachable by construction.** Ablation removes columns; our
  delta is two columns the new census *still contains* but now classifies as
  excluded — already outside the evaluable surface, so ablating them is a
  **no-op** (64/120 before and after). Restoring comparability would require
  *re-admitting* columns the classifier rejected.
- **So the other arm fires spuriously.** A shrink always reads "non-null", and
  the protocol's "THAT is when to stop and say so" would have refused a re-pin
  on the epoch with **the cleanest instrument evidence this witness has ever
  had**.
- **"Family by family" has no unit here**: nothing was registered; the 97
  value-movers span 37 prefixes, 24 of them singletons.

**The generalisation it proposed instead is better than the original:**
ablate the symmetric difference from *both* epochs and ask whether *either*
side moves. Neither did — so the surface delta's share is **exactly zero**.
And the complementary control gave the scorer a null this witness never had:
ablating all 97 value-movers from both epochs reads **51/100 identically, arm
for arm**, with the positive control reproducing the pinned 68/120 and its
exact arm breakdown.

That licenses a re-pin to `(64, 120, 0, 0)` as a **world** move, incomparable
to the six-epoch series for the *ordinary* reason rather than an instrument
one — and explicitly does **not** license adding the reading to that series,
which it did not do. All four named sites re-stated.

### Two measurements worth keeping

**The 41 absorbed commits moved almost nothing.** This branch's own earlier
delivery `bdc59cc18` and this census differ on exactly **three** columns, all
astronomy — and all eleven calibration re-pins return to their delivery-era
values. The merge churn was ours re-measured against main's stale fixtures,
not new world movement.

**The softened sweep found 31 quantities, 12 of them masked** behind a failing
assert — including both halves of the duplicated `goblin` row and the kobold
arms the goblin-first loop hides. It also ran `golden-pins.sql` through DuckDB
**before** editing as an independent second opinion: same 19 rows, same values,
agreeing to ~1e-15. The 38 literals were resynced under an **assertion that
each old literal appeared exactly twice per line**, so the half-moved row I
warned about could not be written.

### A SIXTH instance of `PROC-prose-claims-no-assertion-checks`, parked

`idea-registry.md`'s `TOOL-anomaly-report` row says *"116 evaluable columns —
47 excluded"*; live is **179/53**. Pre-existing, not one of the twelve,
correctly reported rather than touched. Same class as the margin ratio: a
prose number nothing checks. Recorded in the row's instance list.

**Ideonomy passes / overturns:** none; a task acceptance plus a protocol found
not to fit its case.

## #38 [Ruling] — The settlement ceiling retired, adopting The Tidemark's treatment verbatim; and two docs my own change falsified

**Nathan's ruling.** *"The [75, 400] settlement sane band is actually
nonsensical (as we add races, we're going to increase settlements without
bound), and is being dealt with in a separate campaign, The Tidemark — feel
free to look at their worktree to see how they addressed it."*

**What The Tidemark did** (`b5edf6106`, `campaign/the-tidemark`, worktree
`.claude/worktrees/the-shoal`, **not merged to main** — verified with
`git merge-base --is-ancestor`): replaced the two-sided band with a
minimum-viable floor in three tests, raising the floor 40 → 75 in two of them
and deleting the ceiling in all three. Their words: *"subsequent world growth
is a valid success, not calibration drift"*; *"growth above the old
calibration window is healthy and must remain observable."* They also renamed
"sane band" to "historical calibration window" throughout, which is the honest
name — it was never a sanity check.

**Adopted by applying their patch, not by retyping it.** `git apply --3way` of
their four-file diff landed cleanly on this branch. That is deliberate: both
branches merge into main, so byte-identical text merges without conflict while
an independently-worded equivalent would collide for no gain. Three tests pass
(`confluence` 4.872 s, `history_placement` 4.585 s, `history_tumult` 7.635 s).

**Mutation-proved rather than assumed.** A floor of 75 against a reading in the
400s could be a guard that can never fire. Raising `MIN_LIVE_SETTLEMENTS` to
500 reds `emergent_settlement_count_stays_in_the_sane_band` with *"regressed
settlement count: 411 (minimum viable floor 500)"*. Restored.

### The mutation surfaced a conflation I was about to propagate

That red printed **411**, where the note I was editing said **413**. They are
different measurements and this campaign's own text had merged them:

- **413** — `is-settlement` facts in the committed full-build fixture
  `cli/tests/fixtures/world-seed-42.json` (counted directly; `is-place` and
  `population` agree at 413).
- **411** — `all_settlements` on a live `BuildDepth::Settlements` build, which
  is the number a red from those three tests actually prints.

Both clear 400, so the finding is unchanged — but "413 settlements" was a fact
count wearing a settlement count's frame, and I had preserved it verbatim while
rewriting the paragraph around it. Same class as ledger #31a's three peer
errors: *a true number carried into a frame where it means something else.*
Corrected in `weft_ledger_guard.rs` by stating both readings and what separates
them.

### Two docs my change falsified, fixed in the same commit

1. **`staple_d2_probe.rs`'s `SETTLEMENT_COUNT_BAR`** doc claimed to be *"the
   live settlement-count sane band in `history_tumult.rs`"*. Nothing enforced
   that — it is a hand-transcribed copy with **no source-scan** — so my edit to
   `history_tumult.rs` silently made it false. Rewritten to say what it is: The
   Staple D2's **frozen preregistered bar**. The constant is left at `40..=400`
   untouched; decision 0016 forbids retuning another campaign's preregistered
   bar, and The Tidemark left it alone too.

2. **`lib.rs`'s `CONDENSATION_THRESHOLD` sweep record**, two lines below the
   line The Tidemark *did* fix, still read `[100, 400] count band` as a live
   gate. Marked historical ("the then-current", "that window"). A freshness
   sweep skipping the doc adjacent to its own diff is a recorded pattern.

### A finding filed, not fixed: the probe's upper bar is now probably dead

`staple_d2_probe` counts **treatment-only** breaches — a world where control
*and* treatment breach is deliberately not counted. That is not inferred; the
probe's own positive control pins it
(`demographic_instability_counts_only_treatment_only_bar_breaches`, whose
`pairs[1]` sets control **and** treatment to 401 and expects a count of 1, from
the unrelated `39` case). So once the baseline settlement count clears 400
everywhere, both arms breach on every world and the settlement half of that
instability detector **can never fire again**. Both live probes are
`#[ignore]`d, so nothing has observed it; the next run would read a silent zero
as health.

Filed in the constant's own doc rather than repaired: it is The Staple D2's
bar, the decision is theirs, and whoever re-runs it should re-derive the counts
rather than trust my note. Class: *a guard over a lifecycle population expires
when the lifecycle advances.*

### Nathan's springs observation, recorded as he asked

*"Springs would be a really good place to have settlements, I'd think. Maybe we
should record that for later."* Filed as
`SOC-springs-are-invisible-to-settlement-siting` (raw / high, 581 chars against
the 600 cap). It is a **missing join, not a tuning gap**, and Task 14 already
measured it: `Hydro::Spring` has exactly two readers — the linguistic exposure
classifier and one lab metric — while the bake sites settlements from
`carrying_capacity`, whose freshwater term reads river proximity only. Zeroing
springs entirely left counts identical on seeds 0/7/42 (325/340/411). `DOM-5`
already records the river half as shipped (The Confluence); springs are the
half nobody wired.

**Ideonomy passes / overturns:** none.

## #39 [Prereg] — Task 15: the yield form. Methane's deficit is ARITY, not chemistry — frozen before the code that moves it

**Written before any Task 15 code exists** (decision 0016). The predictions
below are falsifiable and H4 is the one that would sink the whole approach.

### The structural reading

Reading all seven yields in `windows/worldgen/src/energy.rs` (`yield_at`), six
of seven are products of terms each individually shaped into `[0,1]`:

| source | form | terms |
| --- | --- | --- |
| Serpentinization | `bump(silica) · moisture` | 2 |
| IronReduction | `bump(silica) · water_gate(moisture)` | 2 |
| Radiolysis | `bump(silica) · water_gate(moisture)` | 2 |
| SulphideOxidation | `metamorphic_grade · front · water_gate` | 3, each shaped |
| **Methanogenesis** | **`carbonate · porosity · moisture`** | **3, raw** |
| Geothermal | `t/(t+reach) · water_gate` | 2 |
| DetritalImport | `depth_falloff · drainage_term` | 2 |

A product of *k* terms each ~U[0,1] has mean `1/2^k`. So a 3-product sits
structurally at half a 2-product **whatever the chemistry says**, and Task 12's
measured dominance — H .258 / Fe .339 / S .280 / CH₄ .124 — is what that
predicts. Methane is not rare because methanogenesis is hard; it is rare
because it was modelled with one more factor than its neighbours.

That is the argument for the **geometric mean** as the general rule: yield
becomes the *k*-th root of a *k*-term product, so it reads "typical gate
satisfaction" instead of "joint probability", and a source is not penalised for
being described in more detail. It is a rule, not a tuned constant — which is
the only reason it is admissible at all under 0016.

### Frozen predictions

- **H1 (arity explains the deficit).** Under geometric-mean-everywhere,
  methane's dominance share rises from **0.124 to ≥ 0.18**, and the spread
  (max − min dominance across the four axes) falls below the current
  **0.215** (0.339 − 0.124).
- **H2 (depth structure survives).** Sulphur still wins **zero** readings at
  `Band::Undercroft`. Any sulphur win at Undercroft falsifies H2 and says the
  transform destroyed the depth handoff Task 12 found.
- **H3 (no degeneracy).** No axis exceeds **0.60** dominance, and each axis
  keeps **≥ 1,000 distinct values** over the sample. Task 13 bought carbonate
  19,681 distinct values; a transform that collapses that is a loss, not a fix.
- **H4 (THE FALSIFIER FOR THE WHOLE APPROACH — discrimination survives).** A
  geometric mean compresses toward the middle. If it compresses far enough, all
  four axes read nearly equal everywhere, no chamber can be named by its
  chemistry, and step 5's leader-and-margin readout is dead on arrival.
  Prediction: the **median leader margin stays ≥ 0.02**. Below that, the
  uniform geometric mean is REFUSED regardless of how good H1–H3 look, and the
  fallback is the narrower change (methane alone, Task 12's arm D).
- **H5 (overshoot is also a finding).** If methane's dominance lands **above
  0.35** it has become the leader too often and the rule has overcorrected.

### What must be measured against the POST-Task-13 world

Task 12's numbers were taken before Task 13 widened `carbonate` (2 → 19,681
distinct) and `metamorphic_grade` (5 → 4,248). Both feed yields in this table —
carbonate is one of methanogenesis's three terms and metamorphic grade is one
of sulphide oxidation's. **So the Task 12 baseline is stale for exactly the two
sources this ruling is about**, and the probe must re-measure the current forms
as its own control rather than citing 0.124/0.339 from the earlier entry. Those
figures appear above as the *prediction's* reference point, not as a live
reading. Re-derive them.

**Ideonomy passes / overturns:** none.

## #40 [Q] — CORRECTION: my own H4 falsifier could never have fired, and it failed open in the direction I wanted

Two corrections to entry **#39**, both found by the Task 15 implementer and
both verified against the code before being accepted here.

### 1. H4 was vacuous, and vacuous in the flattering direction

I wrote H4 as *"the median leader margin stays ≥ 0.02"* and called it "THE
FALSIFIER FOR THE WHOLE APPROACH". The quantity it names already existed:
`leader_margin` in `metabolite_variety_probe.rs` (line 481) returns

```rust
s.sort_by(|a, b| b.total_cmp(a));
s[0] / s[1]
```

— a **ratio of the leader to the runner-up**, which after a descending sort is
bounded below by `1.0` by construction. A `>= 0.02` threshold on it is
satisfied at every reading that exists, under every arm, no matter how
degenerate. **The one check I designated as able to sink the whole approach was
the one check that could not fail.**

The direction matters more than the fact. H4 was supposed to be the clause that
stopped a compressing transform from shipping; written this way it would have
returned HELD for a transform that flattened all four axes into a tie. It
failed open, toward the answer I had already argued for two paragraphs earlier
in the same entry.

**The repair (the implementer's, and it is the right one):** a new
`leader_margin_diff` — absolute `leader − second` on the axis-value scale,
where compression genuinely drives it toward zero — with H4 adjudicated against
that. Both readings happen to hold on the measured arms, which is luck and not
evidence the clause was working.

The class is one this campaign has now produced twice: *a check that can never
fire is worse than an absent one*, and its cause here is the narrower rule —
**a preregistered numeric threshold must name the QUANTITY and its UNIT, not
just a number.** "Margin" named two different quantities (a ratio and a
difference) that differ by whether 0.02 is trivial or decisive, and I never
said which. The probe was free to pick either, and the honest implementer
picked the one under which the prediction could lose.

### 2. The arity table in #39 mis-stated sulphide oxidation

#39's table calls `SulphideOxidation` *"3, each shaped"*. Verified false:
`buffer.metamorphic_grade` (`energy.rs:574`) is a raw `pub` field on the
material buffer (`domains/terrain/src/lithology.rs:96`), used directly with no
`bump()` or `water_gate()` — exactly like `carbonate` and `porosity` in
methanogenesis. So sulphide oxidation is a 3-term product carrying **one raw
factor**.

The arity argument survives unchanged (both 3-term sources have k=3, and that
is what the geometric mean addresses). What does not survive is the sharper
rhetorical claim standing beside it — that methanogenesis is the *lone* bare
product of raw fields. It is the only **all**-raw one; it is not the only one
with raw factors. I wrote the stronger version because it made a cleaner story.

**Ideonomy passes / overturns:** none.

## #41 [Ruling] — Task 15: GEO ships. H1 is falsified and the target was mine to mis-set. And the ladder worry was a conflation, not a defect in the transform.

### The conflation, first, because it is what the entry is for

I had been treating "which yield form ships" and "how a raw magnitude maps onto
the corpus ladder" as one question. They are two, and they are **orthogonal**:

- The yield form decides **which axis wins where** — dominance, variety, the
  depth handoff. That is what a chamber's *character* is made of.
- The ladder decides **where a magnitude sits between LEAN and TEEMING**. That
  is what a chamber's *reading* is.

They do not interact, and the reason is mechanical: the saturating transfer
this project already uses, `f(x) = x/(1+x)`, has `f'(x) = 1/(1+x)^2 > 0`, so it
is **strictly monotone** and preserves `argmax` exactly. Dominance shares are
identical with or without it. So no choice about band mapping can change the
yield-form answer, and no yield form can fix a band-mapping problem.

I asked the implementer whether GEO pushes the other three axes into `.5–1.0`
and breaks the ladder at the top the way CONTROL breaks it at the bottom. The
answer is **yes, exactly as framed** — CONTROL's four medians occupy the bottom
two bands, GEO's occupy the top two — and it is **not an argument against GEO**,
because it is not a fact about GEO. It is a fact about measuring four axes
against one shared ladder.

### The ruling: GEO ships

| | CONTROL | **GEO** | SELECTIVE (k=3 only) |
| --- | --- | --- | --- |
| dominance H / Fe / S / CH₄ | .369 / .419 / .169 / **.043** | .287 / .402 / .206 / **.105** | — / — / — / **.242** |
| spread (max − min) | .376 | **.297** | — |
| sulphur wins @ Undercroft | 0 | **0** | **2,347 / 20,969 (11.2%)** |

GEO wins the question the yield form actually answers: the spread falls
`.376 → .297`, methane's dominance nearly triples, sulphur rises `.169 → .206`,
and H2/H3/H4/H5 all hold. The two depressed axes are exactly the two fed by
`k=3` sources (sulphur ← SulphideOxidation, methane ← Methanogenesis) while
hydrogen and iron are fed by `k=2` sources — so the arity mechanism predicts
the observed pattern, and the correction moves the right two axes.

### H1 is FALSIFIED, and the target was miscalibrated by me

Methane reaches `.105` against my predicted `≥ .18`; spread `.297` against my
`< .215`. **Arity does not fully explain the deficit.**

The honest decomposition, kept separate on purpose so the second does not
launder the first:

1. **The prediction is falsified.** Correcting arity leaves methane well short
   of parity. That is the finding and it ships as the finding.
2. **The target was built on a number that no longer existed.** I extrapolated
   `≥ .18` from Task 12's `.124`, which Task 13 had already invalidated by
   widening `carbonate`. On this tree CONTROL methane is **`.043`**, so GEO's
   `.105` is a **2.4x** improvement against the real baseline rather than the
   1.5x my target imagined.

(2) is a defect in my preregistration, not evidence for the hypothesis. The
direction is confirmed and large; the magnitude claim is dead. Both are true
and neither rescues the other. Per decision 0016 the constant is not being
retuned to reach `.18`, and the chronicle will carry the falsification.

### SELECTIVE is REFUSED, and its mechanism is the best argument for uniformity

Geometric mean on the two `k=3` sources alone gives a better ladder spread AND
better methane dominance (`.242`) — and falsifies **H2**: sulphur wins 11.2% of
`Band::Undercroft` readings where the ΔT-front physics says ~0 and where both
CONTROL and GEO report exactly zero.

The implementer's mechanism, which is the durable part and is stronger than my
own arity argument: **a cube root inflates a near-zero value far more, in
relative terms, than a square root does.** Boost sulphur without boosting the
axes it competes against and it leaks into shallow depths it has no physical
business occupying. Under uniform GEO, hydrogen and iron get the same *kind* of
boost, so the ordering that keeps sulphur at zero near the surface survives.

Stated generally: **what preserves an invariant is the boost DIFFERENTIAL
across competing axes, not any one axis's correction in isolation.** A
non-uniform fix can break a physical invariant that a uniform one preserves.
That argues for uniformity better than "it is a principled rule" does, because
it names a thing that actually breaks.

### The fourth candidate is DECLINED, on two independent grounds

The proposal: apply `x/(1+x)` per-axis before band classification, reusing the
corpus ladder. Declined because —

1. **Nathan already ruled it out.** *"I believe we want per-axis bands... so I
   think we want raw numbers and [no] lossy abstractions."* Forcing four axes
   through one saturating transfer onto one shared `E_LEAN..E_TEEMING` ladder
   is precisely the lossy abstraction that ruling rejects.
2. **`E_TEEMING` would be unreachable by construction.** `f(x) = x/(1+x)`
   reaches `0.75` only at `x = 3` and **approaches 1.0 asymptotically without
   ever attaining it**. Every axis's top band would be a category nothing can
   ever occupy — the same defect as this campaign's own vacuous H4 (#40), one
   level up. A band no reading can enter is not a band.

The observation underneath it is still valuable and is kept: iron and sulphur
**already** exceed 1.0 in 31% and 16% of CONTROL readings, before Task 15
touches anything. The per-axis mismatch is pre-existing and is Task 16's to
answer.

### What Task 16 therefore is

Set band thresholds **per axis, against that axis's own post-GEO distribution**,
in raw units. The compression worry dissolves: it existed only because four
axes were being read against one ladder. Per-axis bands do not care that
methane's median is `.644` and sulphur's `.541` — they care whether each axis
discriminates *within itself*, which is the question "are there methane-eating
fungi in this chamber" actually asks.

**Ideonomy passes / overturns:** one — the boost-differential finding above
generalises past this campaign and belongs in the retrospective.

### Addendum — arm D looks best on the summary statistics, and that is an artifact of what was being watched

Re-running the probe myself reproduced every headline figure exactly (CONTROL
`.3692/.4190/.1689/.0429`; GEO methane `.1052`, spread `.2965`; SELECTIVE's
sulphur leak `2,347/20,969`; all five verdicts). One thing deserves stating
because it argues *against* the arm the numbers flatter.

**Arm D — a cube root on methane alone — beats GEO on every summary statistic
reported:** spread `.2357` vs `.2965`, methane dominance `.2963` vs `.1052`,
and the best ladder-band spread of any arm (medians in three distinct bands
against GEO's two). It also holds H2, because it does not touch sulphur.

*(Arm D's dominance figures are the implementer's; my own re-run printed a
dominance block only for SELECTIVE. What I did reproduce directly is the
structural fact below, and it is the one the argument rests on.)*

**In my run, arm D's per-axis rows for hydrogen, reduced_iron and
reduced_sulphur are byte-identical to CONTROL** — `.368150` / `.331819` /
`.080173` medians, to six places — and only methane moves. That is what makes
the flattering statistics legible:

1. **Dominance is `argmax`, so it is zero-sum.** Methane's rise from `.043` to
   `.296` is necessarily taken from the other three. Hydrogen falls
   `.369 → .165` — a 55% relative demotion of an axis *nothing about which
   changed*. The spread "improved" because the distribution happened to land
   nearer uniform, not because any axis is better calibrated.
2. **The ladder-band spread is an artifact of the same thing.** Three axes stay
   at their low CONTROL values and one is lifted into a higher band. Occupying
   three bands is arithmetic, not evidence of better band behaviour.
3. **And the check that would have caught it was not written.** SELECTIVE was
   refused because H2 preregistered a *sulphur* invariant, and SELECTIVE
   boosted sulphur. **No equivalent invariant was preregistered for hydrogen**,
   which is the axis arm D damages. So "arm D holds H2" is much weaker evidence
   than it reads as: H2 does not watch the axis arm D moves.

That is the boost-differential finding again, from the other side: the same
mechanism that let SELECTIVE leak sulphur into the Undercroft lets arm D demote
hydrogen, and only the first was instrumented. **A targeted fix is not safer
than a uniform one for being narrower — it is only less likely to trip a guard,
because the guards were written for the axis somebody was already worried
about.** GEO corrects every axis for its own arity so the comparison stays
like-for-like; arm D inflates one axis until it wins more often. The first is a
calibration, the second is a thumb on the scale, and the summary statistics
cannot tell them apart.

**Carried to the retrospective**, because it generalises past this campaign:
*an asymmetric invariant set makes narrow fixes look safe.*

## #42 [Prereg] — Task 16: per-axis bands. The RULES are frozen here; the numbers are not chosen yet, and that ordering is the point.

Written **before** the post-GEO distributions exist. Task 15's shipping change
is still in flight, so the world these thresholds describe has not been
measured yet. That is deliberate: thresholds picked after looking at the
distribution are fitted, and I would not be able to tell my judgement from the
data's shape. What follows freezes **how** a threshold is chosen and **what
would make the scheme fail** — not one number.

### What Nathan asked for, quoted, because the design answers it directly

> *"I believe we want per-axis bands — we're trying to determine which flora
> and fauna are in which particular zones, e.g. allow wild or cultivated
> methane-eating fungi as a basic nutrient for a Drow empire or whatever, so I
> think we want raw numbers and [no] lossy abstractions."*

Three requirements, taken separately:

1. **Per-axis**, not one shared ladder. Justified by measurement rather than
   taste: under GEO the four axes are not alike. Hydrogen is **absent in 39%**
   of readings while methane is **never zero anywhere** (`zero_frac` `.3899` vs
   `.0000`). A single threshold set cannot serve both — it would call methane
   "present" everywhere, which is true and useless, and hydrogen's absence
   would be the only thing it ever said.
2. **Raw numbers preserved.** Bands are a *named view over* the raw magnitude,
   never a replacement for it. The raw per-axis value stays available to every
   consumer; a species requirement may cite either. This is what "no lossy
   abstractions" forbids — a band that discards the number it summarises.
3. **Bands exist to answer "what can live here."** So the test of a band is
   whether it *partitions habitat*, not whether it looks tidy.

### The threshold rule (frozen)

- Thresholds are **authored constants with stated provenance**, per axis, in
  **raw units** — not quantiles computed at runtime. A runtime quantile would
  make every band a function of whatever world is loaded, so the same chamber
  would change its description when an unrelated seed moved. Bands must be a
  property of the *chemistry*, not of the sample.
- Each threshold's provenance comment names the distribution it was read
  against **and the date**, because a committed baseline is a claim with a date.
- No threshold may be moved to rescue a downstream count (decision 0016).

### THE ANTI-VACUITY GUARD, which is this campaign's own finding turned into a test

Ledger #41 declined per-axis saturation onto the corpus ladder partly because
`x/(1+x)` approaches `1.0` asymptotically without attaining it, so `E_TEEMING`
would have been **a category nothing could ever enter**. That is the same
defect as this campaign's vacuous H4 (#40), one level up, and it is the failure
mode most likely to recur here.

So the guard is **two-way and mandatory**:

- **Every band, on every axis, must be occupied** by at least a stated minimum
  fraction of readings over the probe's seed/rung sample. An unreachable top
  band fails. A bottom band nothing falls into fails too.
- The guard asserts **occupancy of each band**, never merely that the
  thresholds are ordered — ordering is what the type system already gives, and
  a guard the type system guarantees is not a guard.

### Frozen predictions

- **K1.** A per-axis threshold set exists, for all four axes, satisfying the
  occupancy guard. *Falsifier:* if some axis's distribution is so concentrated
  that no threshold set makes every band non-empty, that axis **cannot be
  banded at this resolution** — report it as a finding and reduce that axis's
  band count rather than forcing thresholds to fit.
- **K2.** The bands partition **habitat**, not just numbers: on at least one
  axis, band membership varies across the five depth rungs rather than being
  constant with depth. *Falsifier:* if every axis reads the same band at every
  rung, the bands describe the world no better than a constant does, and the
  scheme has failed at its stated purpose whatever the occupancy numbers say.
- **K3.** Hydrogen's 39% zero population lands in a band that **names absence**
  rather than being pooled with merely-low readings. "Absent" and "poor" are
  different affordances for a species requirement; collapsing them is exactly
  the lossy abstraction requirement 2 forbids.

**Ideonomy passes / overturns:** none.

## #43 [Q] — CORRECTION: the deliberately-red set is 53, not "~64". I had restated an unmeasured figure four times.

The repair round's scope has been carried in this campaign's own text as
*"roughly 64 workspace tests are deliberately red"*, and I have repeated it in
that form across several entries and one dispatch brief without ever running
the suite to check.

**Measured** on the unmodified tree at `b97dbf3a8`, one full
`cargo nextest run --workspace --no-fail-fast` (6,234 tests):

```
grep -E '^\s+FAIL \[' /tmp/hv-before.txt | sed -E 's/^.*\)[[:space:]]+//' | sort -u | wc -l
53
```

**53**, not 64. The sorted list is preserved as the attribution baseline for
Task 15's shipping change, which is the only reason the number got measured at
all — the dispatch needed a before-set, and producing one falsified the figure
in passing.

Where 64 came from is not recoverable from the record, which is itself the
finding: **no entry ever cites a command or a date for it.** It may have been
true when first written and shrunk as Tasks 12–14 repaired things incidentally,
or it may never have been counted. Both readings are consistent with the
evidence and neither is better than measuring.

This is the same class as **#27** (a rotted reason restated three times) and
**#32** (a claim repeated eleven times, asserted zero times), and it is the
third instance in this one campaign. The tell was available each time: *I wrote
the same number in the same breath as the same instruction, and agreement with
myself felt like corroboration.* The rule the three share, stated once:
**a count with no command beside it is an estimate wearing a measurement's
clothes** — and a scope figure is exactly the kind that gets planned against.

No downstream decision changes: the repair round's plan was "one round, after
Tasks 15–17 settle", which is indifferent to whether the set is 53 or 64. The
correction matters because the next reader would have sized the work off it.

**Ideonomy passes / overturns:** none.

## #44 [Q] — STOP: the geometric mean costs 40% of seed 42's population, and every code path I can read says it should have done the opposite

Task 15's shipping change is implemented and correct-as-specified (seven arms,
verified arities, `math::powf`, zero term still yields zero, fmt/clippy clean).
It is **not committed**, because measuring the world it produces turned up a
regression large enough to reopen the ruling.

### The measurement

`fixture::the_fixture_equals_a_live_build` reds: the committed fixture holds
**23,810** facts, a live build now produces **13,262** — a 44% fall. Predicate
families, counted directly from a freshly generated seed-42 world against the
committed one:

| predicate | shipped | GEO | |
| --- | --- | --- | --- |
| `is-settlement` / `is-place` / `population` | 413 | **256** | −38% |
| `is-occupation` | 1,211 | **560** | −54% |
| `is-ruin` | 800 | **306** | −62% |
| `is-person` | 244 | 234 | −4% |

**The benign explanation was tested and failed.** If higher carrying capacity
had merely consolidated people into fewer, larger settlements, total population
would hold and median size would rise. It does not:

| | settlements | total population | median size | max size |
| --- | --- | --- | --- | --- |
| shipped | 411 | **11,726** | 26 | 84 |
| GEO | 254 | **7,001** | 27 | 84 |

Settlement *sizes* are unchanged to the person at both ends. The world supports
**40% fewer people** in the same-sized settlements. It got poorer, not denser.

### Why this is a stop and not a repair

**Every path I can read says the number should have gone UP.** The geometric
mean of terms in `[0,1]` is ≥ their product, so every one of the seven yields
rises; `chemical_supply` sums them and multiplies by a geothermal modifier that
also rises; and the capacity loop's use of the result is monotone —
`axis_supply_with` is a plain weighted sum (`lib.rs:1576`) and `score_at`
saturates it with `supply / (1.0 + supply)`, which is strictly increasing.
`EnvironmentVector::new`'s `[0,1]` rejection, my first hypothesis, is not on
this path at all: the per-axis values never reach it, only the saturated
aggregate does. The one surface consumer,
`marine_chemosynthate_supply_field`, is documented as reaching **no** consumer
("No `Surface`-realm kind weights `CHEMOSYNTHATE`") and cannot be the cause.

So a change that raises every input reduced the population by 40%, and reading
the code does not explain it. **That is the finding.** Either there is a
compensating normalisation I have not found, or the effect runs through species
*assignment and competition* rather than capacity — a different species mix
winning the underworld and proving less productive. Both are worth knowing;
neither is established, and I will not guess at it in a chronicle.

Applying the campaign's own rule (#41, #43): *a mechanism read from the code is
a hypothesis until counted.* I have counted the effect and not the cause.

### What is NOT in doubt

The Task 15 *measurement* stands unchanged — GEO does improve metabolite
variety on every axis it was chosen for (spread `.376 → .297`, methane's
dominance nearly tripled, H2–H5 held). Nothing above touches that. What is now
in doubt is whether that improvement is worth its price, and the price was
invisible until the world was built, because the probe measured **supply
fields** and never built a **world**.

That is the transferable lesson and it is one this project keeps relearning:
**a probe over a field is not a probe over a world.** The four-metabolite probe
sampled 104,845 readings and could not have seen this, because population is
downstream of a history bake the probe never runs.

### Held for Nathan

Three options, none taken unilaterally — this is a ruling about what the world
is for, not a repair:

1. **Investigate the mechanism first**, then decide. Cost: one focused
   measurement pass. My recommendation, because shipping or reverting on an
   unexplained 40% is the same mistake in either direction.
2. **Ship GEO and accept a poorer, more chemically varied world.**
3. **Keep the shipped yield forms** and take the narrower methane-only
   correction, which #41's addendum already argued against on its own merits.

**Ideonomy passes / overturns:** none.

## #45 [Ruling] — Root cause found: one non-metabolite arm did all of it, the four metabolite axes have NO CONSUMERS, and #44's "40%" was seed 42's anecdote

Investigated under `systematic-debugging`. Four phases, no fix proposed before
the cause was established.

### Positive control first

Neutralising `geometric_mean` to a plain product restores seed 42 to **23,810**
facts — the committed fixture exactly. The change is the sole cause; the
rebaseline and the absorb contributed nothing.

### The bisection: six arms cost nothing, one costs everything

Env-gated per-arm instrumentation, one build, nine runs, seed 42:

| arm | mask | facts |
| --- | --- | --- |
| all off (control) | `0000000` | 23,810 |
| Serpentinization → `HYDROGEN` | `1000000` | 23,810 |
| IronReduction → `REDUCED_IRON` | `0100000` | 23,810 |
| Radiolysis → `HYDROGEN` | `0010000` | 23,810 |
| SulphideOxidation → `REDUCED_SULPHUR` | `0001000` | 23,810 |
| Methanogenesis → `METHANE` | `0000100` | 23,810 |
| Geothermal → Modifier | `0000010` | 23,810 |
| **DetritalImport → `DETRITUS`** | `0000001` | **13,262** |
| all on | `1111111` | 13,262 |

`DetritalImport` alone reproduces the entire collapse. **The four metabolite
axes — Task 15's whole subject — cost exactly nothing.**

### THE ROOT CAUSE: the metabolite axes have no consumers

Counted in `domains/species/src/lib.rs`: the number of niche weights on
`HYDROGEN`, `REDUCED_IRON`, `REDUCED_SULPHUR` and `METHANE` is **zero, on all
four**. Only the `CHEMOSYNTHATE` aggregate is weighted, once. `DETRITUS`, by
contrast, is weighted by **six** niches — three at `1.0`, one at `0.7`, two
(the underworld peoples) at `0.50`.

So the asymmetry is total. The metabolites are **measured but never eaten**;
`DETRITUS` is eaten by six species whose weights were calibrated when its
supply was a flat `DETRITUS_AMBIENT = 0.2` land-mask (this campaign's own Task
4 made it variable). Changing a metabolite axis is invisible to the world;
changing `DETRITUS` rescales six niches' entire food supply.

**Positive control on the instrument, because "no change" and "dead
instrument" look identical.** Running the Task 15 probe with the metabolite
mask on, its `CONTROL` arm — which calls production `yield_at` — reads
`.2873/.4017/.2057/.1052`, GEO's exact values, against `.3692/.4190/.1689/
.0429` unmasked. The fields genuinely moved. The world did not.

### CORRECTION to #44: "40% of the population" was one seed

#44 reported a 40% population loss as the effect. It is seed 42's anecdote.
Across five seeds, the `DetritalImport` change:

| seed | shipped | geo-detritus | delta |
| --- | --- | --- | --- |
| 0 | 17,340 | 16,892 | −2.6% |
| 7 | 15,213 | 13,536 | −11.0% |
| 13 | 36,987 | 36,961 | −0.1% |
| **42** | 23,810 | 13,262 | **−44.3%** |
| 100 | 7,178 | 6,318 | −12.0% |

The effect is real and systematic — negative 5/5 — and its typical magnitude is
around 10%, not 40%. Seed 42 is a 3–4× outlier. **One world is an anecdote**,
and I published one as an effect size in the entry immediately above.

The response is also **non-monotone**, which is why magnitude reasoning failed:
holding everything else fixed and varying only the detritus yield gives 20,840
(zero), 19,431 (quarter), 19,104 (half), **23,810 (shipped product)**, 20,205
(double), 13,262 (sqrt), 22,857 (constant 1.0). Since `product ∈ [0,1]`,
`sqrt(product)` lies *between* product and 1.0 — and both endpoints score
~23,000 while the midpoint scores 13,262. The outcome tracks the capacity
field's **ordering**, which the bake consumes discontinuously, not its
magnitude. That is why every monotone argument I made from reading the code
was correct about the inputs and useless about the result.

### The ruling

The geometric mean ships on the **five sources routed to the four metabolite
axes**, and not on `Geothermal` or `DetritalImport`. Verified world-neutral:
seeds 0/7/13/42/100 produce 17,340 / 15,213 / 36,987 / 23,810 / 7,178 — byte-
identical to shipped, and `fixture::the_fixture_equals_a_live_build` passes.

**Why this is not the narrow fix #41 rejected**, stated exactly because it
looks like one:

- #41's `SELECTIVE` boosted **some** members of the competing set (the two
  `k = 3` sources), creating a boost differential *inside* a competition, and
  it measurably broke the depth handoff.
- This boosts **all five** members of that set, so no differential is created
  within it, and H2 holds by construction (the probe's `GEO` arm reads sulphur
  winning 0 at Undercroft).
- `Geothermal` is a **common factor** across all four metabolite axes, and a
  common positive factor cannot change an `argmax` — outside the competition by
  construction, not by choice.
- `DetritalImport` is alone on a different axis with different consumers. It
  competes with nothing, so arity fairness has no purchase; and it is the one
  arm with real consumers to decalibrate.

The full argument lives on `geometric_mean`'s own doc comment, not only here.

### The finding that outlives this task

**We have built a chemistry that nothing eats.** Four metabolite axes, a
calibrated yield form, per-axis distributions, a 104,845-reading probe — and
zero species that consume any of it. That is not a defect in the work; it is
exactly the gap Nathan's own goal names ("allow wild or cultivated
methane-eating fungi as a basic nutrient for a Drow empire"). It does mean the
honest description of Tasks 12–17 is **preparatory**: they make the chemistry
coherent and describable so that something can be authored to live on it.

It also explains why Task 15's shipping change is free, and why that freedom is
temporary: the first species that weights `METHANE` makes every one of these
choices world-affecting, and the multi-seed discipline this entry had to
discover will be mandatory from that moment.

**Ideonomy passes / overturns:** one — *an axis with no consumer is
unfalsifiable by the world*, which generalises to any field a simulation
computes and nothing reads.

## #46 [Ruling] — What "food for things that do not exist yet" actually implies, enumerated against the code

Nathan: *"this campaign exists to provide food for things that don't strictly
exist yet. Does that have practical implications for what we do in the
remaining tasks?"* It does, and #45's headline was too broad. Corrected and
enumerated.

### First, the accurate state of the seam — "nothing eats it" was imprecise

| consumer | what it reads | can it tell methane from hydrogen? |
| --- | --- | --- |
| species niches (`domains/species`) | the four axes, **all weights 0** | **no** — the data is missing |
| `per_species_capacity`'s dot product | all four axes, per-axis | **capable, unused** |
| `dominant_inhabitant` (`windows/vessel`) | `energy: f64`, the saturated **sum** | **no** — the scalar discards identity |
| `dominant_source` → `source_phrase` → chamber prose | the **argmax** source | **YES — and it is the only one** |

So the chemistry is **narrated but not eaten**. `inhabitant_datum` renders "A
xorn moves in the dark here, drawn to the porous, water-logged carbonate", and
all seven sources carry distinct phrases. #45 said the axes have no consumer;
they have no *ecological* consumer, and exactly one *descriptive* one.

**The most useful consequence: this is a DATA gap, not an ARCHITECTURE gap.**
`per_species_capacity`'s `per_axis` list already carries `HYDROGEN`,
`REDUCED_IRON`, `REDUCED_SULPHUR` and `METHANE` into the capacity calculation
with a per-species weight. A future author writes
`ResourceVector::new(&[(METHANE, 1.0)])` and it works. Nothing needs building.

### Implication 1 — the campaign's central claim is untested end to end

Every guard here is over a **field**. Nothing asserts the contract the campaign
is actually selling: *that a niche weighting `METHANE` gets more capacity where
methane is abundant.* That is exactly why "the axes are inert" survived twelve
tasks unnoticed.

**Resolution:** a **synthetic-consumer test**. Construct a `BiosphereTraits`
weighting `METHANE 1.0` in the test itself, run it through the real
`per_species_capacity_at`, and assert its capacity tracks methane supply and
diverges from a `HYDROGEN 1.0` twin at the same vertices. No species is
shipped, no accession cohort moves, no world changes, no epoch. It makes every
calibration in this campaign falsifiable **now** rather than whenever biota
arrive.

### Implication 2 — Task 17 should retarget onto the consumer that exists

The plan was "a metabolite leader-and-margin readout". There is already a
leader readout in the player's face (`source_phrase`), so a parallel one would
be a readout nobody reads.

**Resolution:** Task 17 **improves the chamber prose** instead — name the
leader *and* whether it is decisive or contested, which is precisely
leader-and-margin, delivered where a human meets it. Testable in both
directions: the phrase must vary across chambers, and a near-tie must read
differently from a runaway.

### Implication 3 — Task 16's bands are an INTERFACE, not a description

#42 framed thresholds as read off the measured distribution. With no
ecological consumer, fitting to that distribution is fitting to variation
nothing will ever notice, and it will rot silently.

**Resolution, amending #42:** choose thresholds for **authorability** — round,
nameable, physically meaningful values a species author can reason about —
keep #42's two-way occupancy guard, and **add a distribution-drift guard**.
That guard is the deliberate substitute for the missing consumer: if the
distribution moves out from under the bands, nothing else in the tree will
ever go red. #42's other clauses stand.

### Implication 4 — there are TWO fit paths and only one carries identity

`per_species_capacity` (the bake) is per-axis capable.
`dominant_inhabitant` (the walk) takes `energy: f64`. So even after a species
weights `METHANE`, **chamber residents would still be chosen by total energy,
not by chemistry.** A methane specialist would be sited correctly by the bake
and then ignored by the walk.

**Resolution:** not this campaign's to fix — it is a signature change on a
vessel seam — but it is the first thing the biota campaign will hit, so it is
recorded as an idea-registry row rather than a ledger line.

### Implication 5 — do NOT author a species here

Authoring one is an accession-cohort event (an inserted cohort re-deals every
later lexicon draw and renames committed peoples), world-changing, and census
churn. Nathan already ruled the biota out to a successor campaign under Option
A. **Hold that line**; prove the seam with Implication 1's test and register
the gap.

### Implication 6 — the absence must be recorded where a future author hits it

Not only in this ledger. A note belongs on the axes themselves: these four
have zero niche weights today, and **the first niche that weights one makes
every calibration in this campaign world-affecting**, at which point the
multi-seed discipline #45 had to discover becomes mandatory rather than
advisory.

**Ideonomy passes / overturns:** one — *a field with no consumer is
unfalsifiable by the world, so its guard must be a drift check over its own
distribution.* That generalises to anything a simulation computes before its
reader exists.

## #47 [G5] — Task 16: the per-axis bands, and three statistics the guard itself falsified

The bands ship as an **interface** rather than a description, per #46's
amendment to #42: authored cuts in raw units, per axis, with the raw `f64`
fields still public and authoritative. Everything below was found by writing
the guard and running it, not by designing it.

### The cuts, and why one set was re-placed

Measured at the shipped yield form (12 seeds x 5 rungs, 104,845 readings):
hydrogen `0.000 / 0.643 / 0.958` with **39.0% exactly zero**; iron
`0.386 / 0.645 / 1.175`; sulphur `0.179 / 0.461 / 1.463`; methane
`0.313 / 0.546 / 0.867` with **0.0% zero**. Four differently-shaped
distributions, so one shared ladder cannot serve them — which is the
measurement backing Nathan's "per-axis bands", not a preference.

**Hydrogen's cuts were authored, measured, and re-placed.** Round quarter-steps
(`0.25 / 0.50 / 0.75`) put **75** readings in `Trace` and **57** in `Thin`
against 5,670 and 7,503 above — two of five bands holding 0.2% of the axis
between them. That *passes* an occupancy guard and is still a bad ladder: a
band holding one reading in four hundred is a category no author would name.
Re-placed to `0.50 / 0.70 / 0.90`, giving `132 / 3,787 / 5,801 / 3,585`. This
is the occupancy guard doing its job, not a fit-to-result: the cut moved
because a band was vestigial, not to reach a number.

### The empty-band roster needed TWO reasons, not one

The guard's first run failed on `reduced_iron/Absent` — empty over three seeds.
Exempting it as unreachable would have been **false**: the 12-seed run measures
iron's zero share at `0.0002`, about 1 in 5,000, so 29,305 readings expect ~6
and observing none is ordinary. The band is real; this sample cannot resolve
it.

So `EXPECTED_EMPTY` carries a `WhyEmpty` discriminant:

- `NeverOccurs` — the world **cannot** produce it. Two-way guarded: if one ever
  occurs the claim was false and the row must go. Carries `METHANE/Absent`
  (the three gating terms are never simultaneously zero).
- `BelowSampleResolution { per_100k }` — the world **does** produce it, more
  rarely than this probe resolves. Carries the measured frequency so a reader
  can check the arithmetic instead of taking the word "rare", and is
  deliberately **not** two-way guarded, because observing one would confirm the
  row rather than refute it.

Collapsing these into one row type would have let an "it cannot happen" that
is really an "I did not look hard enough" sit here looking identical to a true
claim.

### THREE statistics I chose were wrong, and the guard caught all three

1. **Ratio spread, where absolute was the question.** The geometric mean
   compressed every axis's `p90/p10` hard — methane `19.7x -> 2.8x` — which
   read as a flattened world. Bands use **absolute** thresholds, and in
   absolute terms methane's range *grew* 35% (`.411 -> .554`) and sulphur's
   held. I nearly reopened Task 15 over the wrong statistic.
2. **Modal band, where a distribution shift was the question.** K2's first form
   compared each axis's modal band shallow-vs-deep and reported **1 of 4** axes
   moving. That was an artifact: a mode moves only when the bulk crosses a cut
   and is blind to a distribution sliding underneath it. Re-measured as the
   `Ample+Abundant` share, **4 of 4** axes shift, by 12 to 46 points:

   | axis | Undercroft | Nadir | shift |
   | --- | --- | --- | --- |
   | hydrogen | 0.158 | 0.384 | +0.226 |
   | reduced_iron | 0.549 | 0.670 | +0.121 |
   | reduced_sulphur | **0.000** | 0.460 | **+0.460** |
   | methane | 0.272 | 0.580 | +0.308 |

   Sulphur reading exactly `0.000` at `Undercroft` independently reproduces the
   ΔT-front physics from the dominance measurement — two instruments, one fact.
3. **Plain median, on a bimodal axis.** The drift guard's first form tracked
   each axis's median and **hydrogen's is exactly `0.0000`**, because 54.6% of
   its readings are absent. A statistic pinned to zero cannot report drift: it
   sits still while the present-population moves anywhere, then jumps when the
   absent share crosses one half. Replaced by the pair *(zero share,
   median-where-present)*, degenerate on no axis.

### The drift guard is the deliberate stand-in for the missing consumer

With no niche weighting these axes, a distribution can slide arbitrarily far
with no settlement moving and no artifact drifting — the bands would quietly
start meaning something else while every ordinary instrument stayed green.
`each_axis_still_sits_where_its_cuts_were_authored` is the only thing in the
tree that objects. **Mutation-proved**: multiplying methanogenesis by 1.5 moves
median-where-present `0.5423 -> 0.8134` and reds with the intended diagnostic.

### Four gates the new surface had to satisfy, each a real requirement

`world-build-sites.tsv` (a second `artifacts` row), type-audit
(`bare-ok(ratio: ...)` on four new pub primitives), plumb (`universal(...)` on
the four cut constants — `pending(wave-N)` was available and would have been
dishonest, since these are authored now with full provenance, not deferred),
and both audit reports regenerated. Worth recording because two of the three
`make *-report` targets are drift **checks** that name the regeneration command
rather than running it — reading the target name as the fix costs a gate cycle.

**Ideonomy passes / overturns:** one — *a summary statistic can be wrong for
the question in three different ways (wrong normalisation, wrong order
statistic, degenerate on the distribution's shape), and writing the guard is
what exposes each.*

## #48 [Q] — Nathan asked whether #47's design points would bite later. Four would, and two of them were this campaign's own lessons re-committed.

Asked directly: *"do you think those design points are likely to cause issues
down the road?"* Checked rather than judged. Four found, all fixed, and the
two that matter most are failures I had already diagnosed **in writing** before
committing them again one file over.

### 1. A declared frequency that nothing read (the seventh instance)

`WhyEmpty::BelowSampleResolution { per_100k: 20.0 }` carried the measured rate
"so a reader can check the arithmetic". `grep per_100k` returned **two** hits:
the field declaration and the one construction. Nothing consumed it. That is
`PROC-prose-claims-no-assertion-checks` — the row this campaign minted, whose
instance list I extended to six in ledger #37 — authored a seventh time by me,
in the file whose entire purpose is guarding an unfalsifiable surface.

**Fixed:** `a_rarity_claim_is_checked_against_what_actually_occurs` compares the
observed count against the declared rate with Poisson slack. One-sided by
design: the falsifier for "rare" is "common", and observing zero confirms the
row rather than refuting it. **Mutation-proved** — shutting iron reduction
above a silica threshold puts 12,340 of 29,305 readings in the band against a
ceiling of 68.6, and the failure quotes the false claim back.

### 2. One tolerance for four differently-shaped axes — the same error, one level up

`ZERO_SHARE_TOLERANCE = 0.08`, shared, against authored zero shares of
`0.546 / 0.0000 / 0.0304 / 0.0000`. On hydrogen that is ~15% relative; on the
two axes that are **never** zero it is unbounded in relative terms. Concretely:
iron could acquire an 8% absent population — **8,000 per 100,000, against a
`BelowSampleResolution` row declaring 20** — with the drift guard silent. The
check protecting the claim was 400x looser than the claim.

This is precisely the argument the band cuts themselves are built on: four
differently-shaped distributions cannot share a threshold, and **a tolerance is
a threshold**. I gave the bands per-axis cuts after measuring and then handed
their guard a single shared number.

**Fixed:** per-axis tolerances, tightest on the axes claimed never-zero
(`0.0020` methane, `0.0050` iron). **Proved load-bearing rather than argued** —
a perturbation putting iron's zero share at `0.0133` sits *under* the old
shared `0.08` and *over* the new `0.0050`: the old guard passes it silently,
the new one reds. Both this and the rarity guard catch it, independently.

*A mutation that proves nothing, recorded because it nearly counted as
evidence:* the first attempt used `silica > 0.80` and produced **zero** absent
readings — silica never exceeds 0.80 in these worlds, so the mutation could not
have moved and its green said nothing. Bisecting to 0.74 found the
discriminating case.

### 3. Positional indices on a four-element array

`metabolite_bands()` returned `[MetaboliteBand; 4]` and `bands[2]` is correct
for sulphur exactly until someone reorders the axes. **Fixed:** named
accessors (`methane_band()` and three siblings) are now the primary API and the
array is built from them, so there is one axis-to-cuts pairing in the codebase
rather than one per call site.

### 4. A mismatch the type system permits

`metabolite_band(chem.methane, HYDROGEN_CUTS)` type-checks and is wrong — two
`f64`-shaped arguments with no relation the compiler can see. The named
accessors remove the need to pair them by hand; the free function stays public
for authors reasoning about a threshold directly.

### And one thing that LOOKED like a defect and is not

`MetaboliteBand` derives `Ord`, so `methane_band() > hydrogen_band()` compiles
across axes with different cuts. First read: a frame-loss bug. Second read: it
is meaningful, and the doc now says exactly what it means — **relative standing
on each axis's own ladder** ("this place suits a methane specialist better than
a hydrogen specialist"), which is a real question a generalist asks. What it is
**not** is a magnitude comparison, since `Ample` begins at `0.70` on hydrogen
and `0.55` on methane. Documented with the correct instrument named for the
other question (`dominant_source`).

**The pattern worth carrying:** three of the four defects are *the same
defect* — a threshold, an index, or a number that means something only in a
context the code does not carry. And I wrote the first two immediately after
writing the paragraphs explaining why they were wrong.

**Ideonomy passes / overturns:** one — *diagnosing a defect class in prose
confers no immunity to committing it; the campaign that logs the instance list
is as likely to author the next instance as anyone.*

## #49 [G5] — Task 17 shipped, and three things about GUARDS rather than about chemistry

The chamber prose now carries the margin: a contested chamber reads *"drawn
to the porous, water-logged carbonate, though a sulphide-laced seam runs it
close"*. 28.0% of 29,305 readings are contested at `CONTESTED_SHARE = 0.80`,
with both outcomes asserted to occur — a word that applied always or never
would carry no information, which is the occupancy argument from #47 applied
to a narrative distinction instead of a numeric one.

`dominant_source` delegates to `source_standing` now, so the leader has one
derivation rather than two that must agree.

### 1. The equivalence check was large and missed the risky path

The delegation is asserted against an **independent re-derivation** of the
expression it replaced — not against itself — over 29,305 readings, because
committed prose depends on the answer. It reported **0 exact ties**.

So the tie-break, the one behaviour the rewrite most endangered, was exercised
by **none** of those 29,305 readings. A large sample is not a covering one, and
the number that made this visible was one the test prints rather than one it
asserts. Covered now by a constructed case: `moisture = 0, drainage = 0` zeroes
all seven sources, all seven tie, and the documented rule (the LAST source
wins) is asserted directly.

### 2. THE PROSE HAS NO COMMITTED WITNESS, and the seam registration was inert

Checked across every path in `docs/generated-paths.txt`: **no committed
artifact contains a chamber-resident line at all.** `possession-seed-42.md` has
zero; every vessel fixture has zero. The walk those artifacts record never
enters a chamber with a resident. So the rendering-drift check that normally
catches a prose change is structurally blind here, and `source_phrase` could
return one constant for all seven sources with every gate green.

Registered as a seam — and **the registration did not work**. It reported
*"(no call sites found)"*, because every call sat inside a `format!` and `syn`
does not descend into macro token streams. A registration the tool cannot probe
is worse than none: the committed roster reads healthy while nothing is
checked, which is exactly the hazard the root `CLAUDE.md` describes, reached
from a direction it does not name (not "nobody ran it" but "the scanner cannot
see it"). Fixed by hoisting the calls into `let` bindings, with a comment
saying why they must stay hoisted.

**Verdict, run rather than predicted: `GUARDED` at both call sites, 2 ok, 0
needing action.**

### 3. A killed seam-guard leaves the mutation in the tree

The first attempt was wrapped in `timeout 900` and the kill left
`let leader_phrase = ("the rock");` in the working tree, unrestored. It
surfaced only because the next run refused on a dirty tree and I read the diff
instead of assuming the dirt was mine — a `git add -A` would have committed a
neutralised seam whose own guard is the test it defeats.

The tool's refusal message already prescribes the fix
(`git checkout -- <file>`) and it is load-bearing, not boilerplate. **Do not
wrap `seam-guard run` in a timeout**: two call sites at a full scoped vessel
test run each exceed 15 minutes, and the cost of the kill is a silently
mutated tree.

### The absorb, folded in here because its result is a measurement

118 commits, 13 merges, including The Coherence's 2,700-line `facet.rs`. Two
conflicts, both generated audit reports, resolved by **regeneration** — a
generated artifact's correct content is whatever its generator produces from
the tree that lands, and hand-resolving invents a third version belonging to
neither side.

**The absorb is chemically neutral, and the drift guard is what says so.**
Every axis reports byte-identical to its authored shape. The Coherence touches
the renderer's ground patches, not the material buffer feeding chemosynthesis.
That is #47's guard answering *positively* on its first real encounter rather
than by silence — the difference between "nothing objected" and "the instrument
looked and reported no movement".

**Ideonomy passes / overturns:** one — *a guard can be defeated by the syntax
its scanner cannot parse, not only by the case nobody wrote; check that a
newly registered guard can SEE its subject before trusting its green.*

## #50 [Q] — The absorb's 11 new failures are a COLLISION, not inherited debt. Measured, because the three possibilities need different responses.

The 118-commit absorb took the workspace from 53 failures to 64. The eleven
new ones had three possible owners and each implies a different response, so
guessing was not available:

1. **main landed red** — inherited debt, not mine to repair, and worth telling
   main's operator about.
2. **my branch was already red** — my debt, and the pre-merge baseline lied.
3. **the merge product only** — a semantic collision between two campaigns,
   mine to resolve because I am the one absorbing.

**Measured.** A detached worktree at `origin/main`, cold-built, running exactly
those eleven: **11 passed, 0 failed.** They also passed on this branch at
`b97dbf3a8` before the merge. So it is (3), and there is no ambiguity left.

This is the failure mode the root `CLAUDE.md` says has actually bitten — *"No
gate has an opinion about whether two campaigns changed the same idea in
incompatible ways"* — arriving on schedule. Both sides were green; the product
is not.

### The four clusters, and what each one is

- **Spring (4 tests).** Task 13 widened carbonate and porosity, raising the
  `Hydro::Spring` share; main's coherent-terrain work changed which ground the
  walk band crosses. Neither alone moved it — together the band now sees spring
  **65 times over 4,440 facets** where The Warp measured an exact zero.

  **The Warp built for this moment and said so.** Its doc records that the zero
  was *"a property of the SAMPLE, not of the world"* — spring occurs on ~1.3%
  of locations, so 78 draws miss it entirely about a third of the time
  (`(1 - 0.01337)^78 = 0.350`) — and it set spring's two floors to `0.0, 0`
  **specifically so that deleting the witness could not leave a live bound
  vacuously satisfied**, with the instruction to re-derive both in the same
  edit. That is a guard written by someone who expected to be wrong later and
  made the later reader's job possible. It is the best-designed thing this
  campaign has collided with.

- **World-moved baselines (5).** Byte goldens, founder drops, person facts,
  seed 42 resolving 40,596 vertices against a pinned 40,585. One is explicitly
  **not** a rebaseline: `the_client_fixtures_are_current` says *"the
  vessel/session/v2 wire shape moved. Decide whether that is an epoch BEFORE
  rebaselining."* That is a ruling.

- **The Coherence's own `facet_scene` proof fixture (1)**, moved by this
  branch's world.

- **`survivorship_probe` (1)** — see below; a finding, not a repair.

### The survivorship probe, and a correction that does NOT get vindicated

`the_separation_survives_conditioning_on_tenure` now reads **stratified
z = 0.435 against pooled 5.366**. This is the exact test of ledger **#27**,
where I corrected myself for asserting three times that the §5.2 claim had
stopped holding — I had restated a dead measurement without re-running it, and
running it showed it passed.

It fails now. Both facts stand and neither cancels the other: **the correction
was right when made, and the claim has since genuinely stopped holding.** The
tempting reading is that #27 was unnecessary and my original instinct was
sound. That reading is wrong and worth naming, because it is the comfortable
one: what #27 criticised was not the conclusion but **carrying a number I had
not re-derived**. A stale measurement that later comes true was still stale
when I used it. This is a NEW falsification, with a date, a cause (the absorb)
and a fresh statistic — not a retroactive licence for the old one.

**Ideonomy passes / overturns:** one — *a stale claim that later turns out true
does not retroactively become evidence; provenance is a property of when it was
asserted, not of whether it eventually held.*

## #51 [G5] — The repair round: 64 → 54, and the count closes to the unit

Ten of the eleven collision failures repaired; the eleventh left failing on
purpose. **54 remain, and that number is exactly right rather than
approximately right:**

```
53  pre-merge baseline (measured, ledger #43)
+1  survivorship_probe -- falsified by the absorb, deliberately unrepaired
----
54
```

`survivorship_probe` does **not** appear in the pre-merge 53, so the arithmetic
closes to the unit with nothing unexplained. The implementer flagged the 54 as
"one more than your 53, worth a quick look"; it is not a residue, it is the
Cluster D finding counted once.

### Cluster A — The Warp's instruction followed, and its CONTROL ran down

`SILENT_IN_THE_WALK_BAND` and both special-case blocks removed, spring's two
zero floors re-derived in the same edit exactly as The Warp's doc required:

| bound | silent era | re-derived | measured | headroom |
| --- | --- | --- | --- | --- |
| `max_allowed_delta` | 0.010 | **0.02** | 0.01070 | 1.87x |
| `min_pooled_spread` | 0.0 | **0.45** | 0.95000 | 2.11x |
| `min_total_occurs` | 0 | **30** | 65 | 2.17x |

Real margin on each, none fitted to the reading. The walk pool itself moved
(74 walks / 4,440 facets, from 78 / 4,680) — the land mask shifted, which
affects every kind.

**The control golden moved too, and that was the thing worth chasing.**
`thicket` and `erratic` are The Warp's non-regression controls; a moved control
is a different and more serious claim than a moved subject. Diffed **by vertex
id** rather than line position, since the land set itself moved (240 out, 221
in of 11,218):

- **`erratic`: changed at 0 of 10,978 common facets.** Its `macro_state` is a
  bare constant with no pack dependency — the strongest available evidence that
  the geometry and noise math are untouched.
- **`thicket`: changed at 8,215 of 10,978 (75%)** — it reads
  `pack.temperature`/`pack.moisture`, climate fields downstream of elevation,
  which is exactly what main's coherent-terrain rework moves.
- `git log` over `windows/worldgen/src/weft/` across the merge: **zero files
  touched.**

So: a world move, not a recipe move. The 75% figure looked alarming and
resolved cleanly, which is the point of checking rather than regenerating.

### Cluster B — three pins re-measured, shapes intact

`founder_collision` `(1439, 0) -> (1439, 1)`; `kinship_facts` baseline
`1,487 -> 1,635` facts over 242 persons (was 227), regenerated by the file's
own documented mechanical procedure; `portolan_resolution`
`40,585 -> 40,596` of 40,962. Every one keeps its `assert_eq!` shape — no pin
became a range or an inequality — and each carries a **dated** provenance note
saying what moved it, appended beside the previous entry rather than
overwriting it.

### Cluster C — a generator's own output, regenerated by the generator

`surface-seed-42-proof.json` is written by `live_proof()` in the test itself.
Regenerated by calling it, never hand-edited; confirmed no field added or
removed at any level, only values.

### Cluster D — untouched, and the reason restated

`survivorship_probe::the_separation_survives_conditioning_on_tenure` is
unmodified (`git status --porcelain` clean on that path) and still fails at
stratified z = 0.435 against pooled 5.366. A preregistered claim the absorb
falsified is a finding. Repairing it would be retuning to rescue a prediction,
which decision 0016 forbids, and this project ships nulls as headlines.

**Ideonomy passes / overturns:** one — *when a control moves alongside its
subject, diff by identity rather than position: the population itself may have
shifted, and a line-ordered diff then reports a recipe change that did not
happen.*
