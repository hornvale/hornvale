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

## #22 [R] — Task 3 executed: the four metabolite resource axes

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
