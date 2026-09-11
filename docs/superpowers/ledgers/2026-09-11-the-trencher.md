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
