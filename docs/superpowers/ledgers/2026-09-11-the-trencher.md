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
