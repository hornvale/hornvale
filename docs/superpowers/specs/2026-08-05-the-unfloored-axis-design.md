# The unfloored axis — a stage-6 amendment to The Tilth

**Not its own campaign.** This is **The Tilth, stage 6**, on
`campaign/the-tilth`, amending
[`2026-08-04-the-tilth-design.md`](2026-08-04-the-tilth-design.md).

It was drafted as a separate campaign ("The Sovereign", off `main`) on the
belief that the defect predated The Tilth. **It does not — The Tilth causes it.**
On `main` the four condition responses combine by *product*:

```rust
saturated * temp.eval(..) * moisture.eval(..) * insolation.eval(..) * elevation.eval(.., 0.0)
```

Elevation is unfloored there too, but under a product every axis always
contributes, so an unfloored term suppresses *magnitude* without erasing the
others' spatial variation — "binding axis" is not even a defined notion without a
`min`. §3.3 of The Tilth replaced that product with Liebig's minimum, and it is
the minimum that converts one unfloored axis from a scale factor into a veto.

The error is recorded rather than quietly fixed because it is the campaign's own
lesson recurring: a defect measured in a *working tree* was attributed to the
*branch point*. The pre-commit hook caught it, on an unresolved import.

Fixing it here — in the campaign that introduced Liebig, on the branch that is
already red and awaiting one golden rebaseline — keeps cause and repair together
and pays for a single re-measurement instead of two.

## 1. The thesis

`sovereignty_floor` exists — idea-registry `BIO-26`, status **shipped** — so that
tolerance is a **soft preference** rather than a hard constraint: a species with
mass and potency is never fully excluded, only denser at its optimum. Three of
the four condition axes honour that. Elevation does not — `tolerance_liebig`
passes it a literal `0.0`.

Under Liebig's law of the minimum, one unfloored axis does not merely add a
constraint. It **out-votes every floored one**, because it is the only term that
can fall below the floor at all. The result is not a species that dislikes
altitude; it is a species whose entire climatic niche has been switched off.

### 1a. Constant provenance

§3.2 needs the rule that an **Earth-contingent constant may not be calibrated
against Hornvale's own census** — internal measurement can validate an internal
choice but not an external fact, so that pairing is circular by construction.
That is **decision 0106**, ratified on this branch and therefore available here.

It is worth noting what the abandoned separate-campaign framing would have cost:
0106 does not exist on `main`, so a campaign branched there would have had to
restate the rule and would then have minted a colliding `0106` of its own, since
both branches see `0103` as the log's tip. Mint decision numbers against
`git log origin/main -- docs/decisions/`, never the working tree.

## 2. What is wrong, measured

`windows/worldgen/tests/niche_breadth_probe.rs`, seed 42, 11,066 land cells.

### 2.1 Half the settling roster has no climatic preference at all

Which of the four condition responses is the binding Liebig minimum:

```
species         temp%     moist%     insol%      ELEV%
kobold          31.2%       1.4%      23.6%      43.7%
goblin           0.0%       0.0%       0.0%     100.0%
hobgoblin       23.7%       1.6%       0.0%      74.8%
bugbear         12.0%       9.4%       5.7%      72.9%
gnoll            0.0%       0.0%       0.0%     100.0%
human            0.0%       0.0%       0.0%     100.0%
```

For goblin, gnoll and human, elevation binds on **every cell of the world**.
Their authored temperature, moisture and insolation curves are inert — goblin's
18 °C optimum, its 0.50 moisture optimum and its 0.13 insolation optimum never
determine anything anywhere. Their whole `ConditionNiche` evaluates to a single
authored scalar.

### 2.2 The mechanism: `devotion` is a peak, not a sharpness

```rust
eval = floor + (1 - floor) · devotion · exp(-z²/2)
```

At the optimum `bump = 1`, so `eval = floor + (1-floor)·devotion`. **`devotion`
is the highest suitability the axis can ever reach**; breadth is `width`.

Indifference has been authored throughout as *low devotion*:

```rust
// goblin — "wide/indifferent, centred on the settleable-land median (p49)"
elevation: ConditionResponse { optimum: 1500.0, width: 3000.0, devotion: 0.35 }
```

`width` 3000 against a 0–4,074 m land range makes `bump ≈ 1` everywhere, so with
`floor = 0.0` the term is `≈ 0.35` — a constant. Goblin's three floored axes sit
at or above its floor of 0.335. The minimum is therefore elevation, always, by
construction and not by ecology.

The authoring is not careless; the *comments say what was meant*. "Wide/
indifferent" is the right intent and low devotion is a plausible-looking way to
write it. The field name is what misleads: `devotion` reads as "how much this
species cares", and the formula means "the best this species ever gets".

### 2.3 It is not a niche-breadth problem

The natural suspicion — that a mis-sited species is too narrow to live anywhere
— is false, and worth recording because it cost an earlier session a wrong
hypothesis. Fraction of land at each viability bar (`K > 5` seats a
genesis community, `K > 14.3` lets it spread):

```
species      K>0      K>5   K>14.3      p50      p90
kobold     99.5%    59.1%    42.9%     9.31    47.90
goblin     99.5%    72.8%    53.3%    16.82    38.17
```

Kobold — the roster's one deliberate specialist — can survive on 59% of land.
Nothing here is too narrow. The defect suppresses *magnitude*, not extent.

### 2.4 What the repair recovers

Buffering elevation by the same `sovereignty_floor` the other three axes use,
changing no authored number:

```
-- binding axis, elevation buffered --
species         temp%     moist%     insol%      ELEV%
kobold          39.5%       2.5%      29.7%      28.2%
goblin          56.1%       7.1%      18.0%      18.8%
hobgoblin       67.6%      10.0%       0.0%      22.5%
bugbear         50.3%      24.9%      11.1%      13.7%
gnoll           76.4%      22.4%       1.1%       0.0%
human           87.7%      12.3%       0.0%       0.0%
```

Temperature becomes the dominant constraint, moisture and insolation begin to
matter, and elevation falls to a minority voice — except for kobold, which stays
elevation-bound 28% of the time, which is exactly right for a highland
specialist. Climate discriminates again.

## 3. Design

### 3.1 Floor every axis

`tolerance_liebig` passes `floor_buf` to all four responses. One line. The
`ConditionNiche` doc gains the invariant: **no axis may be evaluated unfloored**,
with §2.1's table as the reason, because the next author's instinct will be to
special-case "geometry, not physiology" exactly as this one did.

### 3.2 Re-derive `CAPACITY_V_MAX`

Removing the suppression raises capacity 1.6–2.8× (goblin p50 16.82 → 26.58,
kobold 9.31 → 25.71; good ground ≈ 1.66×). `CAPACITY_V_MAX = 140.2` was solved
so that today's good ground keeps roughly today's headcount, so it is now stale
by construction. Re-derive through `windows/worldgen/tests/tilth_probe.rs`,
printing the arithmetic longhand.

The rule of §1a binds this: the target is the *pre-campaign good-ground level* —
a **gauge** choice, whose absolute value is a Hornvale convention rather than an
Earth fact, and which may therefore legitimately be measured internally. It may
not be fitted to rescue any prediction (decision 0106, §1a). `CAPACITY_K_M`
is the median `axis_supply` over land and is untouched by this change — supply
is not a condition term — but the probe re-measures it and the spec expects it
to come back unchanged. **If it moves, something else moved too, and that is a
finding, not a nuisance.**

### 3.3 Rename `devotion`

`ConditionResponse.devotion` → `peak`, with the doc stating that it is the
maximum attainable suitability and that breadth is `width`. A pure rename across
the roster and kernel; no arithmetic changes.

This is the guard rail, and it is the cheap half of the fix. §2.2's error was
made six times by three different campaigns, every time with a correct comment
beside it. In the idiom of decision 0103 — which introduced `SuitabilityMap` and
`CapacityMap` precisely because two things that were both `CellMap<f64>` got
silently interchanged — the name is where the lesson goes.

### 3.4 Ship the instrument

`niche_breadth_probe.rs` lands as a standing `#[ignore]`d probe. It prints the
binding-axis table under **both** floor conventions, deliberately: its first
version hardcoded `0.0` and so reported an unchanged table while the library
under test had already been changed. A probe that cannot see the variable it is
varying is worse than none.

### 3.5 Out of scope, explicitly

**Re-authoring the roster's `devotion` values is deferred.** Once §3.1 lands, low
devotion merely lowers a ceiling instead of vetoing three axes, so it is a lore
and scale refinement rather than a defect. It is ~30 authored numbers across six
species; landing it in the same commit as a physics change would confound the
measurement of that change — the mistake decision-ledger #3 of The Tilth exists
to avoid. Follow-up, against the buffered world as baseline.

## 4. Preregistration (decision 0016)

Frozen before implementation. Instrument: `niche_breadth_probe.rs`.

**H1 — climate discriminates.** After §3.1, no settling species has any single
axis binding on more than **90%** of land. Today three of six bind at 100%.

**H2 — the specialist survives the repair.** Kobold's elevation axis still binds
on **≥ 20%** of land. The repair must not flatten a deliberate specialist into a
generalist; if it does, sovereignty is too strong a buffer and the finding is
about `SOVEREIGNTY_FLOOR_MAX`, not about elevation.

**H3 — the gauge holds.** After §3.2's re-derivation, median capacity on good
ground returns to within **±10%** of its pre-campaign value (68.87). This is what
makes §3.2 a re-derivation rather than a rescale.

**H4 — the null.** If buffering elevation leaves the binding-axis table
substantially unchanged, then the suppression was not the cause of the flat
niches and the defect is in the authored data after all — which reverses §3.5
and makes the re-authoring the campaign.

## 5. Blast radius

Every world moves; this is a genesis epoch. Expect the same downstream families
The Tilth's parked rewire moved — settlement placement → culture → language →
book prose → scene and vessel goldens.

**Ordering within the branch matters.** The per-species bake rewire (`64db5432`,
red) sits below this stage and must be **re-measured after it**, before any of
its 22 drifted goldens are accepted. That rewire's measured 25–55% world
contraction was taken against the suppressed field; with capacity 1.6–2.8×
higher, some or all of it may simply not exist. The same applies to
`history_tithe::extraction_does_not_depopulate_the_map` (281 occupations against
a floor of 400) — **do not rule on that floor until this stage has landed**, and
recall that the floor is a seed-42 tripwire whose natural spread across the five
probe seeds is 31–2037.

The two changes stay in **separate commits** on this branch so a bisect can still
tell "siting changed" from "the field changed". One rebaseline serves both.

## 6. Risks

- **Nothing can be excluded any more.** With every axis floored, a species is
  never barred from anywhere by conditions, only made sparse. That is BIO-26's
  explicit thesis, but it removes the last hard climatic gate, leaving only the
  era mask and resource supply. H2 is the check that this has not gone too far.
- **`SOVEREIGNTY_FLOOR_MAX = 0.95` is now doing more work.** It was authored as a
  biological prior when it applied to three axes; it applies to four now.
  Untouched by this stage, but its influence grew and that is worth stating.
- **The rename touches every species file** and will conflict with any parallel
  campaign editing the roster. Cheap to redo, annoying to merge; do it in one
  focused pass.

## 7. Open questions

1. Should `width` be validated against the axis's real range? Goblin's elevation
   width of 3000 m makes its curve flat over the whole world, which is what
   "indifferent" wants — but nothing distinguishes deliberate flatness from a
   width authored in the wrong units. A `width > range` check could not tell
   them apart either, so this may need to stay a review question.
2. Does `insolation` want a floor at all? It is the axis nearest to geometry —
   the original argument for leaving elevation unfloored — and after this change
   it binds 0.0% for four of six species. Possibly it is now inert for the
   opposite reason.
3. Is elevation a *condition* axis at all, or a proxy for temperature and
   oxygen, which are already modelled? Removing it entirely is a larger and more
   honest change than buffering it. Out of scope here; recorded because §2 makes
   it askable for the first time.

---

## 8. Stage 7 — temperature is not buffered (added 2026-08-05, after stage 6.1)

### 8.1 Why this is a separate stage

Stage 6.1 floored elevation and the follow-up measurement found that
**cold-indifference predates this campaign entirely**: temperature, moisture and
insolation were already floored, so a floored *temperature* axis — BIO-26 as
shipped — is what has always stopped cold from excluding anyone. Measured as the
share of land below the bake's own −10 °C snowline that a species still calls
survivable (`K > 5`):

```
species     before stage 6.1    after
goblin            48.4%         56.1%
human             47.6%         62.3%
bugbear           15.7%         39.6%
```

Goblin already thought half the sub-freezing world was fine. Stage 6.1 amplified
that; it did not cause it.

So this stage changes **ratified, pre-existing behaviour**, which stage 6 did not,
and it gets its own preregistration rather than riding stage 6's.

### 8.2 The change

`tolerance_liebig` passes `0.0` for **temperature** and keeps `floor_buf` on
moisture, insolation and elevation.

The asymmetry is the argument, and it is the opposite of stage 6's. Elevation is
a *geometric proxy* — it has no lethal value, and a species excluded by altitude
alone is excluded by a stand-in for temperature and oxygen that are modelled
elsewhere. Temperature is the one axis with a genuine physiological limit.
Sovereignty as homeostatic buffering is exactly the right model for "prefers
warmth, tolerates less" and exactly the wrong one for −50 °C.

Consequence, accepted deliberately: cold becomes able to **exclude**, so the
capacity field and the bake's era mask stop contradicting each other over roughly
half the planet. That is the point. It also makes a snowball world lethal on the
surface rather than merely unpopulated, which is what the deferred subterranean
ecology rung ([[MAP-10]], [[MAP-69]]) exists to survive — the deep's energy base
is geothermal and does not care about the snowline.

### 8.3 Preregistration (decision 0016)

**H5 — cold excludes.** After the change, no species calls more than **10%** of
sub-snowline land survivable. Today the range is 39.6–64.0%.

**H6 — temperature does not become the new veto.** No species has temperature
binding on more than **90%** of land. This is stage 6's own pathology mirrored:
an unfloored axis under a minimum out-votes floored ones, and the only reason to
accept it here is that temperature is the axis that *should* bind when it is
lethal. If it binds everywhere regardless, the other three are inert again and
the repair has merely moved the defect.

**H7 — the null, and the likeliest failure.** If unflooring temperature leaves
any settling species viable (`K > 5`) on less than **5%** of land, the roster is
authored for a warmer world than the climate model generates — median land on
both probe seeds is about −10 °C — and the finding is about the authored optima
or the climate model, **not** about the floor. Report it and revert; do not
re-author optima to rescue the stage.

### 8.4 Ordering

Stage 7 lands **before** §3.2's `CAPACITY_V_MAX` re-derivation, not after.
Unflooring temperature lowers tolerance and therefore capacity, so re-deriving
the gauge first would immediately stale it again. One derivation, at the end.
