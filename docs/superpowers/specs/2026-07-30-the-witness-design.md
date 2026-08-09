# The Witness — design

> **STATUS: COMPLETE — all repairs and guards shipped; documentation half
> landed.** Chronicle: `book/src/chronicle/the-witness.md`. Retrospective:
> `docs/retrospectives/the-witness.md` (follow-up table F5/F7/F13 discharged;
> F14–F19 open). §3.1's original porosity-formula repair was superseded by
> measurement before it shipped (see the plan's Task 4/5/5b); the shipped
> repair moves a threshold, not the formula, and adds a grain term for range
> rather than to cross a gate. §5 flagged item ① (two epoch bumps) was
> reversed after G3 to one bump, `language/<species>/lexicon/cascade/v2` —
> see the plan's "The epoch this campaign owes" section. H1 (§6) is
> **supported**, measured on one merged tree rather than the cross-tree
> comparison an earlier draft of the readout used (retrospective, "what
> worked"). §9's decision-record instruction is superseded: decision 0094,
> ratified by a different campaign one day after this one reached the same
> principle independently, is cited rather than duplicated.

**Campaign:** The Witness · **Date:** 2026-07-30 · **Branch:** `the-witness`
**Discharges:** The Wearing's follow-ups **F5**, **F7**, **F13**
(`docs/retrospectives/the-wearing.md`)

---

## 1. The premise

Three open follow-ups, one defect shape. In each, a check passed for years
while the thing it checked was structurally impossible, because **the check's
input was authored rather than derived**:

| | The defect | What masked it |
|---|---|---|
| **F5** | `Hydro::Spring` and `Hydro::Aquifer` are unreachable on every seed | a unit test hand-builds a `MaterialBuffer` the real derivation cannot produce |
| **F7** | a *leading* `Tonogenesis` rule is provably the identity | the test fixture puts the merger first, so the pending-conditioning is never absent |
| **F13** | the lab's independent duplicate of worldgen's `Steeped` rules has gone stale three campaigns running | nothing tells a campaign the duplicate exists |

The campaign fixes all three and then builds the guard the shape implies:
**derive the checklist from the type, leave the predicate authored.**

The name is the repo's own vocabulary. `windows/worldgen/tests/exposure.rs`
already calls a seed at which a concept was observed to steep a *witness*, and
its doc comment names the exact gap that let F5 ship — a check "blind to
whether the `Steeped` rule that list-membership claims actually fires in any
world."

---

## 2. What is verified, and how

The follow-up register's own closing lesson is that **a follow-up entry is a
claim, and it decays like any other** — two of the five entries closed in the
post-close pass turned out to be wrong about themselves. Every load-bearing
claim below was therefore re-derived, not inherited.

### 2.1 F5 is real, and the evidence is already committed

`aquifer-fraction` is column 34 of
`book/src/laboratory/generated/the-census/rows.csv`. Across all 1000 seeds it
takes **exactly one distinct value, `0`**. The column beside it,
`karst-fraction`, takes **1000 distinct values**. A dead census column, on the
committed artifact, at zero cost to check.

The cause is arithmetic, not branch order (`domains/terrain/src/lithology.rs`):

```
porosity = clamp(0.5·carbonate + 0.3·(1 − metamorphic_grade))     # line 445
```

`hydrogeology` reaches `Spring`/`Aquifer` only at `porosity > 0.5`, and `Karst`
pre-empts at `carbonate > 0.5 ∧ porosity > 0.4`. The second porosity term caps
at `0.3`, so `porosity > 0.5` *forces* `carbonate > 0.4`. The surviving window
is `carbonate ∈ (0.4, 0.5]` with `metamorphic_grade ≈ 0` — non-empty in the
abstract, empty in a thousand worlds.

Stated physically: **the model cannot represent a porous non-carbonate rock.**
Sandstone — the archetypal aquifer — is unreachable by construction, because
porosity is derived from carbonate alone, with no term for grain packing and no
penalty for cementation.

Worldgen already knows and works around it. `is_spring_cell`
(`windows/worldgen/src/lib.rs:3894`) reads `Hydro::Karst` as a *proxy* for the
unreachable `Hydro::Spring`, and discloses the cost in its own comment: at seed
42, 132 of the 137 qualifying Karst cells are also `WaterKind::River`, so
`spring` is "river partitioned by rock type, not an independent signal."

### 2.2 F7 is real, and provable from the source

`evolve` (`domains/language/src/etymology.rs:765`) opens with
`let mut pending: Option<ToneConditioning> = None;`. `apply_tonogenesis`
returns `(segs.to_vec(), false)` on `None`. Only a *fired* `ClusterSimplify` or
`FinalLoss` ever sets `pending`. Therefore **a `Tonogenesis` drawn at position 0
of any cascade is the identity, always, on every word, in every language** —
and more generally, so is one drawn before any merger.

The severity differs by regime. `CascadeRegime::SETTLED` is 2–4 rules, so a
leading tonogenesis wastes a slot. `CascadeRegime::WEAR` is **1–2** rules, so a
leading tonogenesis can be *the entire cascade*: with kinds drawn uniformly from
six, that is a 1-in-6 chance of a wasted lead and roughly 1-in-12 of a wholly
inert wear. The register measured 8 of 20 production cascades containing one, 7
leading, **3 inert entirely**.

This is not a cosmetic waste. `book/src/frontier/idea-registry.md` states
LANG-11's blocked lever outright: *"the lever is the cascade's **match rate**,
NOT the survival guard — measured innocent."* A one-in-six chance of spending
the only rule in a wear cascade on a provable identity is match rate discarded.

> **Task 1 re-derives the 8/20/7/3 counts and the 14/650 funnel on this tree
> before any repair lands.** They are the register's numbers, not this spec's.

### 2.3 F13 is real, and is mechanically coupled to F5

`exposure-sound-{goblin,kobold}` read false on **767 / 759** of 1000 worlds —
true only where a species is unplaced. Worldgen is correct (`exposure.rs` is
19/19); the lab's `independently_steeped_concepts` never learned The Watershed's
six staples. (These counts are the register's; Task 1 re-derives them from the
committed census alongside F7's, for the same reason.)

The coupling is verified from source, not inferred: `lab_is_spring_cell`
(`windows/lab/src/metrics.rs:4747`) restates *the Karst proxy itself* —
`hydro_at(cell) == Hydro::Karst && drainage_at(cell) >= RIVER_MIN_DRAINAGE`.

The coupling that forces F13 into this campaign: `spring` is one of the seven
rows in the lab's restated gate table, and the module note
(`windows/lab/src/metrics.rs:4665`) states the asymmetry precisely — an
over-inclusive lab set is silent, an **under-inclusive one is loud**: "if
worldgen ever RELAXES a gate … it will mint a `Root` for a concept this reading
does not steep and the invariant goes red."

**F5 relaxes that gate.** Making `Hydro::Spring` reachable widens `is_spring_cell`
beyond the Karst proxy. So F5 reddens `exposure_sound` unless F13 lands first.
Shipping F5 without F13 would not merely defer F13 — it would *author its fourth
recurrence*.

---

## 3. The repairs

### 3.1 F5 — the threshold is on the wrong rock's scale

> **SUPERSEDED 2026-07-30, after measurement.** The subsection below this box
> proposed adding a granular term to `porosity`. That repair is **abandoned**:
> it cannot work, and it was aimed at the wrong defect. Both the diagnosis
> in §2.1 and the repair here were reasoned rather than measured, and the
> measurement overturned them. Kept in place, not deleted, because the
> campaign's subject is precisely claims that were never checked.
>
> **What the measurement showed** (8 seeds, continental land cells):
>
> ```
> non-carbonate : n=4666  min=0.0250  p50=0.1000  p75=0.2500  p95=0.3250  max=0.3250
> carbonate     : n=1095  min=0.3500  p50=0.4250  p75=0.5750  max=0.6500
> ```
>
> 1. **Clastic porosity maxes at exactly 0.325.** The `porosity > 0.5` gate is
>    not marginally out of reach — it is **54% above the entire clastic range**.
>    No coefficient on a grain term closes that: the analytic ceiling is
>    `0.325 + 0.423·k_g`, so reaching 0.5 needs `k_g ≥ 0.414`, which would make
>    the added term the model's second-largest.
> 2. **The two classes do not overlap.** `[0.025, 0.325]` and `[0.35, 0.65]`.
>    Porosity *perfectly separates* carbonate from clastic, so it is not an
>    independent axis at all — it is carbonate plus a small within-class spread.
>    `Karst`'s `porosity > 0.4` is therefore nearly redundant with its own
>    `carbonate > 0.5`.
> 3. **`0.5` sits between the carbonate class's p50 and p75** — that is, *inside
>    the Karst region*. Every cell it selects satisfies `carbonate > 0.5 &&
>    porosity > 0.4` and is caught by the branch above. **The threshold lives
>    entirely inside the branch that pre-empts it.** That is the whole defect.
>
> **§2.1's framing was also wrong.** It said the surviving window
> `carbonate ∈ (0.4, 0.5]` was "non-empty in the abstract, empty in a thousand
> worlds." `carbonate_at` returns **only `0.7` or `0.05`** — it is binary. The
> window is empty *by construction*; `Spring`/`Aquifer` are **analytically**
> unreachable, not merely empirically.

**The repair.** `0.5` is a **carbonate-scale threshold applied to clastic
rock**. A sandstone at 0.325 is at the top of its class; a limestone at 0.325
does not exist. The fix is per-family thresholds, each named for the rock family
it is calibrated against, and it changes **no formula**:

```rust
if carbonate > 0.5 && porosity > KARST_MIN_POROSITY   { Karst }     // 0.4, carbonate scale — unchanged
if porosity < AQUITARD_MAX_POROSITY                   { Aquitard }  // 0.15 — unchanged, and reachable
if porosity > CLASTIC_AQUIFER_MIN_POROSITY            { Spring/Aquifer } // ~0.30, CLASTIC scale — was 0.5
```

`CLASTIC_AQUIFER_MIN_POROSITY` is pinned to the measured p75/p95 boundary of the
clastic distribution, not to a round number, and its doc comment carries the
distribution it was set against — which is the documentation that stops this
recurring.

**Blast radius collapses against the superseded plan.** `MaterialBuffer` is
untouched, so `cave_proneness` does not drift, `classify_rock`'s
Sandstone/Shale gate does not move, and the four other `MaterialBuffer`
consumers are unaffected. Only one branch of `hydrogeology` moves. It also
removes the collision risk with `the-pigment` flagged in §5 ③, which was
premised on this campaign changing a `MaterialBuffer` field derivation.

**`Aquitard` was checked and is fine.** Clastic p50 is 0.10, so roughly half of
clastic cells sit under 0.15. A carbonate cell in `[0.35, 0.40]` falls past
Karst and lands on `Runoff`. No second dead branch here — and Task 6's witness
guard proves all five variants rather than taking that on trust.

Once `Spring` is reachable, `is_spring_cell` drops the Karst proxy and reads
`Hydro::Spring`, and the `spring ⊆ river` disclosure is retired.

### 3.1-superseded — give porosity the two terms it lacks

Add a granular contribution and a cementation penalty, so a porous non-carbonate
rock can exist:

```
porosity = clamp( 0.5·carbonate                      # dissolution (unchanged)
                + k_g·grain·(1 − induration)         # NEW: packing, less cement
                + 0.3·(1 − metamorphic_grade) )      # recrystallisation (unchanged)
```

`k_g` is **calibrated by sweep, not chosen in this spec.** The plan's first
terrain task sweeps candidate values and reports **four** metrics, not one:
`aquifer-fraction`, `karst-fraction`, the `rock_class` split (porosity also
gates `Sandstone`/`Shale` at `lithology.rs:222`), and `cave_proneness`. The
acceptance condition is that `Spring` and `Aquifer` become witnessed on a real
seed sweep **while `karst-fraction` stays within its present distribution** —
the campaign is repairing a dead branch, not re-tuning karst.

`domains/terrain/CLAUDE.md` is explicit that terrain's calibration constants
"were chosen from data sweeps against worst-case seeds (decision 0057)" and that
changing one "is a retune, not a cleanup." That is why the fix adds a *term*
rather than nudging the `0.5` gate or `SPRING_DRAINAGE_THRESHOLD`: a threshold
nudge would re-classify cells on no physical basis and leave the modelling error
in place.

Once `Spring` is reachable, `is_spring_cell` drops the Karst proxy and reads
`Hydro::Spring`, and the `spring ⊆ river` disclosure in its doc comment is
retired — `spring` becomes the independent signal it was always meant to be.

### 3.2 F7 — a cascade may not draw a tonogenesis it cannot condition

`draw_rule` becomes position-aware: `Tonogenesis` is offered only at a position
where a merger (`ClusterSimplify` or `FinalLoss`) has already been drawn.

**Consumption count is unchanged** — one `pick`, one `range_u32`, exactly as
today, so no draw-count variance is introduced and the pin-isolation contracts
are untouched. The picked *values* change, which is what the epoch records.

The repair lands in the **shared** `draw_rule`, so it governs both the historical
`SETTLED` cascade and the `WEAR` cascade. The alternative — fixing wear only —
is cheaper (one epoch, and a blast radius of settlement names instead of every
word in every lexicon in every world) and is a **live veto option at G3**. It is
not recommended, because making `draw_rule` correct for one caller and knowingly
wrong for the other requires threading a flag through it, which is this
campaign's own theme committed a fourth time: one rule maintained in two places
*is* F13.

The model claim this encodes is not merely mechanical: **a language does not
innovate tone before it has a merger to feed it.** Tonogenesis is a consequence
of segmental loss. Drawing it unconditioned was the modelling error; the wasted
slot was the symptom.

### 3.3 F13 — teach the duplicate The Watershed's staples

Bring `independently_steeped_concepts` current: the six staples, plus whatever
Task 1's audit finds (the entry names three prior recurrences — The Wearing's
toponymic concepts, The Toponym's variants, The Watershed's staples — so the
audit enumerates rather than trusts the list). `exposure-sound-{goblin,kobold}`
should return to reading the world rather than the duplicate's blind spots.

The predicates stay restated, not imported. That is the design and it has caught
real bugs twice.

---

## 4. The keystone — derive the checklist from the type

Fixing three instances without fixing the shape would guarantee a fourth. Three
guards, one move:

1. **Variant witness (terrain).** Over a fixed, deterministic seed sweep, every
   variant of `Hydro` must be witnessed in a real derivation. A variant no seed
   can produce fails, and no sweep width saves it — which is exactly the property
   `exposure.rs`'s existing concept sweep has, generalised from a hand-written
   concept list to an enum's own variants.
2. **Rule witness (language).** Over that sweep, every `RuleKind` must be
   witnessed *firing* — changing at least one word. The reachability check's
   rule-shaped twin, and the one that would have caught F7.
3. **Roster parity (lab).** The lab's gate table must consider the *same set of
   concepts* worldgen classifies, while its *predicates* stay independently
   restated.

Guard 3 is the resolution of F13's apparent tension, and is the campaign's real
finding. The duplicate's independence is load-bearing and importing worldgen's
predicates would turn a second opinion into an echo — but **nothing in that
argument requires the roster to be hand-maintained too.** Separating the two —
**parity of roster, independence of predicate** — keeps every property the
duplicate was built for and removes the only failure mode it has actually
suffered, three campaigns running.

Every existing guard in this repo derives its checklist from an author:
`TOPONYMIC_CORE` is "a hand-maintained list", the lab table is seven hand-written
rows, the `Hydro` tests are hand-built structs. `exposure.rs` is the single guard
already deriving its *input* from a seed sweep, and it is the one that caught
`spring` — one round late, and only in review.

---

## 5. Flagged for the owner

**1 — Two epoch bumps (the largest item).**
`language/<species>/name/settlement` **v3 → v4** and
`language/<family>/lexicon/root` **v3 → v4**.

Decision 0089 and the doc comment at `domains/language/src/naming.rs:989` say it
outright: *"`/v3` is frozen and the next consumption change here owes `/v4`."*
The Wearing merged at `0b65be20`, so a world saved off main already carries
`language/<species>/name/settlement: v3` in its `derived_under` stamp. There is
no unreleased epoch left to ride — this is the precise case the freeze rule was
written for. The second bump follows from §3.2 landing in the shared draw path;
choosing the wear-only variant instead would drop it to one.

**2 — F5 owes NO epoch, contradicting the framing this campaign was handed.**
The task described both follow-ups as owing "an epoch bump and a census regen."
F5 owes the regen; it does not owe a bump, and this was checked rather than
assumed. An epoch is declared only when a derivation moved (0084) *and* the move
can be stamped on a saved world (0089). `versioned_labels()`
(`cli/src/streams.rs:123`) records only `/vN`-carrying labels, and the generated
manifest lists sixteen — **none of them terrain's**. Terrain's labels are
unversioned by design. The "epoch v3/v4" phrases in
`domains/terrain/src/streams.rs` name *campaign* epochs from the decision log
(0056), not `derived_under` rows. Porosity is a derived scalar, not a drawn one:
no stream consumption changes. What moves is committed artifact bytes.

Minting a terrain label so the change becomes stampable was considered and
rejected as scope creep — but the gap is real (terrain output can move with
nothing in `derived_under` to record it) and is captured as **F14** rather than
annexed.

**3 — Semantic collision watch: `the-pigment`.**
Its plan (dated today) projects terrain's `MaterialBuffer` to a reflectance
mixture; F5 changes a `MaterialBuffer` field derivation. `make preflight` cannot
see this — the Tumult/Waterline lesson is that a clean GO says nothing about two
campaigns changing the same idea. Named here so the close does not discover it.

**4 — Census regen.** One regen, authorised at the close per the carve-out, on
lefford, as the last act before merge and committed separately from the pin
re-baseline.

---

## 6. Preregistration

One prediction, frozen before any repair lands (decision 0016;
`preregistration_guard` enforces it):

> **H1.** Removing the unconditioned tonogenesis draw raises the wear cascade's
> match rate, and therefore raises toponymic name survival above the present
> 14/650.

**The null is publishable.** If survival does not move, the finding is that
LANG-11's bottleneck is downstream of the cascade, not in it — which contradicts
the registry row's current reading and would be the campaign's headline. The
prediction is the registry's own, not a post-hoc one.

**Deliberately NOT preregistered:** F5's and F13's census effects. A column that
is identically `0` can only rise; a boolean false on 767/1000 worlds can only
rise. Preregistering a foregone conclusion is theatre, and dressing repairs as
predictions is precisely the metric-chasing the discipline exists to contain.

---

## 7. Sequencing

**F13 → F5 → F7**, then the regen.

F13 first because F5 relaxes a gate that F13's stale table would redden; landing
F13 second means knowingly pushing a red commit. F7 last because it has the
largest blast radius and the preregistered measurement wants a stable tree
underneath it. The census regen is the **last act before merge, after the final
absorb**, with goldens committed immediately and separately from the pins — three
regens died at ~14 minutes each to commits landing in the window, and a mid-run
epoch poisons a regen with nothing going red.

Per decision 0086: campaigns and `make gate` on the Mac; the heavy tier and the
census on lefford.

---

## 8. Non-goals

- **Re-gating `Karst` on drainage instead of porosity.** More physical still
  (dissolution needs flow), and it was the ideonomy pass's first recommendation.
  Backed out: moving both sides of the branch in one campaign makes the
  measurement uninterpretable — a karst shift could not be attributed. Captured
  as **F15**.
- **Introducing a versioned terrain stream label** (**F14**, above).
- **The rest of LANG-11's name cycle** — conventionalization, reanalysis and
  renewal stay deferred, as The Wearing left them.
- **F3, F6, F9, F10** — registry rows and demography's routing, not this
  campaign's work.
- **Splitting porosity into matrix and fracture axes.** YAGNI.

---

## 9. Decisions promoted from the ledger

| # | Decision |
|---|---|
| 2 | F5 owes no epoch — terrain has no stampable label (0084 + 0089) |
| 3 | F7 owes `name/settlement/v4` and `lexicon/root/v4` (0089) |
| 4 | F7's repair lands in the shared `draw_rule`, not wear-only |
| 5 | F5's repair adds porosity terms; `k_g` calibrated by sweep, not by this spec |
| 6 | F13 is mechanically coupled to F5 and cannot be deferred |
| 7 | Keystone: derive the checklist from the type; parity of roster, independence of predicate |
| 9 | One preregistered prediction (H1); repairs are not dressed as predictions |
