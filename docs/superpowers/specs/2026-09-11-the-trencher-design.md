# The Trencher — what the world grows, and what can eat it

**Campaign:** The Trencher. Rung 1 of the food-system program; successor to
The Ceiling, whose branch it continues rather than merges.

**Classification:** Architectural.

**Base:** `d84ac908a` (The Ceiling's tip). **Decision block:** 0976-0985.
**Predecessors:** The Gossan (rung 1 of the larder), The Sources (rung 2),
The Ceiling (the measurement that found the root cause), The Ground (which
reserved `thaumic` for exactly this).

## 1. The one-sentence claim

The world grows food at high resolution and lets creatures eat it at low
resolution. This campaign makes the two vocabularies the same size — and
then puts things in the world that use the difference.

## 2. The root cause, measured

Every number here is The Ceiling's, re-run at its own base and reviewed;
nothing in this section is inferred.

**The bridge between supply and diet is a mean, and it was not designed.**
`subterranean_energy` averages seven `EnergySource` terms into one scalar
before any diet sees them. Its own doc records the mean was chosen
**empirically** — a clamped sum pinned every rung's median to exactly 1.0, so
the mean was picked to dodge a saturating clamp.

**That mean averages across category boundaries.** Reading the seven sources'
own docs:

| source | what it actually is |
|---|---|
| Serpentinization | H₂ from rock + water |
| Radiolysis | H₂ from radioactive decay splitting water |
| IronReduction | reduced iron |
| SulphideOxidation | reduced sulphur |
| Methanogenesis | methane |
| **Geothermal** | **"the gradient itself"** — a condition, not a substance |
| **DetritalImport** | **surface organic matter**, reads `drainage` not rock; its own doc says it is "not one of the row's six" |

Five are chemical foods. One is a thermal condition. One is detritus — for
which a `DETRITUS` resource axis **already exists**. The mean adds all three
kinds together and calls the result "chemical food."

**Consequences, all measured by The Ceiling:**

- **Composition is real and unreachable.** `M1 = 3`: worlds disagree about
  which source dominates, at every rung, with median within-rung TV distances
  0.1424–0.2436 (nowhere near zero). But a diet weights only `ResourceAxis`
  values, `EnergySource` appears **nowhere** in `domains/species`, and
  `dominant_source`'s sole production caller carries it "purely for
  `inhabitant_datum`'s flavour text."
- **The ceiling is arithmetically unreachable.** The field's realized maximum
  anywhere over twelve seeds is `0.424277`; the corpus's top two of five
  `ENERGY` bands (`rich`, `teeming`) are **never realized at any rung in any
  world**. "An Underworld as lush as the Overworld" cannot be expressed today.
- **Changing the reduction rule does not fix it.** `separation(max-of-seven) =
  0.040745` against the shipped mean's `0.145249` — a non-averaging rule
  separated worlds **3.6x worse**. The cause is order-statistic saturation:
  every `yield_at` arm saturates, so the winner sits near its own ceiling and
  which ceiling wins is set by a few shared near-constant inputs.

**And the same shape appears twice more**, which is why this is a food-system
campaign and not a chemosynthate patch: nine `MaterialBuffer` lithology fields
reach no tolerance axis (`BIO-lithology-is-not-a-tolerance-axis`), and the
depth-attenuated light ladder is populated and read by nothing
(`BIO-underground-light-is-unfed`). **The underworld is richly derived and
thinly read.**

**Those three are NOT one defect, and an earlier draft of this paragraph
treated them as one** (corrected after The Tidemark's plankton finding, ledger
#18). They divide:

| finding | producer | consumer vocabulary | what is wrong |
|---|---|---|---|
| lithology's nine fields | exists | **absent** — no tolerance axis | a missing HALF |
| composition | exists | **absent** — no niche axis names a source | a missing HALF |
| the light ladder | exists | **exists** — the kernel's registered `LIGHT` axis | a missing **JOIN** |

**A missing half needs vocabulary built; a missing join needs a wire run.**
Different costs, different owners, and different odds of being closed by
accident. This campaign builds vocabulary (§4.2) and therefore addresses the
first two; the light join is named in §6 as out of scope rather than folded in
under a heading that made it look identical.

## 3. What makes the fix cheap, verified

**The diet vocabulary is OPEN.** `kernel/src/ecology.rs:32` — "A registered
member of the **open** resource-axis basis." Sparse, keyed by a stable `u16`,
ids 0-6 used, append-only. The Sources exercised it weeks ago by adding
`CHEMOSYNTHATE` (id 6). **Adding axes is a data change.**

**And it is not a save-format break.** `ResourceVector` carries no
`Serialize` derive and is not persisted; all 39 niches are authored in
`domains/species/src/lib.rs`. The id-stability warning protects the authored
registry's meaning, not saved bytes. Changing niches **moves worlds** —
capacity feeds placement feeds artifacts — so this is a `rebaseline`, not an
epoch.

**`TrophicMode`'s blast radius is three production files**, measured:

```
domains/species/src/lib.rs    45 mentions, all before the test module
                              (the definition + 39 authored kinds, one file)
windows/lot/src/slots.rs       4 mentions, no test module in the file
windows/worldgen/src/lib.rs    2 mentions
windows/sentiment/*            5 mentions, ALL inside #[cfg(test)] — fixtures
```

47 construction sites and 49 match arms, but the sites are the authored kinds
living beside the type. This is a large mechanical edit in one registry, not a
workspace-wide ripple.

**`thaumic` is a hook built for this.** The Ground reserved it on 2026-07-14
in as many words: "keeping the slot in the type from day one means the overlay
lands **additively, without an epoch**, and every mundane-world byte is
unchanged because the axis is zero everywhere." Ten sites set it, every one
hardcoded `0.0`, none via `Default`.

## 4. Design

### 4.1 The structural move: `TrophicMode` is three axes, not one

`BIO-trophic-trichotomy` (Nathan, 2026-08-26) states it, and this campaign
executes it:

> Microbiology factorises as **energy source** (photo/chemo) × **electron
> donor** (litho/organo) × **carbon source** (auto/hetero) — 2×2×2 = eight
> named modes, of which Hornvale spells four. … Fantasy then enters as VALUES
> on existing axes — a thaumic donor, a thaumic acceptor — **which costs
> nothing structurally, rather than as a fourth axis, which costs every
> consumer.**

This is the campaign's keystone and everything else is cheaper because of it.
`TrophicMode`'s four flattened values become a point in a three-axis space,
and **the fantasy tier is a value, not a new mechanism** — which is the whole
reason stage 3 costs what it does.

The implementer derives the axes' variants from the code and the row, not from
this spec: what the eight combinations are named, and which of them any
shipped kind currently occupies, is a reading of the 39 authored kinds.

### 4.2 The food vocabulary: by metabolite, not by reaction

New `ResourceAxis` members for what an organism actually eats, not for the
reaction that produced it. The reaction→metabolite mapping is **authored and
argued in the code**, because it is a judgment: two reactions both yielding H₂
feed one axis.

Two members of the current seven move rather than staying:

- **`DetritalImport` routes to the existing `DETRITUS` axis.** It is surface
  organic matter reading `drainage`; its own doc already says it is not one of
  the chemical six. This is a correction, not an addition.
- **`Geothermal` leaves the diet vocabulary.** "The gradient itself" is a
  condition that modulates chemistry, not a substance. It becomes a
  **modifier** on the chemical supplies. The implementer chooses the form and
  states the argument; what is fixed here is that it stops being food.

**`CHEMOSYNTHATE` survives as an aggregate** alongside the new axes (ruling,
ledger #1). A generalist eats the aggregate; a specialist names a metabolite.
The Tidemark is mid-campaign authoring a vent commensal that weights it, and
breaking a live peer campaign is not a cost this campaign pays for tidiness.

### 4.3 The reduction: a sum per metabolite, not a mean over categories

Each metabolite's supply is the **sum** of the reactions producing it. A sum
is physically right where a mean never was — two sources of hydrogen add — and
it is the direct remedy for §2's ceiling, because a sum is not bounded by its
own largest term the way a seven-way mean is.

**Whether it actually lifts the ceiling is preregistered (§5), not assumed.**
The Ceiling already measured that changing the rule alone does not help when
the change is `max`; the argument for `sum` is different in kind (it stops
averaging across categories rather than picking a different order statistic),
but the argument is not the measurement.

### 4.4 `thaumic`, gated on metaphysics

Give `thaumic` a real derivation instead of `0.0`, **gated so an inert world
is byte-identical to today**. The Ground's reservation is the licence and its
own terms are the constraint: additive, no epoch, mundane worlds unchanged.

**THE GATE DOES NOT EXIST AND THIS CAMPAIGN BUILDS IT.** Corrected here after
an earlier draft of this section assumed one: a grep for
`Metaphysics`/`metaphysics:` across `kernel/`, `domains/`, `windows/` and
`cli/` returns **no type, no field, no flag** — only doc comments describing
"the metaphysically-inert tier this campaign builds", which is a statement
that the current tier is inert *by construction*, not that anything selects
it.

The shape is settled by precedent rather than invention: `TerrainPins`
(`domains/terrain/src/pins.rs:9`) is `#[derive(Debug, Clone, Copy, PartialEq,
Default)]` with every field an `Option`. A metaphysics pin follows it —
**default `None`, meaning inert**. An unpinned world therefore takes the
existing code path unchanged, which is what makes The Ground's byte-identity
condition satisfiable rather than aspirational.

Thaumic then enters the biota as **values on §4.1's axes** — a thaumic donor,
a thaumic acceptor — not as a new axis and not as a new mechanism.

Two things this unlocks that were measured as blocked:

- **The `LIGHT` axis underground is unfed, not absent.** Climate's
  preregistered H5 measured it at `{0.0, 0.2}` and named its two missing
  mechanisms: "there is no bioluminescence term and `MaterialBuffer::thaumic`
  is identically zero." A strongly-fluxed chamber can then genuinely glow.
- **`MAP-40`/`MAP-53`'s ley-lines** become reachable, because Sculpting's
  carve seam was written potential-agnostically on purpose. **Not built here**
  (§6) — named so the successor knows the hook is live.

### 4.5 The biota: three doorways, and a web on each

Energy enters the underworld three ways, and they sort by depth — which is the
one axis that genuinely varies down there (`BIO-underground-tolerance-is-one-axis`):

```
ENTRANCE   biotic import     energy gathered OUTSIDE and deposited inside
SHALLOW    detrital import   surface matter percolating and falling in
DEEP       chemosynthesis    rock chemistry — and, where flux allows, thaumic
```

Consistent with measurement: `DetritalImport` wins shallow, `SulphideOxidation`
deep.

**Biotic import is new and is the campaign's one addition to the supply side
beyond the metabolites.** Something that feeds outside and returns — guano is
the canonical form — gives entrance caves a character no other doorway
produces, and it is the first supply in this model created by a creature's
*behaviour* rather than by geology.

The web, by role rather than by name (the roster is authored, not specified
here):

- **Producers** — chemolithotrophic mats and films, one per metabolite. A
  hydrogen mat is a different organism in a different cave from a sulphur mat,
  and the model can now say so.
- **Decomposers** — fungi, on **detritus and guano**, near entrances.
  Heterotrophs, which is why they belong at the doorways and not in the deep
  chemistry. The mushroom forest lands in the right place for the right
  reason.
- **Grazers** — the missing middle; nothing eats mats or fungi today.
- **Predators** — `rust-monster` exists with no base under it, the same
  predator-guild-with-no-producers shape The Tidemark found in the sea.
- **The weird** — kinds exploiting one chemistry, or thaumic flux, hard enough
  to live only where it dominates. These are only expressible after §4.1-4.4
  and they are the point.

**Target: 8-20 organisms**, mostly `SocialForm::Sessile` producers and
decomposers. `Sessile` is already "Rooted; placed on the map, never
agentified (autotrophs)", with `treant`/`shrieker`/`twig-blight` as
terrestrial precedent.

**Two rules inherited from The Tidemark, which paid for both today:**

1. **No two kinds may differ only by stratum.** The Delvers authored two
   subterranean peoples and withdrew them over exactly this. With metabolite
   axes, chemistry becomes a legitimate separator — which is what the
   underworld lacked.
2. **A distinctness test may count only axes the engine READS.** The Tidemark's
   M5 would have passed a pair separated solely by insolation curves, an axis
   nothing consumes. Counting authored differences measures the author, not the
   world.

### 4.6 Peoples are consumers, not prerequisites

`census-yellow-fix` is readmitting four underworld peoples (`mountain-dwarf`,
`duergar`, `kuo-toa`, `svirfneblin`) alongside the shipped `drow`. **This
campaign does not depend on that landing and does not duplicate it.** The
biota is what they would eat; if the readmission lands first, five peoples eat
it, and if it does not, `drow` and the new organisms do.

## 5. Preregistered measurement

Frozen before the code that would move it (0016). Both poles named; a
falsified prediction is a finding.

**T1 — Does the sum lift the ceiling?** The corpus-band occupancy table
(`inert`/`lean`/`fed`/`rich`/`teeming`) and the realized maximum, per rung,
over the frozen twelve-seed set, under the new per-metabolite supplies.

- *Baseline, measured:* max `0.424277`; `rich` and `teeming` never realized.
- *PREDICTION:* at least one rung realizes `fed` at ≥ 25%, and the realized
  maximum exceeds `0.5`.
- *If it fails:* the ceiling is not the reduction rule, and the remaining
  candidate is the saturating `yield_at` arms themselves — a finding that
  redirects the successor, reported as the headline.

**T2 — Is the vocabulary load-bearing?** The Ceiling's M4 shape, which its own
scope never got to run: for each authored chemotroph, capacity and placement
under (a) its authored niche and (b) the same niche with its metabolite weight
removed.

- *PREDICTION:* `placed(b) < placed(a)` for every authored chemotroph.
- *Equality is a RED, not a finding* — it means the kind places on its other
  axes and the new vocabulary feeds nothing, which is the defect this campaign
  exists to close.
- **Precedent, unverified here and treated as a prior:** The Staple D5B's
  shadow probe reports 12/12 seeds changing at least one capacity statistic
  under niche ablation at `CHEMOSYNTHATE` weights 0.51/0.65/0.80, with ablated
  profiles collapsing to `Undercroft` exclusively. That is capacity evidence
  from another campaign's arms, not this one's measurement.

**T3 — Does chemistry separate the biota?** Over the frozen seed set, the
count of distinct producer kinds whose occupied vertices are dominated by
different metabolites.

- *PREDICTION:* ≥ 3 producers occupy materially different chemical regions.
- *If it fails:* the metabolite axes exist and the world does not sort on
  them, which sends §4.5's roster back to authoring rather than the
  vocabulary back to design.

**T4 — The metaphysics gate, TWO-WAY.** A default-off pin makes the
byte-identity half hold because the new code path never runs, and **a control
the type system guarantees is not a control**. So both directions are
asserted:

- **Unpinned (inert):** seed-42 artifacts **byte-identical** to the
  pre-`thaumic` baseline. The Ground's own condition on the reservation, and
  the campaign fails if it is violated.
- **Pinned (thaumic present):** the same seed's artifacts **must differ**, and
  the readout names *where* — which fields moved, at how many vertices. A pin
  that changes nothing is a gate wired to a derivation that does not fire, and
  that failure is invisible from the inert side alone.

Both arms are required. The first alone is the vacuous half.

## 6. What is deliberately NOT in this campaign

- **Ley-lines** (`MAP-40`/`MAP-53`). The carve seam is potential-agnostic and
  the hook goes live here; carving with it is its own campaign.
- **Production, goods and exchange** — The Staple D5B's chokepoints 4-6. They
  consume this vocabulary, which is why they come after it. A chemotroph that
  places and still trades nothing is a D2 finding.
- **Settled underworld peoples** — §4.6.
- **The marine half of anything.** The Tidemark's, confirmed both ways on the
  wire: no marine kind, no `MARINE_FORAGE` change, no `Surface`-realm
  `CHEMOSYNTHATE` weight.
- **Unifying `ConditionNiche` and `Substrate` into the kernel** (Nathan's
  ruling, The Ceiling ledger #7). It is the *tolerance* half of the same
  boundary and this campaign is the *diet* half. Doing both at once doubles a
  registry-wide edit; the diet half is the one with measurements behind it.
- **The capacity/seating rung mismatch** — D5B measured it (2 of 2 endpoints,
  capacity-winning `Nadir` vs seated `Undercroft`) and owns the
  reconciliation.

## 7. Provenance

The Ceiling's measurements and its four peer exchanges — two with The Tidemark
(which corrected this line of work twice and had its own corrected once), two
with The Staple D5B (whose review found the ablation hole, and whose shadow
probe supplies T2's prior). The structural move is Nathan's own
`BIO-trophic-trichotomy` row, found by grepping the registry rather than
invented here; the `thaumic` licence is The Ground's reservation, likewise.
