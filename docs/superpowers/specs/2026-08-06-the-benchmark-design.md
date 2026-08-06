# The Benchmark — the elevation datum

**Campaign:** The Benchmark · **Date:** 2026-08-06 · **Status:** COMPLETE — merged 2026-08-06;
see [the chronicle](../../../book/src/chronicle/the-benchmark.md) · **Branch:** `the-benchmark` off main @ 402670c5

A surveyor's benchmark is a permanent mark of *known* elevation — the physical
admission that a height means nothing until you say what it is a height above.
This campaign gives Hornvale that mark in the type system.

---

## 1. The defect, as measured

A bug report on seed 42: a room described as `Tropical seasonal forest` reports
`-2936 m elevation`.

Nothing is wrong with the world. Sea level is **derived per world**, not zero —
`derive_sea_level` (`domains/terrain/src/elevation.rs:656`) returns the elevation
at the percentile matching the pinned ocean fraction, and the land test is
`elevation >= sea_level` (`elevation.rs:1089`). For seed 42 that value is
**-2936.17 m**. The reported room sits **0.2 m below sea level** — a shoreline
forest cell. Over all 40,962 cells, land biomes below sea level: **0**. Marine
biomes at or above it: **0**. The classifier and the datum agree exactly.

What is wrong is that two different physical quantities share one representation.

| | `ReferenceElevation` | height above sea level |
|---|---|---|
| datum | isostatic reference (0 m = reference-thickness crust at equilibrium) | this world's derived sea level |
| zero means | the same physical thing on **every** world | a different number on every world |
| comparable across worlds | yes | no |
| type today | `ReferenceElevation` (kernel) | **none** — a bare `f64` |

The kernel already says this in prose. `kernel/src/units.rs:34-39`:

> *"This is NOT height above sea level — sea level is itself a value of this
> type, derived from the elevation field."*

So the convention was stated once, then re-derived by hand at every consumer.
**Every consumer that re-derived it is correct. Every consumer that forgot the
question exists is wrong.** That distribution is the whole finding:

- **Correct** (subtracts): `windows/locale/src/substrate.rs:28`,
  `windows/locale/src/budget.rs:91,117`, `domains/climate/src/biome.rs:302`,
  `domains/terrain/src/render.rs:137`, `windows/worldgen/src/lib.rs:1653,2579`,
  `cli/src/repl.rs:85`, and all of `windows/lab/src/metrics.rs`
  (`:1052` `e - sea > 2000.0`, `:3809` `sea_level - elevation`, `:3879`
  `>= sea_level`, `:5511` `.max(sea_level)`).
- **Wrong** (uses the raw value where a height was meant):
  `windows/scene/src/surrounds.rs:30-40,251` and `windows/vessel/src/focalize.rs:79`,
  plus the locale CLI renderer.

### 1.1 The consequence that is not cosmetic

`relief_band` (`windows/scene/src/surrounds.rs:30-40`) bands **raw** elevation
against absolute thresholds — `< -3000 → abyss`, `< 0 → shelf`, `< 300 →
lowland`, `< 1000 → upland`, `< 2500 → highland`, else `alpine`. With seed 42's
sea level at -2936, nearly all land falls in the `< 0` bucket. Measured over
seed 42's 11,066 land cells:

| band | raw (what ships) | sea-level-relative |
|---|---|---|
| `abyss` | 0 | 0 |
| `shelf` | **8162 (73.8%)** | 0 |
| `lowland` | 760 (6.9%) | 1536 (13.9%) |
| `upland` | 1373 (12.4%) | 1484 (13.4%) |
| `highland` | 770 (7.0%) | 3878 (35.0%) |
| `alpine` | **1 (0.0%)** | 4168 (37.7%) |

Three-quarters of the planet's land carries a **marine** relief band, and the
world has exactly **one** alpine cell instead of 4168. The committed golden
`book/src/gallery/scene-surrounds-seed-42.json` has a relief histogram of
`{1: 31}` — all 31 cells `shelf`. The wrong datum is published.

Worse, it is *ratified*. `book/src/reference/scene-surrounds-v1.md:172-180`
tabulates the band boundaries and states they are half-open **"against
`elevation_m`"**, adding that *"changing a boundary mints
`scene/surrounds/v2`"*.

### 1.2 Why nothing caught it

Decision 0044 considered this exact type and deferred it *on a stated
condition*, written into the source at `kernel/src/units.rs:86-88`:

> *"A local intermediate (lapse rate, depth shading) — a height-above-a-datum
> earns its own type only if it crosses a pub boundary."*

That condition was false when written and is false today (`relief` in
`substrate.rs:28` is a local in a `pub(crate)` fn). It becomes **true as a
consequence of fixing this bug**, because a corrected `surrounds` must emit a
sea-level-relative height across a pub schema boundary. Between the campaign
that named the trigger and the campaign that pulled it, nothing re-checked.
This is not a relitigation of 0044 — it is the trigger 0044 wrote down.

---

## 2. Non-goals

- **No change to the elevation field, sculpting, or sea-level derivation.** The
  physics is correct and is not touched.
- **No epoch, and no save-format change.** Verified: the only elevation-bearing
  ledger predicates are `sea-level-m` (`-2936.1668` on seed 42) and
  `highest-elevation-m` (`2668.7366`), both on the isostatic datum, both
  unchanged. The datum is *already* a committed fact — every consumer can
  recover it.
- **No census regeneration and no committed census number moves.** Verified: the
  entire measurement layer already subtracts (§1). This is the campaign's
  largest deliberate cost saving.
- **No lint or drift-check for raw-elevation emits.** Considered and dropped at
  G1 — see §7.
- **No `Depth` type.** One signed quantity with a named accessor (§4.2).
- **No re-tuning of the band boundaries.** −3000/0/300/1000/2500 stay exactly as
  they are; only the quantity they measure changes. Whether that distribution is
  well-calibrated is a separate question (§9).

---

## 3. Design principle

**A quantity whose zero is a choice must not share a type with one whose zero is
universal.**

This is decision 0008 applied to a datum rather than a unit, and 0008's own
motivation is verbatim this failure mode: *"code has twice mixed up 'absolute
reading' with 'difference from present' when feeding the same function."* The
`Temperature`/`TempAnomaly` pair is the shipped precedent; this campaign builds
its elevation sibling.

**This is not a new principle — it is ratified doctrine, unapplied.** Decision
0044's kernel-units doctrine classifies quantities by Stevens scale and requires
that **interval types carry their datum**; `ReferenceElevation` does (the word
"Reference" is the datum), and its sibling has been missing since. 0044 also
names `ReferenceElevation` as the pilot "retiring the elevation bare-`f64`
waiver".

**That last clause turned out to be wrong, and the campaign disproved it rather
than fulfilling it.** The audit at close retired **zero of the waiver's five
sites**: each carries a genuine *absolute* reading kept deliberately beside the
new height, because the two answer different questions. Checked empirically —
deleting the tag at `LocaleFields.elevation_m` makes `type-audit check` fail.
The doctrine now records the correction. "Elevation" was never one convention
to retire, which is §4.1's polymorphism finding one level up.

---

## 4. The kernel type

### 4.1 `SeaLevelHeight`

A new newtype in `kernel/src/units.rs`, beside `ReferenceElevation`:

```rust
/// Metres above this world's sea level. Signed: negative below.
///
/// Distinguished at the type level from [`ReferenceElevation`], which is an
/// absolute reading on the planet-independent isostatic datum. A
/// `SeaLevelHeight` is per-world — its zero is a *derived value* of the other
/// type — so two `SeaLevelHeight`s from different worlds are comparable to each
/// other in a way their `ReferenceElevation`s are not, and vice versa.
pub struct SeaLevelHeight(f64);
```

The conversion is a **named method**, `ReferenceElevation::above(datum) ->
SeaLevelHeight`, and `Sub` keeps its `f64` output.

**Amended after G3, on evidence.** This spec originally retyped `Sub`'s output,
following `Sub for Temperature`. Task 2's implementer ran the compiler and it
produced 21 errors naming counterexamples the survey had missed — decisively
`domains/climate/src/moisture.rs:169` and `provider.rs:182`, which compute
`elevation.get(c) - elevation.get(upwind)`. That is an **orographic rise between
two places**, not a height above any datum. Subtracting two elevations is
polymorphic in meaning, so typing the *operator's* output as a datum-named
quantity would make the type system assert something false about every
non-sea-level difference.

The temperature analogy does not carry: `a - b` on two `Temperature`s is always
an anomaly, because there is only one thing a temperature difference can mean.
Elevation has at least three (height above sea level, terrain rise between
places, local relief detail), so the meaning must be *named at the call*, which
is exactly what decision 0008 prescribes — "validating constructors and **named
conversions**."

The revision is strictly smaller: workspace blast radius drops from 21 compile
errors to **zero**, and enforcement is unaffected, because it never lived in the
operator (§5).

Surface: `get() -> f64`, `total_cmp`, `depth()`, and one escape-hatch
constructor (§4.3). No `Add`/`Mul` until a consumer needs one — the doctrine's
"richer surface stays reactive" rule (`docs/design/kernel-units-doctrine.md`).

### 4.2 `depth()`, not a `Depth` type

Depth is a live third quantity — `metrics.rs:3809` computes `sea_level -
elevation` today. A separate type doubles the API for one sign flip; leaving
callers to write `-h` by hand reintroduces the sign confusion this campaign
removes. So `SeaLevelHeight::depth() -> f64` returns the positive-downward
reading, and `metrics.rs:3809` is rewritten through it.

### 4.3 The escape hatch is deliberate and documented

Deserialization cannot subtract two readings it does not have, so
`SeaLevelHeight::from_metres(f64)` exists. `TempAnomaly::from_offset_c` is the
exact precedent and its doc states why. Ours additionally states what it costs:
it is the hole through which this bug class can return, and a caller reaching
for it should be sure it is not holding two `ReferenceElevation`s.

### 4.4 What the type does NOT catch — stated honestly

Producing a height is *not* forced. `Sub` still returns `f64`, and `.get()` is
used liberally, so a consumer can compute `elevation - sea_level` and pass a
bare number around. `substrate.rs:28` is exactly this shape and stays uncaught.

So the type is not a total guarantee, and this spec does not claim one.

**Enforcement lives at the consumer, not the producer** — which is what the G1
matrix said before the operator idea was ever considered: only the compiler can
refuse a *band function* the wrong quantity. A function whose parameter is a
`SeaLevelHeight` cannot be handed a raw reading, however many `.get()` calls
precede it, and the only ways to produce one are `above()` and the documented
escape hatch — both of which name what they are doing. That is why losing the
operator overload costs nothing: it was belt-and-braces over the mechanism that
actually works, and it was asserting a falsehood to get there.

---

## 5. The consuming surfaces

| surface | change |
|---|---|
| `windows/scene/src/surrounds.rs` | `relief_band(h: SeaLevelHeight)`; call site at `:251` subtracts. The wrong call becomes a compile error. |
| `windows/locale/src/lib.rs` | `LocaleFields` gains `height_asl_m: SeaLevelHeight` beside `elevation_m`, blended by the same integer-weighted mean; `elevation_m` keeps its name, type and meaning (it is a correct `ReferenceElevation` reading, and removing it would break the correct consumers). |
| `windows/vessel/src/focalize.rs:79` | the biome datum reports height above sea level, not the raw reading. |
| `cli/` locale renderer | same. |
| `windows/worldgen/src/lib.rs:1653` | the field named `elevation` holds a *difference* (correctly computed, misleadingly named); renamed to carry the datum. |

**The naming half.** Every field or local holding a height above sea level says
so in its name — the gauge-vs-absolute-pressure discipline (`psia`/`psig`),
which is what engineering reached for when the same confusion cost it real
hardware. A type cannot travel through JSON; a field name can, which is why
naming and typing are both required and neither suffices.

---

## 6. `scene/surrounds/v2`

Correcting `relief` changes every observable band value, which is what the v1
reference page's versioning promise is about. So the schema is minted as
`scene/surrounds/v2`, carrying:

- `relief` — banded against height above sea level, boundaries unchanged.
- `sea_level_m` — **new**, matching what `scene/tiles/v1` and
  `scene/region/v1` already publish (`scene/src/lib.rs:153`,
  `region.rs:249`). Its absence from `surrounds` was an omission, not a
  decision: it left the one scene kind whose values were wrong also the one
  kind a client could not correct.
- `height_asl_m` — **new**, a sea-level-relative height on the observer's own
  cell, beside the existing raw `elevation_m`, and `null` on non-`here` cells
  exactly as `elevation_m` already is.

**No v1 emitter is retained.** Verified before deciding: `scene/surrounds/v1`
is **not** in the `clients/world-wasm` catalog (which carries `system`, `moons`,
`neighbors`, `tiles`, `tiles-region`, `eclipses` only), and no in-repo client
reads `relief`. There is nothing to migrate, so a retained v1 emitter would be
pure cost.

---

## 7. Why no lint

A drift-check scanning for raw-elevation emits was in the original framing and
was dropped at G1. Scored across the four things one can do with a height:

```
                  | newtype + naming | lint
------------------+------------------+---------------------------
display           | compile error    | caught once; silent on the
                  |                  | next new caller
classify          | compile error    | cannot see intent
compute           | already correct  | already correct
serialize         | field name       | a schema omission is not a
                  | states the datum | call site at all
```

The lint is dominated in every row. Decisively: the failure mode here is
**omission** — the wrong sites never asked the question — and a lint on omission
is a lint on the absence of code.

---

## 8. Testing

1. **The emitted band matches the emitted height.** For every cell carrying a
   height, `relief == relief_band(height_asl_m)`. This is the assertion that
   pins the **call site**, and it is the one that matters: a unit test on
   `relief_band` alone passes both before *and* after the fix, because the
   defect is which argument the call site passes, not what the function
   computes. (The Manikin's lesson — a mutation proves only what it perturbs.)
2. **No land cell bands as marine, at globe scale.** Banding every land cell by
   height puts zero of them in `abyss` or `shelf`. This is airtight rather than
   statistical: a land cell is *defined* by `elevation >= sea_level`, so its
   height is `>= 0` by construction. Today 8162 of seed 42's 11,066 land cells
   band `shelf`, so it fails loudly against unfixed code.
3. **Require RED first.** Each test above is run against unfixed code and
   observed to fail *on its assertion*, not merely to fail compiling — the
   lesson from The Timekeeper, where only "require RED" caught defects inside
   the detector itself.
4. **The datum is recoverable from the document.** `scene/surrounds/v2` carries
   `sea_level_m`, so a client can re-derive any band from `height_asl_m` alone.

**A tempting assertion that is false, recorded so no one adds it.** "A room on
dry land never reports a negative height" does *not* hold, and a test asserting
it could never pass. A room's height is an integer-weighted blend of three
corner cells while its `water` kind is a point sample of the dominant corner, so
a shoreline room can be dry-land-dominant and still blend a few centimetres
below sea level — the reported room is 0.2 m under. That asymmetry is real and
out of scope (§12.4); the correct guards are the two above, which are stated over
*cells* (where the invariant holds by definition) and over *self-consistency*
(where it holds by construction).
5. **Determinism unchanged.** Same seed → byte-identical world; the seed-42
   ledger's `sea-level-m` and `highest-elevation-m` are unchanged.
6. **Type-audit tags** on every new pub boundary, and the `elevation-convention`
   waiver retired where the new type replaces a bare `f64`
   (`docs/design/kernel-units-doctrine.md` calls that waiver *temporary*, to be
   retired by exactly this type).

---

## 9. Open question deliberately not answered

Banded correctly, seed 42's land is 35.0% `highland` and 37.7% `alpine` —
72.7% above 1000 m. That may be right for a bimodal hypsometry or may indicate
the band boundaries suit Earth rather than Hornvale. **This campaign does not
re-tune them**, because doing so inside a datum correction would confound two
changes and leave neither measurable. It is recorded as a follow-up.

---

## 10. Success criteria

- A walker on land is told a height above sea level, and it is not negative.
- `relief_band` cannot be called with a `ReferenceElevation` — the old bug is a
  compile error, demonstrated by a commented-out failing line or a doc test.
- `scene/surrounds/v2` carries `sea_level_m`; its seed-42 golden shows a relief
  histogram other than `{1: 31}`.
- No committed census number moves; the seed-42 world is byte-identical.
- `make gate` green; `make rebaseline` diff reviewed and limited to the
  artifacts enumerated in §11.

## 11. Artifacts that move

Verified by command, not inferred:

- `book/src/gallery/scene-surrounds-seed-42.json` (histogram `{1: 31}` today)
- `book/src/gallery/generated/surrounds-seed-42/*.txt` — three ascii charts
  (`scripts/regenerate-artifacts.sh:356-368`)
- `book/src/reference/scene-surrounds-v1.md` → the v2 reference page
- `docs/audits/type-audit-report.md` — any pub-boundary change drifts it
- the seed-42 possession/locale transcripts in the gallery

## 12. Risks

1. ~~**`Sub`'s return-type change is a workspace-wide compile break.**~~
   **Retired by the §4.1 amendment** — the named conversion touches no existing
   site, and `cargo check --workspace --all-targets` is clean. The risk was
   real: the compiler found 21 errors, and the mitigation it named ("a
   mechanical sweep is where a wrong `.get()` gets inserted to silence an
   error") is precisely what would have happened, since three of those sites
   were not sea-level quantities at all and `.get()` would have "fixed" them
   into a type that lied about them. The survey that missed them was fooled by
   `_m` suffixes on parameters that were already typed — a grep-derived plan is
   only as complete as its grep.
2. **The `.get()` escape means the guarantee is partial** (§4.4). Stated in the
   spec so no reviewer infers a total guarantee from "the compiler catches it."
3. **The band distribution may look wrong after the fix** because it is finally
   measuring the right thing (§9). A reviewer expecting Earth-like proportions
   may read a correct result as a regression.
4. **The blend/point-sample asymmetry survives this campaign.** A room's height
   is a three-corner weighted mean; its `water` kind and `biome` are point
   samples of the dominant corner. At a shoreline they disagree by centimetres,
   which is why the reported room reads 0.2 m below sea level under a forest
   biome. This is not the reported defect (which was 2936 m, not 0.2 m) and is
   deliberately not fixed here, but it bounds what any test may assert (§8).

## 13. Decisions (promoted from the ledger)

The full ledger is at `.superpowers/sdd/decision-ledger.md`. The load-bearing
ones: newtype + naming and no lint (#1); 0044's condition met rather than
relitigated (#2); `SeaLevelHeight` naming the datum not the relation (#3); one
signed type with `depth()` (#4); a documented escape hatch (#5);
`scene/surrounds/v2` with no v1 emitter (#6); no census regen (#7).
