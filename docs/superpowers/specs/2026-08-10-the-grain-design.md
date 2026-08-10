# The Grain — the fine layer's inheritance policy

**Status**: spec, awaiting review (G3)
**Campaign**: The Grain
**Branch**: `campaign/the-grain`
**Occasioned by**: an attempt to add colour to the TUI game client, which found
the client had nothing local to colour. That campaign is deferred — see §9.

"Grain" is the project's own word for this. `SurroundsCell`'s fields are
documented "fine grain, `null` when coarse"; `LegendEntry`'s doc calls map and
prose "two grains of one lens". This campaign is about what happens at the fine
grain, and the answer turns out to be: less than the sim already knows.

## 1. What this campaign produces

A **fine layer that varies**. Today a room inherits most of what it is from the
canonical grid cell containing it — one cell in 4096 — so a walk-band
neighbourhood is uniform in almost every field. The sim computes genuine
sub-cell variation and then discards it at the emit boundary, or overrules it
with a coarser value.

Three changes, all in the producer. No client work is in scope.

## 2. The defect, measured

Walk depth is `globe_level + 6` (`windows/vessel/src/agent.rs:35`), so 4^6 =
4096 rooms per canonical grid cell, and a radius-4 neighbourhood is 31 rooms —
roughly 1/132 of one cell.

**Measured on seed 42's flagship possession, 31 rooms** (the committed fixture
`clients/game/core/tests/fixtures/session-seed-42-turn-0.json`, corroborated
independently by `book/src/gallery/scene-surrounds-seed-42.json`):

| field | across the 31-room neighbourhood |
|---|---|
| `biome` | **1 distinct value** — tropical-rainforest, all 31 |
| `water` | **1 distinct value** — river, all 31 |
| `color` | **1 distinct value** — `[31,31,1]`, all 31 |
| `relief` | 2 of 6 bands (10 shelf, 21 lowland) |
| `temperature_c`, `moisture`, `elevation_m`, `height_asl_m`, `regime` | **`null` on 30 of 31** |

The consequence is visible in the product. `possess --seed 42`'s own `map` verb
reports `0 tinted, 31 withheld (water, a mark, or you)` — the render discloses
that it carries no colour — while the prose one line above says *"Tropical
rainforest — buttressed canopy, shaded, in a hollow"*. **The chart says the
player stands in open water; the prose says a rainforest hollow. Both read the
same locale, and the prose is right.**

`hornvale locale --room <id> --json` over eight of those same 31 rooms shows
what the locale actually holds:

```
field            range across the neighbourhood        verdict
temperature_c    23.4997 .. 23.5749   (0.075 C)        flat
moisture         0.69843 .. 0.70729   (0.0089)         flat
height_asl_m      -1.458 .. 12.463    (13.9 m)         real -- drives `relief`
micro.openness   -0.9977 .. +0.9792   (full range)     real
micro.wetness    -0.9959 .. +0.6251                    real
micro.aspect     -0.3758 .. +0.8947                    real
micro.relief     -0.5235 .. +0.9492                    real
descriptor_noun  "a stream gully" | "buttressed canopy" | "a liana tangle"
```

So the sub-cell variation exists, it is large, and none of it reaches a
consumer.

## 3. The thesis: four inheritance strategies in one struct

`windows/scene/src/surrounds.rs`'s cell literal uses four different policies in
adjacent lines:

| line | field | policy |
|---|---|---|
| 311 | `biome` | dominant corner — **inherited** |
| 312 | `water` | dominant corner — **inherited** |
| 313 | `relief` | `relief_band(locale.fields.height_asl_m)` — **banded from the blend** |
| 315-318 | `temperature_c`, `moisture`, `elevation_m`, `height_asl_m` | blended, then **discarded** unless `is_here` |

`windows/locale/src/lib.rs` documents the split it intends — :100 "the
**blended** continuous fields at the room centroid (weighted mean of the three
corner cells)", :199 "the corner cell a room's *categorical* readings come from:
the greatest blend weight" — but the split is not categorical-vs-continuous.
**`relief` is categorical too**, six ordered bands, and it is banded from the
blend rather than inherited. That is the model. Water is a banding of a
continuous underlay in the same sense, and it is on the wrong side of the line.

Biome is genuinely different and stays inherited: averaging *desert* and
*tundra* yields no biome, and the classification is a lookup, not a ramp.

## 4. Change A — emit `regime.micro` per cell

`MicroField` (`windows/locale/src/regime.rs:95`) is four axes in [-1, +1]:
`relief` (hollow..rise), `aspect` (shaded..sunlit), `wetness` (dry..wet),
`openness` (**canopy openness, closed..open**). Its struct doc calls it "the
sub-cell micro-field". `Regime` already derives `Serialize`.

`ctx.describe(addr, at)` already runs for every cell in the loop
(`surrounds.rs:284`), so the derivation cost of emitting this is **zero** — the
values are computed and thrown away.

Emit `micro` on `SurroundsCell` for every cell, appended last in key order (the
additive move `spatial` and `color` already made).

**Also correct the doc comments** on `regime`, `temperature_c`, `moisture`,
`elevation_m` and `height_asl_m` (`surrounds.rs:121-135`). They say "fine grain,
`null` when coarse". That is false: the gate is `is_here`, not grain. A reader
of those comments concludes the data does not exist at coarse grain, which is
the opposite of true.

**Deliberately NOT un-gating `temperature_c` and `moisture`**: measured flat to
four decimals across a neighbourhood (§2). Emitting them would ship real and
useless variation and move artifact bytes for nothing. They stay `is_here`-gated
until a consumer wants them at a zoom where they vary; `elevation_m` and
`height_asl_m` are judged the same way and left alone, since `relief` already
carries the height signal a consumer needs.

## 5. Change B — band water from the blend, as relief already is

Replace `water: locale.fields.water.index()` with a room-level water kind banded
from the blended/micro wetness, mirroring `relief_band`'s shape.

> **CORRECTED AT PLAN TIME.** The first version of this section banded water
> from `micro.wetness`, on the evidence that `wetness > 0` separates the sim's
> own `descriptor_noun` 3/3 and 5/5 over eight sampled rooms. **That evidence
> was circular and the mechanism was wrong.** `micro_field`
> (`windows/locale/src/micro.rs`) draws four sub-streams of `LOCALE_MICRO` from
> the room's *address noise* — its module doc says so: "grounded per-room
> continuous axes drawn from the room's address noise, so a walk through
> homogeneous biome still varies room-to-room". It has no coupling to terrain.
> And `descriptor_noun` is *rendered from* `micro`
> (`grammar::render(negations, micro, expr, …)`), so wetness predicting "stream
> gully" is a definition, not a correlation. Banding water from it would scatter
> river rooms as salt-and-pepper noise through a canonical cell — spatially
> incoherent, and exactly the plausible-and-wrong shape this project guards
> against. What follows is the corrected mechanism.

**Blend the physical underlay, then apply the existing threshold** — the same
two steps `relief` takes. `WaterKind` is already a pure function of continuous
inputs: `hornvale_terrain::water::classify(elevation_m, sea_level_m, drainage,
endorheic, is_terminal_sink)`, "pure and total", `pub`. `Globe` carries
`drainage: CellMap<f64>` and `endorheic: CellMap<bool>`.

So in `LocaleContext::describe`, where the `blend` closure and the blended
`elevation_m` / quantized `sea_level_m` already exist:

- **blend `drainage`** across the three corner cells, as elevation already is;
- reuse the already-blended `elevation_m` and `sea_level_m`;
- take `endorheic` and `is_terminal_sink` from the **dominant corner** — they are
  flags, not ramps, and a flag has no meaningful weighted mean;
- call `classify` on that.

**Two things this buys over the version it replaces.** It is spatially coherent:
blended drainage falls off with distance from the river-carrying corner, so river
rooms form a gradient rather than static. And **no new constant is introduced** —
`RIVER_MIN_DRAINAGE = 15.0` is already tuned with a documented rationale
("keeps rivers the minority landform, ~6.7% of seed-42's land"), so there is no
threshold to fit and §8's preregistration gets simpler, not harder.

**The honest open question.** Drainage accumulation is a flow-network quantity,
not a smooth field — it jumps by orders of magnitude along a channel. An
area-weighted blend of a corner at drainage 200 may leave *every* room in the
cell above 15, reproducing the defect, or may threshold somewhere arbitrary.
**This is unmeasured**, it is what H1 tests, and a null is a real result: it
would say sub-cell water needs actual hydrology — `MAP-64`'s flow graph — rather
than a blend of a network statistic.

**What the coarse fact still means.** A canonical river cell genuinely is river
country; the defect is claiming every room in it is standing water. The coarse
kind remains available to any consumer that wants it via the corner weights, so
this change narrows a claim rather than deleting one.

**Consumers, enumerated** (`grep` over `fields.water` / `is_fresh()`): exactly
two live call sites — `surrounds.rs:312` (the chart) and
`windows/vessel/src/liveness.rs:704` (`is_fresh()`, drinkability). Both are
things this change *should* affect: a dry canopy room should not be drinkable,
and today it is. That behaviour change is the point, not a side effect, and it
wants its own test.

## 6. Change C — caves on the surrounds wire

`surrounds.rs` emits exactly one mark `kind`: `"settlement"` (line 483); the
session adds `"agent"`. Caves exist in `domains/terrain` (The Hollow,
`cave_color` in `render.rs`) and reach no field of `scene/surrounds/v2`, so no
consumer can draw a cave mouth. Emit them as marks, alongside settlements.

This is the smallest of the three and is included because it is the same shape
as Change A — surfacing something the sim already holds — and because a client
cannot distinguish "no caves here" from "caves are not emitted".

## 7. Out of scope, named rather than dropped

- **Colour's own coarse inheritance.** A cell's `color` is the bedrock's, and
  bedrock comes from a coarse lithology, so colour stays flat locally even after
  A and B. `surrounds.rs:913` states this ("a finer colour would need a finer
  lithology, not a different builder") and
  `the_color_is_no_finer_grained_than_the_chart_already_was` (line 916) **pins
  it deliberately**. Do not "fix" that test; it guards a real invariant. A finer
  colour is a lithology campaign.
- **`MAP-64`'s vector flow graph.** Change B gives a room an honest *local*
  answer about water. It does not give rivers *connectivity* below the canonical
  grid — a stream that runs from room to room. That is what MAP-64 defers and it
  stays deferred.
- **Any client work.** No glyphs, no palette, no animation. See §9.
- **Zoom on the snapshot channel.** `session.rs:951` builds the snapshot chart
  as `purview(0)`, hardcoded, while `map`'s own arm uses `purview(zoom)`.
  Letting the session remember a zoom is small and additive, and it was on this
  campaign's shortlist. It is out because Changes A and B remove the reason it
  was urgent: the local map gains texture without it. Recorded as a follow-up.

## 8. Preregistered measurement

Frozen before the code that would move it, per decision 0016. The freeze lives
here because a study JSON has no hypothesis field.

**H1.** With Change B in place, the number of distinct `water` values in a
seed-42 radius-4 walk-depth neighbourhood is > 1.
*Falsifiable, and a null is publishable*: if wetness does not vary enough at
walk depth to cross any threshold, Change B is the wrong mechanism and the
finding is that the local water answer needs something the sim does not yet
compute.

**H2.** The banded room water kind is **spatially coherent**, not static: within
a neighbourhood, rooms sharing a water kind are adjacent more often than a
random relabelling of the same multiset would give. This is the hypothesis the
corrected §5 mechanism earns and the rejected `micro.wetness` version could not
have passed — noise fails it by construction, which is what makes it the
discriminating test rather than a decoration.

**H3.** Change A's `micro.openness` spans more than half of [-1, +1] within a
single walk-band neighbourhood, on a seed sample. §2's n=8 suggests it spans
nearly all of it; H3 is the version that is allowed to fail.

**No threshold is fitted.** `RIVER_MIN_DRAINAGE = 15.0` is reused as-is. If H1
nulls, the response is **not** to retune that constant — it is tuned against a
documented canonical-level distribution and moving it would change every world's
rivers to fix a sub-cell rendering problem. The response is to record the null
and hand sub-cell water to `MAP-64`. Stating that here, before the measurement,
is the point of preregistering it.

## 9. The deferred client campaign

The colour and glyph work that occasioned this campaign is designed and
deferred, not abandoned. It carries a name — **The Illumination** — and its
design decisions are recorded so none of it is re-derived:

- Three land glyphs `.` `,` `;` by plant thickness from `micro.openness`; four
  water glyphs `~` `-` `=` `_` by depth, lighter shallow to darker deep; `^` for
  high relief; `>` for enterable things. **Colour carries biome**; the 22-glyph
  vocabulary is rejected, because 22 punctuation marks are not distinguishable
  without a legend whereas an ordinal ladder reads at a glance.
- The palette is a function of two coordinates read off `classify_land`'s own
  Whittaker bins, not a table of 22 swatches, modulated locally by
  `micro.wetness` and `micro.aspect`. Lightness is clamped away from black and
  white because a terminal's background is not ours to choose.
- **Animation without a clock**: the frame phase is a counter incremented when
  `crossterm::event::poll` times out. `clippy.toml` bans `Instant` and
  `SystemTime`, and that ban reaches `clients/` — clippy's config lookup walks
  up from the crate dir independently of cargo's manifest search, so an empty
  `[workspace]` table does not escape it.
- **A hazard to carry forward**: `surrounds_ascii::faded` maps remembered
  `.`→`,` and `:`→`;`. Under the thickness ladder those are *levels*, so reusing
  that convention would silently promote remembered sparse ground to medium. The
  client uses `Weight::Dim` instead — it has an attribute channel the plain-text
  renderer does not.

## 10. Costs

- **Artifact rebaseline.** Changes A, B and C all move committed bytes:
  `book/src/gallery/scene-surrounds-seed-42.json` at minimum, plus whatever else
  `scripts/regenerate-artifacts.sh` touches, plus the `clients/game` session
  fixtures, plus `docs/audits/type-audit-report.md` (new `pub` fields). **The
  size of that diff is not predicted here** — it gets measured by running
  `make rebaseline` and reading the diff, and Change B's is expected to be the
  interesting one because it changes a world *reading*, not just an emit.
- **Not an epoch.** No new seed draws, no change to stream consumption order, no
  seed-derivation label touched. Change B alters a derived reading, not a
  drawn one.
- **Not a save-format change.** `windows/locale` is a window; windows read the
  ledger and do not write to it, so a room's water kind is never persisted — a
  world re-derives it from its seed. This is an architectural guarantee from the
  layering rule, not a hopeful claim.
- **Cross-repo.** `scene/surrounds/v2` gains a field (A) and a mark kind (C),
  both additive; Change B changes the *values* of an existing field. The
  external Orrery parses this schema. Additive-or-versioned holds for the shape;
  the value change is the one to call out in the release note.

## 11. Testing

- Change A: a per-cell assertion that `micro` is present on **every** cell, not
  just `here` — the mutation that reintroduces `is_here` must fail it. And an
  assertion that the four axes are not all equal across a neighbourhood, which
  is the property H3 states.
- Change B: the H1 distinctness assertion at a discriminating room, plus the
  H2 agreement check against `descriptor_noun`, plus a drinkability test at a
  room the descriptor calls dry — the behaviour change in §5, stated as a test
  rather than a note.
- Change C: a cave-bearing room emits a cave mark; a cave-free room emits none.
  The direction this enforces is *emitted ⊆ real*; it does not catch a cave the
  terrain has and the scene omits, and its doc comment says so.
- Every mutation test asserts its target text existed before substituting, so a
  no-op mutation cannot masquerade as a robust implementation.

## 12. Decisions to promote

Three candidates for `docs/decisions/`, subject to G3:

1. **A fine layer bands from the blend; it does not inherit a category.** With
   biome named as the deliberate exception and why.
2. **An emit gate is not a grain gate.** `is_here` gating documented as "null
   when coarse" misled a whole campaign's design; a field's doc must say which
   condition makes it absent.
3. Possibly: **a window may narrow a coarse world fact for its own grain
   without that being a save-format change**, since windows do not commit.
