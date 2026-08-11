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
blend rather than inherited. That is the model.

> **THE THESIS SURVIVES ONLY FOR ORDINAL FIELDS — §5 records why.** This
> section's first draft concluded "water is a banding of a continuous underlay
> in the same sense, and it is on the wrong side of the line." That inference was
> built, measured, and reverted. The reason the table above cannot show it:
> `relief` is **ordinal**, so banding a blend of its underlay moves a value at
> most one band and conserves the distribution's shape; `water` is **nominal**,
> so thresholding a blend of its underlay silently *deletes a category*. The
> real line is neither categorical-vs-continuous nor banded-vs-inherited — it is
> **whether the field's values are ordered.** Ordinal fields may band from a
> blend; nominal ones must take a partition.

Biome stays inherited for a related but distinct reason: averaging *desert* and
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

## 5. Change B — attempted twice, reverted, replaced by disclosure

> **BUILT, MEASURED, AND REVERTED.** This section has been wrong twice and is
> now a record of both attempts, because the reasoning is the deliverable.
>
> **Attempt 1** banded water from `regime.micro.wetness`. Rejected before
> implementation: `micro_field` draws four sub-streams of `LOCALE_MICRO` from
> the room's *address noise* with no coupling to terrain, and the evidence for
> it was circular — `descriptor_noun` is rendered *from* `micro`, so wetness
> "predicting" a stream gully is a definition.
>
> **Attempt 2** banded water from blended `Globe::drainage`, reusing
> `hornvale_terrain::water::classify` and introducing no new constant. It was
> implemented (`dd523ab2`), passed H1 and H2, passed the gate at 3350 tests —
> and was reverted (`76068e6a`). Three reasons, and the third is the one that
> generalizes.

**1. It splits a documented coupling invariant.** `dominant_corner`'s own doc:
"every categorical field a room reports — biome, water kind, substrate, and …
the rock whose reflectance the colour layer reads — names the same cell.
Splitting this would let a room be described as granite lowland and drawn in
basalt grey." Banding water from a blend while the other three still take the
dominant corner *is* that split. The ten flagship rooms the change "fixed" went
from `(river, shelf)` to `(ocean, tropical-rainforest)`: the contradiction moved
onto the documented invariant rather than being removed. **This invariant has no
test** — the change broke it and 3350 tests stayed green.

**2. It breaks a calibrated coarse statistic.** `RIVER_MIN_DRAINAGE`'s doc
records that 15.0 "keeps rivers the minority landform (~6.7% of seed-42's
land)". Measured: fresh water shrinks **29%** at walk depth (River 66→47 over a
4000-point sweep), thirst-driven fauna movement halves
(`possession-seed-42.md`, `766 stirred` → `389 stirred`), and 102 lines of
committed affect trace move. The constitution's `coarse constrains fine` says
higher fidelity refines, never contradicts, lower — so a refinement that shrinks
a calibrated coarse quantity is disallowed regardless of how good its local
behaviour looks.

**3. The bias was structural, predictable, and predicted.** A threshold is
maximally nonlinear, so `classify(blend(drainage))` is not the area-weighted
vote of `classify(drainage)` over corners. **Nearest-corner assignment is a
partition and therefore conserves area by construction; threshold-of-a-blend
does not, and it loses exactly the thin channels.** The measured loss landed
where theory says: concentrated in drainage `[15, 20)`, where 267 of seed 42's
700 river cells sit. A measurement confirming a predicted systematic bias is the
strongest form of this argument, and it is why no amount of retuning rescues the
mechanism.

**What the existing code was actually doing.** `dominant_corner` is evaluated
**per room**, over that room's own three corner weights — so water is *categorical
nearest-neighbour interpolation*, not "inheritance from a cell 4096× too big".
That is the correct method for a categorical field. Its apparent flatness at
radius 4 is what nearest-neighbour interpolation looks like when the view is
smaller than the interpolation stencil, which is a fact about the view, not a
defect in the field.

### What replaces it: disclose the resolution

The chart's uniformity was never the defect. It correctly reports that water is
defined at grid resolution while the view is finer than the water model. The
honest fix is at the layer that owns the confusion — the document should say what
resolution it speaks at, exactly as the `sight` block already declares what the
colour projection does *not* carry ("the red-green axis is not carried").

So `scene/surrounds/v2` gains a resolution disclosure: which of its fields are
decided at canonical-grid resolution and are therefore constant below it. A
consumer can then caption the difference instead of inferring a contradiction,
and a future reader does not repeat this campaign's diagnosis.

**Sub-cell water goes to `MAP-64`'s vector flow graph**, which is the only
mechanism that can put a stream *somewhere in particular* inside a cell. Naming
it here closes the question rather than leaving it open.

### The coupling invariant gets the test it never had

Independent of everything above: assert that a room's `biome`, `water`,
`substrate` and rock all resolve to the same cell. That is the invariant
`dominant_corner` documents and nothing checks, and attempt 2 is the proof it is
breakable in silence.

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

**H1 and H2 are retired, having served.** They were stated over the reverted
water mechanism, both passed, and passing them is what exposed the mechanism as
illegal — the hypotheses were about local behaviour and the failure was about a
global conservation property they did not ask about. **That is the lesson worth
keeping: a hypothesis about local variation cannot detect a violated global
invariant.** Recorded rather than quietly deleted.

**H3 stands and is now the campaign's only quantitative claim.**
Change A's `micro.openness` spans more than half of `[-1, 1]` within a single
walk-band neighbourhood, on a seed sample. The n=8 probe in §2 suggests it spans
nearly all of it; H3 is the version allowed to fail.

**H4 (new).** The coupling invariant holds: for any room, `biome`, `water`,
`substrate` and the rock the colour layer reads all resolve to the same cell.
Not a discovery claim — a regression guard on a property the code already has
and never checked. It fails against `dd523ab2`, which is what makes it worth
writing.

> **Measured: all four members covered.** A mid-campaign note claimed the test
> reached only three of the four; that was wrong, and Task 4 corrected it with
> file:line evidence. `dominant_corner`'s documented invariant
> (`windows/locale/src/lib.rs:220-227`) names exactly those four, and the test at
> `lib.rs:1337` covers all of them.
>
> **The real gap is a separate, fifth claim, and it is untested.** Two other
> paths resolve a cell for a room and do *not* use `dominant_corner`:
> `chamber_column_here` (the cave `delve` reads) and `column_here` (the water
> column `dive` reads) both break weight ties with `max_by_key` — last-max wins —
> against `dominant_corner`'s lowest-`CellId`. On an exact integer-weight tie
> they can name a different cell than `biome`/`water`/`substrate`/rock do. That
> divergence predates this campaign, is out of its scope, and is recorded rather
> than fixed. **It needs a decision: either those two paths join the invariant,
> or they are exempted in writing.** Leaving it as a comment is the failure mode
> this campaign already documented once.

**H5 (new, and it is a conservation criterion, not a preference).** Aggregating
room-level water back over a canonical cell reproduces that cell's own water
kind. Nearest-corner assignment satisfies this by construction; any future
sub-cell water mechanism — including `MAP-64`'s flow graph — must satisfy it too.
**This is the test attempt 2 would have failed before it was ever built**, and
writing it down is this campaign's most durable output.

**No threshold is fitted, and `RIVER_MIN_DRAINAGE` is not touched.** It is
calibrated against a documented canonical-level distribution; moving it to
change a sub-cell rendering would be the tail wagging the dog.

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

  > **REVERTED — THERE IS NO VALUE CHANGE TO CALL OUT.** Change B was built,
  > measured, and reverted (§5); no world *reading*'s values moved in what
  > merged. Everything cross-repo-visible this campaign actually ships is
  > additive: the new `micro` field (A), the new `"cave"` mark kind (C), and
  > the `resolution` disclosure block that replaced Change B (§5's "What
  > replaces it"). A release note drafted from the sentence above would
  > announce a water-value change that does not exist in the shipped schema —
  > this is the hazardous inheritance from an otherwise-superseded paragraph,
  > flagged here so nobody drafts the note from it.

## 11. Testing

- Change A: a per-cell assertion that `micro` is present on **every** cell, not
  just `here` — the mutation that reintroduces `is_here` must fail it. And an
  assertion that the four axes are not all equal across a neighbourhood, which
  is the property H3 states.
- Change B: the H1 distinctness assertion at a discriminating room, plus the
  H2 agreement check against `descriptor_noun`, plus a drinkability test at a
  room the descriptor calls dry — the behaviour change in §5, stated as a test
  rather than a note.

  > **NONE OF THIS EXISTS. Change B was reverted (§5) and H1/H2 are retired
  > (§8).** There is no water-from-drainage mechanism left for an H1
  > distinctness assertion or an H2 `descriptor_noun` agreement check to run
  > against, and no drinkability behaviour change to test — the drinkability
  > test named here describes the reverted mechanism, not the shipped
  > disclosure. What actually landed in Change B's place: `dominant_corner`'s
  > coupling invariant (§5 "The coupling invariant gets the test it never
  > had", H4) and the conservation criterion H5 (§8) — both regression guards
  > on the disclosure replacement, neither a distinctness or agreement claim
  > about sub-cell water.
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

> **SUPERSEDED BY WHAT ACTUALLY SHIPPED: four decisions, not three, and
> proposal 1 as stated above is the exact claim the revert falsified.**
>
> - **0120 — an ordinal field may band a blend, a nominal field must take a
>   partition.** This is the reformulation of proposal 1, not proposal 1
>   itself: "a fine layer bands from the blend; it does not inherit a
>   category" is precisely the claim Change B's attempt 2 built, measured, and
>   falsified (§5) — `relief` (ordinal) may band safely, `water` (nominal) may
>   not. Biome is not a deliberate exception to a banding rule; it is a
>   nominal field like water, governed by the same line.
> - **0121 — an emit gate is not a grain gate.** Shipped as proposal 2 above
>   states it.
> - **0122 — disclose a resolution rather than refine a field.** Not on the
>   original shortlist. This is what replaced Change B once refining water was
>   ruled out (§5's "What replaces it").
> - **0123 — a refinement preregisters a conservation criterion.** Also not on
>   the original shortlist. This is the H1/H2-passed-yet-illegal lesson (§8)
>   made durable: a variation-only preregistration cannot detect a broken
>   global invariant.
>
> **Proposal 3 (a window may narrow a coarse fact without a save-format
> change) was dropped, not promoted.** No decision record exists for it; the
> campaign's actual decisions were about *how* a refinement may vary
> (ordinal-vs-nominal, disclosure-vs-invention, conservation-vs-variation),
> not about the window/save-format boundary this proposal named.
