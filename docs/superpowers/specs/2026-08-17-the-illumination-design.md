# The Illumination — colour, glyph, and the chart at world scale

**Status:** brainstormed, at G3. Successor to The Rhumb (merged 9aae0d27,
decision 0141), which settled the chart's *orientation semantics* before
this campaign rewrites how the chart is *drawn*.

**Decision claimed:** 0142 (board notice `6f240b0f`, posted after
`make board-sync && make board`).

**Branch:** `campaign/the-illumination`.

---

## 0. Read this first: three inherited statements are wrong

This spec's starting material is The Rhumb spec §8, six idea-registry rows,
and a brief. Every load-bearing claim in those was re-checked against the
tree before being used. Three did not survive, and each would have produced
a defect at a different depth. They are stated here rather than buried
because the registry rows and §8 still carry them, and a reader who skips
this section will re-derive them.

**(a) The surface mixture cannot be built where the design puts it.**
The Rhumb §8 and `RENDER-surface-mixture` both say to extend
`domains/terrain/src/lithology.rs::reflectance`, weighting it by cover and
by `temperature_at(cell, day)`. `temperature_at` is in `domains/climate`
(`provider.rs:323`), as is `classify_land` (`biome.rs`);
`domains/terrain/Cargo.toml` declares `hornvale-kernel` and nothing else,
and `cli/tests/architecture.rs:111::domains_depend_only_on_the_kernel`
enforces the constitutional rule. As written it is a layering violation.
§3 relocates it.

**(b) `scene/surrounds/v3` is the expensive path, and the project's own
rule prefers additive.** `clients/vessel/src/pane_chart.ts:22` allowlists
exactly `"scene/surrounds/v2"` and refuses every other tag, deliberately —
a denylist "fails open on a schema nobody anticipated". Minting v3 darkens
the vessel chart pane until every client updates in the same commit. §4
goes additive instead, which decision 0055 sanctions in as many words.

**(c) The cross-renderer agreement test cannot be held green through a
deliberate projection change.** The brief says "keep it green by fixing
renderers, never by rebaselining it". The test asserts
`grid_shape(&g) == shape_of(REFERENCE_SHAPE)`
(`clients/game/core/tests/chart.rs:149`), where `REFERENCE_SHAPE` is a
committed transcript of the *sim's own* output. A deliberate north-up
change moves that output by construction. §5.3 restates the rule in the
form that is actually satisfiable and still protective.

---

## 1. What this campaign produces

A world that is **looked at** rather than read about: a chart whose colour
means the surface a character can see, whose glyph means how hard the
ground is to cross, and whose weight means how sure the character is —
oriented to north, with the seam cells drawn for the first time.

Three stages, in this order, each independently shippable:

1. **The vocabulary and what colour means.** No wire change at all.
2. **The wire.** One batched additive change at `scene/surrounds/v2`.
3. **North-up.** The projection, the seam cells, three renderers.

**Why this order** — and it is not the order the brief proposed. The
surface mixture changes what the **existing** `color` field means, so it is
visible with zero schema risk; the premise that "neither is visible alone"
does not hold. And batching stage 2's three fields makes the harder
key-path *shape* argument (`scripts/shapecheck.py`) once instead of three
times.

The zoom ladder and the fogged world-map rung are **deferred, not dropped**
— see §10.

---

## 2. The protocol: two channels and a modulator

The brief states the rule as three peers: nominal → colour, ordinal →
glyph, epistemic → weight. Lifting it one level shows the three are not
peers, and the refinement changes an implementation decision.

Nominal and ordinal are **scale types** — they describe what was measured.
Epistemic describes **how much to trust the measurement**, and is
perpendicular to both. They also run on three different clocks:

```
  channel   encodes            sourced from              refreshes
  -------   ----------------   -----------------------   ------------
  colour    nominal (kind)     physics: reflectance      per day
                               x illuminant x eye
  glyph     ordinal (degree)   geology / slope           ~never
  weight    epistemic (trust)  the observer's history    per move
```

So the rule this campaign implements is:

> **Two encoding channels — colour carries the nominal axis, glyph carries
> the ordinal axis — and one modulator, weight, which applies to both.**

**The consequence that matters.** A remembered cell **dims its colour and
its glyph**. It does not substitute a *different* glyph. That deletes
`surrounds_ascii.rs::faded()`, whose seven substitutions
(`- , ; n a o %`) are the current epistemic encoding.
`clients/game/core/src/chart.rs:64` already maps epistemic to
`Weight::{Bold, Normal, Dim}` across the whole cell; that renderer already
had it right and is the precedent for the other two.

### 2.1 The glyph budget is already spent, and it balances at eleven

`surrounds_ascii.rs` draws exactly **eleven** base glyphs today:

```
  @  the observer          ~  ocean          _  relief 0-1
  &  an agent              =  salt basin     .  relief 2
  #  a mark                +  river          :  relief 3
                                             ^  relief 4
                                             A  relief 5+
```

plus seven memory twins, eighteen in all. Deleting the twins per §2 lands
the base set on eleven with **no glyph budget spent**. "Around eleven" is
therefore not a target this campaign aims at; it is what the vocabulary
already costs once epistemic stops consuming glyphs. A twelfth stays a
deliberate act.

This also explains an ordering constraint the brief did not state: the
proposed walk-band ladder uses `,` and `;` as ordinal rungs, and both are
*currently* memory twins of `.` and `:`. The ladder is unreachable until
epistemic moves to weight, which is why the vocabulary is one stage and not
two.

### 2.2 The ordinal a walk-band cell draws is impedance

Relief alone answers "how high", which is not what a walker needs. The
walk-band ordinal is **impedance** — how hard this ground is to cross —
absorbing both vegetation and slope into one ranked answer. The caption
names which ordinal the chart is drawing, because the same glyphs re-mean
across bands (a chamber's ordinal is not a walk band's).

The rungs and their sources are a **stage-1 design task against real
fixtures**, not fixed here: `Micro::openness` (canopy),
`Micro::relief`/`aspect` (slope proxies), the `relief` band, and the
`water` class are the available inputs, all already on the wire for every
cell. What is fixed here is the *arity* — one ordinal, one glyph per cell —
and that the caption must name it.

### 2.3 Degradation: lose a whole axis, and say so

A client that lacks a channel **loses that channel's entire axis and
declares the loss in its caption**. It does not recover the lost axis by
reallocating another channel — a monochrome client does not spend glyphs on
cover. One cell draws one glyph, on every client.

This is not a claim that the three renderers draw the *same* glyph.
Decision 0022 licenses divergence and
`clients/vessel/src/pane_chart.ts:8` invokes it explicitly: "Two renderings
of one scene for different purposes is exactly what decision 0022 licenses;
they are not expected to agree glyph-for-glyph."
`clients/game/core/src/chart.rs:52` draws one glyph for everything placed,
on purpose. What is shared is the **rule** and the **geometry** — not the
vocabulary.

The caption machinery already exists: `Sight`
(`windows/scene/src/surrounds.rs:228`) carries `channels`, `chromatic`,
`projection` and `preserves`, and
`surrounds_scene_colored_in` fills them from the observer
(`:620`). A monochrome caption is a new sentence built from fields that are
already populated.

---

## 3. Colour: a surface mixture, composed above the domain layer

### 3.1 Where it goes, and why not where the design said

`windows/locale/src/lib.rs:599`:

```rust
pub fn reflectance_at(&self, addr: &RoomAddr)
    -> Result<Reflectance, LocaleError>
{
    let geo = self.climate.geosphere();
    let weights = addr.corner_weights(geo, &self.index).ok_or(...)?;
    let cell = dominant_corner(&weights).0;
    let buffer = self.terrain.material_at(cell);
    let rock = self.terrain.rock_at(cell);
    Ok(hornvale_terrain::lithology::reflectance(&buffer, rock).integrate())
}
```

This is already the seam. It holds `self.climate` and `self.terrain`
together — legal, because `windows/locale` is a *window*
(`windows/locale/Cargo.toml` declares both) — and it **already throws the
Mixture away** on the last line. `lithology.rs:871` says the function
returns a `Mixture` rather than a `Reflectance` precisely "so a later
texture layer can arrange the components spatially instead of re-deriving
them." This campaign is that later layer.

So: **`domains/terrain` does not change.** The mineral mixture stays a
mineral mixture. The surface layer composes above it.

### 3.2 The composition

Keep the mineral mixture un-integrated, append surface cover endmembers,
weight them by cover fraction, then integrate once:

```
  final = integrate(
      mineral_mixture * (1 - covered_fraction)
    + surface_endmembers * their cover weights
  )
```

The endmembers follow the existing pattern exactly — authored `[f64;
BANDS]` curves at `BANDS = 10` (`kernel/src/color.rs:27`), tagged
`type-audit: bare-ok(ratio)`, mirroring
`lithology.rs::endmembers::{FELSIC, MAFIC, CARBONATE, IRON_OXIDE}`. The set:
**chlorophyll, litter, snow, sand, silt**.

The climate inputs already exist and are day-parameterised where the design
needs them to be:

```
  snow_fraction_at(cell)          provider.rs:413    annual
  is_frozen_at(cell, day)         provider.rs:610    seasonal
  temperature_at(cell, day)       provider.rs:323    seasonal
  moisture_at(cell)               provider.rs:401
  biome_expr_at(cell)             provider.rs:490    Realm/Formation/Stratum
```

`Variant` (`domains/climate/src/variants.rs`, 30 values) is the richest
cover signal available — `Erg` and `Hamada` are sand and rock pavement,
`Snowfield` and `ScouredIce` are snow, `ClosedCanopy` and `LianaForest` are
chlorophyll, `Burn` is litter and char. Mapping `Variant` and `Formation`
onto cover weights is stage-1 design work against real fixtures.

**The one real signature cost:** `reflectance_at` takes no `WorldTime`, so
a seasonal term requires adding one. Its caller
`surrounds_scene_colored_in` already has `at: WorldTime` in hand
(`surrounds.rs:599`), so the change is local. Every other caller of
`reflectance_at` must be found and passed a day — a grep, not a guess.

### 3.3 What this breaks, and it is a cross-tree change

Colour today means **bedrock**, and that word is load-bearing in five
places outside `surrounds_ascii.rs`, three of them outside the cargo
workspace or in committed prose:

```
  clients/vessel/src/pane_chart.ts:167,170   comments + the `ground` logic
  clients/vessel/src/pane_cell.ts:15         doc comment
  cli/src/main.rs:75                         CLI help text
  book/src/gallery/possession-seed-42.md     committed artifact
  windows/scene/src/surrounds_ascii.rs:164   the disclosure sentence
```

This is the Rhumb §11 class exactly — a rendered sentence is a cross-tree
interface, and `gate-commit` and `cargo nextest` are structurally incapable
of seeing the `.ts` consumers. **Grep `.ts`, `.mjs` and `.js` as well as
`.rs`**, and expect the `clients` phase of the stage gate to be where a
miss surfaces.

There is a second-order consequence worth naming. `terrain_glyph` returns
`(char, bool)` where the bool means "this glyph draws the ground the colour
describes" (`surrounds_ascii.rs:26`), and the colour lens **withholds the
tint** wherever it is false — because bedrock colour under a river
describes something the reader cannot see. Once colour describes the
*surface*, that withholding rule shrinks: a vegetated cell's colour is now
honest. Whether it shrinks to nothing (water still covers the ground it
would describe) is a stage-1 decision against fixtures, and the disclosure
sentence must be rewritten to match whatever it becomes.

---

## 4. The wire: additive at v2, not v3

### 4.1 The decision

Add four fields to `SurroundsCell`, keep `color`, and keep the schema tag
at `scene/surrounds/v2`:

```
  signal        Vec<f64>   per-channel response, quantized at emit
  cover         u32        index into a new nominal `cover_legend`
  bearing_deg   f64        great-circle azimuth from the observer
  distance_rad  f64        great-circle angular distance from the observer
```

`bearing_deg` and `distance_rad` are `Option`-free and present on **every**
cell including seams; `signal` and `cover` follow `color`'s existing
`skip_serializing_if` discipline so an uncoloured document's bytes do not
move.

### 4.2 Why additive rather than v3

Three reasons, in ascending order of importance.

**It breaks nothing.** `pane_chart.ts:22` refuses any tag but v2;
`clients/game/core` never checks the tag; and **no struct in the tree sets
`deny_unknown_fields`** (verified by grep across `kernel domains windows
cli clients`). So adding fields is invisible to every existing consumer,
and each renderer opts in on its own schedule. Decision 0055: "additive-or-
versioned only."

**It makes the migration control stronger.**
`RENDER-appearance-signal-protocol` proposes the control as "projecting a
v3 signal through the carried projection must reproduce the v2 `color`
byte-for-byte" — a cross-version comparison. Carrying both in **one
document** turns it into a live invariant a single test can assert on every
emitted scene, which is strictly stronger than comparing two documents.

**The cut point is a ladder, not a point.** This is the real argument:

```
  rung  what ships                     usable by             thin client must
  ----  ---------------------------    ------------------    ----------------
   1    10-band reflectance            any eye, any device   implement an EYE
   2    per-channel Signal             any device            implement a PROJECTION
   3    sRGB bytes                     any colour device     nothing
   4    nominal cover token            16-colour, autotile   nothing
   5    nothing                        monochrome            nothing
```

Rung 1 stays rejected for the settled reason: it would let a truecolour
client render a dichromat's world trichromatically, the lie
`windows/vessel/src/lens.rs:60` already refuses when it declines a hue
term for seed 42's dichromat bugbear. But each rung is derivable *downward*
from the one above. Shipping rung 2 alone forces every client to implement
optics — including a 173-line TypeScript pane. Shipping **2 and 3 together**
hands every client rungs 2–5 and lets each choose its own cut.

Carrying `color` beside `signal` is not the lie 0022 and 0003 forbid.
`color` is already post-eye and post-projection, and `sight.projection` /
`sight.preserves` name exactly what it preserves — cartography's answer,
which `kernel/src/color.rs` already adopts by name.

### 4.3 Why `cover` is categorical and must ship beside the signal

You cannot round a continuous mixture into sixteen terminal colours
meaningfully, and you cannot autotile on one at all — both are categorical
questions, and two clients would threshold them differently, producing two
disagreeing worlds from one document. `cover` is a small closed vocabulary
(a coarsening of `Variant`/`Formation`, not a copy of either — 30 values is
a legend, not a palette) with a `cover_legend` beside the existing
`biome_legend`/`water_legend`/`relief_legend`.

Note the lattice constraint on autotiling, verified: the chamber/building
plan is square 4-neighbour (`windows/vessel/src/session.rs:111` — `HEADINGS`
is orthogonal only), so classic bitset autotiling works there; the walk-band
surrounds is triangular (3 edge-neighbours plus up-parity), where square tile
masks do not apply; `scene/tiles/v1` is equirectangular, where they do. A
cover class serves two of the three lattices, and this campaign ships none
of the autotiling itself.

### 4.4 The shape argument must be made on its own

`scripts/shapecheck.py` (`make shapecheck OLD= NEW=`) answers "did the wire
shape move, or only a value?" Stage 1 moves **values only** — the easy
argument, and the same one The Rhumb made (133 key paths identical, 1 of
888 leaf values moved). Stage 2 moves the **key-path set**, which is a
different and harder argument, and batching all four fields into one commit
means making it once.

### 4.5 The kernel gains one function

There is **no great-circle distance function in the kernel**.
`RoomAddr::bearing_to` exists (`room.rs:474`); distance does not. Since
`centroid()` returns unit vectors, angular distance is small — but it is
new `pub` kernel surface, so it needs a `type-audit:` tag **on the struct's
doc comment, never on a field's**, and it drifts
`docs/audits/type-audit-report.md` in the same commit.

---

## 5. North-up, and the seam cells

### 5.1 What unblocks

`SurroundsCell` carries no position — verified, `surrounds.rs:106`: `room`,
the lattice offsets `u/v/w/up` (all `Option`, `null` on a seam), `seam`,
`state`, the three legend indices, `micro`, `marks`, `color`, and four
environmental fields that are emitted **only for the observer's own cell**.
So a client cannot project north-up: it would need `RoomAddr::unpack` +
`coord()`, which is kernel Rust, and `pane_chart.ts` is TypeScript while
`clients/game/core` carries no hornvale crate by design.

Stage 2's `bearing_deg` + `distance_rad` are computed in the sim and are
the whole unblock. Not lat/lon (that pushes spherical trigonometry into
every client) and not row/col (that bakes one renderer into the wire, which
0022 exists to prevent).

`SurroundsScene.orientation` is a `String` reading `"lattice"`
(`surrounds.rs:304`, set at `:502`) whose doc comment says "Always
`"lattice"`". It gains `"north-up"`; the doc comment is part of the change.

### 5.2 Seam cells become drawable

`chart.rs:179` skips seam cells today because their `u/v/w/up` are `null`
where the lattice bends across a base face, and inventing a coordinate
would be a worse lie than omitting them. A seam cell still carries `room`
(a `u64`, not an `Option`), so bearing and distance exist for it. The lie is
retired rather than worked around.

**Collisions become possible, and this is the highest-risk part of the
campaign.** A lattice projection is injective by construction; a
bearing/distance projection onto a character grid is not.
`chart.rs::draw`'s second pass already lets the most salient mark win its
box; extending that discipline to terrain cells is required, not optional.
Whether both projections coexist behind `orientation` or the lattice one
retires is a **stage-3 decision against fixtures**, not decided here.

### 5.3 The control, restated so it is satisfiable

`the_shape_matches_the_sims_own_ascii_render` exists in both
`clients/game/core/tests/chart.rs:150` and `tests/plan.rs:156`. It asserts
`grid_shape(&g) == shape_of(REFERENCE_SHAPE)` — **shape only**, explicitly
tolerating "this campaign's glyph vocabulary is deliberately coarser".

Two consequences:

- **Stage 1 cannot redden it.** A vocabulary change is invisible to a shape
  comparison. That is a feature: it means the control responds to the
  projection axis alone, and stages 1 and 3 do not confound each other.
- **Stage 3 must move `REFERENCE_SHAPE`.** A deliberate projection change
  moves the sim's output by construction, so "never rebaseline" is
  unsatisfiable as literally stated. The rule that *is* satisfiable and
  still protective is directional:

> Re-capture `REFERENCE_SHAPE` from **the sim's own render**, never from the
> client's output. The client's projection is written independently and must
> then agree.

Pasting `chart.rs`'s output into `REFERENCE_SHAPE` would make the test
vacuous — that is the failure it exists to prevent, and the module doc
records a plausible-looking wrong formula that once passed every other test
in its file.

---

## 6. What is unverified, and how each is settled

Each is a **stage-opening probe with a branch table**, not a prediction. No
build was run while writing this spec, so nothing below is measured.

### 6.1 Does seed 42 have a season worth seeing?

The design's headline claim is "a peak is white in winter because its
mixture changed." That needs the world to *have* a winter at the latitudes
its peaks occupy. Sample `temperature_at(cell, day)` and
`is_frozen_at(cell, day)` across a year at the highest-relief cells
reachable in a walk band.

- *Frozen fraction varies across the year* → the seasonal snow term is
  measurable; H1 proceeds as written.
- *Frozen year-round or never frozen at those cells* → the seasonal term is
  real but unobservable **on this seed**; widen the probe to a seed sweep
  before concluding, and if it is unobservable everywhere, say so in the
  chronicle and ship snow as an annual term rather than inventing a
  seasonal one nothing can see.
- *`is_frozen_at` is constant by construction* → read its implementation
  before designing against it; a constant is a scope error, not a hard
  measurement.

### 6.2 How many callers does `reflectance_at` have?

Adding `WorldTime` to its signature is a breaking change to every caller.

- *One caller* (`surrounds_scene_colored_in`) → change in place.
- *Several* → add the day-aware method beside the existing one only if some
  caller genuinely has no day to give; otherwise thread the day and delete
  the old signature. Do not add an overload to avoid an edit.

### 6.3 Does the colour actually move, and is `color` even populated?

`color` is `Option` and skipped when absent. Before asserting any colour
change, confirm a coloured document is what the committed fixtures contain.

- *Fixtures carry `color`* → the value-only shapecheck argument applies.
- *Fixtures are uncoloured* → stage 1 is invisible in those fixtures, and
  the campaign needs a coloured fixture to measure at all. That is a task,
  not a surprise.

### 6.4 Do collisions actually occur under north-up?

Project a real walk band by bearing and distance at the shipped radius and
count cells landing in an occupied box.

- *Zero collisions at this radius* → the collision rule is still written,
  but as a guard with a test that forces one, never as dead code trusted to
  be unreachable.
- *Collisions occur* → extend `chart.rs::draw`'s salience discipline to
  terrain and pin the tie-break deterministically (`total_cmp`, then a
  stable key — no `HashMap`).

### 6.5 The recycled-worktree hazard, already paid once

`make worktree-take` printed a freshness warning for this campaign's
worktree, and **its first suggested fix does not work**: re-running exits
at `worktree-take.sh:43-46` before the invalidation step. The same sentence
(line 149) offers a **second** fix — force a rebuild of the affected crates
— and that one is sound. So the hazard is the **asymmetry**: a reader
taking the dead clause does nothing and *believes they have acted*, and the
resulting failures read exactly like a red main, as lines 146-147 say.

**A second, latent defect, measured rather than reasoned about.** Line 135
is `echo "$hits" | sed 's/^/  /' | head -10`, under this file's
`set -euo pipefail` (line 38). When `head` exits after ten lines and `sed`
still has data to write, `sed` takes SIGPIPE and returns 141; `pipefail`
propagates it and `set -e` kills the script **before the explanation and
the fix line print at all**. Remediation becomes unreachable exactly when
contamination is worst. Measured on the identical construct: 359 paths
(~42 KB) exits 0 and continues; 700 (~83 KB) exits 141 and does not —
consistent with a 64 KiB pipe buffer. Fix is one character of plumbing
(`sed 10q`), not a redesign.

**It did not fire here, and the reason is a correction worth keeping.** An
earlier draft of this section said 756 contaminated files. That is not the
script's finding — it is a `grep -rl` over *every* file under `deps/`,
including the `.rlib`/`.rmeta`/`.d` entries the script deliberately
excludes. By the script's own criterion (`find -maxdepth 1 -type f -perm
-u+x`) the count is **361**, about 42 KB, under the threshold. Verified by
re-running: full trailer printed, exit 1.

**Force a rebuild before trusting any red** — these failures read exactly
like a red main and are not one. The working invalidation, applied here
(52 files touched): `grep -rl` for `CARGO_MANIFEST_DIR`,
`CARGO_TARGET_TMPDIR`, `CARGO_BIN_EXE_` and `touch` the hits.

Board technique `dd9135db` is the settled account; it supersedes
`e7c5bcb8`, `5fd03dec` and `9370de8a`. Note for a reader tempted by the
obvious inference: running the checker by hand *without* an argument
returning "clean" is **not** a defect — its usage header (lines 5-20)
documents that fallback and names its resolution.

**Force a rebuild before trusting any red** — these failures read exactly
like a red main and are not one.

---

## 7. Preregistered measurement

Frozen before the code that would move it (decision 0016). A falsified
prediction is a finding; §7.1 of The Rhumb's spec is the model for how one
gets recorded.

**H1 — The surface mixture increases the number of distinguishable colours
in a walk band.** Colour today is bedrock, so cells differing only in cover
are identical. Preregistered as a **count**, on a fixed seed-42 walk band at
the shipped radius: the number of distinct quantized `color` values strictly
increases.

- Floor: strictly greater than the bedrock baseline count.
- Ceiling: **not equal to the cell count.** If every cell gets a unique
  colour, the mixture is tracking address noise rather than cover, and the
  result is a defect dressed as a success.

**H2 — The seasonal term is observable, and it is seasonal.** At the
highest-relief land cell in that band, the distinct-colour count sampled at
four evenly spaced days of one year is **greater than one**, and the day of
peak lightness falls in the cold half of that cell's year.

The second clause is the falsifiable half. A colour that varies across the
year but peaks in the *warm* half is not a snow term; it is something else,
and finding that out is the point of stating it. Contingent on §6.1.

**H3 — Every client keeps its axes, or declares their loss.** For each of
the three renderers, the number of *distinct rendered cells* under a
monochrome caption is strictly less than under colour, and the caption
names the lost axis. This measures the degradation rule (§2.3) rather than
asserting it.

**Not a hypothesis, an invariant:** projecting a cell's `signal` through
the observer's carried projection reproduces that cell's `color`
byte-for-byte, on every emitted scene. That is §4.2's migration control and
belongs in a test, not here.

---

## 8. Costs and artifacts

A rendering change is an artifact change. There are **three** regeneration
paths here, not the two the brief names, and the third is the one that
bites.

1. `make rebaseline` → `scripts/regenerate-artifacts.sh`. Covers the
   transcripts, gallery charts, `docs/audits/`, `docs/digest/`, and
   `clients/game/core/tests/fixtures/`. Diff the paths
   `docs/generated-paths.txt` declares (15 lines) — **that file is the
   list**, not this prose.
2. `make rebaseline-goldens` → `REBASELINE=1` against a **hardcoded list of
   eight test targets** (`Makefile:596-604`). The three this campaign
   touches are `hornvale-scene --test golden`,
   `hornvale-vessel --test session_snapshot`, and
   `hornvale --test lens_purity`.
3. **Neither covers a golden that is not already in one of those lists.**
   If a stage adds a new byte-golden fixture, it must be added to
   `rebaseline-goldens`' list in the same commit, or it is regenerated by
   nothing and drift-checked by nothing.

The Rhumb's plan named only the first and left its branch red.

Branch table, not a prediction:

- *Possession transcripts and gallery charts move* → expected at every
  stage; commit in the same commit as the change.
- *`clients/game/core/tests/fixtures/` moves* → expected at stages 2 and 3,
  since the committed session snapshots embed `SurroundsScene`. Review the
  chart by eye before accepting.
- *`docs/audits/` moves* → a `pub` boundary changed (certain at stages 2
  and 3). Regenerate the type-audit report in the same commit.
- *`docs/digest/` moves* → the decision index drifts when 0142 lands.
  Regenerate, never hand-edit.
- *`book/src/domesday/` or a census CSV moves* → **STOP.** Nothing here
  should reach a census. That is a signal the change leaked into a domain —
  and given §0(a), it is the specific failure this spec's first correction
  exists to prevent.

**No epoch.** No new stream label, no new draw, no world moves. Every
change is at or above the locale window, and `domains/terrain` is not
edited at all.

---

## 9. Testing

- The three renderers agree on **geometry**, via
  `the_shape_matches_the_sims_own_ascii_render` in both
  `tests/chart.rs` and `tests/plan.rs`, under §5.3's directional rule.
- The signal/colour invariant (§4.2), asserted per-cell on an emitted scene.
- A monochrome observer's caption names the lost axis, and its render
  spends no glyph on cover (§2.3) — the assertion that keeps the
  degradation rule from decaying into prose.
- Navigation is untouched. Note for anyone carrying it over from The
  Rhumb's spec: `compass_variants_must_all_be_rostered`
  (`windows/locale/src/lib.rs:1424`) is **not a test** — it is a total
  function over `Compass` whose exhaustiveness the compiler enforces, so it
  cannot "stay green"; it either compiles or it does not. It is named here
  only so the next reader does not go looking for a test that is not there.
- `two_eyes_on_one_world_disagree_about_colour`
  (`windows/vessel/src/purview.rs:232`) stays green through the mixture
  change — it is the existing pin that the eye, not the palette, decides
  colour.
- Seam cells are drawn, asserted on a band known to contain one.

### 9.1 Every test this campaign writes is invisible to `gate-commit`

`scripts/subfloor-roster.sh` states its own direction in its header: "it
selects *tests present in the roster*. A test ABSENT from the roster is not
selected — the exclude-unknown rule. That is deliberate (a new test enters
on the next green stage gate) and it means this script can never be read as
'the commit gate covers everything new'."

So a green `make gate-commit` immediately after writing a test says
**nothing about that test**. It was compiled and not run. Run new tests
directly — `cargo test -p <crate> --test <name>`.

**This is not hypothetical.** The Begat (landed 2026-08-17) reported a
green `gate-commit` over 1,281 tests with **zero `hornvale-hearsay::`
lines**; the equivalence oracle that was the entire correctness argument
of its change ran only because it invoked `cargo test -p hornvale-hearsay`
by hand. A byte-identity thesis, and its oracle was compiled and not run.

**It is a one-boundary lag, not a standing condition.** The chamber's
`gate` phase now rewrites the roster and commits it with the merge product.
Verified by tracing the file rather than taking it on report:

```
  894b482e  2026-08-14  rows=2748  hearsay=0   <- hand-authored, the only one
  cd8b7d06  2026-08-15  rows=2843  hearsay=32
  f905923a  2026-08-16  rows=2904  hearsay=48
  068d1b8c  2026-08-16  rows=2919  hearsay=48
  b60af966  2026-08-17  rows=2921  hearsay=48  <- current
```

Five automated rewrites across three days, so the mechanism is working
repeatedly rather than once. **Consequence for this campaign:** each stage's
tests enter the roster at that stage's gate, so by stage 3 the stage-1 tests
are in the commit gate. Plan for one boundary of lag, not permanent exile.

**Give each stage boundary a falsifiable read-out.** Name the tests you
expect the roster to pick up *in advance*, then check them **by name** in
the roster commit afterwards. The weaker question — "did a roster commit
appear?" — passes even when the mechanism records a subset, which is the
same vacuous-green shape this section exists to warn about.

**A row is not a test.** One row may select more than one, so the roster's
row count and the tier's executed-test count legitimately differ. Do not
read a shortfall from that gap. Counts here are **non-comment, non-blank
lines at `b60af966`**: 2,921 total, of which `hornvale-scene` 28,
`hornvale-locale` 26, `hornvale-vessel` 352. The criterion and the ref are
stated because an earlier draft of this section carried "2,748 rows" — true
at `894b482e` on 2026-08-14, six commits stale by the time it was written
down, and taken on report rather than measured.

---

## 10. Out of scope, carried forward

- **The zoom ladder.** Session-remembered zoom, `-`/`+`/`=`, look-only
  above the default scale, and the snapshot channel's hardcoded
  `purview(0)` (`CLIENT-snapshot-chart-cannot-zoom`). Look-only is a
  *refusal*, not a scheduler: `self.day` is assigned in exactly one place,
  `Session::wait` (`session.rs:2749`, verified by grep) — so refusing
  time-advancing verbs at non-default scale needs no scheduling machinery.
- **The fogged world-map rung** (`RENDER-fogged-world-map-rung`). The
  schema half is free; the ASCII equirectangular renderer does not exist,
  and that is the whole cost.
- **Autotiling** (Compendium item 3.1, Nice Walls with Bitsets). `cover`
  makes it possible on the square and equirectangular lattices; this
  campaign ships none of it.
- **A bias-correcting rhumb resolution** — The Rhumb §7.1's follow-up. A
  different algorithm, not this campaign.
- **`CLIENT-glyphs-22-rejected` stays rejected.** Twenty-two nominal biome
  glyphs failed on legibility; nothing here reopens it, and §2 is the rule
  that failure discovered without naming.

---

## 11. Decision to promote

**0142**, claimed after `make board-sync && make board` (§0 header).

> **A rendering channel carries one measurement axis, and a client that
> lacks the channel loses the axis and says so.** Colour carries the
> nominal axis, glyph the ordinal, and weight modulates both with the
> observer's confidence. A client may not recover a lost axis by
> reallocating another channel. The wire ships adjacent rungs of the
> signal-to-pixel ladder — the post-eye signal and the sim's own projection
> of it — so that each client chooses where to cut without any client being
> required to implement optics.

---

## 12. Task outline

**Stage 1 — the vocabulary and what colour means** (no wire change)
1. Probe §6.1, §6.2, §6.3 against a running possession; record real output.
2. Surface endmembers and the cover-weighting in `windows/locale`;
   `reflectance_at` gains a day.
3. Epistemic moves to weight in `surrounds_ascii.rs`; `faded()` deleted;
   the impedance ladder (§2.2) designed against fixtures.
4. The bedrock→surface sweep of §3.3's five sites, `.ts`/`.mjs`/`.js`
   included; the disclosure sentence rewritten.
5. H1, H2, H3. Rebaseline both ways (§8).

**Stage 2 — the wire** (one additive shape move)
6. `signal`, `cover`, `bearing_deg`, `distance_rad` + `cover_legend`; the
   kernel's distance function and its type-audit tag.
7. The signal/colour invariant test; `make shapecheck` argued on its own.

**Stage 3 — north-up**
8. The projection, the collision rule (§6.4), seam cells, `orientation`
   gains `"north-up"`, all three renderers, `REFERENCE_SHAPE` re-captured
   per §5.3.
9. Artifacts, chronicle, retrospective, book freshness sweep, close.

Absorb main at every stage boundary via `make sluice-stage BRANCH=… REF=<full-sha>`.

### 12.1 Two queue requirements that postdate The Rhumb

Both verified against `origin/main` at `3c91bb3b`, not taken on report.

- **A merge submission needs a `Sluice-Headline:` trailer.** Landed as
  `58feb338`. `scripts/sluice-request.sh:142` refuses at the mouth without
  one, in milliseconds, before the box is taken. Three properties, each
  load-bearing: it need not be the last commit and a later commit does not
  displace it; it must sit **adjacent to other trailers with no blank line
  between**, because the check reads git's trailer parser and that only
  sees the message's final block; and it carries **no `merge(...)` prefix**
  — the chamber composes `merge(<branch-leaf>): <text>`, which is what
  fixes the doubled headline The Rhumb landed permanently.
  A **`sluice-stage` request is exempt** — `sluice-request.sh:118` guards
  the check with `kind = "merge"`, because a stage merge commit is
  discarded and no subject it carries can become permanent.
- **`make shellcheck` now runs in the `outboard` phase** of every stage
  gate and merge (`scripts/lane-outboard.sh:80`). It had been in `.PHONY`
  and nowhere else, so nothing ran it. Relevant here only if the
  `TOOL-worktree-take-remedy-is-a-no-op` followup is taken up inside this
  campaign, since that fix edits `scripts/`; run it locally first if so.
