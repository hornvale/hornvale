# The Beholding — design

**Status:** spec, awaiting G3 review.
**Date:** 2026-08-07
**Campaign:** The Beholding — colour reaches the possession panes, through
the possessed agent's own eyes.
**Measured on:** `9c1c8b1e` (main at branch point).

## 1. The question

The Pigment built a real spectral colour model — reflectance × illuminant ×
observer, ten bands, an `Observer` whose channel count is a `Vec` precisely
so it need not be four — and proved it on one producer and both consumer
poles. It deliberately stopped one step short:

> The Pigment ships a `StandardObserver` … and the *slot* for others. It
> does not yet wire `PerceptionVector` into that slot; that is campaign 2.
> — Pigment spec §3.4

This is campaign 2, and it arrives with a ruling that fixes its shape:

> **Possessing a bugbear means seeing the world in bugbear colours.**

Everything else follows. Colour is a property of the *relationship* between
an observer and a cell, not of the cell. The calculation happens in Hornvale
and is emitted, so no client re-implements it. A toggle selects the default
human observer against the possessed agent's own.

The reason this is not a small wiring job is one line in the kernel:

```rust
pub fn to_srgb(&self, signal: &Signal) -> Option<[u8; 3]> {
    if !self.srgb_native || signal.get().len() != 4 { return None; }
```

**A non-human observer has no truthful sRGB image, and the kernel refuses to
invent one.** That refusal is correct and stays. Possessing a bugbear
therefore requires the thing the Pigment named and deferred: a *declared*
false-colour mapping, whose honesty lives in the caption rather than the
picture (RENDER-9).

## 2. What exists today (measured, not assumed)

**Shipped and load-bearing.**

- `hornvale_kernel::color` — `Spectrum`/`Reflectance`/`Illuminant`/
  `Observer`/`Signal`/`Mixture`, `standard_observer()`, `Signal::distance_to`.
  The hot path is multiply-and-add only and is bit-identical by IEEE-754;
  the one transcendental (`encode_srgb_byte`) routes through `math::powf`.
- `LocaleContext::reflectance_at(&RoomAddr)` → the cell's **bedrock**
  lithology reflectance (`windows/locale/src/lib.rs:351`).
- `surrounds_scene_colored_in(...)` → `SurroundsCell.color: Option<[u8;3]>`,
  `#[serde(skip_serializing_if = "Option::is_none")]`.
- `surrounds_ascii.rs`'s `colour` lens, with the discipline this campaign
  inherits: the bedrock colour is withheld wherever the glyph is drawing
  something other than the ground (the observer, a mark, water).
- `illuminant::daylight(&Star)` and `illuminant::at_elevation(&Illuminant,
  f64)` (`domains/astronomy/src/illuminant.rs:44,75`).
- `Calendar::solar_altitude_at(StdDays, latitude)`
  (`domains/astronomy/src/calendar.rs:657`).
- `pack_depths(&PerceptionVector) -> PackDepths`
  (`windows/worldgen/src/lib.rs:4306`) and its companion
  `perceptual_reason`. `hue = 2 + ((1 - night_vision) * 3).round()`.
- Seven authored hue exemplar reflectances
  (`domains/language/src/exemplars.rs`).

**Missing, and why each is not trivial.**

1. **The possession never asks for colour.** `purview_scene`
   (`windows/vessel/src/purview.rs:86`) calls the *uncoloured*
   `surrounds_scene_in`, and the `map` verb hardcodes the `terrain` lens
   (`session.rs:2617`).
2. **There is no per-species observer anywhere.** `standard_observer()` is
   the only producer in the workspace. `PerceptionVector` carries
   `activity`, `night_vision`, `sky_attention` — no colour-vision axis.
3. **A non-standard observer projects to nothing.** `to_srgb` returns `None`
   for every observer but the standard one, so the naive wiring renders a
   bugbear's world grey — indistinguishable on screen from a build with no
   colour layer at all.
4. **The panes cannot carry colour.** `chartRows`/`planRows` return
   `string[]`; `main.ts` does `map.textContent = rows.join("\n")`.
5. **The chamber band has no fabric.** `CellKind::Wall` is documented as
   "the building's fabric" and carries no material, no lithology, no source.
   `PaletteEntry` is `{ kind, chambers }`.

**Two things the session prompt asserted that measurement contradicts.**

- **`scene/surrounds/v2` is not a cross-repo contract.** `world-wasm`
  exports `hw_scene_{system,moons,neighbors,tiles,tiles_selected,
  tiles_region,eclipses}` and nothing else; `grep -rn surrounds
  clients/world-wasm/src/` is empty. Every consumer of the schema is
  in-repo and moves in one commit. The schema question is therefore an
  ordinary additive-field question.
- **No committed artifact is currently coloured.**
  `scripts/regenerate-artifacts.sh` passes no `--color` on any of its four
  surrounds invocations, and `grep -c '"color"'` on the gallery JSON and all
  four vessel fixtures returns 0.

## 3. The measurement that reshaped this spec

Before writing "a bugbear confuses red and green" into a spec, a probe
measured whether a candidate derivation differentiates the axis at all. It
needs no world build — only the seven authored exemplars.
(`windows/worldgen/tests/beholding_probe.rs`, promoted into the campaign as
its calibration test.) Three findings, all of which would otherwise have
entered this document as assertions.

**M1 — a derivation keyed on `pack_depths`' hue *tier* cannot tell its own
species apart.** Hobgoblin (0.6), bugbear (0.7) and kobold (0.9) all land on
hue depth 3 or 2, and the first candidate gave all three the byte-identical
swatch set — `red` = `(146,146,64)` for every one. Making the merge
*continuous* in `night_vision` separates them: `(157,157,64)` /
`(155,155,64)` / `(149,149,64)`. **A tiered eye derived from a tiered gate
is a model with fewer species than the roster.**

**M2 — `Signal::distance_to` is the wrong discriminability metric.** It is
dominated by brightness: for a *full trichromat* the probe put red/green at
0.025 relative separation, nearly the closest pair in the set. Signal
magnitude carries luminance and luminance swamps hue. The metric that
measures what an observer swap is claimed to move is **chromaticity** —
each channel over the signal's own total.

**M3 — the falsification. The candidate dichromat does not confuse red and
green.** On chromaticity, human and bugbear red/green separation are
indistinguishable (~0.025 vs ~0.026); red/green does not appear among the
eight pairs the bugbear loses most. The cause is specific: the retained
**scotopic channel carries the distinction** — green's exemplar peaks at
520 nm, exactly the rod peak — and `Observer::sense` treats every channel
alike, so a "dichromat" that still has a rod is trichromatic to any metric
that counts all its channels.

Real dichromats have rods. What makes them dichromats is that a single
achromatic channel's signal cannot be told apart from intensity, so it
contributes no hue. **The model has no way to say that, because `Observer`
has no notion of a channel's role.** The shipped `to_srgb` already assumes
one — its own comment reads *"the scotopic channel carries no hue and is not
projected"* — but the assumption is hardcoded to one observer instead of
being something any observer can declare.

**So channel roles are the enabling change**, and they also settle how a
projection is chosen. A projection cannot be derived from peak wavelength:
ranking the standard observer's channels by peak gives long (600), medium
(560), scotopic (520), short (440), so a "three longest peaks" rule would
feed the **rod** into blue and contradict the shipped native mapping. With
roles declared, the question does not arise.

**What this evidence does *not* support, and the spec will not claim:** that
a bugbear's map looks dramatically unlike a human's. Measured, the
difference is a hue shift (human `red` `(160,127,64)`, orange-brown →
bugbear `(155,155,64)`, olive) plus the loss of the red–green axis in the
projection. The preregistered claims in §6 are sized to that evidence.

## 4. The model

### 4.1 Channel roles (kernel)

```rust
/// What a channel contributes to sight.
pub enum ChannelRole {
    /// Carries hue. A projection may read it; chromaticity counts it.
    Chromatic,
    /// Carries brightness only. A real eye has such channels (rods), and
    /// their signal cannot be told from intensity — so no projection reads
    /// one and no chromaticity metric counts one.
    Achromatic,
}
```

`Observer` gains `roles: Vec<ChannelRole>`, one per channel.

- `Observer::new(channels)` — unchanged behaviour: every channel
  `Chromatic`, **no projection**, so `to_srgb` still returns `None`. This is
  what keeps `a_four_channel_synthetic_observer_still_has_no_srgb_image`
  (an explicitly mutation-proved test) green and meaningful.
- `Observer::with_roles(channels, roles, projection)` — the new constructor.
  Validating: equal lengths, at least one `Chromatic` channel, and every
  index the projection names must be `Chromatic`.

### 4.2 The projection (kernel)

Named after the discipline that already solved this problem. Every map
projection is a lie, and cartography's answer is not to find a true one — it
is to **name the projection on the map and say which invariant it
preserves**.

```rust
/// A named way of putting a signal on a three-channel screen, and what it
/// preserves.
pub struct Projection {
    /// The registered name, e.g. "native" or "yellow-blue".
    name: &'static str,
    /// What survives the projection. The caption's load-bearing half.
    preserves: &'static str,
    /// Which channel drives R, G, B. Every index names a Chromatic channel.
    rgb: [usize; 3],
    /// Per-channel normalizer: the response a unit-reflectance surface
    /// under a unit illuminant gives on that channel.
    norms: [f64; 3],
}
```

**`norms` is carried, not derived — this is a byte-identity requirement, not
a style choice.** `standard_observer`'s existing normalizers are the
*rounded* channel sums (`SHORT_NORM = 1.98`, and
`standard_observer_channels_sum_to_the_declared_norms` asserts agreement
only to two places). Deriving them live from the curves would change the
emitted bytes of every colour the standard observer has ever produced.
`standard_observer()` declares a `Projection` carrying exactly today's three
constants, so its output cannot move.

`to_srgb` consults `self.projection` instead of the `srgb_native` flag. The
flag is deleted; `native` is now simply the name of the standard observer's
projection, which is the same demotion The Lens performed on `natural`.

### 4.3 Chromaticity (kernel)

```rust
/// Each CHROMATIC channel's share of the chromatic total.
pub fn chromaticity(&self, signal: &Signal) -> Vec<f64>;
/// Squared distance in chromaticity space. The discriminability metric.
pub fn chromatic_distance(&self, a: &Signal, b: &Signal) -> f64;
```

Shipped as one named function rather than re-invented per test, because M2
shows the obvious alternative silently measures brightness.

### 4.4 The observer a species implies (`windows/worldgen`)

Lives beside `pack_depths`, which derives a perceptual gate from the same
vector at the same seam and already carries the authored model card this one
extends.

```rust
pub fn observer_for(p: &PerceptionVector) -> Observer;
/// Word why this species sees as it does — the companion `perceptual_reason`
/// already is for the lexicon.
pub fn ocular_reason(p: &PerceptionVector) -> String;
```

**Model card.**

| `pack_depths(p).hue` | who | eye |
|---|---|---|
| 5 | human (0.15) | the standard observer, unchanged |
| 4 | goblin (0.50) | anomalous trichromat: medium and long pulled partway together |
| 3 | hobgoblin (0.60), bugbear (0.70), gnoll (0.75) | dichromat: medium and long merged |
| 2 | kobold (0.90), the three dragons (0.90) | dichromat, merged harder |

Channel *count* is read off `pack_depths`' own hue ladder, so the eye and
the lexicon cannot disagree by construction — a species that lacks a word
for green lacks the channel that would distinguish it. The *degree* of
merging is continuous in `night_vision` (M1).

**The formulas, stated so they are transcribed rather than invented**, with
`S`, `M`, `L`, `R` the standard observer's four authored curves:

```
hue 5  ->  channels [S, M, L, R]                     (identical to standard)
hue 4  ->  M' = (M + (M+L)/2) / 2                    both pulled halfway
           L' = (L + (M+L)/2) / 2                    toward their mean
           channels [S, M', L', R]
hue<=3 ->  t = clamp((night_vision - 0.5) / 0.5, 0, 1)
           C = (1 - t)*L + t*(M + L)/2               one merged channel
           channels [S, C, R]
```

`t` is 0.2 for hobgoblin, 0.4 for bugbear, 0.5 for gnoll, 0.8 for kobold and
the dragons — which is what separates species that share a hue tier.

Roles are `[Chromatic, …, Achromatic]`: the scotopic channel is always
present, always last, and always achromatic. Its **gain** scales with
`pack_depths(p).luminance`, which matters for naming under low light and, by
construction, can never affect hue — an achromatic channel is read by no
projection and counted by no chromaticity metric.

Projections: hue 5 and 4 declare `native`-shaped mappings (`rgb = [2,1,0]`);
hue ≤ 3 declares **`yellow-blue`** — `rgb = [1,1,0]`, the merged channel
driving both red and green, preserving *"the short-to-long opposition; the
red–green axis is not carried"*. Every dichromat triple therefore has
`R == G`, which is not an artifact to be smoothed away: it is what a
two-chromatic-channel colour space honestly looks like on a three-channel
screen, and it is exactly what the caption declares.

The authored reasoning is the one `pack_depths` already states: a species
that sees well in the dark spent less of its evolutionary history straining
at daylight hue distinctions. This spec adds only that the trade is
*physical* — rod-dominant sight and a compressed long-medium separation are
the same adaptation seen from two sides.

**The positive control this design buys.** `observer_for` applied to the
human row must reproduce `standard_observer()` **exactly**, byte for byte.
The standard observer stops being a privileged base case and becomes what
one row of the roster derives to.

### 4.5 The declaration on the wire (`windows/scene`)

A client cannot caption what it cannot see, and RENDER-9 makes the caption
the load-bearing honesty. `SurroundsScene` gains one field, appended after
`legend` (an append is not a reorder), skipped entirely when absent so
uncoloured documents stay byte-identical:

```rust
#[serde(skip_serializing_if = "Option::is_none")]
pub sight: Option<Sight>,
```

```rust
pub struct Sight {
    /// Whose eyes: a species name, or "standard".
    pub observer: String,
    /// How many channels those eyes have.
    pub channels: u32,
    /// How many of them carry hue.
    pub chromatic: u32,
    /// The projection's registered name.
    pub projection: String,
    /// What the projection preserves.
    pub preserves: String,
    /// The sun's altitude, degrees, that lit these colours.
    pub sun_altitude_deg: f64,   // quantized at emit
}
```

`surrounds_scene_colored_in` stops computing its own light and takes an
`&Illuminant` from the caller — which is what its own doc invites (*"A
caller with an elevation in hand should say so — `at_elevation` is the
seam"*). The scene stops knowing about the star, and the "which light"
question moves to the composition point where the answer is known.

### 4.6 The possession (`windows/vessel`)

```rust
pub enum Eyes {
    /// The possessed agent's own species.
    Own,
    /// A NAMED observer: any row of `perception_registry()`, or "standard".
    Named(String),
    /// Decline the observer step entirely.
    Off,
}
```

`Session` holds one, defaulting to `Own`; `PossessOpts` gains the same field
so the CLI and tests can pin it.

**Named, not a three-value toggle.** The obvious design was
`Eyes { Own, Human, Off }`, and it is wrong on two counts. It **ages badly**:
the Pigment already designed the accessibility path as *"an `Observer` with
a shifted or absent channel — the same code path as a goblin"* (§7), so
every colour-blindness observer would need another variant of a closed enum.
And it makes the campaign's own claim **hard to see**: which species you
possess depends on the seed's flagship, so comparing two eyes would mean two
possessions in two worlds — varying the world in order to demonstrate that
only the observer varies. With names, `eyes kobold` / `eyes human` /
`eyes own` change the map in one world, one room, one hour, holding
everything constant but the eye. That is the campaign's thesis, made
operable in three keystrokes, for the cost of one registry lookup and an
unknown-name error arm that lists the roster.

- **Light**: `at_elevation(daylight(star), calendar.solar_altitude_at(day,
  latitude))`. The vessel already holds a `Calendar`, built at `start` for
  the NPC wake cycle's real-sun read.
- **Observer**: `observer_for(agent.perception)` under `Own`,
  `standard_observer()` under `Human`.
- `Eyes::Off` declines the observer step entirely — the Pigment §7
  mechanism, shared by screen readers and `NO_COLOR`, and the state in which
  output is byte-identical to today's.
- The `map` verb renders the `colour` lens whenever eyes are not `Off`.
- **A new verb, `eyes`**: bare reports whose eyes, the arity, and the
  projection's caption; `eyes <species>` / `eyes standard` / `eyes own` /
  `eyes off` switch. An unknown name fails loudly and lists the roster —
  generation never guesses.

`PaletteEntry` gains `color: Option<[u8;3]>`, additive and `None` for every
entry this campaign (§8).

### 4.7 The panes (`clients/vessel`)

`chartRows`/`planRows` become `chartCells`/`planCells`, returning a grid of
cells rather than strings:

```ts
interface PaneCell { glyph: string; color: [number, number, number] | null }
```

Not parallel arrays. `plan.rs`'s own module doc already rejected that shape
one layer down — *"a per-cell string would carry exactly one attribute, so
every later attribute would become another array to keep length-synced with
the grid"* — and the attributes it names as coming (a colour triple, an
occupant's `EntityId`, a temperature) are the same ones coming here.

`main.ts` builds **one `<span>` per run of like-coloured cells**, so node
count stays near today's rather than multiplying by the cell count every
turn. Each span is `createElement` + `textContent`, never `innerHTML`:
`pane_plan.ts:128` already draws a **sim-authored** character
(`mark.noun.charAt(0).toLowerCase()`), so a settlement or creature name
containing `<` must never reach the parser.

The pane shows the caption from `sight` beneath the map.

**Colour is withheld where the glyph is not drawing the ground.** The
bedrock reflectance is a truthful claim about the cell only when the cell's
glyph *is* that ground; `surrounds_ascii.rs` already draws this line and the
panes adopt it rather than tinting a river with the colour of the rock
beneath it.

## 5. Determinism and epoch analysis

**No epoch is owed.** Colour is derived, never committed (Pigment decision
1): the ledger holds mineralogy and stellar mass, and nothing in this
campaign draws from a stream or adds a seed label. `git grep` for new
`streams.rs` constants in the plan must come back empty.

| change | verdict |
|---|---|
| `ChannelRole`, `Projection`, `chromaticity` | additive kernel API; no existing output moves |
| `Projection.norms` carried, not derived | **required** to hold standard-observer bytes (§4.2) |
| `srgb_native` → `projection: Option<_>` | internal; `Observer::new` keeps returning `None` from `to_srgb` |
| `SurroundsScene.sight` | additive, skipped when absent → uncoloured docs byte-identical |
| `PaletteEntry.color` | additive, `None` this campaign → plan docs byte-identical |
| `surrounds_scene_colored_in` takes `&Illuminant` | signature change, two call sites, both in-repo |
| purview colours by default | **vessel fixtures move** — re-pinned in the drifting commit |

The compute path stays exact. `sense` and `integrate` are multiply-and-add
over fixed-size arrays in fixed order, which IEEE-754 requires to be exact;
`chromaticity` is division and subtraction, likewise. The only transcendental
remains `encode_srgb_byte`'s `math::powf`, and `at_elevation`'s `sin`/`exp`,
both already routed through `kernel::math` (decision 0041). Colour lands as
`[u8; 3]`, integral, so quantize-at-emit is satisfied; `sun_altitude_deg`
goes through `quantize_serde`.

**Artifacts that move, and that do not.** The four vessel fixtures move
(purview colours by default). The gallery scene JSON and the three committed
ASCII charts do **not** — they go through `cli/`'s uncoloured path. The
type-audit report moves (new pub-boundary primitives).
`book/src/reference/scene-surrounds-v2.md` is **hand-authored**, not
generated, so `sight` must be documented there by hand.

## 6. Preregistered claims

Frozen before the code that would move them. Each names the metric, and each
carries an anti-vacuity requirement: **the test must first assert that its
probe discriminates**, because a colour assertion that cannot tell
`Withheld` from `Native` passes green on a grey map.

**H1 — the model resolves the axis it reads, and only that axis.** For every
pair of species with **distinct `night_vision`**, the derived observers
produce different sRGB triples on at least one of the seven hue exemplars.
*Falsified if any such pair collides* — which the first candidate model did
for hobgoblin/bugbear/kobold (M1), so this claim has already failed once and
is not decoration.

Its second half is the honest converse, asserted rather than left implicit:
**species sharing a `night_vision` derive the identical eye.** Kobold and
all three chromatic dragons sit at 0.90, and `PerceptionVector`'s other two
axes — `activity` and `sky_attention` — do not enter the eye model. A black
dragon and a kobold see the same colours. That is a *stated consequence* of
the model reading one axis, not an accident, and it is the honest place to
hang a future campaign that gives the clade its own eye.

**H2 — the human row is not privileged.** `observer_for` applied to human's
`PerceptionVector` equals `standard_observer()` exactly, including its
projection and its emitted bytes.

**H3 — dichromacy is real once roles are declared.** With the achromatic
channel excluded, a dichromat's chromatic separation of the `red` and
`green` exemplars is strictly less than a trichromat's.
**On today's evidence this claim is FALSE** (M3 measured them
indistinguishable *with* the rod counted). The campaign tests whether roles
repair it. **If it still fails, the null ships as the headline**: the model
produces species that see differently but not species that are colour-blind,
and the spec is wrong rather than the constant needing a retune.

**H4 — the light moves the colour.** For a fixed seed, room and observer,
the emitted triples at a low sun altitude are redder (higher R:B ratio) than
at high sun. *Falsified if quantization to `u8` eats the effect at the
altitudes a real day produces at the flagship's latitude* — a live risk,
since `at_elevation`'s attenuation is gentle near the zenith.

**Seeds, not a seed.** Every claim that needs a world sweeps seeds and fails
loudly if none qualifies. Seed 42 alone has given four wrong readings in
this project's history.

## 7. Testing

- **Kernel**: role validation; a projection naming an achromatic channel is
  rejected; `Observer::new` still yields no sRGB image at arity four (the
  existing mutation-proved test, unchanged); `standard_observer`'s bytes are
  pinned against a literal so the `norms` refactor cannot move them.
- **Mutation, and it must prove it mutated.** Each guard's test asserts the
  target text exists before substitution, and records which assertion the
  mutation is supposed to kill — a mutation that reddens at an *earlier*
  assertion proved nothing (The Sighting, round 3).
- **Positive control, mandatory.** The bugbear-vs-human test asserts the two
  paths produce *different* triples on a named cell, not that either is
  non-`None`.
- **Negative control's positive control.** `Eyes::Off` must produce output
  byte-identical to a pre-campaign build — and the same test must show the
  `Own` path producing colour, or "suppressed everything" passes green.
- **Client**: pane tests over the cell grid; a mark noun containing `<` must
  appear as text and create no element; a run-length test pinning span count.
- **Enumerate by what a site EMITS.** Every surface that *renders* a cell
  becomes a colour consumer. The plan enumerates renderers, not readers.

## 8. Out of scope

Each becomes an idea-registry row.

- **Building fabric.** `CellKind::Wall` has no material. Where a wall's
  reflectance comes from — local stone, culture, climate — is a modelling
  question with real content, not a lookup. The `PaletteEntry.color` slot
  ships empty and waits for it.
- **The interior illuminant.** Indoors the light is not the noon sun.
  `interior/pattern.rs` already reasons about doorway light and hearths;
  turning that into an `Illuminant` is the other half of the chamber band.
- **Colour in prose.** Naming (`hue_exemplar` + `in_ladder`) already ships
  and is untouched here; wiring it into focalized description is its own
  campaign.
- **Retiring `Biome::color()`**, sky scattering, texture, the pigment
  economy — all carried forward from Pigment §9, unchanged.

## 9. Risks

1. **H3 may stay false.** Merging two cone curves may not be enough to
   produce confusable pairs among seven exemplars chosen to be distinct.
   Ship the null; do not retune the merge to rescue it.
2. **H4 may be eaten by `u8`.** If the day's altitude range moves fewer than
   one quantization step, the light channel is real but invisible. Measure
   before claiming; the fallback finding is a measured threshold altitude.
3. **The bedrock/ground discipline is easy to lose.** Three renderers now
   apply it (ASCII lens, chart pane, plan pane) and only one of them has a
   test today.
4. **`Projection.norms` is a byte-identity landmine.** Anyone "simplifying"
   it to a derived sum moves every colour the standard observer emits. The
   pinned-literal test in §7 exists for exactly this.
5. **Parallel campaigns.** `the-delvers` is active. `make preflight`
   mechanizes only the checkable half; read its chronicle before absorbing.

## 10. Decisions (the durable record)

1. **A false-colour mapping is a named projection that declares what it
   preserves** — a peer in a registry, never an attribute of the eye and
   never invented at the renderer.
2. **The document carries the declaration.** A client cannot caption what it
   cannot see.
3. **`Observer` channels carry roles.** An achromatic channel is read by no
   projection and counted by no chromaticity metric. Without this, every
   observer with a rod is trichromatic (M3).
4. **Channel count is read off `pack_depths`' hue ladder**, so the eye and
   the lexicon cannot disagree; the *degree* is continuous in
   `night_vision`, because a tiered eye cannot tell its own species apart
   (M1).
5. **`observer_for(human)` == `standard_observer()`.** The standard observer
   is a derived row, not a privileged base case.
6. **Discriminability is chromaticity, not signal distance** (M2).
7. **Colour is on by default, through the possessed agent's own eyes**
   (Nathan's ruling); `eyes off` is the decline path and restores
   byte-identical output.
8. **The walk band is lit by the sun's real altitude** at the observer's
   hour and latitude, from shipped machinery.
9. **The chamber band ships the slot, not the value** (Nathan's call): the
   fabric and the interior light are unshipped models, and inventing either
   is what `RENDER-sourced-effects` forbids.
10. **No epoch.** Colour is derived, never committed; no stream label moves.
