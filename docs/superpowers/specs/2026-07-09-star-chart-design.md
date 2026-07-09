# The Star Chart — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-09-rendering-strategy-design.md` (roadmap item 2; Ring 1 contracts govern). Constitution §3.4 promises the almanac "star charts"; this spec pays that promise.
**Provenance:** The rendering strategy's Ring-1 roadmap named the star chart the next committed artifact. Designing it against the astronomy domain surfaced two model gaps the chart cannot honestly ignore: neighbor stars have no sky positions (`neighborhood.rs`: "a full starfield and constellations are tier 3"), and moon illumination phase runs on the sidereal period instead of the synodic — registry SKY-20, a confirmed physics defect. This campaign fixes the defect, draws the positions, and renders the chart.

---

## 1. Goal

Three deliverables, strictly ordered because each feeds the next:

1. **Fix SKY-20** — moon phase and months-per-year cycle on the synodic period.
2. **Give each neighbor star a fixed position** on the celestial sphere, drawn at genesis from a new seed stream that leaves every existing world's sky byte-identical.
3. **Render the chart**: `domains/astronomy/src/render.rs` producing a 72×24 equirectangular ASCII chart and a 256×128 planisphere-pair PNG, surfaced as `hornvale star-chart` and committed to the gallery.

## 2. The SKY-20 fix — synodic moon phase

**The defect.** `Calendar::moon_phase` and `Calendar::months_per_year` (`domains/astronomy/src/calendar.rs`) feed `Moon.period` — a Kepler *sidereal* period (`moons.rs`, base 27.32 d) — directly into illumination phase and month counts. Illumination cycles with the *synodic* period (sun–moon–observer geometry), ~29.53 d for a Luna-like moon: an ~8% error in every phase the sim reports.

**The fix.** The calendar derives the synodic period where illumination is computed:

```
P_syn = P_sid × Y / (Y − P_sid)      (all in standard days; Y = year length)
```

- `Moon.period` stays sidereal and untouched — it *is* the orbital period; the model card states it correctly. The sidereal→synodic conversion lives in the calendar, the layer that owns observed cycles.
- `moon_phase(t, index)` cycles on `P_syn`; `months_per_year(index)` becomes `Y / P_syn`.
- **Degenerate guard:** genesis stability bounds (Hill cap) keep `P_sid ≪ Y`, but if `P_sid ≥ Y` the conversion is meaningless — return `None` (the existing out-of-range convention) rather than a negative period. A unit test pins the guard.

**Contract posture.** This is a bug fix *toward* the stated model: the almanac's own fixture prose quotes synodic values while the code derives sidereal. No epoch suffix — the formula was wrong, not superseded. The chronicle entry records the correction explicitly.

**Blast radius (enumerated, regenerated in one commit):** the three committed almanac fixtures (`almanac-seed-42*.md` — month counts and any phase words change); `calendar.rs` unit tests that assert `months = year / sidereal` (e.g. `moon_phase_and_months_derive_from_kepler_periods`); any REPL/almanac test asserting phase vocabulary. CI's drift check confirms nothing else moved.

## 3. Star positions at genesis

**The draw.** Each neighbor gains a fixed direction on the celestial sphere, uniform over the sphere:

- declination `δ = asin(u)` with `u` uniform in [−1, 1] — degrees, −90…90;
- right ascension `α` uniform in [0°, 360°).

**Two new fields on `Neighbor`** (`neighborhood.rs`): `declination: f64`, `right_ascension: f64` — bare `f64` degrees, following the kernel's `GeoCoord` precedent for angles (typed quantities are for units crossing API boundaries; angular degrees on one struct match the geosphere's existing convention).

**Stream discipline (save-format critical).** Positions draw from a **new** derived stream — `streams::NEIGHBOR_POSITIONS = "neighbor-positions"` under the astronomy root, published via `stream_labels()` into the generated manifest. The existing `neighbors` stream keeps its exact consumption order, so classes, distances, and brightnesses of every existing world are byte-preserved. Draw order: one (δ, α) pair per neighbor, in draw order, *inside* the generation loop — the struct carries its position through the brightness sort, so a star keeps its own place in the sky regardless of where it ranks.

**Pin isolation.** The `--neighbor` class pin must not shift any position draw. Because positions come from their own stream, consumption is identical pinned or unpinned; a new property test in `genesis_properties.rs` asserts: same seed, pinned vs unpinned → identical (δ, α) lists (the pinned star's class differs; nothing's position does).

**Frame definition.** The celestial equator is the anchor's rotational equator; declination is measured from it. This costs nothing now (positions are only ever drawn, never transformed) and is exactly the alignment SKY-7 (solar position) and SKY-8 (latitude) will need — the chart does not foreclose them.

## 4. The render — `domains/astronomy/src/render.rs`

New module, patterned on the terrain/climate/settlement renders: pixel/glyph composition split from encoding, `hornvale_kernel::png::encode_rgb` for the raster, `#![warn(missing_docs)]` throughout, byte-deterministic.

**ASCII chart (72×24 equirectangular).** RA 0°→360° across, dec 90°→−90° down — the house lattice. Each star plots as its **index digit** (1..N, brightness-ranked; N ≤ 5 since the neighbor count draw is `range_u32(2, 5)`) — the label is the glyph. The field is space; the celestial equator row is a dashed line (`- - -`) with plotted stars overwriting it. Collisions (two stars on one cell) resolve by brightness — the brighter index wins; the legend still lists both.

**PNG chart (256×128 planisphere pair).** Two azimuthal-equidistant discs on a near-black field (`[10, 14, 28]`): north celestial hemisphere left (dec ≥ 0), south right. Within a disc: radius `r = (90 − |δ|) / 90 × R`, angle `θ = α` (north disc counterclockwise, south clockwise, so RA reads consistently). Dashed declination rings at 30° and 60°. Stars: filled dots, radius 1–3 px binned by apparent brightness, RGB by spectral class (one small `class → [u8; 3]` table, kin to `class_color`); each dot tagged with its index in a hand-rolled 3×5-pixel digit font (digits 1–5 needed; ~15 lines of bitmap constants). The PNG is starfield only — legend and moons live in the page.

**The page** (generated markdown, `map`-page shape): title ("The Night Sky of Seed N"), a sun line (class, color), the ASCII chart in a code block, a legend line per star (index, class, color, distance in ly, apparent brightness), a **phase-cycle strip** per moon — the moon's full synodic cycle as an ASCII glyph run (`o` new → `)` waxing → `O` full → `(` waning, 16 columns), labeled with its synodic period in standard days and apparent size word (the existing `size_word`) — and, with `--out`, the embedded PNG (`![Full-color render](./name)`). Timeless throughout: fixed stars, cycle strips, no reference day (dodging SKY-4's epoch-0 grand alignment).

## 5. CLI, gallery, CI

- `hornvale star-chart [--world PATH] [--out PNG]` — markdown to stdout, PNG to `--out`; exactly `cmd_map`'s shape. A world with the constant sun (no generated sky) errors loudly: `this world has no generated sky; no chart to draw` (the `sky` report convention).
- Gallery: `book/src/gallery/star-chart-seed-42.md` + `star-chart-seed-42.png`, generated from the CI sky world. **One chart per seed** — pin isolation makes the locked-rotation variant's starfield identical, so no `-locked` sibling.
- CI's "Artifacts are current" gains the `star-chart` line; the drift check covers both files.
- The book's gallery index/SUMMARY gains the page; the astronomy domain chapter gets a freshness pass (it currently describes a positionless neighborhood).

## 6. Testing

- **Calendar:** synodic conversion against a hand-computed value (Luna-like: 27.32 d sidereal, 365.25 d year → 29.53 d synodic); `months_per_year = Y / P_syn`; the `P_sid ≥ Y` guard returns `None`; existing phase tests updated to synodic expectations.
- **Genesis:** uniform-sphere draw bounds (δ ∈ [−90, 90], α ∈ [0, 360)); pin-isolation property test (§3); determinism (same seed → same positions).
- **Render:** ASCII chart is 72×24 with every star's digit present exactly once (absent collisions); PNG starts with the signature, IHDR says 256×128, byte-deterministic (the established render-test shape); collision rule unit test with a constructed two-stars-one-cell system.
- **Fixtures:** almanac fixtures regenerate; the star-chart artifacts join the drift check; full gate green.

## 7. Consequences for existing documents

- Registry: SKY-20 flips to `shipped` on merge (its row text already records the defect story). SKY-12 (constellations) stays `raw` — but positions are its missing prerequisite; this spec is the breadcrumb.
- Rendering strategy spec roadmap item 2: satisfied on merge.
- Book: chronicle entry (this is a physics-touching campaign — decision 0013 applies) + freshness sweep (astronomy chapter, sky gallery pages).
- Stream manifest: regenerated with the new label (additive).

## 8. Non-goals

- **No constellations, no star names** (SKY-12/MAP-3): stars are index-labeled; cultural naming is diegetic content for a future campaign.
- **No full starfield**: the chart shows the 2–5 notable neighbors the model holds; a dense background field would be decoration pretending to be data.
- **No observer:** no latitude, no solar position, no rise/set (SKY-7/SKY-8 untouched); the chart is the whole fixed sphere, not a night's view.
- **No epoch-offset fix** (SKY-4): the page is timeless by design; the grand alignment stays a known deficit.
- **No REPL `chart` command:** the REPL's `sky` prose stands; the chart is an almanac artifact (Ring 1), reachable via the CLI.
