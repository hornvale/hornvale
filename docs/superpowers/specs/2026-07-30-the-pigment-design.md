# The Pigment — design

**Status:** spec, awaiting G3 review.
**Date:** 2026-07-30
**Campaign:** The Pigment — the colour substrate, and one vertical slice
proving it end to end.

## 1. The question

Hornvale has no colour. It has four unconnected gestures at colour, none of
which can talk to another, and none of which can answer the question a
walked world actually asks: *what colour is this, to whoever is looking, in
this light?*

The ask that prompted this campaign was broad — colour for alchemical
substances, stars and moons, cultural significance, species skin tones,
rocks and soils and water; colour embedded in prose and in the roguelike
view; colour discardable for screen readers, remappable for colour
blindness, and re-derivable for creatures whose eyes are not human. That is
four campaigns' worth of surface. This spec builds the **substrate** they
all stand on, and proves it on one producer and both consumer poles.

The load-bearing observation is that colour is not a property of an object.
It is a three-way product:

> **colour = illuminant × reflectance × observer**

A material has a *reflectance* — the fraction of light it returns, per
wavelength — and that is a property of the stuff, identical in a cave and at
noon. Light has a *spectrum*. An eye has a handful of *sensitivity curves*
and collapses the incoming mixture down to that many numbers. Colour exists
only where all three meet.

Every hard thing in the ask is a consequence of that one sentence. A goblin
seeing differently is a substituted observer. Colour blindness is a
substituted observer. A screen reader is the observer step declined. An
amber sun reddening a green cloak differently than it reddens a red one is
the per-wavelength product doing its job. Store a finished colour and all
four become guesswork applied after the fact, because the observer step has
already happened and been discarded.

## 2. What exists today (measured, on `31320e0d`)

**Four disconnected representations, none of them a colour model.**

1. `Biome::color(self) -> [u8; 3]` (`domains/climate/src/biome.rs:260`),
   twenty-two hand-picked RGB triples tagged `type-audit: bare-ok(artifact)`
   — explicitly for the PNG and nothing else.
2. `Appearance { albedo, hue, coarseness, hardness }`
   (`domains/terrain/src/lithology.rs:614`), a `[0,1]` abstraction projected
   from the material buffer. `hue` is a single scalar, "0 grey/black → 1
   red/ochre (iron)" — not colorimetric, and not extensible to a third axis.
3. `spectral_color(class) -> &'static str`
   (`domains/astronomy/src/render.rs:262`) and its sibling
   `class_rgb(class) -> [u8; 3]` (`render.rs:149`) — a *word* and a triple,
   both switch statements over a class enum.
4. `color_pack()` (`domains/language/src/packs.rs:279`) — the Berlin & Kay
   basic-colour-term acquisition ladder, as concepts.

**The fourth one is not a gesture. It is half the campaign, already
finished.** `PackDepths { hue, luminance }` (`packs.rs:532`) and
`in_ladder(entry, depths)` (`packs.rs:545`) gate which colour terms a
lexicon holds, and `windows/worldgen/src/lib.rs:3621` already derives those
depths from a species' perception:

```rust
pub fn pack_depths(p: &PerceptionVector) -> PackDepths {
    let hue = 2 + ((1.0 - p.night_vision) * 3.0).round() as u8;
    let luminance = if p.night_vision > 0.6 { 3 } else { 1 };
    PackDepths { hue, luminance }
}
```

The authored model card reasons that a species which sees well in the dark
spent less of its evolutionary history straining at daylight hue
distinctions. At the roster values that yields, verbatim from the doc
comment: goblins (`night_vision == 0.5`) reach `hue == 4` — they have blue
and lack brown; kobolds (`night_vision == 0.9`) reach `hue == 2` — dark,
light, and red, and nothing further. `perceptual_reason`
(`worldgen/src/lib.rs:3636`) already words *why* a given species lacks a
given term.

So the culture-specific-colour-language half needs **no new gate**. It needs
something to name. Memory `exposure-gates-belong-in-the-lexicon-not-the-
consumer` records the failure mode if we forget this: a gate written at the
point of use changed zero bytes, because the lexicon had already filtered.

**Two further facts that shape the design.**

- `PerceptionVector` (`domains/species/src/lib.rs:196`) carries `activity`
  (Diurnal / Nocturnal / Crepuscular) and `night_vision`, authored per kind
  in `perception_registry()` (`lib.rs:2055`).
- **There is no ANSI anywhere in the workspace.** The terminal chart
  (`render_surrounds_ascii`, `windows/scene/src/surrounds_ascii.rs:63`,
  reachable at `hornvale scene surrounds --render ascii`,
  `cli/src/main.rs:1271`, and built on by the vessel's purview at
  `windows/vessel/src/purview.rs:66`) is pure glyphs. That half is
  greenfield.

## 3. The model

### 3.1 The primitive (kernel, new module)

```rust
/// Spectral power / reflectance sampled on the fixed band grid.
pub struct Spectrum([f64; BANDS]);      // BANDS = 10
pub struct Reflectance(Spectrum);        // fraction returned, per band, [0,1]
pub struct Illuminant(Spectrum);         // radiance arriving, per band
pub struct Observer { channels: Vec<Spectrum> }  // one curve per channel
pub struct Signal(Vec<f64>);             // one number per observer channel
```

Validating newtypes, per the typed-quantities convention; the band grid is a
kernel constant. The entire computation is:

```
signal[c] = Σ over BANDS of  reflectance[b] × illuminant[b] × sensitivity[c][b]
```

**The band grid: 10 uniform 40 nm bands spanning 340–740 nm.**

Two choices there, both deliberate:

- **Uniform, not weighted toward human sensitivity peaks.** Weighting the
  grid where human cones are most sensitive would rebuild, in the substrate,
  exactly the anthropocentrism the model exists to remove. The grid is
  observer-neutral the way the room lattice is.
- **340 nm, not 380 nm.** The grid is a *contract*: widening it later
  rewrites every authored reflectance in the repo. Near-UV costs two extra
  multiply-adds for a human observer whose sensitivity there is ~0, and buys
  UV-sensitive and tetrachromat vision for the species campaign at no epoch.
  Wide now is strictly cheaper than wide later.

Thermal infrared is deliberately **excluded**. A pit viper's sense detects
emitted radiance in the 5–30 µm range, not reflectance — it is a different
sense organ answering a different question, and pretending it is a colour
band would make both wrong.

### 3.2 Mixture, and the three laws

Combining two colours is not one operation. It is three, and they disagree:

| Situation | Law | Mechanism |
|---|---|---|
| Two torches on one wall | **Additive** — sum the illuminants | Two streams arrive together |
| Speckled granite at 10 m | **Area** — weighted *arithmetic* mean of reflectances | Separate reflections average en route to the eye |
| Two dyes in one vat | **Subtractive** — weighted *geometric* mean | Light passes through both absorbers in series |

This is why blue and yellow *paint* make green while blue and yellow *light*
make white. An RGB triple cannot distinguish these; a per-wavelength model
gets it for free, because absorption is a per-wavelength phenomenon.

**The Pigment implements area mixing only.** The other two are declared on
the type with their laws documented and unimplemented: additive arrives with
multi-light (torches, hearths, two suns), subtractive with alchemy — where
it makes "interpolating between substances" a physical operation rather than
a lerp.

The producer therefore returns a **mixture**, not a finished reflectance:

```rust
pub struct Mixture { components: Vec<Reflectance>, weights: Vec<f64> }
impl Mixture { pub fn integrate(&self) -> Reflectance { /* area law */ } }
```

The Pigment only ever calls `integrate()`, so it ships exactly as if the
producer returned a `Reflectance` directly. The reason to keep the
components is §9's texture layer: collapsing early would force a later
campaign to re-derive components that had already been computed and thrown
away. This is the additive-latent-seam pattern this repo has arrived at four
separate times (memory: `additive-latent-byte-identity-pattern`).

### 3.3 The illuminant

`Star` (`domains/astronomy/src/star.rs:12`) carries mass, luminosity,
`class_name`, habitable zone, and age — but no effective temperature. Add
one, **derived from the mass already drawn**:

```
L ∝ M^3.5   (already the repo's relation, star.rs:16)
R ∝ M^0.8   (main-sequence approximation)
L = 4πR²σT⁴  ⇒  T ∝ M^0.475
T_eff = 5772 K · M^0.475
```

Over the drawn mass range 0.6–1.4 M☉ that spans **4528 K to 6772 K**
(computed, not estimated) — cool orange through yellow-white, with a
solar-mass star landing on 5772 K by construction. Sampling Planck's law into the
ten bands costs one `kernel::math::exp` per band per world, not per cell.

`T_eff` carries **the same containment rule `age` already carries**
(`star.rs:21-26`, which documents that age "**does not feed** `luminosity`
or `habitable_zone`" so it "can never move a world's insolation, orbit
admission, or climate"). `T_eff` feeds colour and nothing else. Same shape,
same guard, same test obligation.

No draw, no stream label, no epoch: the quantity is derived from a value the
seed already fixes.

### 3.4 The observer

The Pigment ships a `StandardObserver` — three photopic channels plus a
rod-like scotopic channel used at low light, four in total — and the *slot*
for others. It does not yet wire `PerceptionVector` into that slot; that is
campaign 2, and doing it here would widen the slice past one producer.

What it does ship is the guarantee that the slot is real: the naming
function in §3.5 must work at any channel count, tested against a synthetic
two-channel dichromat and a synthetic five-channel observer — counts either
side of the standard's four, so neither test can pass by accidentally
exercising the standard path.

**Showing a non-standard observer on a three-channel screen is a lens
choice, and must be captioned as one.** A four- or five-channel signal has
no truthful sRGB image; any mapping is a false-colour decision. RENDER-9's
rule applies directly — the caption, not the picture, carries the honesty —
so each non-standard observer declares its display mapping and the render
states that what you are seeing is a translation. The Pigment ships this
rule and the standard observer's own (real) mapping; campaign 2 ships the
first observer that needs the translation.

### 3.5 Naming: exemplars, not centroids

The obvious design is to give each colour concept a centroid in signal
space. It is wrong, and finding out why early is worth a paragraph.

Signal space is *observer-shaped*. A four-channel observer's signals live in
a different space than a three-channel observer's, so centroids authored in
signal space would need re-authoring per observer — a hidden authoring tax
levied on exactly the campaign this substrate exists to enable.

Instead: **each colour concept is authored as a canonical exemplar
reflectance**, and naming pushes the exemplar through the *same* illuminant
and the *same* observer as the sample before comparing.

```
name(sample, illuminant, observer, lexicon):
    s = signal(sample, illuminant, observer)
    for each concept the lexicon holds (in_ladder(entry, depths)):
        e = signal(exemplar(concept), illuminant, observer)
    return the nearest, ties broken by ladder_rank then concept id
```

Exemplars are observer-neutral and illuminant-neutral by construction, so
any new observer works with zero new authoring. It is also how naming
physically works: you compare a thing against remembered examples under the
light you and they share.

The lexicon filter is `in_ladder`, unmodified. A kobold naming an ochre
outcrop has only dark, light, and red available and must reach for one of
them.

**Only the hue ladder gets exemplars.** `color_pack` holds ten concepts on
two ladders: seven hue terms (dark, light, red, green, yellow, blue, brown)
and three luminance terms (gloom, shadow, starlit). The luminance ladder
describes *ambient darkness*, not a surface — it is selected by the
illuminant's level, not by comparing a reflectance against an exemplar. So
The Pigment authors **seven** exemplar reflectances, and the luminance terms
are chosen on a separate, simpler test. The rule for when a luminance term
should preempt a hue term is unresolved; see risk 5.

## 4. Determinism and epoch analysis

**No epoch. No stream label. No new seed draw.** Colour is derived from
committed causes, never committed itself — a world is a seed plus a ledger,
and everything else is re-derived.

**The hot path needs no libm at all.** `Σ r[b]·i[b]·s[b]` is multiplication
and addition of `f64` over fixed-size arrays: fixed summation order, and
decision 0041 names these operations explicitly — *"IEEE 754 requires exact
results for `sqrt`, `abs`, `floor`/`ceil`/`round`, `mul_add`, `powi`, and
arithmetic, so those inherent methods remain allowed everywhere."*
Bit-identical cross-platform by IEEE mandate rather than by convention.

Three obligations follow:

1. **Do not mix `a * b + c` with `a.mul_add(b, c)`.** Both are IEEE-exact;
   they round differently *from each other*. This spec picks separate
   `a * b + c` throughout, and the reviewer greps for stray `mul_add`. Rust
   does not auto-contract, so this is a discipline note, not a hazard.
2. **Two transcendentals exist, both away from the hot path.** Planck
   sampling uses `kernel::math::exp` (already wrapped, once per band per
   world). The final sRGB encode uses a gamma curve, `kernel::math::powf`,
   at the emit boundary — or a fixed table, which the implementation should
   prefer if it is not uglier.
3. **Quantize at emit only.** If a colour value is ever serialized, it goes
   through `hornvale_kernel::quantize` like every other float. The
   compute path stays full-precision.

Worth stating plainly, because it inverts the intuition: **the spectral
model carries less cross-platform surface than the perceptual-vector
alternative would have.** A hue-angle-and-chroma representation needs
`atan2`, and a perceptual space needs `cbrt` — which has no
`kernel::math` wrapper today and would owe one plus its clippy ban.

**Committed artifacts.** The colour render is a *new registered lens*, not a
change to the existing one. `scripts/regenerate-artifacts.sh:317-329` writes
three drift-checked gallery files
(`book/src/gallery/generated/surrounds-seed-42/{flagship,coastline,seam}.txt`)
from `scene surrounds --render ascii`; emitting escape sequences into that
string would move all three. The uncoloured output stays byte-identical, and
RENDER-9 wants colour registered and captioned anyway rather than smuggled
into an existing lens.

`Biome::color()` is **not** touched. Retiring the hand-picked palette in
favour of a derived one would move the biome PNG's bytes and owe a
rebaseline; that cost is elective and unrelated to proving the seam. No
claim is made here about the size of that diff — it gets measured by running
the regen when the question is taken up, not predicted now.

## 5. What The Pigment builds

### 5.1 Scope

| Layer | Deliverable |
|---|---|
| Kernel | `Spectrum` / `Reflectance` / `Illuminant` / `Observer` / `Signal` / `Mixture`; area mixing; sRGB projection; `StandardObserver` |
| Astronomy | `Star::t_eff` (derived, contained); Planck sampling into the band grid |
| Terrain | `MaterialBuffer` → `Mixture`, a second projection of axes that already exist |
| Language | Seven exemplar reflectances, one per `color_pack` hue term; a level test for the three luminance terms |
| Prose | Name a material through the speaker's lexicon, via `in_ladder` unmodified |
| Terminal | A `colour` lens for `render_surrounds_ascii`, opt-in, leaving the existing lens byte-identical |

Terrain is the producer because its reflectance is a **re-projection of the
material buffer it already holds** (`silica`, `carbonate`, `grain`,
`induration`, `metamorphic_grade`, `porosity` —
`domains/terrain/src/lithology.rs:83`), not new authored data. It is also
underfoot in every room, so every scene exercises the whole seam.

### 5.2 The two falsifiable claims

Preregistered, and both are deliberately broken during review — memory
`mutation-test-the-deliverable-tests` records The Purview shipping a thesis
with two clauses that *could not fail*:

1. **Same outcrop, same light, two species → different words**, and the
   difference traces to `pack_depths`, not to a hardcoded branch. Breaking
   `pack_depths` must turn this test red.
2. **Same outcrop, one species, noon vs dusk → different words**, and the
   difference traces to the illuminant. Flattening `T_eff` to a constant
   must turn this test red.

Reviewers report the measured values, not a verdict — memory
`a-passing-test-can-pass-by-the-wrong-path`.

### 5.3 Testing

- **Byte-identity:** two builds of seed 42 produce identical coloured
  output; the *uncoloured* gallery charts are unchanged from `main`, checked
  by `cmp` and not by eye.
- **Observer-agnosticism:** naming through a synthetic four-channel observer
  succeeds without new exemplar authoring.
- **Containment:** `T_eff` reaches colour and nothing else — mirroring the
  test `age` already owes.
- **Energy conservation:** no reflectance exceeds 1.0 in any band; area
  mixing of two reflectances lies between them, per band.
- **Reachability:** every `color_pack` concept is nameable by at least one
  roster species under at least one illuminant — memory
  `modelled-authored-unreachable` records this project repeatedly shipping
  types that are defined, prose-authored, and cannot occur.

## 6. Two channels, kept apart

The original ask bundled two different wants: *the goblin is greenish* and
*exits are subliminally brighter*. They are different channels and they stay
different.

- **Simulated light** rides the spectral pipeline and comes from the scene
  layer.
- **Interface emphasis** rides the vessel's overlay — which is already where
  non-diegetic state lives. `windows/scene/src/surrounds.rs:3-5` documents
  the scene as *"Semantic-only and FOG-FREE — this builder never invents
  epistemic state; a session-owning consumer (the vessel) overlays what it
  alone knows"*, and `faded()` in `surrounds_ascii.rs` is already exactly
  such an overlay, expressed in glyph choice.

They composite at the terminal. Merging them would let a colour-blindness
remap silently destroy the exit cue — the accessibility transform would eat
the affordance it was supposed to preserve.

## 7. Accessibility is the same mechanism, not a second one

| Need | Mechanism |
|---|---|
| Screen reader | Decline the observer step. Prose was never coloured; it was *worded*. |
| Deuteranopia, protanopia, tritanopia | An `Observer` with a shifted or absent channel — the same code path as a goblin |
| `NO_COLOR`, dumb terminal | The uncoloured lens, which remains the default |
| Nocturnal or crepuscular sight | An `Observer` with rod-weighted sensitivity and night-vision gain |

Four items from the original ask; one mechanism. Any environment reading
(`NO_COLOR`) happens in `cli/`, never in the sim core.

## 8. Risks and open questions

1. **The `T_eff` exponent must agree with `class_name`.** The star already
   carries a human-readable spectral character; a derived temperature that
   contradicts it would be a visible inconsistency. Implementation must
   check the derived range against the existing class labelling and reconcile
   — this is a real obligation, not a formality.
2. **Exemplar authoring is a judgement call.** Seven canonical reflectances
   decide every hue name the world produces. They should be grounded in
   published reflectance data where it exists, and the spec-time choice
   recorded, so a later disagreement is with the data and not with taste.
3. **Naming may be too stable.** If the illuminant range 4528–6772 K never
   moves a name across an exemplar boundary, claim (2) is unfalsifiable in
   practice. This must be measured *before* the claim is preregistered, not
   after — and if it is true, the claim changes to something that can fail
   rather than the constant being retuned to rescue it.
4. **ANSI capability and width.** Terminal colour depth varies. The lens
   should target a conservative palette and degrade, and the degradation
   path needs a test, not an assumption.
5. **Scotopic naming.** At low light a rod-dominant observer has hue
   information but poor hue *discrimination*. Whether the luminance ladder
   (gloom/shadow/starlit) should preempt the hue ladder below some
   threshold is unresolved; the ladders exist separately in `color_pack`
   already, so the machinery is there, but the switching rule is not
   designed.

## 9. Out of scope — and the campaigns that follow

Deferred deliberately, each to be captured as an idea-registry row:

- **Texture and pattern.** The same rock is a mixture at map scale and a
  pattern at hand-lens scale — pattern is *scale-relative*, which makes it
  "coarse constrains fine" restated, with a hard invariant: **the mean of a
  sampled texture must equal its mixture.** Descriptors are data, samplers
  are code (decision 0011's shape), so hand-authored and procedural stop
  being different cases. Conglomerate and breccia differ in exactly one
  parameter — clast angularity; gneiss banding is driven by
  `metamorphic_grade`, an axis the buffer already carries. Another
  projection of the same data.
- **Animated water.** Frame-rate shimmer is client-side and outside the
  determinism boundary (decisions 0055, 0022/0023): the sim emits ripple
  statistics and a phase function, never frames. Slow world-time variation —
  tides, seasonal turbidity, ice — is ordinary simulation on `WorldTime`.
- **Species integument and the observer swap** (campaign 2).
- **The pigment economy** (campaign 3): alchemical substances, subtractive
  mixing, and cultural significance colours falling out of what a people can
  actually *make* — a colour is expensive because the murex is scarce.
- **Sky scattering** — Rayleigh, Mie, twilight; the EXP-3a "scattering
  regime" the frontier already names.
- **Retiring `Biome::color()`** in favour of a derived palette.
- **Thermal IR as a separate sense**, not a colour band.

## 10. Decisions (the durable record)

1. **Colour is derived, never committed.** The ledger holds causes —
   mineralogy, stellar mass, pigment identity. No epoch is owed.
2. **Coarse spectral fidelity** (Nathan's call at G1, a fidelity carve-out):
   ten bands over 340–740 nm, uniform. Chosen over a perceptual vector and
   over an sRGB triple, on the grounds that the observer step is the one
   every downstream ask needs to vary — and that spectral carries *less*
   libm surface than the perceptual alternative.
3. **The primitive lives in the kernel; reflectances are authored per
   domain.** A shared named-pigment library is deferred to alchemy rather
   than speculated now.
4. **Three mixing laws, one implemented.** Area now; additive and
   subtractive declared, documented, and unimplemented.
5. **The producer returns a `Mixture`, not a `Reflectance`**, so the texture
   layer arrives additively.
6. **`T_eff` is derived and contained** — colour only, never insolation,
   orbit admission, or climate. The rule `age` already carries.
7. **Naming compares exemplars, not centroids**, so any observer works with
   no new authoring.
8. **Colour words gate through the existing lexicon** — `pack_depths` and
   `in_ladder`, unmodified. The consumer adds no gate of its own.
9. **Simulated light and interface emphasis are separate channels**,
   composited at the terminal.
10. **Colour is a new registered lens.** The existing uncoloured chart stays
    byte-identical.
