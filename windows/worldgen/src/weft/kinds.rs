//! A derived-feature kind, as data — spec §5.7: "a kind is three things and
//! nothing else — a component bundle, a prevalence recipe, and the three
//! scalars." This file carries the recipe and the scalars for all four
//! kinds spec §5.6 names. The component bundle for the one enterable-
//! adjacent kind (overhang/hollow's shelter-and-fire affordance) is **not**
//! wired here: `ObjectProperty`/`object_registry` live in `windows/vessel`,
//! which depends on `hornvale-worldgen` (not the reverse — the same
//! layering [`super`]'s module doc states for `LocaleContext`), so this
//! crate cannot reference that vocabulary at all. [`WeftKind::Overhang`]'s
//! own doc states the affordance claim in prose; wiring an `overhang` row
//! into `windows/vessel`'s `object_registry` is a later task's work.
//!
//! **Eligibility is a per-kind gate, tested before any noise is drawn (Task
//! 7, controller ruling R1).** Task 5's review measured that 59% of all
//! spring occurrences (seed 42, every walk-depth facet over all 40,962
//! vertices) landed on facets with no macro cause at all, including open
//! ocean — the mixing lerp's `(1 - contextuality) * noise` floor is real and
//! unconditional, so nothing stopped a spring from surfacing mid-sea.
//! [`eligible`](WeftKind::eligible) closes it, mirroring
//! `GeneratedTerrain::cave_at`'s own `if self.is_ocean(id) { return None; }`
//! one level down (`domains/terrain/src/provider.rs`). See
//! `crate::weft::mod`'s [`super::prevalence`] for where the gate is applied,
//! and this file's `land_eligible` for the shared ground test every kind
//! currently uses.

use hornvale_kernel::math;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Vertex, blend_corner_weights};

use crate::FieldPack;
use crate::streams;

/// A derived feature kind (spec §5.6's spanning set). `Copy` — a kind is a
/// bare discriminant, never carries data of its own; every scalar it names
/// lives in this module's per-kind constants, looked up through `self`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WeftKind {
    /// Spring / seep — spec §5.6's "sign case": high contextuality, long
    /// correlation length, an enterable water source diagnostic of what is
    /// underfoot (karst carbonate crossed with channelized drainage).
    Spring,
    /// Overhang / hollow — spec §5.6: medium contextuality (induration ×
    /// slope), short–medium correlation length. **Not enterable**, but
    /// affords shelter and fire: "a place to get out of the rain and start
    /// a fire" is a component bundle in the existing `ObjectProperty`
    /// vocabulary (`SupportsRest`-adjacent shelter plus a warmth variant),
    /// once wired at `windows/vessel`'s `object_registry` — see this
    /// module's own doc for why that wiring is not, and cannot be, done
    /// here. Proves the affordance path end to end (spec §5.6's "what it
    /// proves" column), once that later wiring lands.
    Overhang,
    /// Thicket / brake — spec §5.6: high contextuality (productivity —
    /// temperature × moisture, Liebig-combined), long correlation length.
    /// Not enterable; texture, aimed directly at the biome-monotony defect
    /// this campaign addresses (`Biome` is categorical and `blend_at`
    /// structurally cannot smooth it; this kind reads the two continuous
    /// causes underneath instead).
    Thicket,
    /// Erratic / scatter — spec §5.6's **negative control**: deliberately
    /// LOW contextuality (near-zero — mostly free noise, position-
    /// continuous but uncorrelated with any macro cause), short correlation
    /// length. Not enterable. Exists so Task 9's legibility metric can be
    /// shown to *discriminate*: if springs and erratics score alike, the
    /// metric measures nothing. See [`Self::contextuality`]'s doc for why
    /// this kind must never be "improved" by tying it to a real macro
    /// cause.
    Erratic,
}

impl WeftKind {
    /// Every kind that exists, in a fixed order. [`super::window::WeftWindow`]
    /// enumerates this once per facet entering its radius (Task 6) — the one
    /// place a new kind must be added for the residency window to pick it up.
    /// Task 7 grows this by appending, never editing an existing entry (spec
    /// §5.7: "kind N+1 is an append").
    pub const ALL: [WeftKind; 4] = [
        WeftKind::Spring,
        WeftKind::Overhang,
        WeftKind::Thicket,
        WeftKind::Erratic,
    ];

    /// This kind's seed-derivation root leg (a save-format contract; see
    /// `windows/worldgen/src/streams.rs`). [`super::prevalence`] and
    /// [`super::occurs`] each derive their own decorrelated sub-leg under it.
    pub(crate) fn stream_label(self) -> StreamLabel<'static> {
        match self {
            WeftKind::Spring => streams::WEFT_SPRING,
            WeftKind::Overhang => streams::WEFT_OVERHANG,
            WeftKind::Thicket => streams::WEFT_THICKET,
            WeftKind::Erratic => streams::WEFT_ERRATIC,
        }
    }

    /// The abundance ceiling — `prevalence`'s maximum possible answer for
    /// this kind, reached only where macro state and noise both saturate.
    pub(crate) fn abundance(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_ABUNDANCE,
            WeftKind::Overhang => OVERHANG_ABUNDANCE,
            WeftKind::Thicket => THICKET_ABUNDANCE,
            WeftKind::Erratic => ERRATIC_ABUNDANCE,
        }
    }

    /// How many facets, walked in a straight line, before this kind's noise
    /// contribution meaningfully changes (spec §5.2's correlation length, in
    /// facets — converted to an [`hornvale_terrain::SphereFbm`] frequency at
    /// the call site, since the facet-to-radians conversion depends on the
    /// *facet being sampled*, not the kind alone).
    pub(crate) fn correlation_length_facets(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_CORRELATION_LENGTH_FACETS,
            WeftKind::Overhang => OVERHANG_CORRELATION_LENGTH_FACETS,
            WeftKind::Thicket => THICKET_CORRELATION_LENGTH_FACETS,
            WeftKind::Erratic => ERRATIC_CORRELATION_LENGTH_FACETS,
        }
    }

    /// The macro-state/free-noise mixing weight: `1.0` reads as entirely
    /// macro-state-driven ("wallpaper" in the sense of a smooth, place-
    /// grounded texture), `0.0` as entirely free noise — position-continuous
    /// but uncorrelated with any macro cause. Erratic/scatter sits near the
    /// `0.0` end, matching its own §5.6 description ("low — mostly free
    /// noise").
    ///
    /// **"Speckle" is reserved for a different, banned state and does not
    /// belong on this axis.** Spec §5.2's own three-state paragraph is
    /// explicit: *address-hashed* noise (spatially incoherent between
    /// adjacent facets, never sampled by anything in this module — see
    /// [`super`]'s module doc) is the failure H2 forbids outright; low
    /// contextuality is something else, a field that is still smooth and
    /// position-continuous but simply uncorrelated with macro state. §5.2's
    /// table row briefly conflated the two by glossing `0` as "speckle" —
    /// fixed in the spec (fix round 2) to name the property instead of
    /// reusing the loaded word.
    ///
    /// **Spec §5.2's endpoint labels were also inverted relative to this
    /// implementation and to §5.6 in an earlier draft** (`0 = wallpaper, 1 =
    /// speckle`, under which spring's "high" contextuality would have read
    /// as the banned state and erratic's "low" would have read as
    /// wallpaper). Fixed in the spec
    /// (`docs/superpowers/specs/2026-09-03-the-weft-design.md` §5.2) in fix
    /// round 1; this doc states the corrected direction rather than quoting
    /// the sentence that was wrong.
    ///
    /// **`WeftKind::Erratic`'s own value must never be raised to tie it to
    /// macro state (Task 7, controller ruling R5).** The negative control's
    /// whole purpose is a prevalence surface uncorrelated with any macro
    /// cause; a later "improvement" that reads a real `FieldPack` scalar for
    /// it would make Task 9's H3 legibility metric unable to discriminate a
    /// real cause from none — which is precisely what the erratic exists to
    /// let that metric prove it can do.
    pub(crate) fn contextuality(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_CONTEXTUALITY,
            WeftKind::Overhang => OVERHANG_CONTEXTUALITY,
            WeftKind::Thicket => THICKET_CONTEXTUALITY,
            WeftKind::Erratic => ERRATIC_CONTEXTUALITY,
        }
    }

    /// This kind's blended macro-state signal in `[0,1]`, given `facet`'s
    /// bilinear corner weights ([`hornvale_kernel::Facet::corner_weights`])
    /// and the materialized [`FieldPack`].
    pub(crate) fn macro_state(self, weights: [(Vertex, u64); 4], pack: &FieldPack) -> f64 {
        match self {
            WeftKind::Spring => {
                let carbonate = blend_corner_weights(weights, &pack.carbonate);
                let drainage = blend_corner_weights(weights, &pack.drainage);
                spring_macro_state(carbonate, drainage)
            }
            WeftKind::Overhang => {
                let induration = blend_corner_weights(weights, &pack.induration);
                let slope = blend_corner_weights(weights, &pack.slope);
                overhang_macro_state(induration, slope)
            }
            WeftKind::Thicket => {
                let temperature = blend_corner_weights(weights, &pack.temperature);
                let moisture = blend_corner_weights(weights, &pack.moisture);
                thicket_macro_state(temperature, moisture)
            }
            WeftKind::Erratic => erratic_macro_state(),
        }
    }

    /// Whether `self` may occur at all at `weights`' blended ground — the
    /// early eligibility gate [`super::prevalence`] tests *before* reading a
    /// macro-state cause or drawing any noise (Task 7, R1; see this file's
    /// own module doc). Per-kind, not a single free-standing test, so a
    /// future kind may diverge (spec §5.7: kind N+1 is an append) even
    /// though every kind today shares [`land_eligible`]'s ground test — none
    /// of the four is sensible mid-ocean (a water-source diagnostic, a rock
    /// overhang, standing vegetation, and a deposited boulder all want dry
    /// ground under them).
    pub(crate) fn eligible(self, weights: [(Vertex, u64); 4], pack: &FieldPack) -> bool {
        match self {
            WeftKind::Spring | WeftKind::Overhang | WeftKind::Thicket | WeftKind::Erratic => {
                land_eligible(weights, pack)
            }
        }
    }
}

/// The blended land fraction, `[0,1]`, at or above which ground counts as
/// eligible — `0.5` reads as "the facet's own bilinear blend of
/// [`FieldPack::land`] is majority-land", the continuous analogue of
/// `GeneratedTerrain::is_ocean`'s discrete per-vertex test, applied here to
/// a facet's blended four corners rather than a single vertex (position-
/// continuous, like every other test this module performs — never a single
/// discrete vertex flip at a facet straddling the coastline).
/// plumb: universal(a majority-land threshold on the blended [0,1] ground-eligibility flag, fixed across every world and shared by every kind)
const LAND_ELIGIBILITY_THRESHOLD: f64 = 0.5;

/// The shared ground-eligibility test every [`WeftKind::eligible`] arm
/// currently delegates to (Task 7, R1): `weights`' blend of
/// [`FieldPack::land`] at or above [`LAND_ELIGIBILITY_THRESHOLD`]. `pub(crate)`
/// rather than a private free function so `crate::fieldpack`'s own module
/// doc can point at it by name.
pub(crate) fn land_eligible(weights: [(Vertex, u64); 4], pack: &FieldPack) -> bool {
    blend_corner_weights(weights, &pack.land) >= LAND_ELIGIBILITY_THRESHOLD
}

/// Abundance ceiling for spring/seep (spec §5.2). Chosen well under `1.0` —
/// a spring is a notable feature, not the default ground state — and
/// independently dialable from every other kind's own ceiling (spec §5.2:
/// "tunable individually and severally"; §5.2 also forbids a simplex
/// constraint across kinds, so this value trades against nothing else).
/// plumb: universal(an authored design ceiling on spring/seep frequency, fixed across every world and not derived from any seed or pin)
const SPRING_ABUNDANCE: f64 = 0.35;

/// Spring/seep's correlation length, in facets (spec §5.2, §5.6: "long").
/// A walker should cross many facets of one karst zone before the signal
/// drifts, matching a real spring/seep region's real geographic extent —
/// texture at the scale of a *place*, not a per-step coin flip.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names spring/seep's correlation length "long" and this is the chosen magnitude)
const SPRING_CORRELATION_LENGTH_FACETS: f64 = 40.0;

/// Spring/seep's contextuality (spec §5.2, §5.6: "high — carbonate ×
/// drainage × elevation"). A spring is diagnostic of what is underfoot, so
/// its prevalence must actually track macro state rather than merely being
/// textured near it — the opposite end from erratic/scatter's negative
/// control.
///
/// **`0.85`, restored here after a wrong-headed detour to `0.7` (fix round
/// 1, F2).** The first pass of this task lowered this constant to `0.7`
/// after a mutation-discrimination check (see `weft_prevalence.rs`'s test
/// doc) appeared to pass more comfortably there — but review measured the
/// real signal-to-noise ratio (real-mechanism max delta vs. address-hashed
/// mutant max delta, seed 42, 6 starting vertices) at **42.6× at `0.85`** and
/// **38.7× at `0.7`**: lowering contextuality scales BOTH sides of that ratio
/// by the same factor, so the move bought no discrimination at all, and was
/// marginally worse. The actual defect was a `weft_prevalence.rs` bound set
/// too loose (`0.02`, which the `c=0.85` mutant's `~0.049` max delta slipped
/// under) — the fix belongs in the test's bound, not in this world constant.
/// This is decision 0016's forbidden shape mirrored: a world parameter
/// retuned to rescue a miscalibrated measurement. `0.85` is also the value
/// spec §5.6's "high" reads most naturally against, and matches spec §5.2's
/// own contextuality-endpoint labels once their inverted wording is
/// corrected (see the spec's own fix in fix round 1, and
/// [`WeftKind::contextuality`]'s doc).
/// plumb: universal(an authored design choice fixing how strongly spring/seep tracks macro state versus free noise, identical across every world)
const SPRING_CONTEXTUALITY: f64 = 0.85;

/// Soft-cap scale for blended drainage before it enters spring/seep's
/// `[0,1]` macro-state mix. `FieldPack::drainage` is an unbounded upstream-
/// vertex COUNT (its own documented range), not a `[0,1]` ratio like
/// `carbonate`; `tanh(drainage / SCALE)` saturates smoothly rather than
/// clamping, so two heavily-drained facets are still told apart the way two
/// lightly-drained ones are, right up to where drainage stops mattering at
/// all. **Not one of spec §5.2's three per-kind scalars** — a units
/// conversion the recipe needs to combine two differently-scaled causes, not
/// a design dial that trades against another kind's abundance/correlation/
/// contextuality.
/// plumb: universal(a units-conversion constant bringing an unbounded upstream-vertex count into the same [0,1] register carbonate already occupies; not itself a design dial)
const SPRING_DRAINAGE_SATURATION: f64 = 12.0;

/// Spring/seep's macro-state recipe (spec §5.6: "carbonate × drainage ×
/// elevation"): blended carbonate (karst solubility) times a saturating read
/// of blended drainage (channelization) — both must be present for a sign of
/// water underfoot, so the recipe multiplies rather than averages.
///
/// **`elevation` is spec §5.6's third listed cause and is not read here.**
/// `FieldPack` does not carry a raw elevation field (Task 4's report records
/// this as a deliberate, flagged scope decision, and Task 7's own
/// `land`-flag addition deliberately avoids reopening the elevation-
/// convention question rather than adding one — see `crate::fieldpack`'s
/// module doc) — this recipe reads the two causes that exist rather than
/// blocking on a third that would need a typed newtype added first. A later
/// task may add it.
fn spring_macro_state(carbonate: f64, drainage: f64) -> f64 {
    let wet = math::tanh(drainage / SPRING_DRAINAGE_SATURATION);
    (carbonate * wet).clamp(0.0, 1.0)
}

/// Abundance ceiling for overhang/hollow (spec §5.2). Lower than spring's —
/// a rock overhang big enough to shelter under and light a fire in is a
/// rarer landmark than a seep — and independently dialable (spec §5.2).
/// plumb: universal(an authored design ceiling on overhang/hollow frequency, fixed across every world and not derived from any seed or pin)
const OVERHANG_ABUNDANCE: f64 = 0.20;

/// Overhang/hollow's correlation length, in facets (spec §5.2, §5.6:
/// "short–medium"). Shorter than spring's `40.0`: a rock face's own
/// character changes over a smaller footprint than a karst zone's, but an
/// overhang is still a feature of a *stretch* of terrain, not a per-step
/// coin flip — hence "medium", not spring's own erratic-adjacent floor.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names overhang/hollow's correlation length "short-medium" and this is the chosen magnitude)
const OVERHANG_CORRELATION_LENGTH_FACETS: f64 = 15.0;

/// Overhang/hollow's contextuality (spec §5.2, §5.6: "medium — induration ×
/// slope"). Between spring's `0.85` (diagnostic of what is underfoot) and
/// erratic's near-zero (uncorrelated with any cause): an overhang is more
/// likely on hard, steep rock, but plenty of texture is legitimately free —
/// not every qualifying cliff face grows one.
/// plumb: universal(an authored design choice fixing how strongly overhang/hollow tracks macro state versus free noise, identical across every world)
const OVERHANG_CONTEXTUALITY: f64 = 0.5;

/// Soft-cap scale for blended slope (metres of fall per radian,
/// [`FieldPack::slope`]) before it enters overhang/hollow's `[0,1]`
/// macro-state mix — the same `tanh(x / SCALE)` saturation
/// [`SPRING_DRAINAGE_SATURATION`] uses for a different unbounded cause.
/// An order of magnitude gentler than
/// [`hornvale_terrain::GORGE_SLOPE`] (`40_000.0`, the slope at which a
/// channel's floodplain band fully closes): an overhang needs a steep rock
/// face, not a canyon wall, so this saturates well before terrain reaches
/// gorge-grade steepness. **Not one of spec §5.2's three per-kind scalars**
/// — a units-conversion constant, the same carve-out
/// [`SPRING_DRAINAGE_SATURATION`]'s own doc states.
/// plumb: universal(a units-conversion constant bringing an unbounded slope reading into the same [0,1] register induration already occupies; not itself a design dial)
const OVERHANG_SLOPE_SATURATION: f64 = 8_000.0;

/// Overhang/hollow's macro-state recipe (spec §5.6: "induration × slope"):
/// blended induration (rock hardness — a soft rock cannot hold its own
/// roof) times a saturating read of blended slope's steepness (a rock
/// overhang needs a steep face to undercut). `slope.abs()` because
/// [`hornvale_terrain::local_slope`] is signed (fall toward a downhill
/// target) and steepness itself is not directional — an overhang forms on a
/// steep face regardless of which way the local drainage happens to run.
fn overhang_macro_state(induration: f64, slope: f64) -> f64 {
    let steep = math::tanh(slope.abs() / OVERHANG_SLOPE_SATURATION);
    (induration * steep).clamp(0.0, 1.0)
}

/// Abundance ceiling for thicket/brake (spec §5.2). The highest of the four
/// — texture aimed directly at the biome-monotony defect wants real
/// coverage, not a rare landmark's sparse frequency — and independently
/// dialable (spec §5.2).
/// plumb: universal(an authored design ceiling on thicket/brake frequency, fixed across every world and not derived from any seed or pin)
const THICKET_ABUNDANCE: f64 = 0.45;

/// Thicket/brake's correlation length, in facets (spec §5.2, §5.6: "long").
/// The same order as spring/seep's own `40.0` — a stand of vegetation
/// persists over a comparable stretch of terrain to a karst zone — but
/// somewhat longer, since a productive belt (a river's gallery forest, a
/// whole windward slope) is often the larger of the two real-world
/// analogues.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names thicket/brake's correlation length "long" and this is the chosen magnitude)
const THICKET_CORRELATION_LENGTH_FACETS: f64 = 60.0;

/// Thicket/brake's contextuality (spec §5.2, §5.6: "high — productivity ×
/// moisture"). Matches spring/seep's own `0.85`: a thicket is diagnostic of
/// where the ground can actually support it, the same "sign case" posture
/// spring/seep has, just for vegetation instead of water.
/// plumb: universal(an authored design choice fixing how strongly thicket/brake tracks macro state versus free noise, identical across every world)
const THICKET_CONTEXTUALITY: f64 = 0.85;

/// The temperature (°C) at which thicket/brake's Liebig temperature
/// response peaks, mirroring `windows/locale`'s own Miami-model NPP proxy
/// constant (`NPP_TEMP_OPTIMUM_C`) — the same biologically-motivated
/// magnitude, independently owned here rather than imported, because
/// `hornvale-worldgen` may not depend on `windows/locale` (the same
/// layering [`super`]'s module doc states). Not required to track
/// `windows/locale`'s constant bit-for-bit: the two recipes answer
/// different questions (a room's food value vs. a facet's odds of carrying
/// standing vegetation) and are permitted to diverge if a future tuning
/// pass finds a reason to.
/// plumb: universal(an authored biologically-motivated design constant fixed across every world, independently owned from windows/locale's own analogous constant)
const THICKET_TEMP_OPTIMUM_C: f64 = 20.0;

/// The temperature tolerance (°C) either side of [`THICKET_TEMP_OPTIMUM_C`]
/// over which thicket/brake's Liebig temperature response falls linearly to
/// zero — mirrors `windows/locale`'s own `NPP_TEMP_TOLERANCE_C` for the same
/// reason and under the same independence [`THICKET_TEMP_OPTIMUM_C`]'s doc
/// states.
/// plumb: universal(an authored biologically-motivated design constant fixed across every world, independently owned from windows/locale's own analogous constant)
const THICKET_TEMP_TOLERANCE_C: f64 = 30.0;

/// Thicket/brake's macro-state recipe (spec §5.6: "productivity — temperature
/// × moisture"): a Miami-model net-primary-productivity proxy, computed
/// **blend-then-combine** — `temperature`/`moisture` are each already
/// blended by the caller ([`WeftKind::macro_state`]'s `Thicket` arm) before
/// this function combines them via a Liebig minimum, the same order
/// `windows/locale`'s `LocaleContext::productivity_with_weights` uses and
/// the exact order `crate::fieldpack`'s module doc forbids inverting (a
/// materialized `productivity` field would combine-then-blend instead, and
/// the minimum's non-linearity makes that a different quantity). This is an
/// independent reimplementation, not a call into `windows/locale` — the
/// same layering [`super`]'s module doc states forbids that dependency
/// direction outright.
fn thicket_macro_state(temperature_c: f64, moisture: f64) -> f64 {
    let temp_response = (1.0
        - (temperature_c - THICKET_TEMP_OPTIMUM_C).abs() / THICKET_TEMP_TOLERANCE_C)
        .clamp(0.0, 1.0);
    temp_response.min(moisture.clamp(0.0, 1.0))
}

/// Abundance ceiling for erratic/scatter (spec §5.2). The lowest of the
/// four — a glacially-deposited boulder or a wind-scoured outcrop is meant
/// to read as a rare, isolated find, not texture — and independently
/// dialable (spec §5.2).
/// plumb: universal(an authored design ceiling on erratic/scatter frequency, fixed across every world and not derived from any seed or pin)
const ERRATIC_ABUNDANCE: f64 = 0.08;

/// Erratic/scatter's correlation length, in facets (spec §5.2, §5.6:
/// "short"). The shortest of the four: a negative control with a long
/// correlation length would read as a smooth region of "sometimes an
/// erratic", which is a texture, not the isolated, spatially-continuous-
/// but-uncorrelated scatter spec §5.6 asks for.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names erratic/scatter's correlation length "short" and this is the chosen magnitude)
const ERRATIC_CORRELATION_LENGTH_FACETS: f64 = 5.0;

/// Erratic/scatter's contextuality — deliberately near-zero (spec §5.2,
/// §5.6: "low — mostly free noise"; Task 7, controller ruling R5). Under
/// the corrected §5.2 mapping (`0` = free noise, `1` = wallpaper), a small
/// but nonzero value keeps this kind exercising the same general mixing
/// recipe every other kind uses (spec §5.7: a kind is its recipe plus its
/// three scalars, never a special-cased fifth thing) while contributing
/// almost nothing: [`erratic_macro_state`] returns a spatially uninformative
/// constant, so even this kind's own tiny macro-state weight carries no
/// real signal — see that function's own doc. **Must never be raised to tie
/// this kind to macro state** — see [`WeftKind::contextuality`]'s doc for
/// why.
/// plumb: universal(an authored design choice fixing erratic/scatter as the negative control, near-zero macro tracking, identical across every world)
const ERRATIC_CONTEXTUALITY: f64 = 0.05;

/// Erratic/scatter's constant "macro state" — deliberately **not** a real
/// `FieldPack` scalar (Task 7, controller ruling R5). The negative control's
/// whole purpose is a prevalence surface uncorrelated with any macro cause,
/// so this returns the identical value at every facet regardless of the
/// ground beneath it. A constant carries no spatial information, so even
/// multiplied against [`ERRATIC_CONTEXTUALITY`]'s own small weight it
/// contributes no signal-bearing texture — only the free-noise term does.
///
/// **Never change this to read a real [`FieldPack`] field.** Doing so ties
/// the negative control to macro state, which is the exact "improvement"
/// Task 9's H3 legibility-discrimination check depends on this kind never
/// receiving: if erratic/scatter starts tracking a real cause, it stops
/// being a control and the metric loses the one designed case that proves
/// it can tell a real signal from none.
/// plumb: universal(a fixed, deliberately uninformative constant standing in for "no macro cause" — the negative control's whole point, identical across every world)
const ERRATIC_MACRO_BASELINE: f64 = 0.5;

/// Erratic/scatter's macro-state recipe: [`ERRATIC_MACRO_BASELINE`],
/// unconditionally. See that constant's own doc for why this is correct and
/// must stay this way.
fn erratic_macro_state() -> f64 {
    ERRATIC_MACRO_BASELINE
}
