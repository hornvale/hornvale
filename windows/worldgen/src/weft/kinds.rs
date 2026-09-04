//! A derived-feature kind, as data — spec §5.7: "a kind is three things and
//! nothing else — a component bundle, a prevalence recipe, and the three
//! scalars." This file carries the recipe and the scalars; the component
//! bundle (the affordance vocabulary a placed feature carries, `ObjectProperty`
//! / `object_registry`) is Task 7's addition, once an enterable/affording kind
//! exists to need one.
//!
//! **Only [`WeftKind::Spring`] exists through Task 5.** Spec §5.7's whole
//! point is that kind N+1 is an append: Task 7 adds the remaining three
//! (overhang/hollow, thicket/brake, erratic/scatter) as new match arms
//! alongside this one, never by editing it.
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
//! and this file's `land_eligible` for the shared ground test.

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
}

impl WeftKind {
    /// Every kind that exists, in a fixed order. [`super::window::WeftWindow`]
    /// enumerates this once per facet entering its radius (Task 6) — the one
    /// place a new kind must be added for the residency window to pick it up.
    /// Task 7 grows this by appending a new element alongside a new match arm
    /// in every method below, never by editing an existing entry (spec §5.7:
    /// "kind N+1 is an append").
    pub const ALL: [WeftKind; 1] = [WeftKind::Spring];

    /// This kind's seed-derivation root leg (a save-format contract; see
    /// `windows/worldgen/src/streams.rs`). [`super::prevalence`] and
    /// [`super::occurs`] each derive their own decorrelated sub-leg under it.
    pub(crate) fn stream_label(self) -> StreamLabel<'static> {
        match self {
            WeftKind::Spring => streams::WEFT_SPRING,
        }
    }

    /// The abundance ceiling — `prevalence`'s maximum possible answer for
    /// this kind, reached only where macro state and noise both saturate.
    pub(crate) fn abundance(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_ABUNDANCE,
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
        }
    }

    /// The macro-state/free-noise mixing weight: `1.0` reads as entirely
    /// macro-state-driven ("wallpaper" in the sense of a smooth, place-
    /// grounded texture), `0.0` as entirely free noise — position-continuous
    /// but uncorrelated with any macro cause. Erratic/scatter (Task 7) sits
    /// near the `0.0` end, matching its own §5.6 description ("low — mostly
    /// free noise").
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
    pub(crate) fn contextuality(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_CONTEXTUALITY,
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
        }
    }

    /// Whether `self` may occur at all at `weights`' blended ground — the
    /// early eligibility gate [`super::prevalence`] tests *before* reading a
    /// macro-state cause or drawing any noise (Task 7, R1; see this file's
    /// own module doc).
    pub(crate) fn eligible(self, weights: [(Vertex, u64); 4], pack: &FieldPack) -> bool {
        match self {
            WeftKind::Spring => land_eligible(weights, pack),
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

/// The shared ground-eligibility test [`WeftKind::eligible`] delegates to
/// (Task 7, R1): `weights`' blend of [`FieldPack::land`] at or above
/// [`LAND_ELIGIBILITY_THRESHOLD`]. `pub(crate)` rather than a private free
/// function so `crate::fieldpack`'s own module doc can point at it by name.
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
/// control (Task 7).
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
/// [`Self::contextuality`]'s doc).
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
