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
    /// macro-state-driven, `0.0` as entirely free noise (spec §5.2: "0 =
    /// wallpaper, 1 = speckle"; erratic/scatter, Task 7, sits near the free-
    /// noise end, matching its own "mostly free noise" description).
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
/// **Not `0.85` — chosen at `0.7` because a higher value was measured to
/// defeat this campaign's own continuity guard.** At `0.85` (the first value
/// tried), noise contributes at most `(1 - 0.85) * SPRING_ABUNDANCE = 0.0525`
/// to any one step's prevalence delta, which is *smaller* than the guard's
/// bound needs to be to tolerate real macro-state texture — so an
/// address-hashed noise bug (measured by temporarily swapping
/// `Facet::centroid()` for `Facet::seed(seed).stream().next_f64()` in
/// `prevalence`, see `weft_prevalence.rs`'s test doc) produced a max delta of
/// only `~0.049` over 200 real-world steps, indistinguishable from genuine
/// texture. At `0.7` the same mutation produces a max delta of `~0.10`
/// (measured across 6 seed-42 starting vertices, 200 steps each) against a
/// real-mechanism max of `~0.0025` — a 40x margin — while still reading as
/// "high" relative to erratic/scatter's own "mostly free noise" (Task 7 must
/// keep its own contextuality well below this).
/// plumb: universal(an authored design choice fixing how strongly spring/seep tracks macro state versus free noise, identical across every world; the value is chosen so the campaign's own position-continuity guard actually discriminates an address-hashed regression, per the doc above)
const SPRING_CONTEXTUALITY: f64 = 0.7;

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
/// `FieldPack` does not carry it (Task 4's report records this as a
/// deliberate, flagged scope decision: the field pack's literal Produces
/// signature has exactly three fields, none of them elevation) — this recipe
/// reads the two causes that exist rather than blocking on a third that
/// would need `FieldPack` extended first. A later task may add it.
fn spring_macro_state(carbonate: f64, drainage: f64) -> f64 {
    let wet = math::tanh(drainage / SPRING_DRAINAGE_SATURATION);
    (carbonate * wet).clamp(0.0, 1.0)
}
