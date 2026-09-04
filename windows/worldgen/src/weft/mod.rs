//! The derived surface: prevalence and occurrence, position-continuous by
//! construction (The Weft, Task 5; spec §5.1).
//!
//! Two stages per kind, mirroring `cave_process -> presence_prob` one
//! resolution down:
//!
//! 1. **Prevalence** ([`prevalence`]) — macro state
//!    ([`hornvale_kernel::blend_corner_weights`] over a [`crate::FieldPack`]
//!    scalar, per [`WeftKind::macro_state`](kinds::WeftKind)) mixed with a
//!    position-continuous [`hornvale_terrain::SphereFbm`] sample, in `[0,1]`.
//! 2. **Occurrence** ([`occurs`]) — a SECOND, decorrelated position-
//!    continuous sample decides whether the facet actually carries a
//!    feature, against the prevalence just computed.
//!
//! **Every noise sample is passed through [`uniformize`](hornvale_terrain::features::uniformize)
//! before use (fix round 1, F1).** `SphereFbm::sample` is the mean of three
//! fBm slices, so its marginal is concentrated near 0.5 (SD ≈0.076), not
//! spread over `[0,1]` — comparing it raw against a probability, as `occurs`
//! did before this fix, can never fire once the probability is smaller than
//! the field's own floor. `GeneratedTerrain::cave_at` established the fix
//! this module now shares: `uniformize` maps the same three-slice
//! construction (any frequency, `CAVE_GATE_OCTAVES`-many octaves — this
//! module's `WEFT_OCTAVES` matches) onto a genuine `[0,1]` uniform variate,
//! monotonically, so spatial clustering is untouched and the marginal is
//! corrected. See that function's own doc for the calibration (measured over
//! 64 level-5 globes) and why it must not be re-derived here.
//!
//! **Noise is keyed on POSITION, never on [`Facet::seed`].** `Facet::seed`
//! is address-hashed by design
//! (`kernel/src/room.rs`: "derived from the integer address only ... so all
//! room content is platform-exact") — exactly the trap this module must not
//! fall into. Two geometrically adjacent facets can carry unrelated integer
//! addresses (a cube-face seam, a quadtree digit boundary), so an address-
//! hashed draw decorrelates neighbours and the surface reads as speckle —
//! the failure `channel-band-monotonicity` names as "address-hashed noise
//! leaked into a band edge". Every sample in this module reads
//! [`Facet::centroid`].
//!
//! **No `&LocaleContext` here, ever (controller ruling, Task 5).**
//! `windows/locale` depends on `hornvale-worldgen`, so a `LocaleContext`
//! parameter here would be a circular edge. Every function instead takes
//! `geo: &Geosphere, index: &NearestVertexIndex` — exactly what
//! `LocaleContext::blend_at` reads internally to do the same corner-weight
//! blend, so nothing about the *mechanism* changes, only where its inputs
//! come from.
//!
//! **Nothing normalises across kinds** (spec §5.2). Per-kind prevalences do
//! not sum to 1 — a simplex constraint would make raising one kind's
//! abundance necessarily lower another's, the structural cap on enterable
//! density the spec forbids.
//!
//! **[`prevalence`] gates on ground eligibility before either stage runs
//! (Task 7, controller ruling R1).** Task 5's review measured 59% of all
//! spring occurrences landing on facets with no macro cause at all,
//! including open ocean — the lerp's `(1 - contextuality) * noise` floor is
//! real and unconditional, so nothing stopped it. [`kinds::WeftKind::eligible`]
//! is the fix, an early ground test every kind now shares before macro state
//! is read or noise is drawn — see `kinds`'s own module doc.

pub mod kinds;
pub mod window;

pub use kinds::WeftKind;
pub use window::{WeftFeature, WeftKey, WeftWindow, all_features_at_cached, features_at_cached};

use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Facet, Geosphere, NearestVertexIndex, Seed};
use hornvale_terrain::SphereFbm;

use crate::FieldPack;

/// fBm octaves for every weft noise sample. Matches
/// `domains/terrain::crust`'s own feature-scale octave counts (e.g.
/// `LOBE_OCTAVES`) rather than the single octave `value_noise_2d` alone
/// would give — enough to break up single-octave blockiness at facet scale.
/// Not tuned per world.
/// plumb: universal(matches domains/terrain's own SphereFbm octave counts for feature-scale noise; identical across every world)
const WEFT_OCTAVES: u32 = 4;

/// The dynamic sub-leg naming prevalence's modulating noise, under a kind's
/// own [`WeftKind::stream_label`] — see that method's doc for why this is a
/// dynamic leg and not a second registered label.
const PREVALENCE_LEG: &str = "prevalence";

/// The dynamic sub-leg naming occurrence's threshold noise — decorrelated
/// from [`PREVALENCE_LEG`] by construction, since the two derive under
/// different leg strings from the same kind root (spec §5.1: "a second,
/// decorrelated ... sample").
const OCCURRENCE_LEG: &str = "occurrence";

/// The [`SphereFbm`] frequency giving `kind`'s correlation length (spec
/// §5.2's "how far you walk before the answer changes, in facets") at
/// `facet`'s own scale: a full noise-lattice cycle spans
/// `correlation_length_facets * facet.edge_rad()` — coordinate distance,
/// which for the tiny angles a walk-band facet subtends is indistinguishable
/// from great-circle distance (chord length ≈ arc length as the angle → 0).
/// [`Facet::edge_rad`] is the promoted, single-implementation form of what
/// used to be a local duplicate here (The Weft, fix round 1, F4) — see its
/// own doc for the promotion history.
fn noise_frequency_for(kind: WeftKind, facet: &Facet) -> f64 {
    let edge = facet.edge_rad().max(f64::EPSILON);
    1.0 / (kind.correlation_length_facets() * edge)
}

/// `prevalence ∈ [0,1]` at `facet`: [`WeftKind::macro_state`] over `pack`,
/// mixed against a position-continuous noise sample by the kind's own
/// [`WeftKind::contextuality`], scaled by [`WeftKind::abundance`].
///
/// `None` exactly when [`Facet::corner_weights`] is — `facet` shallower than
/// `geo`'s own level, which has nothing to blend between. `Some(0.0)` when
/// [`Facet::corner_weights`] succeeds but `kind` is not eligible at this
/// ground (Task 7, R1 — see `kinds`'s own module doc): a real, measurable
/// zero rather than skipping the facet, computed *before* any macro-state
/// read or noise draw.
/// type-audit: bare-ok(ratio: return)
pub fn prevalence(
    kind: WeftKind,
    facet: &Facet,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    pack: &FieldPack,
    seed: Seed,
) -> Option<f64> {
    let weights = facet.corner_weights(geo, index)?;
    if !kind.eligible(weights, pack) {
        return Some(0.0);
    }
    let macro_state = kind.macro_state(weights, pack);

    let noise_seed = seed
        .derive(kind.stream_label())
        .derive(StreamLabel::dynamic(PREVALENCE_LEG));
    let fbm = SphereFbm::new(noise_seed, noise_frequency_for(kind, facet), WEFT_OCTAVES);
    let noise = hornvale_terrain::features::uniformize(fbm.sample(facet.centroid()));

    let contextuality = kind.contextuality();
    let mixed = contextuality * macro_state + (1.0 - contextuality) * noise;
    Some((kind.abundance() * mixed).clamp(0.0, 1.0))
}

/// Whether `facet` actually carries a `kind` feature, given a `prevalence`
/// already computed for it (typically [`prevalence`]'s own answer): a
/// SECOND position-continuous sample, drawn under [`OCCURRENCE_LEG`] — a
/// different stream leg from [`prevalence`]'s own [`PREVALENCE_LEG`], so the
/// two draws are decorrelated — compared against `p`.
///
/// **Uniformized before the comparison (fix round 1, F1).** A raw
/// `SphereFbm::sample` almost never falls under a small `p` (its marginal
/// sits near 0.5 with SD ≈0.076), which is why `occurs` fired zero times in
/// 21,640 seed-42 walk facets before this fix — the threshold was ~5.7
/// standard deviations into a tail the raw field does not have. Comparing
/// the uniformized value instead makes this a genuine Bernoulli trial at
/// rate `p`, matching `GeneratedTerrain::cave_at`'s own gate.
/// type-audit: bare-ok(ratio: p), bare-ok(flag: return)
pub fn occurs(kind: WeftKind, facet: &Facet, seed: Seed, p: f64) -> bool {
    let noise_seed = seed
        .derive(kind.stream_label())
        .derive(StreamLabel::dynamic(OCCURRENCE_LEG));
    let fbm = SphereFbm::new(noise_seed, noise_frequency_for(kind, facet), WEFT_OCTAVES);
    hornvale_terrain::features::uniformize(fbm.sample(facet.centroid())) < p
}
