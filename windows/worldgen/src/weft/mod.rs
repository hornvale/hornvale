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

pub mod kinds;

pub use kinds::WeftKind;

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

/// The angular length (radians) of `facet`'s shortest edge. Duplicates the
/// formula `windows/locale::room_edge` uses (min pairwise great-circle
/// separation of the four corners) rather than importing it, because that
/// crate depends on this one (the same circularity the module doc's
/// `LocaleContext` ruling names) — there is no lower layer either could share
/// it through without promoting it out of `windows/locale` entirely, which
/// this task does not need.
fn facet_edge_rad(facet: &Facet) -> f64 {
    let [a, b, c, d] = facet.corners();
    let sep = |u: [f64; 3], v: [f64; 3]| -> f64 {
        let dp: f64 = u[0] * v[0] + u[1] * v[1] + u[2] * v[2];
        hornvale_kernel::math::acos(dp.clamp(-1.0, 1.0))
    };
    sep(a, b).min(sep(b, c)).min(sep(c, d)).min(sep(d, a))
}

/// The [`SphereFbm`] frequency giving `kind`'s correlation length (spec
/// §5.2's "how far you walk before the answer changes, in facets") at
/// `facet`'s own scale: a full noise-lattice cycle spans
/// `correlation_length_facets * facet_edge_rad` — coordinate distance, which
/// for the tiny angles a walk-band facet subtends is indistinguishable from
/// great-circle distance (chord length ≈ arc length as the angle → 0).
fn noise_frequency_for(kind: WeftKind, facet: &Facet) -> f64 {
    let edge = facet_edge_rad(facet).max(f64::EPSILON);
    1.0 / (kind.correlation_length_facets() * edge)
}

/// `prevalence ∈ [0,1]` at `facet`: [`WeftKind::macro_state`] over `pack`,
/// mixed against a position-continuous noise sample by the kind's own
/// [`WeftKind::contextuality`], scaled by [`WeftKind::abundance`].
///
/// `None` exactly when [`Facet::corner_weights`] is — `facet` shallower than
/// `geo`'s own level, which has nothing to blend between.
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
    let macro_state = kind.macro_state(weights, pack);

    let noise_seed = seed
        .derive(kind.stream_label())
        .derive(StreamLabel::dynamic(PREVALENCE_LEG));
    let fbm = SphereFbm::new(noise_seed, noise_frequency_for(kind, facet), WEFT_OCTAVES);
    let noise = fbm.sample(facet.centroid());

    let contextuality = kind.contextuality();
    let mixed = contextuality * macro_state + (1.0 - contextuality) * noise;
    Some((kind.abundance() * mixed).clamp(0.0, 1.0))
}

/// Whether `facet` actually carries a `kind` feature, given a `prevalence`
/// already computed for it (typically [`prevalence`]'s own answer): a
/// SECOND position-continuous sample, drawn under [`OCCURRENCE_LEG`] — a
/// different stream leg from [`prevalence`]'s own [`PREVALENCE_LEG`], so the
/// two draws are decorrelated — compared against `p`.
/// type-audit: bare-ok(ratio: p), bare-ok(flag: return)
pub fn occurs(kind: WeftKind, facet: &Facet, seed: Seed, p: f64) -> bool {
    let noise_seed = seed
        .derive(kind.stream_label())
        .derive(StreamLabel::dynamic(OCCURRENCE_LEG));
    let fbm = SphereFbm::new(noise_seed, noise_frequency_for(kind, facet), WEFT_OCTAVES);
    fbm.sample(facet.centroid()) < p
}
