//! Seed-derivation labels for astronomy (permanent contracts, spec §3).

use hornvale_kernel::seed::StreamLabel;

/// Root stream label for astronomy.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOT: StreamLabel<'static> = StreamLabel::from_static("astronomy");
/// Star mass draw.
/// type-audit: bare-ok(identifier-text: return)
pub const STAR_MASS: StreamLabel<'static> = StreamLabel::from_static("star-mass");
/// Anchor mass draw.
/// type-audit: bare-ok(identifier-text: return)
pub const ANCHOR_MASS: StreamLabel<'static> = StreamLabel::from_static("anchor-mass");
/// Rotation regime/period draw.
/// type-audit: bare-ok(identifier-text: return)
pub const ROTATION: StreamLabel<'static> = StreamLabel::from_static("rotation");
/// Anchor orbital-distance draw.
/// type-audit: bare-ok(identifier-text: return)
pub const ORBIT: StreamLabel<'static> = StreamLabel::from_static("orbit");
/// Obliquity draw.
/// type-audit: bare-ok(identifier-text: return)
pub const OBLIQUITY: StreamLabel<'static> = StreamLabel::from_static("obliquity");
/// Moon count draw.
/// type-audit: bare-ok(identifier-text: return)
pub const MOON_COUNT: StreamLabel<'static> = StreamLabel::from_static("moon-count");
/// Per-moon parameter draws (one stream reused across attempts).
/// type-audit: bare-ok(identifier-text: return)
pub const MOONS: StreamLabel<'static> = StreamLabel::from_static("moons");
/// Neighborhood draws.
/// type-audit: bare-ok(identifier-text: return)
pub const NEIGHBORS: StreamLabel<'static> = StreamLabel::from_static("neighbors");
/// Deep-time orbital-forcing draws (eccentricity + obliquity oscillation + precession).
/// type-audit: bare-ok(identifier-text: return)
pub const FORCING: StreamLabel<'static> = StreamLabel::from_static("forcing");
/// Per-body genesis phase offsets (year, day, and each moon).
/// type-audit: bare-ok(identifier-text: return)
pub const PHASE_OFFSETS: StreamLabel<'static> = StreamLabel::from_static("phase-offsets");
/// Per-neighbor celestial position draws (declination, right ascension).
/// type-audit: bare-ok(identifier-text: return)
pub const NEIGHBOR_POSITIONS: StreamLabel<'static> = StreamLabel::from_static("neighbor-positions");
/// Spin-direction draw: prograde or retrograde (SKY-22).
/// type-audit: bare-ok(identifier-text: return)
pub const SPIN_DIRECTION: StreamLabel<'static> = StreamLabel::from_static("spin-direction");
/// Per-moon orbital-inclination draws (SKY-6).
/// type-audit: bare-ok(identifier-text: return)
pub const MOON_INCLINATIONS: StreamLabel<'static> = StreamLabel::from_static("moon-inclinations");
/// Wanderer count draw.
/// type-audit: bare-ok(identifier-text: return)
pub const WANDERER_COUNT: StreamLabel<'static> = StreamLabel::from_static("wanderer-count");
/// Per-wanderer parameter draws, sequential (no attempts loop —
/// `generate_wanderers` draws each wanderer's parameters once, in order).
/// type-audit: bare-ok(identifier-text: return)
pub const WANDERERS: StreamLabel<'static> = StreamLabel::from_static("wanderers");
/// Background starfield draws: count, then per-star position/brightness (derived catalog — consumed on demand, never in genesis).
/// type-audit: bare-ok(identifier-text: return)
pub const STARFIELD: StreamLabel<'static> = StreamLabel::from_static("starfield");
/// Per-moon ascending-node longitude draws (Eclipse Seasons).
/// type-audit: bare-ok(identifier-text: return)
pub const MOON_NODES: StreamLabel<'static> = StreamLabel::from_static("moon-nodes");
/// Stellar age draw (The Reckoning).
/// type-audit: bare-ok(identifier-text: return)
pub const STAR_AGE: StreamLabel<'static> = StreamLabel::from_static("star-age");
/// Per-moon formation-mechanism draw (The Reckoning). Drawn after admission
/// and the distance sort, so count/mass/distance stay byte-identical.
/// type-audit: bare-ok(identifier-text: return)
pub const MOON_FORMATION: StreamLabel<'static> = StreamLabel::from_static("moon-formation");
/// Per-moon density draw (The Reckoning). Drawn after formation, one draw
/// per moon in every branch — a `GiantImpact` moon's density is a derived
/// constant, not a drawn one, but it still consumes a draw so that moon
/// *i*'s density stream position never depends on how many earlier moons
/// drew `Capture`.
/// type-audit: bare-ok(identifier-text: return)
pub const MOON_DENSITY: StreamLabel<'static> = StreamLabel::from_static("moon-density");
/// Per-moon age draw (The Reckoning). Drawn after formation, one draw per
/// moon in every branch, for the same index-stability reason as
/// [`MOON_DENSITY`].
/// type-audit: bare-ok(identifier-text: return)
pub const MOON_AGE: StreamLabel<'static> = StreamLabel::from_static("moon-age");

#[cfg(test)]
mod tests {
    use super::*;

    /// Every `pub const` declared in this file. Rust has no reflection, so
    /// this list is maintained by hand — but the test below turns "forgot
    /// to publish a new label" into a hard failure instead of a silent gap.
    /// This is the closest honest cross-check without reflection or a
    /// build-script/macro (I1, The Reckoning code review: `STAR_AGE` was
    /// added here but omitted from `stream_labels()` in `lib.rs`, so the
    /// generated stream-manifest page silently under-documented a frozen
    /// save-format contract, and nothing caught it).
    /// ROOT is a StreamLabel, and so — as of this crate's PROC-17 migration
    /// — is every other constant here; all are handled via `.as_str()`
    /// below.
    const ALL_LABELS: &[StreamLabel<'static>] = &[
        STAR_MASS,
        ANCHOR_MASS,
        ROTATION,
        ORBIT,
        OBLIQUITY,
        MOON_COUNT,
        MOONS,
        NEIGHBORS,
        FORCING,
        PHASE_OFFSETS,
        NEIGHBOR_POSITIONS,
        SPIN_DIRECTION,
        MOON_INCLINATIONS,
        WANDERER_COUNT,
        WANDERERS,
        STARFIELD,
        MOON_NODES,
        STAR_AGE,
        MOON_FORMATION,
        MOON_DENSITY,
        MOON_AGE,
    ];

    /// Every label constant in this file must appear (root-qualified, since
    /// `stream_labels()` publishes `"astronomy/<label>"` for every draw
    /// except the root itself) in `crate::stream_labels()`. Add a const here
    /// without adding it to `ALL_LABELS` and to `stream_labels()`, and this
    /// fails.
    #[test]
    fn every_stream_label_constant_is_published_in_stream_labels() {
        let published: std::collections::BTreeSet<&str> = crate::stream_labels()
            .into_iter()
            .map(|(label, _)| label)
            .collect();
        // Check ROOT separately (it's a StreamLabel, not &str)
        let root_str = ROOT.as_str();
        assert!(
            published.contains(root_str),
            "stream label constant ROOT (qualified {root_str:?}) is missing from \
             stream_labels() — publish it or the generated stream-manifest page silently \
             under-documents a frozen save-format contract"
        );
        // Check all other labels
        for &label in ALL_LABELS {
            let label = label.as_str();
            let qualified = format!("astronomy/{label}");
            assert!(
                published.contains(qualified.as_str()),
                "stream label constant {label:?} (qualified {qualified:?}) is missing from \
                 stream_labels() — publish it or the generated stream-manifest page silently \
                 under-documents a frozen save-format contract"
            );
        }
        assert_eq!(
            published.len(),
            ALL_LABELS.len() + 1, // +1 for ROOT
            "stream_labels() and ALL_LABELS have diverged in count — update ALL_LABELS in \
             streams.rs to match"
        );
    }
}
