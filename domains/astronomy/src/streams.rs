//! Seed-derivation labels for astronomy (permanent contracts, spec §3).

/// Root stream label for astronomy.
/// type-audit: bare-ok(identifier-text)
pub const ROOT: &str = "astronomy";
/// Star mass draw.
/// type-audit: bare-ok(identifier-text)
pub const STAR_MASS: &str = "star-mass";
/// Anchor mass draw.
/// type-audit: bare-ok(identifier-text)
pub const ANCHOR_MASS: &str = "anchor-mass";
/// Rotation regime/period draw.
/// type-audit: bare-ok(identifier-text)
pub const ROTATION: &str = "rotation";
/// Anchor orbital-distance draw.
/// type-audit: bare-ok(identifier-text)
pub const ORBIT: &str = "orbit";
/// Obliquity draw.
/// type-audit: bare-ok(identifier-text)
pub const OBLIQUITY: &str = "obliquity";
/// Moon count draw.
/// type-audit: bare-ok(identifier-text)
pub const MOON_COUNT: &str = "moon-count";
/// Per-moon parameter draws (one stream reused across attempts).
/// type-audit: bare-ok(identifier-text)
pub const MOONS: &str = "moons";
/// Neighborhood draws.
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBORS: &str = "neighbors";
/// Deep-time orbital-forcing draws (eccentricity + obliquity oscillation + precession).
/// type-audit: bare-ok(identifier-text)
pub const FORCING: &str = "forcing";
/// Per-body genesis phase offsets (year, day, and each moon).
/// type-audit: bare-ok(identifier-text)
pub const PHASE_OFFSETS: &str = "phase-offsets";
/// Per-neighbor celestial position draws (declination, right ascension).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_POSITIONS: &str = "neighbor-positions";
/// Spin-direction draw: prograde or retrograde (SKY-22).
/// type-audit: bare-ok(identifier-text)
pub const SPIN_DIRECTION: &str = "spin-direction";
/// Per-moon orbital-inclination draws (SKY-6).
/// type-audit: bare-ok(identifier-text)
pub const MOON_INCLINATIONS: &str = "moon-inclinations";
/// Wanderer count draw.
/// type-audit: bare-ok(identifier-text)
pub const WANDERER_COUNT: &str = "wanderer-count";
/// Per-wanderer parameter draws, sequential (no attempts loop —
/// `generate_wanderers` draws each wanderer's parameters once, in order).
/// type-audit: bare-ok(identifier-text)
pub const WANDERERS: &str = "wanderers";
/// Background starfield draws: count, then per-star position/brightness (derived catalog — consumed on demand, never in genesis).
/// type-audit: bare-ok(identifier-text)
pub const STARFIELD: &str = "starfield";
/// Per-moon ascending-node longitude draws (Eclipse Seasons).
/// type-audit: bare-ok(identifier-text)
pub const MOON_NODES: &str = "moon-nodes";
/// Stellar age draw (The Reckoning).
/// type-audit: bare-ok(identifier-text)
pub const STAR_AGE: &str = "star-age";

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
    const ALL_LABELS: &[&str] = &[
        ROOT,
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
        for &label in ALL_LABELS {
            let qualified = if label == ROOT {
                label.to_string()
            } else {
                format!("astronomy/{label}")
            };
            assert!(
                published.contains(qualified.as_str()),
                "stream label constant {label:?} (qualified {qualified:?}) is missing from \
                 stream_labels() — publish it or the generated stream-manifest page silently \
                 under-documents a frozen save-format contract"
            );
        }
        assert_eq!(
            published.len(),
            ALL_LABELS.len(),
            "stream_labels() and ALL_LABELS have diverged in count — update ALL_LABELS in \
             streams.rs to match"
        );
    }
}
