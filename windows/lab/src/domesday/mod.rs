//! The Domesday — a generated survey of the thousand census worlds.
//!
//! Reads the COMMITTED census (`rows.csv` + `schema.json`) and never re-runs
//! it: decision 0032 established that the gate loads the committed fixture,
//! and 0110 that the census is the suite's shared world-building pass.

pub mod anomaly;
pub mod census;
pub mod comparators;
pub mod detect;
pub mod render;
pub mod stats;

#[cfg(test)]
mod guard {
    /// The census reader — and the anomaly report that reads through it —
    /// must never gain the means to rebuild a world.
    ///
    /// Scanned from *here*, not from inside `census.rs`'s or `anomaly.rs`'s
    /// own test modules: this forbidden-word list, if embedded via
    /// `include_str!` in the same file that states it, would always find
    /// itself — a quine, not a guard. Keeping the assertion and the scanned
    /// text in separate files is what makes a genuine regression (someone
    /// importing `BuildDepth` into `census.rs` or `anomaly.rs`)
    /// distinguishable from the check merely finding its own source.
    #[test]
    fn loading_never_builds_a_world() {
        for (name, src) in [
            ("census.rs", include_str!("census.rs")),
            ("anomaly.rs", include_str!("anomaly.rs")),
        ] {
            for forbidden in ["build_world", "BuildDepth", "build_to", "RunResult"] {
                assert!(
                    !src.contains(forbidden),
                    "{name} must not construct worlds; found {forbidden}"
                );
            }
        }
    }

    /// Every detector name `detect.rs` emits appears in `DECLARED_DETECTORS`.
    ///
    /// **The direction this enforces, and the one it does not.** It checks
    /// `observed ⊆ declared`: a *new or renamed* detector that is missing
    /// from the roster fails here, because the survey could then only learn
    /// its name by seeing it fire and so could never publish its zero. It is
    /// deliberately blind to the opposite direction — a declared name no
    /// detector emits passes, because the index renders `declared ∪
    /// observed` and an over-declaration becomes a harmless `0` row, not a
    /// false claim. A green result therefore means "nothing emits an
    /// undeclared name", NOT "the roster is exactly right".
    ///
    /// Scanned from here rather than from inside `detect.rs`'s own test
    /// module, for the reason `loading_never_builds_a_world` states above: a
    /// scan pattern written into the file it scans always finds itself.
    #[test]
    fn every_emitted_detector_name_is_declared() {
        use crate::domesday::detect::DECLARED_DETECTORS;
        use std::collections::BTreeSet;

        let src = include_str!("detect.rs");
        let mut emitted: BTreeSet<&str> = BTreeSet::new();
        for tail in src.split("detector: \"").skip(1) {
            let (name, _) = tail
                .split_once('"')
                .expect("a string literal closes its quote");
            emitted.insert(name);
        }
        // Anti-vacuity: if the construction syntax ever changes, the scan
        // finds nothing and the loop below passes without asserting anything.
        assert!(
            emitted.contains("D1"),
            "the scan matched no D1 finding, so it is no longer reading detect.rs's \
             emitted names and this check asserts nothing (found {emitted:?})"
        );
        for name in &emitted {
            assert!(
                DECLARED_DETECTORS.contains(name),
                "detect.rs emits the detector name {name:?}, which is missing from \
                 DECLARED_DETECTORS; add it there so the survey can publish its count \
                 even when it is zero"
            );
        }
    }
}
