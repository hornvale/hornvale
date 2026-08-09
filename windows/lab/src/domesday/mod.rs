//! The Domesday — a generated survey of the thousand census worlds.
//!
//! Reads the COMMITTED census (`rows.csv` + `schema.json`) and never re-runs
//! it: decision 0032 established that the gate loads the committed fixture,
//! and 0110 that the census is the suite's shared world-building pass.

pub mod census;
pub mod stats;

#[cfg(test)]
mod guard {
    /// The census reader must never gain the means to rebuild a world.
    ///
    /// Scanned from *here*, not from inside `census.rs`'s own test module:
    /// this forbidden-word list, if embedded via `include_str!` in the same
    /// file that states it, would always find itself — a quine, not a
    /// guard. Keeping the assertion and the scanned text in separate files
    /// is what makes a genuine regression (someone importing `BuildDepth`
    /// into `census.rs`) distinguishable from the check merely finding its
    /// own source.
    #[test]
    fn loading_never_builds_a_world() {
        let src = include_str!("census.rs");
        for forbidden in ["build_world", "BuildDepth", "build_to", "RunResult"] {
            assert!(
                !src.contains(forbidden),
                "the census reader must not construct worlds; found {forbidden}"
            );
        }
    }
}
