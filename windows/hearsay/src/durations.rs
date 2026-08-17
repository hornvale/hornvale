//! Per-people social durations, as plain data.
//!
//! This type deliberately performs NO lookup. The allometric derivation lives
//! at the composition root (`windows/worldgen`), which `windows/hearsay`'s
//! library may not depend on — see this crate's `Cargo.toml`. The caller
//! assembles the table and hands it in; the window reads only what it is
//! given, which is the same posture every other window takes toward the
//! ledger.

use hornvale_astronomy::units::StdDays;
use std::collections::BTreeMap;

/// Each people's generation length and lifespan, in std days.
#[derive(Clone, Debug, Default)]
pub struct PeopleDurations {
    /// people label -> (generation, lifespan); either may be absent.
    by_people: BTreeMap<String, (Option<StdDays>, Option<StdDays>)>,
}

impl PeopleDurations {
    /// Record one people's durations, replacing any earlier entry.
    /// type-audit: bare-ok(identifier-text: people)
    pub fn insert(&mut self, people: &str, generation: Option<StdDays>, lifespan: Option<StdDays>) {
        self.by_people
            .insert(people.to_string(), (generation, lifespan));
    }

    /// This people's `(generation, lifespan)`, both `None` when unknown.
    ///
    /// An absent people is not an error: a world may carry a kind with no
    /// mass-derived life history at all, exactly as a moonless world carries
    /// no lunar rung.
    /// type-audit: bare-ok(identifier-text: people)
    pub fn get(&self, people: &str) -> (Option<StdDays>, Option<StdDays>) {
        self.by_people.get(people).copied().unwrap_or((None, None))
    }

    /// Every people with an entry, ascending — the readout iterates this.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn peoples(&self) -> Vec<&str> {
        self.by_people.keys().map(|s| s.as_str()).collect()
    }
}
