//! Read-only counts for the world's historical and present population layers.

use hornvale_kernel::{EntityId, Vertex, World};
use std::collections::BTreeSet;

/// Committed occupations and the columns they occupy, by population layer.
///
/// Historical membership includes every reconstructed occupation record,
/// including ended occupations. Present membership includes only records that
/// are still living. A column appears once in each layer regardless of how
/// many occupation records share its site.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PopulationCensus {
    /// Identities of all committed historical occupation records.
    pub historical_occupations: BTreeSet<EntityId>,
    /// Identities of historical occupation records that are still living.
    pub present_occupations: BTreeSet<EntityId>,
    /// Columns occupied at any point in recorded history.
    pub historical_columns: BTreeSet<Vertex>,
    /// Columns occupied by a living occupation.
    pub present_columns: BTreeSet<Vertex>,
}

/// Count historical and present population layers from committed occupations.
///
/// The ledger is decoded once through [`crate::history_emit::occupation_records`].
/// The readout is deterministic: it performs no draws, and ordered sets make
/// column de-duplication independent of record insertion order.
pub fn population_census(world: &World) -> PopulationCensus {
    let records = crate::history_emit::occupation_records(world);
    let mut historical_occupations = BTreeSet::new();
    let mut present_occupations = BTreeSet::new();
    let mut historical_columns = BTreeSet::new();
    let mut present_columns = BTreeSet::new();

    for occupation in &records {
        historical_occupations.insert(occupation.id);
        historical_columns.insert(occupation.core.site);
        if occupation.is_alive() {
            present_occupations.insert(occupation.id);
            present_columns.insert(occupation.core.site);
        }
    }

    PopulationCensus {
        historical_occupations,
        present_occupations,
        historical_columns,
        present_columns,
    }
}
