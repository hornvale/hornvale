//! Projection metadata and the persistent-consequence boundary.
//!
//! A projection is a view of a substrate cohort, not a substitute population
//! count: its selection lens and materiality describe how the reader obtained
//! an account of that cohort and whether consequences may write back.

use hornvale_kernel::{KindId, Vertex};

/// The substrate cohort a projection came from.
/// type-audit: bare-ok(count: year)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SourceCohort {
    /// Host people.
    pub people: KindId,
    /// Cohort site.
    pub site: Vertex,
    /// Cohort year.
    pub year: f64,
}

/// Whether the projection is analytical, in-world, aggregate, or materialized.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProjectionMateriality {
    /// Persistent aggregate state.
    AggregateState,
    /// An out-of-world analytical construction.
    Analytical,
    /// An in-world representative construction, still non-causal.
    InWorld,
    /// A materialized individual.
    MaterializedIndividual,
}

/// A projection's form.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProjectionKind {
    /// Aggregate readout.
    Aggregate,
    /// Composite case.
    Composite,
    /// Materialized individual.
    MaterializedIndividual,
    /// Salient character.
    SalientCharacter,
}

/// Selection lens used to obtain the projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SelectionLens {
    /// Population-weighted selection.
    PopulationWeighted,
    /// Representative selection.
    Representative,
    /// Salience-weighted selection.
    Salient,
}

/// Sampling bias declared by the projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SamplingBias {
    /// No individual sampling.
    None,
    /// Population-weighted sampling.
    PopulationWeighted,
    /// Deliberate representative construction.
    Representative,
    /// Deliberate salience bias.
    SalienceWeighted,
}

/// Complete projection metadata.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Projection {
    /// Projection form.
    pub kind: ProjectionKind,
    /// Source population cohort.
    pub source_cohort: SourceCohort,
    /// Selection lens.
    pub selection_lens: SelectionLens,
    /// Materiality and causal status.
    pub materiality: ProjectionMateriality,
    /// Declared sampling bias.
    pub sampling_bias: SamplingBias,
}

/// A non-causal projection refused a persistent consequence.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ProjectionWriteRefused;

impl Projection {
    /// Construct a persistent aggregate-state projection.
    pub fn aggregate(source_cohort: SourceCohort) -> Self {
        Self {
            kind: ProjectionKind::Aggregate,
            source_cohort,
            selection_lens: SelectionLens::Representative,
            materiality: ProjectionMateriality::AggregateState,
            sampling_bias: SamplingBias::None,
        }
    }

    /// Construct a composite case.
    /// type-audit: bare-ok(flag: in_world)
    pub fn composite(source_cohort: SourceCohort, in_world: bool) -> Self {
        Self {
            kind: ProjectionKind::Composite,
            source_cohort,
            selection_lens: SelectionLens::Representative,
            materiality: if in_world {
                ProjectionMateriality::InWorld
            } else {
                ProjectionMateriality::Analytical
            },
            sampling_bias: SamplingBias::Representative,
        }
    }

    /// Construct a materialized individual.
    pub fn materialized_individual(source_cohort: SourceCohort) -> Self {
        Self {
            kind: ProjectionKind::MaterializedIndividual,
            source_cohort,
            selection_lens: SelectionLens::PopulationWeighted,
            materiality: ProjectionMateriality::MaterializedIndividual,
            sampling_bias: SamplingBias::PopulationWeighted,
        }
    }

    /// Construct a salient-character projection, optionally materialized.
    /// type-audit: bare-ok(flag: materialized)
    pub fn salient_character(source_cohort: SourceCohort, materialized: bool) -> Self {
        Self {
            kind: ProjectionKind::SalientCharacter,
            source_cohort,
            selection_lens: SelectionLens::Salient,
            materiality: if materialized {
                ProjectionMateriality::MaterializedIndividual
            } else {
                ProjectionMateriality::Analytical
            },
            sampling_bias: SamplingBias::SalienceWeighted,
        }
    }

    /// Whether persistent consequences are permitted.
    /// type-audit: bare-ok(flag: return)
    pub fn consequences_write_back(&self) -> bool {
        matches!(
            self.materiality,
            ProjectionMateriality::AggregateState | ProjectionMateriality::MaterializedIndividual
        )
    }

    /// Apply one persistent consequence when this projection is causal.
    pub fn write_persistent_consequence<T>(
        &self,
        write: impl FnOnce() -> T,
    ) -> Result<T, ProjectionWriteRefused> {
        if self.consequences_write_back() {
            Ok(write())
        } else {
            Err(ProjectionWriteRefused)
        }
    }
}
