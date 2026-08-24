//! Hornvale kernel: the substrate every domain depends on — and the only
//! thing any domain may depend on (Constitution §2.6). Domains communicate
//! solely through its trace protocol — facts, phenomena, fields
//! (decision 0003).

#![warn(missing_docs)]

pub mod astar;
pub mod claim;
pub mod color;
pub mod component;
pub mod derived;
pub mod domain;
pub mod ecology;
mod fact_index;
pub mod field;
pub mod geosphere;
pub mod golden;
pub mod ledger;
pub mod manifest;
pub mod math;
pub mod noise;
pub mod phenomena;
pub mod png;
mod polyline;
pub mod precision;
pub mod provenance;
pub mod quantize;
pub mod refine;
pub mod registry;
pub mod room;
pub mod schedule;
pub mod seed;
pub mod streams;
pub mod units;
pub mod world;

pub use astar::{AStarSolver, FieldSolver, SearchSpace, Solver, astar};
pub use claim::Claim;
pub use color::{
    BAND_CENTERS_NM, BANDS, ChannelRole, Illuminant, Mixture, Observer, Projection, Reflectance,
    Signal, Spectrum, standard_observer,
};
pub use component::{Component, ComponentStore};
pub use domain::{Domain, WorldContext};
pub use ecology::{
    ANIMAL_PREY, AxisValence, ConditionResponse, DETRITUS, DISTURBANCE, ENERGY, EnvironmentAxis,
    EnvironmentVector, LIGHT, MARINE_FORAGE, MINERAL, PHOTOSYNTHATE, PHYSIOGNOMY, PLANT_FORAGE,
    ResourceAxis, ResourceKind, ResourceVector, SUBSTRATE, WATER, environment_v1_basis,
    sovereignty_floor, v1_basis,
};
pub use field::{ConstantField, Field, NoiseField, Position, WorldTime};
pub use geosphere::{CellId, CellMap, GeoCoord, Geosphere, NearestCellIndex};
pub use ledger::{
    EntityId, Fact, KindId, Ledger, LedgerError, Lineage, Value, derive_entity_id, test_lineage,
};
pub use manifest::{CognitiveHandle, Correspondent, Lexicalization, Manifest, PerceptKind, Void};
pub use noise::{Fbm, fbm_2d, value_noise_2d};
pub use phenomena::{
    ObserverContext, PerceptionLens, PhenomenaSource, Phenomenon, Referent, VISIBILITY_FLOOR,
    Venue, Visibility, observe,
};
pub use polyline::{SphericalPolyline, band};
pub use precision::Precision;
pub use provenance::Provenance;
pub use quantize::{QUANTIZE_SIG_DIGITS, quantize};
pub use refine::choose_consistent;
pub use registry::{ConceptDef, ConceptKind, ConceptRegistry, PredicateDef, RegistryError};
pub use room::{
    FaceLattice, MAX_DEPTH, RoomAddr, RoomAddrError, RoomId, RoomIdError, RoomMeshMemo,
};
pub use schedule::{CapabilitySchema, ScheduleError, System, TickSystem, tick};
pub use seed::{Seed, Stream};
pub use streams::stream_labels;
pub use units::{
    Mass, Precipitation, ReferenceElevation, SeaLevelHeight, TempAnomaly, Temperature, UnitError,
    Years,
};
pub use world::{INSTANCE_OF, KERNEL_CORE_PREDICATES, NAME, NAME_GLOSS, World};
