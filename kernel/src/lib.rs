//! Hornvale kernel: the substrate every domain depends on — and the only
//! thing any domain may depend on (Constitution §2.6). Domains communicate
//! solely through its trace protocol — facts, phenomena, fields
//! (decision 0003).

#![warn(missing_docs)]

pub mod domain;
pub mod ecology;
pub mod field;
pub mod geosphere;
pub mod golden;
pub mod ledger;
pub mod math;
pub mod noise;
pub mod phenomena;
pub mod png;
pub mod quantize;
pub mod refine;
pub mod registry;
pub mod room;
pub mod seed;
pub mod streams;
pub mod units;
pub mod world;

pub use domain::Domain;
pub use ecology::{
    ANIMAL_PREY, ConditionResponse, DETRITUS, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE, ResourceAxis,
    ResourceKind, ResourceVector, sovereignty_floor, v1_basis,
};
pub use field::{ConstantField, Field, NoiseField, Position, WorldTime};
pub use geosphere::{CellId, CellMap, GeoCoord, Geosphere, NearestCellIndex};
pub use ledger::{EntityId, Fact, Ledger, LedgerError, Value};
pub use noise::{fbm_2d, value_noise_2d};
pub use phenomena::{ObserverContext, PerceptionLens, PhenomenaSource, Phenomenon, Venue, observe};
pub use quantize::{QUANTIZE_SIG_DIGITS, quantize};
pub use refine::choose_consistent;
pub use registry::{ConceptDef, ConceptKind, ConceptRegistry, PredicateDef, RegistryError};
pub use room::{MAX_DEPTH, RoomAddr, RoomAddrError, RoomId, RoomIdError};
pub use seed::{Seed, Stream};
pub use streams::stream_labels;
pub use units::{Mass, ReferenceElevation, TempAnomaly, Temperature, UnitError, Years};
pub use world::{NAME, World};
