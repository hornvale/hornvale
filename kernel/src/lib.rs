//! Hornvale kernel: the substrate every domain depends on — and the only
//! thing any domain may depend on (Constitution §2.6).

#![warn(missing_docs)]

pub mod field;
pub mod geosphere;
pub mod ledger;
pub mod noise;
pub mod phenomena;
pub mod png;
pub mod refine;
pub mod registry;
pub mod seed;
pub mod world;

pub use field::{ConstantField, Field, NoiseField, Position, WorldTime};
pub use geosphere::{CellId, CellMap, GeoCoord, Geosphere};
pub use ledger::{EntityId, Fact, Ledger, LedgerError, Value};
pub use noise::{fbm_2d, value_noise_2d};
pub use phenomena::{ObserverContext, PerceptionLens, PhenomenaSource, Phenomenon, Venue, observe};
pub use refine::choose_consistent;
pub use registry::{ConceptRegistry, PredicateDef, RegistryError};
pub use seed::{Seed, Stream};
pub use world::{NAME, World};
