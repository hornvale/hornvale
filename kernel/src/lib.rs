//! Hornvale kernel: the substrate every domain depends on — and the only
//! thing any domain may depend on (Constitution §2.6).

pub mod field;
pub mod noise;
pub mod registry;
pub mod seed;

pub use field::{ConstantField, Field, NoiseField, Position, WorldTime};
pub use noise::{fbm_2d, value_noise_2d};
pub use registry::{ConceptRegistry, PredicateDef, RegistryError};
pub use seed::{Seed, Stream};
