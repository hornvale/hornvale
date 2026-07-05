//! Hornvale kernel: the substrate every domain depends on — and the only
//! thing any domain may depend on (Constitution §2.6).

pub mod noise;
pub mod seed;

pub use noise::{fbm_2d, value_noise_2d};
pub use seed::{Seed, Stream};
