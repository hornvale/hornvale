#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

use crate::chunk::ChunkPlane;

/// The `IsAChunkPlane` component.
///
/// A `ChunkPlane` is the largest spatial unit in the game. It is a 2D plane
/// composed of `Chunk`s. Other areas, e.g. the Underdark, will be managed
/// in different planes.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsAChunkPlane(pub ChunkPlane);
