#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

use crate::chunk::Chunk;

/// The `IsAChunkComponent` struct.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsAChunk(pub Chunk);
