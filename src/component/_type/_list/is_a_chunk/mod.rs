use specs::prelude::*;

use crate::chunk::Chunk;

/// The `IsAChunkComponent` struct.
#[derive(Clone, Component, Debug)]
pub struct IsAChunk(pub Chunk);
