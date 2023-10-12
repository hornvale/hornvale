use specs::prelude::*;

/// The `IsInChunk` component.
#[derive(Clone, Component, Debug)]
pub struct IsInChunk(pub Entity);
