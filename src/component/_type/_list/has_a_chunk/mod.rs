use specs::prelude::*;

/// The `HasAChunk` component.
#[derive(Clone, Component, Debug)]
pub struct HasAChunk(pub Entity);
