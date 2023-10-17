#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

/// The `IsInChunk` component.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsInChunk(pub Entity);
