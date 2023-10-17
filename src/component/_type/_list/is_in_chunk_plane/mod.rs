#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

/// The `IsInChunkPlane` component.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsInChunkPlane(pub Entity);
