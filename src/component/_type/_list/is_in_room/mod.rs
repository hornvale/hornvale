#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

/// The `IsInRoom` component.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsInRoom(pub Entity);
