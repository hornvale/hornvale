#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

use crate::actor::Actor;

/// The `IsAnActorComponent` struct.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsAnActor(pub Actor);
