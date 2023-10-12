use specs::prelude::*;

use crate::actor::Actor;

/// The `IsAnActorComponent` struct.
#[derive(Clone, Component, Debug)]
pub struct IsAnActor(pub Actor);
