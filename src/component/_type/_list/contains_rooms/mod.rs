use specs::prelude::*;
use std::collections::HashMap;

/// The `ContainsRooms` component.
#[derive(Clone, Component, Debug)]
pub struct ContainsRooms(pub HashMap<String, Entity>);
