use specs::prelude::*;

use crate::room::Room;

/// The `IsARoomComponent` struct.
#[derive(Clone, Component, Debug)]
pub struct IsARoom(pub Room);
