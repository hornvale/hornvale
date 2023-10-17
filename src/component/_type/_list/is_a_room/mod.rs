#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

use crate::room::Room;

/// The `IsARoomComponent` struct.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsARoom(pub Room);
