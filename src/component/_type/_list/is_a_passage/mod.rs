#[allow(deprecated)]
use specs::error::NoError;
use specs::prelude::*;
use specs::saveload::*;

use crate::passage::Passage;

/// The `IsAPassageComponent` struct.
#[derive(Clone, Component, ConvertSaveload, Debug)]
pub struct IsAPassage(pub Passage);
