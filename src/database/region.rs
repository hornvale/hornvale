use crate::database::prelude::*;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// The Region Entity.
pub mod entity;
/// The Region Identifier.
pub mod identifier;
/// The name of the Region.
pub mod name;
/// The Region Point component.
pub mod point;
/// A Region spawner.
pub mod spawner;

/// A region.
#[derive(Builder, Clone, Debug, Serialize, Deserialize)]
#[builder(derive(Debug, Serialize, Deserialize))]
pub struct Region {
  /// The region identifier.
  #[builder(setter(into))]
  pub identifier: RegionIdentifier,
  /// The region name.
  #[builder(setter(into))]
  pub name: RegionName,
  /// The region point.
  #[builder(setter(into))]
  pub point: RegionPoint,
}

impl DatabaseType for Region {
  type Builder = RegionBuilder;
  type Entity = RegionEntity;
  type Identifier = RegionIdentifier;
  type Name = RegionName;
}
