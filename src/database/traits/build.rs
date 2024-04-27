use crate::database::prelude::*;
use crate::database::region::Region;
use anyhow::Error as AnyError;

/// Build a particular resource.
pub trait Build<T: DatabaseType> {
  /// Build a particular kind of structure.
  fn build(&mut self, builder: T::Builder) -> Result<T::Entity, AnyError>;
}

impl Build<Region> for Database {
  fn build(&mut self, builder: RegionBuilder) -> Result<RegionEntity, AnyError> {
    let region = builder.build()?;
    let entity = self.insert(region)?;
    Ok(entity)
  }
}
