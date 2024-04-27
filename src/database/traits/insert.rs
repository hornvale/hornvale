use crate::database::prelude::*;
use crate::database::region::Region;
use anyhow::Error as AnyError;

/// Insert a particular object.
pub trait Insert<T: DatabaseType> {
  /// Insert a particular kind of structure.
  fn insert(&mut self, object: T) -> Result<T::Entity, AnyError>;
}

impl Insert<Region> for Database {
  fn insert(&mut self, object: Region) -> Result<RegionEntity, AnyError> {
    let id = RegionEntity(self.next_region_id);
    self.next_region_id += 1;
    self.region_identifiers.insert(id, object.identifier.clone());
    self.region_entities.insert(object.identifier.clone(), id);
    self.regions.insert(id, object);
    Ok(RegionEntity::default())
  }
}
