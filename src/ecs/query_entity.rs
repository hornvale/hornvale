use super::{
  component::Component, entities::Entities, generational_index::GenerationalIndex,
  generational_index_array::GenerationalIndexArray,
};
use anyhow::Error as AnyError;
use std::{
  any::{Any, TypeId},
  cell::{Ref, RefMut},
};

type ExtractedComponents<'a> = &'a GenerationalIndexArray<Component>;

/// An entity in a query.
#[derive(Debug)]
pub struct QueryEntity<'a> {
  /// The entity ID.
  pub index: GenerationalIndex,
  /// The borrowed entities.
  entities: &'a Entities,
}

impl<'a> QueryEntity<'a> {
  /// Constructor.
  pub fn new(index: GenerationalIndex, entities: &'a Entities) -> Self {
    Self { index, entities }
  }

  fn extract_components<T: Any>(&self) -> Result<ExtractedComponents, AnyError> {
    let type_id = TypeId::of::<T>();
    self
      .entities
      .component_map
      .get(&type_id)
      .ok_or_else(|| anyhow::anyhow!("The component is not registered."))
  }

  /// Get a component.
  pub fn get<T: Any>(&self) -> Result<Ref<T>, AnyError> {
    let components = self.extract_components::<T>()?;
    let borrowed_component = components
      .get(self.index)
      .as_ref()
      .ok_or(anyhow::anyhow!("The component is not registered."))?
      .borrow();
    Ok(Ref::map(borrowed_component, |any| any.downcast_ref::<T>().unwrap()))
  }

  /// Get a mutable component.
  pub fn get_mut<T: Any>(&self) -> Result<RefMut<T>, AnyError> {
    let components = self.extract_components::<T>()?;
    let borrowed_component = components
      .get(self.index)
      .ok_or(anyhow::anyhow!("The component is not registered."))?
      .borrow_mut();
    Ok(RefMut::map(borrowed_component, |any| any.downcast_mut::<T>().unwrap()))
  }
}
