use super::entities::Entities;
use anyhow::Error as AnyError;
use std::{
  any::{Any, TypeId},
  cell::{Ref, RefCell, RefMut},
  rc::Rc,
};

type ExtractedComponents<'a> = &'a Vec<Option<Rc<RefCell<dyn Any>>>>;

/// An entity in a query.
#[derive(Debug)]
pub struct QueryEntity<'a> {
  /// The entity ID.
  pub id: usize,
  /// The borrowed entities.
  entities: &'a Entities,
}

impl<'a> QueryEntity<'a> {
  /// Constructor.
  pub fn new(id: usize, entities: &'a Entities) -> Self {
    Self { id, entities }
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
    let borrowed_component = components[self.id]
      .as_ref()
      .ok_or(anyhow::anyhow!("The component is not registered."))?
      .borrow();
    Ok(Ref::map(borrowed_component, |any| any.downcast_ref::<T>().unwrap()))
  }

  /// Get a mutable component.
  pub fn get_mut<T: Any>(&self) -> Result<RefMut<T>, AnyError> {
    let components = self.extract_components::<T>()?;
    let borrowed_component = components[self.id]
      .as_ref()
      .ok_or(anyhow::anyhow!("The component is not registered."))?
      .borrow_mut();
    Ok(RefMut::map(borrowed_component, |any| any.downcast_mut::<T>().unwrap()))
  }
}
