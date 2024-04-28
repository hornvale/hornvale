use super::{entities::Entities, generational_index::GenerationalIndex};
use anyhow::Error as AnyError;
use std::{any::Any, cell::RefCell, rc::Rc};

/// The actual component type.
pub type Component = Rc<RefCell<dyn Any>>;

/// Add components to an entity.
pub trait AddComponentTuple {
  /// Add the components to the entity.
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError>;
}

impl<T1> AddComponentTuple for (T1,)
where
  T1: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)
  }
}

impl<T1, T2> AddComponentTuple for (T1, T2)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)
  }
}

impl<T1, T2, T3> AddComponentTuple for (T1, T2, T3)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)
  }
}

impl<T1, T2, T3, T4> AddComponentTuple for (T1, T2, T3, T4)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5> AddComponentTuple for (T1, T2, T3, T4, T5)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6> AddComponentTuple for (T1, T2, T3, T4, T5, T6)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7> AddComponentTuple for (T1, T2, T3, T4, T5, T6, T7)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8> AddComponentTuple for (T1, T2, T3, T4, T5, T6, T7, T8)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9> AddComponentTuple for (T1, T2, T3, T4, T5, T6, T7, T8, T9)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> AddComponentTuple for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> AddComponentTuple for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12> AddComponentTuple
  for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
  T12: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)?;
    entities.add(self.11.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13> AddComponentTuple
  for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
  T12: Any + 'static + Clone,
  T13: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)?;
    entities.add(self.11.clone(), index)?;
    entities.add(self.12.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14> AddComponentTuple
  for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
  T12: Any + 'static + Clone,
  T13: Any + 'static + Clone,
  T14: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)?;
    entities.add(self.11.clone(), index)?;
    entities.add(self.12.clone(), index)?;
    entities.add(self.13.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15> AddComponentTuple
  for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
  T12: Any + 'static + Clone,
  T13: Any + 'static + Clone,
  T14: Any + 'static + Clone,
  T15: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)?;
    entities.add(self.11.clone(), index)?;
    entities.add(self.12.clone(), index)?;
    entities.add(self.13.clone(), index)?;
    entities.add(self.14.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16> AddComponentTuple
  for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
  T12: Any + 'static + Clone,
  T13: Any + 'static + Clone,
  T14: Any + 'static + Clone,
  T15: Any + 'static + Clone,
  T16: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)?;
    entities.add(self.11.clone(), index)?;
    entities.add(self.12.clone(), index)?;
    entities.add(self.13.clone(), index)?;
    entities.add(self.14.clone(), index)?;
    entities.add(self.15.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17> AddComponentTuple
  for (
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
  )
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
  T12: Any + 'static + Clone,
  T13: Any + 'static + Clone,
  T14: Any + 'static + Clone,
  T15: Any + 'static + Clone,
  T16: Any + 'static + Clone,
  T17: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)?;
    entities.add(self.11.clone(), index)?;
    entities.add(self.12.clone(), index)?;
    entities.add(self.13.clone(), index)?;
    entities.add(self.14.clone(), index)?;
    entities.add(self.15.clone(), index)?;
    entities.add(self.16.clone(), index)
  }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18> AddComponentTuple
  for (
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
  )
where
  T1: Any + 'static + Clone,
  T2: Any + 'static + Clone,
  T3: Any + 'static + Clone,
  T4: Any + 'static + Clone,
  T5: Any + 'static + Clone,
  T6: Any + 'static + Clone,
  T7: Any + 'static + Clone,
  T8: Any + 'static + Clone,
  T9: Any + 'static + Clone,
  T10: Any + 'static + Clone,
  T11: Any + 'static + Clone,
  T12: Any + 'static + Clone,
  T13: Any + 'static + Clone,
  T14: Any + 'static + Clone,
  T15: Any + 'static + Clone,
  T16: Any + 'static + Clone,
  T17: Any + 'static + Clone,
  T18: Any + 'static + Clone,
{
  fn add_to_entity(&self, entities: &mut Entities, index: GenerationalIndex) -> Result<(), AnyError> {
    entities.add(self.0.clone(), index)?;
    entities.add(self.1.clone(), index)?;
    entities.add(self.2.clone(), index)?;
    entities.add(self.3.clone(), index)?;
    entities.add(self.4.clone(), index)?;
    entities.add(self.5.clone(), index)?;
    entities.add(self.6.clone(), index)?;
    entities.add(self.7.clone(), index)?;
    entities.add(self.8.clone(), index)?;
    entities.add(self.9.clone(), index)?;
    entities.add(self.10.clone(), index)?;
    entities.add(self.11.clone(), index)?;
    entities.add(self.12.clone(), index)?;
    entities.add(self.13.clone(), index)?;
    entities.add(self.14.clone(), index)?;
    entities.add(self.15.clone(), index)?;
    entities.add(self.16.clone(), index)?;
    entities.add(self.17.clone(), index)
  }
}
