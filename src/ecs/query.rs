use crate::ecs::prelude::*;
use crate::ecs::query_entity::QueryEntity;
use anyhow::Error as AnyError;
use std::any::{Any, TypeId};

/// Indices of entities.
pub type QueryIndices = Vec<usize>;

/// Components of entities.
pub type QueryComponents = Vec<Vec<Component>>;

/// A query for entities and components.
#[derive(Debug)]
pub struct Query<'entities> {
  /// The bit mask.
  pub bit_mask: u32,
  /// The entities, borrowed from the ECS.
  pub entities: &'entities Entities,
  /// The component type IDs in the query.
  pub type_ids: Vec<TypeId>,
}

impl<'entities> Query<'entities> {
  /// Create a new query.
  pub fn new(entities: &'entities Entities) -> Self {
    let bit_mask = 0;
    let type_ids = Vec::new();
    Query {
      bit_mask,
      entities,
      type_ids,
    }
  }

  /// With a component.
  pub fn with<T: Any + 'static>(&mut self) -> Result<&mut Self, AnyError> {
    match self.entities.get_bit_mask::<T>() {
      Some(bit_mask) => {
        self.bit_mask |= bit_mask;
        self.type_ids.push(TypeId::of::<T>());
        Ok(self)
      },
      None => anyhow::bail!("The component is not registered."),
    }
  }

  /// Run the query.
  pub fn run(&self) -> (QueryIndices, QueryComponents) {
    let indexes: Vec<usize> = self
      .entities
      .map
      .iter()
      .enumerate()
      .filter_map(|(index, entity_map)| {
        if entity_map & self.bit_mask == self.bit_mask {
          Some(index)
        } else {
          None
        }
      })
      .collect();
    let mut result = vec![];
    for type_id in &self.type_ids {
      let entity_components = self.entities.component_map.get(type_id).unwrap();
      let mut components_to_keep = vec![];
      for index in &indexes {
        components_to_keep.push(entity_components[*index].as_ref().unwrap().clone());
      }
      result.push(components_to_keep);
    }
    (indexes, result)
  }

  /// Run the query for entities.
  pub fn run_entity(&self) -> Vec<QueryEntity> {
    self
      .entities
      .map
      .iter()
      .enumerate()
      .filter_map(|(index, entity_map)| {
        if entity_map & self.bit_mask == self.bit_mask {
          Some(QueryEntity::new(index, self.entities))
        } else {
          None
        }
      })
      .collect()
  }
}

#[cfg(test)]
mod test {

  use super::*;
  use crate::test_utilities::prelude::*;
  use core::f32;
  use std::{
    cell::{Ref, RefMut},
    u32,
  };

  #[test]
  fn query_mask_updating_with() -> Result<(), AnyError> {
    init();
    let mut entities = Entities::default();
    entities.register::<u32>();
    entities.register::<f32>();
    let mut query = Query::new(&entities);
    query.with::<u32>()?.with::<f32>()?;

    assert_eq!(query.bit_mask, 3);
    assert_eq!(TypeId::of::<u32>(), query.type_ids[0]);
    assert_eq!(TypeId::of::<f32>(), query.type_ids[1]);

    Ok(())
  }

  #[test]
  #[allow(clippy::float_cmp)]
  fn run_query() -> Result<(), AnyError> {
    init();
    let mut entities = Entities::default();
    entities.register::<u32>();
    entities.register::<f32>();
    entities.create().with(10_u32)?.with(20.0_f32)?;
    entities.create().with(5_u32)?;
    entities.create().with(50.0_f32)?;
    entities.create().with(15_u32)?.with(25.0_f32)?;
    let mut query = Query::new(&entities);
    query.with::<u32>()?.with::<f32>()?;

    let query_result = query.run();
    let u32s = &query_result.1[0];
    let f32s = &query_result.1[1];
    let indexes = &query_result.0;

    assert!(u32s.len() == f32s.len() && u32s.len() == indexes.len());
    assert_eq!(u32s.len(), 2);

    let borrowed_first_u32 = u32s[0].borrow();
    let first_u32 = borrowed_first_u32.downcast_ref::<u32>().unwrap();
    assert_eq!(*first_u32, 10);

    let borrowed_first_f32 = f32s[0].borrow();
    let first_f32 = borrowed_first_f32.downcast_ref::<f32>().unwrap();
    assert_eq!(*first_f32, 20.0);

    let borrowed_second_u32 = u32s[1].borrow();
    let second_u32 = borrowed_second_u32.downcast_ref::<u32>().unwrap();
    assert_eq!(*second_u32, 15);

    let borrowed_second_f32 = f32s[1].borrow();
    let second_f32 = borrowed_second_f32.downcast_ref::<f32>().unwrap();
    assert_eq!(*second_f32, 25.0);

    assert_eq!(indexes[0], 0);
    assert_eq!(indexes[1], 3);

    Ok(())
  }

  #[test]
  fn run_query_with_no_components() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<u32>();
    entities.create().with(10_u32)?;
    entities.create();
    let mut query = Query::new(&entities);
    query.with::<u32>()?;
    let query_result = query.run();
    let u32s = &query_result.1[0];
    assert_eq!(u32s.len(), 1);
    Ok(())
  }

  #[test]
  fn query_after_deleting_entity() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<u32>();
    entities.create().with(10_u32)?;
    entities.create().with(20_u32)?;
    entities.delete(1)?;
    let (query_indexes, query_results) = Query::new(&entities).with::<u32>()?.run();
    assert_eq!(query_indexes.len(), query_results.len());
    assert_eq!(query_results[0].len(), 1);
    assert_eq!(query_indexes[0], 0);
    let borrowed_first_u32 = query_results[0][0].borrow();
    let first_u32 = borrowed_first_u32.downcast_ref::<u32>().unwrap();
    assert_eq!(*first_u32, 10);
    Ok(())
  }

  #[test]
  fn query_for_entity_ref() -> Result<(), AnyError> {
    let mut entities = Entities::default();

    entities.register::<u32>();
    entities.register::<f32>();
    entities.create().with(100_u32)?;
    entities.create().with(10.0_f32)?;

    let mut query = Query::new(&entities);
    let entities: Vec<QueryEntity> = query.with::<u32>()?.run_entity();

    assert_eq!(entities.len(), 1);

    for entity in entities {
      assert_eq!(entity.id, 0);
      let health: Ref<u32> = entity.get::<u32>()?;
      assert_eq!(*health, 100);
    }
    Ok(())
  }

  #[test]
  fn query_for_entity_mut() -> Result<(), AnyError> {
    let mut entities = Entities::default();

    entities.register::<u32>();
    entities.register::<f32>();
    entities.create().with(100_u32)?;
    entities.create().with(10.0_f32)?;

    let mut query = Query::new(&entities);
    let entities: Vec<QueryEntity> = query.with::<u32>()?.run_entity();

    assert_eq!(entities.len(), 1);

    for entity in entities {
      assert_eq!(entity.id, 0);
      let mut health: RefMut<u32> = entity.get_mut::<u32>()?;
      assert_eq!(*health, 100);
      *health += 1;
    }

    let entities: Vec<QueryEntity> = query.with::<u32>()?.run_entity();
    for entity in entities {
      let health: Ref<u32> = entity.get::<u32>()?;
      assert_eq!(*health, 101);
    }
    Ok(())
  }
}
