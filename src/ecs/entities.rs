use super::{
  component::AddComponentTuple, component_map::ComponentMap, generational_index::GenerationalIndex,
  generational_index_allocator::GenerationalIndexAllocator, query::Query,
};
use anyhow::Error as AnyError;
use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Entities and components.
#[derive(Debug, Default)]
pub struct Entities {
  /// Generational index allocator.
  pub allocator: GenerationalIndexAllocator,
  /// The components.
  pub component_map: ComponentMap,
  /// Bit masks for components.
  pub bit_masks: HashMap<TypeId, u32>,
  /// The entities.
  pub map: Vec<u32>,
  /// Inserting into index.
  pub inserting_into_index: GenerationalIndex,
}

impl Entities {
  /// Register a component.
  pub fn register<T: Any + 'static>(&mut self) {
    assert!(!self.component_map.contains_key(&TypeId::of::<T>()));
    assert!(self.bit_masks.len() < 32, "The maximum number of components is 32.");
    let type_id = TypeId::of::<T>();
    self.component_map.entry(type_id).or_default();
    self.bit_masks.insert(type_id, 1 << self.bit_masks.len());
  }

  /// Create an entity.
  pub fn create(&mut self) -> &mut Self {
    self.inserting_into_index = self.allocator.allocate();
    self
  }

  /// Create a batch of entities.
  pub fn create_batch<T: AddComponentTuple + Clone>(&mut self, count: usize, components: T) -> Vec<u32> {
    for _ in 0..count {
      self.create().with_components(components.clone()).unwrap();
    }
    self.map.clone()
  }

  /// Add a component to an entity.
  pub fn with<T: Any + 'static>(&mut self, component: T) -> Result<&mut Self, AnyError> {
    let type_id = TypeId::of::<T>();
    match self.component_map.get_mut(&type_id) {
      Some(components) => {
        let index = self.inserting_into_index;
        let component = Rc::new(RefCell::new(component));
        components.set(index, component);
        let bitmask = self.bit_masks.get(&type_id).unwrap();
        if self.map.len() <= index.index {
          self.map.resize(index.index + 1, 0);
        }
        self.map[index.index] |= *bitmask;
        Ok(self)
      },
      None => anyhow::bail!("The component is not registered."),
    }
  }

  /// With components.
  pub fn with_components<T: AddComponentTuple>(&mut self, components: T) -> Result<(), AnyError> {
    components.add_to_entity(self, self.inserting_into_index)
  }

  /// Get a bit mask for a component type.
  pub fn get_bit_mask<T: Any + 'static>(&self) -> Option<u32> {
    self.bit_masks.get(&TypeId::of::<T>()).copied()
  }

  /// Query for entities and components.
  pub fn query(&self) -> Query {
    Query::new(self)
  }

  /// Does the entity have a component?
  pub fn has(&mut self, index: GenerationalIndex, mask: u32) -> bool {
    if self.map.len() <= index.index {
      self.map.resize(index.index + 1, 0);
    }
    self.map[index.index] & mask == mask
  }

  /// Remove a component.
  pub fn remove<T: Any>(&mut self, index: GenerationalIndex) -> Result<(), AnyError> {
    let type_id = TypeId::of::<T>();
    let mask = self
      .bit_masks
      .get(&type_id)
      .ok_or_else(|| anyhow::anyhow!("The component is not registered."))?;
    if self.map.len() <= index.index {
      self.map.resize(index.index + 1, 0);
    }
    self.map[index.index] &= !*mask;
    Ok(())
  }

  /// Add a component.
  pub fn add(&mut self, data: impl Any, index: GenerationalIndex) -> Result<(), AnyError> {
    let type_id = data.type_id();
    let mask = self
      .bit_masks
      .get(&type_id)
      .ok_or_else(|| anyhow::anyhow!("The component is not registered."))?;
    if self.map.len() <= index.index {
      self.map.resize(index.index + 1, 0);
    }
    self.map[index.index] |= *mask;
    let components = self.component_map.get_mut(&type_id).unwrap();
    components.set(index, Rc::new(RefCell::new(data)));
    Ok(())
  }

  /// Delete an entity.
  pub fn delete(&mut self, index: GenerationalIndex) -> Result<(), AnyError> {
    match self.map.get_mut(index.index) {
      Some(map) => {
        *map = 0;
        Ok(())
      },
      None => anyhow::bail!("The entity does not exist."),
    }
  }
}

#[cfg(test)]
mod test {
  use super::super::generational_index::GenerationalIndex;
  use super::*;
  use std::any::TypeId;
  use std::borrow::Borrow;

  #[test]
  fn register_an_entity() {
    let mut entities = Entities::default();
    entities.register::<Health>();
    let type_id = TypeId::of::<Health>();
    let health_components = entities.component_map.get(&type_id).unwrap();
    assert_eq!(health_components.len(), 0);
  }

  #[test]
  fn bitmask_updated_when_registering_entities() {
    let mut entities = Entities::default();
    entities.register::<Health>();
    let type_id = TypeId::of::<Health>();
    let mask = entities.bit_masks.get(&type_id).unwrap();
    assert_eq!(*mask, 1);

    entities.register::<Speed>();
    let type_id = TypeId::of::<Speed>();
    let mask = entities.bit_masks.get(&type_id).unwrap();
    assert_eq!(*mask, 2);
  }

  #[test]
  fn create() {
    let mut entities = Entities::default();
    entities.register::<Health>();
    entities.register::<Speed>();
    entities.create();
    let health = entities.component_map.get(&TypeId::of::<Health>()).unwrap();
    let speed = entities.component_map.get(&TypeId::of::<Speed>()).unwrap();
    assert_eq!(health.len(), speed.len());
    assert_eq!(health.len(), 0);
    let index = GenerationalIndex {
      index: 0,
      generation: 0,
    };
    assert!(health.get(index).is_none() && speed.get(index).is_none());
  }

  #[test]
  fn with() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<Health>();
    entities.register::<Speed>();
    entities.create().with(Health(100))?.with(Speed(15))?;
    let index = GenerationalIndex {
      index: 0,
      generation: 0,
    };
    let first_health = &entities.component_map.get(&TypeId::of::<Health>()).unwrap().get(index);
    let _wrapped_health = first_health.as_ref().unwrap();
    // let borrowed_health = wrapped_health.borrow();
    // let health = borrowed_health.downcast_ref::<Health>().unwrap();
    // assert_eq!(health.0, 100);
    Ok(())
  }

  #[test]
  fn map_is_updated_when_creating_entities() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<Health>();
    entities.register::<Speed>();
    entities.create().with(Health(100))?.with(Speed(15))?;
    let entity_map = entities.map[0];
    assert_eq!(entity_map, 3);

    entities.create().with(Speed(15))?;
    let entity_map = entities.map[1];
    assert_eq!(entity_map, 2);
    Ok(())
  }

  #[test]
  fn delete_component_by_entity_id() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<Health>();
    entities.register::<Speed>();
    entities.create().with(Health(100))?.with(Speed(50))?;

    entities.remove::<Health>(GenerationalIndex {
      index: 0,
      generation: 0,
    })?;

    assert_eq!(entities.map[0], 2);
    Ok(())
  }

  #[test]
  fn add_component_to_entity_by_id() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<Health>();
    entities.register::<Speed>();
    entities.create().with(Health(100))?;
    entities.add(
      Speed(50),
      GenerationalIndex {
        index: 0,
        generation: 0,
      },
    )?;
    assert_eq!(entities.map[0], 3);
    let index = GenerationalIndex {
      index: 0,
      generation: 0,
    };
    let speed_type_id = TypeId::of::<Speed>();
    let wrapped_speeds = entities.component_map.get(&speed_type_id).unwrap();
    let wrapped_speed = wrapped_speeds.get(index);
    let _speed = wrapped_speed.as_ref().unwrap();
    let _borrowed_speed = wrapped_speed.borrow();
    //let speed = borrowed_speed.downcast_ref::<Speed>().unwrap();
    //assert_eq!(speed.0, 50);
    Ok(())
  }

  #[test]
  fn delete_entity_by_id() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<Health>();
    entities.create().with(Health(100))?;
    entities.delete(GenerationalIndex {
      index: 0,
      generation: 0,
    })?;
    assert_eq!(entities.map[0], 0);
    Ok(())
  }

  #[test]
  fn created_entities_are_inserted_into_deleted_entities_columns() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<Health>();
    entities.create().with(Health(100))?;
    entities.create().with(Health(50))?;
    entities.delete(GenerationalIndex {
      index: 0,
      generation: 0,
    })?;
    entities.create().with(Health(25))?;
    assert_eq!(entities.map[0], 0);
    let _type_id = TypeId::of::<Health>();
    let _index = GenerationalIndex {
      index: 0,
      generation: 0,
    };
    //let borrowed_health = &entities
    //  .component_map
    //  .get(&type_id)
    //  .unwrap()
    //  .get(index)
    //  .as_ref()
    //  .unwrap()
    //  .borrow();
    //let health = borrowed_health.downcast_ref::<Health>().unwrap();
    //assert_eq!(health.0, 25);
    Ok(())
  }

  #[test]
  fn should_not_add_component_back_after_deleting_twice() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<u32>();
    entities.register::<f32>();
    entities.create().with(100_u32)?.with(50.0_f32)?;
    entities.remove::<u32>(GenerationalIndex {
      index: 0,
      generation: 0,
    })?;
    entities.remove::<u32>(GenerationalIndex {
      index: 0,
      generation: 0,
    })?;
    assert_eq!(entities.map[0], 2);
    Ok(())
  }

  #[test]
  fn inserting_into_index_should_change_when_adding_components() -> Result<(), AnyError> {
    let mut entities = Entities::default();
    entities.register::<f32>();
    entities.register::<u32>();

    // Inserting an entity with 2 components to make sure that inserting_into_index is correct
    let creating_entity = entities.create();
    assert_eq!(
      creating_entity.inserting_into_index,
      GenerationalIndex {
        index: 0,
        generation: 0,
      }
    );
    creating_entity.with(100.0_f32)?.with(10_u32)?;
    assert_eq!(
      entities.inserting_into_index,
      GenerationalIndex {
        index: 0,
        generation: 0,
      }
    );

    // Inserting another entity with 2 components to make sure that the inserting_into_index is now 1
    let creating_entity = entities.create();
    assert_eq!(
      creating_entity.inserting_into_index,
      GenerationalIndex {
        index: 1,
        generation: 0,
      }
    );
    creating_entity.with(110.0_f32)?.with(20_u32)?;
    assert_eq!(
      entities.inserting_into_index,
      GenerationalIndex {
        index: 1,
        generation: 0,
      }
    );

    // delete the first entity, and re-create to make sure that inserting_into_index is back
    // to 0 again
    entities.delete(GenerationalIndex {
      index: 0,
      generation: 0,
    })?;
    let creating_entity = entities.create();
    assert_eq!(
      creating_entity.inserting_into_index,
      GenerationalIndex {
        index: 2,
        generation: 0,
      }
    );
    creating_entity.with(100.0_f32)?.with(10_u32)?;
    assert_eq!(
      entities.inserting_into_index,
      GenerationalIndex {
        index: 2,
        generation: 0,
      }
    );
    Ok(())
  }

  #[allow(dead_code)]
  struct Health(u32);
  #[allow(dead_code)]
  struct Speed(u32);
}
