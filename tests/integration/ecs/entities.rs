use anyhow::Error as AnyError;
use hornvale::ecs::generational_index::GenerationalIndex;
use hornvale::ecs::*;
use std::{any::Any, cell::RefCell, rc::Rc};

#[test]
fn create_entity() -> Result<(), AnyError> {
  let mut ecs = ECS::new();
  ecs.register_component::<TestComponent1>();
  ecs.register_component::<TestComponent2>();

  ecs
    .create_entity()
    .with(TestComponent1(42.0, 24.0))?
    .with(TestComponent2(10.0))?;
  Ok(())
}

#[test]
#[allow(clippy::float_cmp)]
fn query_for_entities() -> Result<(), AnyError> {
  let mut ecs = ECS::new();
  ecs.register_component::<TestComponent1>();
  ecs.register_component::<TestComponent2>();

  ecs
    .create_entity()
    .with(TestComponent1(42.0, 24.0))?
    .with(TestComponent2(10.0))?;

  ecs.create_entity().with(TestComponent2(11.0))?;

  ecs.create_entity().with(TestComponent1(43.0, 25.0))?;

  ecs
    .create_entity()
    .with(TestComponent1(44.0, 26.0))?
    .with(TestComponent2(12.0))?;

  let query = ecs.query().with::<TestComponent1>()?.with::<TestComponent2>()?.run();

  let locations: &Vec<Rc<RefCell<dyn Any>>> = &query.1[0];
  let sizes: &Vec<Rc<RefCell<dyn Any>>> = &query.1[1];

  assert_eq!(locations.len(), sizes.len());
  assert_eq!(locations.len(), 2);

  let borrowed_first_location = locations[0].borrow();
  let first_location = borrowed_first_location.downcast_ref::<TestComponent1>().unwrap();
  assert_eq!(first_location.0, 42.0);
  let borrowed_first_size = sizes[0].borrow();
  let first_size = borrowed_first_size.downcast_ref::<TestComponent2>().unwrap();
  assert_eq!(first_size.0, 10.0);

  let borrowed_second_location = locations[1].borrow();
  let second_location = borrowed_second_location.downcast_ref::<TestComponent1>().unwrap();
  assert_eq!(second_location.0, 44.0);
  let mut borrowed_second_size = sizes[1].borrow_mut();
  let second_size = borrowed_second_size.downcast_mut::<TestComponent2>().unwrap();
  second_size.0 += 1.0;
  assert_eq!(second_size.0, 13.0);

  Ok(())
}

#[test]
fn deleted_component_from_entity() -> Result<(), AnyError> {
  let mut ecs = ECS::new();

  ecs.register_component::<TestComponent1>();
  ecs.register_component::<TestComponent2>();

  ecs
    .create_entity()
    .with(TestComponent1(10.0, 11.0))?
    .with(TestComponent2(10.0))?;

  ecs
    .create_entity()
    .with(TestComponent1(20.0, 21.0))?
    .with(TestComponent2(20.0))?;

  ecs.remove_component::<TestComponent1>(GenerationalIndex {
    index: 0,
    generation: 0,
  })?;

  let query = ecs.query().with::<TestComponent1>()?.with::<TestComponent2>()?.run();

  assert_eq!(query.0.len(), 1);
  assert_eq!(query.0[0], 1);

  Ok(())
}

#[test]
fn add_component_to_entity() -> Result<(), AnyError> {
  let mut ecs = ECS::new();
  ecs.register_component::<TestComponent1>();
  ecs.register_component::<TestComponent2>();
  ecs.create_entity().with(TestComponent1(10.0, 15.0))?;

  ecs.add_component(
    TestComponent2(20.0),
    GenerationalIndex {
      index: 0,
      generation: 0,
    },
  )?;

  let query = ecs.query().with::<TestComponent1>()?.with::<TestComponent2>()?.run();
  assert_eq!(query.0.len(), 1);
  assert_eq!(query.0[0], 0);
  assert_eq!(query.1[0].len(), 1);
  assert_eq!(query.1[1].len(), 1);

  Ok(())
}

#[test]
#[allow(clippy::float_cmp)]
fn deleting_an_entity() -> Result<(), AnyError> {
  let mut ecs = ECS::new();
  ecs.register_component::<TestComponent1>();
  ecs.register_component::<TestComponent2>();
  ecs.create_entity().with(TestComponent1(10.0, 15.0))?;
  ecs.create_entity().with(TestComponent1(20.0, 25.0))?;

  ecs.delete_entity(GenerationalIndex {
    index: 0,
    generation: 0,
  })?;

  let query = ecs.query().with::<TestComponent1>()?.run();

  assert_eq!(query.0.len(), 1);

  let borrowed_location = query.1[0][0].borrow();
  let location = borrowed_location.downcast_ref::<TestComponent1>().unwrap();

  assert_eq!(location.0, 20.0);
  assert_eq!(location.1, 25.0);

  ecs.create_entity().with(TestComponent1(30.0, 35.0))?;
  let query = ecs.query().with::<TestComponent1>()?.run();
  let borrowed_location = query.1[0][0].borrow();
  let location = borrowed_location.downcast_ref::<TestComponent1>().unwrap();
  assert_eq!(location.0, 20.0);
  assert_eq!(location.1, 25.0);

  Ok(())
}

struct TestComponent1(pub(crate) f32, pub(crate) f32);
struct TestComponent2(pub(crate) f32);
