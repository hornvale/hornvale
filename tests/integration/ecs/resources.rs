use hornvale::ecs::ECS;
use std::ops::Deref;

#[test]
fn create_and_get_resource_immutably() {
  let ecs = initialize_ecs();
  let fps = ecs.get_resource::<TestResource>().unwrap();
  assert_eq!(fps.0, 360)
}

#[test]
fn get_resources_mutably() {
  let mut ecs = initialize_ecs();
  {
    let fps: &mut TestResource = ecs.get_resource_mut::<TestResource>().unwrap();
    fps.0 += 1;
  }
  let fps = ecs.get_resource::<TestResource>().unwrap();
  assert_eq!(fps.0, 361);
}

#[test]
fn delete_resource() {
  let mut ecs = initialize_ecs();
  ecs.delete_resource::<TestResource>();
  let deleted_resource = ecs.get_resource::<TestResource>();
  assert!(deleted_resource.is_none());
}

fn initialize_ecs() -> ECS {
  let mut ecs = ECS::new();
  ecs.add_resource(TestResource(360));
  ecs
}

#[derive(Debug)]
struct TestResource(pub(crate) u32);

impl Deref for TestResource {
  type Target = u32;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
