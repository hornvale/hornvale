use hornvale::ecs::ECS;

#[derive(Clone, Copy)]
struct A(f32);
#[derive(Clone, Copy)]
struct B(f32);

#[derive(Debug)]
pub struct Benchmark(ECS, Vec<u32>);

impl Benchmark {
  pub fn new() -> Self {
    let mut ecs = ECS::default();

    ecs.register_component::<A>();
    ecs.register_component::<B>();
    let entities = ecs.create_entities(10000_usize, (A(0.0),));
    assert_eq!(entities.len(), 10000);

    Self(ecs, entities)
  }

  pub fn run(&mut self) {
    for entity in &self.1 {
      self.0.add_component(B(0.0), *entity as usize).unwrap();
    }

    for entity in &self.1 {
      self.0.remove_component::<B>(*entity as usize).unwrap();
    }
  }
}
