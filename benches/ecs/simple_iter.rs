use cgmath::*;
use hornvale::ecs::query_entity::QueryEntity;
use hornvale::ecs::ECS;
use std::cell::{Ref, RefMut};

#[derive(Copy, Clone)]
struct Transform(Matrix4<f32>);

#[derive(Copy, Clone)]
struct Position(Vector3<f32>);

#[derive(Copy, Clone)]
struct Rotation(Vector3<f32>);

#[derive(Copy, Clone)]
struct Velocity(Vector3<f32>);

#[derive(Debug)]
pub struct Benchmark(ECS);

impl Benchmark {
  pub fn new() -> Self {
    let mut ecs = ECS::new();
    ecs.register_component::<Transform>();
    ecs.register_component::<Position>();
    ecs.register_component::<Rotation>();
    ecs.register_component::<Velocity>();
    ecs.create_entities(
      10_000,
      (
        Transform(Matrix4::from_scale(1.0)),
        Position(Vector3::unit_x()),
        Rotation(Vector3::unit_x()),
        Velocity(Vector3::unit_x()),
      ),
    );
    Self(ecs)
  }

  pub fn run(&mut self) {
    let mut query = self.0.query();
    let entities: Vec<QueryEntity> = query
      .with::<Velocity>()
      .unwrap()
      .with::<Position>()
      .unwrap()
      .run_entity();
    for entity in entities {
      let mut position: RefMut<Position> = entity.get_mut::<Position>().unwrap();
      let velocity: Ref<Velocity> = entity.get::<Velocity>().unwrap();
      position.0 += velocity.0;
    }
  }
}
