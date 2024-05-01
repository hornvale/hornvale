use cgmath::*;
use hornvale::ecs::ECS;

#[derive(Copy, Clone)]
struct Transform(Matrix4<f32>);

#[derive(Copy, Clone)]
struct Position(Vector3<f32>);

#[derive(Copy, Clone)]
struct Rotation(Vector3<f32>);

#[derive(Copy, Clone)]
struct Velocity(Vector3<f32>);

#[derive(Clone, Copy, Debug)]
pub struct Benchmark;

impl Default for Benchmark {
  fn default() -> Self {
    Self::new()
  }
}

impl Benchmark {
  pub fn new() -> Self {
    Self
  }

  pub fn run(&mut self) {
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
  }
}
