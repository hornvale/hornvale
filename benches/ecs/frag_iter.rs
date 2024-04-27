use hornvale::ecs::query_entity::QueryEntity;
use hornvale::ecs::ECS;
use std::cell::RefMut;

macro_rules! create_entities {
  ($ecs:ident; $( $variants:ident ),*) => {
    $(
        #[derive(Clone, Copy)]
        struct $variants(f32);
        $ecs.register_component::<$variants>();
        $ecs.create_entities(20, ($variants(0.0),));
    )*
  };
}

struct Data(f32);

#[derive(Debug)]
pub struct Benchmark(ECS);

impl Benchmark {
  pub fn new() -> Self {
    let mut ecs = ECS::default();
    ecs.register_component::<Data>();
    create_entities!(ecs; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);
    Self(ecs)
  }

  pub fn run(&mut self) {
    let mut query = self.0.query();
    let entities: Vec<QueryEntity> = query.with::<Data>().unwrap().run_entity();
    for entity in entities {
      let mut data: RefMut<Data> = entity.get_mut::<Data>().unwrap();
      data.0 *= 2.0;
    }
  }
}
