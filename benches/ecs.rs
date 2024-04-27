//! Benchmarking the ECS system.
#![allow(missing_docs)]
#![allow(unused_imports)]
#![allow(dead_code)]

pub mod ecs {
  pub mod add_remove;
  pub mod frag_iter;
}

use criterion::{criterion_group, criterion_main, Criterion};

fn add_remove(c: &mut Criterion) {
  c.bench_function("add_remove", |b| b.iter(|| ecs::add_remove::Benchmark::new().run()));
}
fn frag_iter(c: &mut Criterion) {
  // Broken ATM.
  c.bench_function("frag_iter", |b| b.iter(|| ecs::frag_iter::Benchmark::new().run()));
}

criterion_group!(benchmarks, add_remove, frag_iter);
criterion_main!(benchmarks);
