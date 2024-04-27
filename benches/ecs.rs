//! Benchmarking the ECS system.
#![allow(missing_docs)]
#![allow(unused_imports)]
#![allow(dead_code)]

pub mod ecs {
  pub mod add_remove;
  pub mod frag_iter;
  pub mod simple_insert;
  pub mod simple_iter;
}

use criterion::{criterion_group, criterion_main, Criterion};

fn add_remove(c: &mut Criterion) {
  c.bench_function("add_remove", |b| b.iter(|| ecs::add_remove::Benchmark::new().run()));
}
fn frag_iter(c: &mut Criterion) {
  c.bench_function("frag_iter", |b| b.iter(|| ecs::frag_iter::Benchmark::new().run()));
}
fn simple_insert(c: &mut Criterion) {
  c.bench_function("simple_insert", |b| {
    b.iter(|| ecs::simple_insert::Benchmark::new().run())
  });
}
fn simple_iter(c: &mut Criterion) {
  c.bench_function("simple_iter", |b| b.iter(|| ecs::simple_iter::Benchmark::new().run()));
}

criterion_group!(benchmarks, add_remove, frag_iter, simple_insert, simple_iter);
criterion_main!(benchmarks);

/*
     Running benches/ecs.rs (target/release/deps/ecs-b65c8151b7d65087)
Gnuplot not found, using plotters backend
add_remove              time:   [17.853 ms 17.932 ms 18.064 ms]
                        change: [+0.7796% +1.6434% +2.6386%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 3 outliers among 100 measurements (3.00%)
  1 (1.00%) high mild
  2 (2.00%) high severe

frag_iter               time:   [129.81 µs 130.32 µs 131.02 µs]
                        change: [+1.4587% +1.9353% +2.4886%] (p = 0.00 < 0.05)
                        Performance has regressed.
Found 5 outliers among 100 measurements (5.00%)
  3 (3.00%) high mild
  2 (2.00%) high severe

simple_insert           time:   [20.079 ms 20.268 ms 20.517 ms]
                        change: [+2.0625% +3.0752% +4.2357%] (p = 0.00 < 0.05)
                        Performance has regressed.
Found 11 outliers among 100 measurements (11.00%)
  6 (6.00%) high mild
  5 (5.00%) high severe

simple_iter             time:   [20.657 ms 20.695 ms 20.736 ms]
Found 6 outliers among 100 measurements (6.00%)
  4 (4.00%) high mild
  2 (2.00%) high severe
*/
