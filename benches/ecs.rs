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
Warning: Unable to complete 100 samples in 5.0s. You may wish to increase target time to 7.4s, enable flat sampling, or reduce sample count to 50.
add_remove              time:   [1.4707 ms 1.4772 ms 1.4859 ms]
                        change: [-91.757% -91.666% -91.600%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 8 outliers among 100 measurements (8.00%)
  1 (1.00%) low mild
  1 (1.00%) high mild
  6 (6.00%) high severe

frag_iter               time:   [57.968 µs 58.396 µs 58.961 µs]
                        change: [-45.269% -44.880% -44.450%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 12 outliers among 100 measurements (12.00%)
  5 (5.00%) high mild
  7 (7.00%) high severe

simple_insert           time:   [3.6362 ms 3.6521 ms 3.6697 ms]
                        change: [-82.177% -81.874% -81.657%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 6 outliers among 100 measurements (6.00%)
  2 (2.00%) high mild
  4 (4.00%) high severe

simple_iter             time:   [4.5176 ms 4.5367 ms 4.5607 ms]
                        change: [-78.243% -78.115% -77.993%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 8 outliers among 100 measurements (8.00%)
  1 (1.00%) low mild
  3 (3.00%) high mild
  4 (4.00%) high severe
*/
