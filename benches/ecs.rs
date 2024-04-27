//! Benchmarking the ECS system.
#![allow(missing_docs)]
#![allow(unused_imports)]
#![allow(dead_code)]

pub mod ecs {
  pub mod add_remove;
  pub mod frag_iter;
  pub mod simple_insert;
}

use criterion::{criterion_group, criterion_main, Criterion};

fn add_remove(c: &mut Criterion) {
  c.bench_function("add_remove", |b| b.iter(|| ecs::add_remove::Benchmark::new().run()));
}
fn frag_iter(c: &mut Criterion) {
  // Broken ATM.
  c.bench_function("frag_iter", |b| b.iter(|| ecs::frag_iter::Benchmark::new().run()));
}
fn simple_insert(c: &mut Criterion) {
  // Broken ATM.
  c.bench_function("simple_insert", |b| {
    b.iter(|| ecs::simple_insert::Benchmark::new().run())
  });
}

criterion_group!(benchmarks, add_remove, frag_iter, simple_insert);
criterion_main!(benchmarks);

/*
Running benches/ecs.rs (target/release/deps/ecs-b65c8151b7d65087)
Gnuplot not found, using plotters backend
add_remove              time:   [17.558 ms 17.642 ms 17.763 ms]
                        change: [-2.6872% -1.2767% -0.1483%] (p = 0.03 < 0.05)
                        Change within noise threshold.
Found 4 outliers among 100 measurements (4.00%)
  1 (1.00%) high mild
  3 (3.00%) high severe

frag_iter               time:   [127.43 µs 127.79 µs 128.23 µs]
                        change: [-2.2723% -1.6045% -0.9935%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 19 outliers among 100 measurements (19.00%)
  11 (11.00%) low mild
  3 (3.00%) high mild
  5 (5.00%) high severe

simple_insert           time:   [19.606 ms 19.663 ms 19.730 ms]
                        change: [-1.9916% -1.1736% -0.5479%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 4 outliers among 100 measurements (4.00%)
  4 (4.00%) high severe

     Running benches/scanner.rs (target/release/deps/scanner-f36dd233bc1623b7)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
*/
