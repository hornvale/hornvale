//! Benchmarks for entity allocation and management.

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use hornvale::World;
use hornvale::core::EntityAllocator;

/// Benchmark raw entity ID allocation.
fn bench_allocator_create(c: &mut Criterion) {
    let mut group = c.benchmark_group("entity_allocator");

    for count in [100, 1_000, 10_000, 100_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("create", count), &count, |b, &count| {
            b.iter(|| {
                let mut alloc = EntityAllocator::new();
                for _ in 0..count {
                    black_box(alloc.create());
                }
            });
        });
    }

    group.finish();
}

/// Benchmark entity creation through World.
fn bench_world_create_entity(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_entity");

    for count in [100, 1_000, 10_000, 100_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("create", count), &count, |b, &count| {
            b.iter(|| {
                let mut world = World::new();
                for _ in 0..count {
                    black_box(world.create_entity());
                }
            });
        });
    }

    group.finish();
}

/// Benchmark iterating over all entities.
fn bench_all_entities_iteration(c: &mut Criterion) {
    let mut group = c.benchmark_group("entity_iteration");

    for count in [100, 1_000, 10_000, 100_000] {
        // Pre-create world with entities
        let mut world = World::new();
        for _ in 0..count {
            world.create_entity();
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("all_entities", count), &count, |b, _| {
            b.iter(|| {
                let mut sum = 0u64;
                for entity in world.all_entities() {
                    sum += entity.raw();
                }
                black_box(sum)
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_allocator_create,
    bench_world_create_entity,
    bench_all_entities_iteration,
);
criterion_main!(benches);
