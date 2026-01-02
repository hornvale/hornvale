//! Benchmarks for relation storage operations.

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use hornvale::{Cardinality, RelationSchema, World};

/// Benchmark inserting many-to-one relations (e.g., Location).
fn bench_insert_many_to_one(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_insert_many_to_one");

    for count in [100, 1_000, 10_000, 100_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("location", count), &count, |b, &count| {
            b.iter(|| {
                let mut world = World::new();
                world.register_relation(RelationSchema::new(
                    "Location",
                    Cardinality::Many,
                    Cardinality::One,
                ));

                // Create one room and many entities located there
                let room = world.create_entity();
                for _ in 0..count {
                    let entity = world.create_entity();
                    world.add_relation("Location", entity, room);
                }
                black_box(&world);
            });
        });
    }

    group.finish();
}

/// Benchmark inserting one-to-many relations (e.g., Contains).
fn bench_insert_one_to_many(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_insert_one_to_many");

    for count in [100, 1_000, 10_000, 100_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("contains", count), &count, |b, &count| {
            b.iter(|| {
                let mut world = World::new();
                world.register_relation(RelationSchema::new(
                    "Contains",
                    Cardinality::One,
                    Cardinality::Many,
                ));

                // Create one container with many contained entities
                let container = world.create_entity();
                for _ in 0..count {
                    let entity = world.create_entity();
                    world.add_relation("Contains", container, entity);
                }
                black_box(&world);
            });
        });
    }

    group.finish();
}

/// Benchmark inserting many-to-many relations (e.g., Knows).
fn bench_insert_many_to_many(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_insert_many_to_many");

    for count in [100, 1_000, 10_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("knows", count), &count, |b, &count| {
            b.iter(|| {
                let mut world = World::new();
                world.register_relation(RelationSchema::new(
                    "Knows",
                    Cardinality::Many,
                    Cardinality::Many,
                ));

                // Create entities that form a chain of "knows" relations
                let mut entities = Vec::with_capacity(count);
                for _ in 0..count {
                    entities.push(world.create_entity());
                }

                // Each entity knows the next one (and some others)
                for i in 0..count {
                    let next = (i + 1) % count;
                    world.add_relation("Knows", entities[i], entities[next]);
                }
                black_box(&world);
            });
        });
    }

    group.finish();
}

/// Benchmark forward relation queries.
fn bench_query_forward(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_query_forward");

    for count in [100, 1_000, 10_000] {
        // Pre-create world: one container with many items
        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));
        let container = world.create_entity();
        for _ in 0..count {
            let entity = world.create_entity();
            world.add_relation("Contains", container, entity);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("one_to_many", count), &count, |b, _| {
            b.iter(|| {
                let contents = world.query_relation_forward("Contains", container);
                black_box(contents.len())
            });
        });
    }

    group.finish();
}

/// Benchmark reverse relation queries.
fn bench_query_reverse(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_query_reverse");

    for count in [100, 1_000, 10_000] {
        // Pre-create world: many entities in one room
        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));
        let room = world.create_entity();
        for _ in 0..count {
            let entity = world.create_entity();
            world.add_relation("Location", entity, room);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("many_to_one", count), &count, |b, _| {
            b.iter(|| {
                let in_room = world.query_relation_reverse("Location", room);
                black_box(in_room.len())
            });
        });
    }

    group.finish();
}

/// Benchmark contains() check for relation existence.
fn bench_has_relation(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_has");

    for count in [100, 1_000, 10_000] {
        // Pre-create world: many entities, half in a relation
        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));
        let room = world.create_entity();
        let mut entities = Vec::with_capacity(count);
        for i in 0..count {
            let entity = world.create_entity();
            if i % 2 == 0 {
                world.add_relation("Location", entity, room);
            }
            entities.push(entity);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("mixed_check", count), &count, |b, _| {
            b.iter(|| {
                let mut found = 0usize;
                for &entity in &entities {
                    if world.has_relation("Location", entity, room) {
                        found += 1;
                    }
                }
                black_box(found)
            });
        });
    }

    group.finish();
}

/// Benchmark updating relations (replacing old target with new).
fn bench_relation_update(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_update");

    for count in [100, 1_000, 10_000] {
        // Pre-create world: entities in room1, will move to room2
        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));
        let room1 = world.create_entity();
        let room2 = world.create_entity();
        let mut entities = Vec::with_capacity(count);
        for _ in 0..count {
            let entity = world.create_entity();
            world.add_relation("Location", entity, room1);
            entities.push(entity);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("move_all", count), &count, |b, _| {
            b.iter(|| {
                // Move all entities to room2
                for &entity in &entities {
                    world.add_relation("Location", entity, room2);
                }
                black_box(&world);
            });
        });
    }

    group.finish();
}

/// Benchmark symmetric relation insertion.
fn bench_symmetric_relation(c: &mut Criterion) {
    let mut group = c.benchmark_group("relation_symmetric");

    for count in [100, 1_000, 10_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(
            BenchmarkId::new("friendship", count),
            &count,
            |b, &count| {
                b.iter(|| {
                    let mut world = World::new();
                    world.register_relation(RelationSchema::symmetric(
                        "Friendship",
                        Cardinality::Many,
                    ));

                    let mut entities = Vec::with_capacity(count);
                    for _ in 0..count {
                        entities.push(world.create_entity());
                    }

                    // Create friendships in a chain
                    for i in 0..count.saturating_sub(1) {
                        world.add_relation("Friendship", entities[i], entities[i + 1]);
                    }
                    black_box(&world);
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_insert_many_to_one,
    bench_insert_one_to_many,
    bench_insert_many_to_many,
    bench_query_forward,
    bench_query_reverse,
    bench_has_relation,
    bench_relation_update,
    bench_symmetric_relation,
);
criterion_main!(benches);
