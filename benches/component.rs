//! Benchmarks for component storage operations.

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use hornvale::{ComponentTypeId, Value, World};

/// Benchmark setting components on entities.
fn bench_set_component(c: &mut Criterion) {
    let mut group = c.benchmark_group("component_set");

    for count in [100, 1_000, 10_000, 100_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(
            BenchmarkId::new("single_type", count),
            &count,
            |b, &count| {
                b.iter(|| {
                    let mut world = World::new();
                    let name = ComponentTypeId::new("Name");
                    for i in 0..count {
                        let entity = world.create_entity();
                        world.set_component(entity, name, Value::Int(i as i64));
                    }
                    black_box(&world);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark setting multiple component types on each entity.
fn bench_set_multiple_components(c: &mut Criterion) {
    let mut group = c.benchmark_group("component_set_multiple");

    for count in [100, 1_000, 10_000] {
        group.throughput(Throughput::Elements(count as u64 * 5)); // 5 components each
        group.bench_with_input(
            BenchmarkId::new("five_types", count),
            &count,
            |b, &count| {
                b.iter(|| {
                    let mut world = World::new();
                    let name = ComponentTypeId::new("Name");
                    let hp = ComponentTypeId::new("HP");
                    let pos_x = ComponentTypeId::new("PosX");
                    let pos_y = ComponentTypeId::new("PosY");
                    let level = ComponentTypeId::new("Level");

                    for i in 0..count {
                        let entity = world.create_entity();
                        world.set_component(entity, name, Value::string("entity"));
                        world.set_component(entity, hp, Value::Int(100));
                        world.set_component(entity, pos_x, Value::Int(i as i64));
                        world.set_component(entity, pos_y, Value::Int(i as i64));
                        world.set_component(entity, level, Value::Int(1));
                    }
                    black_box(&world);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark getting components from entities.
fn bench_get_component(c: &mut Criterion) {
    let mut group = c.benchmark_group("component_get");

    for count in [100, 1_000, 10_000, 100_000] {
        // Pre-create world with entities and components
        let mut world = World::new();
        let name = ComponentTypeId::new("Name");
        let mut entities = Vec::with_capacity(count);
        for i in 0..count {
            let entity = world.create_entity();
            world.set_component(entity, name, Value::Int(i as i64));
            entities.push(entity);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("existing", count), &count, |b, _| {
            b.iter(|| {
                let mut sum = 0i64;
                for &entity in &entities {
                    if let Some(Value::Int(v)) = world.get_component(entity, name) {
                        sum += v;
                    }
                }
                black_box(sum)
            });
        });
    }

    group.finish();
}

/// Benchmark has_component checks.
fn bench_has_component(c: &mut Criterion) {
    let mut group = c.benchmark_group("component_has");

    for count in [100, 1_000, 10_000, 100_000] {
        // Pre-create world - half have the component, half don't
        let mut world = World::new();
        let name = ComponentTypeId::new("Name");
        let mut entities = Vec::with_capacity(count);
        for i in 0..count {
            let entity = world.create_entity();
            if i % 2 == 0 {
                world.set_component(entity, name, Value::Int(i as i64));
            }
            entities.push(entity);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("mixed", count), &count, |b, _| {
            b.iter(|| {
                let mut found = 0usize;
                for &entity in &entities {
                    if world.has_component(entity, name) {
                        found += 1;
                    }
                }
                black_box(found)
            });
        });
    }

    group.finish();
}

/// Benchmark iterating over entities with a component.
fn bench_entities_with(c: &mut Criterion) {
    let mut group = c.benchmark_group("component_entities_with");

    for count in [100, 1_000, 10_000, 100_000] {
        // Pre-create world - half have the component
        let mut world = World::new();
        let name = ComponentTypeId::new("Name");
        for i in 0..count {
            let entity = world.create_entity();
            if i % 2 == 0 {
                world.set_component(entity, name, Value::Int(i as i64));
            }
        }

        let expected = count / 2;
        group.throughput(Throughput::Elements(expected as u64));
        group.bench_with_input(BenchmarkId::new("half_matching", count), &count, |b, _| {
            b.iter(|| {
                let mut sum = 0i64;
                for (_, value) in world.entities_with(name) {
                    if let Value::Int(v) = value {
                        sum += v;
                    }
                }
                black_box(sum)
            });
        });
    }

    group.finish();
}

/// Benchmark getting all components for a single entity.
fn bench_components_of(c: &mut Criterion) {
    let mut group = c.benchmark_group("component_components_of");

    for num_components in [1, 5, 10, 20] {
        // Pre-create world with one entity having many components
        let mut world = World::new();
        let entity = world.create_entity();
        for i in 0..num_components {
            let comp = ComponentTypeId::new(&format!("Comp{i}"));
            world.set_component(entity, comp, Value::Int(i as i64));
        }

        group.throughput(Throughput::Elements(num_components as u64));
        group.bench_with_input(
            BenchmarkId::new("components", num_components),
            &num_components,
            |b, _| {
                b.iter(|| {
                    let components: Vec<_> = world.components_of(entity).collect();
                    black_box(components.len())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark overwriting existing component values (tests OrdMap update path).
fn bench_overwrite_component(c: &mut Criterion) {
    let mut group = c.benchmark_group("component_overwrite");

    for count in [100, 1_000, 10_000] {
        // Pre-create world with entities and components
        let mut world = World::new();
        let hp = ComponentTypeId::new("HP");
        let mut entities = Vec::with_capacity(count);
        for _ in 0..count {
            let entity = world.create_entity();
            world.set_component(entity, hp, Value::Int(100));
            entities.push(entity);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("update", count), &count, |b, _| {
            b.iter(|| {
                for (i, &entity) in entities.iter().enumerate() {
                    world.set_component(entity, hp, Value::Int(i as i64));
                }
                black_box(&world);
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_set_component,
    bench_set_multiple_components,
    bench_get_component,
    bench_has_component,
    bench_entities_with,
    bench_components_of,
    bench_overwrite_component,
);
criterion_main!(benches);
