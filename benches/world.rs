//! Benchmarks for world-level operations and tick cycles.

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use hornvale::io::WorldIO;
use hornvale::rules::{Effect, Pattern, Rule, RuleSet, Trigger};
use hornvale::{Cardinality, ComponentTypeId, RelationSchema, Value, World};

/// Null I/O for benchmarks - discards all output.
struct NullIO {
    output_len: usize,
}

impl NullIO {
    fn new() -> Self {
        Self { output_len: 0 }
    }
}

impl WorldIO for NullIO {
    fn print(&mut self, message: &str) {
        self.output_len += message.len();
    }

    fn read_line(&mut self) -> Option<String> {
        None
    }
}

/// Benchmark world cloning (tests persistent data structure overhead).
fn bench_world_clone(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_clone");

    for count in [100, 1_000, 10_000] {
        // Pre-create world with entities, components, and relations
        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));
        let room = world.create_entity();
        world.set_component(room, "Name", "Room");

        for i in 0..count {
            let entity = world.create_entity();
            world.set_component(entity, "Name", Value::string(format!("Entity{i}")));
            world.set_component(entity, "HP", Value::Int(100));
            world.add_relation("Location", entity, room);
        }

        group.bench_with_input(BenchmarkId::new("with_entities", count), &count, |b, _| {
            b.iter(|| {
                let cloned = world.clone();
                black_box(cloned.entity_count())
            });
        });
    }

    group.finish();
}

/// Benchmark tick advancement (simple case, no rules).
fn bench_tick_advance(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_tick");

    group.bench_function("advance_single", |b| {
        let mut world = World::new();
        b.iter(|| {
            world.advance_tick();
            black_box(world.tick())
        });
    });

    group.bench_function("advance_100", |b| {
        let mut world = World::new();
        b.iter(|| {
            world.advance_ticks(100);
            black_box(world.tick())
        });
    });

    group.finish();
}

/// Benchmark a simulated tick with rule evaluation.
fn bench_tick_with_rules(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_tick_rules");

    for entity_count in [10, 100, 1_000] {
        for rule_count in [1, 5, 10] {
            // Pre-create world with entities
            let mut world = World::new();
            for i in 0..entity_count {
                let entity = world.create_entity();
                world.set_component(entity, "Name", Value::string(format!("entity{i}")));
                world.set_component(entity, "HP", Value::Int(100));
            }

            // Create rules that match different patterns
            let mut rules = RuleSet::new();
            for r in 0..rule_count {
                let pattern_value = format!("entity{r}");
                let rule_name = format!("rule{r}");
                rules.add_rule(Rule::new(
                    rule_name.as_str(),
                    Pattern::component_value("?e", "Name", pattern_value.as_str()),
                    Trigger::every(10),
                    Effect::emit_message("Rule fired"),
                ));
            }

            let id = format!("{entity_count}e_{rule_count}r");
            group.bench_with_input(BenchmarkId::new("evaluate", &id), &id, |b, _| {
                let mut io = NullIO::new();
                b.iter(|| {
                    // Simulate a tick at interval boundary
                    let mut rules_clone = rules.clone();
                    rules_clone.evaluate(&world, &mut io, 10);
                    black_box(io.output_len)
                });
            });
        }
    }

    group.finish();
}

/// Benchmark mixed operations typical of a game tick.
fn bench_mixed_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_mixed_ops");

    for count in [100, 1_000, 10_000] {
        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(
            BenchmarkId::new("game_tick_sim", count),
            &count,
            |b, &count| {
                b.iter(|| {
                    let mut world = World::new();
                    world.register_relation(RelationSchema::new(
                        "Location",
                        Cardinality::Many,
                        Cardinality::One,
                    ));

                    let name = ComponentTypeId::new("Name");
                    let hp = ComponentTypeId::new("HP");

                    // Create a room
                    let room = world.create_entity();
                    world.set_component(room, name, "Room");

                    // Create entities
                    let mut entities = Vec::with_capacity(count);
                    for _ in 0..count {
                        let entity = world.create_entity();
                        world.set_component(entity, name, Value::string("creature"));
                        world.set_component(entity, hp, Value::Int(100));
                        world.add_relation("Location", entity, room);
                        entities.push(entity);
                    }

                    // Simulate a tick: read, update, query
                    let mut total_hp = 0i64;
                    for &entity in &entities {
                        // Read component
                        if let Some(Value::Int(current_hp)) = world.get_component(entity, hp) {
                            total_hp += current_hp;
                            // Update component
                            world.set_component(entity, hp, Value::Int(current_hp - 1));
                        }
                    }

                    // Query relations
                    let in_room = world.query_relation_reverse("Location", room);

                    black_box((total_hp, in_room.len()))
                });
            },
        );
    }

    group.finish();
}

/// Benchmark world state after many modifications (tests structural sharing).
fn bench_incremental_modifications(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_incremental");

    for count in [100, 1_000, 10_000] {
        // Pre-create world with entities
        let mut world = World::new();
        let hp = ComponentTypeId::new("HP");
        let mut entities = Vec::with_capacity(count);
        for _ in 0..count {
            let entity = world.create_entity();
            world.set_component(entity, hp, Value::Int(100));
            entities.push(entity);
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(BenchmarkId::new("update_all", count), &count, |b, _| {
            b.iter(|| {
                // Update all entities (simulates a tick that affects everything)
                for (i, &entity) in entities.iter().enumerate() {
                    world.set_component(entity, hp, Value::Int(i as i64));
                }
                black_box(world.tick())
            });
        });
    }

    group.finish();
}

/// Benchmark queries across multiple component types.
fn bench_multi_component_query(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_multi_query");

    for count in [100, 1_000, 10_000] {
        // Pre-create world: entities have varying component combinations
        let mut world = World::new();
        let name = ComponentTypeId::new("Name");
        let hp = ComponentTypeId::new("HP");
        let pos = ComponentTypeId::new("Position");

        for i in 0..count {
            let entity = world.create_entity();
            world.set_component(entity, name, Value::string("entity"));
            if i % 2 == 0 {
                world.set_component(entity, hp, Value::Int(100));
            }
            if i % 3 == 0 {
                world.set_component(entity, pos, Value::Int(i as i64));
            }
        }

        group.throughput(Throughput::Elements(count as u64));
        group.bench_with_input(
            BenchmarkId::new("filter_two_components", count),
            &count,
            |b, _| {
                b.iter(|| {
                    // Find entities with both Name and HP
                    let mut matching = 0usize;
                    for (entity, _) in world.entities_with(name) {
                        if world.has_component(entity, hp) {
                            matching += 1;
                        }
                    }
                    black_box(matching)
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_world_clone,
    bench_tick_advance,
    bench_tick_with_rules,
    bench_mixed_operations,
    bench_incremental_modifications,
    bench_multi_component_query,
);
criterion_main!(benches);
