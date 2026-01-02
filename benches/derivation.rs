//! Benchmarks for derivation engine and caching.

use std::sync::Arc;

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use hornvale::core::EntityId;
use hornvale::derive::{ComposeMode, Dependency, DerivationRule, DerivedProperty};
use hornvale::rules::Pattern;
use hornvale::{DerivationEngine, Value, World};

/// Benchmark deriving values with no caching.
fn bench_derive_no_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("derivation_no_cache");

    for entity_count in [100, 1_000, 10_000] {
        // Pre-create world with entities
        let mut world = World::new();
        for i in 0..entity_count {
            let entity = world.create_entity();
            world.set_component(entity, "BaseHP", Value::Int(100));
            world.set_component(entity, "Level", Value::Int(i as i64 % 20 + 1));
        }

        // Create engine without caching
        let mut engine = DerivationEngine::new_without_cache();
        engine.add_rule(DerivationRule::new(
            "effective-hp",
            Pattern::and(vec![
                Pattern::has_component("?e", "BaseHP"),
                Pattern::has_component("?e", "Level"),
            ]),
            DerivedProperty::new("EffectiveHP"),
            Arc::new(|w, e| {
                let base = w
                    .get_component(e, "BaseHP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                let level = w
                    .get_component(e, "Level")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(base + level * 10)
            }),
        ));

        group.throughput(Throughput::Elements(entity_count as u64));
        group.bench_with_input(
            BenchmarkId::new("derive_all", entity_count),
            &entity_count,
            |b, &count| {
                b.iter(|| {
                    let mut sum = 0i64;
                    for i in 0..count {
                        let entity = EntityId::from_raw(i as u64);
                        if let Ok(Some(Value::Int(v))) =
                            engine.derive(&world, entity, "EffectiveHP")
                        {
                            sum += v;
                        }
                    }
                    black_box(sum)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark deriving values with caching (cache hits).
fn bench_derive_cache_hit(c: &mut Criterion) {
    let mut group = c.benchmark_group("derivation_cache_hit");

    for entity_count in [100, 1_000, 10_000] {
        // Pre-create world with entities
        let mut world = World::new();
        for i in 0..entity_count {
            let entity = world.create_entity();
            world.set_component(entity, "BaseHP", Value::Int(100));
            world.set_component(entity, "Level", Value::Int(i as i64 % 20 + 1));
        }

        // Create engine with caching
        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "effective-hp",
            Pattern::and(vec![
                Pattern::has_component("?e", "BaseHP"),
                Pattern::has_component("?e", "Level"),
            ]),
            DerivedProperty::new("EffectiveHP"),
            Arc::new(|w, e| {
                let base = w
                    .get_component(e, "BaseHP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                let level = w
                    .get_component(e, "Level")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(base + level * 10)
            }),
        ));

        // Warm up cache
        for i in 0..entity_count {
            let entity = EntityId::from_raw(i as u64);
            let _ = engine.derive(&world, entity, "EffectiveHP");
        }

        group.throughput(Throughput::Elements(entity_count as u64));
        group.bench_with_input(
            BenchmarkId::new("derive_all_cached", entity_count),
            &entity_count,
            |b, &count| {
                b.iter(|| {
                    let mut sum = 0i64;
                    for i in 0..count {
                        let entity = EntityId::from_raw(i as u64);
                        if let Ok(Some(Value::Int(v))) =
                            engine.derive(&world, entity, "EffectiveHP")
                        {
                            sum += v;
                        }
                    }
                    black_box(sum)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark cache invalidation.
fn bench_cache_invalidation(c: &mut Criterion) {
    let mut group = c.benchmark_group("derivation_invalidation");

    for entity_count in [100, 1_000, 10_000] {
        // Pre-create world with entities
        let mut world = World::new();
        let mut entities = Vec::with_capacity(entity_count);
        for _ in 0..entity_count {
            let entity = world.create_entity();
            world.set_component(entity, "HP", Value::Int(100));
            entities.push(entity);
        }

        // Create engine with caching
        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "derived-hp",
            Pattern::has_component("?e", "HP"),
            DerivedProperty::new("DerivedHP"),
            Arc::new(|w, e| {
                let hp = w
                    .get_component(e, "HP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(hp * 2)
            }),
        ));

        // Warm up cache with dependencies
        for &entity in &entities {
            let deps = vec![Dependency::component(entity, "HP")];
            let _ = engine.derive_with_dependencies(&world, entity, "DerivedHP", deps);
        }

        group.throughput(Throughput::Elements(entity_count as u64));
        group.bench_with_input(
            BenchmarkId::new("invalidate_all", entity_count),
            &entity_count,
            |b, _| {
                b.iter(|| {
                    // Invalidate all entities
                    for &entity in &entities {
                        engine.notify_component_changed(entity, "HP");
                    }
                    black_box(engine.cache_stats())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark multiple rules composing values.
fn bench_multiple_rules(c: &mut Criterion) {
    let mut group = c.benchmark_group("derivation_multi_rule");

    for rule_count in [1, 5, 10, 20] {
        // Pre-create world with entities
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "IsCreature", Value::Bool(true));
        for r in 0..rule_count {
            let trait_name = format!("Trait{r}");
            world.set_component(entity, trait_name.as_str(), Value::Bool(true));
        }

        // Create engine with multiple additive rules
        let mut engine = DerivationEngine::new_without_cache();
        for r in 0..rule_count {
            let trait_name = format!("Trait{r}");
            let rule_name = format!("resistance-{r}");
            let contribution = 0.1 * (r + 1) as f64;
            engine.add_rule(
                DerivationRule::new(
                    rule_name.as_str(),
                    Pattern::has_component("?e", trait_name.as_str()),
                    DerivedProperty::new("TotalResistance"),
                    Arc::new(move |_, _| Value::Float(contribution)),
                )
                .with_composition(ComposeMode::Add),
            );
        }

        group.bench_with_input(
            BenchmarkId::new("compose", rule_count),
            &rule_count,
            |b, _| {
                b.iter(|| {
                    let result = engine.derive(&world, entity, "TotalResistance");
                    black_box(result)
                });
            },
        );
    }

    group.finish();
}

/// Benchmark pattern matching overhead.
fn bench_pattern_matching(c: &mut Criterion) {
    let mut group = c.benchmark_group("derivation_pattern");

    // Test different pattern complexities
    let patterns = vec![
        ("single_component", Pattern::has_component("?e", "Name")),
        (
            "two_components",
            Pattern::and(vec![
                Pattern::has_component("?e", "Name"),
                Pattern::has_component("?e", "HP"),
            ]),
        ),
        (
            "three_components",
            Pattern::and(vec![
                Pattern::has_component("?e", "Name"),
                Pattern::has_component("?e", "HP"),
                Pattern::has_component("?e", "Level"),
            ]),
        ),
        (
            "value_check",
            Pattern::component_value("?e", "Name", "goblin"),
        ),
    ];

    for (name, pattern) in patterns {
        // Pre-create world with matching entities
        let mut world = World::new();
        for _ in 0..1000 {
            let entity = world.create_entity();
            world.set_component(entity, "Name", "goblin");
            world.set_component(entity, "HP", Value::Int(100));
            world.set_component(entity, "Level", Value::Int(5));
        }

        let mut engine = DerivationEngine::new_without_cache();
        engine.add_rule(DerivationRule::new(
            "test-rule",
            pattern,
            DerivedProperty::new("TestDerived"),
            Arc::new(|_, _| Value::Int(42)),
        ));

        group.bench_with_input(BenchmarkId::new("pattern", name), &name, |b, _| {
            b.iter(|| {
                let mut count = 0usize;
                for i in 0..1000 {
                    let entity = EntityId::from_raw(i as u64);
                    if let Ok(Some(_)) = engine.derive(&world, entity, "TestDerived") {
                        count += 1;
                    }
                }
                black_box(count)
            });
        });
    }

    group.finish();
}

/// Benchmark deriving through World's get_component_derived API.
fn bench_world_derived(c: &mut Criterion) {
    let mut group = c.benchmark_group("world_get_derived");

    for entity_count in [100, 1_000, 10_000] {
        // Pre-create world with entities
        let mut world = World::new();
        for i in 0..entity_count {
            let entity = world.create_entity();
            world.set_component(entity, "BaseValue", Value::Int(i as i64));
        }

        // Create engine
        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "doubled",
            Pattern::has_component("?e", "BaseValue"),
            DerivedProperty::new("DerivedValue"),
            Arc::new(|w, e| {
                let base = w
                    .get_component(e, "BaseValue")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(base * 2)
            }),
        ));

        group.throughput(Throughput::Elements(entity_count as u64));
        group.bench_with_input(
            BenchmarkId::new("get_derived", entity_count),
            &entity_count,
            |b, &count| {
                b.iter(|| {
                    let mut sum = 0i64;
                    for i in 0..count {
                        let entity = EntityId::from_raw(i as u64);
                        if let Ok(Some(Value::Int(v))) =
                            world.get_component_derived(entity, "DerivedValue", &engine)
                        {
                            sum += v;
                        }
                    }
                    black_box(sum)
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_derive_no_cache,
    bench_derive_cache_hit,
    bench_cache_invalidation,
    bench_multiple_rules,
    bench_pattern_matching,
    bench_world_derived,
);
criterion_main!(benches);
