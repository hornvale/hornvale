//! Benchmarks for graph traversal queries.

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use hornvale::{Cardinality, RelationSchema, RelationTypeId, World};

/// Build a tree structure: one root with `fanout` children at each level, `depth` levels deep.
fn build_tree(fanout: usize, depth: usize) -> (World, hornvale::EntityId) {
    let mut world = World::new();
    world.register_relation(RelationSchema::new(
        "Contains",
        Cardinality::One,
        Cardinality::Many,
    ));

    let root = world.create_entity();
    world.set_component(root, "Name", "root");

    fn add_children(
        world: &mut World,
        parent: hornvale::EntityId,
        fanout: usize,
        remaining_depth: usize,
    ) {
        if remaining_depth == 0 {
            return;
        }
        for _ in 0..fanout {
            let child = world.create_entity();
            world.add_relation("Contains", parent, child);
            add_children(world, child, fanout, remaining_depth - 1);
        }
    }

    add_children(&mut world, root, fanout, depth);
    (world, root)
}

/// Build a chain: entity 0 -> entity 1 -> ... -> entity n
fn build_chain(length: usize) -> (World, hornvale::EntityId) {
    let mut world = World::new();
    world.register_relation(RelationSchema::new(
        "Contains",
        Cardinality::One,
        Cardinality::Many,
    ));

    let first = world.create_entity();
    let mut current = first;
    for _ in 1..length {
        let next = world.create_entity();
        world.add_relation("Contains", current, next);
        current = next;
    }
    (world, first)
}

/// Benchmark descendants on tree structures.
fn bench_descendants_tree(c: &mut Criterion) {
    let mut group = c.benchmark_group("query_descendants_tree");

    // Test different tree configurations: (fanout, depth, expected_nodes)
    let configs = [
        (2, 4, 30),   // Binary tree, 4 levels: 2+4+8+16 = 30 nodes
        (3, 3, 39),   // Ternary tree, 3 levels: 3+9+27 = 39 nodes
        (5, 3, 155),  // 5-ary tree, 3 levels: 5+25+125 = 155 nodes
        (10, 2, 110), // 10-ary tree, 2 levels: 10+100 = 110 nodes
    ];

    for (fanout, depth, expected) in configs {
        let (world, root) = build_tree(fanout, depth);
        let id = format!("f{fanout}_d{depth}");

        group.throughput(Throughput::Elements(expected as u64));
        group.bench_with_input(BenchmarkId::new("full", &id), &id, |b, _| {
            b.iter(|| {
                let result = world.descendants_all(root, "Contains", 100);
                black_box(result.len())
            });
        });
    }

    group.finish();
}

/// Benchmark descendants with depth limits.
fn bench_descendants_depth_limited(c: &mut Criterion) {
    let mut group = c.benchmark_group("query_descendants_depth");

    // Build a deep tree
    let (world, root) = build_tree(3, 6); // 3^6 = 729 leaves, total ~1092 nodes

    for max_depth in [1, 2, 3, 4, 5, 6] {
        group.bench_with_input(
            BenchmarkId::new("depth", max_depth),
            &max_depth,
            |b, &depth| {
                b.iter(|| {
                    let result = world.descendants_all(root, "Contains", depth);
                    black_box(result.len())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark descendants with stop predicate.
fn bench_descendants_with_stop(c: &mut Criterion) {
    let mut group = c.benchmark_group("query_descendants_stop");

    // Build tree and mark some nodes as "closed"
    let (mut world, root) = build_tree(4, 4); // 4^4 = 256 leaves

    // Mark ~25% of level-1 nodes as closed
    let level1 = world.query_relation_forward("Contains", root);
    for (i, &entity) in level1.iter().enumerate() {
        if i % 4 == 0 {
            world.set_component(entity, "Closed", true);
        }
    }

    group.bench_function("with_stop_predicate", |b| {
        b.iter(|| {
            let result = world.descendants(root, "Contains", 100, |e| {
                world.get_component(e, "Closed") == Some(&hornvale::Value::Bool(true))
            });
            black_box(result.len())
        });
    });

    group.bench_function("without_stop_predicate", |b| {
        b.iter(|| {
            let result = world.descendants_all(root, "Contains", 100);
            black_box(result.len())
        });
    });

    group.finish();
}

/// Benchmark ancestors (reverse traversal).
fn bench_ancestors(c: &mut Criterion) {
    let mut group = c.benchmark_group("query_ancestors");

    for length in [10, 50, 100, 500] {
        let (world, first) = build_chain(length);

        // Get the last entity in the chain
        let mut current = first;
        for _ in 1..length {
            let children = world.query_relation_forward("Contains", current);
            if children.is_empty() {
                break;
            }
            current = children[0];
        }
        let last = current;

        group.throughput(Throughput::Elements(length as u64 - 1));
        group.bench_with_input(BenchmarkId::new("chain", length), &length, |b, _| {
            b.iter(|| {
                let result = world.ancestors(last, "Contains", 1000);
                black_box(result.len())
            });
        });
    }

    group.finish();
}

/// Benchmark reachable with multiple relation types.
fn bench_reachable(c: &mut Criterion) {
    let mut group = c.benchmark_group("query_reachable");

    // Build a world with two relation types
    let mut world = World::new();
    world.register_relation(RelationSchema::new(
        "Exit",
        Cardinality::Many,
        Cardinality::Many,
    ));
    world.register_relation(RelationSchema::new(
        "Portal",
        Cardinality::Many,
        Cardinality::Many,
    ));

    // Create a grid of rooms connected by exits, with some portals
    let size = 10;
    let mut rooms = Vec::new();
    for _ in 0..(size * size) {
        rooms.push(world.create_entity());
    }

    // Connect horizontally and vertically via Exit
    for y in 0..size {
        for x in 0..size {
            let idx = y * size + x;
            if x + 1 < size {
                world.add_relation("Exit", rooms[idx], rooms[idx + 1]);
                world.add_relation("Exit", rooms[idx + 1], rooms[idx]);
            }
            if y + 1 < size {
                world.add_relation("Exit", rooms[idx], rooms[idx + size]);
                world.add_relation("Exit", rooms[idx + size], rooms[idx]);
            }
        }
    }

    // Add some portals (diagonal shortcuts)
    world.add_relation("Portal", rooms[0], rooms[99]); // corner to corner
    world.add_relation("Portal", rooms[99], rooms[0]);
    world.add_relation("Portal", rooms[45], rooms[54]); // middle area
    world.add_relation("Portal", rooms[54], rooms[45]);

    let start = rooms[0];
    let exit: RelationTypeId = "Exit".into();
    let portal: RelationTypeId = "Portal".into();

    group.bench_function("exits_only", |b| {
        b.iter(|| {
            let result = world.reachable(start, &[exit], 20);
            black_box(result.len())
        });
    });

    group.bench_function("exits_and_portals", |b| {
        b.iter(|| {
            let result = world.reachable(start, &[exit, portal], 20);
            black_box(result.len())
        });
    });

    group.finish();
}

/// Benchmark cycle handling in traversal.
fn bench_cycle_handling(c: &mut Criterion) {
    let mut group = c.benchmark_group("query_cycle");

    // Build a cyclic graph
    let mut world = World::new();
    world.register_relation(RelationSchema::new(
        "Link",
        Cardinality::Many,
        Cardinality::Many,
    ));

    let node_count = 100;
    let mut nodes = Vec::new();
    for _ in 0..node_count {
        nodes.push(world.create_entity());
    }

    // Connect in a ring plus some cross-links
    for i in 0..node_count {
        world.add_relation("Link", nodes[i], nodes[(i + 1) % node_count]);
        // Add some cross-links
        if i % 10 == 0 {
            world.add_relation("Link", nodes[i], nodes[(i + 50) % node_count]);
        }
    }

    group.bench_function("cyclic_graph", |b| {
        b.iter(|| {
            let result = world.descendants_all(nodes[0], "Link", 100);
            black_box(result.len())
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_descendants_tree,
    bench_descendants_depth_limited,
    bench_descendants_with_stop,
    bench_ancestors,
    bench_reachable,
    bench_cycle_handling,
);
criterion_main!(benches);
