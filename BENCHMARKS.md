# Hornvale Performance Benchmarks

This document tracks performance baselines for the Hornvale world VM.

## Running Benchmarks

```bash
cargo bench                    # Run all benchmarks
cargo bench --bench entity     # Run specific benchmark
cargo bench -- component_set   # Filter by name
```

Results are stored in `target/criterion/` with HTML reports.

---

## Baseline Results

**Platform**: macOS (Darwin), Apple Silicon
**Rust**: 1.85 (2024 edition)
**Date**: January 2025

### Entity Operations

| Operation | 100 | 1,000 | 10,000 | 100,000 |
|-----------|-----|-------|--------|---------|
| `allocator/create` | 34 ns | 335 ns | 3.3 µs | 33.8 µs |
| `world/create` | 257 ns | 610 ns | 4.1 µs | 32.6 µs |
| `all_entities` | 381 ps | 385 ps | 387 ps | 390 ps |

**Key insight**: Entity iteration is essentially free (~380 ps) regardless of count—it's just a range iterator over IDs.

### Component Operations

| Operation | 100 | 1,000 | 10,000 | 100,000 |
|-----------|-----|-------|--------|---------|
| `set/single_type` | 3.8 µs | 51 µs | 711 µs | 9.0 ms |
| `set/five_types` | 27 µs | 355 µs | 4.5 ms | — |
| `get/existing` | 1.1 µs | 16.5 µs | 266 µs | 3.5 ms |
| `has/mixed` | 674 ns | 14.7 µs | 252 µs | 3.2 ms |
| `entities_with` | 294 ns | 2.3 µs | 22 µs | 229 µs |
| `overwrite` | 2.9 µs | 35 µs | 556 µs | — |

| `components_of` | 1 comp | 5 comp | 10 comp | 20 comp |
|-----------------|--------|--------|---------|---------|
| time | 125 ns | 208 ns | 270 ns | 360 ns |

**Key insight**: `entities_with` iteration (220 Melem/s) is much faster than individual `get` calls (28 Melem/s at 100K). Prefer iteration over random access.

### Relation Operations

| Operation | 100 | 1,000 | 10,000 | 100,000 |
|-----------|-----|-------|--------|---------|
| `insert/many_to_one` | 7.9 µs | 106 µs | 1.6 ms | 20 ms |
| `insert/one_to_many` | 10.9 µs | 122 µs | 1.6 ms | 20 ms |
| `insert/many_to_many` | 26 µs | 379 µs | 8.8 ms | — |
| `query_forward` | 600 ns | 4.7 µs | 49 µs | — |
| `query_reverse` | 613 ns | 4.8 µs | 49 µs | — |
| `has_relation` | 3.0 µs | 37 µs | 487 µs | — |
| `update/move_all` | 15 µs | 192 µs | 3.4 ms | — |
| `symmetric` | 42 µs | 673 µs | 13.7 ms | — |

**Key insight**: Many-to-many relations are 2-4x slower than functional relations. Symmetric relations are the most expensive (~2x regular). Forward/reverse queries are equally fast due to dual indexing.

### World Operations

| Operation | 100 | 1,000 | 10,000 |
|-----------|-----|-------|--------|
| `clone` | 14 ns | 14 ns | 14 ns |
| `tick_advance` | 375 ps | — | — |
| `mixed_ops` | 26 µs | 329 µs | 4.3 ms |
| `incremental_update` | 2.9 µs | 35 µs | 544 µs |
| `multi_component_query` | 1.4 µs | 21 µs | 313 µs |

| `tick_with_rules` | 1 rule | 5 rules | 10 rules |
|-------------------|--------|---------|----------|
| 10 entities | 370 ns | 1.5 µs | 3.0 µs |
| 100 entities | 1.9 µs | 8.9 µs | 17.7 µs |
| 1,000 entities | 21 µs | 106 µs | 212 µs |

**Key insight**: World clone is ~14 ns regardless of size—structural sharing works! At 10K entities with mixed operations, tick time is 4.3 ms, well under the 16 ms budget for 60 fps.

### Derivation Operations

| Operation | 100 | 1,000 | 10,000 |
|-----------|-----|-------|--------|
| `derive/no_cache` | 22.8 µs | 251 µs | 2.9 ms |
| `derive/cache_hit` | 7.5 µs | 92 µs | 1.2 ms |
| `invalidate_all` | 17 µs | 203 µs | 3.3 ms |
| `world_get_derived` | 7.4 µs | 95 µs | 1.2 ms |

| `multi_rule/compose` | 1 rule | 5 rules | 10 rules | 20 rules |
|----------------------|--------|---------|----------|----------|
| time | 86 ns | 328 ns | 479 ns | 714 ns |

| `pattern` | single | two | three | value_check |
|-----------|--------|-----|-------|-------------|
| 1K entities | 117 µs | 133 µs | 158 µs | 117 µs |

**Key insight**: Cache hits are 2-3x faster than uncached derivation. Pattern complexity has modest impact (~35% overhead for 3 components vs 1).

---

## Key Findings

### What's Fast

1. **World cloning** (~14 ns) — Structural sharing makes snapshots essentially free
2. **Entity iteration** (~380 ps) — Just a range iterator, no allocation
3. **Tick advancement** (~375 ps) — Just an integer increment
4. **Forward/reverse queries** — Equally fast due to dual indexing

### What's Slower

1. **Many-to-many relations** — 2-4x slower than functional cardinalities
2. **Symmetric relations** — 2x overhead for auto-inserting reverse direction
3. **Random component access** — Prefer `entities_with` iteration over individual `get`

### Scaling Answers

| Question | Answer |
|----------|--------|
| Entities before tick > 16ms? | ~40K (extrapolated from 10K = 4.3ms) |
| Is World::clone() cheap for planners? | Yes, ~14ns regardless of size |
| Cache effectiveness? | 2-3x speedup on cache hits |
| Query overhead per additional component? | ~15-20% per component in pattern |

---

## Optimization Opportunities

If benchmarks reveal problems, consider:

1. **Hot path indexes** — Pre-computed queries for common patterns
2. **Stratified derivation** — Batch invalidations by dependency tier
3. **Arena allocation** — Pool entities to improve cache locality
4. **Lazy iteration** — Avoid collecting to `Vec` in queries
5. **Epoch-based invalidation** — Coarse-grained cache invalidation

---

## Running Updated Benchmarks

To update baselines:

```bash
cargo bench
# Results in target/criterion/*/new/estimates.json
```

Compare against baselines:

```bash
cargo bench -- --baseline main
```
