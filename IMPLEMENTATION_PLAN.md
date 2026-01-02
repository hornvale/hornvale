# Hornvale Implementation Plan

## Phase 1: "Goat in a Room"
**Goal**: The simplest possible world that does something.
**Status**: Complete

### Deliverables
- [x] EntityId, Value (basic types)
- [x] Symbol interning
- [x] World struct with component storage
- [x] Basic queries (get component, iterate entities)
- [x] Tick loop (time passes)
- [x] Hardcoded "goat baas" rule in Rust (every 10 ticks)
- [x] WorldIO trait for I/O abstraction
- [x] Minimal REPL: `tick`, `inspect`, `list`, `help`, `quit`

### Files Created
- `src/symbol.rs` — Symbol interning with global intern table
- `src/core/entity.rs` — EntityId, EntityAllocator
- `src/core/value.rs` — Value enum (Int, Float, Bool, String, Symbol, EntityRef)
- `src/core/component.rs` — ComponentStorage using im::OrdMap
- `src/core/world.rs` — World struct
- `src/core.rs` — Module re-exports
- `src/io.rs` — WorldIO trait, StdIO, TestIO
- `src/systems.rs` — Hardcoded goat-baa system
- `src/repl.rs` — REPL commands

### Tests
35 unit tests covering all modules.

---

## Phase 2: Relations + Rules
**Goal**: Entities can relate to each other. Rules are data, not Rust code.
**Status**: Complete

### Deliverables
- [x] Relation tables (cardinality-aware per ARCHITECTURE.md)
- [x] Rule struct (pattern + effect)
- [x] Pattern matching (entity has component, InRelation)
- [x] Rule evaluation loop
- [x] Rules defined in Rust (DSL parsing is Phase 4)

### Files Created
- `src/core/relation.rs` — RelationTypeId, RelationSchema, RelationTable, RelationRegistry
- `src/rules.rs` — Module root
- `src/rules/pattern.rs` — Pattern enum, Var, matching logic
- `src/rules/rule.rs` — Rule struct, Trigger enum
- `src/rules/effect.rs` — Effect enum, execution logic
- `src/rules/engine.rs` — RuleSet, rule evaluation

### Tests
66 unit tests covering all modules.

---

## Phase 3: Derivation
**Goal**: Properties can be computed from other properties.
**Status**: Complete

### Deliverables
- [x] Derived components (computed on access)
- [x] Composition modes (Add, Multiply, Max, Min, Override, OverrideIfHigher, OverrideIfLower)
- [x] Caching + invalidation (dependency tracking, lazy invalidation)
- [x] GenerationContext (derivation path tracking)

### Files Created
- `src/derive.rs` — Module root
- `src/derive/compose.rs` — ComposeMode enum, compose_values(), Contribution
- `src/derive/rule.rs` — DerivationRule, DerivedProperty, ValueFn
- `src/derive/engine.rs` — DerivationEngine with caching, cycle detection
- `src/derive/cache.rs` — DerivedCache, CacheEntry, Dependency
- `src/derive/context.rs` — GenerationContext, GenerationStep

### Files Modified
- `src/core/world.rs` — Added get_component_derived(), set_component_notify(), add_relation_notify(), remove_relation_notify()
- `src/lib.rs` — Export derive module

### Tests
124 unit tests covering all modules.

---

## Phase 4: Language
**Goal**: Author worlds in DSL, not Rust.
**Status**: Not Started

### Planned Deliverables
- [ ] S-expr lexer/parser
- [ ] Entity/rule/template syntax
- [ ] Expression evaluation (bytecode compiler + register VM)
- [ ] Enhanced REPL

---

## Phase 5: Seeded Generation
**Goal**: Same seed → same world.
**Status**: Not Started

### Planned Deliverables
- [ ] Seeded RNG integrated with derivation
- [ ] Template instantiation with variation
- [ ] Lazy generation
