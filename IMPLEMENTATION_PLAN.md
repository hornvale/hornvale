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
**Status**: Complete

### Stage 1: Lexer and S-Expression Parser
**Status**: Complete

- [x] `src/lang.rs` — Module root
- [x] `src/lang/error.rs` — LexError, ParseError, Span
- [x] `src/lang/token.rs` — TokenKind, Token
- [x] `src/lang/lexer.rs` — Tokenizer (parens, numbers, strings, symbols, keywords)
- [x] `src/lang/ast.rs` — SExpr, Atom
- [x] `src/lang/parser.rs` — Parser, parse(), parse_all(), is_complete()

### Stage 2: Bytecode VM Core
**Status**: Complete

- [x] `src/vm.rs` — Module root
- [x] `src/vm/bytecode.rs` — OpCode enum (28 instructions)
- [x] `src/vm/chunk.rs` — Bytecode container with constant pool
- [x] `src/vm/exec.rs` — VM execution engine (256 registers)
- [x] `src/vm/stdlib.rs` — Standard library (15 functions)

### Stage 3: Expression Compiler
**Status**: Complete

- [x] `src/compiler.rs` — AST → bytecode (literals, arithmetic, comparison, logic, if, let, stdlib calls, world access)

### Stage 4: DSL Syntax & Integration (Minimal)
**Status**: Complete

- [x] `src/lang/loader.rs` — WorldLoader
- [x] Entity/relation/rule definitions

### Stage 5: Enhanced REPL
**Status**: Complete

- [x] load, eval, define, parse commands
- [x] Multi-line input (detects incomplete expressions)

### Tests
268 unit tests (63 for Stage 1, 39 for Stage 2, 33 for Stage 3, 4 for Stage 4, 5 for Stage 5)

---

## Phase 5: Seeded Generation
**Goal**: Same seed → same world.
**Status**: Not Started

### Stage 1: Seeded RNG
**Goal**: Deterministic random number generation from seeds.
**Status**: In Progress

- [x] `src/rng.rs` — SeededRng struct wrapping xoshiro256** PRNG
- [x] Core methods: `next_u64()`, `next_f64()`, `range()`, `range_f64()`, `choice()`, `shuffle()`, `chance()`
- [x] `fork()` and `fork_with_key()` methods for creating child RNGs with derived seeds
- [x] State serialization: `state()` and `from_state()` for save/load
- [ ] Integration with GenerationContext (seed field already exists)
- [ ] VM stdlib functions: `random`, `random-range`, `random-choice`

### Stage 2: Templates
**Goal**: Define archetypes that can be instantiated with variation.
**Status**: Not Started

- [ ] `src/template.rs` — Template struct (name, fields, defaults, variations)
- [ ] DSL syntax: `(template name :fields {...} :defaults {...} :variations {...})`
- [ ] Template registry in World
- [ ] `instantiate()` — Create entity from template + seed
- [ ] Field variation: ranges `(range 10 20)`, choices `(choice :a :b :c)`
- [ ] VM opcode or stdlib: `instantiate-template`

### Stage 3: Lazy Generation
**Goal**: Generate content on-demand, deterministically.
**Status**: Not Started

- [ ] `GenerationStub` — Placeholder for not-yet-generated content
- [ ] Generation triggers (on first access)
- [ ] `World::get_or_generate()` — Lazy component access
- [ ] Deterministic: same stub + same world state = same result
- [ ] DSL syntax for generators: `(generator name :inputs (...) :produces (...))`

### Tests
Target: ~40 tests across all stages (RNG determinism, template instantiation, lazy generation)
