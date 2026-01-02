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
**Status**: Complete

### Stage 1: Seeded RNG
**Goal**: Deterministic random number generation from seeds.
**Status**: Complete

- [x] `src/rng.rs` — SeededRng struct wrapping xoshiro256** PRNG
- [x] Core methods: `next_u64()`, `next_f64()`, `range()`, `range_f64()`, `choice()`, `shuffle()`, `chance()`
- [x] `fork()` and `fork_with_key()` methods for creating child RNGs with derived seeds
- [x] State serialization: `state()` and `from_state()` for save/load
- [x] Integration with GenerationContext: `rng()`, `try_rng()`, `fork()`, `fork_with_key()`, `has_seed()`
- [x] VM opcodes: `Random`, `RandomRange` (with `with_seed()` / `with_rng()` builder methods)

### Stage 2: Templates
**Goal**: Define archetypes that can be instantiated with variation.
**Status**: Complete

- [x] `src/template.rs` — Template, FieldSpec, TemplateRegistry
- [x] FieldSpec variants: Fixed, IntRange, FloatRange, Choice, Chance
- [x] `instantiate()` — Create entity from template + SeededRng
- [x] `instantiate_with_seed()` — Convenience method
- [x] Template tags for categorization
- [x] DSL syntax: `(template name :doc "..." :tags (...) :fields (...))`
- [x] Integration with WorldLoader (`loader.templates()` accessor)

### Stage 3: Lazy Generation
**Goal**: Generate content on-demand, deterministically.
**Status**: Complete

- [x] `src/generator.rs` — GenerationStub, Generator, GeneratorRegistry, StubStorage
- [x] `GenerationStub` — Placeholder with generator name, seed, and parameters
- [x] `Generator` — Named function taking (world, entity, rng, params) → Value
- [x] `get_or_generate()` — Lazy component access that triggers generation
- [x] `has_or_stub()` — Check existence, creating stub if needed
- [x] Deterministic: same stub + same world state = same result
- [x] DSL syntax: `(generator name :doc "..." :produces (...))`
- [x] DSL syntax: `(stub entity :component C :generator G :seed S :params (...))`
- [x] Integration with WorldLoader (generators(), stubs() accessors)

### Tests
Target: ~40 tests across all stages
Current: 51 new tests (23 rng + 8 context + 12 template + 15 generator + 4 loader)
Total project: 339 tests passing

---

## Phase 6: Performance & Tooling Foundations
**Goal**: Establish performance baselines and unify REPL with DSL.
**Status**: Not Started

Performance and scalability are the chief risks to this project. Before adding content complexity, we need visibility into how the system scales and where the cliffs are.

### Stage 1: Benchmark Suite
**Goal**: Criterion benchmarks for core operations with documented baselines.
**Status**: Complete

- [x] Add `criterion` dev-dependency
- [x] `benches/entity.rs` — Entity allocation/deletion throughput
- [x] `benches/component.rs` — Component get/set/iterate at 100, 1K, 10K, 100K entities
- [x] `benches/relation.rs` — Relation add/remove/query (forward + reverse)
- [x] `benches/world.rs` — Full tick cycle with mixed operations
- [x] `benches/derivation.rs` — Cache hit/miss/invalidation scenarios
- [x] Document baselines in `BENCHMARKS.md`
- [ ] CI integration (track regressions, but don't block on them yet)

### Key Findings
- **~40K entities** before tick exceeds 16ms (60fps budget)
- **World::clone() is ~14ns** regardless of size — structural sharing works!
- **Cache hits are 2-3x faster** than uncached derivation
- **Many-to-many relations are 2-4x slower** than functional cardinalities

**Success Criteria**: Can answer "how many entities before tick takes >16ms?"

### Stage 2: Query Primitives
**Goal**: Expose graph traversal operations needed for gameplay.
**Status**: Not Started

- [ ] `descendants(entity, relation, depth_limit, stop_predicate)` — Transitive closure
- [ ] `ancestors(entity, relation, depth_limit)` — Reverse traversal
- [ ] `reachable(start, relations[], constraints)` — Multi-relation BFS/DFS
- [ ] Benchmark traversals at various depths/fan-outs
- [ ] Expose to DSL via new built-in functions

**Success Criteria**: Can query "all entities contained in this room, recursively, stopping at closed containers."

### Stage 3: REPL-DSL Bridge
**Goal**: REPL commands defined as data, not hardcoded Rust.
**Status**: Not Started

- [ ] `(repl-command name :args (...) :expands-to ...)` DSL form
- [ ] Argument substitution (`$1`, `$2`, or named args)
- [ ] Loader parses repl-command definitions
- [ ] REPL checks command registry before hardcoded commands
- [ ] Migrate existing commands to DSL definitions (optional, can coexist)
- [ ] Default command file loaded at REPL startup

**Example**:
```lisp
(repl-command "l" :doc "List all entities" :expands-to (list-entities))
(repl-command "i" :args (id) :doc "Inspect entity" :expands-to (inspect $id))
(repl-command "t" :args (?n) :doc "Advance N ticks" :expands-to (tick (or $n 1)))
```

**Success Criteria**: Can define new REPL shortcuts without touching Rust code.

### Tests
Target: ~30 tests (query primitives + REPL bridge)
Benchmarks: 6-8 benchmark groups with multiple scenarios each

---

## Phase 7: Interactive World
**Goal**: A minimal playable text adventure loop.
**Status**: Not Started

This phase proves the kernel can support interactive fiction as authored content, not just static world definitions.

### Stage 1: Input Pipeline
**Goal**: Player text becomes world state, processed by systems.
**Status**: Not Started

- [ ] `Input` component schema (raw text, source entity, tick)
- [ ] Host injects `Input` events into world via dedicated API
- [ ] Tokenizer system: `Input` → `Tokens` component
- [ ] Basic verb/noun parsing (no complex grammar yet)
- [ ] `Command` component (verb symbol, target entity, arguments)

**Success Criteria**: Typing "go north" creates a Command entity with verb=go, direction=north.

### Stage 2: Reference Resolution
**Goal**: Noun phrases resolve to entities via world queries.
**Status**: Not Started

- [ ] Candidate entities from scope (`InRoom`, `Carrying`, `Visible`)
- [ ] Match by `Name`, `Alias`, `Adjective` components
- [ ] Score boosting for distinguishing adjectives (e.g., "stunned goblin")
- [ ] Ambiguity handling (ask player or pick best match)
- [ ] `ResolvedCommand` with entity references instead of strings

**Success Criteria**: "take brass lamp" resolves to the correct entity among multiple objects.

### Stage 3: Core Verbs
**Goal**: Basic interactive fiction verbs as DSL-defined systems.
**Status**: Not Started

- [ ] `look` — Describe current room and visible contents
- [ ] `go <direction>` — Move through exits (handle doors, blocked, missing)
- [ ] `take <object>` — Pick up object (handle weight, fixed, container)
- [ ] `drop <object>` — Drop carried object
- [ ] `inventory` — List carried objects
- [ ] `examine <object>` — Detailed object description
- [ ] Output system: effects produce `Output` components, rendered by host

**Success Criteria**: Can navigate rooms, pick up and drop objects, look around.

### Stage 4: Minimal Demo World
**Goal**: A 3-room playable demo authored entirely in DSL.
**Status**: Not Started

- [ ] `examples/house.hvl` — Entry hall, kitchen, garden
- [ ] 2-3 objects (key, lamp, note)
- [ ] 1 locked door (key unlocks it)
- [ ] 1 NPC with simple reaction (greets player on entry)
- [ ] Win condition (reach garden with lamp)
- [ ] Can play through from REPL

**Success Criteria**: A complete, if tiny, playable game with no Rust code changes.

### Tests
Target: ~50 tests (parsing, resolution, verbs, integration)

---

## Phase 8: World Layers & Transactions
**Goal**: Base/overlay split and transactional rollback for planners.
**Status**: Not Started (Future)

### Planned Deliverables
- [ ] `BaseWorld` (immutable after generation) + `OverlayWorld` (mutable gameplay)
- [ ] `World` combines both with overlay-first lookup
- [ ] `begin_transaction()` / `commit()` / `rollback()` API
- [ ] Nested transactions for planner branching
- [ ] Snapshot API for cheap world copies
- [ ] World events that modify overlay without touching base

**Rationale**: Enables GOAP/A* planners to branch speculatively, cheap save/load (only overlay persists), and "world events don't rewrite history" semantics.

---

## Phase 9: Sensory Propagation
**Goal**: Stimuli propagate through the room graph.
**Status**: Not Started (Future)

### Planned Deliverables
- [ ] `Stimulus` component (kind, intensity, origin)
- [ ] `PropagationRule` per stimulus kind (attenuation, blockers)
- [ ] `Perceives(observer, stimulus)` derived relation
- [ ] Sound, smell, danger as stimulus kinds
- [ ] Portals as edges that can transmit/block stimuli
- [ ] Per-observer sense modifiers (enhanced hearing, etc.)

**Use Cases**:
- "You hear shouting from the west"
- "Your sword glows blue" (danger sense through portal)
- Enhanced senses extend perception range

---

## Phase 10: Advanced Content
**Goal**: Build upward as desired.
**Status**: Not Started (Future)

These are optional content layers, not kernel features:

- [ ] **Combat** — HP, attacks, damage, death as rules over components
- [ ] **Geography** — Room/area generation from templates
- [ ] **History** — Forward simulation of civilizations/events
- [ ] **Cosmos** — Stars, planets, calendars, zodiacs (the long-term vision)

Each layer is optional. The kernel doesn't require any of them.

---

## Appendix: Risk Registry

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Performance cliff at scale | Medium | High | Phase 6 benchmarks, early profiling |
| Query explosion (deep traversals) | Medium | Medium | Depth limits, materialized views |
| Cache invalidation storms | Low | High | Stratified derivation, epoch-based invalidation |
| DSL expressiveness gaps | Medium | Medium | Rust stdlib escape hatch, iterate on syntax |
| Scope creep (cosmos before house) | High | High | Strict phase ordering, playable demo first |

---

## Appendix: Definition of Done

For any phase to be marked Complete:

- [ ] All deliverables checked off
- [ ] Tests written and passing
- [ ] No clippy warnings
- [ ] Benchmarks added (if applicable)
- [ ] IMPLEMENTATION_PLAN.md updated with file list and test counts
