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
**Status**: Complete ✓

- [x] `descendants(entity, relation, depth_limit, stop_predicate)` — Transitive closure
- [x] `ancestors(entity, relation, depth_limit)` — Reverse traversal
- [x] `reachable(start, relations[], constraints)` — Multi-relation BFS
- [x] Benchmark traversals at various depths/fan-outs
- [x] Expose to DSL via new built-in functions

**Implementation Summary**:
- Added `Value::List` variant with Arc<Vec<Value>> for cheap cloning
- New opcodes: `Descendants`, `Ancestors` in VM bytecode
- DSL forms: `(descendants entity :Rel depth)`, `(ancestors entity :Rel depth)`
- List stdlib functions: `length`, `first`, `rest`, `nth`, `empty?`, `cons`
- Benchmarks: tree traversal, chain traversal, cycle handling, multi-relation
- 363 tests passing

**Success Criteria**: Can query "all entities contained in this room, recursively, stopping at closed containers." ✓

### Stage 3: REPL-DSL Bridge
**Goal**: REPL commands defined as data, not hardcoded Rust.
**Status**: Complete ✓

- [x] `(repl-command name :args (...) :expands-to ...)` DSL form
- [x] Argument substitution (`$name` named args)
- [x] Loader parses repl-command definitions
- [x] REPL checks command registry before hardcoded commands
- [x] DSL commands shown in help output

**Implementation Summary**:
- `ReplCommand` struct with name, aliases, doc, args, expansion
- `ReplCommandRegistry` for storing DSL-defined commands
- `parse_repl_command()` parses DSL form into command struct
- REPL `execute_command` checks DSL commands first, expands and evaluates
- Expansion uses string template with `$arg` placeholders
- Help command shows both built-in and DSL-defined commands
- 375 tests passing

**Example**:
```lisp
(repl-command "add"
  :doc "Add two numbers"
  :args (a b)
  :expands-to "(+ $a $b)")

(repl-command "double"
  :aliases ("d")
  :args (x)
  :expands-to "(* $x 2)")
```

**Success Criteria**: Can define new REPL shortcuts without touching Rust code. ✓

### Tests
Target: ~30 tests (query primitives + REPL bridge)
Benchmarks: 6-8 benchmark groups with multiple scenarios each

---

## Phase 7: Interactive World
**Goal**: A minimal playable text adventure loop.
**Status**: Complete

This phase proves the kernel can support interactive fiction as authored content, not just static world definitions.

### Stage 1: Input Pipeline
**Goal**: Player text becomes world state, processed by systems.
**Status**: Complete ✓

- [x] `Input` component schema (raw text, source entity, tick)
- [x] Host injects `Input` events into world via dedicated API
- [x] Tokenizer system: `Input` → `Tokens` component
- [x] Basic verb/noun parsing (no complex grammar yet)
- [x] `Command` component (verb symbol, target entity, arguments)

**Implementation Summary**:
- Created `src/input.rs` with Input, Token, Command, ObjectRef types
- Tokenizer splits input on whitespace, normalizes to lowercase
- Parser handles: single verbs, verb+direction, verb+object, verb+object+preposition+object
- Direction abbreviations (n/s/e/w) expanded automatically
- Pronouns (it, them) recognized
- World methods: `inject_input()`, `inject_input_event()`, `parse_input_entity()`, `pending_inputs()`, `process_inputs()`
- 18 new tests (14 input + 4 world)
- 393 tests passing

**Success Criteria**: Typing "go north" creates a Command entity with verb=go, direction=north. ✓

### Stage 2: Reference Resolution
**Goal**: Noun phrases resolve to entities via world queries.
**Status**: Complete ✓

- [x] Candidate entities from scope (`InRoom`, `Carrying`, `Contains`)
- [x] Match by `Name`, `Alias`, `Adjective` components
- [x] Score boosting for distinguishing adjectives (e.g., "brass lamp" vs "rusty lamp")
- [x] Ambiguity handling (return multiple candidates, let caller decide)
- [x] `ResolvedCommand` with entity references instead of strings

**Implementation Summary**:
- Created `src/input/resolve.rs` with Resolver, EntityCandidate, ResolutionResult, ResolvedCommand
- ScopeProvider trait for customizable scoping
- DefaultScope finds entities in same room, carried by actor, or in containers
- Scoring: name match = 100 points, alias match = 80 points, adjective bonus = 50 points
- Case-insensitive matching
- 9 new tests
- 402 tests passing

**Success Criteria**: "take brass lamp" resolves to the correct entity among multiple objects. ✓

### Stage 3: Core Verbs
**Goal**: Basic interactive fiction verbs as DSL-defined systems.
**Status**: Complete ✓

- [x] `look` — Describe current room and visible contents
- [x] `go <direction>` — Move through exits (handle blocked, missing)
- [x] `take <object>` — Pick up object (handle fixed, portable)
- [x] `drop <object>` — Drop carried object
- [x] `inventory` — List carried objects
- [x] `examine <object>` — Detailed object description
- [x] `VerbResult` struct for handler output

**Implementation Summary**:
- Created `src/verbs.rs` with verb handlers
- Components: Name, Brief, Description, RoomDescription, IsRoom, Portable, Fixed, IsPlayer
- Relations: InRoom, Contains (for inventory)
- Exit system via `Exit_<direction>` components on rooms
- Direction shortcuts (n/s/e/w/etc.) map to go verb
- 10 new tests
- 412 tests passing

**Success Criteria**: Can navigate rooms, pick up and drop objects, look around. ✓

### Stage 4: Minimal Demo World
**Goal**: A 3-room playable demo authored entirely in DSL.
**Status**: Complete ✓

- [x] `examples/house.hvl` — Entry hall, kitchen, garden
- [x] 3 objects (brass lamp, iron key, yellowed note)
- [x] Cat NPC (fixed in kitchen)
- [x] Game commands integrated into REPL
- [x] Can play through from REPL with load command

**Implementation Summary**:
- Created `examples/house.hvl` with 3 rooms, 3 objects, 1 NPC
- Integrated game commands (look, go, take, drop, inventory, examine) into REPL
- Direction shortcuts (n/s/e/w/etc.) work in REPL
- find_player() locates player entity with IsPlayer=true
- 2 new REPL tests for game commands
- 414 tests passing

**How to Play**:
```
> load examples/house.hvl
> look
> take lamp
> n
> take key
> e
```

**Success Criteria**: A complete, if tiny, playable game with no Rust code changes. ✓

### Tests
Target: ~50 tests (parsing, resolution, verbs, integration)
Current: 39 tests (Stage 1: 18, Stage 2: 9, Stage 3: 10, Stage 4: 2)
Total project: 414 tests passing

---

## Phase 8: World Layers & Transactions
**Goal**: Base/overlay split and transactional rollback for planners.
**Status**: Complete

**Rationale**: Enables GOAP/A* planners to branch speculatively, cheap save/load (only overlay persists), and "world events don't rewrite history" semantics.

### Stage 1: World Layering
**Goal**: Base/overlay split with overlay-first lookup.
**Status**: Complete ✓

- [x] `WorldSnapshot` struct for frozen base state
- [x] `freeze()` method to make current state the immutable base
- [x] `is_frozen()` to check if base layer exists
- [x] `reset_overlay()` to discard all changes since freeze
- [x] `unfreeze()` to merge overlay into single layer
- [x] `is_component_modified(entity, component)` to check if value differs from base
- [x] `get_base_component()` to access base values
- [x] `is_relation_modified()` to check relation changes
- [x] `modified_components()` to iterate all modified values

**Implementation Summary**:
- Added `WorldSnapshot` struct holding entities, components, relations, tick
- `freeze()` clones current state to base snapshot (O(1) via structural sharing)
- `reset_overlay()` restores from snapshot (O(1))
- 11 new tests for layering functionality
- 425 tests passing

**Success Criteria**: Can load world, freeze it, make changes, then reset to original state. ✓

### Stage 2: Transaction Basics
**Goal**: Single-level transaction support.
**Status**: Complete ✓

- [x] `begin_transaction()` creates savepoint (stack-based from the start)
- [x] `commit_transaction()` drops savepoint, keeping changes
- [x] `rollback_transaction()` restores from savepoint
- [x] `in_transaction()` checks if active
- [x] `transaction_depth()` returns nesting level

**Implementation Summary**:
- Transaction savepoint stack stores WorldSnapshot per level
- O(1) begin/rollback/commit via structural sharing
- 11 new tests for transactions
- 436 tests passing

**Success Criteria**: Can speculatively modify world and rollback. ✓

### Stage 3: Nested Transactions
**Goal**: Stack of transaction frames for planner branching.
**Status**: Complete ✓

- [x] Transaction stack instead of single transaction
- [x] Each frame stores full snapshot
- [x] Rollback pops one frame, restores state
- [x] Commit pops one frame, keeps current state
- [x] `transaction_depth()` to query nesting level
- [x] `transaction_savepoint(depth)` to access savepoints

**Implementation Summary**:
- Implemented as part of Stage 2 using Vec<WorldSnapshot>
- Nested to arbitrary depth
- Tests verify 3-level nesting with mixed commit/rollback

**Success Criteria**: Can explore multiple decision branches with nested savepoints. ✓

### Stage 4: Snapshot API
**Goal**: Cheap world copies via structural sharing.
**Status**: Complete ✓

- [x] `snapshot()` returns lightweight World copy
- [x] `to_snapshot()` returns WorldSnapshot struct
- [x] `restore_from_snapshot()` restores state from WorldSnapshot
- [x] Snapshots share structure via im crate
- [x] Benchmarks for snapshot, transaction, freeze/reset performance
- [x] 8 new tests for snapshot functionality

**Implementation Summary**:
- Snapshots use Clone which is O(1) due to structural sharing
- `restore_from_snapshot()` preserves base layer but clears transactions
- Added benchmarks: `world_snapshot`, `world_transactions`, `world_nested_transactions`, `world_freeze_reset`
- 444 tests passing

**Success Criteria**: 10K entity world snapshots in <100μs. ✓ (Structural sharing makes this ~14ns regardless of size)

### Tests
Target: ~30 tests across all stages
Current: 30 new tests (11 layering + 11 transactions + 8 snapshot)
Total project: 444 tests passing

---

## Phase 9: Command Architecture
**Goal**: Data-driven verb system with grammar DSL, syntax tables, and object hooks.
**Status**: In Progress

**Rationale**: The current verb implementation conflates syntax and semantics, hardcodes verb handlers in Rust, and lacks object-specific responses. A proper IF engine needs:
- Multiple syntaxes routing to the same action ("look at X" = "examine X")
- Grammar patterns defined in DSL, not Rust
- Objects that respond to being acted upon (Before/On/After hooks)
- Preconditions as data (reachable, visible, portable)

This is more foundational than sensory propagation — it's the soul of an IF engine.

### Stage 1: Syntax Tables
**Goal**: Decouple surface syntax from underlying actions.
**Status**: Complete

- [x] `Action` type — the semantic operation (examine, take, throw-into)
- [x] `SyntaxElement` enum — Word, Noun, Direction, Any, Optional
- [x] `SyntaxPattern` type — pattern matching words/slots to actions
- [x] `SyntaxTable` registry of all syntax patterns (priority-based matching)
- [x] Multiple syntaxes per action (look at X, examine X, x X → examine)
- [x] Named slot support (noun:object, direction:dir)
- [x] DSL form: `(syntax "look" "at" noun :to examine)`
- [x] Integration with REPL command pipeline (`execute_action`)
- [x] Entity resolution by name from action slots

**Files Created/Modified**:
- `src/syntax.rs` — Action, SyntaxElement, SyntaxPattern, SyntaxTable, PatternBuilder
- `src/lang/loader.rs` — Added `load_syntax()`, syntax_table field
- `src/verbs.rs` — Added `execute_action()`, `resolve_entity_by_name()`
- `src/repl/mod.rs` — Integrated syntax table matching

**Tests**: 22 tests for syntax module, 6 tests for loader syntax parsing, 1 integration test

**Example**:
```lisp
(syntax "look" :to look-around)
(syntax "look" "at" noun :to examine)
(syntax "look" "in" noun :to search)
(syntax "look" "under" noun :to look-under)
(syntax "look" direction :to look-direction)
(syntax "l" :to look-around)
(syntax "examine" noun :to examine)
(syntax "x" noun :to examine)
```

**Success Criteria**: "look at lamp" and "examine lamp" and "x lamp" all route to examine action.

### Stage 2: Grammar DSL
**Goal**: Define command patterns in .hvl files with semantic constraints.
**Status**: Not Started

- [ ] Grammar pattern syntax with slots
- [ ] Semantic type annotations on slots (`:portable`, `:container`, `:living`)
- [ ] Optional elements in patterns `["with" weapon]`
- [ ] Compile patterns to efficient matcher (trie or PEG)
- [ ] Disambiguation via semantic constraints
- [ ] DSL form: `(grammar action-name ...patterns...)`

**Example**:
```lisp
(grammar take
  "take" object:portable
  "get" object:portable
  "pick" "up" object:portable
  "take" object:portable "from" container:container)

(grammar attack
  "attack" target:living
  "attack" target:living "with" weapon:weapon
  "kill" target:living
  "hit" target:living)

(grammar put
  "put" object:held "in" container:container
  "put" object:held "on" surface:supporter
  "drop" object:held "in" container:container)
```

**Success Criteria**: Can define new verbs entirely in DSL without Rust changes.

### Stage 3: Object Hooks
**Goal**: Objects respond to being acted upon via Before/On/After handlers.
**Status**: Not Started

- [ ] `Before:<action>` component — runs before action, can veto/modify
- [ ] `On:<action>` component — primary handler, can replace default
- [ ] `After:<action>` component — runs after action completes
- [ ] `:handled` return value to suppress default behavior
- [ ] `:veto` return value to cancel action entirely
- [ ] Hook invocation order: actor → direct-object → indirect-object → room
- [ ] DSL syntax for defining hooks inline on entities

**Example**:
```lisp
(entity holy-book
  (Name "book")
  (On:burn
    (say "As the flames touch the sacred text, lightning strikes you down!")
    (kill actor)
    :handled))

(entity candles
  (Name "candles")
  (On:count
    (say "Let's see, how many objects in a pair? Don't tell me...")
    :handled))

(entity river
  (Name "river")
  (On:throw
    (cond
      [(eq? direct-object actor)
       (say "You dive in. The current is too strong...")
       (drown actor)]
      [(lit? direct-object)
       (say "It floats for a moment, then sinks.")
       (destroy direct-object)]
      [else
       (say "It splashes into the water and is gone.")
       (destroy direct-object)])))
```

**Success Criteria**: Objects have custom responses to verbs; "burn book" triggers deity intervention.

### Stage 4: Precondition System
**Goal**: Actions have declarative preconditions checked before execution.
**Status**: Not Started

- [ ] `Precondition` type — predicate on world state
- [ ] Built-in preconditions: `reachable?`, `visible?`, `held?`, `portable?`, `not-held?`
- [ ] Custom preconditions via DSL expressions
- [ ] Automatic failure messages based on violated precondition
- [ ] Actions declare required preconditions
- [ ] DSL form: `(action name :preconditions [...] :handler ...)`

**Example**:
```lisp
(action take
  :preconditions
    [(reachable? actor direct-object)
     (visible? actor direct-object)
     (portable? direct-object)
     (not (held-by? direct-object actor))]
  :handler take-handler)

(action unlock
  :preconditions
    [(reachable? actor direct-object)
     (locked? direct-object)
     (has-key-for? actor direct-object)]
  :handler unlock-handler)
```

**Failure messages**:
- `(not (reachable? ...))` → "You can't reach the lamp."
- `(not (portable? ...))` → "The lamp is fixed in place."
- `(not (has-key-for? ...))` → "You don't have the right key."

**Success Criteria**: Action preconditions are data; failure messages auto-generated.

### Stage 5: Description System
**Goal**: Contextual descriptions based on object state.
**Status**: Not Started

- [ ] Multiple description components: `Description`, `InitialDescription`, `GroundDescription`
- [ ] `FirstSeen` component to track player discovery
- [ ] Derivation rules to select appropriate description
- [ ] `Describe` function that picks correct description for context
- [ ] Support for dynamic descriptions via derivation

**Example**:
```lisp
(entity brass-lamp
  (Description "An ornate brass lamp, slightly tarnished.")
  (InitialDescription "A brass lamp sits on the mantle, gleaming softly.")
  (GroundDescription "There's a brass lamp here."))

;; Derivation rules select the right one
(rule initial-description
  :when (and (first-seen? ?obj) (has? ?obj InitialDescription))
  :derive (CurrentDescription ?obj)
  :from (InitialDescription ?obj))

(rule ground-description
  :when (and (on-ground? ?obj) (has? ?obj GroundDescription))
  :derive (CurrentDescription ?obj)
  :from (GroundDescription ?obj))
```

**Success Criteria**: Lamp has different descriptions based on context.

### Stage 6: Configurable Directions
**Goal**: Direction set defined in DSL, not hardcoded.
**Status**: Not Started

- [ ] `(directions ...)` DSL form to define direction set
- [ ] Direction properties: abbreviation, opposite, display name
- [ ] Contextual directions: "home", "out", "back"
- [ ] Computed directions: "toward X" (pathfinding)
- [ ] Component naming convention: `Exit:north` (with pattern matching)

**Example**:
```lisp
(directions
  (north :abbrev "n" :opposite south :display "to the north")
  (south :abbrev "s" :opposite north :display "to the south")
  (east :abbrev "e" :opposite west :display "to the east")
  (west :abbrev "w" :opposite east :display "to the west")
  (up :abbrev "u" :opposite down :display "above")
  (down :abbrev "d" :opposite up :display "below")
  (in :abbrev nil :opposite out :display "inside")
  (out :abbrev nil :opposite in :display "outside")
  ;; Game-specific
  (upstream :opposite downstream)
  (home :contextual true))  ;; resolved via player state
```

**Success Criteria**: Can add custom directions without Rust changes.

### Architecture Summary

Command processing pipeline after Phase 9:

```
Player Input: "throw book into river"
       ↓
┌─────────────────────────────────┐
│  1. TOKENIZE                    │
│  ["throw", "book", "into", "river"]
└─────────────────────────────────┘
       ↓
┌─────────────────────────────────┐
│  2. GRAMMAR MATCH               │
│  Pattern: "throw" obj "into" obj│
│  Action: throw-into             │
│  Slots: {direct: "book",        │
│          indirect: "river"}     │
└─────────────────────────────────┘
       ↓
┌─────────────────────────────────┐
│  3. RESOLVE REFERENCES          │
│  "book" → holy-book (in scope)  │
│  "river" → river (in scope)     │
│  Ambiguity? → ask player        │
└─────────────────────────────────┘
       ↓
┌─────────────────────────────────┐
│  4. CHECK PRECONDITIONS         │
│  - Is book reachable? ✓         │
│  - Is river a valid target? ✓   │
└─────────────────────────────────┘
       ↓
┌─────────────────────────────────┐
│  5. BEFORE HOOKS                │
│  - Check book's Before:throw    │
│  - Check river's Before:receive │
│  - Check room's Before:throw    │
│  (any can veto or modify)       │
└─────────────────────────────────┘
       ↓
┌─────────────────────────────────┐
│  6. OBJECT ACTION (river)       │
│  - river.On:throw runs          │
│  - Returns :handled             │
│  (default action skipped)       │
└─────────────────────────────────┘
       ↓
┌─────────────────────────────────┐
│  7. AFTER HOOKS                 │
│  - Check book's After:throw     │
│  - Check river's After:receive  │
│  - Check room's After:throw     │
└─────────────────────────────────┘
       ↓
Output: "The book splashes into the water and is gone."
```

### Tests
Target: ~80 tests across all stages
- Stage 1: ~15 tests (syntax matching, routing)
- Stage 2: ~20 tests (grammar parsing, pattern compilation)
- Stage 3: ~20 tests (hook invocation, veto/handled)
- Stage 4: ~10 tests (precondition checking)
- Stage 5: ~10 tests (description selection)
- Stage 6: ~5 tests (direction configuration)

---

## Phase 10: Sensory Propagation
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

## Phase 11: Advanced Content
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
