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
- Relations: InRoom, Contains (for inventory), Exit (room→exit entity), Destination (exit→target room)
- Exit system via composite entities: exit entities have Direction component, rooms reference exits via Exit relation
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
**Status**: REMOVED — Superseded by Stage 2 grammar system

The SyntaxTable system was originally implemented but later **removed entirely** in favor of the more powerful grammar/command system (Stage 2). The grammar system provides:
- Type predicates for noun validation (not just string matching)
- Trie-based matching for efficient lookup
- Better integration with entity resolution

**Files Removed**:
- `src/syntax.rs` — Deleted entirely
- Syntax-related code removed from `loader.rs`, `verbs.rs`, `repl.rs`

**Migration**: Use the `(command ...)` DSL form instead of `(syntax ...)`.

**Success Criteria**: "look at lamp" and "examine lamp" and "x lamp" all route to examine action.

### Stage 2: Grammar DSL
**Goal**: Define command patterns in .hvl files with semantic constraints.
**Status**: Complete ✓

- [x] Grammar pattern syntax with typed slots (`name:type`)
- [x] Type predicate definitions via `(type name predicate)` DSL form
- [x] Command definitions via `(command name :aliases (...) :forms (...))` DSL form
- [x] Trie-based intent dispatch (longest prefix match for aliases)
- [x] Linear form matching with slot capture
- [x] Type predicate evaluation during matching
- [x] Standard verbs file (`examples/std-verbs.hvl`)
- [x] Removed hardcoded game commands from REPL

**Implementation Summary**:
- Created `src/grammar/` module with types, trie, matcher, predicate, registry
- Type predicates: `(type portable (has? entity :Portable))` — evaluated via AST pattern matching
- Command definition: aliases + forms in one place
- Form patterns: slots (`obj:noun`, `dir:direction`) + literal words
- IntentTrie dispatches on first word(s) to find command, then form matcher validates pattern
- Entity resolution via existing `Resolver` (scope, adjectives, scoring)
- Lexer updated to allow `:` in middle of symbols for clean slot syntax
- 48 new grammar tests, 520 total tests passing

**DSL Syntax**:
```lisp
;; Type predicates
(type portable (has? entity :Portable))
(type held (held-by? entity actor))

;; Command definitions
(command look
  :aliases ("l")
  :forms
    ((() -> look-around)
     ((dir:direction) -> (look-direction dir))
     (("at" obj:noun) -> (examine obj))
     (("in" obj:noun) -> (search obj))))

(command take
  :aliases ("get" "grab" "pick")
  :forms
    (((obj:noun) -> (take obj))
     (("up" obj:noun) -> (take obj))
     ((obj:noun "up") -> (take obj))))
```

**Success Criteria**: Can define new verbs entirely in DSL without Rust changes. ✓

### Stage 3: Object Hooks
**Goal**: Objects respond to being acted upon via Before/On/After handlers.
**Status**: Complete ✓

- [x] `Before:<action>` component — runs before action, can veto/modify
- [x] `On:<action>` component — primary handler, can replace default
- [x] `After:<action>` component — runs after action completes
- [x] `:handled` return value to suppress default behavior
- [x] `:veto` return value to cancel action entirely
- [x] Hook invocation order: actor → direct-object → indirect-object → room
- [x] DSL syntax for defining hooks inline on entities

**Implementation Summary**:
- Created `src/hooks.rs` with HookResult, HookError, HookPipelineResult
- Hook context opcodes: GetHookActor, GetHookDirectObject, GetHookIndirectObject, GetHookRoom
- Effect functions: `(say ...)` outputs text, `(destroy ...)` marks entity for deletion
- Hook bodies stored as `Value::List`, automatically wrapped in `(do ...)` during execution
- Added `do`/`begin`/`progn` form to compiler for sequencing expressions
- World helpers: `get_hook()`, `has_hook()`, `get_hooks_for_action()`, `hooks_for_phase()`, `delete_entity()`
- Integrated into verb execution pipeline with proper Before/On/After ordering
- DSL loader parses hook components and detects duplicates
- 33 new tests for hooks module

**Files Created/Modified**:
- `src/hooks.rs` — Hook execution engine (new)
- `src/vm/bytecode.rs` — Added hook context and effect opcodes
- `src/vm/exec.rs` — Added HookContext struct, output buffer, pending deletions
- `src/compiler.rs` — Added context accessors, effect functions, do/begin/progn form
- `src/core/component.rs` — Added `components_matching()` iterator
- `src/core/world.rs` — Added hook helpers and `delete_entity()`
- `src/verbs.rs` — Integrated hook pipeline into `execute_grammar_action()`
- `src/lang/loader.rs` — Parse hook DSL syntax
- `src/repl/mod.rs` — Pass stdlib to verb execution
- `src/lib.rs` — Export hooks module

**Example**:
```lisp
(entity holy-book
  (Name "book")
  (On:burn
    (say "As the flames touch the sacred text, lightning strikes you down!")
    (destroy (actor))
    :handled))

(entity candles
  (Name "candles")
  (On:count
    (say "Let's see, how many objects in a pair? Don't tell me...")
    :handled))

(entity torch
  (Name "torch")
  (On:burn
    (say "The torch flares brightly!")
    :handled))
```

**Success Criteria**: Objects have custom responses to verbs; "burn book" triggers deity intervention. ✓

### Stage 4: Precondition System
**Goal**: Actions have declarative preconditions checked before execution.
**Status**: Complete ✓

**Design Notes**:
- **Commands** are requests/inputs (grammar/syntax layer)
- **Actions** are attempts to do something in the world (semantic layer)
- Not all commands trigger actions (e.g., "help", "save", "quit" are meta-commands)
- Not all actions come from commands (e.g., NPC actions, triggered effects)

**Deliverables**:
- [x] `Precondition` type — named predicate with check expression and failure message template
- [x] `(precondition name :params (...) :check expr :failure "template")` DSL form
- [x] Built-in preconditions with simple semantics:
  - `reachable?` — entity is in scope (same room, carried, or in open container in scope)
  - `visible?` — same as reachable for now (lighting deferred to Phase 11)
  - `held?` — `(held? obj)` → obj is held by current actor (implicit)
  - `held-by?` — `(held-by? obj holder)` → obj is held by specified holder (explicit)
  - `portable?` — entity has `Portable` component (or lacks `Fixed`)
  - `not-held?` — inverse of `held?`
- [x] `Action` type — named action with preconditions list and handler reference
- [x] `(action name :preconditions (...) :handler handler-name)` DSL form
- [x] `ActionRegistry` for storing action definitions
- [x] Precondition checking integrated into verb execution pipeline (after resolution, before hooks)
- [x] Automatic failure messages with entity name interpolation
- [x] Handler registry for referencing Rust handlers by symbol

**Files Created/Modified**:
- `src/precondition.rs` — Precondition type, PreconditionRegistry, built-in definitions (new)
- `src/action.rs` — Action type, ActionRegistry, HandlerRegistry, handler dispatch (new)
- `src/lang/loader.rs` — Parse `(precondition ...)` and `(action ...)` forms
- `src/verbs.rs` — Added `execute_grammar_action_full()` with precondition checking
- `src/lib.rs` — Export new modules and types
- `examples/std-verbs.hvl` — Added precondition and action definitions

**Example**:
```lisp
;; Define preconditions with failure messages
(precondition reachable?
  :params (actor target)
  :check (in-scope? target actor)
  :failure "You can't reach ~(name target).")

(precondition portable?
  :params (obj)
  :check (has? obj :Portable)
  :failure "~(Name obj) is fixed in place.")

(precondition not-held?
  :params (obj)
  :check (not (held? obj))
  :failure "You're already holding ~(name obj).")

;; Define actions with preconditions (note: uses () not [])
(action take
  :preconditions
    ((reachable? actor direct-object)
     (visible? actor direct-object)
     (portable? direct-object)
     (not-held? direct-object))
  :handler take-handler)

(action unlock
  :preconditions
    ((reachable? actor direct-object)
     (locked? direct-object)
     (has-key-for? actor direct-object))
  :handler unlock-handler)
```

**Success Criteria**: Action preconditions are data; failure messages auto-generated.

### Stage 4.5: Action Specificity (Future) (NOT DONE)
**Goal**: Actions can have `:when` guards for context-specific behavior.
**Status**: Not Started (deferred)

This stage adds CLOS-style method dispatch to actions:
- `:when` guards for action specificity (most specific match wins)
- `:failure-override` for context-specific messages
- Dispatch ordering based on guard specificity

**Example** (future):
```lisp
;; Base action
(action take
  :preconditions [...]
  :handler take-handler)

;; More specific: taking from a container
(action take
  :when (container? indirect-object)
  :preconditions [(open? indirect-object) ...]
  :handler take-from-handler)

;; Most specific: taking the crown from the dragon's hoard
(action take
  :when (and (= direct-object crown) (= indirect-object dragon-hoard))
  :failure-override [(not (distracted? dragon)) "The dragon watches you too closely."]
  :handler take-crown-handler)
```

### Stage 5: DSL-First Verbs & Description System
**Goal**: Move verb logic from Rust to DSL. Add contextual descriptions.
**Status**: Complete ✓ (5A, 5B, 5C, 5D, 5E, 5F all complete)

This is a significant architectural change: all game verbs move to DSL, with Rust providing efficient primitives. This also includes the description system for contextual object descriptions.

#### Stage 5A: Core Primitives
**Goal**: Rust primitives for world mutation and queries.
**Status**: Complete ✓

**World Mutation** (infallible, `fn!` convention):
- [x] `set!` opcode — set component value on entity
- [x] `relate!` opcode — add relation between entities (cardinality auto-enforced)
- [x] `unrelate!` opcode — remove relation between entities
- [x] `destroy` already exists (buffers for later deletion)
- Note: `move!` and `give!` are DSL functions built on `relate!` (cardinality handles atomicity)

**World Queries** (pure functions):
- [x] `location` — get entity's room (via InRoom relation)
- [x] `holder` — get entity's holder (who Contains it)
- [x] `contents` — list entities contained by container
- [x] `exits` — list available exit directions from room
- [x] `exit-target` — get destination for exit direction (or nil)
- [x] `(room)` — get action context room (0 args, existing)

**Predicates**:
- [x] `in-scope?` — check if entity is reachable by actor
- [x] `held-by?` — check if item is held by holder
- [x] `portable?` — check if entity is portable (Portable=true or !Fixed)
- Note: `descendant?` can use existing `descendants` opcode

**Implementation Summary**:
- Added opcodes: `SetComponent`, `Relate`, `Unrelate`, `GetHolder`, `GetContents`,
  `GetExits`, `GetExitTarget`, `GetRoom`, `InScope`, `IsHeldBy`, `IsPortable`
- Mutations are buffered: `pending_set_components`, `pending_relate`, `pending_unrelate`
- VM helper methods: `is_in_scope()`, `is_held_by()`, `is_portable()`, `get_entity_room()`
- Compiler support for all new DSL forms

**Files Modified**:
- `src/vm/bytecode.rs` — Added 11 new opcodes
- `src/vm/exec.rs` — Implemented opcodes + predicate helpers
- `src/compiler.rs` — Added compile functions for DSL forms

**Tests**: 13 new tests (606 total, up from 593)

#### Stage 5B: Description System
**Goal**: Article generation with name flags. Description selection is DSL-defined.
**Status**: Complete ✓

**Rust Primitives** (minimal, mechanical):
- [x] `(the entity)` opcode — definite article + name, respecting flags
- [x] `(a entity)` opcode — indefinite article + name, respecting flags
- [x] `(tamper! entity)` opcode — set Tampered=true on entity
- [x] Name flag components: `ProperNoun`, `VowelSound`, `NoArticle`, `PluralNoun`
- [x] Description flag components: `InitialDescription`, `GroundDescription`, `Tampered`

**DSL-Defined** (semantic, customizable — deferred to Stage 5D):
- `(describe entity context)` — composes fallback chain based on context
- `(tampered? entity)` → `(has? entity :Tampered)`
- Description selection rules (InitialDescription vs GroundDescription)
- Room visit tracking via HasVisited relation

**Design Notes**:
- `InitialDescription`: Shown until object is "tampered with" (picked up, moved, etc.)
- `GroundDescription`: Shown in room listings after tampering
- `Tampered` flag: Set when object is interfered with (global, not per-observer)
- Room visits: Per-observer via relation (e.g., `HasVisited(player, room)`)
- `ProperNoun`: No article, capitalize (e.g., "Bob" not "the Bob")
- `VowelSound`: Use "an" instead of "a" (e.g., "an hour")
- `NoArticle`: Omit articles entirely (e.g., "you" not "the you")

**Files Modified**:
- `src/vm/bytecode.rs` — Added `The`, `A`, `Tamper` opcodes
- `src/vm/exec.rs` — Implemented opcodes with `format_the()`, `format_a()` helpers
- `src/compiler.rs` — Added `compile_the()`, `compile_a()`, `compile_tamper()` functions
- `src/verbs.rs` — Added component type helpers for name flags and descriptions

**Tests**: 10 new tests (4 for `the`, 5 for `a`, 1 for `tamper!`)
- `test_the_basic`, `test_the_proper_noun`, `test_the_no_article`, `test_the_no_name`
- `test_a_basic`, `test_a_vowel_heuristic`, `test_a_vowel_flag_override`, `test_a_proper_noun`, `test_a_no_article`
- `test_tamper_sets_component`

Total project: 615 tests passing

#### Stage 5C: File Loading
**Goal**: `(load "path.hvl")` for including definitions.
**Status**: Complete ✓

- [x] `(load "path.hvl")` form in DSL
- [x] Relative path resolution (relative to loading file)
- [x] Circular dependency detection (error at load time)
- [x] Track loaded files to prevent double-loading

**Implementation Summary**:
- Added `loaded_files: OrdSet<PathBuf>` to track loaded files (canonical paths)
- Added `loading_stack: Vec<PathBuf>` for circular dependency detection
- `load_file()` canonicalizes paths, checks for cycles, prevents double-loading
- `load_include()` handles `(load "path")` DSL form with relative path resolution
- `is_file_loaded()` and `loaded_file_count()` helper methods for testing
- Added `LoadError::CircularDependency` error variant

**Files Modified**:
- `src/lang/loader.rs` — Added file loading with tracking and cycle detection

**Tests**: 9 new tests
- `test_load_file_basic` — Basic file loading
- `test_load_file_double_load_is_noop` — Same file loaded twice is no-op
- `test_load_include_directive` — `(load "file")` form works
- `test_load_nested_includes` — Nested includes resolve correctly
- `test_load_circular_dependency_error` — Circular deps detected
- `test_load_self_reference_error` — Self-reference detected as cycle
- `test_load_diamond_dependency` — Diamond dependencies work (shared file loaded once)
- `test_load_invalid_path_error` — Nonexistent file gives IO error
- `test_load_invalid_load_syntax` — Bad syntax gives helpful errors

Total project: 593 tests passing

#### Stage 5D: User-Defined Functions (defun)
**Goal**: Support user-defined functions in DSL for building standard library.
**Status**: Complete ✓

**Implementation Summary**:
- [x] `FunctionDef` struct — stores function name, parameter list, and body AST
- [x] `FunctionRegistry` — stores user-defined functions with OrdMap
- [x] `(defun name (params...) body...)` DSL form in loader
- [x] Inline expansion at compile time (no runtime call frames needed)
- [x] Multiple body expressions wrapped in `(do ...)`
- [x] Arity checking at compile time

**Files Created/Modified**:
- `src/lang/function.rs` — `FunctionDef`, `FunctionRegistry` (new)
- `src/lang.rs` — Added module and exports
- `src/lang/loader.rs` — Added `function_registry` field, `load_defun()` method
- `src/compiler.rs` — Added `with_functions()`, `compile_with_functions()`, `compile_user_function_call()`

**Design Notes**:
- Inline expansion: function calls are expanded at compile time, not runtime
- No recursion support (would cause infinite expansion)
- Functions must be defined before use (no forward references)
- Standard library (`libs/std/_lib.hvl`) convention planned for Stage 5E

**Tests**: 17 new tests
- 9 compiler tests: simple, multiple params, do body, let, conditionals, stdlib calls, nested calls, arity mismatch, no params
- 8 loader tests: basic, multiple params, no params, multi body, missing name/params/body, multiple functions

Total project: 635 tests passing

#### Stage 5E: Standard Library (formerly 5D)
**Goal**: Standard verbs defined in DSL. Deprecate Rust verb handlers.
**Status**: Complete ✓

**Design Decisions**:
- Helper functions (`move!`, `give!`) are `defun` wrappers around `relate!` (not primitives)
- `describe` function is DSL-defined in stdlib
- All verb handlers move to DSL; Rust handlers remain as fallback for backward compatibility
- Standard library at `libs/std/_lib.hvl`

**Deliverables**:

- [x] Create `libs/std/_lib.hvl` (entry point for standard library)
- [x] Helper functions:
  - `(defun move! (entity destination) ...)` — relocate entity via InRoom
  - `(defun give! (obj from to) ...)` — transfer object via Contains
  - `(defun describe (entity context) ...)` — compose description based on context
  - `(defun try-go (direction) ...)` — directional movement helper
- [x] Type predicates: `portable`, `container`, `held`
- [x] Standard actions with DSL handlers:
  - `look-around` — describe current room and contents
  - `examine` — describe object
  - `go-north`, `go-south`, etc. — directional movement
  - `take` — pick up portable object
  - `drop` — release held object
  - `inventory` — list carried objects
- [x] Grammar commands for all standard verbs
- [x] Update `examples/house.hvl` to `(load "libs/std/_lib.hvl")`

**Files Created/Modified**:
- `libs/std/_lib.hvl` — Standard library (new, ~280 lines)
- `examples/house.hvl` — Updated to load stdlib
- `.claude/ARCHITECTURE.md` — Updated DSL-First Verb Architecture section

**Notes**:
- Rust verb handlers in `src/verbs.rs` are kept as fallback for backward compatibility
- Games can `override` or `extend` any stdlib action
- Direction-specific actions (`go-north`, etc.) use `try-go` helper

**Tests**: Existing tests pass (634 total)

#### Stage 5F: DSL Action Handlers (formerly 5E)
**Goal**: Actions execute DSL code, not Rust functions.
**Status**: Complete ✓

**Prerequisite Design**: The action execution contract is documented in `.claude/ARCHITECTURE.md` under "Action Execution Contract". Key points:
- **Action-as-Transaction**: Entire action attempt runs in a single transaction
- **Read-your-writes**: Each phase sees pending writes from earlier phases
- **Rollback semantics**: Veto/failure → rollback all changes
- **Nested transactions**: Triggered actions get nested transactions

**Transaction Infrastructure** (Complete):
- [x] `PendingMutations` struct — collects all VM pending changes (set_components, relate, unrelate, deletions)
- [x] `PendingMutations::apply_to(world)` — applies all collected mutations
- [x] Implement action-as-transaction model in `execute_grammar_action_full`
- [x] Begin transaction at action start (after precondition checks)
- [x] Apply mutations after each hook phase (Before, On, After) for read-your-writes
- [x] Rollback on veto or hook error
- [x] Commit on success

**DSL Handler Infrastructure** (Complete):
- [x] `ActionHandler` enum — `Builtin(Symbol)` or `Dsl(SExpr)` for handler storage
- [x] `ActionResult` enum — `Success` or `Failure(Option<String>)` for explicit return values
- [x] `execute_dsl_handler()` — compile and execute DSL handler through VM
- [x] Explicit return values: `:success`, `:failure`, `(:failure "msg")`
- [x] `Action` struct modified: handler is `ActionHandler` enum
- [x] Handler compilation and execution through VM
- [x] DSL handler integration in `execute_grammar_action_full`
- [x] Loader parses DSL handlers in `(action ... :handler (do ...))` form
- [x] `(override action name ...)` for replacing existing actions
- [x] `(extend action name :before ...)` for adding preconditions

**Cleanup Work** (completed):
- [x] ~~Deprecate~~ **Removed** Rust verb handlers from `src/verbs.rs` entirely
- [x] Update REPL to use DSL-defined actions exclusively via `execute_grammar_action_full`
- [x] Remove fallback to `verbs::execute_command` (old command parsing system)
- [x] Remove `execute_command` re-export from `lib.rs`
- [x] Remove `HandlerRegistry::with_builtins()` and `register_builtins()`
- [x] Update tests to use DSL action handlers

**Files Modified**:
- `src/action.rs` — Added `ActionHandler` enum, `ActionResult`, `execute_dsl_handler()`, `DslHandlerResult`; removed builtin handlers
- `src/lang/loader.rs` — Parse DSL handlers, `(override ...)`, `(extend ...)` forms
- `src/verbs.rs` — Removed Rust handlers; now only provides `execute_grammar_action_full` and component/relation helpers
- `src/repl.rs` — Updated to use `execute_grammar_action_full` with action/precondition registries
- `src/hooks.rs` — Added `PendingMutations` struct
- `src/lib.rs` — Export new types; removed deprecated `execute_command`

**Tests**: 627 tests passing
- All verb handlers now defined in DSL (`libs/std/_lib.hvl`)
- Rust handlers completely removed (~530 lines deleted from `src/verbs.rs`)
- Composite exit entities with Exit/Destination relations (not Exit_<direction> components)

#### Example: Full DSL Verb Definition

```lisp
;; stdlib.hvl

;; Preconditions
(precondition reachable?
  :params (actor target)
  :check (in-scope? target actor)
  :failure "You can't reach ~(the target).")

(precondition portable?
  :params (obj)
  :check (has? obj :Portable)
  :failure "~(The obj) is fixed in place.")

(precondition not-held?
  :params (obj)
  :check (not (held-by? obj actor))
  :failure "You're already holding ~(the obj).")

;; Action
(action take
  :preconditions
    ((reachable? actor direct-object)
     (portable? direct-object)
     (not-held? direct-object))
  :handler
    (do
      (give! direct-object (holder direct-object) actor)
      (say "Taken.")
      :success))

(action examine
  :preconditions
    ((reachable? actor direct-object))
  :handler
    (do
      (say (describe direct-object :examine))
      :success))
```

**Success Criteria**:
- All standard verbs defined in DSL, not Rust
- `(load "stdlib.hvl")` works
- Description system shows InitialDescription once, then Description
- Games can override standard verbs without Rust changes

### Stage 6: Configurable Directions
**Goal**: Direction set defined in DSL, not hardcoded.
**Status**: Complete ✓

- [x] `(directions ...)` DSL form to define direction set
- [x] Direction properties: abbreviation, opposite, display name
- [x] `DirectionDef` and `DirectionRegistry` types in Rust
- [x] Remove hardcoded DIRECTIONS arrays from matcher.rs and input.rs
- [x] Standard directions in `libs/std/_lib.hvl`
- [ ] Contextual directions: "home", "out", "back" (deferred to future)
- [ ] Computed directions: "toward X" (deferred to future, requires pathfinding)

**Implementation Summary**:
- Created `src/direction.rs` with `DirectionDef` and `DirectionRegistry`
- `DirectionRegistry` stores canonical names, abbreviations, opposites, display text
- `normalize()` method handles case-insensitive lookup with abbreviation expansion
- `WorldLoader` parses `(directions ...)` form, syncs to `CommandRegistry`
- Grammar matcher uses registry for direction slot validation
- Input parser uses registry for direction classification
- Default standard directions loaded by `DirectionRegistry::with_standard_directions()`
- Custom directions fully supported: games can define "upstream", "clockwise", etc.

**Files Created/Modified**:
- `src/direction.rs` — DirectionDef, DirectionRegistry (new)
- `src/lib.rs` — Export direction module
- `src/lang/loader.rs` — Parse (directions ...) form, sync to command registry
- `src/grammar/matcher.rs` — Use DirectionRegistry instead of hardcoded array
- `src/grammar/registry.rs` — Hold DirectionRegistry, pass to matcher
- `src/input.rs` — Use DirectionRegistry for direction classification
- `src/core/world.rs` — Updated parse_input_entity signature
- `libs/std/_lib.hvl` — Added standard directions definition

**Example**:
```lisp
(directions
  (north :abbrev "n" :opposite south :display "to the north")
  (south :abbrev "s" :opposite north :display "to the south")
  (east :abbrev "e" :opposite west :display "to the east")
  (west :abbrev "w" :opposite east :display "to the west")
  (up :abbrev "u" :opposite down :display "above")
  (down :abbrev "d" :opposite up :display "below")
  (in :opposite out :display "inside")
  (out :opposite in :display "outside")
  ;; Game-specific custom directions
  (upstream :opposite downstream :display "upstream")
  (clockwise :opposite counterclockwise))
```

**Tests**: 27 new direction-related tests (11 direction module + 4 loader + 12 updated matcher/input tests)
Total project: 643 tests passing

**Success Criteria**: Can add custom directions without Rust changes. ✓

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
Target: ~200 tests across all stages
- Stage 1: 29 tests (syntax matching, routing) ✓
- Stage 2: 48 tests (grammar trie, matcher, predicate, registry) ✓
- Stage 3: 33 tests (hook invocation, veto/handled, context opcodes, effects) ✓
- Stage 4: 31 tests (precondition types, action registry, built-ins, failure messages, DSL parsing) ✓
- Stage 4.5: ~10 tests (action specificity, :when guards) — deferred
- Stage 5: ~95 tests (primitives, descriptions, loading, defun, stdlib, DSL handlers)
  - 5A: 13 tests (core primitives) ✓
  - 5B: 10 tests (article generation, tamper!) ✓
  - 5C: 9 tests (file loading) ✓
  - 5D: 17 tests (defun user-defined functions) ✓
  - 5E: ~15 tests (stdlib integration)
  - 5F: 19 tests (DSL action handlers) ✓
- Stage 6: 27 tests (direction configuration) ✓

Current: 236 tests (Stage 1-5F: 209 + Stage 6: 27)
Total project: 643 tests passing

---

## Phase 10: Value System Cleanup
**Goal**: Distinguish `Nil` from `Bool(false)` and clean up value semantics.
**Status**: Complete

**Rationale**: Previously, `Value::Bool(false)` served double duty as both boolean false and "no value" (nil). This conflation caused confusion. Separating these concepts enables cleaner semantics and better error messages.

### Deliverables
- [x] Add `Value::Nil` variant to represent "no value" / "nothing" / "unset"
- [x] Update VM to use `Value::Nil` instead of `Bool(false)` for nil constant
- [x] Update hook context opcodes to return `Nil` for missing optional values
- [x] Define truthiness: `Nil` and `Bool(false)` are falsy, everything else truthy
- [x] `Value::is_truthy()` method for consistent truthiness checks
- [x] `Value::is_nil()` predicate method
- [x] Update DSL parser to recognize `nil` keyword (`Atom::Nil`)
- [x] Audit and fix all places using `Bool(false)` as "nothing"
- [x] Update tests to distinguish nil from false cases

### Files Modified
- `src/core/value.rs` — Added `Nil` variant, `is_nil()`, `is_truthy()`, updated Display/Eq/Ord/Hash
- `src/vm/exec.rs` — Updated NIL constant to `Value::Nil`, use `Value::is_truthy()`
- `src/compiler.rs` — Handle `Atom::Nil` → `Value::Nil`
- `src/lang/ast.rs` — Added `Atom::Nil` variant
- `src/lang/parser.rs` — Recognize `nil` symbol as `Atom::Nil`
- `src/lang/loader.rs` — Handle `Atom::Nil` in `expr_to_value`
- `src/hooks.rs` — Updated `value_to_sexpr` for `Value::Nil`
- `src/template.rs` — Empty choice returns `Value::Nil`

### Semantic Rules
```
Truthiness:
  - Nil         → falsy
  - Bool(false) → falsy
  - Bool(true)  → truthy
  - Int(0)      → truthy (NOT falsy like in C!)
  - Everything else → truthy

Equality:
  - Nil == Nil  → true
  - Nil == Bool(false) → false  (they are distinct!)
  - Nil == anything_else → false
```

### Tests Added
- `test_nil` — Nil equality, display, type_name, is_nil
- `test_is_truthy` — Verifies truthiness for all value types
- `test_value_ordering` — Updated to include Nil (sorts first)

---

## Phase 11: Sensory Propagation
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

## Phase 12: Advanced Content
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

| Risk                              | Likelihood | Impact | Mitigation                                      |
| --------------------------------- | ---------- | ------ | ----------------------------------------------- |
| Performance cliff at scale        | Medium     | High   | Phase 6 benchmarks, early profiling             |
| Query explosion (deep traversals) | Medium     | Medium | Depth limits, materialized views                |
| Cache invalidation storms         | Low        | High   | Stratified derivation, epoch-based invalidation |
| DSL expressiveness gaps           | Medium     | Medium | Rust stdlib escape hatch, iterate on syntax     |
| Scope creep (cosmos before house) | High       | High   | Strict phase ordering, playable demo first      |

---

## Appendix: Definition of Done

For any phase to be marked Complete:

- [ ] All deliverables checked off
- [ ] Tests written and passing
- [ ] No clippy warnings
- [ ] Benchmarks added (if applicable)
- [ ] IMPLEMENTATION_PLAN.md updated with file list and test counts
