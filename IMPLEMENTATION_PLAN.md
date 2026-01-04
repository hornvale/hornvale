# Hornvale Unification Plan: From Thousand Fingers to One Fist

## Executive Summary

Hornvale has grown into a rich collection of powerful subsystems, but they exist as semi-independent fiefdoms. This plan unifies them into a coherent whole through four key insights:

1. **Everything is an entity** - Rules, commands, inputs, traces - not just game objects
2. **Everything is an expression** - Patterns, effects, predicates compile to VM bytecode
3. **Everything obeys layer discipline** - Schema, Meta, World, Execution layers with clear mutation rules
4. **The kernel stays in Rust** - VM, transactions, scheduler are "hardware"; the ECS is the "image"

The result: a Lisp-machine-over-ECS where any behavior can be expressed declaratively, composed arbitrarily, inspected at runtime, and traced through execution.

---

## Part 1: The Layered Architecture

### The Core Insight

The ECS is not just for game objects. It's a **universal substrate** for:
- Game content (rooms, creatures, items)
- Game logic (rules, patterns, effects)
- Execution artifacts (inputs, commands, traces)
- Schemas and types

But without discipline, "everything is an entity" becomes chaos. The solution: **stratified layers** with clear mutation rules.

### The Four Layers

```
+-----------------------------------------------------------------------------+
|                              RUST KERNEL                                    |
|         (not entities - this is the "hardware")                             |
|  +----------------+  +----------------+  +----------------+                 |
|  | VM Interpreter |  | Transaction    |  | Phase          |                 |
|  | (executes      |  | Engine         |  | Scheduler      |                 |
|  |  bytecode)     |  | (ACID)         |  | (tick loop)    |                 |
|  +----------------+  +----------------+  +----------------+                 |
+---------------------------------+---------------------------------------+---+
                                  | reads/writes
                                  v
+-----------------------------------------------------------------------------+
|                         ECS "IMAGE"                                         |
|                                                                             |
|  +-----------------------------------------------------------------------+  |
|  | SCHEMA LAYER (frozen after boot)                                      |  |
|  |   ComponentSchema, RelationSchema, DirectionSchema, TypeSchema        |  |
|  |   - Defines what components/relations exist                           |  |
|  |   - Cannot change during runtime                                      |  |
|  +-----------------------------------------------------------------------+  |
|                                                                             |
|  +-----------------------------------------------------------------------+  |
|  | META LAYER (frozen during tick, mutable between ticks)                |  |
|  |   Rule, Pattern, Effect, CodeChunk, Grammar, Command, TypePredicate   |  |
|  |   Epoch (for global state versioning)                                 |  |
|  |   - Defines game logic and behavior                                   |  |
|  |   - Rules can create/modify rules, but changes apply next tick        |  |
|  +-----------------------------------------------------------------------+  |
|                                                                             |
|  +-----------------------------------------------------------------------+  |
|  | WORLD LAYER (mutable during tick, transactional)                      |  |
|  |   Room, Exit, Creature, Item, Weather, Zodiac, Player, Container      |  |
|  |   DerivedCache (cached computed values)                               |  |
|  |   - The actual game state                                             |  |
|  |   - Mutations within transaction boundaries                           |  |
|  +-----------------------------------------------------------------------+  |
|                                                                             |
|  +-----------------------------------------------------------------------+  |
|  | EXECUTION LAYER (ephemeral, within-tick only)                         |  |
|  |   Input, ParseResult, Binding, ActionAttempt, MutationBatch, Trace    |  |
|  |   - Created during command processing                                 |  |
|  |   - Destroyed at end of tick (or archived for debugging)              |  |
|  +-----------------------------------------------------------------------+  |
|                                                                             |
+-----------------------------------------------------------------------------+
```

### Layer Mutation Rules

| Layer     | During Tick             | Between Ticks | By What           |
| --------- | ----------------------- | ------------- | ----------------- |
| Schema    | Frozen                  | Frozen        | Boot only         |
| Meta      | Frozen                  | Mutable       | REPL, mod loading |
| World     | Mutable (transactional) | Mutable       | Rules, actions    |
| Execution | Created/destroyed       | Cleared       | Pipeline          |

### Why This Matters

**Without layers**: A rule could modify itself mid-evaluation, or delete the rule that's about to fire, or create infinite loops of rule-creates-rule.

**With layers**: Meta-layer rules operate on a **snapshot** during tick evaluation. Changes queue for next tick. Determinism preserved.

---

## Part 2: The Execution Pipeline as Entities

### The Input-to-Output Chain

When the player types "take the brass lamp", the system creates a chain of entities:

```lisp
;; Phase: Ingress
;; Player input creates Input entity

(entity input-42
  (Layer :execution)
  (InputText "take the brass lamp")
  (SourceActor player)
  (SourceRoom living-room)
  (Tick 1000)
  (Phase :ingress))

;; Phase: Parse
;; Grammar system creates ParseResult

(entity parse-42
  (Layer :execution)
  (Intent :take)
  (RawSlots {:obj "the brass lamp"})
  (MatchedForm "(obj:portable)")
  (MatchedGrammar grammar-take)
  (Phase :parse))
(relate DerivedFrom parse-42 input-42)

;; Phase: Resolve
;; Entity resolver creates Command with bindings

(entity command-42
  (Layer :execution)
  (Verb :take)
  (Actor player)
  (DirectObject brass-lamp)
  (Room living-room)
  (Phase :resolve))
(relate DerivedFrom command-42 parse-42)

;; Phase: Validate
;; Preconditions create ActionAttempt

(entity attempt-42
  (Layer :execution)
  (Action :take)
  (Status :validated)  ;; or :failed
  (FailureReason nil)  ;; or "You can't reach the lamp."
  (Phase :validate))
(relate DerivedFrom attempt-42 command-42)

;; Phase: Execute
;; Effects applied, output generated

(entity result-42
  (Layer :execution)
  (Output "Taken.")
  (Mutations [...])
  (Phase :execute))
(relate DerivedFrom result-42 attempt-42)

;; Phase: Egress
;; Cleanup (or archive if tracing enabled)
```

### The Six Phases

| Phase        | Input                  | Output               | Rules That Fire       |
| ------------ | ---------------------- | -------------------- | --------------------- |
| **Ingress**  | Raw text               | `Input` entity       | None (pure creation)  |
| **Parse**    | `Input`                | `ParseResult`        | Grammar rules         |
| **Resolve**  | `ParseResult`          | `Command`            | Resolution rules      |
| **Validate** | `Command`              | `ActionAttempt`      | Precondition rules    |
| **Execute**  | `ActionAttempt`        | `Result` + mutations | Before/On/After rules |
| **Egress**   | All execution entities | Output text          | Cleanup rules         |

### Benefits of Entity Pipeline

1. **Traceability**: "Why did that fail?" → query the chain
2. **Debugging**: Inspect any stage in REPL
3. **Rollback**: Transaction abort removes execution entities
4. **NPC AI**: NPCs can inspect pending commands
5. **Replay**: Save/restore Input entities

---

## Part 3: Dependency Tracking with Epochs

### The Problem

Derived values depend on world state. If "current zodiac" affects 10k creatures, we don't want 10k dependency edges.

### The Solution: Epoch Counters

```lisp
;; Global epoch entities (Meta layer)
(entity epoch-zodiac
  (Layer :meta)
  (EpochType :zodiac)
  (Counter 42))

(entity epoch-weather
  (Layer :meta)
  (EpochType :weather)
  (Counter 17))

(entity epoch-tick
  (Layer :meta)
  (EpochType :tick)
  (Counter 1000))
```

### Cache Entries Store Epoch Snapshots

```lisp
;; Derived value cache (World layer)
(entity cache-fire-resist-goblin-3
  (Layer :world)
  (CacheFor goblin-3)
  (Property :FireResistance)
  (Value 0.8)
  (ComputedAtEpochs {:zodiac 42 :tick 1000})
  (DependsOnComponents [(goblin-3 :Ancestry) (goblin-3 :Biome)]))
```

### Validation Logic

```rust
fn is_cache_valid(cache: &CacheEntry, world: &World) -> bool {
    // Check global epochs
    for (epoch_type, cached_counter) in &cache.computed_at_epochs {
        let current = world.get_epoch_counter(epoch_type);
        if current != cached_counter {
            return false;
        }
    }
    // Check component versions (optional, finer-grained)
    for (entity, component) in &cache.depends_on_components {
        if world.component_modified_since(entity, component, cache.computed_at_tick) {
            return false;
        }
    }
    true
}
```

### Invalidation Strategy

- **Global state changes** (zodiac, weather): Bump epoch counter
- **Entity changes**: Component modification timestamps
- **Validation**: Lazy on access, not eager on change
- **No edge explosion**: Dependencies stored as compact data, not N relations

---

## Part 4: Tracing as First-Class Data

### Trace Entities

```lisp
(entity trace-span-123
  (Layer :execution)
  (TraceType :rule-evaluation)
  (RuleName locked-door-veto)
  (StartTick 1000)
  (EndTick 1000)
  (Status :completed)
  (Result :veto)
  (PatternBindings {:door exit-north})
  (SourceSExpr "(rule locked-door-veto ...)")
  (CompiledBytecode [...])
  (Disassembly "LOAD_CONST r0 k0\nGET_COMPONENT r1 r0 k1\n..."))

(relate TraceParent trace-span-123 trace-span-100)  ;; Tree structure
(relate TraceReads trace-span-123 exit-north)       ;; What was read
(relate TraceWrites trace-span-123 output-buffer)   ;; What was written
```

### REPL Commands for Debugging

```lisp
(trace :last-command)
;; Returns the full trace tree for the most recent command

(why-failed attempt-42)
;; Shows which precondition failed and why

(explain-derivation goblin-3 :FireResistance)
;; Shows all rules that contributed and their values

(ancestors command-42 :DerivedFrom 10)
;; Shows: command-42 <- parse-42 <- input-42
```

### Trace Retention Policy

```lisp
;; Configurable per-session
(set-trace-retention
  :execution-entities :last-10    ;; Keep last 10 command chains
  :rule-traces :last-100          ;; Keep last 100 rule evaluations
  :derivation-traces :on-demand)  ;; Only when explicitly requested
```

---

## Part 5: Implementation Stages

> **Note**: Stages 0, 1, and 2 are designed to be implemented together as a "Foundation Sprint".
> This gives you an immediately usable loop: type command → see entity chain → see which rule
> fired → see bytecode/disassembly → see world mutation, all while preserving determinism.

### Stage 0: Layer Infrastructure
**Goal**: Establish the four-layer architecture with proper invariants
**Status**: Complete

**Changes**:
1. Add `Layer` as **immutable entity metadata** (not a component):
   ```rust
   #[derive(Debug, Clone, Copy, PartialEq, Eq)]
   pub enum Layer {
       Schema,
       Meta,
       World,
       Execution,
   }

   impl World {
       // Layer stored in separate map, set at creation, immutable thereafter
       entity_layers: OrdMap<EntityId, Layer>,

       pub fn create_entity(&mut self, layer: Layer) -> EntityId {
           let id = self.allocator.create();
           self.entity_layers.insert(id, layer);
           id
       }

       pub fn get_layer(&self, entity: EntityId) -> Layer {
           self.entity_layers.get(&entity).copied().unwrap_or(Layer::World)
       }
       // No set_layer() - layer is immutable after creation
   }
   ```

2. Add layer validation with **Meta snapshot per tick**:
   ```rust
   impl World {
       meta_snapshot: Option<MetaSnapshot>,  // Frozen view during tick

       pub fn begin_tick(&mut self) {
           self.phase = Phase::Tick;
           self.meta_snapshot = Some(self.capture_meta_snapshot());
       }

       pub fn end_tick(&mut self) {
           self.phase = Phase::Idle;
           self.meta_snapshot = None;
           self.apply_queued_meta_changes();
           self.cleanup_execution_entities();
       }

       fn validate_mutation(&self, layer: Layer) -> Result<(), LayerError> {
           match (layer, self.phase) {
               (Layer::Schema, _) => Err(LayerError::SchemaFrozen),
               (Layer::Meta, Phase::Tick) => Err(LayerError::MetaFrozenDuringTick),
               _ => Ok(())
           }
       }
   }
   ```

3. Add **ExecutionArena** for efficient entity pooling:
   ```rust
   pub struct ExecutionArena {
       entities: Vec<EntityId>,
       next_slot: usize,
   }

   impl ExecutionArena {
       pub fn allocate(&mut self, world: &mut World) -> EntityId {
           // Reuse or create, always Layer::Execution
       }

       pub fn clear(&mut self, world: &mut World) {
           // Delete all execution entities unless archived
           for &entity in &self.entities {
               if !world.has_component(entity, "Archive") {
                   world.destroy_entity(entity);
               }
           }
           self.entities.clear();
           self.next_slot = 0;
       }
   }
   ```

4. Add Epoch infrastructure:
   ```rust
   impl World {
       epochs: OrdMap<Symbol, u64>,

       pub fn bump_epoch(&mut self, epoch_type: Symbol) {
           let counter = self.epochs.get(&epoch_type).copied().unwrap_or(0);
           self.epochs.insert(epoch_type, counter + 1);
       }

       pub fn get_epoch(&self, epoch_type: Symbol) -> u64 {
           self.epochs.get(&epoch_type).copied().unwrap_or(0)
       }
   }
   ```

5. Add Phase tracking:
   ```rust
   pub enum Phase {
       Boot,      // Schema mutations allowed
       Idle,      // Between ticks, meta mutations allowed
       Tick,      // During tick, only world/execution mutations
   }
   ```

**Success Criteria**:
- [x] Layer is immutable metadata (no set_layer)
- [x] Schema mutations rejected after boot
- [x] Meta mutations rejected during tick (queued instead)
- [x] Meta snapshot captured at tick start (via validation)
- [x] Execution entities cleaned at tick end
- [x] Epoch counters track global state versions

**Tests**:
- [x] Cannot mutate Schema entity after boot
- [x] Cannot mutate Meta entity during tick (returns error or queues)
- [x] Meta changes from tick N not visible until tick N+1 (Hard Invariant A)
- [x] Execution entities deleted at tick end unless archived (Hard Invariant B)
- [x] Epoch counter increments correctly

**Implementation Notes**:
- Created `src/core/layer.rs` with Layer enum and LayerError
- Created `src/core/phase.rs` with Phase enum
- Created `src/core/epoch.rs` with EpochSnapshot for cache invalidation
- Updated `src/core/world.rs` with layer infrastructure, phase lifecycle, epoch tracking

---

### Stage 1: Execution Layer Entities + Minimal Tracing
**Goal**: Input/Command/ActionAttempt as entities, with basic trace skeleton
**Status**: Complete

**Changes**:
1. Define execution entity schemas:
   ```lisp
   (schema Input
     :layer :execution
     :components [InputText SourceActor SourceRoom Tick Phase])

   (schema ParseResult
     :layer :execution
     :components [Intent RawSlots MatchedForm MatchedGrammar])

   (schema Command
     :layer :execution
     :components [Verb Actor DirectObject IndirectObject Room])

   (schema ActionAttempt
     :layer :execution
     :components [Action Status FailureReason])
   ```

2. Refactor input processing to create entities:
   ```rust
   fn process_input(&mut self, text: &str, actor: EntityId) {
       // Create Input entity
       let input = self.arena.allocate(&mut self.world);
       self.world.set_component(input, "InputText", text);
       self.world.set_component(input, "SourceActor", actor);
       self.world.set_component(input, "Tick", self.tick);

       // Run parse phase...
   }
   ```

3. Add `DerivedFrom` relation for execution chain tracing

4. **Minimal TraceSpan skeleton** (behind a flag):
   ```rust
   pub struct TraceSpan {
       pub entity: EntityId,  // The span is itself an execution entity
   }

   impl World {
       tracing_enabled: bool,

       pub fn begin_trace_span(&mut self, span_type: &str, name: &str) -> Option<TraceSpan> {
           if !self.tracing_enabled { return None; }
           let entity = self.arena.allocate(self);
           self.set_component(entity, "TraceType", span_type);
           self.set_component(entity, "TraceName", name);
           self.set_component(entity, "StartTick", self.tick);
           self.set_component(entity, "Status", "running");
           Some(TraceSpan { entity })
       }

       pub fn end_trace_span(&mut self, span: Option<TraceSpan>, status: &str, result: Value) {
           if let Some(span) = span {
               self.set_component(span.entity, "EndTick", self.tick);
               self.set_component(span.entity, "Status", status);
               self.set_component(span.entity, "Result", result);
           }
       }
   }
   ```

   Initial trace types (minimal):
   - `phase-started` / `phase-ended`
   - `rule-fired` / `rule-skipped`
   - `effect-executed`

**Success Criteria**:
- [x] `Input` entity created for each command
- [x] `ParseResult` linked to Input
- [x] `Command` linked to ParseResult
- [x] Full chain queryable via relations (query_execution_chain)
- [x] TraceSpan entities created when tracing enabled
- [x] Traces attached to execution chain via `TraceParent` relation

**Tests**:
- [x] Input entity created with correct components
- [x] Parse creates ParseResult linked to Input
- [x] Command resolution creates Command linked to ParseResult
- [x] Execution chain queryable via DerivedFrom relation
- [x] With tracing enabled, span entities created for phases
- [x] With tracing disabled, no span entities created (zero overhead)

**Implementation Notes**:
- Created `src/execution.rs` with execution entity infrastructure
- Defined DerivedFrom, TraceParent, TraceReads, TraceWrites relations
- Implemented create_input, create_parse_result, create_command_entity, create_action_attempt, create_result
- Implemented query_execution_chain for traversing the execution chain
- Implemented TraceSpan with begin_trace_span/end_trace_span behind tracing_enabled flag

---

### Stage 2: Effects as Bytecode
**Goal**: Rules can execute any VM opcode
**Status**: Complete

**Current State**: `Effect::EmitMessage` is the only variant
**Target State**: Effects are CodeChunk entities in Meta layer

**Changes**:
1. CodeChunk as Meta entity:
   ```lisp
   (entity effect-goat-baas
     (Layer :meta)
     (CodeType :effect)
     (SourceSExpr "(say \"The goat says: Baa!\")")
     (Bytecode [...])
     (Disassembly "LOAD_CONST r0 k0\nSAY r0\nRETURN_NIL"))
   ```

2. Rules reference CodeChunk entities:
   ```lisp
   (entity rule-goat-baas
     (Layer :meta)
     (RuleName "goat-baas")
     (Trigger (every 3600))
     (Pattern pattern-is-goat)     ;; Reference to pattern entity
     (Effect effect-goat-baas))    ;; Reference to effect entity
   ```

3. Effect execution via VM:
   ```rust
   fn execute_effect(&self, effect_entity: EntityId, context: &ActionContext) {
       let bytecode = self.get_component(effect_entity, "Bytecode");
       let chunk = Chunk::from_bytecode(bytecode);
       let mut vm = VM::new(&chunk, self, stdlib)
           .with_action_context(context);
       vm.run()?;
       self.apply_pending_mutations(vm.take_mutations());
   }
   ```

**Success Criteria**:
- [x] Effects can be VM scripts (Effect::Script variant)
- [x] Effects compile Value S-expressions to bytecode
- [x] `(say ...)`, `(set! ...)`, `(relate! ...)` work in script effects
- [x] All existing tests pass

**Tests**:
- [x] Script effect with (say ...) outputs message
- [x] Script effect with multiple expressions executes all
- [x] Effect::execute_with_context returns mutations
- [x] Legacy EmitMessage effect still works (backward compatible)

**Implementation Notes**:
- Updated `src/rules/effect.rs` with Effect::Script variant
- Added Effect::NoOp variant for pattern-only rules
- Added execute_with_context for full mutation support
- Added EffectResult and EffectError types
- Script effects compile via Compiler and execute via VM with action context
- Mutations collected via take_pending_* methods on VM

---

### Stage 3: Predicate Patterns (VM for Filtering, Rust for Enumeration)
**Goal**: Compiled predicates for filtering; Rust query plans for enumeration
**Status**: Complete

**Key Insight**: There are two fundamentally different operations:
- **Filtering** (given entity, is predicate true?) → VM is fine
- **Enumeration** (find all entities where X) → VM loop is death, use Rust

**The Split**:
- `PredicatePattern` (VM): Boolean checks when you already have bindings
- `MatchPlan` (Rust): Query planning, joins, traversal, "find entities where..."

**When to use VM predicates**:
- Precondition checks (entity already resolved)
- Hook guards (direct-object already known)
- Type validation (candidate entity already identified)
- Derivation filters (evaluating one entity at a time)

**When to use Rust MatchPlan**:
- "Find all creatures with HP > 0" → Rust query + optional VM filter
- "Find entities in room" → Rust relation query
- Periodic rule matching → Rust index lookup, then VM filter

**Changes**:
1. PredicatePattern as Meta entity (for filtering):
   ```lisp
   (entity predicate-is-goat
     (Layer :meta)
     (PredicateType :filter)
     (SourceSExpr "(= (get ?e :Name) \"goat\")")
     (Bytecode [...])
     (BoundVariables [?e]))
   ```

2. Predicate compilation (unchanged from before):
   ```rust
   fn compile_predicate(sexpr: &SExpr) -> EntityId {
       let bytecode = Compiler::compile_to_bool(sexpr);
       let entity = world.create_entity(Layer::Meta);
       world.set_component(entity, "Bytecode", bytecode);
       world.set_component(entity, "SourceSExpr", sexpr.to_string());
       entity
   }
   ```

3. Predicate evaluation via VM (for filtering known entities):
   ```rust
   fn matches_predicate(&self, predicate: EntityId, target: EntityId) -> bool {
       let bytecode = self.get_component(predicate, "Bytecode");
       let chunk = Chunk::from_bytecode(bytecode);
       let mut vm = VM::new(&chunk, self, stdlib)
           .with_binding("?e", target);
       let result = vm.run()?;
       result.is_truthy()
   }
   ```

4. **Keep Rust MatchPlan for enumeration** (existing Pattern enum stays):
   ```rust
   // Existing Pattern enum remains for enumeration/binding
   pub enum MatchPlan {
       AllEntities,
       WithComponent(ComponentTypeId),
       InRelation(RelationTypeId, EntityId),
       Intersection(Vec<MatchPlan>),
       // ... existing query planning
   }

   // Enumeration uses Rust, then optional VM filter
   fn query_entities(&self, plan: &MatchPlan, filter: Option<EntityId>) -> Vec<EntityId> {
       let candidates = self.execute_match_plan(plan);  // Rust, fast
       if let Some(predicate) = filter {
           candidates.into_iter()
               .filter(|&e| self.matches_predicate(predicate, e))
               .collect()
       } else {
           candidates
       }
   }
   ```

**Success Criteria**:
- [x] PredicatePattern entities for filtering
- [x] VM evaluation for known-entity checks
- [x] Rust MatchPlan for enumeration (no VM loops over all entities)
- [x] Combined: Rust enumeration + VM filter (PatternFilter trait)

**Tests**:
- [x] Predicate compiles and evaluates against single entity
- [x] MatchPlan enumerates without VM overhead (existing Pattern enum)
- [x] Combined query: MatchPlan + predicate filter (PatternFilter trait)
- [x] Performance: enumeration stays O(index), not O(all entities × VM)

**Implementation Notes**:
- Created `src/rules/predicate.rs` with PredicatePattern struct
- PredicatePattern stores SExpr and recompiles with bindings at evaluation time
- Uses `Compiler::compile_with_bindings` to bake entity binding into bytecode
- Added builder functions for common patterns: `has_component`, `component_equals`, `and`, `or`, `not`, `is_portable`, `is_creature`, `is_container`
- PatternFilter trait allows filtering Vec<EntityId> with predicates
- Existing Pattern enum in `src/rules/pattern.rs` preserved for Rust-based enumeration
- Updated `src/rules.rs` to export PredicatePattern, PredicateError, PatternFilter

---

### Stage 4: Unify Type Predicates
**Goal**: Grammar type predicates use pattern entities
**Status**: Complete

**Changes**:
1. TypePredicate as Meta entity:
   ```lisp
   (entity type-portable
     (Layer :meta)
     (TypeName :portable)
     (Pattern pattern-portable))  ;; Reference to pattern entity

   (entity pattern-portable
     (Layer :meta)
     (SourceSExpr "(and (has? entity :Portable) (not (has? entity :Fixed)))")
     (Bytecode [...]))
   ```

2. Slot validation uses pattern:
   ```rust
   fn validate_slot(&self, type_entity: EntityId, candidate: EntityId) -> bool {
       let pattern = self.get_relation_forward("Pattern", type_entity)?;
       self.matches(pattern, candidate)
   }
   ```

**Success Criteria**:
- [x] Type predicates as entities referencing patterns
- [x] Custom types definable in DSL (via TypePredicate with SExpr)
- [x] Grammar validation uses pattern matching (VM when compiled, interpreter fallback)

**Tests**:
- [x] Built-in `:portable` type works
- [x] User-defined types work (test_type_check_via_vm, test_complex_type_check_via_vm)
- [x] Invalid slot candidates rejected

**Implementation Notes**:
- Updated `PredicatePattern` to support multiple bindings via `evaluate_with_bindings`
- Added `from_sexpr` method for type predicates that use `entity` and `actor` bindings
- Updated `TypePredicate` to automatically compile to `PredicatePattern` on creation
- Added `compiled()`, `is_compiled()`, `try_compile()` methods to TypePredicate
- Updated `PredicateEvaluator::check_type` to use VM evaluation when compiled predicate available
- Kept interpreted evaluation as fallback for complex predicates that may not compile
- Grammar slot validation now uses VM-based type checking for better performance

---

### Stage 5: Hooks as Rules
**Goal**: Before/On/After hooks become rules in Meta layer
**Status**: Not Started

**Changes**:
1. Hook triggers as rule trigger types:
   ```lisp
   (entity rule-holy-book-burn
     (Layer :meta)
     (RuleName "holy-book:On:burn")
     (Trigger (on :burn))
     (Pattern pattern-direct-object-is-holy-book)
     (Effect effect-lightning-veto)
     (Priority 100))
   ```

2. Inline hooks desugar to rules:
   ```rust
   // (entity holy-book (On:burn ...))
   // Desugars to:
   fn desugar_inline_hook(entity: EntityId, phase: &str, action: &str, body: &SExpr) {
       let rule_name = format!("{}:{}:{}", entity_name, phase, action);
       let pattern = compile_pattern(&format!("(= (direct-object) {})", entity));
       let effect = compile_effect(body);
       create_rule(rule_name, Trigger::Hook(phase, action), pattern, effect);
   }
   ```

3. Hook ordering via priority + entity order

**Success Criteria**:
- [ ] Hooks stored as rule entities
- [ ] Inline syntax still works (desugars)
- [ ] Hook ordering preserved
- [ ] Rules queryable: `(rules-for-trigger (on :burn))`

**Tests**:
- Before hook vetoes action
- On hook handles action
- Multiple hooks fire in order
- Inline hook desugars correctly

---

### Stage 6: Derivations with Caching
**Goal**: Derived properties with epoch-based cache invalidation
**Status**: Not Started

**Changes**:
1. Derivation rules in Meta layer:
   ```lisp
   (entity rule-fire-resist-biome
     (Layer :meta)
     (RuleName "fire-resistance:biome")
     (Trigger (derive :FireResistance))
     (Pattern pattern-in-volcanic-biome)
     (Value 0.3)
     (ComposeMode :add))
   ```

2. Cache entries in World layer:
   ```lisp
   (entity cache-12345
     (Layer :world)
     (CacheFor goblin-3)
     (Property :FireResistance)
     (Value 0.8)
     (ComputedAtEpochs {:zodiac 42 :tick 1000})
     (DependsOn [(goblin-3 :Ancestry) (goblin-3 :InRoom)]))
   ```

3. Derivation evaluation:
   ```rust
   fn derive(&self, entity: EntityId, property: Symbol) -> Value {
       // Check cache
       if let Some(cached) = self.get_cached_derivation(entity, property) {
           if self.is_cache_valid(&cached) {
               return cached.value;
           }
       }

       // Compute fresh
       let rules = self.rules_for_trigger(Trigger::Derive(property));
       let values: Vec<_> = rules
           .filter(|r| self.matches(r.pattern, entity))
           .map(|r| (r.value, r.compose_mode))
           .collect();
       let result = compose_values(values);

       // Update cache
       self.set_cached_derivation(entity, property, result, current_epochs);
       result
   }
   ```

**Success Criteria**:
- [ ] Derivation rules as entities
- [ ] Cache entries as entities
- [ ] Epoch-based invalidation works
- [ ] `(derive creature :FireResistance)` returns composed value

**Tests**:
- Add composition returns sum
- Cache hit on repeated query
- Cache miss after epoch bump
- Derivation rules inspectable

---

### Stage 7: Unified Rule Registry (Mandatory Indexing)
**Goal**: All rules in Meta layer with **mandatory** kernel-owned indexes
**Status**: Not Started

**Critical**: The index is **not optional**. Without it, rule lookup becomes O(all rules) which kills performance at scale. The kernel owns index correctness; it rebuilds between ticks.

**Changes**:
1. **Kernel-owned rule index** (mandatory, not optional):
   ```rust
   pub struct RuleIndex {
       by_periodic: Vec<EntityId>,
       by_action: OrdMap<Symbol, Vec<EntityId>>,
       by_before: OrdMap<Symbol, Vec<EntityId>>,
       by_on: OrdMap<Symbol, Vec<EntityId>>,
       by_after: OrdMap<Symbol, Vec<EntityId>>,
       by_derive: OrdMap<Symbol, Vec<EntityId>>,
       by_precondition: OrdMap<Symbol, EntityId>,
       dirty: bool,
   }

   impl World {
       rule_index: RuleIndex,

       pub fn rules_for_trigger(&self, trigger: TriggerType) -> &[EntityId] {
           // O(1) lookup, not O(all entities)
           match trigger {
               TriggerType::Periodic => &self.rule_index.by_periodic,
               TriggerType::Action(name) => self.rule_index.by_action.get(&name).map(|v| &v[..]).unwrap_or(&[]),
               TriggerType::Before(name) => self.rule_index.by_before.get(&name).map(|v| &v[..]).unwrap_or(&[]),
               // ...
           }
       }
   }
   ```

2. **Index rebuild between ticks** (kernel responsibility):
   ```rust
   impl World {
       pub fn end_tick(&mut self) {
           // ... other cleanup ...
           if self.rule_index.dirty {
               self.rebuild_rule_index();
           }
       }

       fn rebuild_rule_index(&mut self) {
           self.rule_index = RuleIndex::default();
           for entity in self.entities_with_layer(Layer::Meta) {
               if let Some(trigger) = self.get_component(entity, "Trigger") {
                   self.rule_index.insert(trigger, entity);
               }
           }
           self.rule_index.dirty = false;
       }

       // Called when meta entities change (between ticks)
       fn mark_rule_index_dirty(&mut self) {
           self.rule_index.dirty = true;
       }
   }
   ```

3. **Priority ordering within index buckets**:
   ```rust
   impl RuleIndex {
       fn insert(&mut self, trigger: TriggerType, entity: EntityId) {
           let bucket = self.bucket_for_mut(trigger);
           bucket.push(entity);
           // Sort by priority (done at rebuild time, not insert time)
       }

       fn sort_by_priority(&mut self, world: &World) {
           for bucket in self.all_buckets_mut() {
               bucket.sort_by_key(|&e| {
                   world.get_component(e, "Priority")
                       .and_then(|v| v.as_int())
                       .unwrap_or(0)
               });
           }
       }
   }
   ```

**Success Criteria**:
- [ ] Rule lookup is O(1) via index, not O(all entities)
- [ ] Index is mandatory (no "optional optimization" escape hatch)
- [ ] Index rebuilt automatically when meta layer changes
- [ ] Priority ordering respected within trigger buckets
- [ ] No separate ActionRegistry, HookRegistry, etc.

**Tests**:
- `rules_for_trigger(Periodic)` returns all periodic rules in O(1)
- `rules_for_trigger(Action("take"))` returns take handlers
- Adding rule via REPL marks index dirty, rebuild happens at tick boundary
- 1000 rules: lookup still O(1)
- Priority ordering: higher priority rules first in bucket

---

### Stage 8: Unified Query API
**Goal**: DSL and Rust share one query interface
**Status**: Not Started

**Changes**:
1. Query forms in DSL:
   ```lisp
   (query-entities :where (and (has? ?e :Creature) (> (get ?e :HP) 0)))
   (query-descendants room :Contains :depth 3)
   (query-ancestors key :Contains :depth 10)
   (query-rules :trigger (derive :FireResistance))
   ```

2. Query as compiled expression:
   ```rust
   pub enum QueryOp {
       Entities { pattern: EntityId },
       Descendants { start: Reg, relation: Symbol, depth: Reg },
       Ancestors { start: Reg, relation: Symbol, depth: Reg },
       Rules { trigger: TriggerType },
   }
   ```

**Success Criteria**:
- [ ] `query-entities` returns matching entities
- [ ] Graph traversals work from DSL
- [ ] Rule queries work from DSL

**Tests**:
- Query creatures with HP > 0
- Query descendants of room
- Query rules for derivation

---

### Stage 9: Preconditions as Rules
**Goal**: Preconditions are rules returning Pass/Fail
**Status**: Not Started

**Changes**:
1. Precondition as rule with special effect type:
   ```lisp
   (entity rule-reachable
     (Layer :meta)
     (RuleName "reachable?")
     (Trigger (precondition))
     (Params [actor target])
     (Check pattern-in-scope)
     (FailureTemplate "You can't reach ~(the target)."))
   ```

2. Precondition evaluation:
   ```rust
   fn check_precondition(&self, name: Symbol, args: &[EntityId]) -> PreconditionResult {
       let rule = self.get_precondition_rule(name);
       let bindings = zip(rule.params, args);
       if self.matches_with_bindings(rule.check, bindings) {
           PreconditionResult::Passed
       } else {
           let message = self.interpolate_template(rule.failure_template, bindings);
           PreconditionResult::Failed(message)
       }
   }
   ```

**Success Criteria**:
- [ ] Preconditions as rule entities
- [ ] Template interpolation works
- [ ] Action validation uses precondition rules

**Tests**:
- `reachable?` passes for in-scope
- `reachable?` fails with message for out-of-scope
- Precondition rules inspectable

---

### Stage 10: Full Trace Infrastructure
**Goal**: Complete the tracing skeleton from Stage 1 with read/write tracking and REPL commands
**Status**: Not Started

**Note**: This stage builds on the minimal TraceSpan skeleton from Stage 1. By this point, basic tracing already exists; this stage adds the full debugging experience.

**Changes**:
1. Extended TraceSpan entities (beyond Stage 1 skeleton):
   ```lisp
   (entity trace-span-123
     (Layer :execution)
     (TraceType :rule-evaluation)
     (RuleName locked-door-veto)
     (StartTick 1000)
     (EndTick 1000)
     (Status :completed)
     (Result :veto)
     (PatternBindings {:door exit-north}))

   (relate TraceParent trace-span-123 trace-span-100)
   (relate TraceReads trace-span-123 exit-north)
   ```

2. Trace collection during execution:
   ```rust
   fn execute_rule(&mut self, rule: EntityId, context: &ActionContext) {
       let span = self.begin_trace_span(rule);
       // ... execute ...
       self.end_trace_span(span, result);
   }
   ```

3. REPL commands:
   ```lisp
   (trace :last-command)
   (why-failed attempt-42)
   (explain-derivation goblin-3 :FireResistance)
   ```

**Success Criteria**:
- [ ] Trace spans created during execution
- [ ] Parent/child relationships tracked
- [ ] Read/write dependencies recorded
- [ ] REPL commands work

**Tests**:
- Trace span created for rule evaluation
- Parent span linked correctly
- `trace :last-command` returns span tree

---

## Part 6: Architecture After Unification

### Module Structure

```
src/
  core/
    entity.rs          # EntityId, allocation
    value.rs           # Value type
    component.rs       # Component storage
    relation.rs        # Relation tables
    world.rs           # World + layer enforcement
    layer.rs           # Layer enum, validation
    epoch.rs           # Epoch counters
    transaction.rs     # Nested transactions

  rule/
    rule.rs            # Rule entity components
    trigger.rs         # Trigger types
    pattern.rs         # Pattern compilation + matching
    effect.rs          # Effect compilation + execution
    compose.rs         # Derivation composition
    index.rs           # Rule indexing

  execution/
    input.rs           # Input entity handling
    parse.rs           # ParseResult creation
    command.rs         # Command resolution
    attempt.rs         # ActionAttempt creation
    pipeline.rs        # Phase orchestration

  trace/
    span.rs            # TraceSpan entities
    collect.rs         # Trace collection
    query.rs           # Trace querying

  cache/
    derived.rs         # Derivation cache
    validate.rs        # Cache validation

  vm/                  # (unchanged)
  compiler.rs          # (extended)
  grammar/             # (uses pattern entities)
  lang/                # (parser, loader)
  repl.rs              # (extended with trace commands)
```

### DSL After Unification

```lisp
;;; SCHEMAS (boot-time only)
(schema Room :components [Name Description Exits])
(schema Creature :components [Name HP Location])

;;; RULES (meta layer)
(rule goat-baas
  :trigger (every 3600)
  :pattern (= (get ?e :Name) "goat")
  :effect (say "The goat says: Baa!"))

(rule action:take
  :trigger (action :take)
  :preconditions [reachable? portable? not-held?]
  :effect (do
            (relate! :Contains (actor) (direct-object))
            (say "Taken.")))

(rule fire-resistance:biome
  :trigger (derive :FireResistance)
  :pattern (in-biome? ?e :volcanic)
  :compose :add
  :value 0.3)

;;; PRECONDITIONS (meta layer)
(precondition reachable?
  :params (actor target)
  :check (in-scope? actor target)
  :failure "You can't reach ~(the target).")

;;; TYPES (meta layer)
(type portable
  (and (has? entity :Portable)
       (not (has? entity :Fixed))))

;;; COMMANDS (meta layer)
(command take
  :aliases ("get" "grab" ("pick" "up"))
  :forms (((obj:portable) -> action:take)))

;;; ENTITIES (world layer)
(entity goat
  (Layer :world)
  (Name "goat")
  (HP 10))

(entity magic-sword
  (Layer :world)
  (Name "sword")
  (Portable true)
  (On:burn (do (say "The sword glows!") :handled)))  ;; Desugars to rule

;;; REPL COMMANDS
(trace :last-command)
(why-failed :last)
(rules-for-trigger (derive :FireResistance))
(inspect rule-goat-baas)
```

---

## Part 7: Migration Path

### Phase A: Foundation Sprint (Stages 0 + 1 + 2 together)
**Recommended**: Implement these three stages as a single sprint.

This gives you an immediately usable loop:
- Type command → see entity chain (Input → ParseResult → Command)
- See which rule fired → see bytecode/disassembly
- See world mutation → verify determinism

Deliverables:
- Layer infrastructure with hard invariants
- Execution entities (Input, Command, etc.)
- Minimal tracing skeleton
- Effects as bytecode entities
- **No breaking changes** - existing DSL still works

### Phase B: Pattern & Type Unification (Stages 3-4)
- Predicate patterns (VM for filtering, Rust for enumeration)
- Type predicates unified with predicate patterns
- **Old syntax desugars to new**

### Phase C: Rule Unification (Stages 5-7)
- Hooks as rules
- Derivations with caching
- Unified registry with mandatory indexing
- **Registries consolidated**

### Phase D: Full Introspection (Stages 8-10)
- Query API
- Preconditions as rules
- Full trace infrastructure with REPL commands
- **Full observability**

---

## Part 8: Risk Assessment

### Technical Risks

| Risk                            | Likelihood | Impact | Mitigation                                     |
| ------------------------------- | ---------- | ------ | ---------------------------------------------- |
| Layer enforcement overhead      | Low        | Medium | Compile-time checks where possible             |
| Entity churn in Execution layer | Medium     | Medium | Pool/reuse execution entities                  |
| Cache invalidation bugs         | Medium     | High   | Property-based tests comparing cached vs fresh |
| Meta-layer mutation complexity  | Medium     | Medium | Clear "queue for next tick" semantics          |
| Performance regression          | Medium     | High   | Benchmark each stage                           |

### Process Risks

| Risk                                  | Likelihood | Impact | Mitigation                                 |
| ------------------------------------- | ---------- | ------ | ------------------------------------------ |
| Scope creep into "infinite cathedral" | High       | High   | Strict stage boundaries, YAGNI checks      |
| Lost in abstraction                   | Medium     | High   | Concrete tests for each stage              |
| Over-engineering layers               | Medium     | Medium | Start simple, add validation incrementally |

---

## Part 9: Success Metrics

### Quantitative
- [ ] All game behavior expressible in DSL (no special-case Rust for logic)
- [ ] Test coverage maintained: >80%
- [ ] Performance: <2x regression on benchmarks
- [ ] Trace overhead: <10% when disabled, <50% when enabled

### Qualitative
- [ ] "Why did that fail?" answerable via REPL
- [ ] Rules inspectable as entities
- [ ] New behaviors composable from primitives
- [ ] NPCs can reason about pending commands

### The Payoff: What Becomes Trivial

```lisp
;; A curse that modifies itself based on moon phase
(rule evolving-curse
  :trigger (on-tick)
  :pattern (and (has? ?e :EvolvingCurse)
                (= (moon-phase) :full))
  :effect (do
            ;; Modify the curse's own derivation rule!
            (set-rule-value (get ?e :CurseRule)
                            (* 2 (get-rule-value (get ?e :CurseRule))))
            (say "The curse grows stronger...")))

;; Debug why a command failed
> take the lamp
You can't reach the lamp.

> (why-failed :last)
Precondition 'reachable?' failed:
  actor: player (in living-room)
  target: lamp (in bedroom)
  check: (in-scope? actor target) => false
  Trace: lamp not in (contents living-room)
         lamp not in (descendants player :Contains 3)

;; Inspect a rule's execution history
> (trace-rule locked-door-veto :last 5)
[tick 1000] matched: exit-north, result: :veto
[tick 950] no match (door was unlocked)
[tick 900] matched: exit-south, result: :veto
```

---

## Appendix A: Stage Dependencies

```
+==============================================+
|  FOUNDATION SPRINT (implement together)      |
|                                              |
|  Stage 0: Layer Infrastructure               |
|      |                                       |
|      v                                       |
|  Stage 1: Execution Entities + Trace Skeleton|
|      |                                       |
|      v                                       |
|  Stage 2: Effects as Bytecode                |
+==============================================+
                    |
                    v
Stage 3: Predicate Patterns (VM filter + Rust enum)
                    |
    +---------------+---------------+
    v                               v
Stage 4: Type                  Stage 5: Hooks
Predicates                     as Rules
    |                               |
    +---------------+---------------+
                    v
Stage 6: Derivations with Caching
                    |
                    v
Stage 7: Unified Rule Registry (mandatory indexing)
                    |
    +---------------+---------------+
    v                               v
Stage 8: Query                 Stage 9: Preconditions
API                            as Rules
    |                               |
    +---------------+---------------+
                    v
Stage 10: Full Trace Infrastructure
```

---

## Appendix B: Definition of Done

For any stage to be marked Complete:

- [ ] All success criteria checked off
- [ ] Tests written and passing
- [ ] No clippy warnings
- [ ] Benchmarks added (if applicable)
- [ ] ARCHITECTURE.md updated with design decisions
- [ ] Layer discipline enforced and tested

---

## Appendix C: Hard Invariants (Test From Day 1)

These invariants must be tested from Stage 0. They are the "no time travel" and "no memory leak" properties that make the system tractable.

### Invariant A: Meta Snapshot Determinism

During a tick, rule selection and code chunks must come from a **frozen meta snapshot**.
Modifications to Meta during a tick are queued, not applied.

```rust
#[test]
fn meta_changes_not_visible_during_tick() {
    let mut world = World::new();

    // Create a rule
    let rule = world.create_entity(Layer::Meta);
    world.set_component(rule, "RuleName", "test-rule");
    world.set_component(rule, "Effect", "old-effect");

    // Start tick - meta is now frozen
    world.begin_tick();

    // Attempt to modify rule during tick (should queue, not apply)
    world.queue_meta_change(rule, "Effect", "new-effect");

    // Current tick still sees old value
    assert_eq!(
        world.get_component(rule, "Effect"),
        Some(&Value::from("old-effect"))
    );

    // End tick - queued changes applied
    world.end_tick();

    // Next tick sees new value
    world.begin_tick();
    assert_eq!(
        world.get_component(rule, "Effect"),
        Some(&Value::from("new-effect"))
    );
}
```

### Invariant B: Execution Entities Don't Leak

Execution layer entities are ephemeral. At end of tick, they are deleted unless explicitly archived.

```rust
#[test]
fn execution_entities_cleaned_after_tick() {
    let mut world = World::new();
    world.begin_tick();

    // Create execution entities (simulating command processing)
    let input = world.create_entity(Layer::Execution);
    world.set_component(input, "InputText", "take lamp");

    let command = world.create_entity(Layer::Execution);
    world.set_component(command, "Verb", "take");
    world.add_relation("DerivedFrom", command, input);

    // Entities exist during tick
    assert!(world.entity_exists(input));
    assert!(world.entity_exists(command));

    // End tick - execution entities cleaned up
    world.end_tick();

    // Entities gone
    assert!(!world.entity_exists(input));
    assert!(!world.entity_exists(command));
}

#[test]
fn archived_execution_entities_retained() {
    let mut world = World::new();
    world.begin_tick();

    let input = world.create_entity(Layer::Execution);
    world.set_component(input, "InputText", "take lamp");
    world.set_component(input, "Archive", true);  // Mark for retention

    world.end_tick();

    // Archived entity retained
    assert!(world.entity_exists(input));
}

#[test]
fn execution_retention_respects_limit() {
    let mut world = World::new();
    world.set_trace_retention(TraceRetention::Last(3));

    // Process 5 commands
    for i in 0..5 {
        world.begin_tick();
        let input = world.create_entity(Layer::Execution);
        world.set_component(input, "InputText", format!("command {}", i));
        world.set_component(input, "Archive", true);
        world.end_tick();
    }

    // Only last 3 retained
    let archived = world.entities_with_component("Archive");
    assert_eq!(archived.len(), 3);
}
```

### Invariant C: Layer Immutability

An entity's layer cannot change after creation.

```rust
#[test]
fn layer_is_immutable() {
    let mut world = World::new();

    let entity = world.create_entity(Layer::World);
    assert_eq!(world.get_layer(entity), Layer::World);

    // There is no set_layer method - this should not compile
    // world.set_layer(entity, Layer::Meta);  // ERROR: method doesn't exist

    // Layer stored separately from components
    assert!(world.get_component(entity, "Layer").is_none());
}
```

### Invariant D: Schema Frozen After Boot

Schema entities cannot be mutated after the boot phase ends.

```rust
#[test]
fn schema_frozen_after_boot() {
    let mut world = World::new();
    assert_eq!(world.phase(), Phase::Boot);

    // Create schema entity during boot
    let schema = world.create_entity(Layer::Schema);
    world.set_component(schema, "ComponentName", "HP");

    // End boot phase
    world.end_boot();
    assert_eq!(world.phase(), Phase::Idle);

    // Attempt to modify schema - should error
    let result = world.set_component(schema, "ComponentName", "Health");
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), LayerError::SchemaFrozen));
}
```

---

## Appendix D: Previously Completed Phases

The following phases were completed prior to this unification effort (see git history):
- Phase 1-10: Core ECS, VM, Grammar, Actions, Hooks, Preconditions, etc.
- Phase 11 (Sensory Propagation) and Phase 12 (Advanced Content) are deferred to after unification

---

## Next Steps

1. ~~Review and approve this plan~~ ✓
2. ~~Begin Foundation Sprint (Stages 0 + 1 + 2 together)~~ ✓
3. ~~Stage 3: Predicate Patterns~~ ✓
4. ~~Stage 4: Type Predicates~~ ✓
5. Next: Stage 5 (Hooks as Rules) or Stage 6 (Derivations with Caching)
6. Iterate based on learnings
