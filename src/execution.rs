//! Execution layer entities and pipeline.
//!
//! This module provides the infrastructure for tracking command execution
//! as a chain of entities. When a player types "take lamp", the system
//! creates a chain of execution entities:
//!
//! ```text
//! Input -> ParseResult -> Command -> ActionAttempt -> Result
//! ```
//!
//! Each entity in the chain is linked via the `DerivedFrom` relation,
//! enabling full traceability of how a result was produced.
//!
//! ## Execution Phases
//!
//! | Phase | Creates | From |
//! |-------|---------|------|
//! | Ingress | Input entity | Raw text |
//! | Parse | ParseResult | Input |
//! | Resolve | Command | ParseResult |
//! | Validate | ActionAttempt | Command |
//! | Execute | Result | ActionAttempt |
//!
//! ## Example
//!
//! ```ignore
//! use hornvale::execution::Pipeline;
//!
//! let mut pipeline = Pipeline::new(&mut world);
//!
//! // Create Input entity
//! let input = pipeline.create_input("take lamp", player);
//!
//! // Parse phase
//! let parse_result = pipeline.parse(input)?;
//!
//! // Resolve phase
//! let command = pipeline.resolve(parse_result)?;
//!
//! // Query the chain
//! let chain = world.query_ancestors(command, "DerivedFrom", 10);
//! // chain = [command, parse_result, input]
//! ```

use crate::World;
use crate::core::{
    Cardinality, ComponentTypeId, EntityId, Layer, RelationSchema, RelationTypeId, Value,
};
use crate::symbol::Symbol;

// =============================================================================
// Well-known component types for execution entities
// =============================================================================

/// Component types for execution layer entities.
pub mod components {
    use super::*;

    // --- Input entity components ---

    /// Raw input text.
    pub fn input_text() -> ComponentTypeId {
        ComponentTypeId::new("InputText")
    }

    /// The actor who provided the input.
    pub fn source_actor() -> ComponentTypeId {
        ComponentTypeId::new("SourceActor")
    }

    /// The room where input was provided.
    pub fn source_room() -> ComponentTypeId {
        ComponentTypeId::new("SourceRoom")
    }

    /// The tick when input was received.
    pub fn input_tick() -> ComponentTypeId {
        ComponentTypeId::new("InputTick")
    }

    /// Current execution phase of this entity.
    pub fn execution_phase() -> ComponentTypeId {
        ComponentTypeId::new("ExecutionPhase")
    }

    // --- ParseResult entity components ---

    /// The intent/action identified from parsing.
    pub fn intent() -> ComponentTypeId {
        ComponentTypeId::new("Intent")
    }

    /// Raw slot values before resolution.
    pub fn raw_slots() -> ComponentTypeId {
        ComponentTypeId::new("RawSlots")
    }

    /// The grammar form that matched.
    pub fn matched_form() -> ComponentTypeId {
        ComponentTypeId::new("MatchedForm")
    }

    /// Reference to the grammar entity that matched.
    pub fn matched_grammar() -> ComponentTypeId {
        ComponentTypeId::new("MatchedGrammar")
    }

    // --- Command entity components ---

    /// The verb/action symbol.
    pub fn verb() -> ComponentTypeId {
        ComponentTypeId::new("Verb")
    }

    /// The actor executing the command.
    pub fn actor() -> ComponentTypeId {
        ComponentTypeId::new("Actor")
    }

    /// The direct object entity.
    pub fn direct_object() -> ComponentTypeId {
        ComponentTypeId::new("DirectObject")
    }

    /// The indirect object entity.
    pub fn indirect_object() -> ComponentTypeId {
        ComponentTypeId::new("IndirectObject")
    }

    /// The room where the command is executed.
    pub fn command_room() -> ComponentTypeId {
        ComponentTypeId::new("CommandRoom")
    }

    // --- ActionAttempt entity components ---

    /// The action being attempted.
    pub fn action() -> ComponentTypeId {
        ComponentTypeId::new("Action")
    }

    /// Status of the attempt (validated, failed, etc.).
    pub fn status() -> ComponentTypeId {
        ComponentTypeId::new("Status")
    }

    /// Reason for failure (if failed).
    pub fn failure_reason() -> ComponentTypeId {
        ComponentTypeId::new("FailureReason")
    }

    // --- Result entity components ---

    /// Output text to show the player.
    pub fn output() -> ComponentTypeId {
        ComponentTypeId::new("Output")
    }

    /// List of mutations performed.
    pub fn mutations() -> ComponentTypeId {
        ComponentTypeId::new("Mutations")
    }

    // --- Trace components ---

    /// Type of trace span (rule-evaluation, effect-executed, etc.).
    pub fn trace_type() -> ComponentTypeId {
        ComponentTypeId::new("TraceType")
    }

    /// Name of the traced operation (rule name, etc.).
    pub fn trace_name() -> ComponentTypeId {
        ComponentTypeId::new("TraceName")
    }

    /// Tick when trace started.
    pub fn trace_start_tick() -> ComponentTypeId {
        ComponentTypeId::new("TraceStartTick")
    }

    /// Tick when trace ended.
    pub fn trace_end_tick() -> ComponentTypeId {
        ComponentTypeId::new("TraceEndTick")
    }

    /// Result of the traced operation.
    pub fn trace_result() -> ComponentTypeId {
        ComponentTypeId::new("TraceResult")
    }
}

// =============================================================================
// Well-known relation types for execution tracking
// =============================================================================

/// Relation types for execution tracking.
pub mod relations {
    use super::*;

    /// DerivedFrom relation: tracks how execution entities are derived.
    /// ParseResult DerivedFrom Input, Command DerivedFrom ParseResult, etc.
    pub fn derived_from() -> RelationTypeId {
        RelationTypeId::new("DerivedFrom")
    }

    /// TraceParent relation: tracks trace span hierarchy.
    pub fn trace_parent() -> RelationTypeId {
        RelationTypeId::new("TraceParent")
    }

    /// TraceReads relation: what entities a trace span read from.
    pub fn trace_reads() -> RelationTypeId {
        RelationTypeId::new("TraceReads")
    }

    /// TraceWrites relation: what entities a trace span wrote to.
    pub fn trace_writes() -> RelationTypeId {
        RelationTypeId::new("TraceWrites")
    }
}

// =============================================================================
// Execution phase constants
// =============================================================================

/// Execution phase symbols.
pub mod phases {
    use super::*;

    pub fn ingress() -> Symbol {
        Symbol::new("ingress")
    }

    pub fn parse() -> Symbol {
        Symbol::new("parse")
    }

    pub fn resolve() -> Symbol {
        Symbol::new("resolve")
    }

    pub fn validate() -> Symbol {
        Symbol::new("validate")
    }

    pub fn execute() -> Symbol {
        Symbol::new("execute")
    }

    pub fn egress() -> Symbol {
        Symbol::new("egress")
    }
}

/// Status values for ActionAttempt.
pub mod status {
    use super::*;

    pub fn pending() -> Symbol {
        Symbol::new("pending")
    }

    pub fn validated() -> Symbol {
        Symbol::new("validated")
    }

    pub fn failed() -> Symbol {
        Symbol::new("failed")
    }

    pub fn handled() -> Symbol {
        Symbol::new("handled")
    }

    pub fn completed() -> Symbol {
        Symbol::new("completed")
    }
}

// =============================================================================
// Relation schema registration
// =============================================================================

/// Register execution-related relation schemas.
///
/// This should be called during world initialization.
pub fn register_execution_relations(world: &mut World) {
    // DerivedFrom: Many-to-One (many entities can derive from one parent)
    // but typically an execution entity has exactly one parent
    world.register_relation(RelationSchema::new(
        "DerivedFrom",
        Cardinality::Many,
        Cardinality::One,
    ));

    // TraceParent: Many-to-One (many child spans under one parent)
    world.register_relation(RelationSchema::new(
        "TraceParent",
        Cardinality::Many,
        Cardinality::One,
    ));

    // TraceReads: Many-to-Many (spans read multiple entities, entities read by multiple spans)
    world.register_relation(RelationSchema::new(
        "TraceReads",
        Cardinality::Many,
        Cardinality::Many,
    ));

    // TraceWrites: Many-to-Many
    world.register_relation(RelationSchema::new(
        "TraceWrites",
        Cardinality::Many,
        Cardinality::Many,
    ));
}

// =============================================================================
// Execution entity creation
// =============================================================================

/// Create an Input execution entity.
///
/// This is the entry point for the execution pipeline.
pub fn create_input(
    world: &mut World,
    text: &str,
    actor: EntityId,
    room: Option<EntityId>,
) -> EntityId {
    let entity = world.create_entity_with_layer(Layer::Execution);

    world.set_component(entity, components::input_text(), Value::string(text));
    world.set_component(entity, components::source_actor(), Value::EntityRef(actor));
    world.set_component(
        entity,
        components::input_tick(),
        Value::Int(world.tick() as i64),
    );
    world.set_component(
        entity,
        components::execution_phase(),
        Value::Symbol(phases::ingress()),
    );

    if let Some(room) = room {
        world.set_component(entity, components::source_room(), Value::EntityRef(room));
    }

    entity
}

/// Create a ParseResult execution entity.
///
/// Links to the Input via DerivedFrom.
pub fn create_parse_result(
    world: &mut World,
    input: EntityId,
    intent: Symbol,
    matched_form: Option<&str>,
) -> EntityId {
    let entity = world.create_entity_with_layer(Layer::Execution);

    world.set_component(entity, components::intent(), Value::Symbol(intent));
    world.set_component(
        entity,
        components::execution_phase(),
        Value::Symbol(phases::parse()),
    );

    if let Some(form) = matched_form {
        world.set_component(entity, components::matched_form(), Value::string(form));
    }

    // Link to input
    world.add_relation(relations::derived_from(), entity, input);

    entity
}

/// Create a Command execution entity.
///
/// Links to the ParseResult via DerivedFrom.
pub fn create_command_entity(
    world: &mut World,
    parse_result: EntityId,
    verb: Symbol,
    actor: EntityId,
    direct_object: Option<EntityId>,
    indirect_object: Option<EntityId>,
    room: Option<EntityId>,
) -> EntityId {
    let entity = world.create_entity_with_layer(Layer::Execution);

    world.set_component(entity, components::verb(), Value::Symbol(verb));
    world.set_component(entity, components::actor(), Value::EntityRef(actor));
    world.set_component(
        entity,
        components::execution_phase(),
        Value::Symbol(phases::resolve()),
    );

    if let Some(obj) = direct_object {
        world.set_component(entity, components::direct_object(), Value::EntityRef(obj));
    }
    if let Some(obj) = indirect_object {
        world.set_component(entity, components::indirect_object(), Value::EntityRef(obj));
    }
    if let Some(room) = room {
        world.set_component(entity, components::command_room(), Value::EntityRef(room));
    }

    // Link to parse result
    world.add_relation(relations::derived_from(), entity, parse_result);

    entity
}

/// Create an ActionAttempt execution entity.
///
/// Links to the Command via DerivedFrom.
pub fn create_action_attempt(
    world: &mut World,
    command: EntityId,
    action: Symbol,
    initial_status: Symbol,
) -> EntityId {
    let entity = world.create_entity_with_layer(Layer::Execution);

    world.set_component(entity, components::action(), Value::Symbol(action));
    world.set_component(entity, components::status(), Value::Symbol(initial_status));
    world.set_component(
        entity,
        components::execution_phase(),
        Value::Symbol(phases::validate()),
    );

    // Link to command
    world.add_relation(relations::derived_from(), entity, command);

    entity
}

/// Create a Result execution entity.
///
/// Links to the ActionAttempt via DerivedFrom.
pub fn create_result(world: &mut World, attempt: EntityId, output: &str) -> EntityId {
    let entity = world.create_entity_with_layer(Layer::Execution);

    world.set_component(entity, components::output(), Value::string(output));
    world.set_component(
        entity,
        components::execution_phase(),
        Value::Symbol(phases::execute()),
    );

    // Link to attempt
    world.add_relation(relations::derived_from(), entity, attempt);

    entity
}

// =============================================================================
// Execution chain queries
// =============================================================================

/// Query the execution chain for an entity.
///
/// Returns the chain of entities that led to this one, from newest to oldest.
/// For example, for a Result entity: [Result, ActionAttempt, Command, ParseResult, Input]
pub fn query_execution_chain(world: &World, entity: EntityId, max_depth: usize) -> Vec<EntityId> {
    let mut chain = vec![entity];
    let mut current = entity;

    for _ in 0..max_depth {
        let parents = world.query_relation_forward(relations::derived_from(), current);
        if let Some(&parent) = parents.first() {
            chain.push(parent);
            current = parent;
        } else {
            break;
        }
    }

    chain
}

/// Get the Input entity that started this execution chain.
pub fn get_chain_input(world: &World, entity: EntityId) -> Option<EntityId> {
    let chain = query_execution_chain(world, entity, 10);
    chain.last().copied()
}

/// Get the original input text for an execution chain.
pub fn get_chain_input_text(world: &World, entity: EntityId) -> Option<String> {
    let input = get_chain_input(world, entity)?;
    world
        .get_component(input, components::input_text())
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
}

// =============================================================================
// Minimal Trace Span infrastructure
// =============================================================================

/// A handle to a trace span entity.
///
/// Used for scoped tracing. The span entity is created when the handle
/// is created, and can be completed via `end_span`.
#[derive(Debug, Clone, Copy)]
pub struct TraceSpan {
    /// The span entity ID.
    pub entity: EntityId,
}

/// Begin a trace span if tracing is enabled.
///
/// Returns `None` if tracing is disabled.
pub fn begin_trace_span(
    world: &mut World,
    span_type: &str,
    name: &str,
    parent: Option<TraceSpan>,
) -> Option<TraceSpan> {
    // Check if tracing is enabled (look for Tracing component on world meta)
    // For now, we'll check for a simple component flag
    if !is_tracing_enabled(world) {
        return None;
    }

    let entity = world.create_entity_with_layer(Layer::Execution);

    world.set_component(entity, components::trace_type(), Value::string(span_type));
    world.set_component(entity, components::trace_name(), Value::string(name));
    world.set_component(
        entity,
        components::trace_start_tick(),
        Value::Int(world.tick() as i64),
    );
    world.set_component(entity, components::status(), Value::string("running"));

    // Link to parent span if provided
    if let Some(parent_span) = parent {
        world.add_relation(relations::trace_parent(), entity, parent_span.entity);
    }

    Some(TraceSpan { entity })
}

/// End a trace span with a result.
pub fn end_trace_span(world: &mut World, span: Option<TraceSpan>, status: &str, result: Value) {
    if let Some(span) = span {
        world.set_component(
            span.entity,
            components::trace_end_tick(),
            Value::Int(world.tick() as i64),
        );
        world.set_component(span.entity, components::status(), Value::string(status));
        world.set_component(span.entity, components::trace_result(), result);
    }
}

/// Record that a trace span read from an entity.
pub fn trace_read(world: &mut World, span: Option<TraceSpan>, entity: EntityId) {
    if let Some(span) = span {
        world.add_relation(relations::trace_reads(), span.entity, entity);
    }
}

/// Record that a trace span wrote to an entity.
pub fn trace_write(world: &mut World, span: Option<TraceSpan>, entity: EntityId) {
    if let Some(span) = span {
        world.add_relation(relations::trace_writes(), span.entity, entity);
    }
}

// =============================================================================
// Tracing configuration
// =============================================================================

/// Component for tracing configuration (on a meta entity).
pub fn tracing_config() -> ComponentTypeId {
    ComponentTypeId::new("TracingConfig")
}

/// Check if tracing is enabled.
///
/// Looks for an entity with TracingConfig component set to true.
fn is_tracing_enabled(world: &World) -> bool {
    // Look for any entity with TracingConfig=true
    world
        .entities_with(tracing_config())
        .any(|(_, v)| v.as_bool() == Some(true))
}

/// Enable tracing by creating a tracing config entity.
pub fn enable_tracing(world: &mut World) -> EntityId {
    let entity = world.create_entity_with_layer(Layer::Meta);
    world.set_component(entity, tracing_config(), Value::Bool(true));
    entity
}

/// Disable tracing by setting the config to false.
pub fn disable_tracing(world: &mut World) {
    let entities: Vec<EntityId> = world
        .entities_with(tracing_config())
        .map(|(e, _)| e)
        .collect();
    for entity in entities {
        world.set_component(entity, tracing_config(), Value::Bool(false));
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_world() -> World {
        let mut world = World::new();
        register_execution_relations(&mut world);
        world.end_boot();
        world
    }

    #[test]
    fn test_create_input() {
        let mut world = setup_world();
        let player = world.create_entity();

        world.begin_tick();
        let input = create_input(&mut world, "take lamp", player, None);

        assert_eq!(world.get_layer(input), Layer::Execution);
        assert_eq!(
            world.get_component(input, components::input_text()),
            Some(&Value::string("take lamp"))
        );
        assert_eq!(
            world.get_component(input, components::source_actor()),
            Some(&Value::EntityRef(player))
        );
        assert_eq!(
            world.get_component(input, components::execution_phase()),
            Some(&Value::Symbol(phases::ingress()))
        );
    }

    #[test]
    fn test_execution_chain() {
        let mut world = setup_world();
        let player = world.create_entity();

        world.begin_tick();

        // Create chain: Input -> ParseResult -> Command -> ActionAttempt
        let input = create_input(&mut world, "take lamp", player, None);
        let parse = create_parse_result(&mut world, input, Symbol::new("take"), Some("(obj)"));
        let cmd = create_command_entity(
            &mut world,
            parse,
            Symbol::new("take"),
            player,
            None,
            None,
            None,
        );
        let attempt =
            create_action_attempt(&mut world, cmd, Symbol::new("take"), status::pending());

        // Query chain from attempt
        let chain = query_execution_chain(&world, attempt, 10);
        assert_eq!(chain.len(), 4);
        assert_eq!(chain[0], attempt);
        assert_eq!(chain[1], cmd);
        assert_eq!(chain[2], parse);
        assert_eq!(chain[3], input);

        // Get input text from chain
        let text = get_chain_input_text(&world, attempt);
        assert_eq!(text, Some("take lamp".to_string()));
    }

    #[test]
    fn test_derived_from_relation() {
        let mut world = setup_world();
        let player = world.create_entity();

        world.begin_tick();

        let input = create_input(&mut world, "look", player, None);
        let parse = create_parse_result(&mut world, input, Symbol::new("look"), None);

        // Check relation exists
        let parents = world.query_relation_forward(relations::derived_from(), parse);
        assert_eq!(parents.len(), 1);
        assert_eq!(parents[0], input);

        // Reverse query
        let children = world.query_relation_reverse(relations::derived_from(), input);
        assert_eq!(children.len(), 1);
        assert_eq!(children[0], parse);
    }

    #[test]
    fn test_tracing_disabled_by_default() {
        let mut world = setup_world();

        world.begin_tick();

        // Tracing should be disabled by default
        let span = begin_trace_span(&mut world, "test", "test-span", None);
        assert!(span.is_none());
    }

    #[test]
    fn test_tracing_enabled() {
        let mut world = setup_world();
        enable_tracing(&mut world);

        world.begin_tick();

        // Should create span when enabled
        let span = begin_trace_span(&mut world, "rule-evaluation", "test-rule", None);
        assert!(span.is_some());

        let span = span.unwrap();
        assert_eq!(world.get_layer(span.entity), Layer::Execution);
        assert_eq!(
            world.get_component(span.entity, components::trace_type()),
            Some(&Value::string("rule-evaluation"))
        );
        assert_eq!(
            world.get_component(span.entity, components::trace_name()),
            Some(&Value::string("test-rule"))
        );
    }

    #[test]
    fn test_trace_span_lifecycle() {
        let mut world = setup_world();
        enable_tracing(&mut world);

        world.begin_tick();

        let span = begin_trace_span(&mut world, "effect", "say-effect", None);
        assert!(span.is_some());

        // Initially running
        assert_eq!(
            world.get_component(span.unwrap().entity, components::status()),
            Some(&Value::string("running"))
        );

        // End span
        end_trace_span(&mut world, span, "completed", Value::Bool(true));

        assert_eq!(
            world.get_component(span.unwrap().entity, components::status()),
            Some(&Value::string("completed"))
        );
        assert_eq!(
            world.get_component(span.unwrap().entity, components::trace_result()),
            Some(&Value::Bool(true))
        );
    }

    #[test]
    fn test_trace_parent_hierarchy() {
        let mut world = setup_world();
        enable_tracing(&mut world);

        world.begin_tick();

        // Create parent span
        let parent = begin_trace_span(&mut world, "phase", "execute", None);

        // Create child span
        let child = begin_trace_span(&mut world, "rule", "burn-handler", parent);

        assert!(child.is_some());

        // Check parent relation
        let parents =
            world.query_relation_forward(relations::trace_parent(), child.unwrap().entity);
        assert_eq!(parents.len(), 1);
        assert_eq!(parents[0], parent.unwrap().entity);
    }

    #[test]
    fn test_trace_reads_writes() {
        let mut world = setup_world();
        enable_tracing(&mut world);

        let item = world.create_entity();
        world.set_component(item, "Name", "lamp");

        world.begin_tick();

        let span = begin_trace_span(&mut world, "effect", "take", None);

        // Record read
        trace_read(&mut world, span, item);

        // Check relation
        let reads = world.query_relation_forward(relations::trace_reads(), span.unwrap().entity);
        assert_eq!(reads.len(), 1);
        assert_eq!(reads[0], item);
    }

    #[test]
    fn test_execution_entities_cleaned_up() {
        let mut world = setup_world();
        let player = world.create_entity();

        world.begin_tick();

        let input = create_input(&mut world, "take lamp", player, None);
        let parse = create_parse_result(&mut world, input, Symbol::new("take"), None);

        // Entities exist during tick
        assert!(
            world
                .get_component(input, components::input_text())
                .is_some()
        );
        assert!(world.get_component(parse, components::intent()).is_some());

        world.end_tick();

        // Execution entities should be cleaned up
        assert!(
            world
                .get_component(input, components::input_text())
                .is_none()
        );
        assert!(world.get_component(parse, components::intent()).is_none());
    }

    #[test]
    fn test_create_full_chain() {
        let mut world = setup_world();
        let player = world.create_entity();
        let lamp = world.create_entity();
        let room = world.create_entity();

        world.begin_tick();

        // Full execution chain
        let input = create_input(&mut world, "take the brass lamp", player, Some(room));
        let parse = create_parse_result(
            &mut world,
            input,
            Symbol::new("take"),
            Some("(obj:portable)"),
        );
        let cmd = create_command_entity(
            &mut world,
            parse,
            Symbol::new("take"),
            player,
            Some(lamp),
            None,
            Some(room),
        );
        let attempt =
            create_action_attempt(&mut world, cmd, Symbol::new("take"), status::validated());
        let result = create_result(&mut world, attempt, "Taken.");

        // Verify chain
        let chain = query_execution_chain(&world, result, 10);
        assert_eq!(chain.len(), 5);

        // Verify components on each
        assert_eq!(
            world.get_component(cmd, components::direct_object()),
            Some(&Value::EntityRef(lamp))
        );
        assert_eq!(
            world.get_component(attempt, components::status()),
            Some(&Value::Symbol(status::validated()))
        );
        assert_eq!(
            world.get_component(result, components::output()),
            Some(&Value::string("Taken."))
        );
    }
}
