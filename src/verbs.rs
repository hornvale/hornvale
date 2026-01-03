//! Core verb handlers for interactive fiction.
//!
//! This module provides the basic verbs for text adventures:
//! - `look` - Describe current location and visible objects
//! - `go` - Move through exits
//! - `take` - Pick up objects
//! - `drop` - Drop carried objects
//! - `inventory` - List carried objects
//! - `examine` - Get detailed description of an object
//!
//! ## Design
//!
//! Verb handlers take a resolved command and world state, and produce
//! Output components that can be rendered to the player.

use crate::core::{ComponentTypeId, EntityId, RelationTypeId, Value, World};
use crate::input::{ResolvedCommand, ResolvedObject};
use crate::symbol::Symbol;
use std::sync::Arc;

/// Well-known component types for game objects.
pub mod components {
    use super::*;

    /// Output text to display to player.
    pub fn output() -> ComponentTypeId {
        ComponentTypeId::new("Output")
    }

    /// Primary name of an entity.
    pub fn name() -> ComponentTypeId {
        ComponentTypeId::new("Name")
    }

    /// Brief description (for lists).
    pub fn brief() -> ComponentTypeId {
        ComponentTypeId::new("Brief")
    }

    /// Detailed description (for examine).
    pub fn description() -> ComponentTypeId {
        ComponentTypeId::new("Description")
    }

    /// Room description (shown on look).
    pub fn room_description() -> ComponentTypeId {
        ComponentTypeId::new("RoomDescription")
    }

    /// Whether entity is a room.
    pub fn is_room() -> ComponentTypeId {
        ComponentTypeId::new("IsRoom")
    }

    /// Whether entity is portable.
    pub fn portable() -> ComponentTypeId {
        ComponentTypeId::new("Portable")
    }

    /// Whether entity is fixed (cannot be taken).
    pub fn fixed() -> ComponentTypeId {
        ComponentTypeId::new("Fixed")
    }

    /// Whether entity is a player.
    pub fn is_player() -> ComponentTypeId {
        ComponentTypeId::new("IsPlayer")
    }
}

/// Well-known relation types.
pub mod relations {
    use super::*;

    /// Entity is located in a room.
    pub fn in_room() -> RelationTypeId {
        RelationTypeId::new("InRoom")
    }

    /// Entity contains another entity (inventory).
    pub fn contains() -> RelationTypeId {
        RelationTypeId::new("Contains")
    }

    /// Exit relation: (room, direction) -> destination room.
    pub fn exit() -> RelationTypeId {
        RelationTypeId::new("Exit")
    }
}

/// Result of executing a verb.
#[derive(Debug, Clone)]
pub struct VerbResult {
    /// Output text to display.
    pub output: Arc<str>,
    /// Whether the verb succeeded.
    pub success: bool,
}

impl VerbResult {
    /// Create a successful result.
    pub fn success(output: impl Into<Arc<str>>) -> Self {
        Self {
            output: output.into(),
            success: true,
        }
    }

    /// Create a failed result.
    pub fn fail(output: impl Into<Arc<str>>) -> Self {
        Self {
            output: output.into(),
            success: false,
        }
    }
}

/// Get the name of an entity, or a fallback.
fn entity_name(world: &World, entity: EntityId) -> Arc<str> {
    world
        .get_component(entity, components::name())
        .and_then(|v| v.as_str())
        .map(Arc::from)
        .unwrap_or_else(|| Arc::from("something"))
}

/// Get brief description of an entity.
fn entity_brief(world: &World, entity: EntityId) -> Arc<str> {
    // Try brief first, then name
    world
        .get_component(entity, components::brief())
        .and_then(|v| v.as_str())
        .map(Arc::from)
        .or_else(|| {
            world
                .get_component(entity, components::name())
                .and_then(|v| v.as_str())
                .map(|s| Arc::from(format!("a {s}")))
        })
        .unwrap_or_else(|| Arc::from("something"))
}

/// Get the room an entity is in.
fn get_room(world: &World, entity: EntityId) -> Option<EntityId> {
    world
        .query_relation_forward(relations::in_room(), entity)
        .first()
        .copied()
}

/// Get all entities in a room.
fn entities_in_room(world: &World, room: EntityId) -> Vec<EntityId> {
    world.query_relation_reverse(relations::in_room(), room)
}

/// Get all entities carried by an entity.
fn carried_by(world: &World, carrier: EntityId) -> Vec<EntityId> {
    world.query_relation_forward(relations::contains(), carrier)
}

/// Check if entity is a room.
fn is_room(world: &World, entity: EntityId) -> bool {
    world
        .get_component(entity, components::is_room())
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

/// Check if entity is portable.
fn is_portable(world: &World, entity: EntityId) -> bool {
    // Portable if Portable=true OR not Fixed
    if let Some(portable) = world.get_component(entity, components::portable()) {
        return portable.as_bool().unwrap_or(false);
    }
    // Not fixed = portable by default
    !world
        .get_component(entity, components::fixed())
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

// ============================================================================
// Verb handlers
// ============================================================================

/// Handle the "look" verb - describe current location.
pub fn handle_look(world: &World, actor: EntityId) -> VerbResult {
    let room = match get_room(world, actor) {
        Some(r) => r,
        None => return VerbResult::fail("You are nowhere."),
    };

    let mut output = String::new();

    // Room name
    let room_name = entity_name(world, room);
    output.push_str(&format!("**{room_name}**\n\n"));

    // Room description
    if let Some(desc) = world.get_component(room, components::room_description()) {
        if let Some(s) = desc.as_str() {
            output.push_str(s);
            output.push_str("\n\n");
        }
    } else if let Some(desc) = world.get_component(room, components::description()) {
        if let Some(s) = desc.as_str() {
            output.push_str(s);
            output.push_str("\n\n");
        }
    }

    // List other entities in room
    let entities = entities_in_room(world, room);
    let visible: Vec<_> = entities
        .into_iter()
        .filter(|&e| e != actor && !is_room(world, e))
        .collect();

    if !visible.is_empty() {
        output.push_str("You can see:\n");
        for entity in visible {
            let brief = entity_brief(world, entity);
            output.push_str(&format!("- {brief}\n"));
        }
    }

    // List exits (TODO: when Exit relation is implemented)

    VerbResult::success(output.trim())
}

/// Handle the "inventory" verb - list carried items.
pub fn handle_inventory(world: &World, actor: EntityId) -> VerbResult {
    let items = carried_by(world, actor);

    if items.is_empty() {
        return VerbResult::success("You are carrying nothing.");
    }

    let mut output = String::from("You are carrying:\n");
    for item in items {
        let brief = entity_brief(world, item);
        output.push_str(&format!("- {brief}\n"));
    }

    VerbResult::success(output.trim())
}

/// Handle the "examine" verb - describe an object in detail.
pub fn handle_examine(world: &World, target: EntityId) -> VerbResult {
    // Get detailed description, or fall back to brief/name
    if let Some(desc) = world.get_component(target, components::description()) {
        if let Some(s) = desc.as_str() {
            return VerbResult::success(s);
        }
    }

    let name = entity_name(world, target);
    VerbResult::success(format!("You see nothing special about the {name}."))
}

/// Handle the "take" verb - pick up an object.
pub fn handle_take(world: &mut World, actor: EntityId, target: EntityId) -> VerbResult {
    let name = entity_name(world, target);

    // Check if already carrying
    let inventory = carried_by(world, actor);
    if inventory.contains(&target) {
        return VerbResult::fail(format!("You're already carrying the {name}."));
    }

    // Check if portable
    if !is_portable(world, target) {
        return VerbResult::fail(format!("The {name} is fixed in place."));
    }

    // Check if in same room
    let actor_room = get_room(world, actor);
    let target_room = get_room(world, target);
    if actor_room != target_room || actor_room.is_none() {
        return VerbResult::fail(format!("You can't reach the {name}."));
    }

    // Remove from room, add to inventory
    if let Some(room) = target_room {
        world.remove_relation(relations::in_room(), target, room);
    }
    world.add_relation(relations::contains(), actor, target);

    VerbResult::success("Taken.")
}

/// Handle the "drop" verb - drop an object.
pub fn handle_drop(world: &mut World, actor: EntityId, target: EntityId) -> VerbResult {
    let name = entity_name(world, target);

    // Check if carrying
    let inventory = carried_by(world, actor);
    if !inventory.contains(&target) {
        return VerbResult::fail(format!("You're not carrying the {name}."));
    }

    // Get current room
    let room = match get_room(world, actor) {
        Some(r) => r,
        None => return VerbResult::fail("You are nowhere."),
    };

    // Remove from inventory, add to room
    world.remove_relation(relations::contains(), actor, target);
    world.add_relation(relations::in_room(), target, room);

    VerbResult::success("Dropped.")
}

/// Handle the "go" verb - move through an exit.
pub fn handle_go(world: &mut World, actor: EntityId, direction: Symbol) -> VerbResult {
    let current_room = match get_room(world, actor) {
        Some(r) => r,
        None => return VerbResult::fail("You are nowhere."),
    };

    // Look for exit in this direction
    // We'll use a component on the room that stores exit directions
    // Format: Exit_<direction> = destination entity
    let exit_comp = ComponentTypeId::new(&format!("Exit_{}", direction.as_str()));

    let destination = match world.get_component(current_room, exit_comp) {
        Some(Value::EntityRef(dest)) => *dest,
        _ => {
            return VerbResult::fail(format!("You can't go {}.", direction.as_str()));
        }
    };

    // Move actor
    world.remove_relation(relations::in_room(), actor, current_room);
    world.add_relation(relations::in_room(), actor, destination);

    // Return look description of new room
    let look_result = handle_look(world, actor);
    VerbResult::success(look_result.output)
}

/// Execute a resolved command.
pub fn execute_command(
    world: &mut World,
    actor: EntityId,
    command: &ResolvedCommand,
) -> VerbResult {
    let verb = command.verb.as_str();

    match verb.as_ref() {
        "look" | "l" => handle_look(world, actor),

        "inventory" | "inv" | "i" => handle_inventory(world, actor),

        "examine" | "x" | "look at" => match &command.direct_object {
            Some(ResolvedObject::Entity(target)) => handle_examine(world, *target),
            _ => VerbResult::fail("Examine what?"),
        },

        "take" | "get" | "pick up" => match &command.direct_object {
            Some(ResolvedObject::Entity(target)) => handle_take(world, actor, *target),
            _ => VerbResult::fail("Take what?"),
        },

        "drop" | "put down" => match &command.direct_object {
            Some(ResolvedObject::Entity(target)) => handle_drop(world, actor, *target),
            _ => VerbResult::fail("Drop what?"),
        },

        "go" | "walk" | "move" => match &command.direct_object {
            Some(ResolvedObject::Direction(dir)) => handle_go(world, actor, *dir),
            _ => VerbResult::fail("Go where?"),
        },

        // Direction shortcuts
        "north" | "n" => handle_go(world, actor, Symbol::new("north")),
        "south" | "s" => handle_go(world, actor, Symbol::new("south")),
        "east" | "e" => handle_go(world, actor, Symbol::new("east")),
        "west" | "w" => handle_go(world, actor, Symbol::new("west")),
        "up" | "u" => handle_go(world, actor, Symbol::new("up")),
        "down" | "d" => handle_go(world, actor, Symbol::new("down")),
        "northeast" | "ne" => handle_go(world, actor, Symbol::new("northeast")),
        "northwest" | "nw" => handle_go(world, actor, Symbol::new("northwest")),
        "southeast" | "se" => handle_go(world, actor, Symbol::new("southeast")),
        "southwest" | "sw" => handle_go(world, actor, Symbol::new("southwest")),

        _ => VerbResult::fail(format!("I don't understand '{verb}'.")),
    }
}

/// Execute an action based on a grammar match.
///
/// This function dispatches to verb handlers based on the action name
/// from a `GrammarMatch`, using pre-resolved entity slots.
///
/// Hooks are executed in order:
/// 1. Before hooks (can veto)
/// 2. On hooks (can handle)
/// 3. Default handler (if not handled)
/// 4. After hooks
pub fn execute_grammar_action(
    world: &mut World,
    actor: EntityId,
    grammar_match: &crate::GrammarMatch,
    stdlib: &crate::vm::StdLib,
) -> VerbResult {
    use crate::hooks::{run_after_hooks, run_before_hooks, run_on_hooks};
    use crate::vm::HookContext;

    let action_name = grammar_match.action.action_name();
    let action_str = action_name.as_str();

    // Build hook context
    let direct_object = grammar_match.get_entity("obj");
    let indirect_object = grammar_match.get_entity("indirect");
    let room = get_room(world, actor);

    let mut hook_context = HookContext::new(actor);
    if let Some(obj) = direct_object {
        hook_context = hook_context.with_direct_object(obj);
    }
    if let Some(obj) = indirect_object {
        hook_context = hook_context.with_indirect_object(obj);
    }
    if let Some(r) = room {
        hook_context = hook_context.with_room(r);
    }

    let mut output_parts: Vec<String> = Vec::new();
    let mut pending_deletions: Vec<EntityId> = Vec::new();

    // Run Before hooks
    if let Ok(before_result) = run_before_hooks(world, &action_str, &hook_context, stdlib) {
        output_parts.extend(before_result.output);
        pending_deletions.extend(before_result.deletions);

        if before_result.vetoed {
            // Apply deletions even on veto
            for entity in pending_deletions {
                world.delete_entity(entity);
            }
            let msg = if output_parts.is_empty() {
                "You can't do that.".to_string()
            } else {
                output_parts.join("\n")
            };
            return VerbResult::fail(msg);
        }
    }

    // Run On hooks
    let mut handled = false;
    if let Ok(on_result) = run_on_hooks(world, &action_str, &hook_context, stdlib) {
        output_parts.extend(on_result.output);
        pending_deletions.extend(on_result.deletions);
        handled = on_result.handled;
    }

    // Run default handler if not handled by On hook
    if !handled {
        let default_result = execute_default_handler(world, actor, grammar_match);
        if !default_result.output.is_empty() {
            output_parts.push(default_result.output.to_string());
        }
    }

    // Run After hooks
    if let Ok(after_result) = run_after_hooks(world, &action_str, &hook_context, stdlib) {
        output_parts.extend(after_result.output);
        pending_deletions.extend(after_result.deletions);
    }

    // Apply pending deletions
    for entity in pending_deletions {
        world.delete_entity(entity);
    }

    // Combine output
    let final_output = output_parts.join("\n");
    VerbResult::success(final_output)
}

/// Execute the default handler for an action (without hooks).
fn execute_default_handler(
    world: &mut World,
    actor: EntityId,
    grammar_match: &crate::GrammarMatch,
) -> VerbResult {
    let action_name = grammar_match.action.action_name();
    let action_str = action_name.as_str();

    match action_str.as_str() {
        "look" | "look-around" => handle_look(world, actor),

        "inventory" => handle_inventory(world, actor),

        "examine" => {
            // Get entity directly from slot (already resolved)
            if let Some(target) = grammar_match.get_entity("obj") {
                handle_examine(world, target)
            } else {
                VerbResult::fail("Examine what?")
            }
        }

        "search" => {
            // Look inside a container
            if let Some(target) = grammar_match.get_entity("obj") {
                // For now, just examine - can be enhanced to show contents
                handle_examine(world, target)
            } else {
                VerbResult::fail("Search what?")
            }
        }

        "take" | "get" => {
            if let Some(target) = grammar_match.get_entity("obj") {
                handle_take(world, actor, target)
            } else {
                VerbResult::fail("Take what?")
            }
        }

        "drop" => {
            if let Some(target) = grammar_match.get_entity("obj") {
                handle_drop(world, actor, target)
            } else {
                VerbResult::fail("Drop what?")
            }
        }

        "go" => {
            if let Some(dir) = grammar_match.get_direction("dir") {
                handle_go(world, actor, dir)
            } else {
                VerbResult::fail("Go where?")
            }
        }

        "look-direction" => {
            if let Some(dir) = grammar_match.get_direction("dir") {
                // For now, just report the direction - can be enhanced later
                VerbResult::success(format!("You look {}.", dir.as_str()))
            } else {
                VerbResult::fail("Look which way?")
            }
        }

        // Direction shortcuts: go-north, go-south, etc.
        _ if action_str.starts_with("go-") => {
            let direction = &action_str[3..]; // Strip "go-" prefix
            handle_go(world, actor, Symbol::new(direction))
        }

        _ => VerbResult::fail(format!("I don't know how to '{action_str}'.")),
    }
}

/// Execute an action based on a syntax table match (deprecated, use execute_grammar_action).
///
/// This function dispatches to verb handlers based on the action name
/// from a `SyntaxMatch`, using the captured slots for arguments.
pub fn execute_action(
    world: &mut World,
    actor: EntityId,
    action_match: &crate::SyntaxMatch,
) -> VerbResult {
    let action_name = action_match.action.name();

    match action_name.as_str() {
        "look" | "look-around" => handle_look(world, actor),

        "inventory" => handle_inventory(world, actor),

        "examine" => {
            // Get noun slot - need to resolve entity from name
            if let Some(noun) = action_match.slots.get("noun") {
                if let Some(target) = resolve_entity_by_name(world, actor, noun) {
                    handle_examine(world, target)
                } else {
                    VerbResult::fail(format!("I don't see '{noun}' here."))
                }
            } else {
                VerbResult::fail("Examine what?")
            }
        }

        "take" | "get" => {
            if let Some(noun) = action_match.slots.get("noun") {
                if let Some(target) = resolve_entity_by_name(world, actor, noun) {
                    handle_take(world, actor, target)
                } else {
                    VerbResult::fail(format!("I don't see '{noun}' here."))
                }
            } else {
                VerbResult::fail("Take what?")
            }
        }

        "drop" => {
            if let Some(noun) = action_match.slots.get("noun") {
                if let Some(target) = resolve_entity_by_name(world, actor, noun) {
                    handle_drop(world, actor, target)
                } else {
                    VerbResult::fail(format!("You're not carrying '{noun}'."))
                }
            } else {
                VerbResult::fail("Drop what?")
            }
        }

        "go" => {
            if let Some(dir) = action_match.slots.get("direction") {
                handle_go(world, actor, Symbol::new(dir))
            } else {
                VerbResult::fail("Go where?")
            }
        }

        _ => VerbResult::fail(format!("I don't know how to '{action_name}'.")),
    }
}

/// Resolve an entity by name from the actor's perspective.
///
/// Looks for entities in the same room or carried by the actor.
fn resolve_entity_by_name(world: &World, actor: EntityId, name: &str) -> Option<EntityId> {
    let name_lower = name.to_lowercase();

    // Get actor's location
    let location = world
        .query_relation_forward(relations::in_room(), actor)
        .into_iter()
        .next();

    // Search entities in the same room
    if let Some(room) = location {
        for entity in world.all_entities() {
            // Check if in same room
            let in_same_room = world
                .query_relation_forward(relations::in_room(), entity)
                .contains(&room);

            if in_same_room {
                if let Some(Value::String(entity_name)) =
                    world.get_component(entity, components::name())
                {
                    if entity_name.to_lowercase() == name_lower {
                        return Some(entity);
                    }
                }
            }
        }
    }

    // Search carried entities
    for carried in world.query_relation_forward(relations::contains(), actor) {
        if let Some(Value::String(entity_name)) = world.get_component(carried, components::name()) {
            if entity_name.to_lowercase() == name_lower {
                return Some(carried);
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema};

    fn setup_world() -> (World, EntityId, EntityId) {
        let mut world = World::new();

        // Register relations
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create room
        let room = world.create_entity();
        world.set_component(room, components::is_room(), Value::Bool(true));
        world.set_component(room, components::name(), Value::string("Test Room"));
        world.set_component(
            room,
            components::room_description(),
            Value::string("A simple test room."),
        );

        // Create player
        let player = world.create_entity();
        world.set_component(player, components::is_player(), Value::Bool(true));
        world.set_component(player, components::name(), Value::string("player"));
        world.add_relation(relations::in_room(), player, room);

        (world, player, room)
    }

    #[test]
    fn test_look() {
        let (world, player, _room) = setup_world();
        let result = handle_look(&world, player);

        assert!(result.success);
        assert!(result.output.contains("Test Room"));
        assert!(result.output.contains("simple test room"));
    }

    #[test]
    fn test_look_with_objects() {
        let (mut world, player, room) = setup_world();

        // Add an object
        let lamp = world.create_entity();
        world.set_component(lamp, components::name(), Value::string("lamp"));
        world.set_component(lamp, components::brief(), Value::string("a brass lamp"));
        world.add_relation(relations::in_room(), lamp, room);

        let result = handle_look(&world, player);
        assert!(result.success);
        assert!(result.output.contains("brass lamp"));
    }

    #[test]
    fn test_inventory_empty() {
        let (world, player, _room) = setup_world();
        let result = handle_inventory(&world, player);

        assert!(result.success);
        assert!(result.output.contains("nothing"));
    }

    #[test]
    fn test_take() {
        let (mut world, player, room) = setup_world();

        // Add a portable object
        let lamp = world.create_entity();
        world.set_component(lamp, components::name(), Value::string("lamp"));
        world.set_component(lamp, components::portable(), Value::Bool(true));
        world.add_relation(relations::in_room(), lamp, room);

        let result = handle_take(&mut world, player, lamp);
        assert!(result.success);

        // Verify it's in inventory
        let inv = handle_inventory(&world, player);
        assert!(inv.output.contains("lamp"));
    }

    #[test]
    fn test_take_fixed() {
        let (mut world, player, room) = setup_world();

        // Add a fixed object
        let statue = world.create_entity();
        world.set_component(statue, components::name(), Value::string("statue"));
        world.set_component(statue, components::fixed(), Value::Bool(true));
        world.add_relation(relations::in_room(), statue, room);

        let result = handle_take(&mut world, player, statue);
        assert!(!result.success);
        assert!(result.output.contains("fixed"));
    }

    #[test]
    fn test_drop() {
        let (mut world, player, _room) = setup_world();

        // Add an item to inventory
        let key = world.create_entity();
        world.set_component(key, components::name(), Value::string("key"));
        world.add_relation(relations::contains(), player, key);

        let result = handle_drop(&mut world, player, key);
        assert!(result.success);

        // Verify it's no longer in inventory
        let inv = handle_inventory(&world, player);
        assert!(!inv.output.contains("key"));
    }

    #[test]
    fn test_examine() {
        let (mut world, _player, room) = setup_world();

        let lamp = world.create_entity();
        world.set_component(lamp, components::name(), Value::string("lamp"));
        world.set_component(
            lamp,
            components::description(),
            Value::string("A beautiful brass lamp, slightly tarnished."),
        );
        world.add_relation(relations::in_room(), lamp, room);

        let result = handle_examine(&world, lamp);
        assert!(result.success);
        assert!(result.output.contains("brass lamp"));
        assert!(result.output.contains("tarnished"));
    }

    #[test]
    fn test_go() {
        let (mut world, player, room1) = setup_world();

        // Create second room
        let room2 = world.create_entity();
        world.set_component(room2, components::is_room(), Value::Bool(true));
        world.set_component(room2, components::name(), Value::string("Garden"));
        world.set_component(
            room2,
            components::room_description(),
            Value::string("A lovely garden."),
        );

        // Add exit from room1 to room2
        world.set_component(
            room1,
            ComponentTypeId::new("Exit_north"),
            Value::EntityRef(room2),
        );

        let result = handle_go(&mut world, player, Symbol::new("north"));
        assert!(result.success);
        assert!(result.output.contains("Garden"));

        // Verify player is in new room
        let new_room = get_room(&world, player).unwrap();
        assert_eq!(new_room, room2);
    }

    #[test]
    fn test_go_blocked() {
        let (mut world, player, _room) = setup_world();

        let result = handle_go(&mut world, player, Symbol::new("north"));
        assert!(!result.success);
        assert!(result.output.contains("can't go"));
    }

    #[test]
    fn test_execute_command() {
        let (mut world, player, _room) = setup_world();

        let command = ResolvedCommand {
            verb: Symbol::new("look"),
            direct_object: None,
            indirect_object: None,
            arguments: vec![],
        };

        let result = execute_command(&mut world, player, &command);
        assert!(result.success);
        assert!(result.output.contains("Test Room"));
    }
}
