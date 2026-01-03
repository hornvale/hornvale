//! Virtual machine execution engine.

use super::bytecode::OpCode;
use super::chunk::Chunk;
use super::stdlib::{StdLib, StdLibError};
use crate::core::{ComponentTypeId, EntityId, RelationTypeId, Value, World};
use crate::rng::SeededRng;

/// Context for action execution.
///
/// Provides access to the entities involved in an action:
/// - `actor`: The entity performing the action (always present)
/// - `direct_object`: The primary target of the action (e.g., "lamp" in "take lamp")
/// - `indirect_object`: Secondary target (e.g., "table" in "put lamp on table")
/// - `room`: The room where the action is taking place
#[derive(Debug, Clone)]
pub struct ActionContext {
    /// The entity performing the action.
    pub actor: EntityId,
    /// The direct object of the action (if any).
    pub direct_object: Option<EntityId>,
    /// The indirect object of the action (if any).
    pub indirect_object: Option<EntityId>,
    /// The room where the action takes place (if known).
    pub room: Option<EntityId>,
}

impl ActionContext {
    /// Create a new action context with just an actor.
    pub fn new(actor: EntityId) -> Self {
        Self {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        }
    }

    /// Set the direct object.
    pub fn with_direct_object(mut self, obj: EntityId) -> Self {
        self.direct_object = Some(obj);
        self
    }

    /// Set the indirect object.
    pub fn with_indirect_object(mut self, obj: EntityId) -> Self {
        self.indirect_object = Some(obj);
        self
    }

    /// Set the room.
    pub fn with_room(mut self, room: EntityId) -> Self {
        self.room = Some(room);
        self
    }
}

/// VM execution error.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum VMError {
    #[error("type error: expected {expected}, got {got}")]
    TypeError {
        expected: &'static str,
        got: &'static str,
    },

    #[error("division by zero")]
    DivisionByZero,

    #[error("register overflow: tried to access register {0}")]
    RegisterOverflow(u8),

    #[error("constant not found: index {0}")]
    ConstantNotFound(u16),

    #[error("unknown function: {0}")]
    UnknownFunction(String),

    #[error("stdlib error: {0}")]
    StdLib(#[from] StdLibError),

    #[error("no entity context set")]
    NoEntityContext,

    #[error("no RNG available (VM needs a seed for random operations)")]
    NoRng,

    #[error("no action context available (operation requires action execution context)")]
    NoActionContext,

    #[error("instruction pointer out of bounds")]
    IPOutOfBounds,
}

/// Nil value (represented as Bool(false) for simplicity).
const NIL: Value = Value::Bool(false);

/// A pending component mutation.
#[derive(Debug, Clone)]
pub struct PendingSetComponent {
    pub entity: EntityId,
    pub component: ComponentTypeId,
    pub value: Value,
}

/// A pending relation mutation.
#[derive(Debug, Clone)]
pub struct PendingRelation {
    pub relation: RelationTypeId,
    pub from: EntityId,
    pub to: EntityId,
}

/// Virtual machine for executing bytecode.
pub struct VM<'a> {
    /// The bytecode chunk being executed.
    chunk: &'a Chunk,
    /// Instruction pointer.
    ip: usize,
    /// Register file (256 registers).
    registers: Box<[Value; 256]>,
    /// World reference (read-only).
    world: &'a World,
    /// Standard library.
    stdlib: &'a StdLib,
    /// Entity context (for pattern matching / derivation).
    entity: Option<EntityId>,
    /// Random number generator (for seeded generation).
    rng: Option<SeededRng>,
    /// Action execution context (actor, objects, room).
    action_context: Option<ActionContext>,
    /// Output buffer for `say` effects.
    output_buffer: Vec<String>,
    /// Entities marked for destruction by `destroy` effects.
    pending_deletions: Vec<EntityId>,
    /// Pending component mutations.
    pending_set_components: Vec<PendingSetComponent>,
    /// Pending relation additions.
    pending_relate: Vec<PendingRelation>,
    /// Pending relation removals.
    pending_unrelate: Vec<PendingRelation>,
}

impl<'a> VM<'a> {
    /// Create a new VM.
    pub fn new(chunk: &'a Chunk, world: &'a World, stdlib: &'a StdLib) -> Self {
        Self {
            chunk,
            ip: 0,
            registers: Box::new(std::array::from_fn(|_| NIL.clone())),
            world,
            stdlib,
            entity: None,
            rng: None,
            action_context: None,
            output_buffer: Vec::new(),
            pending_deletions: Vec::new(),
            pending_set_components: Vec::new(),
            pending_relate: Vec::new(),
            pending_unrelate: Vec::new(),
        }
    }

    /// Set the entity context for world access.
    pub fn with_entity(mut self, entity: EntityId) -> Self {
        self.entity = Some(entity);
        self
    }

    /// Set the RNG for random operations.
    pub fn with_rng(mut self, rng: SeededRng) -> Self {
        self.rng = Some(rng);
        self
    }

    /// Set the RNG from a seed.
    pub fn with_seed(mut self, seed: u64) -> Self {
        self.rng = Some(SeededRng::new(seed));
        self
    }

    /// Set the action context for action execution.
    pub fn with_action_context(mut self, ctx: ActionContext) -> Self {
        self.action_context = Some(ctx);
        self
    }

    /// Take the output buffer, leaving it empty.
    pub fn take_output(&mut self) -> Vec<String> {
        std::mem::take(&mut self.output_buffer)
    }

    /// Take the pending deletions, leaving the list empty.
    pub fn take_pending_deletions(&mut self) -> Vec<EntityId> {
        std::mem::take(&mut self.pending_deletions)
    }

    /// Take the pending component mutations, leaving the list empty.
    pub fn take_pending_set_components(&mut self) -> Vec<PendingSetComponent> {
        std::mem::take(&mut self.pending_set_components)
    }

    /// Take the pending relation additions, leaving the list empty.
    pub fn take_pending_relate(&mut self) -> Vec<PendingRelation> {
        std::mem::take(&mut self.pending_relate)
    }

    /// Take the pending relation removals, leaving the list empty.
    pub fn take_pending_unrelate(&mut self) -> Vec<PendingRelation> {
        std::mem::take(&mut self.pending_unrelate)
    }

    /// Execute the chunk and return the result.
    pub fn run(&mut self) -> Result<Value, VMError> {
        loop {
            let op = self
                .chunk
                .get_instruction(self.ip)
                .ok_or(VMError::IPOutOfBounds)?;

            self.ip += 1;

            match self.execute(op)? {
                ControlFlow::Continue => {}
                ControlFlow::Return(value) => return Ok(value),
            }
        }
    }

    /// Execute a single instruction.
    fn execute(&mut self, op: OpCode) -> Result<ControlFlow, VMError> {
        match op {
            // === Load/Store ===
            OpCode::LoadConst { dst, idx } => {
                let value = self
                    .chunk
                    .get_constant(idx)
                    .ok_or(VMError::ConstantNotFound(idx))?
                    .clone();
                self.set_reg(dst, value);
            }

            OpCode::LoadNil { dst } => {
                self.set_reg(dst, NIL.clone());
            }

            OpCode::LoadBool { dst, value } => {
                self.set_reg(dst, Value::Bool(value));
            }

            OpCode::Move { dst, src } => {
                let value = self.get_reg(src).clone();
                self.set_reg(dst, value);
            }

            // === Arithmetic ===
            OpCode::Add { dst, a, b } => {
                let result = self.binary_arith(a, b, |x, y| x + y, |x, y| x + y)?;
                self.set_reg(dst, result);
            }

            OpCode::Sub { dst, a, b } => {
                let result = self.binary_arith(a, b, |x, y| x - y, |x, y| x - y)?;
                self.set_reg(dst, result);
            }

            OpCode::Mul { dst, a, b } => {
                let result = self.binary_arith(a, b, |x, y| x * y, |x, y| x * y)?;
                self.set_reg(dst, result);
            }

            OpCode::Div { dst, a, b } => {
                // Check for division by zero
                let bv = self.get_reg(b);
                match bv {
                    Value::Int(0) => return Err(VMError::DivisionByZero),
                    Value::Float(f) if *f == 0.0 => return Err(VMError::DivisionByZero),
                    _ => {}
                }
                let result = self.binary_arith(a, b, |x, y| x / y, |x, y| x / y)?;
                self.set_reg(dst, result);
            }

            OpCode::Mod { dst, a, b } => {
                let bv = self.get_reg(b);
                match bv {
                    Value::Int(0) => return Err(VMError::DivisionByZero),
                    Value::Float(f) if *f == 0.0 => return Err(VMError::DivisionByZero),
                    _ => {}
                }
                let result = self.binary_arith(a, b, |x, y| x % y, |x, y| x % y)?;
                self.set_reg(dst, result);
            }

            OpCode::Neg { dst, src } => {
                let result = match self.get_reg(src) {
                    Value::Int(n) => Value::Int(-n),
                    Value::Float(n) => Value::Float(-n),
                    other => {
                        return Err(VMError::TypeError {
                            expected: "number",
                            got: other.type_name(),
                        });
                    }
                };
                self.set_reg(dst, result);
            }

            // === Comparison ===
            OpCode::Eq { dst, a, b } => {
                let result = self.get_reg(a) == self.get_reg(b);
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::Ne { dst, a, b } => {
                let result = self.get_reg(a) != self.get_reg(b);
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::Lt { dst, a, b } => {
                let result = self.compare(a, b, |o| o.is_lt())?;
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::Le { dst, a, b } => {
                let result = self.compare(a, b, |o| o.is_le())?;
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::Gt { dst, a, b } => {
                let result = self.compare(a, b, |o| o.is_gt())?;
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::Ge { dst, a, b } => {
                let result = self.compare(a, b, |o| o.is_ge())?;
                self.set_reg(dst, Value::Bool(result));
            }

            // === Logic ===
            OpCode::Not { dst, src } => {
                let result = !self.is_truthy(self.get_reg(src));
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::And { dst, a, b } => {
                // Short-circuit: if a is falsy, result is a; otherwise result is b
                let av = self.get_reg(a);
                let result = if self.is_truthy(av) {
                    self.get_reg(b).clone()
                } else {
                    av.clone()
                };
                self.set_reg(dst, result);
            }

            OpCode::Or { dst, a, b } => {
                // Short-circuit: if a is truthy, result is a; otherwise result is b
                let av = self.get_reg(a);
                let result = if self.is_truthy(av) {
                    av.clone()
                } else {
                    self.get_reg(b).clone()
                };
                self.set_reg(dst, result);
            }

            // === Control Flow ===
            OpCode::Jump { offset } => {
                self.ip = (self.ip as isize + offset as isize) as usize;
            }

            OpCode::JumpIf { cond, offset } => {
                if self.is_truthy(self.get_reg(cond)) {
                    self.ip = (self.ip as isize + offset as isize) as usize;
                }
            }

            OpCode::JumpIfNot { cond, offset } => {
                if !self.is_truthy(self.get_reg(cond)) {
                    self.ip = (self.ip as isize + offset as isize) as usize;
                }
            }

            // === Function Calls ===
            OpCode::Call {
                dst,
                func_idx,
                args,
                nargs,
            } => {
                // Get function name from constant pool
                let func_name = self
                    .chunk
                    .get_constant(func_idx)
                    .ok_or(VMError::ConstantNotFound(func_idx))?;

                let func_symbol = match func_name {
                    Value::Symbol(s) => *s,
                    _ => {
                        return Err(VMError::TypeError {
                            expected: "symbol",
                            got: func_name.type_name(),
                        });
                    }
                };

                // Get the stdlib function
                let func = self
                    .stdlib
                    .get(func_symbol)
                    .ok_or_else(|| VMError::UnknownFunction(func_symbol.as_str().to_string()))?;

                // Collect arguments
                let mut arg_values = Vec::with_capacity(nargs as usize);
                for i in 0..nargs {
                    arg_values.push(self.get_reg(args + i).clone());
                }

                // Call the function
                let result = func(&arg_values)?;
                self.set_reg(dst, result);
            }

            // === World Access ===
            OpCode::GetComponent {
                dst,
                entity,
                component,
            } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                let component_id = self.as_component_type(self.get_reg(component))?;

                let result = self
                    .world
                    .get_component(entity_id, component_id)
                    .cloned()
                    .unwrap_or_else(|| NIL.clone());
                self.set_reg(dst, result);
            }

            OpCode::HasComponent {
                dst,
                entity,
                component,
            } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                let component_id = self.as_component_type(self.get_reg(component))?;

                let result = self.world.has_component(entity_id, component_id);
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::QueryRelation { dst, rel, from } => {
                let rel_id = self.as_relation_type(self.get_reg(rel))?;
                let from_id = self.as_entity(self.get_reg(from))?;

                let results = self.world.query_relation_forward(rel_id, from_id);
                // Return as a list of entity refs
                let result = Value::list(results.into_iter().map(Value::EntityRef).collect());
                self.set_reg(dst, result);
            }

            OpCode::Descendants {
                dst,
                start,
                relation,
                max_depth,
            } => {
                let start_id = self.as_entity(self.get_reg(start))?;
                let rel_id = self.as_relation_type(self.get_reg(relation))?;
                let depth = self.as_int(self.get_reg(max_depth))? as usize;

                let traversal = self.world.descendants_all(start_id, rel_id, depth);
                let result = Value::list(
                    traversal
                        .entities
                        .into_iter()
                        .map(Value::EntityRef)
                        .collect(),
                );
                self.set_reg(dst, result);
            }

            OpCode::Ancestors {
                dst,
                start,
                relation,
                max_depth,
            } => {
                let start_id = self.as_entity(self.get_reg(start))?;
                let rel_id = self.as_relation_type(self.get_reg(relation))?;
                let depth = self.as_int(self.get_reg(max_depth))? as usize;

                let traversal = self.world.ancestors(start_id, rel_id, depth);
                let result = Value::list(
                    traversal
                        .entities
                        .into_iter()
                        .map(Value::EntityRef)
                        .collect(),
                );
                self.set_reg(dst, result);
            }

            // === Random Number Generation ===
            OpCode::Random { dst } => {
                let rng = self.rng.as_mut().ok_or(VMError::NoRng)?;
                let result = Value::Float(rng.next_f64());
                self.set_reg(dst, result);
            }

            OpCode::RandomRange { dst, min, max } => {
                // Get values first before borrowing rng mutably
                let min_val = self.as_int(self.get_reg(min))?;
                let max_val = self.as_int(self.get_reg(max))?;
                let rng = self.rng.as_mut().ok_or(VMError::NoRng)?;
                let result = Value::Int(rng.range(min_val, max_val));
                self.set_reg(dst, result);
            }

            // === Return ===
            OpCode::Return { src } => {
                return Ok(ControlFlow::Return(self.get_reg(src).clone()));
            }

            OpCode::ReturnNil => {
                return Ok(ControlFlow::Return(NIL.clone()));
            }

            // === Action Context ===
            OpCode::GetContextActor { dst } => {
                let ctx = self
                    .action_context
                    .as_ref()
                    .ok_or(VMError::NoActionContext)?;
                self.set_reg(dst, Value::EntityRef(ctx.actor));
            }

            OpCode::GetContextDirectObject { dst } => {
                let ctx = self
                    .action_context
                    .as_ref()
                    .ok_or(VMError::NoActionContext)?;
                let result = ctx
                    .direct_object
                    .map(Value::EntityRef)
                    .unwrap_or_else(|| NIL.clone());
                self.set_reg(dst, result);
            }

            OpCode::GetContextIndirectObject { dst } => {
                let ctx = self
                    .action_context
                    .as_ref()
                    .ok_or(VMError::NoActionContext)?;
                let result = ctx
                    .indirect_object
                    .map(Value::EntityRef)
                    .unwrap_or_else(|| NIL.clone());
                self.set_reg(dst, result);
            }

            OpCode::GetContextRoom { dst } => {
                let ctx = self
                    .action_context
                    .as_ref()
                    .ok_or(VMError::NoActionContext)?;
                let result = ctx
                    .room
                    .map(Value::EntityRef)
                    .unwrap_or_else(|| NIL.clone());
                self.set_reg(dst, result);
            }

            // === Effects ===
            OpCode::Say { message } => {
                let msg = self.get_reg(message);
                let text = match msg {
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                };
                self.output_buffer.push(text);
            }

            OpCode::Destroy { entity } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                self.pending_deletions.push(entity_id);
            }

            // === World Mutations ===
            OpCode::SetComponent {
                entity,
                component,
                value,
            } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                let component_id = self.as_component_type(self.get_reg(component))?;
                let val = self.get_reg(value).clone();
                self.pending_set_components.push(PendingSetComponent {
                    entity: entity_id,
                    component: component_id,
                    value: val,
                });
            }

            OpCode::Relate { relation, from, to } => {
                let rel_id = self.as_relation_type(self.get_reg(relation))?;
                let from_id = self.as_entity(self.get_reg(from))?;
                let to_id = self.as_entity(self.get_reg(to))?;
                self.pending_relate.push(PendingRelation {
                    relation: rel_id,
                    from: from_id,
                    to: to_id,
                });
            }

            OpCode::Unrelate { relation, from, to } => {
                let rel_id = self.as_relation_type(self.get_reg(relation))?;
                let from_id = self.as_entity(self.get_reg(from))?;
                let to_id = self.as_entity(self.get_reg(to))?;
                self.pending_unrelate.push(PendingRelation {
                    relation: rel_id,
                    from: from_id,
                    to: to_id,
                });
            }

            // === World Queries (containment) ===
            OpCode::GetHolder { dst, entity } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                // Query reverse Contains relation to find holder
                let rel_id = RelationTypeId::new("Contains");
                let holders = self.world.query_relation_reverse(rel_id, entity_id);
                let result = holders
                    .first()
                    .map(|id| Value::EntityRef(*id))
                    .unwrap_or_else(|| NIL.clone());
                self.set_reg(dst, result);
            }

            OpCode::GetContents { dst, container } => {
                let container_id = self.as_entity(self.get_reg(container))?;
                let rel_id = RelationTypeId::new("Contains");
                let contents = self.world.query_relation_forward(rel_id, container_id);
                let result = Value::list(contents.into_iter().map(Value::EntityRef).collect());
                self.set_reg(dst, result);
            }

            OpCode::GetExits { dst, room } => {
                let room_id = self.as_entity(self.get_reg(room))?;
                // Exits are reified entities: Room --Exit--> ExitEntity
                // Each exit entity has a Direction component
                let exit_rel = RelationTypeId::new("Exit");
                let exit_entities = self.world.query_relation_forward(exit_rel, room_id);

                let mut directions = Vec::new();
                for exit_entity in exit_entities {
                    if let Some(dir) = self.world.get_component(exit_entity, "Direction") {
                        directions.push(dir.clone());
                    }
                }
                self.set_reg(dst, Value::list(directions));
            }

            OpCode::GetExitTarget {
                dst,
                room,
                direction,
            } => {
                let room_id = self.as_entity(self.get_reg(room))?;
                let dir_value = self.get_reg(direction).clone();

                // Find the exit entity with matching Direction
                let exit_rel = RelationTypeId::new("Exit");
                let dest_rel = RelationTypeId::new("Destination");
                let exit_entities = self.world.query_relation_forward(exit_rel, room_id);

                let mut result = NIL.clone();
                for exit_entity in exit_entities {
                    if let Some(exit_dir) = self.world.get_component(exit_entity, "Direction") {
                        if *exit_dir == dir_value {
                            // Found matching exit, get destination
                            let targets = self.world.query_relation_forward(dest_rel, exit_entity);
                            if let Some(&target) = targets.first() {
                                result = Value::EntityRef(target);
                            }
                            break;
                        }
                    }
                }
                self.set_reg(dst, result);
            }

            // === Predicates ===
            OpCode::GetRoom { dst, entity } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                let rel_id = RelationTypeId::new("InRoom");
                let rooms = self.world.query_relation_forward(rel_id, entity_id);
                let result = rooms
                    .first()
                    .map(|id| Value::EntityRef(*id))
                    .unwrap_or_else(|| NIL.clone());
                self.set_reg(dst, result);
            }

            OpCode::InScope { dst, actor, target } => {
                let actor_id = self.as_entity(self.get_reg(actor))?;
                let target_id = self.as_entity(self.get_reg(target))?;
                let result = self.world.is_in_scope(actor_id, target_id);
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::IsHeldBy { dst, item, holder } => {
                let item_id = self.as_entity(self.get_reg(item))?;
                let holder_id = self.as_entity(self.get_reg(holder))?;
                let result = self.world.is_held_by(item_id, holder_id);
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::IsPortable { dst, entity } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                let result = self.world.is_portable(entity_id);
                self.set_reg(dst, Value::Bool(result));
            }

            OpCode::The { dst, entity } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                let result = self.format_the(entity_id);
                self.set_reg(dst, Value::string(result));
            }

            OpCode::A { dst, entity } => {
                let entity_id = self.as_entity(self.get_reg(entity))?;
                let result = self.format_a(entity_id);
                self.set_reg(dst, Value::string(result));
            }
        }

        Ok(ControlFlow::Continue)
    }

    // === Helper methods ===

    fn get_reg(&self, reg: u8) -> &Value {
        &self.registers[reg as usize]
    }

    fn set_reg(&mut self, reg: u8, value: Value) {
        self.registers[reg as usize] = value;
    }

    fn is_truthy(&self, value: &Value) -> bool {
        !matches!(value, Value::Bool(false) | Value::Int(0))
    }

    fn binary_arith<I, F>(&self, a: u8, b: u8, int_op: I, float_op: F) -> Result<Value, VMError>
    where
        I: Fn(i64, i64) -> i64,
        F: Fn(f64, f64) -> f64,
    {
        let av = self.get_reg(a);
        let bv = self.get_reg(b);

        match (av, bv) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(int_op(*x, *y))),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(float_op(*x, *y))),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(float_op(*x as f64, *y))),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(float_op(*x, *y as f64))),
            (a, b) => Err(VMError::TypeError {
                expected: "numbers",
                got: Box::leak(format!("{} and {}", a.type_name(), b.type_name()).into_boxed_str()),
            }),
        }
    }

    fn compare<P>(&self, a: u8, b: u8, pred: P) -> Result<bool, VMError>
    where
        P: Fn(std::cmp::Ordering) -> bool,
    {
        let av = self.get_reg(a);
        let bv = self.get_reg(b);

        // For numeric comparisons, coerce to common type
        let ordering = match (av, bv) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::Float(x), Value::Float(y)) => {
                x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::Int(x), Value::Float(y)) => (*x as f64)
                .partial_cmp(y)
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Float(x), Value::Int(y)) => x
                .partial_cmp(&(*y as f64))
                .unwrap_or(std::cmp::Ordering::Equal),
            // For other types, use Value's Ord implementation
            (a, b) => a.cmp(b),
        };

        Ok(pred(ordering))
    }

    fn as_entity(&self, value: &Value) -> Result<EntityId, VMError> {
        match value {
            Value::EntityRef(id) => Ok(*id),
            other => Err(VMError::TypeError {
                expected: "entity",
                got: other.type_name(),
            }),
        }
    }

    fn as_component_type(&self, value: &Value) -> Result<ComponentTypeId, VMError> {
        match value {
            Value::Symbol(s) => Ok(ComponentTypeId(*s)),
            other => Err(VMError::TypeError {
                expected: "symbol (component type)",
                got: other.type_name(),
            }),
        }
    }

    fn as_relation_type(&self, value: &Value) -> Result<RelationTypeId, VMError> {
        match value {
            Value::Symbol(s) => Ok(RelationTypeId(*s)),
            other => Err(VMError::TypeError {
                expected: "symbol (relation type)",
                got: other.type_name(),
            }),
        }
    }

    fn as_int(&self, value: &Value) -> Result<i64, VMError> {
        match value {
            Value::Int(n) => Ok(*n),
            other => Err(VMError::TypeError {
                expected: "int",
                got: other.type_name(),
            }),
        }
    }

    // === Article generation helpers ===

    /// Get the name of an entity, or "something" if no Name component.
    fn get_entity_name(&self, entity: EntityId) -> String {
        self.world
            .get_component(entity, "Name")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| "something".to_string())
    }

    /// Check if entity has a boolean component set to true.
    fn has_flag(&self, entity: EntityId, component: &str) -> bool {
        self.world
            .get_component(entity, component)
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
    }

    /// Format entity with definite article: "the lamp", "Bob" (proper noun)
    fn format_the(&self, entity: EntityId) -> String {
        let name = self.get_entity_name(entity);

        // Proper nouns and no-article entities get no article
        if self.has_flag(entity, "ProperNoun") || self.has_flag(entity, "NoArticle") {
            return name;
        }

        format!("the {name}")
    }

    /// Format entity with indefinite article: "a lamp", "an apple", "Bob" (proper noun)
    fn format_a(&self, entity: EntityId) -> String {
        let name = self.get_entity_name(entity);

        // Proper nouns and no-article entities get no article
        if self.has_flag(entity, "ProperNoun") || self.has_flag(entity, "NoArticle") {
            return name;
        }

        // Check VowelSound flag first (explicit override)
        let use_an = if self.has_flag(entity, "VowelSound") {
            true
        } else {
            // Fall back to heuristic: check if name starts with a vowel
            name.chars()
                .next()
                .map(|c| matches!(c.to_ascii_lowercase(), 'a' | 'e' | 'i' | 'o' | 'u'))
                .unwrap_or(false)
        };

        if use_an {
            format!("an {name}")
        } else {
            format!("a {name}")
        }
    }
}

/// Control flow after executing an instruction.
enum ControlFlow {
    Continue,
    Return(Value),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;

    fn run_chunk(chunk: &Chunk) -> Result<Value, VMError> {
        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(chunk, &world, &stdlib);
        vm.run()
    }

    #[test]
    fn test_load_const_and_return() {
        let mut chunk = Chunk::new();
        let c = chunk.add_constant(Value::Int(42));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(42));
    }

    #[test]
    fn test_load_bool() {
        let mut chunk = Chunk::new();
        chunk.emit(
            OpCode::LoadBool {
                dst: 0,
                value: true,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 0 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_arithmetic_add() {
        let mut chunk = Chunk::new();
        let c1 = chunk.add_constant(Value::Int(10));
        let c2 = chunk.add_constant(Value::Int(20));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c1 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c2 }, 1);
        chunk.emit(OpCode::Add { dst: 2, a: 0, b: 1 }, 1);
        chunk.emit(OpCode::Return { src: 2 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(30));
    }

    #[test]
    fn test_arithmetic_mul() {
        let mut chunk = Chunk::new();
        let c1 = chunk.add_constant(Value::Int(6));
        let c2 = chunk.add_constant(Value::Int(7));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c1 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c2 }, 1);
        chunk.emit(OpCode::Mul { dst: 2, a: 0, b: 1 }, 1);
        chunk.emit(OpCode::Return { src: 2 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(42));
    }

    #[test]
    fn test_mixed_arithmetic() {
        // (10 + 20) * 2 = 60
        let mut chunk = Chunk::new();
        let c10 = chunk.add_constant(Value::Int(10));
        let c20 = chunk.add_constant(Value::Int(20));
        let c2 = chunk.add_constant(Value::Int(2));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c10 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c20 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 2, idx: c2 }, 1);
        chunk.emit(OpCode::Add { dst: 3, a: 0, b: 1 }, 1);
        chunk.emit(OpCode::Mul { dst: 4, a: 3, b: 2 }, 1);
        chunk.emit(OpCode::Return { src: 4 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(60));
    }

    #[test]
    fn test_float_arithmetic() {
        let mut chunk = Chunk::new();
        let c1 = chunk.add_constant(Value::Float(2.5));
        let c2 = chunk.add_constant(Value::Float(4.0));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c1 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c2 }, 1);
        chunk.emit(OpCode::Mul { dst: 2, a: 0, b: 1 }, 1);
        chunk.emit(OpCode::Return { src: 2 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Float(10.0));
    }

    #[test]
    fn test_negation() {
        let mut chunk = Chunk::new();
        let c = chunk.add_constant(Value::Int(42));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c }, 1);
        chunk.emit(OpCode::Neg { dst: 1, src: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(-42));
    }

    #[test]
    fn test_comparison_eq() {
        let mut chunk = Chunk::new();
        let c = chunk.add_constant(Value::Int(5));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c }, 1);
        chunk.emit(OpCode::Eq { dst: 2, a: 0, b: 1 }, 1);
        chunk.emit(OpCode::Return { src: 2 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_comparison_lt() {
        let mut chunk = Chunk::new();
        let c3 = chunk.add_constant(Value::Int(3));
        let c5 = chunk.add_constant(Value::Int(5));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c3 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c5 }, 1);
        chunk.emit(OpCode::Lt { dst: 2, a: 0, b: 1 }, 1);
        chunk.emit(OpCode::Return { src: 2 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_not() {
        let mut chunk = Chunk::new();
        chunk.emit(
            OpCode::LoadBool {
                dst: 0,
                value: true,
            },
            1,
        );
        chunk.emit(OpCode::Not { dst: 1, src: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_jump() {
        // Jump over a load, return 1 instead of 99
        let mut chunk = Chunk::new();
        let c1 = chunk.add_constant(Value::Int(1));
        let c99 = chunk.add_constant(Value::Int(99));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c1 }, 1);
        chunk.emit(OpCode::Jump { offset: 1 }, 1); // Skip next instruction
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c99 }, 1); // Skipped
        chunk.emit(OpCode::Return { src: 0 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(1));
    }

    #[test]
    fn test_jump_if() {
        // if true, return 1; else return 2
        let mut chunk = Chunk::new();
        let c1 = chunk.add_constant(Value::Int(1));
        let c2 = chunk.add_constant(Value::Int(2));
        chunk.emit(
            OpCode::LoadBool {
                dst: 0,
                value: true,
            },
            1,
        );
        chunk.emit(OpCode::JumpIf { cond: 0, offset: 2 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c2 }, 1);
        chunk.emit(OpCode::Jump { offset: 1 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c1 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(1));
    }

    #[test]
    fn test_stdlib_call() {
        // Call abs(-5) -> 5
        let mut chunk = Chunk::new();
        let c_func = chunk.add_constant(Value::Symbol(Symbol::new("abs")));
        let c_arg = chunk.add_constant(Value::Int(-5));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c_arg }, 1);
        chunk.emit(
            OpCode::Call {
                dst: 1,
                func_idx: c_func,
                args: 0,
                nargs: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 1 }, 1);

        assert_eq!(run_chunk(&chunk).unwrap(), Value::Int(5));
    }

    #[test]
    fn test_division_by_zero() {
        let mut chunk = Chunk::new();
        let c1 = chunk.add_constant(Value::Int(10));
        let c0 = chunk.add_constant(Value::Int(0));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c1 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c0 }, 1);
        chunk.emit(OpCode::Div { dst: 2, a: 0, b: 1 }, 1);
        chunk.emit(OpCode::Return { src: 2 }, 1);

        assert!(matches!(run_chunk(&chunk), Err(VMError::DivisionByZero)));
    }

    #[test]
    fn test_get_component() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let mut chunk = Chunk::new();
        let c_entity = chunk.add_constant(Value::EntityRef(entity));
        let c_hp = chunk.add_constant(Value::Symbol(Symbol::new("HP")));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: c_entity,
            },
            1,
        );
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c_hp }, 1);
        chunk.emit(
            OpCode::GetComponent {
                dst: 2,
                entity: 0,
                component: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        assert_eq!(vm.run().unwrap(), Value::Int(100));
    }

    #[test]
    fn test_has_component() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let mut chunk = Chunk::new();
        let c_entity = chunk.add_constant(Value::EntityRef(entity));
        let c_hp = chunk.add_constant(Value::Symbol(Symbol::new("HP")));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: c_entity,
            },
            1,
        );
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c_hp }, 1);
        chunk.emit(
            OpCode::HasComponent {
                dst: 2,
                entity: 0,
                component: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        assert_eq!(vm.run().unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_random() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::Random { dst: 0 }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib).with_seed(12345);
        let result = vm.run().unwrap();

        // Should be a float in [0, 1)
        match result {
            Value::Float(f) => assert!((0.0..1.0).contains(&f)),
            _ => panic!("Expected float, got {result:?}"),
        }
    }

    #[test]
    fn test_random_determinism() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::Random { dst: 0 }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let world = World::new();
        let stdlib = StdLib::with_builtins();

        // Same seed should produce same result
        let mut vm1 = VM::new(&chunk, &world, &stdlib).with_seed(42);
        let mut vm2 = VM::new(&chunk, &world, &stdlib).with_seed(42);

        assert_eq!(vm1.run().unwrap(), vm2.run().unwrap());
    }

    #[test]
    fn test_random_range() {
        let mut chunk = Chunk::new();
        let c_min = chunk.add_constant(Value::Int(1));
        let c_max = chunk.add_constant(Value::Int(10));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c_min }, 1);
        chunk.emit(OpCode::LoadConst { dst: 1, idx: c_max }, 1);
        chunk.emit(
            OpCode::RandomRange {
                dst: 2,
                min: 0,
                max: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib).with_seed(12345);
        let result = vm.run().unwrap();

        // Should be an int in [1, 10]
        match result {
            Value::Int(n) => assert!((1..=10).contains(&n)),
            _ => panic!("Expected int, got {result:?}"),
        }
    }

    #[test]
    fn test_random_no_rng_error() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::Random { dst: 0 }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib); // No seed!

        assert!(matches!(vm.run(), Err(VMError::NoRng)));
    }

    // ============ Stage 5A: World Mutation Tests ============

    fn setup_world_with_relations() -> World {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();

        // Register standard relations
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

        world
    }

    #[test]
    fn test_set_component_pending() {
        let mut world = setup_world_with_relations();
        let entity = world.create_entity();

        let mut chunk = Chunk::new();
        let entity_idx = chunk.add_constant(Value::EntityRef(entity));
        let comp_idx = chunk.add_constant(Value::Symbol(Symbol::new("HP")));
        let value_idx = chunk.add_constant(Value::Int(100));

        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: entity_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: comp_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 2,
                idx: value_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::SetComponent {
                entity: 0,
                component: 1,
                value: 2,
            },
            1,
        );
        chunk.emit(OpCode::ReturnNil, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        vm.run().unwrap();

        // Check pending mutations
        let pending = vm.take_pending_set_components();
        assert_eq!(pending.len(), 1);
        assert_eq!(pending[0].entity, entity);
        assert_eq!(pending[0].value, Value::Int(100));
    }

    #[test]
    fn test_relate_pending() {
        let mut world = setup_world_with_relations();
        let room = world.create_entity();
        let player = world.create_entity();

        let mut chunk = Chunk::new();
        let rel_idx = chunk.add_constant(Value::Symbol(Symbol::new("InRoom")));
        let from_idx = chunk.add_constant(Value::EntityRef(player));
        let to_idx = chunk.add_constant(Value::EntityRef(room));

        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: rel_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: from_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 2,
                idx: to_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::Relate {
                relation: 0,
                from: 1,
                to: 2,
            },
            1,
        );
        chunk.emit(OpCode::ReturnNil, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        vm.run().unwrap();

        let pending = vm.take_pending_relate();
        assert_eq!(pending.len(), 1);
        assert_eq!(pending[0].from, player);
        assert_eq!(pending[0].to, room);
    }

    // ============ Stage 5A: World Query Tests ============

    #[test]
    fn test_get_holder() {
        let mut world = setup_world_with_relations();
        let player = world.create_entity();
        let lamp = world.create_entity();
        world.add_relation("Contains", player, lamp);

        let mut chunk = Chunk::new();
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(OpCode::GetHolder { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::EntityRef(player));
    }

    #[test]
    fn test_get_holder_nil_when_not_held() {
        let mut world = setup_world_with_relations();
        let lamp = world.create_entity();

        let mut chunk = Chunk::new();
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(OpCode::GetHolder { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        // NIL is represented as Bool(false)
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_get_contents() {
        let mut world = setup_world_with_relations();
        let player = world.create_entity();
        let lamp = world.create_entity();
        let key = world.create_entity();
        world.add_relation("Contains", player, lamp);
        world.add_relation("Contains", player, key);

        let mut chunk = Chunk::new();
        let player_idx = chunk.add_constant(Value::EntityRef(player));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: player_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::GetContents {
                dst: 1,
                container: 0,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                assert!(items.contains(&Value::EntityRef(lamp)));
                assert!(items.contains(&Value::EntityRef(key)));
            }
            _ => panic!("Expected list, got {result:?}"),
        }
    }

    #[test]
    fn test_get_room() {
        let mut world = setup_world_with_relations();
        let room = world.create_entity();
        let player = world.create_entity();
        world.add_relation("InRoom", player, room);

        let mut chunk = Chunk::new();
        let player_idx = chunk.add_constant(Value::EntityRef(player));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: player_idx,
            },
            1,
        );
        chunk.emit(OpCode::GetRoom { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::EntityRef(room));
    }

    // ============ Stage 5A: Predicate Tests ============

    #[test]
    fn test_is_held_by_true() {
        let mut world = setup_world_with_relations();
        let player = world.create_entity();
        let lamp = world.create_entity();
        world.add_relation("Contains", player, lamp);

        let mut chunk = Chunk::new();
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        let player_idx = chunk.add_constant(Value::EntityRef(player));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: player_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::IsHeldBy {
                dst: 2,
                item: 0,
                holder: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_is_held_by_false() {
        let mut world = setup_world_with_relations();
        let player = world.create_entity();
        let lamp = world.create_entity();
        // Not held - no Contains relation

        let mut chunk = Chunk::new();
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        let player_idx = chunk.add_constant(Value::EntityRef(player));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: player_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::IsHeldBy {
                dst: 2,
                item: 0,
                holder: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_is_portable_true() {
        let mut world = setup_world_with_relations();
        let lamp = world.create_entity();
        world.set_component(lamp, "Portable", Value::Bool(true));

        let mut chunk = Chunk::new();
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(OpCode::IsPortable { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_is_portable_false_when_fixed() {
        let mut world = setup_world_with_relations();
        let statue = world.create_entity();
        world.set_component(statue, "Fixed", Value::Bool(true));

        let mut chunk = Chunk::new();
        let statue_idx = chunk.add_constant(Value::EntityRef(statue));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: statue_idx,
            },
            1,
        );
        chunk.emit(OpCode::IsPortable { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_in_scope_same_room() {
        let mut world = setup_world_with_relations();
        let room = world.create_entity();
        let player = world.create_entity();
        let lamp = world.create_entity();
        world.add_relation("InRoom", player, room);
        world.add_relation("InRoom", lamp, room);

        let mut chunk = Chunk::new();
        let player_idx = chunk.add_constant(Value::EntityRef(player));
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: player_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::InScope {
                dst: 2,
                actor: 0,
                target: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_in_scope_carried() {
        let mut world = setup_world_with_relations();
        let room = world.create_entity();
        let player = world.create_entity();
        let lamp = world.create_entity();
        world.add_relation("InRoom", player, room);
        world.add_relation("Contains", player, lamp);

        let mut chunk = Chunk::new();
        let player_idx = chunk.add_constant(Value::EntityRef(player));
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: player_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::InScope {
                dst: 2,
                actor: 0,
                target: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_in_scope_different_room() {
        let mut world = setup_world_with_relations();
        let room1 = world.create_entity();
        let room2 = world.create_entity();
        let player = world.create_entity();
        let lamp = world.create_entity();
        world.add_relation("InRoom", player, room1);
        world.add_relation("InRoom", lamp, room2);

        let mut chunk = Chunk::new();
        let player_idx = chunk.add_constant(Value::EntityRef(player));
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: player_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(
            OpCode::InScope {
                dst: 2,
                actor: 0,
                target: 1,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 2 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::Bool(false));
    }

    // === Article Generation Tests ===

    #[test]
    fn test_the_basic() {
        let mut world = World::new();
        let lamp = world.create_entity();
        world.set_component(lamp, "Name", Value::string("brass lamp"));

        let mut chunk = Chunk::new();
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(OpCode::The { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("the brass lamp"));
    }

    #[test]
    fn test_the_proper_noun() {
        let mut world = World::new();
        let bob = world.create_entity();
        world.set_component(bob, "Name", Value::string("Bob"));
        world.set_component(bob, "ProperNoun", Value::Bool(true));

        let mut chunk = Chunk::new();
        let bob_idx = chunk.add_constant(Value::EntityRef(bob));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: bob_idx,
            },
            1,
        );
        chunk.emit(OpCode::The { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("Bob"));
    }

    #[test]
    fn test_the_no_article() {
        let mut world = World::new();
        let you = world.create_entity();
        world.set_component(you, "Name", Value::string("you"));
        world.set_component(you, "NoArticle", Value::Bool(true));

        let mut chunk = Chunk::new();
        let you_idx = chunk.add_constant(Value::EntityRef(you));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: you_idx,
            },
            1,
        );
        chunk.emit(OpCode::The { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("you"));
    }

    #[test]
    fn test_the_no_name() {
        let mut world = World::new();
        let entity = world.create_entity();
        // No Name component

        let mut chunk = Chunk::new();
        let entity_idx = chunk.add_constant(Value::EntityRef(entity));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: entity_idx,
            },
            1,
        );
        chunk.emit(OpCode::The { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("the something"));
    }

    #[test]
    fn test_a_basic() {
        let mut world = World::new();
        let lamp = world.create_entity();
        world.set_component(lamp, "Name", Value::string("lamp"));

        let mut chunk = Chunk::new();
        let lamp_idx = chunk.add_constant(Value::EntityRef(lamp));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: lamp_idx,
            },
            1,
        );
        chunk.emit(OpCode::A { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("a lamp"));
    }

    #[test]
    fn test_a_vowel_heuristic() {
        let mut world = World::new();
        let apple = world.create_entity();
        world.set_component(apple, "Name", Value::string("apple"));
        // No VowelSound flag - should use heuristic

        let mut chunk = Chunk::new();
        let apple_idx = chunk.add_constant(Value::EntityRef(apple));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: apple_idx,
            },
            1,
        );
        chunk.emit(OpCode::A { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("an apple"));
    }

    #[test]
    fn test_a_vowel_flag_override() {
        let mut world = World::new();
        // "hour" starts with consonant but has vowel sound
        let hour = world.create_entity();
        world.set_component(hour, "Name", Value::string("hour"));
        world.set_component(hour, "VowelSound", Value::Bool(true));

        let mut chunk = Chunk::new();
        let hour_idx = chunk.add_constant(Value::EntityRef(hour));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: hour_idx,
            },
            1,
        );
        chunk.emit(OpCode::A { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("an hour"));
    }

    #[test]
    fn test_a_proper_noun() {
        let mut world = World::new();
        let bob = world.create_entity();
        world.set_component(bob, "Name", Value::string("Bob"));
        world.set_component(bob, "ProperNoun", Value::Bool(true));

        let mut chunk = Chunk::new();
        let bob_idx = chunk.add_constant(Value::EntityRef(bob));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: bob_idx,
            },
            1,
        );
        chunk.emit(OpCode::A { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("Bob"));
    }

    #[test]
    fn test_a_no_article() {
        let mut world = World::new();
        let you = world.create_entity();
        world.set_component(you, "Name", Value::string("you"));
        world.set_component(you, "NoArticle", Value::Bool(true));

        let mut chunk = Chunk::new();
        let you_idx = chunk.add_constant(Value::EntityRef(you));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: you_idx,
            },
            1,
        );
        chunk.emit(OpCode::A { dst: 1, entity: 0 }, 1);
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        let result = vm.run().unwrap();

        assert_eq!(result, Value::string("you"));
    }
}
