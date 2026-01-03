//! Bytecode instruction set for the register-based VM.
//!
//! The VM uses 256 registers (indexed by u8) and a constant pool.
//! Instructions operate on registers and produce results in registers.

/// Register index (0-255).
pub type Reg = u8;

/// Constant pool index.
pub type ConstIdx = u16;

/// Bytecode instructions.
///
/// Register-based design (like Lua 5+) for efficiency:
/// - Fewer instructions than stack-based VMs
/// - Better cache locality
/// - Direct register addressing
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCode {
    // === Load/Store ===
    /// Load a constant into a register: R[dst] = K[idx]
    LoadConst { dst: Reg, idx: ConstIdx },

    /// Load nil (unit value) into a register: R[dst] = nil
    LoadNil { dst: Reg },

    /// Load a boolean into a register: R[dst] = value
    LoadBool { dst: Reg, value: bool },

    /// Copy a register: R[dst] = R[src]
    Move { dst: Reg, src: Reg },

    // === Arithmetic ===
    /// Addition: R[dst] = R[a] + R[b]
    Add { dst: Reg, a: Reg, b: Reg },

    /// Subtraction: R[dst] = R[a] - R[b]
    Sub { dst: Reg, a: Reg, b: Reg },

    /// Multiplication: R[dst] = R[a] * R[b]
    Mul { dst: Reg, a: Reg, b: Reg },

    /// Division: R[dst] = R[a] / R[b]
    Div { dst: Reg, a: Reg, b: Reg },

    /// Modulo: R[dst] = R[a] % R[b]
    Mod { dst: Reg, a: Reg, b: Reg },

    /// Negation: R[dst] = -R[src]
    Neg { dst: Reg, src: Reg },

    // === Comparison ===
    /// Equality: R[dst] = R[a] == R[b]
    Eq { dst: Reg, a: Reg, b: Reg },

    /// Not equal: R[dst] = R[a] != R[b]
    Ne { dst: Reg, a: Reg, b: Reg },

    /// Less than: R[dst] = R[a] < R[b]
    Lt { dst: Reg, a: Reg, b: Reg },

    /// Less than or equal: R[dst] = R[a] <= R[b]
    Le { dst: Reg, a: Reg, b: Reg },

    /// Greater than: R[dst] = R[a] > R[b]
    Gt { dst: Reg, a: Reg, b: Reg },

    /// Greater than or equal: R[dst] = R[a] >= R[b]
    Ge { dst: Reg, a: Reg, b: Reg },

    // === Logic ===
    /// Logical not: R[dst] = !R[src]
    Not { dst: Reg, src: Reg },

    /// Logical and (short-circuit): R[dst] = R[a] && R[b]
    And { dst: Reg, a: Reg, b: Reg },

    /// Logical or (short-circuit): R[dst] = R[a] || R[b]
    Or { dst: Reg, a: Reg, b: Reg },

    // === Control Flow ===
    /// Unconditional jump: PC += offset
    Jump { offset: i16 },

    /// Conditional jump if true: if R[cond] then PC += offset
    JumpIf { cond: Reg, offset: i16 },

    /// Conditional jump if false: if !R[cond] then PC += offset
    JumpIfNot { cond: Reg, offset: i16 },

    // === Function Calls ===
    /// Call a standard library function:
    /// R[dst] = stdlib[func_idx](R[args], R[args+1], ..., R[args+nargs-1])
    Call {
        dst: Reg,
        func_idx: ConstIdx,
        args: Reg,
        nargs: u8,
    },

    // === World Access (syscalls) ===
    /// Get a component value: R[dst] = world.get_component(R[entity], R[component])
    GetComponent {
        dst: Reg,
        entity: Reg,
        component: Reg,
    },

    /// Check if entity has component: R[dst] = world.has_component(R[entity], R[component])
    HasComponent {
        dst: Reg,
        entity: Reg,
        component: Reg,
    },

    /// Query forward relation: R[dst] = world.query_relation_forward(R[rel], R[from])
    QueryRelation { dst: Reg, rel: Reg, from: Reg },

    /// Query descendants: R[dst] = world.descendants_all(R[start], R[relation], R[max_depth])
    /// Returns a list of entity refs
    Descendants {
        dst: Reg,
        start: Reg,
        relation: Reg,
        max_depth: Reg,
    },

    /// Query ancestors: R[dst] = world.ancestors(R[start], R[relation], R[max_depth])
    /// Returns a list of entity refs
    Ancestors {
        dst: Reg,
        start: Reg,
        relation: Reg,
        max_depth: Reg,
    },

    // === Random Number Generation ===
    /// Generate a random float in [0, 1): R[dst] = rng.next_f64()
    Random { dst: Reg },

    /// Generate a random integer in [R[min], R[max]]: R[dst] = rng.range(R[min], R[max])
    RandomRange { dst: Reg, min: Reg, max: Reg },

    // === Return ===
    /// Return a value: return R[src]
    Return { src: Reg },

    /// Return nil: return nil
    ReturnNil,

    // === Hook Context ===
    /// Get the actor from hook context: R[dst] = hook_context.actor
    GetHookActor { dst: Reg },

    /// Get the direct object from hook context: R[dst] = hook_context.direct_object (or nil)
    GetHookDirectObject { dst: Reg },

    /// Get the indirect object from hook context: R[dst] = hook_context.indirect_object (or nil)
    GetHookIndirectObject { dst: Reg },

    /// Get the room from hook context: R[dst] = hook_context.room (or nil)
    GetHookRoom { dst: Reg },

    // === Effects ===
    /// Output a message: append R[message] to output buffer
    Say { message: Reg },

    /// Mark an entity for destruction: pending_deletions.push(R[entity])
    Destroy { entity: Reg },
}

impl OpCode {
    /// Get a human-readable name for this opcode.
    pub fn name(&self) -> &'static str {
        match self {
            OpCode::LoadConst { .. } => "LOAD_CONST",
            OpCode::LoadNil { .. } => "LOAD_NIL",
            OpCode::LoadBool { .. } => "LOAD_BOOL",
            OpCode::Move { .. } => "MOVE",
            OpCode::Add { .. } => "ADD",
            OpCode::Sub { .. } => "SUB",
            OpCode::Mul { .. } => "MUL",
            OpCode::Div { .. } => "DIV",
            OpCode::Mod { .. } => "MOD",
            OpCode::Neg { .. } => "NEG",
            OpCode::Eq { .. } => "EQ",
            OpCode::Ne { .. } => "NE",
            OpCode::Lt { .. } => "LT",
            OpCode::Le { .. } => "LE",
            OpCode::Gt { .. } => "GT",
            OpCode::Ge { .. } => "GE",
            OpCode::Not { .. } => "NOT",
            OpCode::And { .. } => "AND",
            OpCode::Or { .. } => "OR",
            OpCode::Jump { .. } => "JUMP",
            OpCode::JumpIf { .. } => "JUMP_IF",
            OpCode::JumpIfNot { .. } => "JUMP_IF_NOT",
            OpCode::Call { .. } => "CALL",
            OpCode::GetComponent { .. } => "GET_COMPONENT",
            OpCode::HasComponent { .. } => "HAS_COMPONENT",
            OpCode::QueryRelation { .. } => "QUERY_RELATION",
            OpCode::Descendants { .. } => "DESCENDANTS",
            OpCode::Ancestors { .. } => "ANCESTORS",
            OpCode::Random { .. } => "RANDOM",
            OpCode::RandomRange { .. } => "RANDOM_RANGE",
            OpCode::Return { .. } => "RETURN",
            OpCode::ReturnNil => "RETURN_NIL",
            OpCode::GetHookActor { .. } => "GET_HOOK_ACTOR",
            OpCode::GetHookDirectObject { .. } => "GET_HOOK_DIRECT_OBJECT",
            OpCode::GetHookIndirectObject { .. } => "GET_HOOK_INDIRECT_OBJECT",
            OpCode::GetHookRoom { .. } => "GET_HOOK_ROOM",
            OpCode::Say { .. } => "SAY",
            OpCode::Destroy { .. } => "DESTROY",
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::LoadConst { dst, idx } => write!(f, "LOAD_CONST r{dst} k{idx}"),
            OpCode::LoadNil { dst } => write!(f, "LOAD_NIL r{dst}"),
            OpCode::LoadBool { dst, value } => write!(f, "LOAD_BOOL r{dst} {value}"),
            OpCode::Move { dst, src } => write!(f, "MOVE r{dst} r{src}"),
            OpCode::Add { dst, a, b } => write!(f, "ADD r{dst} r{a} r{b}"),
            OpCode::Sub { dst, a, b } => write!(f, "SUB r{dst} r{a} r{b}"),
            OpCode::Mul { dst, a, b } => write!(f, "MUL r{dst} r{a} r{b}"),
            OpCode::Div { dst, a, b } => write!(f, "DIV r{dst} r{a} r{b}"),
            OpCode::Mod { dst, a, b } => write!(f, "MOD r{dst} r{a} r{b}"),
            OpCode::Neg { dst, src } => write!(f, "NEG r{dst} r{src}"),
            OpCode::Eq { dst, a, b } => write!(f, "EQ r{dst} r{a} r{b}"),
            OpCode::Ne { dst, a, b } => write!(f, "NE r{dst} r{a} r{b}"),
            OpCode::Lt { dst, a, b } => write!(f, "LT r{dst} r{a} r{b}"),
            OpCode::Le { dst, a, b } => write!(f, "LE r{dst} r{a} r{b}"),
            OpCode::Gt { dst, a, b } => write!(f, "GT r{dst} r{a} r{b}"),
            OpCode::Ge { dst, a, b } => write!(f, "GE r{dst} r{a} r{b}"),
            OpCode::Not { dst, src } => write!(f, "NOT r{dst} r{src}"),
            OpCode::And { dst, a, b } => write!(f, "AND r{dst} r{a} r{b}"),
            OpCode::Or { dst, a, b } => write!(f, "OR r{dst} r{a} r{b}"),
            OpCode::Jump { offset } => write!(f, "JUMP {offset:+}"),
            OpCode::JumpIf { cond, offset } => write!(f, "JUMP_IF r{cond} {offset:+}"),
            OpCode::JumpIfNot { cond, offset } => write!(f, "JUMP_IF_NOT r{cond} {offset:+}"),
            OpCode::Call {
                dst,
                func_idx,
                args,
                nargs,
            } => write!(f, "CALL r{dst} k{func_idx} r{args} {nargs}"),
            OpCode::GetComponent {
                dst,
                entity,
                component,
            } => write!(f, "GET_COMPONENT r{dst} r{entity} r{component}"),
            OpCode::HasComponent {
                dst,
                entity,
                component,
            } => write!(f, "HAS_COMPONENT r{dst} r{entity} r{component}"),
            OpCode::QueryRelation { dst, rel, from } => {
                write!(f, "QUERY_RELATION r{dst} r{rel} r{from}")
            }
            OpCode::Descendants {
                dst,
                start,
                relation,
                max_depth,
            } => {
                write!(f, "DESCENDANTS r{dst} r{start} r{relation} r{max_depth}")
            }
            OpCode::Ancestors {
                dst,
                start,
                relation,
                max_depth,
            } => {
                write!(f, "ANCESTORS r{dst} r{start} r{relation} r{max_depth}")
            }
            OpCode::Random { dst } => write!(f, "RANDOM r{dst}"),
            OpCode::RandomRange { dst, min, max } => {
                write!(f, "RANDOM_RANGE r{dst} r{min} r{max}")
            }
            OpCode::Return { src } => write!(f, "RETURN r{src}"),
            OpCode::ReturnNil => write!(f, "RETURN_NIL"),
            OpCode::GetHookActor { dst } => write!(f, "GET_HOOK_ACTOR r{dst}"),
            OpCode::GetHookDirectObject { dst } => write!(f, "GET_HOOK_DIRECT_OBJECT r{dst}"),
            OpCode::GetHookIndirectObject { dst } => write!(f, "GET_HOOK_INDIRECT_OBJECT r{dst}"),
            OpCode::GetHookRoom { dst } => write!(f, "GET_HOOK_ROOM r{dst}"),
            OpCode::Say { message } => write!(f, "SAY r{message}"),
            OpCode::Destroy { entity } => write!(f, "DESTROY r{entity}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opcode_name() {
        assert_eq!(OpCode::LoadConst { dst: 0, idx: 0 }.name(), "LOAD_CONST");
        assert_eq!(OpCode::Add { dst: 0, a: 1, b: 2 }.name(), "ADD");
        assert_eq!(OpCode::Return { src: 0 }.name(), "RETURN");
    }

    #[test]
    fn test_opcode_display() {
        assert_eq!(
            OpCode::LoadConst { dst: 0, idx: 5 }.to_string(),
            "LOAD_CONST r0 k5"
        );
        assert_eq!(
            OpCode::Add { dst: 2, a: 0, b: 1 }.to_string(),
            "ADD r2 r0 r1"
        );
        assert_eq!(OpCode::Jump { offset: -5 }.to_string(), "JUMP -5");
        assert_eq!(OpCode::Jump { offset: 10 }.to_string(), "JUMP +10");
    }

    #[test]
    fn test_opcode_size() {
        // Ensure OpCode stays reasonably sized
        assert!(std::mem::size_of::<OpCode>() <= 8);
    }
}
