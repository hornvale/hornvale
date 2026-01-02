//! Bytecode chunk - container for compiled bytecode.

use super::bytecode::{ConstIdx, OpCode};
use crate::core::Value;

/// A compiled chunk of bytecode.
///
/// Contains:
/// - The bytecode instructions
/// - The constant pool (literals referenced by instructions)
/// - Debug information (source line numbers)
#[derive(Debug, Clone, Default)]
pub struct Chunk {
    /// Bytecode instructions.
    code: Vec<OpCode>,
    /// Constant pool.
    constants: Vec<Value>,
    /// Source line numbers (one per instruction, for error reporting).
    lines: Vec<u32>,
}

impl Chunk {
    /// Create a new empty chunk.
    pub fn new() -> Self {
        Self::default()
    }

    /// Emit an instruction and return its index.
    pub fn emit(&mut self, op: OpCode, line: u32) -> usize {
        let idx = self.code.len();
        self.code.push(op);
        self.lines.push(line);
        idx
    }

    /// Add a constant to the pool and return its index.
    ///
    /// Panics if the constant pool exceeds 65535 entries.
    pub fn add_constant(&mut self, value: Value) -> ConstIdx {
        let idx = self.constants.len();
        assert!(idx <= u16::MAX as usize, "constant pool overflow");
        self.constants.push(value);
        idx as ConstIdx
    }

    /// Get a constant by index.
    pub fn get_constant(&self, idx: ConstIdx) -> Option<&Value> {
        self.constants.get(idx as usize)
    }

    /// Get an instruction by index.
    pub fn get_instruction(&self, idx: usize) -> Option<OpCode> {
        self.code.get(idx).copied()
    }

    /// Get the source line for an instruction.
    pub fn get_line(&self, idx: usize) -> Option<u32> {
        self.lines.get(idx).copied()
    }

    /// Patch a jump instruction's offset.
    ///
    /// Used for forward jumps: emit a placeholder, then patch when target is known.
    pub fn patch_jump(&mut self, idx: usize, offset: i16) {
        match &mut self.code[idx] {
            OpCode::Jump { offset: o } => *o = offset,
            OpCode::JumpIf { offset: o, .. } => *o = offset,
            OpCode::JumpIfNot { offset: o, .. } => *o = offset,
            _ => panic!("patch_jump called on non-jump instruction"),
        }
    }

    /// Get the current code length (next instruction index).
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// Check if the chunk is empty.
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    /// Get the number of constants.
    pub fn constants_len(&self) -> usize {
        self.constants.len()
    }

    /// Iterate over instructions.
    pub fn instructions(&self) -> impl Iterator<Item = (usize, OpCode)> + '_ {
        self.code.iter().copied().enumerate()
    }

    /// Disassemble the chunk for debugging.
    pub fn disassemble(&self, name: &str) -> String {
        let mut output = format!("== {name} ==\n");
        for (i, op) in self.code.iter().enumerate() {
            let line = self.lines.get(i).copied().unwrap_or(0);
            output.push_str(&format!("{i:04} [{line:4}] {op}\n"));
        }
        if !self.constants.is_empty() {
            output.push_str("-- constants --\n");
            for (i, c) in self.constants.iter().enumerate() {
                output.push_str(&format!("  k{i}: {c:?}\n"));
            }
        }
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_chunk() {
        let chunk = Chunk::new();
        assert!(chunk.is_empty());
        assert_eq!(chunk.len(), 0);
        assert_eq!(chunk.constants_len(), 0);
    }

    #[test]
    fn test_emit_instruction() {
        let mut chunk = Chunk::new();
        let idx = chunk.emit(OpCode::LoadNil { dst: 0 }, 1);
        assert_eq!(idx, 0);
        assert_eq!(chunk.len(), 1);
        assert_eq!(chunk.get_instruction(0), Some(OpCode::LoadNil { dst: 0 }));
        assert_eq!(chunk.get_line(0), Some(1));
    }

    #[test]
    fn test_add_constant() {
        let mut chunk = Chunk::new();
        let idx1 = chunk.add_constant(Value::Int(42));
        let idx2 = chunk.add_constant(Value::string("hello"));

        assert_eq!(idx1, 0);
        assert_eq!(idx2, 1);
        assert_eq!(chunk.constants_len(), 2);
        assert_eq!(chunk.get_constant(0), Some(&Value::Int(42)));
        assert_eq!(chunk.get_constant(1), Some(&Value::string("hello")));
    }

    #[test]
    fn test_emit_with_constant() {
        let mut chunk = Chunk::new();
        let c = chunk.add_constant(Value::Int(100));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c }, 1);

        assert_eq!(chunk.len(), 1);
        assert_eq!(
            chunk.get_instruction(0),
            Some(OpCode::LoadConst { dst: 0, idx: 0 })
        );
    }

    #[test]
    fn test_patch_jump() {
        let mut chunk = Chunk::new();
        let jump_idx = chunk.emit(OpCode::Jump { offset: 0 }, 1);
        chunk.emit(OpCode::LoadNil { dst: 0 }, 2);
        chunk.emit(OpCode::LoadNil { dst: 1 }, 3);

        // Patch to skip 2 instructions
        chunk.patch_jump(jump_idx, 2);

        assert_eq!(
            chunk.get_instruction(jump_idx),
            Some(OpCode::Jump { offset: 2 })
        );
    }

    #[test]
    fn test_disassemble() {
        let mut chunk = Chunk::new();
        let c = chunk.add_constant(Value::Int(42));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: c }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let output = chunk.disassemble("test");
        assert!(output.contains("== test =="));
        assert!(output.contains("LOAD_CONST"));
        assert!(output.contains("RETURN"));
        assert!(output.contains("k0: Int(42)"));
    }

    #[test]
    fn test_instructions_iterator() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::LoadNil { dst: 0 }, 1);
        chunk.emit(OpCode::LoadNil { dst: 1 }, 2);

        let instructions: Vec<_> = chunk.instructions().collect();
        assert_eq!(instructions.len(), 2);
        assert_eq!(instructions[0], (0, OpCode::LoadNil { dst: 0 }));
        assert_eq!(instructions[1], (1, OpCode::LoadNil { dst: 1 }));
    }
}
