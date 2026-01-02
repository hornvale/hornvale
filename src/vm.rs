//! Bytecode virtual machine.
//!
//! This module provides a register-based bytecode VM for executing compiled
//! expressions. The design follows Lua 5+ for efficiency:
//!
//! - 256 registers (indexed by u8)
//! - Constant pool for literals
//! - Direct register addressing
//!
//! # Example
//!
//! ```
//! use hornvale::vm::{Chunk, OpCode, VM, StdLib};
//! use hornvale::core::{World, Value};
//!
//! // Create a chunk that computes 10 + 20
//! let mut chunk = Chunk::new();
//! let c10 = chunk.add_constant(Value::Int(10));
//! let c20 = chunk.add_constant(Value::Int(20));
//! chunk.emit(OpCode::LoadConst { dst: 0, idx: c10 }, 1);
//! chunk.emit(OpCode::LoadConst { dst: 1, idx: c20 }, 1);
//! chunk.emit(OpCode::Add { dst: 2, a: 0, b: 1 }, 1);
//! chunk.emit(OpCode::Return { src: 2 }, 1);
//!
//! // Execute it
//! let world = World::new();
//! let stdlib = StdLib::with_builtins();
//! let mut vm = VM::new(&chunk, &world, &stdlib);
//! assert_eq!(vm.run().unwrap(), Value::Int(30));
//! ```

mod bytecode;
mod chunk;
mod exec;
mod stdlib;

pub use bytecode::{ConstIdx, OpCode, Reg};
pub use chunk::Chunk;
pub use exec::{VM, VMError};
pub use stdlib::{StdFn, StdLib, StdLibError};
