use super::runtime::RuntimeError;
use crate::interpreter::error::Error as InterpreterError;
use crate::native_function::error::Error as NativeFunctionError;
use thiserror::Error;

/// Errors encountered in compiling or executing a script.
#[derive(Clone, Debug, Error)]
pub enum VirtualMachineError {
  /// An interpreter error occurred.
  #[error("an interpreter error occurred ({0})")]
  InterpreterError(#[from] InterpreterError),
  /// A runtime error occurred.
  #[error("a runtime error occurred ({0})")]
  RuntimeError(#[from] RuntimeError),
  /// A native function error occurred.
  #[error("an error occurred in a native function ({0})")]
  NativeFunctionError(#[from] NativeFunctionError),
}
