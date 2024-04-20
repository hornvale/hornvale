use crate::virtual_machine::vm::VirtualMachineError;
use thiserror::Error;

/// Errors encountered in compiling or executing a script.
#[derive(Clone, Debug, Error)]
pub enum Error {
  /// A general error occurred.
  #[error("an error occurred ({0})")]
  GeneralError(#[from] Box<VirtualMachineError>),
}
