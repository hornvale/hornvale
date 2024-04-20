use crate::function::Function;
use crate::garbage_collection::collector::Collector as GarbageCollector;
use crate::garbage_collection::reference::Reference;
use crate::parser::Parser;
use crate::scanner::Scanner;
use derive_more::Display;
use serde::{Deserialize, Serialize};

/// The `Error` type.
pub mod error;
use error::Error;

/// The `Interpreter` type.
///
/// This corresponds to the `compile()` function.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Display, Hash, PartialEq, Serialize)]
pub struct Interpreter {}

impl Interpreter {
  /// Compile the source.
  pub fn compile(
    &mut self,
    source: &str,
    garbage_collector: &mut GarbageCollector,
  ) -> Result<Reference<Function>, Error> {
    let scanner = Scanner::new(source);
    let parser = Parser::new(scanner, garbage_collector);
    let result = parser.compile()?;
    Ok(result)
  }
}

#[cfg(test)]
pub mod test {

  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  pub fn test_interpreter() {
    init();
    let interpreter = Interpreter::default();
    println!("{} = {:#?}", stringify!(interpreter), interpreter);
  }
}
