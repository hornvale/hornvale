use std::collections::HashMap;
use std::sync::Mutex;

use crate::command::NoOpCommand;
use crate::command::_trait::Command;
use crate::command::_type::CommandConstructor;

/// The `Factory` struct, which represents a command factory.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Factory {
  /// The command map.
  #[derivative(Debug = "ignore")]
  command_map: Mutex<HashMap<String, CommandConstructor>>,
}

impl Factory {
  pub fn new() -> Self {
    Self {
      command_map: Mutex::new(HashMap::new()),
    }
  }

  /// Register a command.
  pub fn register_command<T: Command + 'static + Default>(&mut self) {
    let command = T::default();
    let name = command.get_name();
    let mut command_map = self.command_map.lock().unwrap();
    command_map.insert(name.to_string(), Box::new(|| Box::<T>::default() as Box<dyn Command>));
  }

  /// Create a new instance of a command.
  pub fn create_command(&self, name: &str) -> Option<Box<dyn Command>> {
    let command_map = self.command_map.lock().unwrap();
    command_map.get(name).map(|constructor| constructor())
  }
}

impl Default for Factory {
  fn default() -> Self {
    let mut result = Self::new();
    result.register_command::<NoOpCommand>();
    result
  }
}
