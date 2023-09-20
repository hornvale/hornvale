use rustyline_async::Readline;
use specs::prelude::*;

pub mod input;
pub use input::Input as InputResource;
pub mod output;
pub use output::Output as OutputResource;

pub fn insert_resources(ecs: &mut World, seed: &str) {
  let (input, stdout) = Readline::new("> ".to_owned()).unwrap();
  ecs.insert(InputResource(Some(input)));
  ecs.insert(OutputResource(Some(stdout)));
}
