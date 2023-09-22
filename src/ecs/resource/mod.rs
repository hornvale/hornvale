use rustyline_async::Readline;
use specs::prelude::*;

pub mod resources;
pub use resources::*;

pub fn insert_resources(ecs: &mut World, _seed: &str) {
  let (mut input, stdout) = Readline::new("> ".to_owned()).unwrap();
  input.should_print_line_on(true, false);
  ecs.insert(InputResource(Some(input)));
  ecs.insert(OutputResource(Some(stdout)));
}
