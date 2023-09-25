use rustyline_async::Readline;
use specs::prelude::*;

pub mod _list;
pub use _list::*;

pub fn insert_resources(ecs: &mut World, _seed: &str) {
  let (mut input, stdout) = Readline::new("> ".to_owned()).unwrap();
  input.should_print_line_on(false, false);
  ecs.insert(InputResource(Some(input)));
  ecs.insert(OutputResource(Some(stdout)));
  ecs.insert(QuitFlagResource(None));
  ecs.insert(InputReadyFlagResource(true));
}
