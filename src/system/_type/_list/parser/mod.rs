use specs::prelude::*;
use specs::shrev::{EventChannel, ReaderId};

use crate::command::CommandBuilder;
use crate::command::CommandType;
use crate::event::{CommandEvent, InputEvent, OutputEvent};
use crate::passage::PassageDirection;
use crate::resource::InputReadyFlagResource;
use crate::resource::QuitFlagResource;

/// The Parser system.
#[derive(Default)]
pub struct Parser {
  pub reader_id: Option<ReaderId<InputEvent>>,
}

impl Parser {}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub input_event_channel: Read<'data, EventChannel<InputEvent>>,
  pub output_event_channel: Write<'data, EventChannel<OutputEvent>>,
  pub command_event_channel: Write<'data, EventChannel<CommandEvent>>,
  pub input_ready_flag_resource: Write<'data, InputReadyFlagResource>,
  pub quit_flag_resource: Write<'data, QuitFlagResource>,
}

impl<'data> System<'data> for Parser {
  type SystemData = Data<'data>;

  /// Run system.
  fn run(&mut self, mut data: Self::SystemData) {
    let input_events = data
      .input_event_channel
      .read(self.reader_id.as_mut().unwrap())
      .cloned()
      .collect::<Vec<InputEvent>>();
    let event_count = input_events.len();
    if event_count == 0 {
      return;
    }
    info!("Processing {} input event(s)...", event_count);
    for input_event in input_events.iter() {
      let command_type: CommandType = match input_event.input.as_str() {
        "quit" => CommandType::QuitGame,
        "ne" | "northeast" | "north east" | "go northeast" | "go ne" => CommandType::Walk(PassageDirection::Northeast),
        "nw" | "northwest" | "north west" | "go northwest" | "go nw" => CommandType::Walk(PassageDirection::Northwest),
        "se" | "southeast" | "south east" | "go southeast" | "go se" => CommandType::Walk(PassageDirection::Southeast),
        "sw" | "southwest" | "south west" | "go southwest" | "go sw" => CommandType::Walk(PassageDirection::Southwest),
        "n" | "north" | "go north" => CommandType::Walk(PassageDirection::North),
        "s" | "south" | "go south" => CommandType::Walk(PassageDirection::South),
        "e" | "east" | "go east" => CommandType::Walk(PassageDirection::East),
        "w" | "west" | "go west" => CommandType::Walk(PassageDirection::West),
        "u" | "up" | "go up" => CommandType::Walk(PassageDirection::Up),
        "d" | "down" | "go down" => CommandType::Walk(PassageDirection::Down),
        "in" | "inside" | "go inside" => CommandType::Walk(PassageDirection::In),
        "out" | "outside" | "go outside" => CommandType::Walk(PassageDirection::Out),
        "thru" | "through" | "go thru" | "go through" => CommandType::Walk(PassageDirection::Thru),
        "look" | "look around" => CommandType::LookAround,
        "" => CommandType::NoOp,
        _ => CommandType::NoOp,
      };
      let command = CommandBuilder::default()
        .command_type(command_type)
        .build()
        .expect("Failed to build command.");
      data.command_event_channel.single_write(CommandEvent { command });
    }
  }

  fn setup(&mut self, world: &mut World) {
    Self::SystemData::setup(world);
    self.reader_id = Some(world.fetch_mut::<EventChannel<InputEvent>>().register_reader());
  }
}
