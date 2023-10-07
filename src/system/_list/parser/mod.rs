use crate::command::Command;
use crate::command::CommandType;
use crate::game_state::CommandQueueTrait;
use crate::game_state::GameState;
use crate::game_state::InputQueueTrait;
use crate::passage::PassageDirection;
use crate::system::SystemTrait;

/// The `Parser` struct.
///
/// This system parses input and enqueues commands.
#[derive(Debug, Default)]
pub struct Parser {}

impl Parser {
  /// Creates a new `Parser`.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Parser {
  /// Runs the `Parser`.
  fn run(&mut self, game_state: &mut GameState) {
    debug!("Running parser system.");
    while let Some(input) = game_state.dequeue_input() {
      let command: Command = match input.as_str() {
        "quit" => Command::new(CommandType::QuitGame),
        "ne" | "northeast" | "north east" | "go northeast" | "go ne" => {
          Command::new(CommandType::Walk(PassageDirection::Northeast))
        },
        "nw" | "northwest" | "north west" | "go northwest" | "go nw" => {
          Command::new(CommandType::Walk(PassageDirection::Northwest))
        },
        "se" | "southeast" | "south east" | "go southeast" | "go se" => {
          Command::new(CommandType::Walk(PassageDirection::Southeast))
        },
        "sw" | "southwest" | "south west" | "go southwest" | "go sw" => {
          Command::new(CommandType::Walk(PassageDirection::Southwest))
        },
        "n" | "north" | "go north" => Command::new(CommandType::Walk(PassageDirection::North)),
        "s" | "south" | "go south" => Command::new(CommandType::Walk(PassageDirection::South)),
        "e" | "east" | "go east" => Command::new(CommandType::Walk(PassageDirection::East)),
        "w" | "west" | "go west" => Command::new(CommandType::Walk(PassageDirection::West)),
        "u" | "up" | "go up" => Command::new(CommandType::Walk(PassageDirection::Up)),
        "d" | "down" | "go down" => Command::new(CommandType::Walk(PassageDirection::Down)),
        "in" | "inside" | "go inside" => Command::new(CommandType::Walk(PassageDirection::In)),
        "out" | "outside" | "go outside" => Command::new(CommandType::Walk(PassageDirection::Out)),
        "look" | "look around" => Command::new(CommandType::LookAround),
        "" => Command::new(CommandType::NoOp),
        _ => Command::new(CommandType::NoOp),
      };
      game_state.enqueue_command(command);
    }
  }
}
