use anyhow::Error as AnyError;
use std::sync::Arc;

use crate::event::Event;
use crate::event::EventSubscriberBuilder;
use crate::event::EventType;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;
use crate::game_state::LoopTimerTrait;
use crate::game_state::QuitFlagTrait;
use crate::system::CommandSystem;
use crate::system::EventSystem;
use crate::system::InputSystem;
use crate::system::LoopTimerSystem;
use crate::system::OutputSystem;
use crate::system::ParserSystem;
use crate::system::SystemTrait;
use crate::system::TickSystem;

/// The `Game` struct.
///
/// This is basically a wrapper around the run loop.
///
/// General flow:
///   1. Initialization: print welcome message, set up game state, etc.
///   2. Run loop: read input, parse input, execute command, process events,
///      display output, repeat.
///   3. Termination: print goodbye message, clean up game state, etc.
///
/// The run loop is the heart of the game. It's where the player interacts with
/// the game world. It's where the game world interacts with the player. It's
/// where the magic happens.
///
/// For more detail on the run loop, see the documentation for the `run` method
/// and the `ARCHITECTURE.md` file in the root of the project.
#[derive(Debug, Default)]
pub struct Game {}

impl Game {
  /// Creates a new `Game`.
  pub fn new() -> Self {
    Self {}
  }

  /// Runs the `Game`.
  pub fn run(&mut self) -> Result<(), AnyError> {
    debug!("Running game.");

    // Initialization
    println!("Welcome to Hornvale!");

    let mut game_state = GameState::new();
    let mut command_system = CommandSystem::default();
    let mut event_system = EventSystem::default();
    let mut input_system = InputSystem::default();
    let mut loop_timer_system = LoopTimerSystem::default();
    let mut output_system = OutputSystem::default();
    let mut parser_system = ParserSystem::default();
    let debug_logger = EventSubscriberBuilder::new()
      .name("Debug Logger".to_string())
      .will_process(Arc::new(|event: &mut Event, _game_state: &GameState| {
        debug!("Will process event: {:#?}", event);
      }))
      .did_process(Arc::new(|event: &Event, _game_state: &mut GameState| {
        debug!("Did process event: {:#?}", event);
      }))
      .build();
    let _debug_logger_uuid = debug_logger.uuid;
    event_system.event_publisher.add_subscriber(debug_logger);
    let mut tick_system = TickSystem::default();
    game_state.set_input_ready_flag(false);
    let start_game_event = Event::new(EventType::StartedGame, DEFAULT_PRIORITY, Vec::new());
    game_state.enqueue_event(start_game_event);
    loop {
      // Run tick system.
      tick_system.run(&mut game_state);
      // Long-running diegetic actions will set the input_ready flag to false.
      // We don't want to read input until they resolve.
      if game_state.get_input_ready_flag() {
        // Read input from the user.
        input_system.run(&mut game_state);
      }
      // Reset the loop timer, which will be used to measure how long the last
      // tick took to process (excluding input and output).
      game_state.reset_loop_timer();
      // Parse player input into a command or commands.
      parser_system.run(&mut game_state);
      // Execute the command or commands entered by the player. This normally
      // enqueues one or more events.
      command_system.run(&mut game_state);
      // Process event queue, which will execute actions and apply effects
      // and cancel other events and so forth and so on.
      event_system.run(&mut game_state);
      // Run the loop timer.
      loop_timer_system.run(&mut game_state);
      // Display accumulated output to the user.
      output_system.run(&mut game_state);
      // We've been told to quit.
      if game_state.get_quit_flag() {
        break;
      }
    }

    // Termination
    println!("Thanks for playing!");

    Ok(())
  }
}
