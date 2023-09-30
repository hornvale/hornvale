use anyhow::Error as AnyError;
use std::sync::Arc;

use crate::event::Event;
use crate::event::EventSubscriberBuilder;
use crate::event::EventType;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::CurrentRoomIdTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;
use crate::game_state::LoopTimerTrait;
use crate::game_state::PlayerIdTrait;
use crate::game_state::QuitFlagTrait;
use crate::game_state::RoomsTrait;
use crate::room::Room;
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

    // System creation.
    let mut game_state = GameState::new();
    let mut command_system = CommandSystem::default();
    let mut event_system = EventSystem::default();
    let mut input_system = InputSystem::default();
    let mut loop_timer_system = LoopTimerSystem::default();
    let mut output_system = OutputSystem::default();
    let mut parser_system = ParserSystem::default();
    let mut tick_system = TickSystem::default();

    // Give us time (1 tick) to start up before we start reading input.
    game_state.set_input_ready_flag(false);

    // Fire the StartedGame event.
    let start_game_event = Event::new(EventType::StartedGame, DEFAULT_PRIORITY + 10000, Vec::new());
    game_state.enqueue_event(start_game_event);

    // BEGIN TEMPORARY

    // Add a debug logger.
    let debug_logger = EventSubscriberBuilder::new()
      .name("Debug Logger".to_string())
      .event_type(EventType::StartedGame)
      .will_process(Arc::new(|event: &mut Event, _game_state: &GameState| {
        println!("Will process event: {:#?}", event);
      }))
      .did_process(Arc::new(|event: &Event, _game_state: &mut GameState| {
        println!("Did process event: {:#?}", event);
      }))
      .build();
    let _debug_logger_uuid = debug_logger.uuid;
    event_system.event_publisher.add_subscriber(debug_logger);

    // Let's create a room and add it to the game state.
    let room = Room::default();
    game_state.insert_room(room.clone());

    // And throw the player in the room.
    game_state.set_current_room_id(&room.id);
    let entity_did_enter_room_event = Event::new(
      EventType::EntityDidEnterRoom(
        game_state.get_player_id().clone().into(),
        game_state.get_current_room_id().clone(),
      ),
      DEFAULT_PRIORITY + 100,
      Vec::new(),
    );
    game_state.enqueue_event(entity_did_enter_room_event);
    let player_did_enter_room_event = Event::new(
      EventType::PlayerDidEnterRoom(game_state.get_current_room_id().clone()),
      DEFAULT_PRIORITY + 90,
      Vec::new(),
    );
    game_state.enqueue_event(player_did_enter_room_event);

    // Print the name and description of the room when we enter it.
    let room_description_logger = EventSubscriberBuilder::new()
      .name("Room Description Logger".to_string())
      .event_type(EventType::PlayerDidEnterRoom(game_state.get_current_room_id().clone()))
      .did_process(Arc::new(|event: &Event, game_state: &mut GameState| {
        let room_id = match event.r#type {
          EventType::PlayerDidEnterRoom(ref room_id) => room_id,
          _ => panic!("Unexpected event type."),
        };
        let room = game_state.get_room(room_id).unwrap();
        println!("You are in {}.", room.name);
        println!("{}", room.description);
      }))
      .build();
    let _room_description_logger_uuid = room_description_logger.uuid;
    event_system.event_publisher.add_subscriber(room_description_logger);

    // END TEMPORARY
    loop {
      // Run tick system, which increments the tick counter.
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
