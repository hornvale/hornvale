use anyhow::Error as AnyError;
use colored::*;
use rand_seeder::SipHasher;
use specs::prelude::*;
use specs::saveload::SimpleMarkerAllocator;
use specs::shrev::EventChannel;
use std::io::{stdin, stdout, Write};

use crate::chunk::ChunkPlaneBuilder;
use crate::dispatcher::get_initial_dispatcher;
use crate::dispatcher::get_simulation_dispatcher;
use crate::event::ChunkPlaneRequestEvent;
use crate::event::InputEvent;
use crate::event::RoomRequestEvent;
use crate::marker::PersistedEntity;
use crate::marker::PersistedEntityMarker;
use crate::resource::InputReadyFlagResource;
use crate::resource::QuitFlagResource;
use crate::resource::RandomResource;
use crate::resource::SeedStringResource;
use crate::room::RoomBuilder;
use crate::room::RoomFactory;
use crate::room::RoomStatus;

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
/// The run loop is the heart of the game.
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

  /// Read the input from the user.
  pub fn read_input(&self) -> String {
    debug!("Reading input.");
    print!("> ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
  }

  /// Runs the `Game`.
  pub fn run(&mut self, seed_string: &str) -> Result<(), AnyError> {
    // Create the ECS.
    let mut ecs = World::new();
    ecs.register::<PersistedEntityMarker>();
    ecs.insert(SimpleMarkerAllocator::<PersistedEntity>::new());

    // Initializing the game.
    debug!("Initializing game.");
    let mut initial_dispatcher = get_initial_dispatcher(&mut ecs);
    ecs.fetch_mut::<SeedStringResource>().0 = seed_string.to_string();
    debug!("Seed String: {}", ecs.fetch::<SeedStringResource>().0);
    ecs.insert(RandomResource(SipHasher::from(seed_string).into_rng()));
    ecs
      .fetch_mut::<EventChannel<ChunkPlaneRequestEvent>>()
      .single_write(ChunkPlaneRequestEvent {
        chunk_plane: ChunkPlaneBuilder::default()
          .name("default".to_string())
          .seed_string(format!("{}::{}", seed_string, "primary_chunk_plane"))
          .description("The primary chunk plane.".to_string())
          .build()
          .expect("Failed to build chunk plane."),
      });
    ecs
      .fetch_mut::<EventChannel<RoomRequestEvent>>()
      .single_write(RoomRequestEvent {
        room: RoomBuilder::default()
          .name("default".to_string())
          .seed_string(format!("{}::{}", seed_string, "primary_room"))
          .description("The primary room.".to_string())
          .coordinates((0, 0, 0).into())
          .status(RoomStatus::Unknown)
          .is_startable(true)
          .build()
          .expect("Failed to build room."),
        room_factory: RoomFactory::default(),
      });
    initial_dispatcher.dispatch(&ecs);

    // Kicking off the game.
    debug!("Running game.");
    let mut simulation_dispatcher = get_simulation_dispatcher(&mut ecs);

    // Initialization.
    println!(
      "{}",
      format!(
        "Welcome to {}!",
        format!(
          "H{}{}{}{}{}{}{}",
          "o".truecolor(255, 165, 0),
          "r".yellow(),
          "n".green(),
          "v".blue(),
          "a".truecolor(75, 0, 130),
          "l".truecolor(127, 0, 255),
          "e".white()
        )
        .red()
      )
      .bold()
    );
    ecs.fetch_mut::<InputReadyFlagResource>().0 = true;

    // Game loop.
    loop {
      // Long-running diegetic actions will set the input_ready flag to false.
      // We don't want to read input until they resolve.
      if ecs.fetch::<InputReadyFlagResource>().0 {
        let input = self.read_input();
        ecs.fetch_mut::<EventChannel<InputEvent>>().single_write(InputEvent {
          input: input.to_owned(),
        });
      }

      // Run the general simulation dispatcher.
      simulation_dispatcher.dispatch(&ecs);

      // Maintain after every tick.  This enables the use of the lazy systems,
      // which should make it easier to have simple, concise systems.
      ecs.maintain();

      // Check for the quit flag.
      if ecs.fetch::<QuitFlagResource>().0 {
        break;
      }
    }

    // Termination message.
    println!("Thanks for playing!");

    // Exit, finally.
    Ok(())
  }
}
