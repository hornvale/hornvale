use crossterm::{
  cursor::MoveToColumn,
  execute,
  terminal::{disable_raw_mode, Clear, ClearType},
};
use rustyline_async::{ReadlineError, ReadlineEvent};
use specs::prelude::*;
use specs::shrev::EventChannel;
use std::io::Write;
use std::time::Duration;
use tokio::time::interval;
use tokio::time::sleep;

use crate::component::register_components;
use crate::dispatcher::*;
use crate::event::InputEvent;
use crate::event_channel::insert_event_channels;
use crate::resource::*;

pub mod _constant;
use _constant::*;
pub mod error;
use error::Error;
pub use error::Error as GameError;

/// The `Game` struct.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Game {}

impl Game {
  pub fn new() -> Self {
    Self {}
  }

  /// Clear the last line (remove prompt).
  pub fn clear_last_line(&self) -> Result<(), Error> {
    disable_raw_mode()?;
    execute!(std::io::stdout(), MoveToColumn(0), Clear(ClearType::CurrentLine))?;
    Ok(())
  }

  /// Quit with a specified message.
  pub fn quit(&self, message: &str) -> Result<(), Error> {
    self.clear_last_line()?;
    println!("Quit: {}", message);
    println!("Goodbye!");
    Ok(())
  }

  pub async fn read_input_or_wait(&self, ecs: &mut World) -> Result<ReadlineEvent, ReadlineError> {
    if ecs.read_resource::<InputReadyFlagResource>().0 {
      let mut input_resource = ecs.write_resource::<InputResource>();
      let stdin = input_resource.0.as_mut().unwrap();
      stdin.readline().await
    } else {
      sleep(Duration::from_millis(10)).await;
      Ok(ReadlineEvent::Eof)
    }
  }

  /// Run.
  pub async fn run(&self, seed: &str) -> Result<(), Error> {
    let mut ecs = World::new();
    insert_resources(&mut ecs, seed);
    insert_event_channels(&mut ecs);
    register_components(&mut ecs);
    let mut tick_dispatcher = get_tick_dispatcher(&mut ecs);
    let mut deca_tick_dispatcher = get_deca_tick_dispatcher(&mut ecs);
    let mut hecto_tick_dispatcher = get_hecto_tick_dispatcher(&mut ecs);
    let mut kilo_tick_dispatcher = get_kilo_tick_dispatcher(&mut ecs);
    let output = {
      let output_resource = ecs.read_resource::<OutputResource>();
      output_resource.0.as_ref().unwrap().clone()
    };
    run_initial_systems(&mut ecs);
    // If we need to print without sending it through the whole thing.
    let mut stdout = output.clone();
    // It'd be interesting to store this in a resource and possibly modify it
    // on the fly. Very FRP. Much signal.
    let mut tick_timer = interval(Duration::from_millis(TICK_INTERVAL));
    // A local tick counter; just for performing less frequent operations.
    let mut tick: u64 = 0;
    // Main game loop, such as it is.
    loop {
      // Maintain after every tick.  This enables the use of the lazy systems,
      // which should make it easier to have simple, concise systems.
      ecs.maintain();
      // Check the quit flag resource.
      {
        let quit_flag_resource = ecs.read_resource::<QuitFlagResource>();
        if quit_flag_resource.0.is_some() {
          self.quit(quit_flag_resource.0.as_ref().unwrap())?;
          return Ok(());
        }
      }
      // Select the next future to complete.
      tokio::select! {
        _ = tick_timer.tick() => {
          // Each tick, run all of the systems.  We could have multiple
          // dispatchers, each running a subset of the systems, and scheduled
          // differently.
          tick_dispatcher.dispatch(&ecs);
          tick += 1;
          if tick % 10 == 0 {
            deca_tick_dispatcher.dispatch(&ecs);
            if tick % 100 == 0 {
              hecto_tick_dispatcher.dispatch(&ecs);
              if tick % 1000 == 0 {
                kilo_tick_dispatcher.dispatch(&ecs);
              }
            }
          }
        }
        command = self.read_input_or_wait(&mut ecs) => match command {
          Ok(ReadlineEvent::Line(line)) => {
            // This is how we read input.
            let mut input_resource = ecs.write_resource::<InputResource>();
            // Probably move to a prompt system?  Or not?  IDK.
            let stdin = input_resource.0.as_mut().unwrap();
            // Disable further input for the moment.
            // We could conceivably be parsing some commands (like Quit, etc)
            // from here rather than sending them through the system, but I
            // think that's a bad architectural decision.
            let line = line.trim();
            stdin.add_history_entry(line.to_owned());
            // Echo the input to the output.
            writeln!(stdout, "> {}", line)?;
            // We could write "input" in other places.  This might be a way
            // (however unsophisticated) of building macros into the UI.
            ecs
              .write_resource::<EventChannel<InputEvent>>()
              .single_write(InputEvent {
                input: line.to_owned(),
              });
            ecs.write_resource::<InputReadyFlagResource>().0 = false;
          },
          Ok(ReadlineEvent::Eof) => {
          },
          Ok(ReadlineEvent::Interrupted) => {
            return Ok(());
          },
          Err(error) => {
            writeln!(stdout, "Error: {error:?}")?;
            return Err(error.into())
          },
        }
      }
    }
  }
}

impl Default for Game {
  fn default() -> Self {
    Self::new()
  }
}
