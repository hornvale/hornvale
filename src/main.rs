use anyhow::Error as AnyError;
use hornvale::game::Game;
use log::LevelFilter;
use pretty_env_logger::env_logger::builder as pretty_env_logger_builder;
use uuid::Uuid;

fn main() -> Result<(), AnyError> {
  // Set up logging.
  // At least for development, I'll just control the log level in code.
  pretty_env_logger_builder().filter_level(LevelFilter::Debug).init();
  // Create a seed string. Eventually, this will be a command line argument
  // or something.
  let seed_string = Uuid::new_v4().to_string();
  // Create and run the game. Currently, we start a new game every time.
  // This will change in the future.
  let mut game = Game::new();
  game.run(&seed_string)?;
  // Fallback.
  Ok(())
}
