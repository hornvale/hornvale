use log::LevelFilter;
use rustyline_async::{Readline, ReadlineEvent};
use simplelog::Config;
use simplelog::WriteLogger;
use std::io::Write;
use std::time::Duration;
use tokio::time::sleep;

use hornvale::game::Game;
use hornvale::game::GameError;

#[tokio::main]
async fn main() -> Result<(), GameError> {
  let mut game = Game::new("goat boy");
  let stdout = game.output.clone();
  WriteLogger::init(LevelFilter::Off, Config::default(), stdout).unwrap();
  game.run().await?;
  Ok(())
}
