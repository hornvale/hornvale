use crossterm::{
  cursor::MoveToColumn,
  execute,
  terminal::{disable_raw_mode, Clear, ClearType},
};
use log::LevelFilter;
use simplelog::Config;
use simplelog::WriteLogger;
use std::io::stdout;

use hornvale::game::Game;
use hornvale::game::GameError;

fn clear_last_line() -> Result<(), GameError> {
  disable_raw_mode()?;
  execute!(stdout(), MoveToColumn(0), Clear(ClearType::CurrentLine))?;
  Ok(())
}

#[tokio::main]
async fn main() -> Result<(), GameError> {
  let mut game = Game::new("goat boy");
  let stdout = game.output.clone();
  WriteLogger::init(LevelFilter::Off, Config::default(), stdout).unwrap();
  match game.run().await {
    Ok(_) => {
      clear_last_line()?;
    },
    Err(e) => {
      clear_last_line()?;
      eprintln!("{}", e);
    },
  }
  Ok(())
}
