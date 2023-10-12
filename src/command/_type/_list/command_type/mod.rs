use anyhow::Error as AnyError;

use crate::command::Command;
use crate::command::CommandData;
use crate::passage::PassageDirection;

/// The `CommandType` enum.
///
/// This should be an exhaustive collection of commands.
///
/// Commands should be phrased in the imperative mood.
#[derive(Clone, Copy, Debug, Default, Deserialize, Display, Eq, Hash, PartialEq, Serialize)]
pub enum CommandType {
  /// No-Op -- absolutely nothing happens.
  #[default]
  NoOp,
  /// QuitGame -- the player quit.
  QuitGame,
  /// LookAround -- the player looked around.
  LookAround,
  /// Movement in a passage direction.
  Walk(PassageDirection),
}

impl CommandType {
  pub fn execute(&self, _command: &Command, data: &mut CommandData) -> Result<(), AnyError> {
    debug!("Executing {:#?} command.", self);
    use CommandType::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Executing no-op command.");
      },
      QuitGame => {
        debug!("Executing quit-game command.");
        data.quit_flag_resource.0 = true;
        //let action = Action::new(ActionType::QuitGame, command.backtrace.clone());
        //action.attempt(data)?;
      },
      LookAround => {
        debug!("Executing look command.");
        // let player_id = game_state.get_player_id();
        // let current_room_id = game_state.current_room_id.clone().unwrap();
        // let action = Action::new(
        //   ActionType::LookAround(player_id.clone().into(), current_room_id),
        //   command.backtrace.clone(),
        // );
        // action.attempt(game_state)?;
      },
      Walk(_direction) => {
        debug!("Executing walk command.");
        // let player_id = game_state.get_player_id();
        // let action = Action::new(
        //   ActionType::Walk(player_id.clone().into(), direction.clone()),
        //   command.backtrace.clone(),
        // );
        // action.attempt(game_state)?;
      },
    }
    Ok(())
  }

  /// Is this a diegetic command or not?
  pub fn is_diegetic(&self) -> bool {
    use CommandType::*;
    match self {
      NoOp => false,
      QuitGame => false,
      LookAround => false,
      Walk(_) => true,
    }
  }
}
