use anyhow::Error as AnyError;

use crate::action::Action;
use crate::action::ActionType;
use crate::command::Command;
use crate::game_state::GameState;
use crate::game_state::PlayerIdTrait;
use crate::passage::PassageDirection;

/// The `Type` enum.
///
/// This should be an exhaustive collection of commands.
///
/// Commands should be phrased in the imperative mood.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Type {
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

impl Type {
  /// Creates a new `Type`.
  pub fn new() -> Self {
    Self::default()
  }

  pub fn execute(&self, command: &Command, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Executing {:#?} command.", self);
    use Type::*;
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Executing no-op command.");
      },
      QuitGame => {
        debug!("Executing quit-game command.");
        let action = Action::new(ActionType::QuitGame, command.backtrace.clone());
        action.attempt(game_state)?;
      },
      LookAround => {
        debug!("Executing look command.");
        let player_id = game_state.get_player_id();
        let current_room_id = game_state.current_room_id.clone();
        let action = Action::new(
          ActionType::LookAround(player_id.clone().into(), current_room_id),
          command.backtrace.clone(),
        );
        action.attempt(game_state)?;
      },
      Walk(direction) => {
        debug!("Executing walk command.");
        let player_id = game_state.get_player_id();
        let action = Action::new(
          ActionType::Walk(player_id.clone().into(), direction.clone()),
          command.backtrace.clone(),
        );
        action.attempt(game_state)?;
      },
      _ => {
        // By default, we let subscribers react to the event.
      },
    }
    Ok(())
  }
}
