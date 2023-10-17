use anyhow::Error as AnyError;

use crate::action::ActionBuilder;
use crate::action::ActionType;
use crate::command::Command;
use crate::command::CommandData;
use crate::event::ActionEvent;
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
  pub fn execute(&self, command: &Command, data: &mut CommandData) -> Result<(), AnyError> {
    debug!("Executing {:#?} command.", self);
    use CommandType::*;
    let actor_entity = data.player_resource.entity.unwrap();
    #[allow(unreachable_patterns)]
    match self {
      NoOp => {
        debug!("Executing no-op command.");
        let action = ActionBuilder::default()
          .action_type(ActionType::NoOp)
          .backtrace(command.backtrace.clone())
          .build()?;
        data.action_event_channel.single_write(ActionEvent { action });
      },
      QuitGame => {
        debug!("Executing quit-game command.");
        let action = ActionBuilder::default()
          .action_type(ActionType::QuitGame)
          .backtrace(command.backtrace.clone())
          .build()?;
        data.action_event_channel.single_write(ActionEvent { action });
      },
      LookAround => {
        debug!("Executing look command.");
        let room_entity = data.is_in_room_component.get(actor_entity).unwrap().0;
        let action = ActionBuilder::default()
          .action_type(ActionType::LookAround(actor_entity, room_entity))
          .backtrace(command.backtrace.clone())
          .build()?;
        data.action_event_channel.single_write(ActionEvent { action });
      },
      Walk(direction) => {
        debug!("Executing walk command.");
        let action = ActionBuilder::default()
          .action_type(ActionType::Walk(actor_entity, *direction))
          .backtrace(command.backtrace.clone())
          .build()?;
        data.action_event_channel.single_write(ActionEvent { action });
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
