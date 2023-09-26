use crate::game_state::InputReadyFlagTrait;
use crate::game_state::OutputQueueTrait;
use crate::game_state::QuitFlagTrait;

/// The `GameState` trait.
///
/// This is a trait describing the `GameState` struct.
pub trait GameState: OutputQueueTrait + QuitFlagTrait + InputReadyFlagTrait {}
