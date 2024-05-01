/// The `Active` state, representing the main game loop.
pub mod active;
/// The `CleanSlate` state, representing a first-time player.
pub mod clean_slate;
/// The `CharacterCreation` state, representing the character creation process.
pub mod creation;
/// The `Death` state, representing the player character's death.
pub mod death;
/// The `Introduction` state, which sets up the "newbie" experience.
pub mod introduction;
/// The `Load` state, for loading the game.
pub mod load;
/// The `LoadFailed` state, for when loading the game fails.
pub mod load_failed;
/// The `Quit` state, for quitting the game.
pub mod quit;
/// The `Restart` state, for restarting the game.
pub mod restart;
/// The `Save` state, for saving the game.
pub mod save;
/// The `Shutdown` state, which is the final state before the game exits.
pub mod shutdown;
/// The `Startup` state, representing the startup state.
pub mod startup;

/// The prelude.
pub mod prelude {
  pub use super::active::Active;
  pub use super::clean_slate::CleanSlate;
  pub use super::creation::Creation;
  pub use super::death::Death;
  pub use super::introduction::Introduction;
  pub use super::load::Load;
  pub use super::load_failed::LoadFailed;
  pub use super::quit::Quit;
  pub use super::restart::Restart;
  pub use super::save::Save;
  pub use super::shutdown::Shutdown;
  pub use super::startup::Startup;
}
