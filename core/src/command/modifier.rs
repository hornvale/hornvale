use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

/// Traits and trait implementations.
pub mod traits;

/// The "form" of a command; a modifier that can be applied to a verb.
///
/// This is generally either an adverb or a preposition.
#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum CommandModifier {
  /// e.g. "talk about the weather".
  About,
  /// e.g. "look above the door".
  Above,
  /// e.g. "walk across the bridge".
  Across,
  /// e.g. "lean against the wall".
  Against,
  /// e.g. "walk along the path".
  Along,
  /// e.g. "search among the trees".
  Among,
  /// e.g. "remember goblin as dave".
  As,
  /// e.g. "look at the painting".
  At,
  /// e.g. "stand before the king".
  Before,
  /// e.g. "hide behind the curtain".
  Behind,
  /// e.g. "descend below the surface".
  Below,
  /// e.g. "stand beside the statue".
  Beside,
  /// e.g. "choose between the two".
  Between,
  /// e.g. "venture beyond the wall".
  Beyond,
  /// e.g. "pass by the guard".
  By,
  /// e.g. "search for the treasure".
  For,
  /// e.g. "take from the chest".
  From,
  /// e.g. "hide in the shadows".
  In,
  /// e.g. "step into the portal".
  Into,
  /// e.g. "speak of the devil".
  Of,
  /// e.g. "get off the horse".
  Off,
  /// e.g. "put the pie on the oven".
  On,
  /// e.g. "get out of the house".
  Out,
  /// e.g. "climb over the wall".
  Over,
  /// e.g. "go to the castle".
  To,
  /// e.g. "walk toward the light".
  Toward,
  /// e.g. "hide under the bed".
  Under,
  /// e.g. "stand upon the hill".
  Upon,
  /// e.g. "fight with the sword".
  With,
  /// e.g. "go without the sword".
  Without,
  /// e.g. "look around".
  Around,
  /// e.g. "look here".
  Here,
  /// e.g. "take sword then take shield".
  Then,
}
