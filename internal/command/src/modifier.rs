use serde::{Deserialize, Serialize};
use strum::Display;

/// The "form" of a command; a modifier that can be applied to a verb.
///
/// This is generally either an adverb or a preposition.
#[derive(Clone, Copy, Debug, Default, Display, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum CommandModifier {
  /// The base form of a command.
  #[default]
  Default,
  /// A command that takes a direction.
  Direction,
  // Adverbial/prepositional forms.
  /// A command that takes a phrase with "about".
  /// For example, "talk about the weather".
  About,
  /// A command that takes a phrase with "above".
  /// For example, "look above the door".
  Above,
  /// A command that takes a phrase with "across".
  /// For example, "walk across the bridge".
  Across,
  /// A command that takes a phrase with "against".
  /// For example, "lean against the wall".
  Against,
  /// A command that takes a phrase with "along".
  /// For example, "walk along the path".
  Along,
  /// A command that takes a phrase with "among".
  /// For example, "search among the trees".
  Among,
  /// A command that takes a phrase with "as".
  /// For example, "remember goblin as dave".
  As,
  /// A command that takes a phrase with "at".
  /// For example, "look at the painting".
  At,
  /// A command that takes a phrase with "before".
  /// For example, "stand before the king".
  Before,
  /// A command that takes a phrase with "behind".
  /// For example, "hide behind the curtain".
  Behind,
  /// A command that takes a phrase with "below".
  /// For example, "descend below the surface".
  Below,
  /// A command that takes a phrase with "beside".
  /// For example, "stand beside the statue".
  Beside,
  /// A command that takes a phrase with "between".
  /// For example, "choose between the two".
  Between,
  /// A command that takes a phrase with "beyond".
  /// For example, "venture beyond the wall".
  Beyond,
  /// A command that takes a phrase with "by".
  /// For example, "pass by the guard".
  By,
  /// A command that takes a phrase with "for".
  /// For example, "search for the treasure".
  For,
  /// A command that takes a phrase with "from".
  /// For example, "take from the chest".
  From,
  /// A command that takes a phrase with "in".
  /// For example, "hide in the shadows".
  In,
  /// A command that takes a phrase with "into".
  /// For example, "step into the portal".
  Into,
  /// A command that takes a phrase with "of".
  /// For example, "speak of the devil".
  Of,
  /// A command that takes a phrase with "off".
  /// For example, "get off the horse".
  Off,
  /// A command that takes a phrase with "on".
  /// For example, "put the pie on the oven".
  On,
  /// A command that takes a phrase with "out".
  /// For example, "get out of the house".
  Out,
  /// A command that takes a phrase with "over".
  /// For example, "climb over the wall".
  Over,
  /// A command that takes a phrase with "to".
  /// For example, "go to the castle".
  To,
  /// A command that takes a phrase with "toward".
  /// For example, "walk toward the light".
  Toward,
  /// A command that takes a phrase with "under".
  /// For example, "hide under the bed".
  Under,
  /// A command that takes a phrase with "upon".
  /// For example, "stand upon the hill".
  Upon,
  /// A command that takes a phrase with "with".
  /// For example, "fight with the sword".
  With,
  /// A command that takes a phrase with "without".
  /// For example, "go without the sword".
  Without,
  /// A command that takes a phrase with "around".
  /// For example, "look around".
  Around,
  /// A command that takes a phrase with "here".
  /// For example, "look here".
  Here,
  /// A command that takes a phrase with "then".
  /// For example, "take sword then take shield".
  Then,
}
