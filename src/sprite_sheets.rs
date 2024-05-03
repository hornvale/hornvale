//! # Sprite Sheets
//!
//! This plugin manages the sprite sheets for the game.

use bevy::prelude::*;

/// Hexany Monster Menagerie (32x32)
pub mod hexany_mm_32x32;
use hexany_mm_32x32::HexanyMm32x32;
/// Hexany Roguelike Tiles (16x16)
pub mod hexany_rt_16x16;
use hexany_rt_16x16::HexanyRt16x16;
/// Kenney's 1-bit Pack (16x16)
pub mod kenney_1bit_16x16;
use kenney_1bit_16x16::Kenney1bit16x16;

/// The Sprite Sheets plugin.
#[derive(Debug, Clone, Copy)]
pub struct SpriteSheetsPlugin;

impl Plugin for SpriteSheetsPlugin {
  fn build(&self, app: &mut App) {
    app
      .init_resource::<HexanyMm32x32>()
      .init_resource::<HexanyRt16x16>()
      .init_resource::<Kenney1bit16x16>();
  }
}

/// The prelude.
pub mod prelude {
  pub use super::hexany_mm_32x32::*;
  pub use super::hexany_rt_16x16::*;
  pub use super::kenney_1bit_16x16::*;
  pub use super::SpriteSheetsPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
