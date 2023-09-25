// This file is generated (see build.rs). Please do not edit manually.
pub mod delayed;
pub use delayed::Delayed as DelayedEffect;
pub mod null;
pub use null::Null as NullEffect;
pub mod quit_game;
pub use quit_game::QuitGame as QuitGameEffect;
pub mod set_input_ready_flag;
pub use set_input_ready_flag::SetInputReadyFlag as SetInputReadyFlagEffect;
