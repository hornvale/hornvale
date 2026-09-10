//! A renderer-side observation boundary. No simulation or source dependencies.
mod binding;
pub mod documents;
pub use binding::Binding;
#[derive(Debug)]
pub enum ViewError {
    Document(String),
    Binding(String),
    Range(String),
    Capture(String),
}
impl std::fmt::Display for ViewError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Document(s) | Self::Binding(s) | Self::Range(s) | Self::Capture(s) => {
                f.write_str(s)
            }
        }
    }
}
impl std::error::Error for ViewError {}
impl From<serde_json::Error> for ViewError {
    fn from(e: serde_json::Error) -> Self {
        Self::Document(e.to_string())
    }
}
pub mod coordinates;
mod mirror;
pub use mirror::ObservationMirror;
pub mod astronomy;
mod camera;
pub use camera::CameraPose;
mod renderer;
pub use bevy;
pub use renderer::{Renderer, VisualPlugin};
