/// The `QuitFlag` resource.
///
/// This is a flag that indicates whether the game should quit.
#[derive(Clone, Default)]
#[repr(transparent)]
pub struct QuitFlag(pub Option<String>);
