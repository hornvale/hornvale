/// The `InputReadyFlag` resource.
///
/// This is a flag that indicates whether the game should accept input.
#[derive(Clone, Default)]
#[repr(transparent)]
pub struct InputReadyFlag(pub bool);
