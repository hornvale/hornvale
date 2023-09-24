use rustyline_async::SharedWriter;

/// The `Output` resource.
///
/// This represents a shared writer for outputting text to the player.
#[derive(Clone, Default)]
#[repr(transparent)]
pub struct Output(pub Option<SharedWriter>);
