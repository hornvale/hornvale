use std::io::Stdout;

/// The `Output` resource.
#[derive(Debug, Default)]
#[repr(transparent)]
pub struct Output(pub Option<Stdout>);
