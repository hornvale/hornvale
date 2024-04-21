use crate::prelude::*;
use serde::{Deserialize, Serialize};

/// A command syntax type.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CommandSyntax(pub CommandArity, pub CommandModifier, pub CommandModifier);
