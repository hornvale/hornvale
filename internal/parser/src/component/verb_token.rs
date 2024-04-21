use crate::token::Token;
use serde::{Deserialize, Serialize};

/// A component encapsulating a verb token.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct VerbToken(pub Token);
