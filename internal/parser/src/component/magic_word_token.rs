use crate::token::Token;
use serde::{Deserialize, Serialize};

/// A component encapsulating a magic word token.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct MagicWordToken(pub Token);
