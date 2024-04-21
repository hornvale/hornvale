use crate::token::Token;
use serde::{Deserialize, Serialize};

/// A component encapsulating an interrogative or "question word" token.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct QuestionToken(pub Token);
