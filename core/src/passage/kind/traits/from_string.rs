use crate::prelude::*;

impl From<String> for PassageKind {
  fn from(string: String) -> Self {
    PassageKind::NoExit(string)
  }
}
