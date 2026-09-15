use crate::ViewError;
use serde::{Deserialize, Serialize};
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Serialize)]
pub struct Binding {
    pub source_id: String,
    pub scope_id: String,
    pub world_sha256: String,
    pub source_revision: String,
}
impl Binding {
    pub fn validate(&self) -> Result<(), ViewError> {
        let hex = |s: &str, n| {
            s.len() == n
                && s.bytes()
                    .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
        };
        if self.source_id.trim().is_empty()
            || self.scope_id.trim().is_empty()
            || !hex(&self.world_sha256, 64)
            || !hex(&self.source_revision, 40)
        {
            return Err(ViewError::Binding("incomplete source identity".into()));
        }
        Ok(())
    }
}
