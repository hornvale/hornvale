use serde::{Deserialize, Serialize};

/// A component that marks an entity as a parser product.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct IsParserProduct;
