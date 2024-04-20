use std::collections::HashMap;

use crate::garbage_collection::reference::Reference;
use crate::value::Value;

/// The `Table` type.
pub type Table = HashMap<Reference<String>, Value>;
