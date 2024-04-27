use super::component::Component;
use std::any::TypeId;
use std::collections::HashMap;

/// A collection of components by type.
pub type ComponentMap = HashMap<TypeId, Vec<Option<Component>>>;
