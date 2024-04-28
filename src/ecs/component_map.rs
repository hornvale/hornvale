use super::component::Component;
use std::any::TypeId;
use std::collections::HashMap;

/// The array type for a component map.
pub type ComponentArray = Vec<Option<Component>>;

/// A collection of components by type.
pub type ComponentMap = HashMap<TypeId, ComponentArray>;
