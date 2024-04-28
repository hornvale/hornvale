use super::{component::Component, generational_index_array::GenerationalIndexArray};
use std::any::TypeId;
use std::collections::HashMap;

/// The array type for a component map.
pub type ComponentArray = GenerationalIndexArray<Component>;

/// A collection of components by type.
pub type ComponentMap = HashMap<TypeId, ComponentArray>;
