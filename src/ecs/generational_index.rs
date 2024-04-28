/// A generation, which marks the age of an entity.
pub type Generation = u32;

/// An index, which marks the position of an entity in a collection.
pub type Index = usize;

/// A generational index; a unique identifier for an entity.
///
/// This is taken more-or-less verbatim from Catherine West's delightful talk
/// at RustConf 2018 and her article here:
/// @see https://kyren.github.io/2018/09/14/rustconf-talk.html
#[derive(Clone, Copy, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub struct GenerationalIndex {
  /// The generation of the entity.
  pub generation: Generation,
  /// The index of the entity.
  pub index: Index,
}

impl GenerationalIndex {
  /// Create a new generational index.
  pub fn new(generation: Generation, index: Index) -> Self {
    Self { generation, index }
  }
}
