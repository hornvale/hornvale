/// A generational index; a unique identifier for an entity.
///
/// This is taken more-or-less verbatim from Catherine West's delightful talk
/// at RustConf 2018 and her article here:
/// @see https://kyren.github.io/2018/09/14/rustconf-talk.html
#[derive(Clone, Copy, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub struct GenerationalIndex {
  /// The generation of the entity.
  pub generation: u32,
  /// The index of the entity.
  pub index: usize,
}

impl GenerationalIndex {
  /// Create a new generational index.
  pub fn new(generation: u32, index: usize) -> Self {
    Self { generation, index }
  }
}
