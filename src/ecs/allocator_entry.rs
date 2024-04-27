/// An allocator entry for a generational index.
///
/// This is taken more-or-less verbatim from Catherine West's delightful talk
/// at RustConf 2018 and her article here:
/// @see https://kyren.github.io/2018/09/14/rustconf-talk.html
#[derive(Clone, Copy, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub struct AllocatorEntry {
  /// Whether this entry is still alive.
  pub is_live: bool,
  /// The generation of the entity.
  pub generation: u64,
}

impl Default for AllocatorEntry {
  fn default() -> Self {
    Self {
      is_live: true,
      generation: 0,
    }
  }
}
