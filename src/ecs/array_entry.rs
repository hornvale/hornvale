/// An entry in an array with a generation number.
///
/// This is used to store an entry in a vector with a generation number to
/// determine if the entry is still valid.
///
/// This is taken more-or-less verbatim from Catherine West's delightful talk
/// at RustConf 2018 and her article here:
/// @see https://kyren.github.io/2018/09/14/rustconf-talk.html
#[derive(Clone, Debug, PartialEq)]
pub struct ArrayEntry<T> {
  /// The value of the entry.
  pub value: T,
  /// The generation of the entry.
  pub generation: u64,
}
