use super::{array_entry::ArrayEntry, component::Component, generational_index::GenerationalIndex};

/// An associative array from GenerationalIndex to some Value T.
///
/// This is taken more-or-less verbatim from Catherine West's delightful talk
/// at RustConf 2018 and her article here:
/// @see https://kyren.github.io/2018/09/14/rustconf-talk.html
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct GenerationalIndexArray<T>(Vec<Option<ArrayEntry<T>>>);

impl<T> GenerationalIndexArray<T> {
  /// Set the value for some generational index. May overwrite past generation
  /// values.
  pub fn set(&mut self, index: GenerationalIndex, value: T) {
    let generation = index.generation;
    let entry = ArrayEntry { value, generation };
    let index = index.index;
    if index >= self.0.len() {
      self.0.resize_with(index + 1, || None);
    }
    self.0[index] = Some(entry);
  }

  /// Gets the value for some generational index; the generation must match.
  pub fn get(&self, index: GenerationalIndex) -> Option<&T> {
    self.0.get(index.index).and_then(|entry| {
      entry
        .as_ref()
        .filter(|entry| entry.generation == index.generation)
        .map(|entry| &entry.value)
    })
  }

  /// Gets the mutable value for some generational index; the generation must
  /// match.
  pub fn get_mut(&mut self, index: GenerationalIndex) -> Option<&mut T> {
    self.0.get_mut(index.index).and_then(|entry| {
      entry
        .as_mut()
        .filter(|entry| entry.generation == index.generation)
        .map(|entry| &mut entry.value)
    })
  }

  /// Get the length of the array.
  pub fn len(&self) -> usize {
    self.0.len()
  }

  /// Pass through is_empty().
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
}

impl Default for GenerationalIndexArray<Component> {
  fn default() -> Self {
    Self(Vec::new())
  }
}
