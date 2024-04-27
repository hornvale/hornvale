use crate::ecs::prelude::*;

/// An allocator for generational indices.
///
/// This is taken more-or-less verbatim from Catherine West's delightful talk
/// at RustConf 2018 and her article here:
/// @see https://kyren.github.io/2018/09/14/rustconf-talk.html
#[derive(Debug, Default)]
pub struct GenerationalIndexAllocator {
  /// The allocator entries.
  entries: Vec<AllocatorEntry>,
  /// The free list of indices.
  free: Vec<usize>,
}

impl GenerationalIndexAllocator {
  /// Allocate a new generational index.
  pub fn allocate(&mut self) -> GenerationalIndex {
    if let Some(index) = self.free.pop() {
      // If we have a free index, mark it as live and increment the generation.
      let entry = &mut self.entries[index];
      assert!(!entry.is_live);
      entry.is_live = true;
      entry.generation = entry.generation.wrapping_add(1);
      GenerationalIndex::new(entry.generation, index)
    } else {
      // Otherwise, push a new entry and return the index.
      let index = self.entries.len();
      self.entries.push(AllocatorEntry::default());
      GenerationalIndex::new(0, index)
    }
  }

  /// Deallocate a generational index.
  ///
  /// Returns true if the index was allocated before and is now deallocated
  pub fn deallocate(&mut self, index: GenerationalIndex) -> bool {
    if self
      .entries
      .get(index.index)
      .map_or(false, |entry| entry.generation == index.generation)
    {
      self.entries[index.index].is_live = false;
      self.free.push(index.index);
      true
    } else {
      false
    }
  }

  /// Check if a generational index is still live.
  pub fn is_live(&self, index: GenerationalIndex) -> bool {
    self
      .entries
      .get(index.index)
      .map_or(false, |entry| entry.is_live && entry.generation == index.generation)
  }
}
