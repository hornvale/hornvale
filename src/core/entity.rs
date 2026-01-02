//! Entity identifiers and allocation.
//!
//! Entities are opaque identifiers that can have components attached.
//! The allocator produces unique, monotonically increasing IDs.

/// A unique identifier for an entity in the world.
///
/// EntityIds are opaque, cheap to copy, and totally ordered.
/// They should not be created directly; use `EntityAllocator::create()`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntityId(u64);

impl EntityId {
    /// Create an EntityId from a raw value (for deserialization).
    pub fn from_raw(id: u64) -> Self {
        EntityId(id)
    }

    /// Get the raw ID value (for serialization/debugging).
    pub fn raw(&self) -> u64 {
        self.0
    }
}

impl std::fmt::Debug for EntityId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EntityId({})", self.0)
    }
}

impl std::fmt::Display for EntityId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Allocates unique entity IDs.
///
/// Currently uses a simple monotonic counter. IDs are never reused.
/// This ensures determinism and simplifies debugging.
#[derive(Debug, Clone)]
pub struct EntityAllocator {
    next_id: u64,
}

impl EntityAllocator {
    /// Create a new allocator starting from ID 0.
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    /// Allocate a new unique entity ID.
    pub fn create(&mut self) -> EntityId {
        let id = EntityId(self.next_id);
        self.next_id += 1;
        id
    }

    /// Get the number of entities that have been allocated.
    pub fn count(&self) -> u64 {
        self.next_id
    }
}

impl Default for EntityAllocator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocator_produces_unique_ids() {
        let mut alloc = EntityAllocator::new();
        let e1 = alloc.create();
        let e2 = alloc.create();
        let e3 = alloc.create();

        assert_ne!(e1, e2);
        assert_ne!(e2, e3);
        assert_ne!(e1, e3);
    }

    #[test]
    fn test_allocator_ids_are_sequential() {
        let mut alloc = EntityAllocator::new();
        let e1 = alloc.create();
        let e2 = alloc.create();

        assert_eq!(e1.raw(), 0);
        assert_eq!(e2.raw(), 1);
    }

    #[test]
    fn test_allocator_count() {
        let mut alloc = EntityAllocator::new();
        assert_eq!(alloc.count(), 0);

        alloc.create();
        assert_eq!(alloc.count(), 1);

        alloc.create();
        alloc.create();
        assert_eq!(alloc.count(), 3);
    }

    #[test]
    fn test_entity_id_ordering() {
        let mut alloc = EntityAllocator::new();
        let e1 = alloc.create();
        let e2 = alloc.create();

        assert!(e1 < e2);
    }
}
