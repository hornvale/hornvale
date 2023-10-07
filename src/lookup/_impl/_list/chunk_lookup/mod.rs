use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::lookup::ChunkLookupTrait;
use crate::lookup::Lookup;

/// Implementation of the `ChunkLookupTrait` trait.
impl ChunkLookupTrait for Lookup {
  /// List chunks in chunk plane.
  fn get_chunks_in_chunk_plane(&self, chunk_plane_id: ChunkPlaneId) -> Vec<ChunkId> {
    self
      .chunk_plane2chunks
      .get(&chunk_plane_id)
      .cloned()
      .unwrap_or_default()
      .into_iter()
      .collect()
  }
}
