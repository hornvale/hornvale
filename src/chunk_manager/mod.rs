/// The `ChunkManager` struct.
///
/// This indirectly manages most of the chunk-related functionality.
///
/// This struct is responsible for:
/// - Orchestrating the creation and destruction of `ChunkPlane`s
/// - Invoking creation of empty `Chunk`s by their `ChunkPlane`s
/// - Populating empty `Chunk`s with the `ChunkFactory`.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ChunkManager {}
