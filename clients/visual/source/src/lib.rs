//! Native scientific/unrestricted source. Only serialized documents cross this
//! boundary; a renderer never receives a World or an astronomy provider.
mod protocol;
use hornvale_kernel::{World, WorldTime};
use hornvale_scene::{AstronomyContext, SceneContext};
use protocol::{Binding, Initial, Reply, Request};
use serde_json::value::RawValue;
use sha2::{Digest, Sha256};
use std::{collections::BTreeMap, path::Path};

// Test seam counts calls at native construction/export boundaries, never elapsed time.
#[cfg(test)]
thread_local! { static CONSTRUCTIONS: std::cell::Cell<[usize;5]> = const { std::cell::Cell::new([0;5]) }; } // lexicon: standard-library interior-mutability storage for test counters, not geographic sampling
macro_rules! native_build {
    ($index:expr, $build:expr) => {{
        #[cfg(test)]
        CONSTRUCTIONS.with(|counts| {
            let mut value = counts.get();
            value[$index] += 1;
            counts.set(value);
        });
        $build
    }};
}

/// Explicit loading, request, native observation and serialization errors.
#[derive(Debug)]
pub enum SourceError {
    Load(String),
    InvalidRequest(String),
    Observation(String),
    Serialize(String),
}
impl std::fmt::Display for SourceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (kind, message) = match self {
            Self::Load(m) => ("load", m),
            Self::InvalidRequest(m) => ("invalid request", m),
            Self::Observation(m) => ("observation", m),
            Self::Serialize(m) => ("serialize", m),
        };
        write!(f, "{kind}: {message}")
    }
}
impl std::error::Error for SourceError {}
fn raw(json: String) -> Result<Box<RawValue>, SourceError> {
    RawValue::from_string(json).map_err(|e| SourceError::Serialize(e.to_string()))
}

/// One worker's immutable world and cached scientific observation contexts.
pub struct Source {
    _world_bytes: Vec<u8>,
    world: World,
    binding: Binding,
    astronomy: AstronomyContext,
    terrain: Option<SceneContext>,
    system: Box<RawValue>,
    moons: Box<RawValue>,
    initial: BTreeMap<u32, String>,
}
impl Source {
    /// Read bytes once, hash and parse the same input, then register concepts.
    /// Revision must be a full lowercase Git SHA-1, source ID nonempty.
    pub fn open(world_path: &Path, revision: &str, source_id: &str) -> Result<Self, SourceError> {
        if revision.len() != 40
            || !revision
                .bytes()
                .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
        {
            return Err(SourceError::InvalidRequest(
                "source revision must be a full lowercase 40-character Git SHA-1".into(),
            ));
        }
        if source_id.trim().is_empty() {
            return Err(SourceError::InvalidRequest(
                "source_id must not be empty".into(),
            ));
        }
        let bytes = std::fs::read(world_path)
            .map_err(|e| SourceError::Load(format!("{}: {e}", world_path.display())))?;
        let text = std::str::from_utf8(&bytes).map_err(|e| SourceError::Load(e.to_string()))?;
        let mut world = World::from_json(text).map_err(|e| SourceError::Load(e.to_string()))?;
        hornvale_worldgen::register_all(&mut world.registry)
            .map_err(|e| SourceError::Load(e.to_string()))?;
        let astronomy = native_build!(0, AstronomyContext::build(&world))
            .map_err(|e| SourceError::Observation(e.to_string()))?;
        let system = raw(hornvale_scene::system_json(
            &native_build!(1, hornvale_scene::system_scene(&world))
                .map_err(|e| SourceError::Observation(e.to_string()))?,
        ))?;
        let moons = raw(hornvale_scene::moons_json(
            &native_build!(2, hornvale_scene::moons_scene(&world))
                .map_err(|e| SourceError::Observation(e.to_string()))?,
        ))?;
        let binding = Binding {
            source_id: source_id.into(),
            scope_id: "scientific:unrestricted".into(),
            world_sha256: format!("{:x}", Sha256::digest(&bytes)),
            source_revision: revision.into(),
        };
        Ok(Self {
            _world_bytes: bytes,
            world,
            binding,
            astronomy,
            terrain: None,
            system,
            moons,
            initial: BTreeMap::new(),
        })
    }
    /// Static native documents cached by tile width, sharing one lazy terrain context.
    pub fn initial_document(&mut self, tile_width: u32) -> Result<String, SourceError> {
        if !(hornvale_scene::MIN_WIDTH..=hornvale_scene::MAX_WIDTH).contains(&tile_width)
            || !tile_width.is_multiple_of(2)
        {
            return Err(SourceError::InvalidRequest(format!(
                "tile width {tile_width} must be even and in {}..={}",
                hornvale_scene::MIN_WIDTH,
                hornvale_scene::MAX_WIDTH
            )));
        }
        if let Some(doc) = self.initial.get(&tile_width) {
            return Ok(doc.clone());
        }
        if self.terrain.is_none() {
            self.terrain = Some(
                native_build!(3, SceneContext::build(&self.world))
                    .map_err(|e| SourceError::Observation(e.to_string()))?,
            );
        }
        let tiles = native_build!(
            4,
            hornvale_scene::tiles_scene_in(
                &self.world,
                self.terrain.as_ref().expect("terrain initialized"),
                tile_width
            )
        )
        .map_err(|e| SourceError::Observation(e.to_string()))?;
        let tiles = raw(hornvale_scene::scene_json(&tiles))?;
        let doc = serde_json::to_string(&Initial {
            schema: "visual/initial/v1",
            binding: &self.binding,
            system: &self.system,
            moons: &self.moons,
            tiles: &tiles,
            ticks_per_std_day: WorldTime::TICKS_PER_STD_DAY,
        })
        .map_err(|e| SourceError::Serialize(e.to_string()))?;
        self.initial.insert(tile_width, doc.clone());
        Ok(doc)
    }
    /// Validate all identities and query native exact ticks. RawValue preserves
    /// the astronomy document's native bytes and field order inside the reply.
    pub fn observe(&mut self, request_json: &str) -> Result<String, SourceError> {
        let request: Request = serde_json::from_str(request_json)
            .map_err(|e| SourceError::InvalidRequest(e.to_string()))?;
        if request.schema != "visual/request/v1" {
            return Err(SourceError::InvalidRequest(format!(
                "unsupported schema {:?}",
                request.schema
            )));
        }
        if request.binding != self.binding {
            return Err(SourceError::InvalidRequest("binding does not belong to this source (source/scope/world/revision must all match)".into()));
        }
        let scene = hornvale_scene::astronomy_at_scene_in(
            &self.astronomy,
            WorldTime::from_ticks(request.ticks),
        )
        .map_err(|e| SourceError::Observation(e.to_string()))?;
        let astronomy = raw(hornvale_scene::astronomy_at_json(&scene))?;
        serde_json::to_string(&Reply {
            schema: "visual/reply/v1",
            binding: &self.binding,
            request_id: request.request_id,
            ticks: request.ticks,
            astronomy: &astronomy,
        })
        .map_err(|e| SourceError::Serialize(e.to_string()))
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn cached_documents_and_contexts_are_constructed_once() {
        CONSTRUCTIONS.set([0; 5]);
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../../cli/tests/fixtures/world-seed-42.json");
        let mut source = Source::open(
            &path,
            "4e06e33492a82e245aec899559b8cdf33ac7fdcd",
            "cache-test",
        )
        .unwrap();
        assert_eq!(CONSTRUCTIONS.get(), [1, 1, 1, 0, 0]);
        let a = source.initial_document(16).unwrap();
        for ticks in [0, 100000, -1, 0] {
            let request = serde_json::json!({"schema":"visual/request/v1","binding":source.binding,"request_id":0,"ticks":ticks});
            source.observe(&request.to_string()).unwrap();
        }
        assert_eq!(a, source.initial_document(16).unwrap());
        source.initial_document(32).unwrap();
        assert_eq!(CONSTRUCTIONS.get(), [1, 1, 1, 1, 2]);
        assert_eq!(source.initial.len(), 2);
    }
}
