//! Native scientific/unrestricted source. Only serialized documents cross this
//! boundary; a renderer never receives a World or an astronomy provider.
mod protocol;
use hornvale_kernel::{World, WorldTime};
use hornvale_scene::{AstronomyContext, SceneContext};
use protocol::{
    Binding, Initial, Reply, Request, SurfaceReply, SurfaceRequest, SurfaceRevisionWire,
};
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

fn surface_revision_wire(revision: &hornvale_worldgen::SurfaceRevision) -> SurfaceRevisionWire {
    SurfaceRevisionWire {
        source_revision: revision.source_revision.clone(),
        algorithm_version: revision.algorithm_version.into(),
        configuration_hash_hex: revision
            .configuration_hash
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect(),
    }
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
                native_build!(
                    3,
                    SceneContext::build_with_source_revision(
                        &self.world,
                        &self.binding.source_revision
                    )
                )
                    .map_err(|e| SourceError::Observation(e.to_string()))?,
            );
        }
        let surface_revision = surface_revision_wire(
            self.terrain
                .as_ref()
                .expect("terrain initialized")
                .surface_revision(),
        );
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
            surface_revision,
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

    /// Validate and observe one canonical source-owned coherent surface patch.
    /// Binding and revision mismatches are rejected before patch realization.
    pub fn observe_surface(&mut self, request_json: &str) -> Result<String, SourceError> {
        let request: SurfaceRequest = serde_json::from_str(request_json)
            .map_err(|e| SourceError::InvalidRequest(e.to_string()))?;
        if request.schema != "visual/surface-request/v1" {
            return Err(SourceError::InvalidRequest(format!(
                "unsupported surface schema {:?}",
                request.schema
            )));
        }
        if request.binding != self.binding {
            return Err(SourceError::InvalidRequest(
                "binding does not belong to this source (source/scope/world/revision must all match)".into(),
            ));
        }
        let revision = hornvale_worldgen::SurfaceRealizationContext::revision_for(
            &self.world,
            &self.binding.source_revision,
        );
        let expected_hash = revision
            .configuration_hash
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>();
        if request.expected_revision.source_revision != revision.source_revision
            || request.expected_revision.algorithm_version != revision.algorithm_version
            || request.expected_revision.configuration_hash_hex != expected_hash
        {
            return Err(SourceError::InvalidRequest(
                "expected surface revision does not match the active source".into(),
            ));
        }
        if self.terrain.is_none() {
            self.terrain = Some(
                native_build!(
                    3,
                    SceneContext::build_with_source_revision(
                        &self.world,
                        &self.binding.source_revision
                    )
                )
                    .map_err(|e| SourceError::Observation(e.to_string()))?,
            );
        }
        let context = self.terrain.as_ref().expect("terrain initialized");
        let query = hornvale_scene::surface_patch_query_from_packed(
            request.address.macro_face,
            request.address.child_path,
            revision,
        )
        .map_err(|e| SourceError::InvalidRequest(e.to_string()))?;
        let patch = hornvale_scene::surface_patch_scene(
            context,
            &query,
        )
        .map_err(|e| SourceError::Observation(e.to_string()))?;
        let patch = raw(hornvale_scene::surface_patch_json(&patch))?;
        serde_json::to_string(&SurfaceReply {
            schema: "visual/surface-reply/v1",
            binding: &self.binding,
            request_id: request.request_id,
            patch: &patch,
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

    #[test]
    fn stale_surface_revision_is_rejected_before_context_build() {
        CONSTRUCTIONS.set([0; 5]);
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../../cli/tests/fixtures/world-seed-42.json");
        let mut source = Source::open(
            &path,
            "4e06e33492a82e245aec899559b8cdf33ac7fdcd",
            "early-rejection-test",
        )
        .unwrap();
        assert_eq!(CONSTRUCTIONS.get(), [1, 1, 1, 0, 0]);
        let request = serde_json::json!({
            "schema": "visual/surface-request/v1",
            "binding": source.binding,
            "request_id": 1,
            "address": {"macro_face": 0, "child_path": [1]},
            "expected_revision": {
                "source_revision": "0",
                "algorithm_version": "stale",
                "configuration_hash_hex": "0"
            }
        });

        let error = source.observe_surface(&request.to_string()).unwrap_err();
        assert!(matches!(error, SourceError::InvalidRequest(message) if message.contains("revision")));
        assert_eq!(CONSTRUCTIONS.get(), [1, 1, 1, 0, 0]);
    }
}
