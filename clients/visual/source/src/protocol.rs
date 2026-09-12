use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Binding {
    pub source_id: String,
    pub scope_id: String,
    pub world_sha256: String,
    pub source_revision: String,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct Request {
    pub schema: String,
    pub binding: Binding,
    pub request_id: u64,
    pub ticks: i64,
}
#[derive(Serialize)]
pub(crate) struct Reply<'a> {
    pub schema: &'static str,
    pub binding: &'a Binding,
    pub request_id: u64,
    pub ticks: i64,
    pub astronomy: &'a RawValue,
}
#[derive(Serialize)]
pub(crate) struct Initial<'a> {
    pub schema: &'static str,
    pub binding: &'a Binding,
    pub system: &'a RawValue,
    pub moons: &'a RawValue,
    pub tiles: &'a RawValue,
    pub ticks_per_std_day: i64,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct SurfaceRequest {
    pub schema: String,
    pub binding: Binding,
    pub request_id: u64,
    pub address: FacetAddressWire,
    pub expected_revision: SurfaceRevisionWire,
}

#[derive(Serialize)]
pub(crate) struct SurfaceReply<'a> {
    pub schema: &'static str,
    pub binding: &'a Binding,
    pub request_id: u64,
    pub patch: &'a RawValue,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FacetAddressWire {
    pub macro_face: u32,
    pub child_path: Vec<u8>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct SurfaceRevisionWire {
    pub source_revision: String,
    pub algorithm_version: String,
    pub configuration_hash_hex: String,
}
