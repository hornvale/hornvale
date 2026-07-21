//! Seed-derivation labels for the kernel — permanent save-format contracts
//! (The Room Mesh spec §6/§10). Changing one silently corrupts every world.

use crate::seed::StreamLabel;

/// Root label for a room's base face.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_FACE: StreamLabel<'static> = StreamLabel::from_static("room/face");
/// Label for a room's child descent.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_CHILD: StreamLabel<'static> = StreamLabel::from_static("room/child");

/// Every kernel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (ROOM_FACE.as_str(), "room base face"),
        (ROOM_CHILD.as_str(), "room child descent"),
    ]
}
