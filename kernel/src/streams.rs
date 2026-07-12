//! Seed-derivation labels for the kernel — permanent save-format contracts
//! (The Room Mesh spec §6/§10). Changing one silently corrupts every world.

/// Root label for a room's base face.
/// type-audit: bare-ok(identifier-text)
pub const ROOM_FACE: &str = "room/face";
/// Label for a room's child descent.
/// type-audit: bare-ok(identifier-text)
pub const ROOM_CHILD: &str = "room/child";

/// Every kernel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (ROOM_FACE, "room base face"),
        (ROOM_CHILD, "room child descent"),
    ]
}
