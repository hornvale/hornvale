//! Seed-derivation labels for the kernel — permanent save-format contracts
//! (The Room Mesh spec §6/§10). Changing one silently corrupts every world.

use crate::seed::StreamLabel;

/// Root label for a room's base face.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_FACE: StreamLabel<'static> = StreamLabel::from_static("room/face");
/// Label for a room's child descent.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_CHILD: StreamLabel<'static> = StreamLabel::from_static("room/child");

/// Static per-octave derivation labels for `noise::fbm_2d`'s common octave
/// range, indexed by octave number (index 0 is a placeholder — octave 0
/// uses the seed directly and never derives). Byte-for-byte the labels
/// `format!("octave-{n}")` produces, so the derived seeds — and every
/// noise value — are bit-identical; the table only removes the per-call
/// `format!` (a heap-allocated `String` per octave per sample), which a
/// Task 6 profiling pass measured at ~52% of whole-world generation time
/// (fbm is called per octave per slice per field sample). Octave counts
/// beyond the table fall back to `format!` with the same label scheme,
/// preserving exact semantics for any count. Save-format contract: never
/// reorder or re-word an entry.
/// type-audit: bare-ok(artifact: return)
pub const OCTAVE_LABELS: [StreamLabel<'static>; 16] = [
    StreamLabel::from_static("octave-0"),
    StreamLabel::from_static("octave-1"),
    StreamLabel::from_static("octave-2"),
    StreamLabel::from_static("octave-3"),
    StreamLabel::from_static("octave-4"),
    StreamLabel::from_static("octave-5"),
    StreamLabel::from_static("octave-6"),
    StreamLabel::from_static("octave-7"),
    StreamLabel::from_static("octave-8"),
    StreamLabel::from_static("octave-9"),
    StreamLabel::from_static("octave-10"),
    StreamLabel::from_static("octave-11"),
    StreamLabel::from_static("octave-12"),
    StreamLabel::from_static("octave-13"),
    StreamLabel::from_static("octave-14"),
    StreamLabel::from_static("octave-15"),
];

/// Every kernel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (ROOM_FACE.as_str(), "room base face"),
        (ROOM_CHILD.as_str(), "room child descent"),
    ]
}
