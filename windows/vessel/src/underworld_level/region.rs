//! The one primitive left from the Adit's partition tree: `cut`, which
//! `carve.rs`'s partitioned-rooms carver still uses. The level scaffold is
//! the plan's region grid since The Crosscut
//! (`hornvale_worldgen::circuit`).

use hornvale_kernel::Stream;

use crate::lattice::Rect;

/// Split `r` along its longer axis, leaving a one-cell gap for the
/// dividing wall — the same shape as `lattice::allocate`'s private `split`,
/// generalized to take its own minimum span so `carve.rs`'s room-scale
/// subdivision (Task 3) can reuse it with a smaller value.
pub(super) fn cut(r: Rect, min_span: i32, stream: &mut Stream, dof: &mut u32) -> (Rect, Rect) {
    let horizontal = r.w >= r.h;
    let span = if horizontal { r.w } else { r.h };
    let lo = min_span;
    let hi = span - 1 - min_span;
    let at = if hi <= lo {
        lo
    } else {
        let width = (hi - lo + 1) as u64;
        let drawn = stream.next_u64();
        *dof += 1;
        lo + (drawn % width) as i32
    };
    if horizontal {
        (
            Rect {
                x: r.x,
                y: r.y,
                w: at,
                h: r.h,
            },
            Rect {
                x: r.x + at + 1,
                y: r.y,
                w: r.w - at - 1,
                h: r.h,
            },
        )
    } else {
        (
            Rect {
                x: r.x,
                y: r.y,
                w: r.w,
                h: at,
            },
            Rect {
                x: r.x,
                y: r.y + at + 1,
                w: r.w,
                h: r.h - at - 1,
            },
        )
    }
}
