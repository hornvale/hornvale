//! The partition-tree scaffold: a level is a rectangle that is either one
//! leaf (filled by one content generator, `carve.rs`) or split into two
//! children, each independently leaf-or-split — BSP-of-BSP (spec §4.2).
//! Bounded by whichever fires first: a minimum region span (mirroring
//! `lattice::allocate`'s `MIN_CHAMBER_SPAN`, sized for a leaf's own
//! content rather than a building chamber) or a hard depth ceiling of 2.

use hornvale_kernel::{Seed, Stream};

use crate::lattice::Rect;

/// Minimum span (either axis) a region must have to be eligible for
/// further splitting. Chosen larger than `lattice::allocate::MIN_CHAMBER_SPAN`
/// (2) because a leaf here must hold generated dungeon content, not just be
/// walkable.
/// plumb: universal(a fixed geometry bound for splittable dungeon regions)
pub(super) const MIN_REGION_SPAN: i32 = 8;

/// Hard ceiling on composite-region nesting depth (spec §4.2) — past this,
/// nested regions read as visual noise rather than distinct places sharing
/// a level. Matches the deepest case in this campaign's own worked example
/// (cave / mine / outpost).
/// plumb: universal(a fixed nesting-depth cap matched to the campaign's own worked example)
pub(super) const MAX_COMPOSITE_DEPTH: u32 = 2;

/// A node in the partition tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum Region {
    /// Fill this rectangle with one content generator.
    Leaf(Rect),
    /// Partition this rectangle; recurse into both halves.
    Split(Box<Region>, Box<Region>),
}

/// Probability of splitting rather than stopping, before the span/depth
/// bounds are applied. Halves each additional depth so a depth-2 split is
/// rare rather than the common case.
fn split_probability(depth: u32) -> f64 {
    match depth {
        0 => 0.35,
        1 => 0.12,
        _ => 0.0,
    }
}

/// Build the partition tree for `extent`, drawing from `extent`'s own
/// derived stream. Returns the tree and the total number of draws made.
pub(super) fn build_region(extent: Rect, seed: Seed) -> (Region, u32) {
    let mut stream = seed
        .derive(crate::streams::UNDERWORLD_LEVEL_PARTITION)
        .stream();
    let mut dof = 0u32;
    let region = build_node(extent, 0, &mut stream, &mut dof);
    (region, dof)
}

fn build_node(r: Rect, depth: u32, stream: &mut Stream, dof: &mut u32) -> Region {
    let shorter = r.w.min(r.h);
    let can_split = depth < MAX_COMPOSITE_DEPTH && shorter > 2 * MIN_REGION_SPAN;
    if can_split {
        let roll = stream.next_f64();
        *dof += 1;
        if roll < split_probability(depth) {
            let (a, b) = cut(r, MIN_REGION_SPAN, stream, dof);
            return Region::Split(
                Box::new(build_node(a, depth + 1, stream, dof)),
                Box::new(build_node(b, depth + 1, stream, dof)),
            );
        }
    }
    Region::Leaf(r)
}

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

/// Every leaf rectangle, in generation (pre-order) traversal order — the
/// order Task 4's water-table basin choice and Task 7's stairs placement
/// both rely on ("the first leaf" is well-defined).
pub(super) fn leaves(region: &Region) -> Vec<Rect> {
    let mut out = Vec::new();
    collect_leaves(region, &mut out);
    out
}

fn collect_leaves(region: &Region, out: &mut Vec<Rect>) {
    match region {
        Region::Leaf(r) => out.push(*r),
        Region::Split(a, b) => {
            collect_leaves(a, out);
            collect_leaves(b, out);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    const EXTENT: Rect = Rect {
        x: 0,
        y: 0,
        w: 40,
        h: 24,
    };

    /// claim: invariant(seed: 0..20) — the depth ceiling holds for every
    /// partition tree the loop builds, not merely a spot-checked one.
    #[test]
    fn never_exceeds_the_depth_ceiling() {
        fn max_depth(region: &Region, depth: u32) -> u32 {
            match region {
                Region::Leaf(_) => depth,
                Region::Split(a, b) => max_depth(a, depth + 1).max(max_depth(b, depth + 1)),
            }
        }
        for seed_value in 0..20u64 {
            let (region, _dof) = build_region(EXTENT, Seed(seed_value));
            assert!(
                max_depth(&region, 0) <= MAX_COMPOSITE_DEPTH,
                "seed {seed_value} exceeded the depth ceiling"
            );
        }
    }

    /// claim: invariant(seed: 0..20) — every leaf's span meets the minimum
    /// for every partition tree the loop builds, not merely a spot-checked
    /// one.
    #[test]
    fn every_leaf_meets_the_minimum_span() {
        for seed_value in 0..20u64 {
            let (region, _dof) = build_region(EXTENT, Seed(seed_value));
            for rect in leaves(&region) {
                assert!(
                    rect.w >= MIN_REGION_SPAN && rect.h >= MIN_REGION_SPAN,
                    "seed {seed_value} produced an under-span leaf {rect:?}"
                );
            }
        }
    }

    #[test]
    fn leaves_partition_the_extent_without_overlap() {
        let (region, _dof) = build_region(EXTENT, Seed(7));
        let mut covered = std::collections::BTreeSet::new();
        let mut area = 0i64;
        for rect in leaves(&region) {
            area += (rect.w * rect.h) as i64;
            for x in rect.x..(rect.x + rect.w) {
                for y in rect.y..(rect.y + rect.h) {
                    assert!(covered.insert((x, y)), "cell ({x},{y}) claimed twice");
                }
            }
        }
        assert!(
            area <= (EXTENT.w * EXTENT.h) as i64,
            "leaves cannot exceed the extent's area"
        );
    }

    #[test]
    fn is_deterministic() {
        let (a, dof_a) = build_region(EXTENT, Seed(99));
        let (b, dof_b) = build_region(EXTENT, Seed(99));
        assert_eq!(leaves(&a), leaves(&b));
        assert_eq!(dof_a, dof_b);
    }
}
