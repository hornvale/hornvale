//! Located subsurface features — caves and ore deposits (The Lode). Typed
//! bodies derived from the fluid-flow fields terrain already owns, living in
//! The Deep's column. A deterministic hash-noise point process: no sequential
//! stream draws, no committed facts, no epoch. Mundane only — magical ores are
//! metaphysics-gated and stay reserved.

use crate::lithology::MaterialBuffer;

/// A cave type, by the lithologic process that opened the void.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaveKind {
    /// Carbonate dissolution (wet limestone).
    Karst,
    /// Drained basaltic/volcanic tube.
    LavaTube,
    /// Fault/fracture void in tectonically active rock.
    Fracture,
}

/// A located cave at a cell.
/// type-audit: bare-ok(count: depth_reach_bands)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cave {
    /// Which process opened it.
    pub kind: CaveKind,
    /// How many of the column's bands the void penetrates (1 = cover only).
    pub depth_reach_bands: u32,
}

/// The cave type implied by lithology: carbonate dissolves to karst, mafic
/// (low-silica) volcanic drains to lava-tube, else a fault fracture.
/// type-audit: bare-ok(flag: near_fault)
pub fn cave_kind(buf: &MaterialBuffer, near_fault: bool) -> CaveKind {
    if buf.carbonate > 0.5 {
        CaveKind::Karst
    } else if buf.silica < 0.3 {
        CaveKind::LavaTube
    } else {
        let _ = near_fault; // fracture is the fallback; near_fault reinforces presence, not kind
        CaveKind::Fracture
    }
}

/// Lineament proximity weight: features cluster into belts near plate contacts.
/// `hops` is boundary distance (fewer = closer); `None` = cratonic interior,
/// which is the floor — boundaries only *raise* the weight above it, so a
/// far-from-boundary cell never scores below the interior (the `.max` floor).
/// type-audit: bare-ok(count: hops), bare-ok(ratio: return)
pub fn belt_weight(hops: Option<u32>) -> f64 {
    const INTERIOR_FLOOR: f64 = 0.3;
    match hops {
        Some(h) => (1.0 / (1.0 + h as f64 * 0.1)).max(INTERIOR_FLOOR),
        None => INTERIOR_FLOOR,
    }
}

/// Belt-weighted presence probability for a feature, from its gating field.
/// type-audit: bare-ok(ratio: field), bare-ok(ratio: belt), bare-ok(ratio: return)
pub fn presence_prob(field: f64, belt: f64) -> f64 {
    (field * (0.4 + 0.6 * belt)).clamp(0.0, 1.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lithology::{Basement, MarginPolarity, MaterialBuffer, SoilDepth};

    fn buf(carbonate: f64, silica: f64) -> MaterialBuffer {
        MaterialBuffer {
            silica,
            grain: 0.5,
            induration: 0.5,
            carbonate,
            metamorphic_grade: 0.0,
            porosity: 0.5,
            margin: MarginPolarity::Interior,
            soil_depth: SoilDepth::new(1.0),
            basement: Basement::Continental,
            thaumic: 0.0,
        }
    }

    #[test]
    fn cave_kind_reads_lithology() {
        assert_eq!(cave_kind(&buf(0.8, 0.5), false), CaveKind::Karst);
        assert_eq!(cave_kind(&buf(0.1, 0.15), false), CaveKind::LavaTube);
        assert_eq!(cave_kind(&buf(0.1, 0.6), true), CaveKind::Fracture);
    }

    #[test]
    fn belt_weight_is_higher_near_lineaments() {
        assert!(belt_weight(Some(0)) > belt_weight(Some(8)));
        assert!(belt_weight(Some(8)) > belt_weight(None));
        // Far-from-boundary cells never dip below the cratonic-interior floor
        // (max boundary distance is ~49 hops at GLOBE_LEVEL 6).
        assert!(belt_weight(Some(30)) >= belt_weight(None));
        assert!(belt_weight(Some(49)) >= belt_weight(None));
    }
}
