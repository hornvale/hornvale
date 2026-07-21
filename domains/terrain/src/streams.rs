//! Seed-derivation stream labels for tectonic genesis. Labels are permanent
//! save-format contracts; deliberate regeneration uses an epoch suffix
//! (`terrain/plate-count/v2`), never a rename.
//!
//! Retired labels (never reuse, never redraw): "plate-kind" (epoch v2 —
//! superseded by "cratons"; Crust spec §9).

use hornvale_kernel::seed::StreamLabel;

/// Root stream: every terrain chain hangs off `world_seed.derive(ROOT)`.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOT: StreamLabel<'static> = StreamLabel::from_static("terrain");
/// Plate count draw.
/// type-audit: bare-ok(identifier-text: return)
pub const PLATE_COUNT: StreamLabel<'static> = StreamLabel::from_static("plate-count");
/// Per-plate seed-position draws (two `next_f64` per plate, sequential).
/// type-audit: bare-ok(identifier-text: return)
pub const PLATE_SEEDS: StreamLabel<'static> = StreamLabel::from_static("plate-seeds");
/// Per-plate Euler pole draws (axis then rate, sequential).
/// type-audit: bare-ok(identifier-text: return)
pub const PLATE_MOTION: StreamLabel<'static> = StreamLabel::from_static("plate-motion");
/// Per-plate orogenic maturity draws.
/// type-audit: bare-ok(identifier-text: return)
pub const MATURITY: StreamLabel<'static> = StreamLabel::from_static("maturity");
/// Hotspot count, then per-hotspot position and strength draws.
/// type-audit: bare-ok(identifier-text: return)
pub const HOTSPOTS: StreamLabel<'static> = StreamLabel::from_static("hotspots");
/// Target ocean fraction draw.
/// type-audit: bare-ok(identifier-text: return)
pub const OCEAN_FRACTION: StreamLabel<'static> = StreamLabel::from_static("ocean-fraction");
/// Render-lens coastline noise. Hash-noise only — never consumed as a
/// `Stream`; the lens draws nothing (Campaign 25 spec §3).
/// type-audit: bare-ok(identifier-text: return)
pub const COAST_RENDER: StreamLabel<'static> = StreamLabel::from_static("coast-render");
/// Craton draws: a margin draw (Task 9 iteration 3' reinterpretation —
/// originally an independent area-budget fraction; now scales the
/// ocean-fraction-derived budget instead, same single draw, see
/// `crust::draw_cratons`'s doc), craton count (budget-scaled, no draw of
/// its own), then per-craton center (two draws), radius, age —
/// sequential. Draw counts are a save-format contract.
/// type-audit: bare-ok(identifier-text: return)
pub const CRATONS: StreamLabel<'static> = StreamLabel::from_static("cratons");
/// Per-plate Voronoi weight draws (heavy-tailed; one per plate).
/// type-audit: bare-ok(identifier-text: return)
pub const PLATE_WEIGHTS: StreamLabel<'static> = StreamLabel::from_static("plate-weights");
/// Plate-edge noise. Hash-noise only — never consumed as a `Stream`.
/// type-audit: bare-ok(identifier-text: return)
pub const PLATE_EDGE: StreamLabel<'static> = StreamLabel::from_static("plate-edge");
/// Lithology sub-cell patchiness noise (The Ground, spec §2). Hash-noise
/// only — never consumed as a `Stream`; no draw-order/save-format contract.
/// type-audit: bare-ok(identifier-text: return)
pub const LITHOLOGY: StreamLabel<'static> = StreamLabel::from_static("lithology");
/// Terrane draws (Sculpting, spec §3): count, then per terrane a host
/// craton index, rim bearing, size, age. A NEW label — existing stream
/// consumption order is untouched (epoch v3 save-format contract).
/// type-audit: bare-ok(identifier-text: return)
pub const TERRANES: StreamLabel<'static> = StreamLabel::from_static("terranes");
/// Microcontinent draws (Sculpting, spec §3): a NEW label — a fixed
/// candidate count (`crust::MICRO_COUNT_MAX`), then per candidate a
/// position (two draws via `unit_vector`), a radius, and an age,
/// sequential. Every candidate is drawn in full regardless of whether it
/// survives the away-from-majors filter (no rejection loop, no
/// draw-count variance — pin-isolation discipline, mirroring
/// `TERRANES`'s own no-rejection framing for its bearing draws). Existing
/// stream consumption order is untouched (epoch v3 save-format contract).
/// type-audit: bare-ok(identifier-text: return)
pub const MICROCONTINENTS: StreamLabel<'static> = StreamLabel::from_static("microcontinents");
/// Along-strike arc gating noise (Sculpting, spec §3): gates island-arc
/// and coastal-range edifices into discrete chains instead of continuous
/// walls. Hash-noise only — never consumed as a `Stream`, so it carries
/// no draw-order/save-format contract (see `elevation::boundary_profile_m`).
/// type-audit: bare-ok(identifier-text: return)
pub const ARC_GATE: StreamLabel<'static> = StreamLabel::from_static("arc-gate");
/// fBm relief noise (Sculpting, spec §3): zero-mean multi-octave detail
/// added in `assemble_elevation`, amplitude scaled by induration and belt
/// proximity (see `elevation::relief_scale`). Hash-noise only — never
/// consumed as a `Stream`, so it carries no draw-order/save-format
/// contract, mirroring `ARC_GATE`.
/// type-audit: bare-ok(identifier-text: return)
pub const RELIEF: StreamLabel<'static> = StreamLabel::from_static("relief");
/// Rift draws (rift-and-fit, spec §3): ONE global spreading-rate draw.
/// Per-seam geometry uses hash sub-derivations (`seam-{a}-{b}`) — no
/// sequential draws, no draw-count variance. A NEW label — epoch v4
/// save-format contract.
/// type-audit: bare-ok(identifier-text: return)
pub const RIFT: StreamLabel<'static> = StreamLabel::from_static("rift");
/// The lobing-noise sub-leg under a craton's own stream (spec: two
/// production call sites, `crust.rs:294` and `crust.rs:986`, share this
/// exact literal today).
/// type-audit: bare-ok(identifier-text: return)
pub const LOBING: StreamLabel<'static> = StreamLabel::from_static("lobing");
/// The first of three orthogonal noise slices shared between crust
/// sculpting and its render lens (`crust.rs` and `render.rs`).
/// type-audit: bare-ok(identifier-text: return)
pub const CRUST_SLICE_0: StreamLabel<'static> = StreamLabel::from_static("slice-0");
/// The second of three orthogonal noise slices (see [`CRUST_SLICE_0`]).
/// type-audit: bare-ok(identifier-text: return)
pub const CRUST_SLICE_1: StreamLabel<'static> = StreamLabel::from_static("slice-1");
/// The third of three orthogonal noise slices (see [`CRUST_SLICE_0`]).
/// type-audit: bare-ok(identifier-text: return)
pub const CRUST_SLICE_2: StreamLabel<'static> = StreamLabel::from_static("slice-2");
/// The rift's crenulation-noise sub-leg.
/// type-audit: bare-ok(identifier-text: return)
pub const RIFT_CRENULATION: StreamLabel<'static> = StreamLabel::from_static("crenulation");
