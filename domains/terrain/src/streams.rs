//! Seed-derivation stream labels for tectonic genesis. Labels are permanent
//! save-format contracts; deliberate regeneration uses an epoch suffix
//! (`terrain/plate-count/v2`), never a rename.
//!
//! Retired labels (never reuse, never redraw): "plate-kind" (epoch v2 —
//! superseded by "cratons"; Crust spec §9).
//!
//! Declared via `stream_labels!` (PROC-18), which also generates
//! `stream_labels()` (moved here from `lib.rs`). This closed a live gap:
//! `stream_labels()` previously published only 17 of these 22 entries —
//! `LOBING`, `CRUST_SLICE_0`, `CRUST_SLICE_1`, `CRUST_SLICE_2`, and
//! `RIFT_CRENULATION` were undocumented in the generated manifest.

hornvale_kernel::stream_labels! {
    /// Root stream: every terrain chain hangs off `world_seed.derive(ROOT)`.
    root: ROOT = "terrain" => "root stream for tectonic genesis";
    legs {
        /// Plate count draw.
        PLATE_COUNT = "plate-count" => "how many plates";
        /// Per-plate seed-position draws (two `next_f64` per plate, sequential).
        PLATE_SEEDS = "plate-seeds" => "per-plate seed positions on the sphere";
        /// Per-plate Euler pole draws (axis then rate, sequential).
        PLATE_MOTION = "plate-motion" => "per-plate Euler pole axis and rate draws";
        /// Per-plate orogenic maturity draws.
        MATURITY = "maturity" => "per-plate orogenic maturity draws";
        /// Hotspot count, then per-hotspot position and strength draws.
        HOTSPOTS = "hotspots" => "hotspot count, positions, and strengths";
        /// Target ocean fraction draw.
        OCEAN_FRACTION = "ocean-fraction" => "target ocean fraction draw";
        /// Render-lens coastline noise. Hash-noise only — never consumed as a
        /// `Stream`; the lens draws nothing (Campaign 25 spec §3).
        COAST_RENDER = "coast-render" => "render-lens coastline noise (hash-noise only; no stream draws)";
        /// Craton draws: a margin draw (Task 9 iteration 3' reinterpretation —
        /// originally an independent area-budget fraction; now scales the
        /// ocean-fraction-derived budget instead, same single draw, see
        /// `crust::draw_cratons`'s doc), craton count (budget-scaled, no draw of
        /// its own), then per-craton center (two draws), radius, age —
        /// sequential. Draw counts are a save-format contract.
        CRATONS = "cratons" => "margin draw (scales the ocean-fraction-derived budget, Task 9 iteration 3'), craton count, then per-craton center/radius/age";
        /// Per-plate Voronoi weight draws (heavy-tailed; one per plate).
        PLATE_WEIGHTS = "plate-weights" => "per-plate heavy-tailed Voronoi weight draws";
        /// Plate-edge noise. Hash-noise only — never consumed as a `Stream`.
        PLATE_EDGE = "plate-edge" => "plate-edge noise (hash-noise only; no stream draws)";
        /// Lithology sub-cell patchiness noise (The Ground, spec §2). Hash-noise
        /// only — never consumed as a `Stream`; no draw-order/save-format contract.
        LITHOLOGY = "lithology" => "lithology sub-cell hash-noise (hash-noise only; no stream draws)";
        /// Subsurface-features point-process noise (The Lode, spec §6).
        /// Hash-noise only — never consumed as a `Stream`; no draw-order /
        /// save-format contract. A NEW label — existing consumption order is
        /// untouched.
        FEATURES = "features" => "subsurface features point-process hash-noise (hash-noise only; no stream draws)";
        /// Terrane draws (Sculpting, spec §3): count, then per terrane a host
        /// craton index, rim bearing, size, age. A NEW label — existing stream
        /// consumption order is untouched (epoch v3 save-format contract).
        TERRANES = "terranes" => "terrane count, then per terrane host-craton index/bearing/size/age";
        /// Microcontinent draws (Sculpting, spec §3): a NEW label — a fixed
        /// candidate count (`crust::MICRO_COUNT_MAX`), then per candidate a
        /// position (two draws via `unit_vector`), a radius, and an age,
        /// sequential. Every candidate is drawn in full regardless of whether it
        /// survives the away-from-majors filter (no rejection loop, no
        /// draw-count variance — pin-isolation discipline, mirroring
        /// `TERRANES`'s own no-rejection framing for its bearing draws). Existing
        /// stream consumption order is untouched (epoch v3 save-format contract).
        MICROCONTINENTS = "microcontinents" => "fixed candidate count, then per candidate position/radius/age";
        /// Along-strike arc gating noise (Sculpting, spec §3): gates island-arc
        /// and coastal-range edifices into discrete chains instead of continuous
        /// walls. Hash-noise only — never consumed as a `Stream`, so it carries
        /// no draw-order/save-format contract (see `elevation::boundary_profile_m`).
        ARC_GATE = "arc-gate" => "along-strike island-arc gating noise (hash-noise only; no stream draws)";
        /// fBm relief noise (Sculpting, spec §3): zero-mean multi-octave detail
        /// added in `assemble_elevation`, amplitude scaled by induration and belt
        /// proximity (see `elevation::relief_scale`). Hash-noise only — never
        /// consumed as a `Stream`, so it carries no draw-order/save-format
        /// contract, mirroring `ARC_GATE`.
        RELIEF = "relief" => "fBm relief-detail noise (hash-noise only; no stream draws)";
        /// Rift draws (rift-and-fit, spec §3): ONE global spreading-rate draw.
        /// Per-seam geometry uses hash sub-derivations (`seam-{a}-{b}`) — no
        /// sequential draws, no draw-count variance. A NEW label — epoch v4
        /// save-format contract.
        RIFT = "rift" => "ONE spreading-rate draw; per-seam geometry via hash sub-derivations (seam-{a}-{b}); no other sequential draws";
        /// The lobing-noise sub-leg under a craton's own stream (spec: two
        /// production call sites, `crust.rs:294` and `crust.rs:986`, share this
        /// exact literal today).
        LOBING = "lobing" => "craton lobing-noise sub-leg (hash-noise only; no stream draws)";
        /// The first of three orthogonal noise slices shared between crust
        /// sculpting and its render lens (`crust.rs` and `render.rs`).
        CRUST_SLICE_0 = "slice-0" => "first of three orthogonal crust noise slices, shared with the render lens (hash-noise only; no stream draws)";
        /// The second of three orthogonal noise slices (see `CRUST_SLICE_0`).
        CRUST_SLICE_1 = "slice-1" => "second of three orthogonal crust noise slices (hash-noise only; no stream draws)";
        /// The third of three orthogonal noise slices (see `CRUST_SLICE_0`).
        CRUST_SLICE_2 = "slice-2" => "third of three orthogonal crust noise slices (hash-noise only; no stream draws)";
        /// The rift's crenulation-noise sub-leg.
        RIFT_CRENULATION = "crenulation" => "rift crenulation-noise sub-leg (hash-noise only; no stream draws)";
    }
}
