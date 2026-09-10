//! Pure Waterworld observation detail, populated in Stage 4.

/// Waterworld observation detail.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WaterWorldDetail {
    /// Whole-world context.
    Planet,
    /// Regional marine context.
    Regional,
    /// One marine habitat column.
    Habitat,
}
