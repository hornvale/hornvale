//! The cave-kind roster, shared by every domain and window that names a
//! void by the lithologic process that opened it (decision 0517 clause
//! (a)). Terrain derives which kind a site carries and how deep it
//! reaches; climate's corpus names communities *of* these formations under
//! its own genus spellings; this module holds only the roster and the
//! scene-emission legend.

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

impl CaveKind {
    /// The three cave-kind names in stable order — the self-describing
    /// legend for scene emission (mirrors `WaterKind::LEGEND`).
    pub const LEGEND: [&'static str; 3] = ["karst", "lava-tube", "fracture"];

    /// Stable name, for scene emission.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn name(self) -> &'static str {
        match self {
            CaveKind::Karst => "karst",
            CaveKind::LavaTube => "lava-tube",
            CaveKind::Fracture => "fracture",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// LEGEND and name() are the same spelling contract stated twice; they
    /// must agree in order and content.
    #[test]
    fn legend_and_name_agree() {
        let named = [
            CaveKind::Karst.name(),
            CaveKind::LavaTube.name(),
            CaveKind::Fracture.name(),
        ];
        assert_eq!(named, CaveKind::LEGEND);
    }
}
