//! The rate spine: a layer's rate is its cache key (spec §4.3, extending
//! decision 0289).
//!
//! **The invariant this module exists for:** a layer may never read data
//! that changes faster than its own rate. Without it a seasonal quantity
//! baked into the never-invalidated terrain cache renders correctly on the
//! first frame and is frozen forever after — a defect that is nearly
//! invisible because the first frame is right.

/// How often a layer's cache key changes. Ordered coarse-to-fine: a larger
/// discriminant changes more often.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rate {
    /// Never invalidated within a world.
    Geological,
    /// Changes when something is built or destroyed.
    Built,
    /// Changes at a season boundary.
    Seasonal,
    /// Changes as the sun or moons move.
    Diurnal,
    /// Changes once per turn.
    PerTurn,
    /// Changes on the marquee tick (~300 ms).
    Ornamental,
    /// Changes on cursor movement.
    Instantaneous,
}

/// One layer's declaration: its own rate, and the rates of the data it reads.
#[derive(Debug, Clone, Copy)]
pub struct LayerDecl {
    /// The layer's name, for the violation message.
    pub name: &'static str,
    /// The rate at which this layer's cache key changes.
    pub rate: Rate,
    /// The rates of every input this layer reads.
    pub reads: &'static [Rate],
}

/// Every layer reading data faster-changing than its own rate, as messages.
/// Empty means the spine holds.
pub fn violations(layers: &[LayerDecl]) -> Vec<String> {
    let mut out = Vec::new();
    for l in layers {
        for r in l.reads {
            if *r > l.rate {
                out.push(format!(
                    "layer {:?} is {:?} but reads {:?} data, which changes faster",
                    l.name, l.rate, r
                ));
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// FIRES WHEN: a layer declares it reads data that changes faster than
    /// its own cache key. That is the defect the spine exists to prevent —
    /// a seasonal reflectance baked into a never-invalidated terrain cache
    /// renders correctly on the first frame and is frozen thereafter.
    #[test]
    fn a_layer_reading_faster_data_than_its_own_rate_is_a_violation() {
        let bad = LayerDecl {
            name: "terrain",
            rate: Rate::Geological,
            reads: &[Rate::Seasonal],
        };
        let found = violations(&[bad]);
        assert_eq!(
            found.len(),
            1,
            "expected exactly one violation, got {found:?}"
        );
        assert!(
            found[0].contains("terrain"),
            "the message must name the offending layer, got {:?}",
            found[0]
        );
    }

    /// The non-vacuity arm: a correctly declared layer must produce NOTHING,
    /// or the check above would pass on any input at all.
    #[test]
    fn a_layer_reading_only_slower_or_equal_data_is_clean() {
        let good = LayerDecl {
            name: "terrain",
            rate: Rate::Seasonal,
            reads: &[Rate::Geological, Rate::Seasonal],
        };
        assert_eq!(violations(&[good]), Vec::<String>::new());
    }
}
