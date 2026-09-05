//! The within-tenure population shape (spec §4.1): between occupations the
//! committed integral is exact; within one, a rise-then-plateau with the
//! committed area, degrading to a triangle or a rectangle when no rise time
//! fits. The one place the lot interpolates, and it says which shape it used.

/// The bake's epoch length, in years — the one definition, read wherever the
/// brief's shape code had a bare `25.0`. Verified against
/// `hornvale_worldgen::BakeConfig::default_millennia().epoch_years` by a
/// test, so the two cannot silently drift.
/// plumb: per-world(the bake's epoch length, BakeConfig::epoch_years; 25 is default_millennia's value)
/// type-audit: bare-ok(count)
pub const EPOCH_YEARS: f64 = 25.0;

/// The reconstructed curve over `[founded, end]`.
/// type-audit: bare-ok(count: RisePlateau.founded), bare-ok(count: RisePlateau.end), bare-ok(count: RisePlateau.p0), bare-ok(count: RisePlateau.peak), bare-ok(count: RisePlateau.rise), bare-ok(count: Triangle.founded), bare-ok(count: Triangle.end), bare-ok(count: Triangle.p0), bare-ok(count: Triangle.apex), bare-ok(count: Rectangle.founded), bare-ok(count: Rectangle.end), bare-ok(count: Rectangle.level)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Shape {
    /// Linear rise from `p0` to `peak` over `rise` years, then flat.
    RisePlateau {
        /// The bake year the occupation began.
        founded: f64,
        /// The bake year the occupation ended (or the present).
        end: f64,
        /// The opening population.
        p0: f64,
        /// The plateau population.
        peak: f64,
        /// Years spent rising from `p0` to `peak`.
        rise: f64,
    },
    /// Linear rise from `p0` to an apex at `end`, never reaching plateau.
    Triangle {
        /// The bake year the occupation began.
        founded: f64,
        /// The bake year the occupation ended.
        end: f64,
        /// The opening population.
        p0: f64,
        /// The population reached at `end`.
        apex: f64,
    },
    /// Flat at `level` — a zero-tenure occupation, or an area no rise fits.
    Rectangle {
        /// The bake year the occupation began.
        founded: f64,
        /// The bake year the occupation ended.
        end: f64,
        /// The constant population over the span.
        level: f64,
    },
}

/// Choose the shape for one occupation. `person_years` is the committed
/// integral, `p0` the bake's opening population for its founding kind.
/// type-audit: bare-ok(count: founded), bare-ok(count: end), bare-ok(count: peak), bare-ok(count: person_years), bare-ok(count: p0)
pub fn shape_of(founded: f64, end: f64, peak: u32, person_years: f64, p0: f64) -> Shape {
    let t = (end - founded).max(0.0);
    let peak = f64::from(peak);
    if t <= 0.0 {
        // A zero-tenure record still carries one epoch of person-years; the
        // level is the mean population over that credited epoch, and the
        // stored `end` is widened to `founded + EPOCH_YEARS` so `integral`
        // (which reads `end - founded` uniformly, no special case) recovers
        // exactly `person_years` for this shape too.
        return Shape::Rectangle {
            founded,
            end: founded + EPOCH_YEARS,
            level: person_years / EPOCH_YEARS,
        };
    }
    // Area of rise-then-plateau with rise r: p0 r + (peak - p0) r / 2 + peak (t - r)
    //   = peak t - r (peak - p0) / 2   =>   r = 2 (peak t - A) / (peak - p0)
    if peak > p0 {
        let r = 2.0 * (peak * t - person_years) / (peak - p0);
        if (0.0..=t).contains(&r) {
            return Shape::RisePlateau {
                founded,
                end,
                p0,
                peak,
                rise: r,
            };
        }
    }
    // Triangle from p0 to an apex at end with the committed area: A = t (p0 + apex) / 2.
    let apex = 2.0 * person_years / t - p0;
    if apex >= p0 {
        return Shape::Triangle {
            founded,
            end,
            p0,
            apex,
        };
    }
    Shape::Rectangle {
        founded,
        end,
        level: person_years / t,
    }
}

/// Population at bake year `year` (0 outside the span).
/// type-audit: bare-ok(count: year), bare-ok(count: return)
pub fn population_at(s: &Shape, year: f64) -> f64 {
    match *s {
        Shape::RisePlateau {
            founded,
            end,
            p0,
            peak,
            rise,
        } => {
            if year < founded || year > end {
                0.0
            } else if rise > 0.0 && year - founded < rise {
                p0 + (peak - p0) * (year - founded) / rise
            } else {
                peak
            }
        }
        Shape::Triangle {
            founded,
            end,
            p0,
            apex,
        } => {
            if year < founded || year > end {
                0.0
            } else {
                p0 + (apex - p0) * (year - founded) / (end - founded)
            }
        }
        Shape::Rectangle {
            founded,
            end,
            level,
        } => {
            if year < founded || year > end {
                0.0
            } else {
                level
            }
        }
    }
}

/// The exact integral of the shape over its span.
/// type-audit: bare-ok(count: return)
pub fn integral(s: &Shape) -> f64 {
    match *s {
        Shape::RisePlateau {
            founded,
            end,
            p0,
            peak,
            rise,
        } => p0 * rise + (peak - p0) * rise / 2.0 + peak * (end - founded - rise),
        Shape::Triangle {
            founded,
            end,
            p0,
            apex,
        } => (end - founded) * (p0 + apex) / 2.0,
        Shape::Rectangle {
            founded,
            end,
            level,
        } => level * (end - founded),
    }
}
