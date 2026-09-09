//! The shared within-tenure population trajectory (spec §4.1): between
//! occupations the committed integral is exact; within one, a
//! rise-then-plateau with the committed area, degrading to a triangle or a
//! rectangle when no rise time fits. History owns this reconstruction because
//! it is the lower-layer interpretation of committed occupation facts; the
//! Lot and worldgen both consume these same functions.

/// The bake's epoch length, in years — the one definition, read wherever the
/// brief's shape code had a bare `25.0`. Verified against
/// `hornvale_worldgen::BakeConfig::default_millennia().epoch_years` by a
/// test, so the two cannot silently drift.
/// plumb: per-world(the bake's epoch length, BakeConfig::epoch_years; 25 is default_millennia's value)
/// type-audit: bare-ok(count)
pub const EPOCH_YEARS: f64 = 25.0;

/// The reconstructed curve over `[founded, end]`.
/// type-audit: bare-ok(count: RisePlateau.founded), bare-ok(count: RisePlateau.end), bare-ok(count: RisePlateau.p0), bare-ok(count: RisePlateau.peak), bare-ok(count: RisePlateau.rise), bare-ok(count: Triangle.founded), bare-ok(count: Triangle.end), bare-ok(count: Triangle.p0), bare-ok(count: Triangle.apex), bare-ok(count: Rectangle.founded), bare-ok(count: Rectangle.end), bare-ok(count: Rectangle.level), bare-ok(flag: Rectangle.clamped)
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
    /// `apex <= peak + 0.5` by construction (see [`shape_of`]): `shape_of`
    /// never returns a `Triangle` whose apex would overshoot the committed
    /// peak, falling through to `Rectangle` instead when it would.
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
    /// `level <= peak + 0.5` by construction (see [`shape_of`]'s `rectangle`
    /// helper): a raw `person_years / t` that would overshoot the committed
    /// peak is clamped, and `clamped` says so — the caption downstream
    /// (`json.rs`'s `shape_name`) reports `"rectangle-clamped"` rather than
    /// silently narrating an exact integral it no longer has.
    Rectangle {
        /// The bake year the occupation began.
        founded: f64,
        /// The bake year the occupation ended.
        end: f64,
        /// The constant population over the span — `<= peak + 0.5`.
        level: f64,
        /// Whether `level` is the clamp, not the raw `person_years / t`.
        clamped: bool,
    },
}

/// Choose the trajectory for one occupation. `person_years` is the committed
/// integral, `p0` the bake's opening population for its founding kind.
///
/// **`p0` is sometimes wrong, and this function must not trust it past the
/// point the committed data can bear.** The bake's `open` (`history_bake.rs`)
/// sets `peak_population` to the OPENING population and only ever raises it,
/// so the true opening population is always `<= peak` — but a
/// `Founding::From` record's committed facts cannot tell WHICH of five
/// distinct openings produced it: a true daughter colony opens at
/// `DAUGHTER_POP`, while a relocation to vacant land, a conquest, a climate-
/// driven migration, and a raid seat all open at the survivors' own
/// (generally much larger) population — `pop`, `pop * (1 - WAR_LOSS)`,
/// `pop * MIGRATE_SURVIVAL`, `raider_pop` respectively. Callers currently
/// pass `DAUGHTER_POP` for every `Founding::From`, which under-states `p0`
/// for the other four mechanisms. An under-stated `p0` can force the Triangle branch below
/// to fit its area with an apex ABOVE the committed peak — a real, measured
/// defect: 3 of seed 42's 1212 occupations produced a Triangle apex up to
/// 23.6 over peak before this guard existed. So the Triangle branch is
/// admitted only when its apex stays within `peak + 0.5` (the same slack the
/// bake's own accrual invariant allows RisePlateau/Rectangle); an
/// out-of-range fit reads as "this occupation opened larger than
/// `DAUGHTER_POP`" and falls through to `Rectangle`, whose `level =
/// person_years / t` is bounded by that same accrual invariant regardless of
/// what `p0` was — never in `p0` itself, which the caller has no way to
/// correct from the committed facts alone.
/// type-audit: bare-ok(count: founded), bare-ok(count: end), bare-ok(count: peak), bare-ok(count: person_years), bare-ok(count: p0)
pub fn shape_of(founded: f64, end: f64, peak: u32, person_years: f64, p0: f64) -> Shape {
    let t = (end - founded).max(0.0);
    let peak = f64::from(peak);
    if t <= 0.0 {
        // A zero-tenure record still carries one epoch of person-years; the
        // level is the mean population over that credited epoch, and the
        // stored `end` is widened to `founded + EPOCH_YEARS` so `integral`
        // (which reads `end - founded` uniformly, no special case) recovers
        // exactly `person_years` for this shape too — unless the clamp below
        // bites, in which case the recovered integral is honestly less.
        return rectangle(founded, founded + EPOCH_YEARS, person_years, peak);
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
    // Admitted only when apex stays within peak + 0.5 (see shape_of's doc for
    // why an out-of-range fit here means p0 itself was wrong, not that the
    // model needs a bigger ceiling): an under-stated p0 (any Founding::From
    // opening other than a true daughter colony) can otherwise force apex
    // above the committed peak.
    let apex = 2.0 * person_years / t - p0;
    if apex >= p0 && apex <= peak + 0.5 {
        return Shape::Triangle {
            founded,
            end,
            p0,
            apex,
        };
    }
    rectangle(founded, end, person_years, peak)
}

/// Build a `Rectangle`, clamping its level to `peak + 0.5` (the same slack
/// `Triangle`'s apex check above allows) and reporting whether the clamp
/// fired.
///
/// **Why the accrual invariant alone does not bound this level.** A
/// record's committed `person_years` can carry more credited epochs than
/// the reconstructed span `end - founded` has room for — each epoch
/// contributes up to `peak * EPOCH_YEARS` regardless of how much of that
/// epoch the occupation actually held the site, so `credited epochs *
/// EPOCH_YEARS` can exceed the record's own tenure (94 of seed 42's 1,212
/// occupations do, the worst by 0.57% of the bound). Dividing the whole
/// credited `person_years` by the shorter real `t` then overshoots `peak`.
/// Clamping here is what makes [`population_at`] honour the committed peak
/// BY CONSTRUCTION rather than by measured luck; [`integral`] of a clamped
/// rectangle is `level * t`, honestly less than `person_years` rather than
/// silently wrong.
fn rectangle(founded: f64, end: f64, person_years: f64, peak: f64) -> Shape {
    let t = end - founded;
    let raw = person_years / t;
    let bound = peak + 0.5;
    let (level, clamped) = if raw > bound {
        (bound, true)
    } else {
        (raw, false)
    };
    Shape::Rectangle {
        founded,
        end,
        level,
        clamped,
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
            ..
        } => {
            if year < founded || year > end {
                0.0
            } else {
                level
            }
        }
    }
}

/// The exact integral of the shape over its span — exact for
/// `RisePlateau`/`Triangle`, and for an unclamped `Rectangle`; a clamped
/// `Rectangle`'s integral is `level * t`, which is `< person_years` by
/// however much the clamp bit (see [`rectangle`]).
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
            ..
        } => level * (end - founded),
    }
}
