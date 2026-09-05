//! The lot window: one life drawn from everyone who ever lived in a world,
//! told from the committed ledger alone. A lot is an OBSERVATION, never a
//! fact (spec §3 of `docs/superpowers/specs/2026-09-05-the-lot-design.md`;
//! the campaign's decision record for this follows at close, per the
//! ledger's Task 12 entry): its randomness is the reader's `LotIndex`,
//! expanded by pure hash arithmetic, and nothing here draws a `Stream` or
//! commits.
#![warn(missing_docs)]

pub mod context;
pub mod draw;
pub mod hazard;
pub mod shape;

use hornvale_kernel::Vertex;

/// The reader's key: an index into the lives a world could have held.
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LotIndex(pub u64);

/// The exhibit's overrides: a picked birth year and/or a picked site. The
/// index still drives every other choice.
/// type-audit: bare-ok(count: year)
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Pick {
    /// A birth year inside the bake span, or `None` to draw one.
    pub year: Option<f64>,
    /// An occupation site alive in that year, or `None` to draw one.
    pub site: Option<Vertex>,
}

/// Why a lot could not be drawn. Each names the physical reason, the pin
/// discipline of `GenesisError`.
/// type-audit: bare-ok(count: YearOutsideSpan.year), bare-ok(count: YearOutsideSpan.start), bare-ok(count: YearOutsideSpan.end), bare-ok(count: SiteNotAliveInYear.year), bare-ok(prose: Build.0)
#[derive(Clone, Debug, PartialEq)]
pub enum LotError {
    /// No occupation in this world carries `occ-person-years`: a world saved
    /// before The Lot. Regenerate it from its seed.
    NoPersonYears,
    /// The world has no occupation at all.
    NoOccupations,
    /// A picked year outside `[start, end)`.
    YearOutsideSpan {
        /// The pinned year.
        year: f64,
        /// The bake's first year.
        start: f64,
        /// The bake's present.
        end: f64,
    },
    /// A picked site with no occupation alive in the picked or drawn year.
    SiteNotAliveInYear {
        /// The pinned site.
        site: Vertex,
        /// The year asked about.
        year: f64,
    },
    /// The composition root could not rebuild a derived input.
    Build(String),
}

impl std::fmt::Display for LotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LotError::NoPersonYears => write!(
                f,
                "this world carries no occ-person-years facts (saved before The Lot); regenerate it from its seed"
            ),
            LotError::NoOccupations => {
                write!(f, "this world has no occupations to draw a life from")
            }
            LotError::YearOutsideSpan { year, start, end } => {
                write!(f, "year {year} is outside the bake span [{start}, {end})")
            }
            LotError::SiteNotAliveInYear { site, year } => write!(
                f,
                "no occupation is alive at vertex {} in year {year}",
                site.0
            ),
            LotError::Build(e) => write!(f, "derived input could not be rebuilt: {e}"),
        }
    }
}
