//! `PrecisionLadder`: one world's own cycles, read from committed sky facts
//! and ordered into the rungs a retold claim can be coarsened onto.
//!
//! **Why this lives here and not in the kernel:** see `kernel/src/precision.rs`
//! — a day span at a `pub` boundary wants `StdDays`, which lives in
//! `domains/astronomy`, below no part of the kernel. The kernel therefore
//! owns only the rung *index* ([`hornvale_kernel::Precision`]); this window
//! owns the rungs themselves.
//!
//! **The trap this module exists to avoid.** `MOON_PERIOD_STD` is registered
//! non-functional and committed once per moon on the same subject, so
//! `Ledger::value_of` — which returns only the first fact in commit order —
//! silently yields one moon on a two-mooned world. This module reads every
//! `moon-period-std` fact via [`hornvale_kernel::ledger::Ledger::find`], never
//! `value_of`, for exactly that reason.
//!
//! **The rungs deliberately do not nest.** A synodic month does not divide a
//! year; snapping a day to two different rungs in sequence can carry a claim
//! off the event it once contained. That is the modelled phenomenon (spec
//! §5.2), not a bug — do not repair a test failure here by making the spans
//! divide each other.

use hornvale_astronomy::facts::{DAY_LENGTH_STD, MOON_PERIOD_STD, YEAR_LENGTH_STD};
use hornvale_astronomy::units::StdDays;
use hornvale_kernel::Precision;
use hornvale_kernel::ledger::{Ledger, Value};
use std::collections::BTreeMap;

/// One rung of a world's ladder: a label for the readout and the span (in
/// standard days) that a claim coarsened onto it is rounded to.
#[derive(Clone, Debug)]
struct Rung {
    /// Human-readable name for this rung ("day", "moon 1", "year", ...).
    label: String,
    /// The span this rung rounds a day value to.
    span: StdDays,
}

/// A world's own reckonings, ordered finest to coarsest by their actual
/// length — never Earth's calendar, never nested by construction.
///
/// Built once per world from committed sky facts via [`PrecisionLadder::of`];
/// a world with no astronomy facts at all yields an [`PrecisionLadder::is_empty`]
/// ladder, and every lookup on it is a harmless no-op.
#[derive(Clone, Debug)]
pub struct PrecisionLadder {
    rungs: Vec<Rung>,
}

/// Read `value` as a valid span, or `None` when it is not a finite positive
/// number — the validation [`StdDays::new`] already performs, reused rather
/// than duplicated by hand.
fn as_span(value: &Value) -> Option<StdDays> {
    match value {
        Value::Number(n) => StdDays::new(*n).ok(),
        _ => None,
    }
}

impl PrecisionLadder {
    /// This world's astronomical rungs, unsorted: day length, every moon's
    /// period (in commit order — see the module doc for why this is `find`,
    /// not `value_of`), and year length. Shared by [`PrecisionLadder::of`]
    /// and [`PrecisionLadder::with_social`], which each sort and dedup the
    /// result via [`PrecisionLadder::finish`].
    fn rungs_of(ledger: &Ledger) -> Vec<Rung> {
        let mut rungs: Vec<Rung> = Vec::new();

        if let Some(span) = ledger.find(DAY_LENGTH_STD).find_map(|f| as_span(&f.object)) {
            rungs.push(Rung {
                label: "day".to_string(),
                span,
            });
        }

        let mut moon_number = 0usize;
        for fact in ledger.find(MOON_PERIOD_STD) {
            if let Some(span) = as_span(&fact.object) {
                moon_number += 1;
                rungs.push(Rung {
                    label: format!("moon {moon_number}"),
                    span,
                });
            }
        }

        if let Some(span) = ledger
            .find(YEAR_LENGTH_STD)
            .find_map(|f| as_span(&f.object))
        {
            rungs.push(Rung {
                label: "year".to_string(),
                span,
            });
        }

        rungs
    }

    /// Sort `rungs` ascending by actual span (equal spans broken by label),
    /// then collapse rungs of equal span to one. The shared tail of every
    /// constructor.
    fn finish(mut rungs: Vec<Rung>) -> PrecisionLadder {
        rungs.sort_by(|a, b| {
            a.span
                .get()
                .total_cmp(&b.span.get())
                .then_with(|| a.label.cmp(&b.label))
        });
        rungs.dedup_by(|a, b| a.span.get() == b.span.get());

        PrecisionLadder { rungs }
    }

    /// Build this world's ladder from its committed sky facts: its day
    /// length, every moon's period (in commit order — see the module doc for
    /// why this is `find`, not `value_of`), and its year length. Rungs sort
    /// ascending by actual span, non-finite or non-positive values are
    /// dropped, and rungs of equal span collapse to one.
    pub fn of(ledger: &Ledger) -> PrecisionLadder {
        Self::finish(Self::rungs_of(ledger))
    }

    /// This world's ladder as [`PrecisionLadder::of`] builds it, plus the two
    /// SOCIAL rungs a people carries: its generation length and its lifespan.
    ///
    /// Both are optional and both sort by actual span alongside the
    /// astronomical rungs — a generation shorter than this world's year sits
    /// BELOW the year, because a rung's position is its length and never its
    /// origin. A people with no mass-derived life history yields exactly the
    /// astronomical ladder, the same way a moonless world yields no lunar
    /// rung.
    ///
    /// Campaign 2's rungs were read from committed astronomy alone; these are
    /// read from committed allometry. Both are DERIVED from the world, which
    /// is the principle — an authored "century" rung would not be.
    pub fn with_social(
        ledger: &Ledger,
        generation: Option<StdDays>,
        lifespan: Option<StdDays>,
    ) -> PrecisionLadder {
        let mut rungs = Self::rungs_of(ledger);
        if let Some(span) = generation {
            rungs.push(Rung {
                label: "generation".to_string(),
                span,
            });
        }
        if let Some(span) = lifespan {
            rungs.push(Rung {
                label: "lifespan".to_string(),
                span,
            });
        }
        Self::finish(rungs)
    }

    /// How many rungs this world's ladder has.
    /// type-audit: bare-ok(count)
    pub fn len(&self) -> usize {
        self.rungs.len()
    }

    /// True when this world offers no reckonings at all — no day length, no
    /// moons, no year.
    /// type-audit: bare-ok(flag)
    pub fn is_empty(&self) -> bool {
        self.rungs.is_empty()
    }

    /// The name of the rung at this precision, or `None` past this world's
    /// coarsest rung (including every precision on an empty ladder).
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(&self, precision: Precision) -> Option<&str> {
        self.rungs
            .get(precision.rung() as usize)
            .map(|r| r.label.as_str())
    }

    /// The span the rung at this precision rounds a day value to, or `None`
    /// past this world's coarsest rung.
    pub fn span(&self, precision: Precision) -> Option<StdDays> {
        self.rungs.get(precision.rung() as usize).map(|r| r.span)
    }

    /// One rung coarser, saturating at this world's coarsest rung rather than
    /// running off the end of the ladder. On an empty ladder — which has no
    /// coarsest rung to saturate at — returns `precision` unchanged.
    pub fn coarser(&self, precision: Precision) -> Precision {
        if self.rungs.is_empty() {
            return precision;
        }
        let coarsest = Precision((self.rungs.len() - 1) as u8);
        if precision >= coarsest {
            precision
        } else {
            precision.coarser()
        }
    }

    /// Round a day value down to the start of the interval the rung at
    /// `precision` divides the calendar into. Past this world's coarsest rung
    /// (including every precision on an empty ladder), the day passes through
    /// unchanged — there is no rung to lose precision to.
    ///
    /// Deliberately NOT round-to-nearest: two applications at different rungs
    /// use different origins, which is how re-rounding can carry a claim off
    /// the event it once contained (see the module doc).
    /// type-audit: bare-ok(diagnostic-value: day), bare-ok(diagnostic-value: return)
    pub fn apply(&self, precision: Precision, day: f64) -> f64 {
        match self.span(precision) {
            Some(span) => (day / span.get()).floor() * span.get(),
            None => day,
        }
    }

    /// Every rung's label, finest first — the readout's report line.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn labels(&self) -> Vec<&str> {
        self.rungs.iter().map(|r| r.label.as_str()).collect()
    }
}

/// One ladder per people, because generation length varies by people.
///
/// Built once per world. `for_people` falls back to the astronomical-only
/// ladder for a people with no entry, so a lookup never fails and a world
/// with no life history at all behaves exactly as campaign 2's did.
#[derive(Clone, Debug)]
pub struct PeopleLadders {
    /// Each named people's own ladder (astronomical rungs plus its social
    /// ones).
    by_people: BTreeMap<String, PrecisionLadder>,
    /// The astronomical-only ladder, returned for a people with no entry.
    fallback: PrecisionLadder,
}

impl PeopleLadders {
    /// Build a ladder for every people named in `durations`.
    pub fn of(ledger: &Ledger, durations: &crate::durations::PeopleDurations) -> PeopleLadders {
        let mut by_people = BTreeMap::new();
        for people in durations.peoples() {
            let (generation, lifespan) = durations.get(people);
            by_people.insert(
                people.to_string(),
                PrecisionLadder::with_social(ledger, generation, lifespan),
            );
        }
        PeopleLadders {
            by_people,
            fallback: PrecisionLadder::of(ledger),
        }
    }

    /// This people's ladder, or the astronomical-only fallback.
    /// type-audit: bare-ok(identifier-text: people)
    pub fn for_people(&self, people: &str) -> &PrecisionLadder {
        self.by_people.get(people).unwrap_or(&self.fallback)
    }
}
