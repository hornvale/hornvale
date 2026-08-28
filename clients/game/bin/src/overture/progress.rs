//! The progress substrate — spec §2, decision 0359.
//!
//! **There is no global percentage here, and its absence is a measurement, not
//! a taste.** The campaign's own cost model (spec §1, five agreeing runs) puts
//! the settlements phase at **60.2%** of a 3,054 ms build. A global bar would
//! therefore spend most of its life inside one item, crawling, and would read
//! as a hang precisely when the build is healthiest. So the substrate names the
//! phases, marks the ones that are done, and bars only the phase you are
//! actually in.
//!
//! **And the bar it draws is measured, never modelled** (decision 0360): its
//! length comes from the PREVIOUS run's duration for that same phase, read off
//! disk by [`super::timings`]. Determinism makes that exact for a repeat of the
//! same seed and pins, and roughly right for a new seed on the same hardware. A
//! first-ever run has no such file and therefore draws **no bar at all** — see
//! [`progress_line`]. That is the honest state, not a fallback.

use hornvale_worldgen::BuildDepth;

/// A named phase of startup, in the order it happens.
///
/// Five, not four: the [`BuildDepth`] ladder has four rungs, and the *fifth*
/// phase is the `WorldContext` build that follows genesis (27.2% of the wait,
/// nine tenths of it the demography report). It commits no facts and is not a
/// rung, but the player is still waiting through it, so the substrate names it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Phase {
    /// Sky genesis. 0.4 ms — complete before anything else exists.
    Sky,
    /// Terrain genesis. ~202 ms.
    Land,
    /// Settlement placement, naming and glosses. ~1,840 ms — 60.2% of the wait.
    Peoples,
    /// Culture, religion, species and deep time. ~181 ms.
    DeepTime,
    /// The post-genesis `WorldContext` build, demography report included.
    /// ~830 ms. Not a [`BuildDepth`] rung; the player waits through it anyway.
    Living,
}

impl Phase {
    /// Every phase, in the order startup performs them.
    pub const ALL: [Phase; 5] = [
        Phase::Sky,
        Phase::Land,
        Phase::Peoples,
        Phase::DeepTime,
        Phase::Living,
    ];

    /// The phase whose completion a rung of the build ladder reports.
    ///
    /// [`BuildDepth::Full`] maps to [`Phase::DeepTime`] rather than to
    /// [`Phase::Living`]: the `Full` rung is where deep time lands, and the
    /// living world is built *after* the whole of genesis returns.
    pub fn for_rung(rung: BuildDepth) -> Phase {
        match rung {
            BuildDepth::Astronomy => Phase::Sky,
            BuildDepth::Terrain => Phase::Land,
            BuildDepth::Settlements => Phase::Peoples,
            BuildDepth::Full => Phase::DeepTime,
        }
    }

    /// The phase after this one, or `None` for the last.
    pub fn next(self) -> Option<Phase> {
        Phase::ALL
            .iter()
            .position(|p| *p == self)
            .and_then(|i| Phase::ALL.get(i + 1))
            .copied()
    }

    /// The name shown to the player. Lowercase and article-led, matching the
    /// frame's register: it is a caption, not a log level.
    pub fn label(self) -> &'static str {
        match self {
            Phase::Sky => "the sky",
            Phase::Land => "the land",
            Phase::Peoples => "the peoples",
            Phase::DeepTime => "deep time",
            Phase::Living => "living",
        }
    }

    /// The stable identifier this phase is stored under in the on-disk timings
    /// file. Distinct from [`label`](Phase::label) on purpose: the label is
    /// prose a later campaign may reword, and rewording it must not silently
    /// orphan every recorded duration.
    pub fn key(self) -> &'static str {
        match self {
            Phase::Sky => "sky",
            Phase::Land => "land",
            Phase::Peoples => "peoples",
            Phase::DeepTime => "deep-time",
            Phase::Living => "living",
        }
    }

    /// The phase a stored [`key`](Phase::key) names, or `None` for an
    /// unrecognised one (a file written by a future version, say — ignored
    /// rather than fatal).
    pub fn from_key(key: &str) -> Option<Phase> {
        Phase::ALL.into_iter().find(|p| p.key() == key)
    }
}

/// How wide a per-phase bar is drawn. Ten cells: wide enough that a tenth of a
/// phase is visible, narrow enough that all five phases and a fact count fit on
/// one 80-column row.
pub const BAR_WIDTH: usize = 10;

/// The mark on a phase that is finished.
const DONE_MARK: &str = "[x]";
/// The mark on the phase in progress when no previous timings exist to size a
/// bar from. Deliberately not a one-cell bar: it must not be readable as
/// progress, because it is not.
const UNPACED_MARK: &str = "[>]";
/// The mark on a phase that has not started.
const PENDING_MARK: &str = ".";

/// Where the build is right now, as the progress substrate needs to know it.
///
/// Carries no clock and no wall time: the frame measures, this states.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BuildState {
    /// The phase currently in progress.
    phase: Phase,
    /// How far through `phase` we are, in `0.0..=1.0` — or `None` when there is
    /// no measured duration for it to be a fraction OF, which is the
    /// first-ever-run state (spec §2) and draws no bar.
    fraction: Option<f64>,
    /// Facts committed so far. Genesis appends facts, so this needs no
    /// estimate — it is the one honest number on the screen from the start.
    facts: usize,
}

impl BuildState {
    /// The state during the phase `rung` names, `fraction` of the way through
    /// it. `fraction` is clamped into `0.0..=1.0`, and a non-finite value is
    /// treated as unknown (no bar) rather than drawn as zero.
    pub fn at(rung: BuildDepth, fraction: f64) -> BuildState {
        BuildState::in_phase(Phase::for_rung(rung), fraction)
    }

    /// [`at`](BuildState::at) for a [`Phase`] directly — the constructor
    /// [`Phase::Living`] needs, having no rung of its own.
    pub fn in_phase(phase: Phase, fraction: f64) -> BuildState {
        BuildState {
            phase,
            fraction: if fraction.is_finite() {
                Some(fraction.clamp(0.0, 1.0))
            } else {
                None
            },
            facts: 0,
        }
    }

    /// The state during `phase` with **no** measured duration behind it: names
    /// and a fact count, no bar. A first-ever run's state, and the honest one.
    pub fn unpaced(phase: Phase) -> BuildState {
        BuildState {
            phase,
            fraction: None,
            facts: 0,
        }
    }

    /// This state with `facts` committed facts.
    pub fn with_facts(self, facts: usize) -> BuildState {
        BuildState { facts, ..self }
    }

    /// The phase in progress.
    pub fn phase(self) -> Phase {
        self.phase
    }

    /// How far through [`phase`](BuildState::phase), if that is knowable.
    pub fn fraction(self) -> Option<f64> {
        self.fraction
    }

    /// Facts committed so far.
    pub fn facts(self) -> usize {
        self.facts
    }
}

/// `28110` -> `"28,110"`. Grouped because the number is large and its job is to
/// be read at a glance as motion, not parsed.
fn grouped(n: usize) -> String {
    let digits = n.to_string();
    let mut out = String::with_capacity(digits.len() + digits.len() / 3);
    for (i, ch) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i).is_multiple_of(3) {
            out.push(',');
        }
        out.push(ch);
    }
    out
}

/// The bar for a phase that is `fraction` complete: `#####.....`.
fn bar(fraction: f64) -> String {
    let filled = (fraction.clamp(0.0, 1.0) * BAR_WIDTH as f64).round() as usize;
    let filled = filled.min(BAR_WIDTH);
    let mut out = String::with_capacity(BAR_WIDTH);
    out.extend(std::iter::repeat_n('#', filled));
    out.extend(std::iter::repeat_n('.', BAR_WIDTH - filled));
    out
}

/// The whole progress substrate as one line:
///
/// ```text
/// the sky [x]  the land [x]  the peoples #####.....  deep time .  living .  ·  28,110 facts
/// ```
///
/// **It contains no `%` and no total, ever** (decision 0359). Phases before the
/// current one are marked done, the current one carries a bar sized from the
/// previous run's measured duration, and phases after it are marked pending.
/// With no measured duration the current phase carries `[>]` — a marker, not a
/// zero-length bar, so nothing on the row can be mistaken for progress that was
/// never measured.
pub fn progress_line(state: &BuildState) -> String {
    let mut parts: Vec<String> = Vec::with_capacity(Phase::ALL.len() + 1);
    for phase in Phase::ALL {
        let mark = if phase < state.phase() {
            DONE_MARK.to_string()
        } else if phase == state.phase() {
            match state.fraction() {
                Some(f) => bar(f),
                None => UNPACED_MARK.to_string(),
            }
        } else {
            PENDING_MARK.to_string()
        };
        parts.push(format!("{} {}", phase.label(), mark));
    }
    parts.push(format!("{} facts", grouped(state.facts())));
    parts.join("  ·  ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_progress_substrate_names_phases_and_never_a_global_percentage() {
        // Spec §1: one phase is 60% of the whole, so a global bar would spend
        // most of its life inside one item. This is a design constraint the
        // measurements forced, not a style preference — pin it so nobody
        // "improves" it later.
        let line = progress_line(&BuildState::at(BuildDepth::Settlements, 0.4));
        assert!(line.contains("the peoples"), "the phase must be named");
        assert!(
            !line.contains('%'),
            "a global percentage is forbidden: {line}"
        );
    }

    #[test]
    fn every_phase_is_named_on_the_line_not_only_the_current_one() {
        // Non-vacuity guard for the test above: `contains("the peoples")` would
        // also pass if the line named ONLY the current phase, which is not the
        // substrate the spec draws. All five must be there at once, and the
        // marks must differ by position — done, in progress, pending.
        let line = progress_line(&BuildState::at(BuildDepth::Settlements, 0.4));
        for phase in Phase::ALL {
            assert!(
                line.contains(phase.label()),
                "{} missing from {line}",
                phase.label()
            );
        }
        assert!(
            line.contains("the sky [x]") && line.contains("the land [x]"),
            "finished phases must be marked done: {line}"
        );
        assert!(
            line.contains("the peoples ####"),
            "the phase in progress must carry the bar: {line}"
        );
        assert!(
            line.contains("deep time .") && line.contains("living ."),
            "unstarted phases must be marked pending: {line}"
        );
    }

    #[test]
    fn a_first_ever_run_shows_names_and_a_fact_count_and_no_bar() {
        // Spec §2, and a REQUIREMENT rather than a fallback: with no timings on
        // disk there is nothing honest to size a bar from, and a fresh checkout
        // is exactly this case. Assert all three halves — the names are there,
        // the count is there, and there is no bar anywhere on the row.
        let line = progress_line(&BuildState::unpaced(Phase::Peoples).with_facts(28_110));
        assert!(line.contains("the peoples"), "the phase must be named");
        assert!(
            line.contains("28,110 facts"),
            "the fact count must be shown: {line}"
        );
        assert!(
            !line.contains('#'),
            "a first-ever run must draw no bar: {line}"
        );
        assert!(!line.contains('%'), "and never a percentage: {line}");
        assert!(
            line.contains("the peoples [>]"),
            "the phase in progress must still be distinguishable from a \
             pending one: {line}"
        );
    }

    #[test]
    fn the_bar_tracks_the_fraction_and_saturates_at_both_ends() {
        // Non-vacuity: three DIFFERENT fractions, so the assertion cannot pass
        // against an implementation that returns a constant bar.
        assert_eq!(bar(0.0), "..........");
        assert_eq!(bar(0.4), "####......");
        assert_eq!(bar(1.0), "##########");
        // Out of range in both directions, and a fraction is never a total:
        // clamping is the honest answer, not a longer bar.
        assert_eq!(bar(-1.0), "..........");
        assert_eq!(bar(9.0), "##########");
    }

    #[test]
    fn a_non_finite_fraction_is_unknown_rather_than_zero() {
        // A phase with a zero-millisecond baseline divides by zero, and the
        // sky phase's own measured cost is 0.4 ms, so this is reachable rather
        // than theoretical. `NaN`/inf must read as "not measured" — no bar —
        // never as "0% done", which is a claim.
        for bad in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            let state = BuildState::at(BuildDepth::Terrain, bad);
            assert_eq!(state.fraction(), None, "{bad} should read as unknown");
            let line = progress_line(&state);
            assert!(!line.contains('#'), "{bad} drew a bar: {line}");
            assert!(line.contains("the land [>]"), "{bad}: {line}");
        }
        // And a FINITE zero is genuinely zero — an empty bar, not "unknown".
        // Without this half the test above cannot tell the two apart.
        let zero = BuildState::at(BuildDepth::Terrain, 0.0);
        assert_eq!(zero.fraction(), Some(0.0));
        assert!(progress_line(&zero).contains("the land .........."));
    }

    #[test]
    fn phase_keys_round_trip_and_survive_the_tsv_format() {
        // The keys are what the on-disk timings file is keyed by, so two
        // properties matter: they round-trip, and none of them can break the
        // tab-separated format that stores them.
        let mut seen: Vec<&str> = Vec::new();
        for phase in Phase::ALL {
            assert_eq!(Phase::from_key(phase.key()), Some(phase));
            let key = phase.key();
            assert!(
                !key.contains('\t') && !key.contains('\n') && !key.trim().is_empty(),
                "{key:?} cannot key a tab-separated line"
            );
            assert!(!seen.contains(&key), "two phases share the key {key:?}");
            seen.push(key);
        }
        assert_eq!(Phase::from_key("a-phase-from-the-future"), None);
        // Three of the five keys DIFFER from their display label, which is the
        // point of having keys at all: rewording a caption must not orphan a
        // recorded duration. (`living` coincides today; that is a coincidence,
        // not the contract, so it is not asserted either way.)
        let differing = Phase::ALL
            .into_iter()
            .filter(|p| p.key() != p.label())
            .count();
        assert!(
            differing >= 3,
            "the key set has collapsed onto the labels; rewording a caption \
             would now orphan recorded durations"
        );
    }

    #[test]
    fn the_rung_to_phase_map_is_injective_over_the_whole_ladder() {
        // Non-vacuity: a map that answered `Phase::Sky` for everything would
        // pass any single-rung check.
        let mapped: Vec<Phase> = [
            BuildDepth::Astronomy,
            BuildDepth::Terrain,
            BuildDepth::Settlements,
            BuildDepth::Full,
        ]
        .into_iter()
        .map(Phase::for_rung)
        .collect();
        assert_eq!(
            mapped,
            vec![Phase::Sky, Phase::Land, Phase::Peoples, Phase::DeepTime]
        );
        // And `Living` is reachable from no rung at all, which is the whole
        // reason it needs `BuildState::in_phase`.
        assert!(!mapped.contains(&Phase::Living));
    }

    #[test]
    fn grouping_a_fact_count_puts_separators_only_between_triples() {
        assert_eq!(grouped(0), "0");
        assert_eq!(grouped(999), "999");
        assert_eq!(grouped(1_000), "1,000");
        assert_eq!(grouped(28_110), "28,110");
        assert_eq!(grouped(1_234_567), "1,234,567");
    }
}
