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
/// phase is visible, narrow enough that the whole phase roster fits the
/// 80-column floor with room to spare — see [`progress_line`] for the width
/// budget, which is measured by
/// `the_substrate_fits_the_eighty_column_floor_in_every_state` rather than
/// asserted here.
///
/// **This doc previously claimed the roster AND a fact count fit one 80-column
/// row. That was false** — the single-row form measured 85 to 101 characters, so
/// at the floor the fact count was clipped off-screen entirely in every state,
/// including the first-ever run that spec §2 makes it mandatory for. The
/// substrate is two rows now, which is what spec §2 draws.
pub const BAR_WIDTH: usize = 10;

/// The mark on a phase that is finished.
const DONE_MARK: &str = "[x]";
/// The mark on the phase in progress when no previous timings exist to size a
/// bar from. Deliberately not a one-cell bar: it must not be readable as
/// progress, because it is not.
const UNPACED_MARK: &str = "[>]";
/// The mark on a phase that has not started.
///
/// One cell, and it must never be confusable with a bar: a phase that has not
/// started has no progress to report, and drawing it as an empty bar would put
/// a "0% done" claim on work nobody has begun.
/// `a_pending_phase_is_never_drawn_as_a_bar` pins that, by comparing the whole
/// roster row against its exact expected text — a `contains("deep time .")`
/// check is satisfied by `deep time ..........` and cannot see the defect.
const PENDING_MARK: &str = ".";

/// What separates two phase segments on the roster row. Two spaces, matching
/// spec §2's own diagram: the earlier `"  ·  "` cost 15 characters across the
/// row for no legibility a double space does not already give.
const PHASE_SEP: &str = "  ";

/// The last cell of a bar whose phase has OUTRUN its baseline.
///
/// The bar's fill is capped one cell short of full while a phase is in
/// progress, and this replaces that last cell once the previous run's duration
/// has been exceeded. Spec §3 rule 3: *fill the time it is given without
/// implying a total it cannot know* — a bar drawn full implies the phase is
/// done, and a bar that sits full indefinitely is the exact false claim that
/// rule forbids. A new seed on the same hardware, or the same seed on a busier
/// machine, reaches this routinely.
const OVERRUN_MARK: char = '>';

/// Where the build is right now, as the progress substrate needs to know it.
///
/// Carries no clock and no wall time: the frame measures, this states.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BuildState {
    /// The phase currently in progress.
    phase: Phase,
    /// How far through `phase` we are, as a multiple of the baseline duration —
    /// `None` when there is no measured duration for it to be a fraction OF,
    /// which is the first-ever-run state (spec §2) and draws no bar.
    ///
    /// **Not capped at 1.0.** A value above one means the phase has outrun its
    /// baseline, which is a real and frequent state (a new seed, a busier
    /// machine), and the bar draws it distinctly rather than sitting full and
    /// implying completion — spec §3 rule 3.
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
            // Clamped BELOW at zero (a negative elapsed time is nonsense) but
            // deliberately not above: see the field's own doc.
            fraction: if fraction.is_finite() {
                Some(fraction.max(0.0))
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

    /// How far through [`phase`](BuildState::phase), if that is knowable. May
    /// exceed 1.0 — see the field's doc.
    pub fn fraction(self) -> Option<f64> {
        self.fraction
    }

    /// Whether the current phase has outrun the baseline that sized its bar.
    pub fn is_overrunning(self) -> bool {
        self.fraction.is_some_and(|f| f >= 1.0)
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
    // The fill NEVER reaches the last cell. A bar drawn full says "this phase
    // is done", and the phase this bar describes is by definition the one still
    // running — so the last cell is reserved: a dot while inside the baseline,
    // `OVERRUN_MARK` once past it (spec §3 rule 3).
    let cap = BAR_WIDTH - 1;
    let filled = (fraction.max(0.0) * BAR_WIDTH as f64).round() as usize;
    let filled = filled.min(cap);
    let mut out = String::with_capacity(BAR_WIDTH);
    out.extend(std::iter::repeat_n('#', filled));
    out.extend(std::iter::repeat_n('.', cap - filled));
    out.push(if fraction >= 1.0 { OVERRUN_MARK } else { '.' });
    out
}

/// The phase roster: one segment per phase, marked done / in progress /
/// pending.
///
/// ```text
/// the sky [x]  the land [x]  the peoples ####.....  deep time .  living .
/// ```
///
/// **It contains no `%` and no total, ever** (decision 0359). Phases before the
/// current one are marked done, the current one carries a bar sized from the
/// previous run's measured duration, and phases after it are marked pending.
/// With no measured duration the current phase carries `[>]` — a marker, not a
/// zero-length bar, so nothing on the row can be mistaken for progress that was
/// never measured; and a pending phase carries a bare `.`, for the same reason
/// in the other direction.
///
/// Widest possible form is 76 characters (all five labels, two `[x]`, a bar,
/// two pending dots, four separators), which fits the 80-column floor
/// `hornvale-game-core` refuses to render below.
pub fn phase_roster(state: &BuildState) -> String {
    let mut parts: Vec<String> = Vec::with_capacity(Phase::ALL.len());
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
    parts.join(PHASE_SEP)
}

/// The committed-fact count, the one honest number on the screen from the first
/// frame: `"28,110 facts"`. Genesis appends facts, so this needs no estimate and
/// implies no total.
pub fn fact_count(state: &BuildState) -> String {
    format!("{} facts", grouped(state.facts()))
}

/// The whole progress substrate, as the TWO rows the frame draws it on,
/// newline-separated — spec §2's own layout:
///
/// ```text
/// the sky [x]  the land [x]  the peoples ####.....  deep time .  living .
///                                                             28,110 facts
/// ```
///
/// **Two rows, not one, and the reason is the 80-column floor.**
/// `hornvale-game-core`'s module doc states this codebase's register outright:
/// *"Monochrome at 80x24 is the floor … if it only works larger, it is wrong,
/// so `render` refuses anything smaller rather than silently degrading."* The
/// single-row form measured 85-101 characters, and [`super::Frame`]'s text
/// writer clips at the grid's right edge, so at the floor the fact count fell
/// off the screen in every state — including the first-ever run, for which spec
/// §2 makes it mandatory. Widening the test terminal to see it was the wrong
/// direction; the substrate narrowed instead.
///
/// The caller may draw the two rows wherever it likes
/// ([`super::Frame::compose`] puts the roster on the second-to-last row and
/// right-aligns the count on the last), but both rows always exist: the count
/// is never optional.
pub fn progress_line(state: &BuildState) -> String {
    format!("{}\n{}", phase_roster(state), fact_count(state))
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
        // NOT `contains("deep time .")` — that is satisfied by
        // `deep time ..........`, which is the very defect
        // `a_pending_phase_is_never_drawn_as_a_bar` exists to catch. The
        // segment list is compared exactly there; here it is enough to say a
        // pending phase carries no fill.
        assert!(
            !line.contains("deep time ..") && !line.contains("living .."),
            "an unstarted phase was drawn as a bar: {line}"
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
    fn the_bar_tracks_the_fraction_and_never_draws_itself_full() {
        // Non-vacuity: four DIFFERENT fractions, so the assertion cannot pass
        // against an implementation that returns a constant bar.
        assert_eq!(bar(0.0), "..........");
        assert_eq!(bar(0.4), "####......");
        assert_eq!(bar(0.9), "#########.");
        // The last cell is RESERVED: the phase this bar describes is by
        // definition the one still running, so a full bar would claim a
        // completion the frame cannot know (spec §3 rule 3). `0.95` rounds to
        // ten tenths and must still leave the cell alone.
        assert_eq!(bar(0.95), "#########.");
        // A negative elapsed time is nonsense and clamps to empty.
        assert_eq!(bar(-1.0), "..........");
    }

    #[test]
    fn a_phase_that_outruns_its_baseline_says_so_instead_of_sitting_full() {
        // M12 / spec §3 rule 3. Overrunning is routine — a new seed, or the same
        // seed on a busier machine — and the old behaviour clamped to a FULL bar
        // that then sat there implying the phase was done. The marker must be
        // distinguishable from the in-baseline bar at the same fill.
        assert_eq!(bar(1.0), "#########>");
        assert_eq!(bar(3.0), "#########>");
        assert_ne!(
            bar(1.0),
            bar(0.9),
            "an overrunning phase must not look like one still inside its \
             baseline"
        );
        // And it reaches the state, not just the bar helper.
        let over = BuildState::at(BuildDepth::Settlements, 4.0);
        assert!(over.is_overrunning());
        assert_eq!(
            over.fraction(),
            Some(4.0),
            "the fraction must not be capped at 1.0 — the frame needs to know \
             the phase is over, not merely at, its baseline"
        );
        assert!(phase_roster(&over).contains("the peoples #########>"));
        assert!(!BuildState::at(BuildDepth::Settlements, 0.9).is_overrunning());
    }

    #[test]
    fn the_substrate_fits_the_eighty_column_floor_in_every_state() {
        // I1. `hornvale-game-core` refuses to render below 80x24 rather than
        // degrading, so every row of the substrate must fit 80 columns in EVERY
        // state — not just the narrow ones. Enumerated rather than sampled:
        // five phases x five mark shapes x three fact magnitudes.
        let mut widest = 0usize;
        for (i, phase) in Phase::ALL.into_iter().enumerate() {
            for fraction in [None, Some(0.0), Some(0.5), Some(0.999), Some(9.0)] {
                for facts in [0usize, 28_110, 999_999_999] {
                    let state = match fraction {
                        Some(f) => BuildState::in_phase(phase, f),
                        None => BuildState::unpaced(phase),
                    }
                    .with_facts(facts);
                    for row in progress_line(&state).lines() {
                        let width = row.chars().count();
                        widest = widest.max(width);
                        assert!(
                            width <= 80,
                            "phase {i} ({}) at fraction {fraction:?} with \
                             {facts} facts drew a {width}-column row, over the \
                             80-column floor: {row}",
                            phase.label()
                        );
                    }
                }
            }
        }
        // Non-vacuity: the loop must actually be producing long rows, or the
        // assertion above is measuring nothing. The widest roster is 76.
        assert!(
            widest >= 70,
            "the widest row measured only {widest} columns — this test is not \
             exercising the full-roster case it claims to"
        );
    }

    #[test]
    fn a_pending_phase_is_never_drawn_as_a_bar() {
        // I4, and the same defect class as the `[>]` one: a `contains("deep
        // time .")` check is satisfied by `deep time ..........`, so a
        // `PENDING_MARK` rendered as `bar(0.0)` would put a "0% done" claim on
        // three phases nobody has started and survive the whole suite. The row
        // is compared EXACTLY, which is the only assertion that cannot be
        // fooled by a prefix.
        let state = BuildState::at(BuildDepth::Settlements, 0.4);
        assert_eq!(
            phase_roster(&state),
            "the sky [x]  the land [x]  the peoples ####......  deep time .  living ."
        );
        // And in the unpaced state, where the current phase's own mark differs.
        let unpaced = BuildState::unpaced(Phase::Peoples);
        assert_eq!(
            phase_roster(&unpaced),
            "the sky [x]  the land [x]  the peoples [>]  deep time .  living ."
        );
    }

    #[test]
    fn the_fact_count_is_its_own_row_and_is_never_omitted() {
        // Spec §2 makes the count mandatory, including on a first-ever run. It
        // moved to row two precisely so the 80-column floor cannot clip it.
        for state in [
            BuildState::unpaced(Phase::Sky),
            BuildState::at(BuildDepth::Settlements, 0.4).with_facts(28_110),
        ] {
            let whole = progress_line(&state);
            let rows: Vec<&str> = whole.lines().collect();
            assert_eq!(rows.len(), 2, "the substrate is two rows: {rows:?}");
            assert_eq!(rows[1], fact_count(&state));
            assert!(rows[1].ends_with(" facts"), "{:?}", rows[1]);
        }
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
        assert!(phase_roster(&zero).contains("the land .........."));
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
