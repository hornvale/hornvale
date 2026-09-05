//! A derived-feature kind, as data — spec §5.7: "a kind is three things and
//! nothing else — a component bundle, a prevalence recipe, and the three
//! scalars." This file carries the recipe and the scalars for all four
//! kinds spec §5.6 names. The component bundle for the one enterable-
//! adjacent kind (overhang/hollow's shelter-and-fire affordance) is **not**
//! wired here: `ObjectProperty`/`object_registry` live in `windows/vessel`,
//! which depends on `hornvale-worldgen` (not the reverse — the same
//! layering [`super`]'s module doc states for `LocaleContext`), so this
//! crate cannot reference that vocabulary at all. [`WeftKind::Overhang`]'s
//! own doc states the affordance claim in prose; the wiring shipped in this
//! campaign (Task 8) as `windows/vessel::affordance::weft_object_registry`,
//! a **separate** `ComponentStore<WeftKind, ObjectTraits>` — not a row in
//! `object_registry` itself. `object_registry`'s keys are gated closed
//! against `hornvale_thing::THING_KINDS`, which registers into the world's
//! `ConceptRegistry` and so serializes into `world.json`; admitting
//! `"overhang"` there would move `cli/tests/fixtures/world-seed-42.json`,
//! which this campaign's own spec §6 forbids. Spec §5.6 has the full
//! account of why the single-registry route was never available to take.
//!
//! **Eligibility is a per-kind gate, tested before any noise is drawn (Task
//! 7, controller ruling R1).** Task 5's review measured that 59% of all
//! spring occurrences (seed 42, every walk-depth facet over all 40,962
//! vertices) landed on facets with no macro cause at all, including open
//! ocean — the mixing lerp's `(1 - contextuality) * noise` floor is real and
//! unconditional, so nothing stopped a spring from surfacing mid-sea.
//! [`eligible`](WeftKind::eligible) closes it, mirroring
//! `GeneratedTerrain::cave_at`'s own `if self.is_ocean(id) { return None; }`
//! one level down (`domains/terrain/src/provider.rs`). See
//! `crate::weft::mod`'s [`super::prevalence`] for where the gate is applied,
//! and this file's `land_eligible` for the shared ground test every kind
//! currently uses.

use hornvale_kernel::math;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Vertex, blend_corner_weights};

use crate::FieldPack;
use crate::streams;

/// A derived feature kind (spec §5.6's spanning set). `Copy` — a kind is a
/// bare discriminant, never carries data of its own; every scalar it names
/// lives in this module's per-kind constants, looked up through `self`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WeftKind {
    /// Spring / seep — spec §5.6's "sign case": high contextuality, long
    /// correlation length, an enterable water source diagnostic of what is
    /// underfoot (karst carbonate crossed with channelized drainage).
    Spring,
    /// Overhang / hollow — spec §5.6: medium contextuality (induration ×
    /// slope), short–medium correlation length. **Not enterable**, but
    /// affords shelter and fire: "a place to get out of the rain and start
    /// a fire" is a component bundle in the existing `ObjectProperty`
    /// vocabulary (`SupportsRest`-adjacent shelter plus a warmth variant),
    /// wired at `windows/vessel::affordance::weft_object_registry` — a
    /// separate table from `object_registry`, not a row in it; see this
    /// module's own doc for why that route was never available. Proves the
    /// affordance path end to end (spec §5.6's "what it proves" column).
    Overhang,
    /// Thicket / brake — spec §5.6: high contextuality (productivity —
    /// temperature × moisture, Liebig-combined), long correlation length.
    /// Not enterable; texture, aimed directly at the biome-monotony defect
    /// this campaign addresses (`Biome` is categorical and `blend_at`
    /// structurally cannot smooth it; this kind reads the two continuous
    /// causes underneath instead).
    Thicket,
    /// Erratic / scatter — spec §5.6's **negative control**: deliberately
    /// LOW contextuality (near-zero — mostly free noise, position-
    /// continuous but uncorrelated with any macro cause), short correlation
    /// length. Not enterable. Exists so Task 9's legibility metric can be
    /// shown to *discriminate*: if springs and erratics score alike, the
    /// metric measures nothing. See [`Self::contextuality`]'s doc for why
    /// this kind must never be "improved" by tying it to a real macro
    /// cause.
    Erratic,
}

impl WeftKind {
    /// Every kind that exists, in a fixed order. [`super::window::WeftWindow`]
    /// enumerates this once per facet entering its radius (Task 6) — the one
    /// place a new kind must be added for the residency window to pick it up.
    /// Task 7 grows this by appending, never editing an existing entry (spec
    /// §5.7: "kind N+1 is an append").
    pub const ALL: [WeftKind; 4] = [
        WeftKind::Spring,
        WeftKind::Overhang,
        WeftKind::Thicket,
        WeftKind::Erratic,
    ];

    /// This kind's seed-derivation root leg (a save-format contract; see
    /// `windows/worldgen/src/streams.rs`). [`super::prevalence`] and
    /// [`super::occurs`] each derive their own decorrelated sub-leg under it.
    pub(crate) fn stream_label(self) -> StreamLabel<'static> {
        match self {
            WeftKind::Spring => streams::WEFT_SPRING,
            WeftKind::Overhang => streams::WEFT_OVERHANG,
            WeftKind::Thicket => streams::WEFT_THICKET,
            WeftKind::Erratic => streams::WEFT_ERRATIC,
        }
    }

    /// The abundance ceiling — `prevalence`'s maximum possible answer for
    /// this kind, reached only where macro state and noise both saturate.
    ///
    /// **A control-kinds-only dial since The Warp (spec §6.1).** The sign
    /// kinds no longer mix an abundance against a contextuality lerp: their
    /// prevalence is [`Self::rate`] × [`Self::response`] + [`Self::floor`] ×
    /// noise, which has no abundance ceiling in it. Rather than leave
    /// `SPRING_ABUNDANCE`/`OVERHANG_ABUNDANCE` sitting in the file as dead
    /// dials a reader would take for live ones, they are deleted and this
    /// arm is [`unreachable!`] — a caller reaching it has routed a sign kind
    /// down the control path, which is a bug in the branch, not a value
    /// question.
    pub(crate) fn abundance(self) -> f64 {
        match self {
            WeftKind::Thicket => THICKET_ABUNDANCE,
            WeftKind::Erratic => ERRATIC_ABUNDANCE,
            WeftKind::Spring | WeftKind::Overhang => {
                unreachable!("a sign kind has a rate and a floor, not an abundance")
            }
        }
    }

    /// How many facets, walked in a straight line, before this kind's noise
    /// contribution meaningfully changes (spec §5.2's correlation length, in
    /// facets — converted to an [`hornvale_terrain::SphereFbm`] frequency at
    /// the call site, since the facet-to-radians conversion depends on the
    /// *facet being sampled*, not the kind alone).
    pub(crate) fn correlation_length_facets(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_CORRELATION_LENGTH_FACETS,
            WeftKind::Overhang => OVERHANG_CORRELATION_LENGTH_FACETS,
            WeftKind::Thicket => THICKET_CORRELATION_LENGTH_FACETS,
            WeftKind::Erratic => ERRATIC_CORRELATION_LENGTH_FACETS,
        }
    }

    /// The macro-state/free-noise mixing weight: `1.0` reads as entirely
    /// macro-state-driven ("wallpaper" in the sense of a smooth, place-
    /// grounded texture), `0.0` as entirely free noise — position-continuous
    /// but uncorrelated with any macro cause. Erratic/scatter sits near the
    /// `0.0` end, matching its own §5.6 description ("low — mostly free
    /// noise").
    ///
    /// **"Speckle" is reserved for a different, banned state and does not
    /// belong on this axis.** Spec §5.2's own three-state paragraph is
    /// explicit: *address-hashed* noise (spatially incoherent between
    /// adjacent facets, never sampled by anything in this module — see
    /// [`super`]'s module doc) is the failure H2 forbids outright; low
    /// contextuality is something else, a field that is still smooth and
    /// position-continuous but simply uncorrelated with macro state. §5.2's
    /// table row briefly conflated the two by glossing `0` as "speckle" —
    /// fixed in the spec (fix round 2) to name the property instead of
    /// reusing the loaded word.
    ///
    /// **Spec §5.2's endpoint labels were also inverted relative to this
    /// implementation and to §5.6 in an earlier draft** (`0 = wallpaper, 1 =
    /// speckle`, under which spring's "high" contextuality would have read
    /// as the banned state and erratic's "low" would have read as
    /// wallpaper). Fixed in the spec
    /// (`docs/superpowers/specs/2026-09-03-the-weft-design.md` §5.2) in fix
    /// round 1; this doc states the corrected direction rather than quoting
    /// the sentence that was wrong.
    ///
    /// **`WeftKind::Erratic`'s own value must never be raised to tie it to
    /// macro state (Task 7, controller ruling R5).** The negative control's
    /// whole purpose is a prevalence surface uncorrelated with any macro
    /// cause; a later "improvement" that reads a real `FieldPack` scalar for
    /// it would make Task 9's H3 legibility metric unable to discriminate a
    /// real cause from none — which is precisely what the erratic exists to
    /// let that metric prove it can do.
    ///
    /// **A control-kinds-only dial since The Warp**, for the same reason
    /// [`Self::abundance`] is — see that method's doc. A sign kind's
    /// macro-state tracking is now the soft step [`Self::response`] applies,
    /// not a lerp weight.
    pub(crate) fn contextuality(self) -> f64 {
        match self {
            WeftKind::Thicket => THICKET_CONTEXTUALITY,
            WeftKind::Erratic => ERRATIC_CONTEXTUALITY,
            WeftKind::Spring | WeftKind::Overhang => {
                unreachable!("a sign kind reads its cause through a soft step, not a lerp weight")
            }
        }
    }

    /// Whether `self` is a **sign kind** — spring/seep and overhang/hollow,
    /// the two kinds whose occurrence is authored to stand where a walker's
    /// sign is (The Warp, spec §3, §6). A sign kind's prevalence is
    /// [`Self::rate`] × [`Self::response`]`(cause)` + [`Self::floor`] ×
    /// noise; thicket is texture and erratic the negative control, and both
    /// keep the Weft's own `abundance · (contextuality · cause + (1 −
    /// contextuality) · noise)` expression untouched, bit for bit
    /// (`windows/worldgen/tests/suite/weft_controls.rs`).
    /// type-audit: bare-ok(flag: return)
    pub fn is_sign_kind(self) -> bool {
        matches!(self, WeftKind::Spring | WeftKind::Overhang)
    }

    /// A sign kind's **reliability**: how often it occurs where its cause
    /// saturates (The Warp, spec §6.1). Crate-private — nothing outside the
    /// recipe needs it, and Task 6 recalibrates it.
    pub(crate) fn rate(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_RATE,
            WeftKind::Overhang => OVERHANG_RATE,
            WeftKind::Thicket | WeftKind::Erratic => {
                unreachable!("controls keep the Weft's expression")
            }
        }
    }

    /// A sign kind's **floor**: how often it appears where no cause is (The
    /// Warp, spec §6.1). Zero for both kinds today, by design — which is
    /// exactly why this is readable rather than assumed: the honest-silence
    /// property tests read `floor()` and hold only while it is zero, so
    /// lifting it in a later calibration retires the claim visibly instead
    /// of falsifying an assertion that never named the number.
    ///
    /// `pub` for that reason (the precedent [`Self::macro_state`]'s own doc
    /// sets: widen the one accessor a second tenant needs rather than grow a
    /// parallel implementation of the same read).
    /// type-audit: bare-ok(ratio: return)
    pub fn floor(self) -> f64 {
        match self {
            WeftKind::Spring => SPRING_FLOOR,
            WeftKind::Overhang => OVERHANG_FLOOR,
            WeftKind::Thicket | WeftKind::Erratic => {
                unreachable!("controls keep the Weft's expression")
            }
        }
    }

    /// A sign kind's soft-step edges `(lo, hi)` on `macro_state` (The Warp,
    /// spec §6.2): the response is exactly zero at or below `lo` and exactly
    /// one at or above `hi`. `pub` so the step's SHAPE can be asserted
    /// without asserting either calibrated edge value.
    /// type-audit: bare-ok(ratio: return)
    pub fn step_edges(self) -> (f64, f64) {
        match self {
            WeftKind::Spring => (SPRING_STEP_LO, SPRING_STEP_HI),
            WeftKind::Overhang => (OVERHANG_STEP_LO, OVERHANG_STEP_HI),
            WeftKind::Thicket | WeftKind::Erratic => {
                unreachable!("controls keep the Weft's expression")
            }
        }
    }

    /// This kind's response to its cause: [`smoothstep`] between
    /// [`Self::step_edges`] for a sign kind, and the IDENTITY for a control
    /// (The Warp, spec §6.2). Identity rather than [`unreachable!`] on the
    /// controls because this is the one method of the new family a caller
    /// may legitimately apply to any kind — the recipe's own branch never
    /// calls it for a control, but a reader or a metric comparing the four
    /// kinds' cause-to-prevalence transfer wants a total function, and
    /// "the control passes its cause straight through" is the true answer,
    /// not an error.
    /// type-audit: bare-ok(ratio: cause), bare-ok(ratio: return)
    pub fn response(self, cause: f64) -> f64 {
        if self.is_sign_kind() {
            let (lo, hi) = self.step_edges();
            smoothstep(cause, lo, hi)
        } else {
            cause
        }
    }

    /// This kind's blended macro-state signal in `[0,1]`, given `facet`'s
    /// bilinear corner weights ([`hornvale_kernel::Facet::corner_weights`])
    /// and the materialized [`FieldPack`].
    ///
    /// **Widened from `pub(crate)` to `pub` (The Weft, Task 9).** H3's
    /// legibility readout (spec §7: "mutual information between the local
    /// macro state and the feature set, per kind") needs exactly this
    /// value from `windows/lab`, a different crate — and it is the ONE
    /// quantity that recipe already computes and nothing else in this
    /// crate's public surface exposes (`prevalence` mixes it with noise
    /// before returning). Widening the existing accessor, rather than
    /// duplicating the four kinds' macro-state recipes in `windows/lab`,
    /// matches the precedent `Derived::peek` set at Task 8 (widen the one
    /// accessor a second tenant needs; do not grow a second, parallel
    /// implementation of the same read). No behavior changes.
    /// type-audit: bare-ok(count: weights), bare-ok(ratio: return)
    pub fn macro_state(self, weights: [(Vertex, u64); 4], pack: &FieldPack) -> f64 {
        match self {
            WeftKind::Spring => {
                let carbonate = blend_corner_weights(weights, &pack.carbonate);
                let drainage = blend_corner_weights(weights, &pack.drainage);
                spring_macro_state(carbonate, drainage)
            }
            WeftKind::Overhang => {
                let induration = blend_corner_weights(weights, &pack.induration);
                let slope = blend_corner_weights(weights, &pack.slope);
                overhang_macro_state(induration, slope)
            }
            WeftKind::Thicket => {
                let temperature = blend_corner_weights(weights, &pack.temperature);
                let moisture = blend_corner_weights(weights, &pack.moisture);
                thicket_macro_state(temperature, moisture)
            }
            WeftKind::Erratic => erratic_macro_state(),
        }
    }

    /// Whether `self` may occur at all at `weights`' blended ground — the
    /// early eligibility gate [`super::prevalence`] tests *before* reading a
    /// macro-state cause or drawing any noise (Task 7, R1; see this file's
    /// own module doc). Per-kind, not a single free-standing test, so a
    /// future kind may diverge (spec §5.7: kind N+1 is an append) even
    /// though every kind today shares [`land_eligible`]'s ground test — none
    /// of the four is sensible mid-ocean (a water-source diagnostic, a rock
    /// overhang, standing vegetation, and a deposited boulder all want dry
    /// ground under them).
    pub(crate) fn eligible(self, weights: [(Vertex, u64); 4], pack: &FieldPack) -> bool {
        match self {
            WeftKind::Spring | WeftKind::Overhang | WeftKind::Thicket | WeftKind::Erratic => {
                land_eligible(weights, pack)
            }
        }
    }
}

/// The blended land fraction, `[0,1]`, at or above which ground counts as
/// eligible — `0.5` reads as "the facet's own bilinear blend of
/// [`FieldPack::land`] is majority-land", the continuous analogue of
/// `GeneratedTerrain::is_ocean`'s discrete per-vertex test, applied here to
/// a facet's blended four corners rather than a single vertex (position-
/// continuous, like every other test this module performs — never a single
/// discrete vertex flip at a facet straddling the coastline).
/// plumb: universal(a majority-land threshold on the blended [0,1] ground-eligibility flag, fixed across every world and shared by every kind)
const LAND_ELIGIBILITY_THRESHOLD: f64 = 0.5;

/// The shared ground-eligibility test every [`WeftKind::eligible`] arm
/// currently delegates to (Task 7, R1): `weights`' blend of
/// [`FieldPack::land`] at or above [`LAND_ELIGIBILITY_THRESHOLD`]. `pub(crate)`
/// rather than a private free function so `crate::fieldpack`'s own module
/// doc can point at it by name.
pub(crate) fn land_eligible(weights: [(Vertex, u64); 4], pack: &FieldPack) -> bool {
    blend_corner_weights(weights, &pack.land) >= LAND_ELIGIBILITY_THRESHOLD
}

/// The classic Hermite soft step: exactly `0.0` at or below `lo`, exactly
/// `1.0` at or above `hi`, monotone and C¹-continuous between (The Warp,
/// spec §6.2). Chosen over a bare linear ramp because a walker crossing the
/// edge of a cause should not meet a crease in the density; chosen over a
/// hard threshold because a hard threshold puts a visible contour line on
/// the map where the cause happens to cross a number.
///
/// The clamp is what makes the two endpoints EXACT rather than merely
/// asymptotic, which is what lets a zero floor mean literal silence: below
/// `lo` the whole prevalence is `rate · 0 + 0 · noise`, an exact zero, and
/// [`super::occurs`] compares a uniform variate against it and can never
/// fire. No transcendental is involved, so nothing here needs
/// `hornvale_kernel::math`.
///
/// Callers pass `lo < hi` from [`WeftKind::step_edges`], whose authored
/// pairs the test suite asserts satisfy `0 <= lo < hi <= 1`.
fn smoothstep(x: f64, lo: f64, hi: f64) -> f64 {
    let t = ((x - lo) / (hi - lo)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/// **Reliability** — how often spring/seep occurs where its cause
/// saturates (The Warp, spec §6.1). This is the sign kinds' replacement for
/// the Weft's abundance ceiling: under the regrouped recipe a spring's
/// prevalence is `rate · smoothstep(cause) + floor · noise`, so this number
/// is read directly as "on ground whose karst-and-drainage cause is fully
/// present, roughly this fraction of facets carries a seep" — a promise to
/// a walker, not a ceiling on a lerp.
///
/// **Frozen at Task 6's seed-42 calibration** (The Warp; the round-by-round
/// table and every §7 reading are in the ledger's "Task 6 — constants
/// frozen" section). At `0.95` seed 42's springs stand on 1.34% of land
/// facets, 79% of them on a facet whose own cause reads at or above 0.5
/// (§7's H1 asks 0.60), and no sign class of 100 facets or more carries a
/// spring at a rate above 0.075 (§7's H5 bar is 0.75). It is high, and that
/// is the design: the cause it multiplies is a smoothstep that only
/// saturates on the ~1% of land where karst and channelized drainage are
/// both fully present, so "nineteen facets in twenty" is a promise about
/// that ground and not about limestone country generally. Every test shipped
/// with it asserts mechanism, never this value.
/// plumb: universal(an authored design choice, calibrated on seed 42 in The Warp's Task 6 against the spec section 7 bands and frozen there; fixed across every world)
const SPRING_RATE: f64 = 0.95;

/// **Floor** — how often spring/seep appears where no cause is. Zero by
/// design (The Warp, spec §6.1 and §2): the sign case is honest-silent off
/// its sign. The Weft's `(1 - contextuality) · noise` term gave every kind
/// an unconditional floor, which is what let a spring surface on ground
/// with no karst and no drainage at all; a walker who reads a seep as a
/// sign of water underfoot is then reading noise. Zero closes that.
///
/// **Confirmed at zero by Task 6's calibration**, not merely left there: the
/// ledger's "Task 6 — constants frozen" section records that no §7 band
/// needed a floor lifted off zero on seed 42, which is the condition that
/// section was required to state if one had. The honest silence is
/// load-bearing downstream — it is why the walk-band instruments read spring
/// at an exact zero (`weft_prevalence.rs`'s `KIND_BOUNDS` doc) and why the
/// coastal vantage/eligibility conflict population fell from 35 facets to 27
/// (`windows/vessel/tests/suite/the_weft.rs`).
/// plumb: universal(an authored design choice confirmed at zero by The Warp's Task 6 calibration: the sign kind's noise floor is zero by intent, fixed across every world)
const SPRING_FLOOR: f64 = 0.0;

/// The lower edge of spring/seep's soft step on `macro_state` (The Warp,
/// spec §6.2): at or below this cause the response is exactly zero.
///
/// **Frozen at Task 6's seed-42 calibration** (ledger, "Task 6 — constants
/// frozen"). It is the edge, not the rate, that H1's found fraction turns
/// on: the provisional `0.20` sat below 82% of spring's cause range and read
/// 0.567, while `0.35` reads 0.787. Deliberately kept BELOW `0.5` — H1's
/// found fraction is the share of occurrences standing on a cause of 0.5 or
/// more, so an edge at or above that number would make the band read `1.000`
/// by construction and measure nothing.
/// plumb: universal(an authored design choice, calibrated on seed 42 in The Warp's Task 6 and frozen there; fixed across every world)
const SPRING_STEP_LO: f64 = 0.35;

/// The upper edge of spring/seep's soft step on `macro_state` (The Warp,
/// spec §6.2): at or above this cause the response saturates at one.
///
/// **Frozen at Task 6's seed-42 calibration** (ledger, "Task 6 — constants
/// frozen"). Seed 42's spring cause reads 0.515 at its 99th percentile and
/// 0.700 at its maximum, so a saturation point of `0.55` is reachable on
/// real ground — roughly the top 1% of land — rather than a ceiling the
/// world never touches, which is what makes [`SPRING_RATE`] readable as a
/// promise at all. The 0.20 width against [`SPRING_STEP_LO`] is the soft
/// step §6.2 asks for and not a disguised threshold.
/// plumb: universal(an authored design choice, calibrated on seed 42 in The Warp's Task 6 and frozen there; fixed across every world)
const SPRING_STEP_HI: f64 = 0.55;

/// Spring/seep's correlation length, in facets (spec §5.2, §5.6: "long").
/// A walker should cross many facets of one karst zone before the signal
/// drifts, matching a real spring/seep region's real geographic extent —
/// texture at the scale of a *place*, not a per-step coin flip.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names spring/seep's correlation length "long" and this is the chosen magnitude)
const SPRING_CORRELATION_LENGTH_FACETS: f64 = 40.0;

/// Soft-cap scale for blended drainage before it enters spring/seep's
/// `[0,1]` macro-state mix. `FieldPack::drainage` is an unbounded upstream-
/// vertex COUNT (its own documented range), not a `[0,1]` ratio like
/// `carbonate`; `tanh(drainage / SCALE)` saturates smoothly rather than
/// clamping, so two heavily-drained facets are still told apart the way two
/// lightly-drained ones are, right up to where drainage stops mattering at
/// all. **Not one of spec §5.2's three per-kind scalars** — a units
/// conversion the recipe needs to combine two differently-scaled causes, not
/// a design dial that trades against another kind's abundance/correlation/
/// contextuality.
/// plumb: universal(a units-conversion constant bringing an unbounded upstream-vertex count into the same [0,1] register carbonate already occupies; not itself a design dial)
const SPRING_DRAINAGE_SATURATION: f64 = 12.0;

/// Spring/seep's macro-state recipe (spec §5.6: "carbonate × drainage ×
/// elevation"): blended carbonate (karst solubility) times a saturating read
/// of blended drainage (channelization) — both must be present for a sign of
/// water underfoot, so the recipe multiplies rather than averages.
///
/// **`elevation` is spec §5.6's third listed cause and is not read here.**
/// `FieldPack` does not carry a raw elevation field (Task 4's report records
/// this as a deliberate, flagged scope decision, and Task 7's own
/// `land`-flag addition deliberately avoids reopening the elevation-
/// convention question rather than adding one — see `crate::fieldpack`'s
/// module doc) — this recipe reads the two causes that exist rather than
/// blocking on a third that would need a typed newtype added first. A later
/// task may add it.
fn spring_macro_state(carbonate: f64, drainage: f64) -> f64 {
    let wet = math::tanh(drainage / SPRING_DRAINAGE_SATURATION);
    (carbonate * wet).clamp(0.0, 1.0)
}

/// **Reliability** for overhang/hollow — how often it occurs where its
/// induration-and-slope cause saturates (The Warp, spec §6.1). Lower than
/// spring's: a rock overhang big enough to shelter under and light a fire
/// in is a rarer landmark than a seep, even on ground that fully affords
/// one. Independently dialable (spec §5.2 forbids a simplex across kinds,
/// so this trades against nothing else).
///
/// **Frozen at Task 6's seed-42 calibration** (ledger, "Task 6 — constants
/// frozen"), and the gap to [`SPRING_RATE`]'s `0.95` is the design statement
/// the paragraph above makes, now carrying a number: even on ground that
/// fully affords one, about one facet in six holds a shelter-sized overhang.
/// It is also what §7's H2 turned on. Overhang's cause is far more legible
/// from the rendered words than spring's — its induration and slope ARE the
/// rock word and the steepness word, while spring's drainage is no word at
/// all — so at equal frequency overhang out-reads spring on the channel and
/// H2's "spring ≥ overhang" clause fails. Lowering the reliability of the
/// rarer landmark is the one move that satisfies the clause without touching
/// the response's shape: at these constants spring reads 0.02750 bits net of
/// null against overhang's 0.01388, both on found fractions above 0.77.
/// plumb: universal(an authored design choice, calibrated on seed 42 in The Warp's Task 6 against the spec section 7 bands and frozen there; fixed across every world)
const OVERHANG_RATE: f64 = 0.16;

/// **Floor** for overhang/hollow — zero, for the same reason
/// [`SPRING_FLOOR`] is (The Warp, spec §6.1, §2): an overhang standing on
/// flat, unindurated ground is a sign that means nothing. **Confirmed at
/// zero by Task 6's calibration** on the same terms [`SPRING_FLOOR`] was —
/// see that constant's doc and the ledger's "Task 6 — constants frozen".
/// plumb: universal(an authored design choice confirmed at zero by The Warp's Task 6 calibration: the sign kind's noise floor is zero by intent, fixed across every world)
const OVERHANG_FLOOR: f64 = 0.0;

/// The lower edge of overhang/hollow's soft step on `macro_state` (The
/// Warp, spec §6.2). **Frozen at Task 6's seed-42 calibration** (ledger,
/// "Task 6 — constants frozen"), at the same value as [`SPRING_STEP_LO`] and
/// for the same two reasons: it is what carries H1's found fraction (0.301
/// at the provisional `0.20`, 0.771 here), and it stays strictly below the
/// 0.5 that H1's own found-fraction threshold sits at, so the band measures
/// something rather than reading `1.000` by construction. A HIGHER edge
/// would have read better on H1 and worse on H2 — concentrating occurrences
/// on the strongest cause makes them more legible, and overhang's legibility
/// is the quantity §7 asks to stay under spring's.
/// plumb: universal(an authored design choice, calibrated on seed 42 in The Warp's Task 6 and frozen there; fixed across every world)
const OVERHANG_STEP_LO: f64 = 0.35;

/// The upper edge of overhang/hollow's soft step on `macro_state` (The
/// Warp, spec §6.2). **Frozen at Task 6's seed-42 calibration** (ledger,
/// "Task 6 — constants frozen"). Wider than spring's step (0.30 against
/// 0.20) because overhang's cause is the more broadly distributed of the
/// two — seed 42 reads 0.673 at its 99th percentile against spring's 0.515 —
/// so the same saturation fraction needs a higher upper edge. `0.65` keeps
/// [`OVERHANG_RATE`] reachable on real ground rather than asymptotic.
/// plumb: universal(an authored design choice, calibrated on seed 42 in The Warp's Task 6 and frozen there; fixed across every world)
const OVERHANG_STEP_HI: f64 = 0.65;

/// Overhang/hollow's correlation length, in facets (spec §5.2, §5.6:
/// "short–medium"). Shorter than spring's `40.0`: a rock face's own
/// character changes over a smaller footprint than a karst zone's, but an
/// overhang is still a feature of a *stretch* of terrain, not a per-step
/// coin flip — hence "medium", not spring's own erratic-adjacent floor.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names overhang/hollow's correlation length "short-medium" and this is the chosen magnitude)
const OVERHANG_CORRELATION_LENGTH_FACETS: f64 = 15.0;

/// Soft-cap scale for blended slope (metres of fall per radian,
/// [`FieldPack::slope`]) before it enters overhang/hollow's `[0,1]`
/// macro-state mix — the same `tanh(x / SCALE)` saturation
/// [`SPRING_DRAINAGE_SATURATION`] uses for a different unbounded cause.
///
/// **`= hornvale_terrain::GORGE_SLOPE` (`40_000.0`), MEASURED, not reasoned
/// by analogy (Task 7, fix round 1, I2).** The shipped `8_000.0` was picked
/// as "an order of magnitude gentler than `GORGE_SLOPE`" without checking
/// against real terrain — and seed 42's actual land `|slope|` distribution
/// (11,283 land vertices, `pack.land >= 0.5`) falsifies that reasoning:
/// median `14,853.45`, almost double `8_000.0`, so `tanh(median / 8_000)` is
/// already `0.9524` — the recipe collapses to bare `induration` over most of
/// the land (`tanh >= 0.95` above p30, `>= 0.998` above p75), and the slope
/// half of "induration × slope" was doing almost nothing. `GORGE_SLOPE`
/// itself, re-measured against the SAME distribution, is the graded curve
/// this recipe actually wants:
///
/// | percentile | `\|slope\|` | `tanh(x / 8_000)` (shipped) | `tanh(x / GORGE_SLOPE)` |
/// | --- | --- | --- | --- |
/// | p10 | 3,242 | 0.3845 | 0.0809 |
/// | p25 | 6,922 | 0.6990 | 0.1714 |
/// | median | 14,853 | 0.9524 | 0.3552 |
/// | p75 | 28,150 | 0.9982 | 0.6067 |
/// | p90 | 41,423 | 0.9999 | 0.7761 |
/// | p99 | 98,245 | 1.0000 | 0.9854 |
///
/// Reused directly rather than duplicated as a second `40_000.0` literal —
/// not a coincidence that the same number works for both: `GORGE_SLOPE` is
/// already this world model's own "terrain reads as maximally rugged"
/// ceiling, and overhang/hollow wants exactly that ceiling, not a
/// bespoke fraction of it a canyon wall would still fail to reach.
/// **Not one of spec §5.2's three per-kind scalars** — a units-conversion
/// constant, the same carve-out [`SPRING_DRAINAGE_SATURATION`]'s own doc
/// states.
/// plumb: universal(a units-conversion constant, measured against seed 42's real land-slope distribution and reusing hornvale_terrain::GORGE_SLOPE rather than an unmeasured analogy; not itself a design dial)
const OVERHANG_SLOPE_SATURATION: f64 = hornvale_terrain::GORGE_SLOPE;

/// Overhang/hollow's macro-state recipe (spec §5.6: "induration × slope"):
/// blended induration (rock hardness — a soft rock cannot hold its own
/// roof) times a saturating read of blended slope's steepness (a rock
/// overhang needs a steep face to undercut). `slope.abs()` because
/// [`hornvale_terrain::local_slope`] is signed (fall toward a downhill
/// target) and steepness itself is not directional — an overhang forms on a
/// steep face regardless of which way the local drainage happens to run.
fn overhang_macro_state(induration: f64, slope: f64) -> f64 {
    let steep = math::tanh(slope.abs() / OVERHANG_SLOPE_SATURATION);
    (induration * steep).clamp(0.0, 1.0)
}

/// Abundance ceiling for thicket/brake (spec §5.2). The highest of the four
/// — texture aimed directly at the biome-monotony defect wants real
/// coverage, not a rare landmark's sparse frequency — and independently
/// dialable (spec §5.2).
/// plumb: universal(an authored design ceiling on thicket/brake frequency, fixed across every world and not derived from any seed or pin)
const THICKET_ABUNDANCE: f64 = 0.45;

/// Thicket/brake's correlation length, in facets (spec §5.2, §5.6: "long").
/// The same order as spring/seep's own `40.0` — a stand of vegetation
/// persists over a comparable stretch of terrain to a karst zone — but
/// somewhat longer, since a productive belt (a river's gallery forest, a
/// whole windward slope) is often the larger of the two real-world
/// analogues.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names thicket/brake's correlation length "long" and this is the chosen magnitude)
const THICKET_CORRELATION_LENGTH_FACETS: f64 = 60.0;

/// Thicket/brake's contextuality (spec §5.2, §5.6: "high — productivity ×
/// moisture"). Matches spring/seep's own `0.85`: a thicket is diagnostic of
/// where the ground can actually support it, the same "sign case" posture
/// spring/seep has, just for vegetation instead of water.
/// plumb: universal(an authored design choice fixing how strongly thicket/brake tracks macro state versus free noise, identical across every world)
const THICKET_CONTEXTUALITY: f64 = 0.85;

/// The temperature (°C) at which thicket/brake's Liebig temperature
/// response peaks, mirroring `windows/locale`'s own Miami-model NPP proxy
/// constant (`NPP_TEMP_OPTIMUM_C`) — the same biologically-motivated
/// magnitude, independently owned here rather than imported, because
/// `hornvale-worldgen` may not depend on `windows/locale` (the same
/// layering [`super`]'s module doc states). Not required to track
/// `windows/locale`'s constant bit-for-bit: the two recipes answer
/// different questions (a room's food value vs. a facet's odds of carrying
/// standing vegetation) and are permitted to diverge if a future tuning
/// pass finds a reason to.
/// plumb: universal(an authored biologically-motivated design constant fixed across every world, independently owned from windows/locale's own analogous constant)
const THICKET_TEMP_OPTIMUM_C: f64 = 20.0;

/// The temperature tolerance (°C) either side of [`THICKET_TEMP_OPTIMUM_C`]
/// over which thicket/brake's Liebig temperature response falls linearly to
/// zero — mirrors `windows/locale`'s own `NPP_TEMP_TOLERANCE_C` for the same
/// reason and under the same independence [`THICKET_TEMP_OPTIMUM_C`]'s doc
/// states.
/// plumb: universal(an authored biologically-motivated design constant fixed across every world, independently owned from windows/locale's own analogous constant)
const THICKET_TEMP_TOLERANCE_C: f64 = 30.0;

/// Thicket/brake's macro-state recipe (spec §5.6: "productivity — temperature
/// × moisture"): a Miami-model net-primary-productivity proxy, computed
/// **blend-then-combine** — `temperature`/`moisture` are each already
/// blended by the caller ([`WeftKind::macro_state`]'s `Thicket` arm) before
/// this function combines them via a Liebig minimum, the same order
/// `windows/locale`'s `LocaleContext::productivity_with_weights` uses and
/// the exact order `crate::fieldpack`'s module doc forbids inverting (a
/// materialized `productivity` field would combine-then-blend instead, and
/// the minimum's non-linearity makes that a different quantity). This is an
/// independent reimplementation, not a call into `windows/locale` — the
/// same layering [`super`]'s module doc states forbids that dependency
/// direction outright.
fn thicket_macro_state(temperature_c: f64, moisture: f64) -> f64 {
    let temp_response = (1.0
        - (temperature_c - THICKET_TEMP_OPTIMUM_C).abs() / THICKET_TEMP_TOLERANCE_C)
        .clamp(0.0, 1.0);
    temp_response.min(moisture.clamp(0.0, 1.0))
}

/// Abundance ceiling for erratic/scatter (spec §5.2). The lowest of the
/// four — a glacially-deposited boulder or a wind-scoured outcrop is meant
/// to read as a rare, isolated find, not texture — and independently
/// dialable (spec §5.2).
/// plumb: universal(an authored design ceiling on erratic/scatter frequency, fixed across every world and not derived from any seed or pin)
const ERRATIC_ABUNDANCE: f64 = 0.08;

/// Erratic/scatter's correlation length, in facets (spec §5.2, §5.6:
/// "short"). The shortest of the four: a negative control with a long
/// correlation length would read as a smooth region of "sometimes an
/// erratic", which is a texture, not the isolated, spatially-continuous-
/// but-uncorrelated scatter spec §5.6 asks for.
/// plumb: universal(an authored texture-vs-landmark design choice fixed across every world; spec section 5.6 names erratic/scatter's correlation length "short" and this is the chosen magnitude)
const ERRATIC_CORRELATION_LENGTH_FACETS: f64 = 5.0;

/// Erratic/scatter's contextuality — deliberately near-zero (spec §5.2,
/// §5.6: "low — mostly free noise"; Task 7, controller ruling R5). Under
/// the corrected §5.2 mapping (`0` = free noise, `1` = wallpaper), a small
/// but nonzero value keeps this kind exercising the same general mixing
/// recipe every other kind uses (spec §5.7: a kind is its recipe plus its
/// three scalars, never a special-cased fifth thing) while contributing
/// almost nothing: [`erratic_macro_state`] returns a spatially uninformative
/// constant, so even this kind's own tiny macro-state weight carries no
/// real signal — see that function's own doc. **Must never be raised to tie
/// this kind to macro state** — see [`WeftKind::contextuality`]'s doc for
/// why.
/// plumb: universal(an authored design choice fixing erratic/scatter as the negative control, near-zero macro tracking, identical across every world)
const ERRATIC_CONTEXTUALITY: f64 = 0.05;

/// Erratic/scatter's constant "macro state" — deliberately **not** a real
/// `FieldPack` scalar (Task 7, controller ruling R5). The negative control's
/// whole purpose is a prevalence surface uncorrelated with any macro cause,
/// so this returns the identical value at every facet regardless of the
/// ground beneath it. A constant carries no spatial information, so even
/// multiplied against [`ERRATIC_CONTEXTUALITY`]'s own small weight it
/// contributes no signal-bearing texture — only the free-noise term does.
///
/// **Never change this to read a real [`FieldPack`] field.** Doing so ties
/// the negative control to macro state, which is the exact "improvement"
/// Task 9's H3 legibility-discrimination check depends on this kind never
/// receiving: if erratic/scatter starts tracking a real cause, it stops
/// being a control and the metric loses the one designed case that proves
/// it can tell a real signal from none.
/// plumb: universal(a fixed, deliberately uninformative constant standing in for "no macro cause" — the negative control's whole point, identical across every world)
const ERRATIC_MACRO_BASELINE: f64 = 0.5;

/// Erratic/scatter's macro-state recipe: [`ERRATIC_MACRO_BASELINE`],
/// unconditionally. See that constant's own doc for why this is correct and
/// must stay this way.
fn erratic_macro_state() -> f64 {
    ERRATIC_MACRO_BASELINE
}
