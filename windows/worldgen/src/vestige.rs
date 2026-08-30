//! Subsurface historical residue — sealed wards, abandoned delvings, buried
//! ruins, gate-scars (The Vestige). A derived reading of the narrated past
//! (settlement-abandonment history + terrain): no live mutation, no committed
//! facts, no metaphysics — the door and its dread, not the entity.

use crate::history_emit::{occupations_at, occupations_by_vertex, present_year};
use hornvale_history::record::{CauseOfEnd, Function, OccupationRecord};
use hornvale_kernel::{Vertex, VertexMap, World, math};
use hornvale_terrain::GeneratedTerrain;

/// What a residue site is, by maker → purpose.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VestigeKind {
    /// Dormant dimensional wound in the primordial deep (pre-human).
    GateScar,
    /// The deep sealed its own chamber (pre-human, no maker).
    NaturalSeal,
    /// An abandoned mine / exhausted delving.
    AbandonedDelving,
    /// A buried ruin or, at Seat scale, an undercity / necropolis.
    BuriedRuin,
    /// A custodial ward or tomb (a Fort/Cult site).
    SealedVault,
}

/// How intact the containment is, read from the keeper's fate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SealState {
    /// A living keeper still tends it; containment is sound.
    Maintained,
    /// The keeper is gone but the seal has not had time to fail.
    Lapsing,
    /// The seal has failed; the site lies open.
    Breached,
}

/// Remembered-sacred vs forgotten-dreaded — the same axis as seal-state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Valence {
    /// Still tended and held in reverence.
    Venerated,
    /// No longer remembered for what it was; only dread remains.
    Forgotten,
}

/// The kind of danger the residue now poses (feeds the dread field).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HazardKind {
    /// Collapse risk from decayed construction.
    Structural,
    /// Foul or asphyxiating gas pooled underground.
    ToxicGas,
    /// Disease-bearing residue left by a plague end.
    Pestilent,
    /// Standing or seeping water.
    Flooded,
    /// A lingering supernatural charge: a danger the living cannot account
    /// for.
    ///
    /// **Two producers, not one.** A pre-human gate scar
    /// ([`prehuman_vestige`]) is the original, and The Winze added the second:
    /// a delving that ended by [`CauseOfEnd::Breached`], where the model
    /// itself does not know what came through (spec §4.6) and so has nothing
    /// more specific to offer. The parenthetical "(pre-human sites)" this doc
    /// used to carry is therefore retired — a `Numinous` vestige is no longer
    /// evidence that a site predates people.
    Numinous,
    /// Held to be cursed by those who remember it.
    Cursed,
}

/// A located residue feature (one layer of a vertex's palimpsest).
/// type-audit: bare-ok(ratio: dread), bare-ok(ratio: warning_legibility), bare-ok(diagnostic-value: founded_day)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vestige {
    /// What the site is, by maker and purpose.
    pub kind: VestigeKind,
    /// How intact the containment is.
    pub seal_state: SealState,
    /// Remembered-sacred vs forgotten-dreaded.
    pub valence: Valence,
    /// The kind of danger the residue now poses.
    pub hazard: HazardKind,
    /// Dread the site radiates, [0,1]: forgotten + breached is highest.
    pub dread: f64,
    /// How readable the old warning still is, [0,1]: decays fast with age.
    pub warning_legibility: f64,
    /// The historical day it was founded (people-made); None for pre-human.
    pub founded_day: Option<f64>,
}

/// The warning's decay scale, in **bake years** — the single constant behind
/// both of this module's age readings: the `Lapsing -> Breached` seal
/// threshold, and the `exp(-age / this)` legibility curve.
///
/// **RENAMED FROM `WARNING_HALF_LIFE_DAYS` (The Winze, Task 6), because that
/// name was wrong twice and the unit half is a determinism trap.** Grep either
/// half of the old name to land here.
///
/// 1. **The unit is YEARS, not days.** Every caller reaches this through
///    [`vestige_from_occupation`]'s `now`, and every producer of that argument
///    ([`vestiges_at`], [`vestiges_field`]) passes
///    [`crate::present_year`] — a bake year, the same unit
///    `OccupationRecord::founded` and `::ended` carry, which those fields'
///    own docs state outright. The trap is not cosmetic: `present_frame` sits
///    beside `present_year` in the same export list and returns ledger DAYS,
///    so a reader repairing the call to match the old name would have
///    multiplied every ruin's apparent age by the day/year factor and moved
///    every world's residue. Measured on the campaign's 12-seed panel: the
///    ages that actually reach this constant run 25.0 to 1500.0 against a
///    `present_year` of 2000.0, which is years on its face.
/// 2. **It is an e-folding time, not a half-life.** `exp(-t/T)` is `1/e`
///    (0.368) at `t = T`, not one half; the actual half-life is `T ln 2`,
///    about 208 years at this value. The Vestige's chronicle prose calls it a
///    half-life and that reading is what the arithmetic never supported.
///
/// **The VALUE is untouched and deliberately so.** 300 years is roughly an
/// order of magnitude longer than `MEM-2`'s ~3-generation floating gap (the
/// warning is still 77% legible at 80 years and does not fall below 0.1 until
/// ~690 years, ~23 generations). Whether that is the right scale is a
/// calibration question this task did not preregister, and moving it would
/// move the gallery almanacs, the residue lens and four census columns; it is
/// reported rather than tuned.
const WARNING_EFOLD_YEARS: f64 = 300.0;

/// Derive a people-made vestige from one occupation, as of `now` — a bake
/// **year** ([`crate::present_year`]), the same unit `OccupationRecord`'s
/// `founded`/`ended` carry. Read backward: no simulation.
///
/// This doc said "(days)" until The Winze's Task 6, contradicting
/// [`vestiges_at`]'s own doc sixty lines below, which has always said year.
/// See this module's `WARNING_EFOLD_YEARS` for why the disagreement was worth
/// a commit.
/// type-audit: bare-ok(diagnostic-value: now)
pub fn vestige_from_occupation(occ: &OccupationRecord, now: f64) -> Vestige {
    let kind = match occ.core.function {
        Function::Mine => VestigeKind::AbandonedDelving,
        Function::Fort | Function::Cult => VestigeKind::SealedVault,
        // Everything else is a buried ruin; the undercity/ruin size split rides
        // on notability at the consumer (the almanac counts seats as
        // undercities), not on a distinct vestige kind.
        _ => VestigeKind::BuriedRuin,
    };
    let (seal_state, valence) = match occ.core.ended {
        None => (SealState::Maintained, Valence::Venerated), // a living keeper
        Some(end) => {
            let age = (now - end).max(0.0);
            if age < WARNING_EFOLD_YEARS {
                (SealState::Lapsing, Valence::Forgotten)
            } else {
                (SealState::Breached, Valence::Forgotten)
            }
        }
    };
    let hazard = match occ.core.cause {
        Some(CauseOfEnd::Plague) => HazardKind::Pestilent,
        Some(CauseOfEnd::Burned) => HazardKind::Cursed,
        // A BREACH GETS ITS OWN ARM RATHER THAN THE DEFAULT (The Winze, spec
        // §4.3/§4.5), and the arm is [`HazardKind::Numinous`] rather than a
        // seventh variant. Three things decided it:
        //
        // 1. **`Structural` would be wrong in kind, not merely imprecise.**
        //    Its own doc says "collapse risk from decayed construction" — a
        //    danger that accrues after the people leave. A breached working's
        //    danger arrived while they were still in it, and is the reason
        //    they are not. Letting it fall through `_` would have been the
        //    quiet outcome: no compiler error (this match has a wildcard), a
        //    plausible-looking hazard, and Task 6's three legibility states
        //    resting on a category error.
        // 2. **Every other named variant would violate §4.6.** `ToxicGas`,
        //    `Flooded`, `Pestilent` and `Cursed` each assert a specific,
        //    knowable danger — which is to say each of them names what came
        //    through. Nothing knows.
        // 3. **`Numinous` is the model's existing word for an unaccountable
        //    danger**, and this is a *derived read*, not a record: a vestige
        //    is what a later people perceives at a site, so it carries an
        //    appearance and never a source (decision 0003). "Something here
        //    is wrong and we cannot say what" is exactly spec §4.5's DECAYED
        //    state, and it is what `Numinous` already meant.
        //
        // The cost, stated: a breached delving now reads identically to a
        // pre-human gate scar on this axis. That is a property rather than a
        // collision — a later culture genuinely cannot tell the two apart —
        // but it means `HazardKind::Numinous` is no longer a sufficient test
        // for "pre-human", and `prehuman_vestige` is the thing to ask instead.
        Some(CauseOfEnd::Breached) => HazardKind::Numinous,
        // Every other end leaves structural collapse as the default; per-function
        // hazards (a flooded mine, toxic gas) are a later refinement.
        _ => HazardKind::Structural,
    };
    // Warning decays fast (the fastest of the three rates); recent = legible.
    let warning_legibility = match occ.core.ended {
        None => 1.0,
        Some(end) => math::exp(-((now - end).max(0.0)) / WARNING_EFOLD_YEARS),
    };
    // Dread: forgotten + breached + hazardous is highest; venerated is low.
    let base = match valence {
        Valence::Venerated => 0.1,
        Valence::Forgotten => 0.6,
    };
    let dread = (base + 0.4 * (1.0 - warning_legibility)).clamp(0.0, 1.0);
    Vestige {
        kind,
        seal_state,
        valence,
        hazard,
        dread,
        warning_legibility,
        founded_day: Some(occ.core.founded),
    }
}

/// A rare pre-human gate-scar, if this vertex's deep crust is old enough and
/// the shared hash-noise presence test fires
/// (`GeneratedTerrain::prehuman_scar_at`, mirroring `cave_at`/`deposit_at`'s
/// pattern). Pure and deterministic: no draws, no facts, no epoch — the
/// presence gate reuses The Lode's FEATURES noise seed, so no new stream
/// label is introduced. Ocean vertices never qualify (nothing pre-human is
/// legible under open water in this model). `founded_day` is `None`:
/// pre-human residue predates the narrated (people) timeline entirely.
pub fn prehuman_vestige(terrain: &GeneratedTerrain, vertex: Vertex) -> Option<Vestige> {
    if !terrain.prehuman_scar_at(vertex) {
        return None;
    }
    Some(Vestige {
        kind: VestigeKind::GateScar,
        seal_state: SealState::Breached,
        valence: Valence::Forgotten,
        hazard: HazardKind::Numinous,
        // Forgotten before there was anyone to forget it: maximally dreaded,
        // with no warning ever legible to begin with.
        dread: 0.9,
        warning_legibility: 0.0,
        founded_day: None,
    })
}

/// The full palimpsest stack at a vertex, oldest layer first: the pre-human
/// residue (if any) — deep-time-old, so always first — then every people
/// occupation (`occupations_at`, already oldest-founded-first), each read
/// forward to `now` via `vestige_from_occupation`. Pure derived read: no
/// facts are written, nothing is mutated. `now` is the world's committed
/// present frame (see [`crate::present_year`]) — a bake **year**, the same
/// unit `OccupationRecord::founded` carries, which is what
/// `vestige_from_occupation` subtracts it from.
pub fn vestiges_at(world: &World, terrain: &GeneratedTerrain, vertex: Vertex) -> Vec<Vestige> {
    let now = present_year(world);
    let mut layers = Vec::new();
    if let Some(prehuman) = prehuman_vestige(terrain, vertex) {
        layers.push(prehuman);
    }
    layers.extend(
        occupations_at(world, vertex)
            .iter()
            .map(|occ| vestige_from_occupation(occ, now)),
    );
    layers
}

/// The whole world's palimpsest field, one layer-stack per vertex — the
/// batched sibling of [`vestiges_at`] the per-vertex dread field and residue
/// lens now build over. `vestiges_at` calls `occupations_at`, which rescans
/// the entire ledger (`occupation_records`) on every call; asking for it once
/// per vertex in a `VertexMap::from_fn` loop is `O(vertices × occupations)` ledger
/// reconstructions. This instead scans the ledger exactly once
/// ([`occupations_by_vertex`]) and reads `present_year` once, so the whole field
/// costs `O(occupations + vertices)`. Per vertex, the stack is built in the exact
/// same order `vestiges_at` produces (the pre-human layer first, if any, then
/// that vertex's occupations oldest-founded-first via
/// [`vestige_from_occupation`]) — so `vestiges_field(world, terrain).get(vertex)`
/// is byte-for-byte identical to `&vestiges_at(world, terrain, vertex)` for
/// every vertex (see this module's `vestiges_field_matches_vestiges_at_per_vertex`
/// test).
pub fn vestiges_field(world: &World, terrain: &GeneratedTerrain) -> VertexMap<Vec<Vestige>> {
    let now = present_year(world);
    let by_vertex = occupations_by_vertex(world);
    let geo = terrain.geosphere();
    VertexMap::from_fn(geo, |vertex| {
        let mut layers = Vec::new();
        if let Some(prehuman) = prehuman_vestige(terrain, vertex) {
            layers.push(prehuman);
        }
        if let Some(occs) = by_vertex.get(&vertex) {
            layers.extend(occs.iter().map(|occ| vestige_from_occupation(occ, now)));
        }
        layers
    })
}

#[cfg(test)]
// Test fixture (decision 0092): calls the sculpt/fit derivation entry
// points directly to build its own world state, once per test — the
// sanctioned test-fixture posture the weir's spec carves out.
#[allow(clippy::disallowed_methods)]
mod tests {
    use super::*;
    use hornvale_history::record::{
        CauseOfEnd, Ended, Founding, Function, Notability, Occupation, OccupationRecord,
        TechHorizon,
    };
    use hornvale_kernel::{EntityId, KindId, Vertex};

    fn eid(n: u64) -> EntityId {
        EntityId::new(n).unwrap()
    }

    fn occ(
        function: Function,
        ended: Option<f64>,
        cause: Option<CauseOfEnd>,
        notability: Notability,
    ) -> OccupationRecord {
        OccupationRecord {
            core: Occupation {
                people: KindId("test-people"),
                site: Vertex(0),
                founded: 0.0,
                ended,
                peak_population: 100,
                tech: TechHorizon::Iron,
                function,
                deity: None,
                tongue: None,
                cause,
                notability,
                delve_depth_m: 0.0,
            },
            id: eid(1),
            founded_from: Founding::Genesis(Vertex(0)),
            ended_by: Ended::Nature,
        }
    }

    #[test]
    fn a_living_ward_is_maintained_and_venerated() {
        let v = vestige_from_occupation(&occ(Function::Fort, None, None, Notability::Seat), 2000.0);
        assert_eq!(v.seal_state, SealState::Maintained);
        assert_eq!(v.valence, Valence::Venerated);
    }

    #[test]
    fn an_ancient_ruin_is_breached_and_forgotten_with_a_lost_warning() {
        let v = vestige_from_occupation(
            &occ(
                Function::Mine,
                Some(50.0),
                Some(CauseOfEnd::Fled),
                Notability::Common,
            ),
            2000.0,
        );
        assert_eq!(v.seal_state, SealState::Breached);
        assert_eq!(v.valence, Valence::Forgotten);
        assert!(
            v.warning_legibility < 0.2,
            "ancient warning decayed to near-illegible"
        );
    }

    #[test]
    fn a_plague_end_yields_a_pestilent_hazard() {
        let v = vestige_from_occupation(
            &occ(
                Function::Cult,
                Some(1900.0),
                Some(CauseOfEnd::Plague),
                Notability::Common,
            ),
            2000.0,
        );
        assert_eq!(v.hazard, HazardKind::Pestilent);
    }

    // ---------------------------------------------------------------------
    // WHAT A LATER CULTURE CAN KNOW — The Winze, Task 6 (spec §4.5).
    //
    // Three states, and the finding is that the shipped formulas already
    // produce all three: nothing in this module's derivation changed for
    // Task 6 beyond a constant's NAME. These are the assertions that were
    // missing, plus the live corroboration in
    // `windows/worldgen/tests/suite/breach.rs` that a real world reaches
    // each state rather than only a constructed record doing so.
    //
    // TWO `Breached`ES, AND THEY ARE DIFFERENT THINGS. `SealState::Breached`
    // is the WARD having failed — an age reading, derived from `ended` alone.
    // `CauseOfEnd::Breached` is the DELVING having broken through — why an
    // occupation stopped. Neither implies the other, and the tests below turn
    // that on its head deliberately: `a_maintained_ward_reads_as_safe` is
    // about a seal state with no cause at all, while
    // `an_ancient_breach_is_dreaded_and_illegible` is a delving whose seal
    // state happens to share the word.
    // ---------------------------------------------------------------------

    /// The bake's committed present (`BakeConfig::end_year`), so these fixture
    /// ages are the ages a real world actually presents to
    /// [`vestige_from_occupation`] rather than arbitrary numbers.
    const PRESENT_YEAR: f64 = 2000.0;

    /// The youngest breach age on the campaign's 12-seed panel, measured
    /// (`26` breaches, ages `25.0 ..= 1500.0`). Using the measured extreme
    /// rather than `0.0` keeps the RECENT assertion about a state the world
    /// reaches, not a limit it approaches.
    const YOUNGEST_PANEL_BREACH_AGE: f64 = 25.0;

    /// The oldest breach age on the same panel.
    const OLDEST_PANEL_BREACH_AGE: f64 = 1500.0;

    /// §4.5 RECENT — "the warning is legible. A later people can read what
    /// happened."
    ///
    /// What is legible is the SHAPE of the event and not its content: a
    /// delving (`AbandonedDelving`), founded by people (`founded_day`), whose
    /// warning is still all but intact. The hazard is `Numinous` even here,
    /// at full legibility, and that is §4.6 rather than a decay artifact —
    /// nothing knows what came through, so there is nothing more specific to
    /// read at any age.
    #[test]
    fn a_recent_breach_is_legible() {
        let v = vestige_from_occupation(
            &occ(
                Function::Mine,
                Some(PRESENT_YEAR - YOUNGEST_PANEL_BREACH_AGE),
                Some(CauseOfEnd::Breached),
                Notability::Common,
            ),
            PRESENT_YEAR,
        );
        assert_eq!(v.kind, VestigeKind::AbandonedDelving);
        assert!(
            v.warning_legibility > 0.9,
            "a breach {YOUNGEST_PANEL_BREACH_AGE} years old is still legible, got {}",
            v.warning_legibility
        );
        assert_eq!(
            v.seal_state,
            SealState::Lapsing,
            "the ward has not had time to fail yet"
        );
        assert!(
            v.founded_day.is_some(),
            "a people-made vestige carries its founding"
        );
        assert_eq!(
            v.hazard,
            HazardKind::Numinous,
            "even fully legible, the model names nothing that came through (§4.6)"
        );
    }

    /// §4.5 DECAYED — "warning_legibility -> 0, dread -> high. They know
    /// SOMETHING is wrong and not what." MEM-2's floating gap.
    ///
    /// The second half of that sentence is the assertion worth having, and it
    /// is Task 4's inherited cost made mechanical: at this age the vestige's
    /// whole danger reading — hazard, seal state, valence — is the exact
    /// triple [`prehuman_vestige`] writes for a gate-scar older than any
    /// people. A later culture holding only these three fields cannot tell a
    /// working that broke through from a wound that predates them.
    #[test]
    fn an_ancient_breach_is_dreaded_and_illegible() {
        let v = vestige_from_occupation(
            &occ(
                Function::Mine,
                Some(PRESENT_YEAR - OLDEST_PANEL_BREACH_AGE),
                Some(CauseOfEnd::Breached),
                Notability::Common,
            ),
            PRESENT_YEAR,
        );
        assert!(
            v.warning_legibility < 0.01,
            "a breach {OLDEST_PANEL_BREACH_AGE} years old has an all-but-lost warning, got {}",
            v.warning_legibility
        );
        assert!(
            v.dread > 0.99,
            "forgotten and illegible is the top of the dread range, got {}",
            v.dread
        );
        assert_eq!(v.seal_state, SealState::Breached);
        assert_eq!(v.valence, Valence::Forgotten);
        assert_eq!(
            (v.hazard, v.seal_state, v.valence),
            (
                HazardKind::Numinous,
                SealState::Breached,
                Valence::Forgotten
            ),
            "the danger reading is exactly a pre-human gate-scar's — they know \
             something is wrong and not what"
        );
    }

    /// The cost Task 4 stated and this task pins: **`HazardKind::Numinous` is
    /// no longer evidence a site predates people.**
    ///
    /// The variant's doc used to carry a "(pre-human sites)" parenthetical and
    /// The Winze retired it, because a breached delving now reaches the same
    /// arm. Nothing in the tree ever inferred pre-human origin FROM the hazard
    /// — the three live readers all ask `VestigeKind::GateScar` (the almanac's
    /// gate-scar count, the residue lens's palette) or `prehuman_vestige`
    /// itself — and this test is what keeps that true if someone adds a
    /// fourth.
    #[test]
    fn a_numinous_hazard_no_longer_implies_a_prehuman_site() {
        let v = vestige_from_occupation(
            &occ(
                Function::Mine,
                Some(1500.0),
                Some(CauseOfEnd::Breached),
                Notability::Common,
            ),
            PRESENT_YEAR,
        );
        assert_eq!(v.hazard, HazardKind::Numinous);
        assert_ne!(
            v.kind,
            VestigeKind::GateScar,
            "the people-made kind is what separates the two producers"
        );
        assert!(
            v.founded_day.is_some(),
            "a Numinous vestige with a founding day is a people-made one — \
             founded_day and GateScar are the pre-human tests now, not the hazard"
        );
    }

    /// §4.5 WARDED — "`SealState::Maintained` reads SAFE — and may be wrong.
    /// A ward that is being kept is indistinguishable from a place that was
    /// never dangerous."
    ///
    /// # THIS IS NOT A BUG AND MUST NOT BE FIXED
    ///
    /// It is the design's central property (spec §4.5): the model can be
    /// mistaken in the direction that kills people without anyone authoring a
    /// deception, and it is decision 0003's source-blindness arriving
    /// structurally — a later culture receives an appearance (dread), never a
    /// source (what is behind the wall). Spec amendment C.4 sharpens it:
    /// 57% of cave systems are multi-entrance (1.87 apertures per system,
    /// pooled), so a kept ward is *provably* insufficient rather than merely
    /// fallible. **A term added here that lets a reader separate "warded and
    /// dangerous" from "never dangerous" deletes the property**, which is why
    /// the equalities below are asserted field by field rather than left to a
    /// reviewer to notice.
    ///
    /// The one field that DOES separate them is `kind`, and it is asserted
    /// too: a `SealedVault` says what the site is FOR, never whether it holds
    /// anything. `dread`, `seal_state`, `valence`, `hazard` and
    /// `warning_legibility` — every axis danger is read off — agree exactly.
    #[test]
    fn a_maintained_ward_reads_as_safe() {
        let ward = vestige_from_occupation(
            &occ(Function::Fort, None, None, Notability::Seat),
            PRESENT_YEAR,
        );
        let never_dangerous = vestige_from_occupation(
            &occ(Function::Agrarian, None, None, Notability::Common),
            PRESENT_YEAR,
        );

        assert_eq!(ward.kind, VestigeKind::SealedVault);
        assert_eq!(ward.seal_state, SealState::Maintained);
        assert!(
            ward.dread <= 0.1,
            "a kept ward radiates the floor of the dread range, got {}",
            ward.dread
        );

        assert_eq!(ward.dread, never_dangerous.dread);
        assert_eq!(ward.seal_state, never_dangerous.seal_state);
        assert_eq!(ward.valence, never_dangerous.valence);
        assert_eq!(ward.hazard, never_dangerous.hazard);
        assert_eq!(
            ward.warning_legibility, never_dangerous.warning_legibility,
            "a kept ward and a place that was never dangerous read identically \
             on every axis a later people reads danger off"
        );
        assert_ne!(
            ward.kind, never_dangerous.kind,
            "purpose is the only thing that separates them, and purpose is not danger"
        );
    }

    use crate::{SettlementPins, SkyChoice, build_world, occupation_records, terrain_of};
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;

    /// Build the seed-42 Full world (and its terrain provider) once per test,
    /// the same helper `history_emit`'s tests use.
    fn seed_42_world_and_terrain() -> (hornvale_kernel::World, GeneratedTerrain) {
        world_and_terrain(42)
    }

    /// The seed the pre-human tests use, because **seed 42 no longer has a
    /// pre-human gate scar at all.**
    ///
    /// This is a finding, and it is why three tests in this module moved off
    /// the module's default fixture rather than being re-pinned on a different
    /// vertex of it. The scar gate is `land && crust_age > ANCIENT_CRUST_AGE &&
    /// fbm01(...) < PREHUMAN_SCAR_THRESHOLD (0.3)`, and seed 42 cleared it on
    /// exactly ONE vertex before decision 0134 — `Vertex(21966)`, described in
    /// this module's own comment as "the one pre-human hit among that seed's
    /// ~1900 ancient-crust vertices". The terrain epoch moved the coastline to the
    /// shelf break, and seed 42's ancient-crust land fell to 1,664 vertices whose
    /// **minimum noise is 0.3322** — clear of the threshold everywhere. One
    /// marginal hit became none.
    ///
    /// **The feature itself is alive and is not marginal.** Measured at
    /// `GLOBE_LEVEL` over ten seeds after the epoch: 0 -> 19 scars, 1 -> 5,
    /// 2 -> 5, 3 -> 1, 4 -> 18, 5 -> 0, 6 -> 4, 7 -> 0, 8 -> 23, 42 -> 0.
    /// Seven of ten worlds carry one; seed 42 is simply not one of them any
    /// more. Seed 0 is chosen because 19 scars over 4,886 ancient-crust vertices
    /// is a comfortable fixture rather than another single marginal hit — the
    /// shape that made 21966 die to the first epoch that touched it.
    const PREHUMAN_SEED: u64 = 0;

    /// Build a Full world and its terrain provider at any seed.
    fn world_and_terrain(seed: u64) -> (hornvale_kernel::World, GeneratedTerrain) {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let terrain = terrain_of(&world).unwrap();
        (world, terrain)
    }

    /// A vertex carrying a pre-human gate scar: land, ancient continental crust,
    /// and a presence draw that clears terrain's `prehuman_scar_at` gate.
    ///
    /// **Searched, not pinned, and that is a repair.** This used to be the
    /// constant `Vertex(21966)`, described in its own doc comment as "the one
    /// pre-human hit among that seed's ~1900 ancient-crust vertices" — a single
    /// sample standing in for a property, and the narrowest possible one. The
    /// terrain epoch of decision 0134 turned it to ocean, so every test that
    /// read it went red on its own premise assert ("the fixture vertex must be
    /// land"). Re-pinning on another seed-42 vertex was not available either:
    /// that seed now has no qualifying vertex whatever (see [`PREHUMAN_SEED`]).
    ///
    /// This is not the seed-hunting decision 0093 forbids: no world is built to
    /// find the vertex. The scan is a pure read over one already-built terrain,
    /// in deterministic `Vertex` order, and it selects on the same gate
    /// [`prehuman_vestige`] itself applies — so the assertions downstream are
    /// about what a pre-human vestige *is*, which is the constructor's property
    /// and not the vertex's.
    fn deep_ancient_numinous_vertex(terrain: &GeneratedTerrain) -> Vertex {
        terrain
            .geosphere()
            .vertices()
            .find(|&c| {
                !terrain.is_ocean(c)
                    && terrain.crust_age_at(c) > ANCIENT_CRUST_AGE_FOR_TEST
                    && prehuman_vestige(terrain, c).is_some()
            })
            .expect(
                "the fixture seed must carry at least one ancient land vertex whose \
                 presence draw fires the pre-human scar gate — a world with none is a \
                 finding to report, not a test to weaken",
            )
    }

    /// A land vertex at seed 42 whose crust falls short of the ancient
    /// threshold (`crust_age_at` ~0.716) — fails the age gate regardless of
    /// the noise draw.
    const SHALLOW_YOUNG_VERTEX: Vertex = Vertex(5);

    /// Terrain's ancient-crust threshold, mirrored here only for these
    /// fixture assertions (`GeneratedTerrain::prehuman_scar_at`'s internal
    /// gate; see `domains/terrain/src/provider.rs`'s `ANCIENT_CRUST_AGE`).
    const ANCIENT_CRUST_AGE_FOR_TEST: f64 = 0.8;

    #[test]
    fn a_deep_ancient_vertex_can_yield_a_prehuman_gate_scar() {
        let (_world, terrain) = world_and_terrain(PREHUMAN_SEED);
        let vertex = deep_ancient_numinous_vertex(&terrain);
        assert!(!terrain.is_ocean(vertex), "the fixture vertex must be land");
        assert!(
            terrain.crust_age_at(vertex) > ANCIENT_CRUST_AGE_FOR_TEST,
            "the fixture vertex must be ancient"
        );
        let v = prehuman_vestige(&terrain, vertex)
            .expect("ancient crust + a firing presence draw yields a pre-human vestige");
        assert_eq!(v.kind, VestigeKind::GateScar);
        assert_eq!(v.hazard, HazardKind::Numinous);
        assert_eq!(v.valence, Valence::Forgotten);
        assert_eq!(v.seal_state, SealState::Breached);
        assert_eq!(v.founded_day, None);
    }

    #[test]
    fn a_shallow_young_vertex_yields_no_prehuman_vestige() {
        let (_world, terrain) = seed_42_world_and_terrain();
        assert!(
            terrain.crust_age_at(SHALLOW_YOUNG_VERTEX) <= ANCIENT_CRUST_AGE_FOR_TEST,
            "the fixture vertex must fall short of the ancient threshold"
        );
        assert_eq!(prehuman_vestige(&terrain, SHALLOW_YOUNG_VERTEX), None);
    }

    #[test]
    fn vestiges_at_orders_prehuman_first_then_people_and_is_deterministic() {
        let (world_a, terrain_a) = world_and_terrain(PREHUMAN_SEED);
        let (world_b, terrain_b) = world_and_terrain(PREHUMAN_SEED);

        let vertex = deep_ancient_numinous_vertex(&terrain_a);
        let stack_a = vestiges_at(&world_a, &terrain_a, vertex);
        let stack_b = vestiges_at(&world_b, &terrain_b, vertex);
        assert_eq!(stack_a, stack_b, "vestiges_at must be deterministic");

        assert!(
            !stack_a.is_empty(),
            "the fixture vertex has at least the pre-human layer"
        );
        assert_eq!(
            stack_a[0].kind,
            VestigeKind::GateScar,
            "the pre-human layer comes first (oldest)"
        );
        assert_eq!(stack_a[0].founded_day, None);
        for layer in &stack_a[1..] {
            assert!(
                layer.founded_day.is_some(),
                "every layer after the pre-human one is a people occupation"
            );
        }

        // Founded-day ordering among the people layers is oldest-first,
        // inherited from `occupations_at`.
        let founded: Vec<f64> = stack_a[1..].iter().filter_map(|v| v.founded_day).collect();
        assert!(
            founded.windows(2).all(|w| w[0] <= w[1]),
            "people layers must stay oldest-founded-first"
        );
    }

    #[test]
    fn vestiges_at_with_no_prehuman_layer_starts_with_people_only() {
        let (world, terrain) = seed_42_world_and_terrain();
        let stack = vestiges_at(&world, &terrain, SHALLOW_YOUNG_VERTEX);
        assert!(
            stack.iter().all(|v| v.founded_day.is_some()),
            "with no pre-human layer, every entry is a people occupation"
        );
    }

    #[test]
    fn vestiges_field_matches_vestiges_at_per_vertex() {
        // The batched, one-scan field must be byte-identical, vertex by vertex,
        // to the per-vertex path it replaces in the hot loops (`vestige_dread`,
        // the residue lens) — same ordering (pre-human first, then
        // oldest-founded-first occupations), same values.
        // On PREHUMAN_SEED, not seed 42: the batched/per-vertex agreement has to
        // be checked on a vertex that actually carries a pre-human layer, and
        // seed 42 no longer has one (see `PREHUMAN_SEED`). The other two arms
        // below are seed-agnostic.
        let (world, terrain) = world_and_terrain(PREHUMAN_SEED);
        let field = vestiges_field(&world, &terrain);

        // The pre-human fixture vertex (a breached/forgotten gate-scar).
        let deep = deep_ancient_numinous_vertex(&terrain);
        assert_eq!(
            field.get(deep),
            &vestiges_at(&world, &terrain, deep),
            "the pre-human fixture vertex must match"
        );
        // A vertex with no pre-human layer at all, found by scan rather than
        // pinned — `SHALLOW_YOUNG_VERTEX` is a seed-42 fixture and this arm is
        // no longer running on seed 42.
        let shallow = terrain
            .geosphere()
            .vertices()
            .find(|&c| prehuman_vestige(&terrain, c).is_none())
            .expect("some vertex carries no pre-human layer");
        assert_eq!(
            field.get(shallow),
            &vestiges_at(&world, &terrain, shallow),
            "a vertex with no pre-human layer must match"
        );
        // A sample of vertices that actually carry people occupations, so the
        // grouped-by-site path is checked against populated stacks too, not
        // just the two fixture vertices above.
        let occupied_vertices: std::collections::BTreeSet<Vertex> = occupation_records(&world)
            .iter()
            .map(|o| o.core.site)
            .collect();
        for vertex in occupied_vertices.into_iter().take(25) {
            assert_eq!(
                field.get(vertex),
                &vestiges_at(&world, &terrain, vertex),
                "occupied vertex {vertex:?} must match between the batched and per-vertex paths"
            );
        }
    }
}
