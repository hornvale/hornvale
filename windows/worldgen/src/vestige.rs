//! Subsurface historical residue — sealed wards, abandoned delvings, buried
//! ruins, gate-scars (The Vestige). A derived reading of the narrated past
//! (settlement-abandonment history + terrain): no live mutation, no committed
//! facts, no metaphysics — the door and its dread, not the entity.

use hornvale_history::record::{CauseOfEnd, Function, Notability, OccupationRecord};
use hornvale_kernel::math;

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
    /// A lingering supernatural charge (pre-human sites).
    Numinous,
    /// Held to be cursed by those who remember it.
    Cursed,
}

/// A located residue feature (one layer of a cell's palimpsest).
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

/// A ruin older than this (days since it ended) has an all-but-lost warning.
const WARNING_HALF_LIFE_DAYS: f64 = 300.0;

/// Derive a people-made vestige from one occupation, as of `now` (days).
/// Read backward: no simulation.
/// type-audit: bare-ok(diagnostic-value: now)
pub fn vestige_from_occupation(occ: &OccupationRecord, now: f64) -> Vestige {
    let kind = match (occ.function, occ.notability) {
        (Function::Mine, _) => VestigeKind::AbandonedDelving,
        (Function::Fort, _) | (Function::Cult, _) => VestigeKind::SealedVault,
        (_, Notability::Seat) => VestigeKind::BuriedRuin, // undercity scale (see size, later)
        _ => VestigeKind::BuriedRuin,
    };
    let (seal_state, valence) = match occ.ended {
        None => (SealState::Maintained, Valence::Venerated), // a living keeper
        Some(end) => {
            let age = (now - end).max(0.0);
            if age < WARNING_HALF_LIFE_DAYS {
                (SealState::Lapsing, Valence::Forgotten)
            } else {
                (SealState::Breached, Valence::Forgotten)
            }
        }
    };
    let hazard = match occ.cause {
        Some(CauseOfEnd::Plague) => HazardKind::Pestilent,
        Some(CauseOfEnd::Burned) => HazardKind::Cursed,
        _ => match occ.function {
            Function::Mine => HazardKind::Structural,
            _ => HazardKind::Structural,
        },
    };
    // Warning decays fast (the fastest of the three rates); recent = legible.
    let warning_legibility = match occ.ended {
        None => 1.0,
        Some(end) => math::exp(-((now - end).max(0.0)) / WARNING_HALF_LIFE_DAYS),
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
        founded_day: Some(occ.founded),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{
        CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
    };
    use hornvale_kernel::{CellId, EntityId, KindId};

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
            people: KindId("test-people"),
            community: eid(1),
            lineage: eid(1),
            site: CellId(0),
            founded: 0.0,
            ended,
            peak_population: 100,
            tech: TechHorizon::Iron,
            function,
            deity: None,
            tongue: None,
            cause,
            ended_by: Ended::Nature,
            founded_from: Founding::Genesis(CellId(0)),
            notability,
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
}
