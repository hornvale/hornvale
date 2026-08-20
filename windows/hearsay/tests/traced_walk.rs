//! Task 2's guard: [`hornvale_hearsay::traced::traced_variants_about_accumulating`]
//! is a SIBLING of the shipped
//! [`hornvale_hearsay::derive::variants_about_accumulating`], not a
//! replacement, and this file is what makes that a checked claim rather than
//! an assertion in a doc comment.
//!
//! Two batteries:
//!
//! - [`traced_agrees_with_the_shipped_walk_holder_for_holder`] — the
//!   load-bearing one. On real, live-worldgen seeds, for every
//!   `Accumulation` rule and both `Contact` and `Crossing` arms, the traced
//!   walk's claims (`t.claim`, dropping the route it adds) must equal the
//!   shipped walk's claims exactly. It is HEAVY (deferred from the commit
//!   gate, decision 0132) because it builds real worlds through
//!   `hornvale_worldgen`; run it with `cargo test -p hornvale-hearsay
//!   --test traced_walk -- --ignored`.
//! - [`the_route_field_is_load_bearing`] — FAST, on a hand-built ledger where
//!   a holder is reachable from two witnesses of different peoples by two
//!   genuinely different routes, so `witness` and `crossings` are not free to
//!   be constant. The task report records the mutation that proved it.

mod common;

use common::{eid, ledger_with, put};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::contact::{Contact, ContactGraph, contact_of};
use hornvale_hearsay::derive::variants_about_accumulating;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_hearsay::traced::{Carrier, traced_variants_about_accumulating};
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};
use hornvale_kernel::Claim;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};

/// The predicate every claim in this file is about — the only predicate a
/// transmission graph can be built over (only an ending has parties beyond
/// its subject, campaign 2 spec §6.1).
const PREDICATE: &str = hornvale_history::OCC_ENDED;

// ===========================================================================
// The agreement battery (heavy, live worldgen).
// ===========================================================================

/// A small panel — this is a byte-identity check, not a statistical
/// measurement, so four seeds is deliberately not forty.
const PANEL: [u64; 4] = [42, 7, 13, 1];

/// Everything one world contributes that both walks need.
struct WorldRead {
    /// The founding tree.
    lineage: Lineage,
    /// The raid seam.
    contact: ContactGraph,
    /// Per-people generation and lifespan, in std days.
    durations: PeopleDurations,
    /// One ladder per people.
    ladders: PeopleLadders,
}

/// Assemble one world's ladders and durations — a near-copy of
/// `undertow_readout.rs::read_world`, deliberately: an agreement battery that
/// assembled the ladder differently from the walk it is checking would be
/// checking a different model.
///
/// `None` when the world offers no year rung to convert allometric years into
/// standard days, the same skip condition `undertow_readout.rs` uses.
fn read_world(led: &Ledger, components: &hornvale_worldgen::WorldComponents) -> Option<WorldRead> {
    let lineage = lineage_of(led);
    let contact = contact_of(led);
    let astronomical = hornvale_hearsay::ladder::PrecisionLadder::of(led);
    let year_days = astronomical
        .labels()
        .iter()
        .position(|label| *label == "year")
        .and_then(|i| astronomical.span(hornvale_kernel::Precision(i as u8)))
        .map(|span| span.get())?;

    let mut people_of: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for occ in lineage.all() {
        if let Some(Value::Text(people)) = led.value_of(occ, hornvale_history::OCC_PEOPLE) {
            people_of.insert(people.clone());
        }
    }

    let mut durations = PeopleDurations::default();
    for people in &people_of {
        let Some(bio) = components.biosphere.get_by_label(people) else {
            continue;
        };
        let life = hornvale_species::life_history(bio.mass, bio.metabolic_class, bio.schedule);
        let to_days = |years: hornvale_kernel::Years| StdDays::new(years.get() * year_days).ok();
        durations.insert(
            people,
            life.generation_length.and_then(to_days),
            life.lifespan.and_then(to_days),
        );
    }

    let ladders = PeopleLadders::of(led, &durations);

    Some(WorldRead {
        lineage,
        contact,
        durations,
        ladders,
    })
}

/// Endings whose named attacker is of another people — spec §3.1's
/// foreign-attacker population, the one where `Contact` and `Crossing`
/// actually have something to disagree about.
fn foreign_endings(led: &Ledger) -> Vec<EntityId> {
    let mut out = Vec::new();
    for fact in led.find(PREDICATE) {
        let subject = fact.subject;
        if led.value_of(subject, PREDICATE).is_none() {
            continue;
        }
        let people = match led.value_of(subject, hornvale_history::OCC_PEOPLE) {
            Some(Value::Text(p)) => p.clone(),
            _ => String::new(),
        };
        let attacker_people = match led.value_of(subject, hornvale_history::OCC_ENDED_BY) {
            Some(Value::Entity(a)) => match led.value_of(*a, hornvale_history::OCC_PEOPLE) {
                Some(Value::Text(p)) => Some(p.clone()),
                _ => None,
            },
            _ => None,
        };
        if let Some(ap) = attacker_people
            && !ap.is_empty()
            && !people.is_empty()
            && ap != people
        {
            out.push(subject);
        }
    }
    out.sort();
    out.dedup();
    out
}

/// claim: readout(off-gate, heavy:) — the traced walk's claims must equal
/// the shipped walk's claims, holder-for-holder, on every rule and both arms
/// of `Contact` and `Crossing`, over a live-worldgen panel. A red here means
/// `traced.rs` has drifted from `derive.rs` and nothing downstream of it can
/// be trusted.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn traced_agrees_with_the_shipped_walk_holder_for_holder() {
    let components = hornvale_worldgen::WorldComponents::assemble().expect("components assemble");
    let mut checked = 0usize;
    let mut skipped: Vec<u64> = Vec::new();

    for seed in PANEL {
        let world = hornvale_worldgen::build_world(
            hornvale_kernel::Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .expect("panel seed builds");
        let led = &world.ledger;
        let Some(read) = read_world(led, &components) else {
            skipped.push(seed);
            continue;
        };
        let subjects = foreign_endings(led);

        for subject in &subjects {
            for rule in Accumulation::ALL {
                for contact in Contact::ALL {
                    for crossing in Crossing::ALL {
                        let policy = Transmission {
                            contact,
                            crossing,
                            ..Transmission::AS_SHIPPED
                        };
                        let walk = Walk {
                            ledger: led,
                            lineage: &read.lineage,
                            contact: &read.contact,
                            policy,
                        };
                        let shipped: Vec<Claim> = variants_about_accumulating(
                            &walk,
                            &read.ladders,
                            &read.durations,
                            rule,
                            *subject,
                            PREDICATE,
                        );
                        let traced: Vec<Claim> = traced_variants_about_accumulating(
                            &walk,
                            &read.ladders,
                            &read.durations,
                            rule,
                            *subject,
                            PREDICATE,
                        )
                        .into_iter()
                        .map(|t| t.claim)
                        .collect();
                        assert_eq!(
                            traced, shipped,
                            "seed {seed} subject {subject:?} rule {rule:?} \
                             contact {contact:?} crossing {crossing:?}"
                        );
                        checked += 1;
                    }
                }
            }
        }
    }

    assert!(
        checked > 0,
        "the panel must exercise at least one foreign ending, or this test \
         is a vacuous pass; skipped seeds: {skipped:?}"
    );
    println!(
        "traced_walk agreement battery: {checked} (subject, rule, contact, crossing) cells \
         checked, holder-for-holder byte-identical; skipped seeds {skipped:?}"
    );
}

// ===========================================================================
// The route-discrimination battery (fast, hand-built).
// ===========================================================================

/// One generation length for both peoples here, so the two routes differ
/// only by which raid edge and which founding gaps they cross, never by an
/// asymmetric duration table.
fn durations() -> PeopleDurations {
    let mut d = PeopleDurations::default();
    for people in ["human", "kobold"] {
        d.insert(
            people,
            Some(StdDays::new(50.0).expect("positive")),
            Some(StdDays::new(150.0).expect("positive")),
        );
    }
    d
}

/// A ledger where holder `50` is reachable by two genuinely different
/// routes, from two witnesses of two different peoples:
///
/// - `1` (human), founded day 0, `occ-ended` day 500 with `occ-ended-by`
///   `5` (kobold) — the event under test. `witnesses_of` therefore seeds
///   BOTH `1` and `5` at hop 0, of different peoples (spec §6.1: a named
///   `Entity`-valued attacker is a witness).
/// - `2` (human), child of `1`, founded day 200 — the human line's one
///   descent step.
/// - `2` itself later ends (day 700, `occ-ended-by` `50`), which is what
///   plants a raid edge `2 <-> 50` in the seam.
/// - `5` (kobold) similarly ends (day 800, `occ-ended-by` `50`), planting a
///   second raid edge `5 <-> 50`.
/// - `50` has no `occ-people` of its own, so a step onto it is a cross-people
///   step from EITHER side, and it names a founding day (900) so
///   `gen_span` is non-zero on every step that reaches it.
///
/// Under `Contact::WithRaidSeam`, `50` is reachable two ways: the HUMAN
/// route `1 -> 2 -> 50` (2 hops: one descent step, one seam step) and the
/// KOBOLD route `5 -> 50` (1 hop: one seam step, directly from the witness).
/// Under `Accumulation::Additive` and EITHER `Crossing` arm the kobold route
/// is strictly cheaper — `tests/traced_walk.rs`'s own arithmetic (not
/// re-derived here in prose) puts it two days under the human route
/// regardless of the crossing penalty, because the penalty term is identical
/// on both routes' one people-crossing step and the human route pays an
/// extra same-people step on top. So the relaxation keeps the kobold
/// telling: `witness == 5`, `hops == 1`, one `Crossed` step carried by
/// `Carrier::Seam`.
///
/// **This is not a fixture where both routes share a witness** — the
/// Undertow's Task-1 null. `1` and `5` are different entities, of different
/// peoples, each independently reaching `50`.
fn two_witnesses_converging_on_one_holder() -> Ledger {
    let mut led = ledger_with(&[(1, None), (2, Some(1)), (5, None)]);
    for (occ, day) in [(1, 0.0), (2, 200.0), (5, 100.0), (50, 900.0)] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for (occ, people) in [(1, "human"), (2, "human"), (5, "kobold")] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    // The event under test: 1 ends, raided by the kobold 5, which seeds both
    // as hop-0 witnesses of different peoples.
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(500.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );
    // The human line's raid edge onto 50, after the event.
    put(
        &mut led,
        2,
        hornvale_history::OCC_ENDED,
        Value::Number(700.0),
    );
    put(
        &mut led,
        2,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(50)),
    );
    // The kobold witness's own raid edge onto 50, after the event.
    put(
        &mut led,
        5,
        hornvale_history::OCC_ENDED,
        Value::Number(800.0),
    );
    put(
        &mut led,
        5,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(50)),
    );
    common::put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    common::put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    common::put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}

/// Holder `50`'s [`hornvale_hearsay::traced::HeldTelling`] under
/// `Accumulation::Additive` and the given `Crossing` arm.
fn held_50(crossing: Crossing) -> hornvale_hearsay::traced::HeldTelling {
    let led = two_witnesses_converging_on_one_holder();
    let lineage = lineage_of(&led);
    let contact = contact_of(&led);
    let durations = durations();
    let ladders = PeopleLadders::of(&led, &durations);
    let policy = Transmission {
        contact: Contact::WithRaidSeam,
        crossing,
        ..Transmission::AS_SHIPPED
    };
    let walk = Walk {
        ledger: &led,
        lineage: &lineage,
        contact: &contact,
        policy,
    };
    traced_variants_about_accumulating(
        &walk,
        &ladders,
        &durations,
        Accumulation::Additive,
        eid(1),
        PREDICATE,
    )
    .into_iter()
    .find(|t| t.claim.holder == eid(50))
    .expect("fixture must reach holder 50 under Contact::WithRaidSeam")
}

/// The property Step 4 asks for: `witness` and `crossings` distinguish which
/// of the two routes won, on both `Crossing` arms.
///
/// Neutralising `traced.rs` to hard-code `witness` to a constant, or to drop
/// `crossings` (always `Vec::new()`), reddens this test — recorded in the
/// task report rather than committed as a live mutation.
#[test]
fn the_route_field_is_load_bearing() {
    for crossing in Crossing::ALL {
        let held = held_50(crossing);
        assert_eq!(
            held.witness,
            eid(5),
            "the kobold route (1 hop) must win under {crossing:?} — got witness {:?}",
            held.witness
        );
        assert_eq!(
            held.claim.hops, 1,
            "the winning route must be the 1-hop kobold one under {crossing:?}"
        );
        assert_eq!(
            held.crossings.len(),
            1,
            "the winning route crosses exactly one people boundary under {crossing:?}: {:?}",
            held.crossings
        );
        assert_eq!(
            held.crossings[0].carrier,
            Carrier::Seam,
            "the kobold witness's own raid edge is the carrier under {crossing:?}"
        );
        assert_eq!(
            held.crossings[0].edges, 1,
            "exactly one raid edge lies between the kobold and the unlabelled \
             holder's people under {crossing:?}"
        );
    }

    // Negative control: the LOSING route's own shape, read off the human
    // witness's directly-reached holder (2), so this file is not merely
    // trusting one number. Holder 2 is reached from witness 1 alone (no
    // seam involved: the human descent step), so its witness must be the
    // human, not the kobold, and it carries no crossing at all.
    let led = two_witnesses_converging_on_one_holder();
    let lineage = lineage_of(&led);
    let contact = contact_of(&led);
    let durations = durations();
    let ladders = PeopleLadders::of(&led, &durations);
    let policy = Transmission {
        contact: Contact::WithRaidSeam,
        crossing: Crossing::Free,
        ..Transmission::AS_SHIPPED
    };
    let walk = Walk {
        ledger: &led,
        lineage: &lineage,
        contact: &contact,
        policy,
    };
    let held_2 = traced_variants_about_accumulating(
        &walk,
        &ladders,
        &durations,
        Accumulation::Additive,
        eid(1),
        PREDICATE,
    )
    .into_iter()
    .find(|t| t.claim.holder == eid(2))
    .expect("fixture must reach holder 2");
    assert_eq!(
        held_2.witness,
        eid(1),
        "holder 2 is reached only from witness 1"
    );
    assert!(
        held_2.crossings.is_empty(),
        "a same-people descent step must carry no crossing: {:?}",
        held_2.crossings
    );
}
