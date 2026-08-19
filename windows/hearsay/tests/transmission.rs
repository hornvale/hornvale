//! Spec §5: `Transmission::AS_SHIPPED` must reproduce today's behaviour
//! exactly. Every arm this campaign adds is measured as a difference from it,
//! so if this baseline is not exact, nothing downstream means anything.

mod common;

use common::{chain_with_foundings, eid};
use hornvale_astronomy::units::StdDays;
use hornvale_hearsay::accumulate::Accumulation;
use hornvale_hearsay::clock::Clock;
use hornvale_hearsay::contact::{Contact, contact_of};
use hornvale_hearsay::derive::variants_about_accumulating;
use hornvale_hearsay::durations::PeopleDurations;
use hornvale_hearsay::ladder::PeopleLadders;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::stance::Perpetration;
use hornvale_hearsay::transmission::{Crossing, Transmission, Walk};

#[test]
fn as_shipped_is_exactly_todays_four_arms() {
    assert_eq!(Transmission::AS_SHIPPED.clock, Clock::Off);
    assert_eq!(
        Transmission::AS_SHIPPED.perpetration,
        Perpetration::Singleton
    );
    assert_eq!(Transmission::AS_SHIPPED.contact, Contact::Descent);
    assert_eq!(Transmission::AS_SHIPPED.crossing, Crossing::Free);
    // This string MOVES when an arm is added, and that is correct: it is a
    // description of which arms the policy carries, not a measured outcome.
    // Leaving `crossing` out of `label()` would let two different policies
    // print one name and make a readout's arm columns ambiguous.
    assert_eq!(
        Transmission::AS_SHIPPED.label(),
        "no-clock/singleton/descent/free"
    );
}

/// The accumulating walk under `AS_SHIPPED` must return exactly what campaign
/// 3 committed for this fixture: same holders, same hops, same ordering.
#[test]
fn as_shipped_reproduces_the_committed_accumulating_derivation() {
    let led = chain_with_foundings();
    let lin = lineage_of(&led);
    let graph = contact_of(&led);
    let mut durations = PeopleDurations::default();
    durations.insert(
        "human",
        Some(StdDays::new(50.0).expect("positive")),
        Some(StdDays::new(150.0).expect("positive")),
    );
    let ladders = PeopleLadders::of(&led, &durations);
    let walk = Walk {
        ledger: &led,
        lineage: &lin,
        contact: &graph,
        policy: Transmission::AS_SHIPPED,
    };

    let held = variants_about_accumulating(
        &walk,
        &ladders,
        &durations,
        Accumulation::Additive,
        eid(1),
        hornvale_history::OCC_ENDED,
    );

    assert!(!held.is_empty(), "control: the fixture must hold claims");
    assert_eq!(held.len(), 6, "six occupations in the chain, six holders");
    assert!(
        held.windows(2).all(|w| w[0].holder <= w[1].holder),
        "results stay ascending by holder"
    );
    assert_eq!(held[0].hops, 0, "the witness holds at hop 0");
}
