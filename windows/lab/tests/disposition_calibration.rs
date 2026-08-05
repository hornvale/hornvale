//! Preregistered calibration on the ONE per-people axis the deep-history bake
//! actually differentiates: authored `threat_response`.
//!
//! ## Why this file exists — the hypothesis it replaces, and its falsification
//!
//! The Tumult's predation epoch inherited a preregistered directional
//! hypothesis, `kobold_flagships_are_less_coastal_than_goblin_flagships`
//! (`tests/calibration.rs`, spec §9.1). It claimed that the kobold — the cool
//! highlander, staked to an elevation niche far from the fertile coast —
//! should carry its flagship on a *less coastal* cell than the lowland-
//! tolerant goblin does. **This epoch falsified it**, and the investigation
//! that followed (`.superpowers/sdd/coastal-inversion-investigation.md`)
//! found the hypothesis was never testable in the first place:
//!
//! - **The bake is niche-blind, end to end.** `ConditionNiche` never enters
//!   the deep-history bake in any form. Genesis draws each people's
//!   proto-sites uniformly from one species-blind, river-weighted capacity
//!   ranking; the covet rule, the roll-downhill score, the pressure term and
//!   the no-spoils gate all read a single global `eff_capacity` field with no
//!   people argument. Since *The Living Community*'s history-first placement,
//!   **no settling people has ever been placed by its own niche.**
//! - So **the shipped model predicts a kobold−goblin flagship-coastal
//!   difference of exactly zero.** Both peoples draw from the same pool. The
//!   old test passed pre-campaign by ≈ 0.5 σ of draw noise and failed after it
//!   by ≈ 2.2 σ of the same noise, on 769 paired census worlds — two draws
//!   around a zero, not a reversal of anything.
//! - The census movement was fully accounted for as a **flagship-identity
//!   re-selection artifact**, not a relocation: `flagship_of` resolves the
//!   *oldest surviving* occupation, and a raid closes the raider's own record
//!   (`Migrated`) and reopens it at the back of the list. 100 % of the coastal
//!   movement lived in the worlds where the flagship cell *changed*; where it
//!   did not change the rate was byte-identical before and after.
//!
//! Nobody should re-derive that hypothesis. The direction was never the
//! finding; the **re-selection rate itself** was — and that is what this file
//! preregisters instead, on the axis that produces it.
//!
//! ## The hypothesis this file does assert
//!
//! `threat_response` is the *only* per-people input the composition root
//! hands the bake (`bake_history_from` fills `BakeConfig::disposition` from
//! the psyche registry and nothing else). A people below
//! [`RAID_DISPOSITION_MIN`] never takes the initiative, so it almost never
//! vacates an early site; a people above it raids, and every raid it wins
//! closes its own record and re-seats the flagship on a later one.
//!
//! ## **The Tolerance dissolved this file's partition (2026-08-04)**
//!
//! Read the paragraph above as a statement about the roster *before* The
//! Tolerance. `Bake::takes_the_initiative` no longer compares a people's
//! authored `threat_response`; it compares a value **drawn per settlement**
//! around that authored mean, with the people's `Dispersion::mind` as the
//! standard deviation. So the two-way split this file computes from the psyche
//! registry — "raiders" above 0.6, "abstainers" below — is now a split by
//! authored *mean*, not by whether a people's settlements raid. Every one of
//! the six settling peoples has settlements on both sides of the gate; goblin
//! (mean 0.5, σ 0.25) clears it on roughly 38 % of its draws and human (0.5,
//! 0.35) on roughly 42 %.
//!
//! The directional claim may well survive — a people whose mean is 0.5 still
//! raids far less often than one at 0.85, so its flagship should still turn
//! over less — but the *mechanism sentence* is now approximate where it used to
//! be exact, and the measured rates below (goblin 16.7 % vs 42.6/45.8/50.0 %,
//! taken 2026-07-26 on the pre-Tolerance bake) predate the change and are the
//! numbers the bounds were set from.
//!
//! **The thresholds were deliberately NOT retuned here.** Moving a
//! preregistered bound to rescue a prediction after the physics under it
//! changed is exactly what this repo forbids; if this battery reddens on the
//! next heavy-tier run, that is a finding for The Tolerance's readout to
//! report, not a number to adjust. What is corrected here is only the prose
//! that claimed a people never raids.
//!
//! > **Preregistered:** a NON-RAIDING people holds its first-drawn genesis
//! > site as its flagship far more often than a RAIDING people does — the
//! > flagship-re-selection rate is at most [`NONRAIDER_MAX`] for a people
//! > below the raid threshold, at least [`RAIDER_MIN`] for every people above
//! > it, and separated by at least [`SEPARATION_FACTOR`]×.
//!
//! Measured on the shipped bake over seeds 1..=60 (2026-07-26): the one
//! non-raiding people re-seats its flagship on **16.7 %** of worlds, the three
//! raiding peoples on **42.6 % / 45.8 % / 50.0 %** — see the constants below
//! for the headroom each threshold leaves.
//!
//! ## Anti-vacuity
//!
//! The assertion binds on the mechanism, not on the labels: the raiding and
//! non-raiding sets are derived from the shipped psyche registry, and the
//! test fails loudly if either side is empty. Mutation-verified in both
//! directions against `Bake::takes_the_initiative` — forcing it to `true`
//! (everybody raids) reddens the non-raider bound, forcing it to `false`
//! (nobody raids) reddens the raider bound.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, WorldComponents, history_for};
use std::collections::BTreeMap;

/// Seeds `1..=SAMPLE`, the range every reading below is pooled over.
const SAMPLE: u64 = 60;

/// The raid-initiative threshold on authored `threat_response`, mirroring
/// `hornvale_worldgen::history_bake`'s own private `RAID_DISPOSITION_MIN`.
/// Deliberately re-stated here rather than imported: this calibration
/// partitions the roster from the psyche registry independently of the bake,
/// so a silent change to either the constant or the authored dispositions
/// shows up as a failure here instead of passing unnoticed.
const RAID_DISPOSITION_MIN: f64 = 0.6;

/// Ceiling on a NON-raiding people's flagship-re-selection rate. Measured
/// 2026-07-26 over seeds 1..=60: goblin 8/48 = **0.167**. A non-raider still
/// loses its first site sometimes — famine and climate eviction close records
/// too — so this floor is not zero; the bound sits 50 % above the
/// measurement.
const NONRAIDER_MAX: f64 = 0.25;

/// Floor on every RAIDING people's flagship-re-selection rate. Measured
/// 2026-07-26 over seeds 1..=60: hobgoblin 20/47 = **0.426**, kobold
/// 22/48 = 0.458, bugbear 24/48 = 0.500. The bound sits 42 % below the
/// weakest raider.
const RAIDER_MIN: f64 = 0.30;

/// The directional claim proper: the weakest raider's rate over the strongest
/// non-raider's. Measured ratio **2.55** (0.426 / 0.167), so the bound leaves
/// 27 % headroom.
const SEPARATION_FACTOR: f64 = 2.0;

/// Per-people flagship-re-selection rates over seeds `1..=SAMPLE`: the
/// fraction of worlds in which a people's flagship — the oldest occupation
/// still alive at `now`, which is exactly what `flagship_of` resolves — sits
/// on a DIFFERENT cell from that people's first-drawn genesis proto-site.
///
/// Read straight off the bake's own `History.records`, which are in commit
/// order: genesis opens every people's proto-sites first, so the first record
/// carrying a people is its rank-0 draw, and the first *alive* record carrying
/// it is its flagship.
fn reselection_rates(wc: &WorldComponents) -> BTreeMap<KindId, (u32, u32)> {
    let mut tally: BTreeMap<KindId, (u32, u32)> = BTreeMap::new();
    for seed in 1..=SAMPLE {
        let history = history_for(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            wc,
        )
        .expect("the default pins build at Terrain depth on every sampled seed");
        let peoples: Vec<KindId> = {
            let mut seen: Vec<KindId> = history.records.iter().map(|r| r.core.people).collect();
            seen.sort_by(|a, b| a.0.cmp(b.0));
            seen.dedup();
            seen
        };
        for people in peoples {
            let Some(genesis) = history.records.iter().find(|r| r.core.people == people) else {
                continue;
            };
            let Some(flagship) = history
                .records
                .iter()
                .find(|r| r.core.people == people && r.core.ended.is_none())
            else {
                // A people wholly extinguished by `now` has no flagship to
                // re-seat; it is not a world this rate is defined on.
                continue;
            };
            let entry = tally.entry(people).or_insert((0, 0));
            entry.1 += 1;
            if flagship.core.site != genesis.core.site {
                entry.0 += 1;
            }
        }
    }
    tally
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn non_raiding_peoples_hold_their_genesis_flagship_far_longer_than_raiders() {
    let wc = WorldComponents::assemble().expect("assemble the shipped component set");
    // The partition, derived from the shipped psyche registry — never authored
    // in this file. `bake_history_from` fills `BakeConfig::disposition` from
    // exactly this map.
    let (mut raiders, mut abstainers): (Vec<KindId>, Vec<KindId>) = (Vec::new(), Vec::new());
    for (kind, psyche) in wc.psyche.iter() {
        if !wc
            .biosphere
            .get(kind)
            .is_some_and(|b| b.social_form == hornvale_species::SocialForm::Settled)
        {
            continue;
        }
        if psyche.threat_response >= RAID_DISPOSITION_MIN {
            raiders.push(*kind);
        } else {
            abstainers.push(*kind);
        }
    }
    assert!(
        !raiders.is_empty() && !abstainers.is_empty(),
        "vacuous: the roster must contain at least one settling people on each \
         side of RAID_DISPOSITION_MIN ({RAID_DISPOSITION_MIN}) for this \
         hypothesis to mean anything — raiders {raiders:?}, abstainers {abstainers:?}"
    );

    let tally = reselection_rates(&wc);
    for (k, (changed, worlds)) in &tally {
        println!(
            "{k:?}: re-seated {changed}/{worlds} = {:.3}",
            f64::from(*changed) / f64::from(*worlds)
        );
    }
    let rate = |k: &KindId| -> f64 {
        let (changed, worlds) = tally.get(k).copied().unwrap_or((0, 0));
        assert!(
            worlds * 2 >= SAMPLE as u32,
            "{k:?} was flagship-less on more than half of {SAMPLE} worlds \
             ({worlds} usable) — the rate is not measurable"
        );
        f64::from(changed) / f64::from(worlds)
    };

    let mut worst_abstainer = 0.0f64;
    for k in &abstainers {
        let r = rate(k);
        assert!(
            r <= NONRAIDER_MAX,
            "{k:?} does not raid (threat_response < {RAID_DISPOSITION_MIN}) yet \
             re-seated its flagship on {r:.3} of worlds, above the \
             {NONRAIDER_MAX} bound"
        );
        worst_abstainer = worst_abstainer.max(r);
    }
    let mut weakest_raider = f64::INFINITY;
    for k in &raiders {
        let r = rate(k);
        assert!(
            r >= RAIDER_MIN,
            "{k:?} raids (threat_response >= {RAID_DISPOSITION_MIN}) yet re-seated \
             its flagship on only {r:.3} of worlds, below the {RAIDER_MIN} bound"
        );
        weakest_raider = weakest_raider.min(r);
    }
    assert!(
        weakest_raider >= SEPARATION_FACTOR * worst_abstainer,
        "the directional claim failed: weakest raider {weakest_raider:.3} is not \
         {SEPARATION_FACTOR}x the strongest abstainer {worst_abstainer:.3}"
    );
}
