//! Every kind's full life-history profile, frozen.
//!
//! THE GOSSAN's instrument 1 (spec §5.2). This exists because the project's
//! own drift check is VACUOUS for a change to `MetabolicClass`: committed
//! artifacts carry life-history numbers for `goblin` and `kobold` only, both
//! under `book/src/laboratory/generated/`, and those regenerate only under
//! the census flag — so `make rebaseline` never touches them and a local
//! `git diff --exit-code` comes back clean whether the mapping is right or
//! wrong (spec §5.1).
//!
//! # WHAT THIS CANNOT SEE
//!
//! `rise_at` (`windows/vessel/src/liveness.rs`) produces no species-level
//! life-history quantity, so a thermal mis-mapping that only changes thirst
//! is INVISIBLE here. That is instrument 2's job
//! (`rise_at_couples_heat_to_thirst_per_metabolic_class`), and control C1 in
//! spec §5.3 exists to prove this blind spot is real rather than assumed.
//!
//! Regenerate deliberately: `REBASELINE=1 cargo test -p hornvale-species
//! --test suite -- life_history_golden`, then read the diff as a change to
//! every creature in every world.

use hornvale_kernel::quantize::quantize;
use hornvale_species::{biosphere_registry, life_history};

/// One line per kind, tab-separated, quantized at the emit boundary exactly
/// as every other committed float in this project is (decision 0033) — a
/// golden file IS an emit boundary, and an unquantized one would differ in
/// the last ULP between platforms.
fn render() -> String {
    let mut out = String::from("kind\tbmr_w\tlifespan_y\tmaturity_y\ttempo\tgeneration_y\tpace\n");
    for (kind, bio) in biosphere_registry().iter() {
        let lh = life_history(bio.mass, bio.thermal_strategy, bio.schedule);
        let opt = |v: Option<f64>| match v {
            Some(x) => format!("{}", quantize(x)),
            None => "-".to_string(),
        };
        out.push_str(&format!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
            kind.0,
            quantize(lh.basal_metabolic_rate_w),
            opt(lh.lifespan.map(|y| y.get())),
            opt(lh.age_at_maturity.map(|y| y.get())),
            opt(lh.reproductive_tempo),
            opt(lh.generation_length.map(|y| y.get())),
            quantize(lh.pace_of_life),
        ));
    }
    out
}

#[test]
fn every_kinds_life_history_is_frozen() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/life-history-all-kinds.txt"
        )),
        &render(),
        "a kind's life-history profile moved. THE GOSSAN claims to change no \
         number in any world, so during that campaign this drifting is a STOP, \
         not a rebaseline: re-read the bijection in spec §4.2 before touching \
         this fixture. Outside that campaign, a move here is a calibration \
         migration — accept it with REBASELINE=1 and review the diff.",
    );
}

/// The fixture is only a guard if it has rows and if the `Option` columns are
/// not all one value. A registry that yielded nothing, or a `life_history`
/// that returned `None` everywhere, would freeze a table of dashes and the
/// golden would be silently vacuous.
#[test]
fn the_life_history_table_is_not_vacuous() {
    let rendered = render();
    let rows: Vec<&str> = rendered.lines().skip(1).collect();
    assert!(
        rows.len() >= 20,
        "only {} kinds rendered — the registry is not being read",
        rows.len()
    );
    assert!(
        rows.iter().any(|r| r.contains("\t-\t")),
        "no row has an absent life-history column — the Ametabolic branch of \
         `life_history` is not represented, so this fixture cannot witness it"
    );
    assert!(
        rows.iter().any(|r| !r.contains("\t-\t")),
        "every row has an absent column — `life_history` is returning None for \
         everything and the table is a page of dashes"
    );
}
