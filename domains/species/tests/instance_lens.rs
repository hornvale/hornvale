//! The prototype-inheritance join: an instance's effective trait is its own
//! (latest) override fact, else its kind's authored registry default. The
//! keystone property is JOIN == SCAN: the lens over the index equals a naive
//! linear recomputation, over seeded random ledgers (generator coverage is
//! part of the review — the c4 signed-zero lesson).

use hornvale_kernel::{EntityId, KindId, Seed, Value, World};
use hornvale_species::{
    BiosphereTraits, SPECIES_MASS_KG, SPECIES_POTENCY, biosphere_registry, instance_biosphere,
};

fn world() -> World {
    let mut w = World::new(Seed(7));
    hornvale_species::register_concepts(&mut w.registry).unwrap();
    w
}

fn override_fact(e: EntityId, pred: &str, n: f64) -> hornvale_kernel::Fact {
    hornvale_kernel::Fact {
        subject: e,
        predicate: pred.to_string(),
        object: Value::Number(n),
        place: None,
        day: Some(1.0),
        provenance: "test".to_string(),
    }
}

#[test]
fn lens_returns_kind_defaults_when_no_overrides() {
    let mut w = world();
    let reg = biosphere_registry();
    let e = w
        .ledger
        .mint_instance("owlbear", None, "test", &w.registry)
        .unwrap();
    let eff = instance_biosphere(&w.ledger, e, &reg).unwrap();
    let authored = reg.get_by_label("owlbear").unwrap();
    assert_eq!(eff.mass.kilograms(), authored.mass.kilograms());
    assert_eq!(eff.potency, authored.potency);
}

#[test]
fn override_beats_default_and_latest_override_wins() {
    let mut w = world();
    let reg = biosphere_registry();
    let e = w
        .ledger
        .mint_instance("owlbear", None, "test", &w.registry)
        .unwrap();
    w.ledger
        .commit(override_fact(e, SPECIES_MASS_KG, 900.0), &w.registry)
        .unwrap();
    w.ledger
        .commit(override_fact(e, SPECIES_MASS_KG, 950.0), &w.registry)
        .unwrap();
    let eff = instance_biosphere(&w.ledger, e, &reg).unwrap();
    assert_eq!(eff.mass.kilograms(), 950.0, "latest override wins");
    // Untouched fields keep the authored default.
    assert_eq!(eff.potency, reg.get_by_label("owlbear").unwrap().potency);
}

#[test]
fn overrides_survive_kind_change() {
    // The spec §4.3 contract: overrides are per-instance, not
    // per-(instance, kind) — the unusually large owlbear stays large when
    // its kind changes.
    let mut w = world();
    let reg = biosphere_registry();
    let e = w
        .ledger
        .mint_instance("owlbear", None, "test", &w.registry)
        .unwrap();
    w.ledger
        .commit(override_fact(e, SPECIES_MASS_KG, 900.0), &w.registry)
        .unwrap();
    w.ledger
        .change_kind(e, "woolly-mammoth", Some(2.0), "test", &w.registry)
        .unwrap();
    let eff = instance_biosphere(&w.ledger, e, &reg).unwrap();
    assert_eq!(
        eff.mass.kilograms(),
        900.0,
        "override outlives the transition"
    );
    // Non-overridden fields come from the NEW kind.
    assert_eq!(
        eff.potency,
        reg.get_by_label("woolly-mammoth").unwrap().potency
    );
}

#[test]
fn lens_is_total_never_panicking() {
    let mut w = world();
    let reg = biosphere_registry();
    // No instance-of fact at all -> None.
    let bare = w.ledger.mint_entity();
    assert!(instance_biosphere(&w.ledger, bare, &reg).is_none());
    // Dangling label -> None.
    let dangling = w
        .ledger
        .mint_instance("no-such-kind", None, "test", &w.registry)
        .unwrap();
    assert!(instance_biosphere(&w.ledger, dangling, &reg).is_none());
    // Physically invalid override (negative mass) -> None, loudly absent
    // rather than silently defaulted.
    let bad = w
        .ledger
        .mint_instance("owlbear", None, "test", &w.registry)
        .unwrap();
    w.ledger
        .commit(override_fact(bad, SPECIES_MASS_KG, -5.0), &w.registry)
        .unwrap();
    assert!(instance_biosphere(&w.ledger, bad, &reg).is_none());
}

// Tiny deterministic PRNG — splitmix64, the kernel's own test idiom.
fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E3779B97F4A7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
    z ^ (z >> 31)
}

/// The naive oracle: recompute the effective traits by a LINEAR scan of the
/// whole ledger (no index, no kernel reads) + the registry — the definition
/// the lens must refine.
fn naive_effective(
    w: &World,
    e: EntityId,
    reg: &hornvale_kernel::ComponentStore<KindId, BiosphereTraits>,
) -> Option<BiosphereTraits> {
    let mut kind: Option<String> = None;
    let mut mass: Option<f64> = None;
    let mut potency: Option<f64> = None;
    for f in w.ledger.iter() {
        if f.subject != e {
            continue;
        }
        match (f.predicate.as_str(), &f.object) {
            ("instance-of", Value::Text(t)) => kind = Some(t.clone()),
            (p, Value::Number(n)) if p == SPECIES_MASS_KG => mass = Some(*n),
            (p, Value::Number(n)) if p == SPECIES_POTENCY => potency = Some(*n),
            _ => {}
        }
    }
    let mut t = reg.get_by_label(kind.as_deref()?)?.clone();
    if let Some(m) = mass {
        t.mass = hornvale_kernel::Mass::new(m).ok()?;
    }
    if let Some(p) = potency {
        if !p.is_finite() || p < 0.0 {
            return None;
        }
        t.potency = p;
    }
    Some(t)
}

#[test]
fn join_equals_scan_over_random_ledgers() {
    // Generator coverage (spec §5.2, the c4 lesson): kind changes 0/1/2+,
    // overrides before AND after changes, both zero spellings, dangling
    // labels, kindless entities.
    let reg = biosphere_registry();
    // "treant" carries a non-zero authored potency (0.30 = CR 9/30); the other valid
    // labels are all potency 0.0, so without it the property never exercises
    // potency-default resolution after a kind change (the c4 signed-zero
    // lesson: the generator's value-space must cover the field).
    let labels = ["owlbear", "woolly-mammoth", "treant", "no-such-kind"];
    for seed in 0..64u64 {
        let mut w = world();
        let mut st = seed.wrapping_add(1);
        let mut entities: Vec<EntityId> = vec![w.ledger.mint_entity()]; // kindless
        for _ in 0..12 {
            let label = labels[(splitmix(&mut st) as usize) % labels.len()];
            entities.push(
                w.ledger
                    .mint_instance(label, None, "gen", &w.registry)
                    .unwrap(),
            );
        }
        for _ in 0..120 {
            let e = entities[(splitmix(&mut st) as usize) % entities.len()];
            match splitmix(&mut st) % 4 {
                0 => {
                    let label = labels[(splitmix(&mut st) as usize) % labels.len()];
                    w.ledger
                        .change_kind(e, label, None, "gen", &w.registry)
                        .unwrap();
                }
                1 => {
                    let m = match splitmix(&mut st) % 4 {
                        0 => 0.0,
                        1 => -0.0,
                        2 => -3.0, // invalid: lens and scan must agree on None
                        _ => (splitmix(&mut st) % 5000) as f64,
                    };
                    let _ = w
                        .ledger
                        .commit(override_fact(e, SPECIES_MASS_KG, m), &w.registry);
                }
                _ => {
                    let p = match splitmix(&mut st) % 3 {
                        0 => 0.0,
                        1 => -0.0,
                        _ => (splitmix(&mut st) % 100) as f64 / 100.0,
                    };
                    let _ = w
                        .ledger
                        .commit(override_fact(e, SPECIES_POTENCY, p), &w.registry);
                }
            }
        }
        for &e in &entities {
            let join = instance_biosphere(&w.ledger, e, &reg);
            let scan = naive_effective(&w, e, &reg);
            assert_eq!(join, scan, "seed {seed} entity {e:?}");
        }
    }
}

#[test]
fn lens_demo_world_fact_count_is_pinned() {
    // The deterministic perf budget: the lens demonstration's fixed
    // construction commits exactly this many facts. Drift here means the
    // mint/override paths changed shape — re-derive the number deliberately.
    let mut w = world();
    let e = w
        .ledger
        .mint_instance("owlbear", None, "budget", &w.registry)
        .unwrap();
    w.ledger
        .commit(override_fact(e, SPECIES_MASS_KG, 900.0), &w.registry)
        .unwrap();
    w.ledger
        .change_kind(e, "woolly-mammoth", Some(2.0), "budget", &w.registry)
        .unwrap();
    assert_eq!(w.ledger.len(), 3); // instance-of, override, kind-change
}
