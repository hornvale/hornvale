//! THE TIDEMARK, Task 4 Step 1 — MEASURE the marine trophic web before
//! authoring anything against it.
//!
//! This probe answers, with real numbers rather than a reading of the source:
//! does every marine kind (the six peoples plus the four existing marine
//! fauna) sit at trophic height 1.0 with no `predation()` edges among them,
//! because `MARINE_FORAGE`/`CHEMOSYNTHATE` give no height and no
//! `ANIMAL_PREY` weight? And separately — since flatness on the trophic axis
//! says nothing about competition — do these ten kinds' niches actually
//! overlap on `hornvale_kernel::ecology::ResourceVector::overlap`, the
//! quantity `hornvale_demography::coexist::pack` feeds into a world's
//! committed per-vertex population share?
//!
//! Reuses the `(id, Mass, ResourceVector)` row idiom
//! `windows/worldgen/tests/suite/waterline_probe.rs` builds from
//! `biosphere_registry()`, rather than inventing a new one. Unlike
//! `waterline_probe`, this probe needs no built world at all — every
//! quantity it reports is a pure function of the two species registries
//! (`hornvale_species::biosphere_registry`,
//! `hornvale_species::habitat_realm_registry`), so it skips `build_world`
//! entirely.
//!
//! Run with:
//!   cargo nextest run -p hornvale-worldgen --test suite -- marine_web_probe --nocapture --ignored

use hornvale_kernel::{KindId, Mass, ResourceVector, v1_basis};
use hornvale_species::HabitatRealm;

/// The six marine peoples (Task 3) plus the four existing marine fauna (The
/// Vacancy, T8) — the ten kinds this probe measures.
///
/// Hardcoded rather than derived from `habitat_realm_registry()`: that
/// registry gates settlement placement (`per_species_suitability`) and
/// fauna do not settle, so the four fauna never appear in it at all (see the
/// cross-check printed below) — the marine set cannot be recovered from that
/// registry alone.
const MARINE_KINDS: [&str; 10] = [
    "abyssal-elf",
    "kelp-tender",
    "merfolk",
    "reef-mason",
    "triton",
    "vent-commensal",
    "reef-shark",
    "giant-octopus",
    "killer-whale",
    "giant-squid",
];

/// Render a niche's nonzero axis weights, e.g. "marine forage=1.00".
fn fmt_niche(niche: &ResourceVector) -> String {
    v1_basis()
        .iter()
        .filter_map(|axis| {
            let w = niche.weight(*axis);
            if w > 0.0 {
                Some(format!("{}={:.2}", axis.label, w))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// claim: readout(off-gate, prints only, one hard assertion — the harness
/// control) — trophic height, predation edges, and niche overlap for every
/// marine kind, against the full `biosphere_registry()` as control.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn marine_web_probe() {
    let bio = hornvale_species::biosphere_registry();
    let realm_registry = hornvale_species::habitat_realm_registry();

    // Dense (id, name) alignment: `bio.iter()` yields `KindId`-ascending
    // order (the store's `BTreeMap` backing), so `names[i]` and `rows[i]`
    // refer to the same kind for every `i` — the same idiom
    // `waterline_probe.rs` uses over `wc.biosphere`.
    let names: Vec<&'static str> = bio.ids().map(|k| k.0).collect();
    let rows: Vec<(u32, Mass, ResourceVector)> = bio
        .iter()
        .enumerate()
        .map(|(i, (_, b))| (i as u32, b.mass, b.niche.clone()))
        .collect();

    println!(
        "\n=== THE TIDEMARK marine web probe — {} kinds in biosphere_registry() ===",
        names.len()
    );

    // --- Cross-check: which marine kinds does habitat_realm_registry know? ---
    println!("\n-- habitat_realm_registry cross-check (fauna are expected ABSENT)");
    for name in MARINE_KINDS {
        let id = KindId(name);
        let present = realm_registry.contains(&id);
        let realm = realm_registry
            .get(&id)
            .copied()
            .unwrap_or(HabitatRealm::SURFACE);
        println!("   {name:16} present_in_registry={present:5}  resolves_to={realm:?}");
    }

    // --- Trophic height, every kind, sorted descending -----------------------
    let niches: Vec<(u32, ResourceVector)> =
        rows.iter().map(|(id, _, v)| (*id, v.clone())).collect();
    let levels = hornvale_demography::niche::trophic_levels(&niches);

    let mut by_height: Vec<(u32, f64)> = levels.iter().map(|(id, h)| (*id, *h)).collect();
    by_height.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cmp(&b.0)));

    println!(
        "\n-- trophic_levels, ALL {} kinds, sorted by height descending (marine tagged)",
        by_height.len()
    );
    for (id, h) in &by_height {
        let name = names[*id as usize];
        let tag = if MARINE_KINDS.contains(&name) {
            " [MARINE]"
        } else {
            ""
        };
        println!(
            "   {:22} height={:<10.6}{}  niche: {}",
            name,
            h,
            tag,
            fmt_niche(&rows[*id as usize].2)
        );
    }

    let marine_heights: Vec<f64> = MARINE_KINDS
        .iter()
        .map(|name| {
            let id = names.iter().position(|n| n == name).unwrap() as u32;
            levels[&id]
        })
        .collect();
    println!("\n-- marine heights only, in MARINE_KINDS order: {marine_heights:?}");

    // --- predation() restricted to marine kinds -------------------------------
    let web = hornvale_demography::niche::predation(&rows);
    println!("\n-- predation() restricted to the ten marine kinds");
    for name in MARINE_KINDS {
        let id = names.iter().position(|n| *n == name).unwrap() as u32;
        match web.get(&id) {
            Some(prey) => {
                let prey_names: Vec<&str> = prey.iter().map(|p| names[*p as usize]).collect();
                println!("   {name:16} PRESENT in web, prey={prey_names:?}");
            }
            None => println!(
                "   {name:16} ABSENT from web (ANIMAL_PREY weight <= 0, so `predation` `continue`s past it — never inserted, not an empty Vec)"
            ),
        }
    }

    // --- Control: prove the harness can see edges at all ----------------------
    println!("\n-- control: a NON-marine predator's non-empty prey list, same `web`");
    let mut control: Option<(&str, Vec<&str>)> = None;
    for (id, prey) in web.iter() {
        let name = names[*id as usize];
        if !MARINE_KINDS.contains(&name) && !prey.is_empty() {
            let prey_names: Vec<&str> = prey.iter().map(|p| names[*p as usize]).collect();
            control = Some((name, prey_names));
            break;
        }
    }
    match &control {
        Some((name, prey_names)) => {
            println!("   {name:16} eats {prey_names:?}  (proves the harness sees edges)")
        }
        None => println!("   NO non-marine predator has a non-empty prey list in this web"),
    }
    assert!(
        control.is_some(),
        "no non-marine predator produced a non-empty prey list in `web` — the harness cannot be \
         trusted to report marine emptiness as a real finding rather than a broken probe"
    );

    // --- Niche overlap among the ten marine kinds -----------------------------
    // `ResourceVector::overlap` (kernel/src/ecology.rs) is exactly the
    // quantity `hornvale_demography::niche::guild_overlap` derives per pair
    // and `hornvale_demography::coexist::pack` (coexist.rs:453) feeds into
    // `vertex_share`'s competition denominator — a REAL, world-committed
    // per-vertex population share, not a diagnostic-only number. See the
    // report for the full wiring citation.
    let marine_idx: Vec<(usize, &str)> = MARINE_KINDS
        .iter()
        .map(|n| (names.iter().position(|x| x == n).unwrap(), *n))
        .collect();

    println!("\n-- pairwise niche overlap (Pianka symmetric), the ten marine kinds");
    print!("   {:16}", "");
    for (_, n) in &marine_idx {
        print!(" {n:>14}");
    }
    println!();
    for (i, ni) in &marine_idx {
        print!("   {ni:16}");
        for (j, _) in &marine_idx {
            let o = rows[*i].2.overlap(&rows[*j].2);
            print!(" {o:>14.4}");
        }
        println!();
    }

    // Called out explicitly per spec: the six peoples' overlap against every
    // OTHER marine kind (peoples and fauna alike) — the direct answer to
    // "would a newly authored kelp actually compete with a merfolk?"
    const PEOPLES: [&str; 6] = [
        "abyssal-elf",
        "kelp-tender",
        "merfolk",
        "reef-mason",
        "triton",
        "vent-commensal",
    ];
    println!("\n-- the six peoples' overlap against every OTHER marine kind");
    for person in PEOPLES {
        let pi = names.iter().position(|n| *n == person).unwrap();
        for (j, other) in &marine_idx {
            if *other == person {
                continue;
            }
            let o = rows[pi].2.overlap(&rows[*j].2);
            println!("   {person:16} vs {other:16} overlap={o:.4}");
        }
    }
}
