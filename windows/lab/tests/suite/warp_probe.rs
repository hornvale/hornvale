//! The Warp, pre-spec probe: how much does what a WALKER IS TOLD at a facet
//! say about the derived features there?
//!
//! The Weft measured H3 as mutual information between a kind's own hidden
//! macro-state scalar (carbonate × drainage, etc.) and its occurrence. That
//! is the world's knowledge, not the walker's: the walker reads a biome
//! word, a descriptor noun, four micro-habitat qualifiers and (never) the
//! rock underfoot. This probe tabulates the SAME occurrence variable against
//! each rendered sign, over the same land-eligible population, so the spec
//! can say from a number rather than from a belief how legible the surface
//! is from inside it today. Committed rather than left in scratch for the
//! reason The Staple's retrospective gives: a design's numbers are its
//! load-bearing structure, and a scratchpad's go stale unread.
//!
//! Every sign is read through the published predicate the walker's prose
//! uses: `LocaleContext::describe` for the biome word, the descriptor noun
//! and the micro-field; `GeneratedTerrain::rock_at` at the room's dominant
//! corner (the read `reflectance_mixture_with_weights` performs). The one
//! duplicated constant is the ±0.33 micro-habitat threshold
//! (`grammar.rs`'s `land_micro_habitat`), which is private there.
//!
//! # Readout (seed 42, this tree, 2026-09-05; n_land = 11,218; occurrences
//! spring 403 / overhang 843 / thicket 1,517 / erratic 428)
//!
//! Mutual information in bits with the kind's occurrence, real pairing and
//! a lagged null (each facet's signs paired with the occurrence bits of the
//! facet 1,000 places later in vertex order — same marginals, link broken —
//! so a high-cardinality sign's finite-sample bias can be read off). The
//! `cause-4bin` row is the Weft's own H3 estimator and reproduces its
//! committed readings exactly, which is the positive control that this
//! population is the Weft's population.
//!
//! ```text
//! sign (cardinality)            spring     overhang   thicket    erratic
//! biome (19)                    0.0042     0.0022     0.0368     0.0012
//!   null                        0.0010     0.0013     0.0013     0.0012
//! rock (16)  [NOT rendered]     0.0041     0.0015     0.0166     0.0015
//!   null                        0.0006     0.0010     0.0024     0.0013
//! noun (62)                     0.0057     0.0044     0.0388     0.0022
//!   null                        0.0026     0.0029     0.0037     0.0033
//! wet (3)                       0.0006     0.0006     0.0126     0.0003
//! relief / aspect / openness    ≤ 0.0002 everywhere, at their nulls
//! visible-today = biome|noun|wet (177)
//!                               0.0108     0.0108     0.0500     0.0081
//!   null                        0.0083     0.0085     0.0111     0.0090
//! after-half-a = biome|rock|wet (271)
//!                               0.0191     0.0190     0.0587     0.0154
//!   null                        0.0130     0.0164     0.0206     0.0149
//! all-rendered (5,840)          0.1538     0.2391     0.3717     0.1546
//!   null                        0.1496     0.2376     0.3278     0.1542
//! cause-4bin (3), the ceiling   0.0078     0.0025     0.0386     0.0000
//!   null                        0.0001     0.0001     0.0004     0.0000
//! ```
//!
//! Net of null: thicket is legible today at its ceiling (0.039 of 0.039) —
//! its causes are temperature and moisture, and those ARE the biome word;
//! spring reaches 0.0025 of 0.0078 from what is rendered and 0.0061 once the
//! rock word is added; overhang 0.0023 of 0.0025; erratic 0 throughout.
//! The three address-noise axes read at their nulls, as noise must. The
//! whole rendered sentence (5,840 classes over 11,218 facets) reads
//! 0.15–0.37 bits and its null reads the same: a reading over a tuple that
//! rich is bias, not legibility, which is why every reading here is paired
//! with its null.
//!
//! # Where a kind's occurrences sit on its own cause
//!
//! Facets / occurrences / rate per cause bin (`macro_state` in four equal
//! bins over `[0,1]`), and the share of occurrences on a facet whose cause
//! reads ≥ 0.5:
//!
//! ```text
//! spring    b0 10,754/333/0.031   b1 342/39/0.114   b2 122/31/0.254            share 0.077
//! overhang  b0  7,399/474/0.064   b1 3,400/321/0.094  b2 371/42/0.113  b3 48/6/0.125   share 0.057
//! thicket   b0  5,901/392/0.066   b1 2,836/478/0.169  b2 2,195/545/0.248  b3 286/102/0.357  share 0.427
//! erratic   b2 11,218/428/0.038 (its cause is a constant)                       share 1.000
//! ```
//!
//! This is the finding the campaign is built on. The spring's cause class
//! is rare (464 of 11,218 facets read ≥ 0.25) and the recipe's noise floor —
//! `abundance × (1 − contextuality) × noise` — runs over the other 96% of
//! land, so **333 of 403 springs stand on a facet with no cause at all**. A
//! knowledgeable observer told the whole truth about carbonate and drainage
//! would still be unable to predict 92% of springs. The Weft's falsified
//! ordering was never about the estimator: the 19-class biome word ranks
//! thicket above spring exactly as the 4-bin cause does. It is a base-rate
//! effect — a small false-alarm rate over a large population outnumbers a
//! high hit rate over a small one — and it lives in the recipe, which is the
//! only place a campaign can move it.
//!
//! # Cost
//!
//! 1.2–1.3 s wall for the whole sweep in nextest's default (unoptimized)
//! profile on the Mac; `#[ignore]`d because it is a recording, not a guard.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Facet, WorldTime, blend_corner_weights};
use hornvale_locale::LocaleContext;
use hornvale_worldgen::{WeftKind, field_pack_from, occurs, prevalence_with_weights};
use std::collections::BTreeMap;

/// Discrete mutual information, in bits, between a categorical sign and a
/// binary occurrence.
fn mi(joint: &BTreeMap<(String, bool), u64>) -> f64 {
    let total: u64 = joint.values().sum();
    let total = total as f64;
    let mut px: BTreeMap<&str, f64> = BTreeMap::new();
    let mut py = [0.0f64; 2];
    for ((x, y), &c) in joint {
        *px.entry(x.as_str()).or_insert(0.0) += c as f64 / total;
        py[*y as usize] += c as f64 / total;
    }
    let mut out = 0.0;
    for ((x, y), &c) in joint {
        let pxy = c as f64 / total;
        let p = px[x.as_str()] * py[*y as usize];
        if pxy > 0.0 && p > 0.0 {
            out += pxy * hornvale_kernel::math::log2(pxy / p);
        }
    }
    out
}

fn word(v: f64, lo: &str, hi: &str) -> String {
    if v > 0.33 {
        hi.to_string()
    } else if v < -0.33 {
        lo.to_string()
    } else {
        "mid".to_string()
    }
}

/// The Warp's premise measurement, recorded once (see the module doc).
#[test]
#[ignore = "probe: pre-spec measurement — builds seed 42 and describes one room per geosphere vertex (40,962 `LocaleContext::describe` calls); a pre-spec measurement recorded once, not a gate"]
#[allow(clippy::disallowed_methods)]
fn what_the_walker_is_told_says_this_much_about_the_weft() {
    let _ = SkyPins::default();
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = field_pack_from(&terrain, &climate);
    let ctx = LocaleContext::build_from(&world, &terrain, &climate);
    let geo = ctx.climate().geosphere();
    let index = ctx.nearest_index();
    let seed = world.seed;
    let depth = geo.depth() + 7;

    // One row per land-eligible facet: the rendered sign tokens, the four
    // occurrence bits, and the Weft's own 4-bin cause index per kind.
    let signs = [
        "biome",
        "rock",
        "noun",
        "wet",
        "relief",
        "aspect",
        "openness",
        "biome+rock",
        "biome+rock+wet",
        "all-rendered",
        "cause-4bin",
        "visible-today",
        "visible-today+noise",
        "after-half-a",
    ];
    let mut rows: Vec<(Vec<String>, [bool; 4], [String; 4])> = Vec::new();
    let mut n_occ = [0u64; 4];

    for v in 0..geo.vertex_count() {
        let vertex = hornvale_kernel::Vertex(v as u32);
        let facet = Facet::containing(geo.position(vertex), depth);
        let Some(weights) = facet.corner_weights(geo, index) else {
            continue;
        };
        if blend_corner_weights(weights, &pack.land) < 0.5 {
            continue;
        }
        let loc = ctx.describe(&facet, WorldTime::GENESIS).expect("describe");
        let dom = weights
            .iter()
            .max_by_key(|(_, w)| *w)
            .map(|(c, _)| *c)
            .expect("four corners");
        let rock = format!("{:?}", terrain.rock_at(dom));
        let m = loc.regime.micro;
        let wet = word(m.wetness, "dry", "damp");
        let relief = word(m.relief, "hollow", "rise");
        let aspect = word(m.aspect, "shaded", "sun");
        let openness = word(m.openness, "closed", "open");
        let biome = loc.biome.clone();
        let noun = loc.regime.descriptor_noun.clone();
        let rendered = vec![
            biome.clone(),
            rock.clone(),
            noun.clone(),
            wet.clone(),
            relief.clone(),
            aspect.clone(),
            openness.clone(),
            format!("{biome}|{rock}"),
            format!("{biome}|{rock}|{wet}"),
            format!("{biome}|{rock}|{noun}|{wet}|{relief}|{aspect}|{openness}"),
            String::new(), // slot 10 is the cause column, filled from `bins`
            format!("{biome}|{noun}|{wet}"),
            format!("{biome}|{noun}|{wet}|{relief}|{aspect}|{openness}"),
            format!("{biome}|{rock}|{wet}"),
        ];
        let mut ys = [false; 4];
        let mut bins: [String; 4] = Default::default();
        for (slot, kind) in WeftKind::ALL.iter().enumerate() {
            let p = prevalence_with_weights(*kind, &facet, weights, &pack, seed);
            ys[slot] = occurs(*kind, &facet, seed, p);
            if ys[slot] {
                n_occ[slot] += 1;
            }
            let ms = kind.macro_state(weights, &pack);
            bins[slot] = format!("b{}", ((ms.clamp(0.0, 1.0) * 4.0) as usize).min(3));
        }
        rows.push((rendered, ys, bins));
    }
    let n_land = rows.len();

    // `shift` pairs each facet's signs with the occurrence bits of the facet
    // `shift` places later in vertex order (wrapping) — a lagged pairing that
    // breaks the sign/occurrence link while keeping both marginals, so the
    // finite-sample bias of a high-cardinality sign can be read off directly.
    let table = |i: usize, slot: usize, shift: usize| -> BTreeMap<(String, bool), u64> {
        let mut t = BTreeMap::new();
        for (r, row) in rows.iter().enumerate() {
            let (_, ys, _) = &rows[(r + shift) % rows.len()];
            let x = if i == 10 {
                row.2[slot].clone()
            } else {
                row.0[i].clone()
            };
            *t.entry((x, ys[slot])).or_insert(0) += 1;
        }
        t
    };

    println!("n_land={n_land} occurs spring/overhang/thicket/erratic={n_occ:?}");
    for shift in [0usize, 1000] {
        println!(
            "--- shift {shift} ({}) ---",
            if shift == 0 {
                "real pairing"
            } else {
                "lagged null"
            }
        );
        println!(
            "{:<18} {:>10} {:>10} {:>10} {:>10}",
            "sign", "spring", "overhang", "thicket", "erratic"
        );
        for (i, name) in signs.iter().enumerate() {
            let row: Vec<String> = (0..4)
                .map(|k| format!("{:.6}", mi(&table(i, k, shift))))
                .collect();
            println!(
                "{:<18} {:>10} {:>10} {:>10} {:>10}",
                name, row[0], row[1], row[2], row[3]
            );
        }
    }
    println!("--- lift: P(Y|best class with support>=100) / P(Y), and H(Y) bits ---");
    for (slot, &n_y) in n_occ.iter().enumerate() {
        let py = n_y as f64 / n_land as f64;
        let hy = -(py * hornvale_kernel::math::log2(py)
            + (1.0 - py) * hornvale_kernel::math::log2(1.0 - py));
        print!("kind {slot} H(Y)={hy:.4} P(Y)={py:.4} |");
        for i in [0usize, 1, 3, 8, 10, 11, 13] {
            let t = table(i, slot, 0);
            let mut per: BTreeMap<String, (u64, u64)> = BTreeMap::new();
            for ((x, y), &c) in &t {
                let e = per.entry(x.clone()).or_insert((0, 0));
                e.0 += c;
                if *y {
                    e.1 += c;
                }
            }
            let best = per
                .iter()
                .filter(|(_, (n, _))| *n >= 100)
                .map(|(x, (n, k))| (*k as f64 / *n as f64, x.clone(), *n))
                .max_by(|a, b| a.0.total_cmp(&b.0));
            if let Some((rate, x, n)) = best {
                print!(" {}: {:.1}x [{x} n={n}]", signs[i], rate / py);
            }
        }
        println!();
    }
    println!(
        "--- occurrences by cause bin: n facets / n occurs / rate, and the share of occurrences in b2+b3 ---"
    );
    for slot in 0..4 {
        let mut per: BTreeMap<String, (u64, u64)> = BTreeMap::new();
        for (_, ys, bins) in &rows {
            let e = per.entry(bins[slot].clone()).or_insert((0, 0));
            e.0 += 1;
            if ys[slot] {
                e.1 += 1;
            }
        }
        let high: u64 = per
            .iter()
            .filter(|(b, _)| b.as_str() >= "b2")
            .map(|(_, (_, k))| *k)
            .sum();
        print!("kind {slot} |");
        for (b, (n, k)) in &per {
            print!(" {b}: {n}/{k}/{:.4}", *k as f64 / *n as f64);
        }
        println!(" | share in b2+b3: {:.4}", high as f64 / n_occ[slot] as f64);
    }
    for (i, name) in signs.iter().enumerate() {
        let card: std::collections::BTreeSet<String> =
            table(i, 0, 0).keys().map(|(x, _)| x.clone()).collect();
        println!("cardinality {name}: {}", card.len());
    }
}
