//! The occupancy readout (The Vacancy, T3): a committed measurement of
//! where each of the roster's kinds actually lives, over seeds 1..=30 at
//! full build depth. Authored condition optima (spec §5) only mean something
//! relative to the distribution of land a world actually produces — this is
//! the instrument that makes an optimum checkable against that distribution
//! instead of against the author's intuition. A prior campaign shipped the
//! kobold's elevation optimum at or above the highest land on most worlds
//! (its "exclusive highland stronghold" was unoccupiable) undetected for
//! campaigns; this readout exists so that failure mode is now a diffable CSV
//! rather than a silent gap. Tasks 7, 8, and 9 author every new kind's
//! condition niche against percentiles read out of the committed fixture
//! this file regenerates (`fixtures/occupancy.csv`).
//!
//! World-building idiom reused verbatim from `demesne.rs`/`waterline_probe.rs`
//! (`hornvale_worldgen::build_world`, `WorldComponents::assemble`,
//! `terrain_of`/`climate_of`/`sky_of` "reconstruct, never store"). The
//! per-species K comes from [`hornvale_worldgen::per_species_suitability`],
//! whose returned `u32` is a **build-local dense index, not identity** (see
//! its doc comment) — it is the position in the `species_biosphere` slice
//! passed in, so the index -> [`hornvale_kernel::KindId`] mapping here is
//! rebuilt from that exact same `wc.biosphere.iter()` ordering, once, and
//! reused for every seed (the roster's ordering does not vary by seed).
//!
//! The viability floor is [`hornvale_demography::FLOOR`] (1e-6) — the same
//! "AUTHORED prior (task A14)" constant `hornvale_demography::coexist::pack`
//! already uses to decide a share is ecological noise rather than presence
//! (`domains/demography/src/coexist.rs`), reused here unchanged rather than
//! inventing a second threshold, per this task's brief (Task 4 reuses the
//! identical value).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed, quantize};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of, per_species_suitability,
    sky_of, terrain_of,
};
use std::collections::BTreeMap;
use std::ops::RangeInclusive;

/// The viability floor below which a cell's K is ecological noise rather
/// than presence — [`hornvale_demography::FLOOR`], unchanged. Task 4 reuses
/// this identical value; two different floors would let a kind pass one
/// test and fail the other.
const VIABILITY_FLOOR: f64 = hornvale_demography::FLOOR;

/// Linear-interpolation percentile over an ascending-sorted slice (nearest
/// pair, weighted by fractional rank) — a plain float computation, not a
/// serialization boundary, so no quantization here (quantize happens once,
/// at render, per the constitutional emit-only rule).
fn percentile(sorted: &[f64], p: f64) -> f64 {
    match sorted.len() {
        0 => 0.0,
        1 => sorted[0],
        n => {
            let rank = (p / 100.0) * (n - 1) as f64;
            let lo = rank.floor() as usize;
            let hi = rank.ceil() as usize;
            if lo == hi {
                sorted[lo]
            } else {
                let frac = rank - lo as f64;
                sorted[lo] + (sorted[hi] - sorted[lo]) * frac
            }
        }
    }
}

/// Render the occupancy readout CSV for every seed in `seeds`, one row per
/// (kind, biome) that the kind occupies at least once across the whole
/// sweep. Pure aside from world genesis: same `seeds` in, byte-identical
/// string out (the drift check below depends on this).
fn render_occupancy_readout(seeds: RangeInclusive<u64>) -> String {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // The build-local dense index -> KindId mapping, built ONCE from the
    // exact same `wc.biosphere` ordering passed to `per_species_suitability`
    // below (ascending-KindId order, per `per_species_suitability`'s doc
    // comment) so every seed's returned `u32` tags resolve to the correct
    // kind. Verified below in the mandatory inspection step (kobold ->
    // highland, no kind -> predominantly marine).
    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    // Same `wc.biosphere` order as `bios`, so the realm slice stays
    // index-aligned — a kind absent from the sparse habitat-realm store
    // defaults to `Surface`.
    let realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .map(|(k, _)| {
            wc.habitat_realm
                .get(k)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();
    // THE LIVE `biome_affinity` REGISTRY, in the same `wc.biosphere` order as
    // `bios` and `realm` so all three slices stay index-aligned; a kind absent
    // from the sparse store resolves to `None`, which task 3's
    // `an_absent_affinity_is_bit_identical` proved is the unrestricted 1.0
    // no-op.
    //
    // Threaded rather than stubbed to `None`, because a stub would make this
    // file's own header false. It calls itself "a committed measurement of where
    // each of the roster's kinds ACTUALLY lives", and tasks 7-9 of a future
    // campaign author new kinds' condition niches against percentiles read out
    // of the fixture it writes. A biome affinity is precisely a statement about
    // where a kind lives; scoring the readout without it would publish the
    // pre-affinity world under a header promising the current one, and — worse —
    // the drift check would stay green through the change, because nothing would
    // have moved. Green because nothing moved is not the same as green because
    // nothing should have.
    let affinity: Vec<Option<hornvale_species::BiomeAffinity>> = wc
        .biosphere
        .iter()
        .map(|(k, _)| wc.biome_affinity.get(k).cloned())
        .collect();

    // Accumulated across every seed in the sweep, keyed by (kind, biome).
    // `occupied_k`: the K values of cells at/above the viability floor (the
    // presence distribution mean_k/p50_k/p95_k are computed over).
    // `biome_k_sum`: this biome's total K for the kind, unfiltered by the
    // floor (the numerator of `share_of_kind_k` — a biome's fraction of the
    // kind's total carrying capacity, not merely its occupied-cell count).
    // `kind_k_total`: the kind's world total K, unfiltered (the denominator).
    let mut occupied_k: BTreeMap<(&'static str, &'static str), Vec<f64>> = BTreeMap::new();
    let mut biome_k_sum: BTreeMap<(&'static str, &'static str), f64> = BTreeMap::new();
    let mut kind_k_total: BTreeMap<&'static str, f64> = BTreeMap::new();

    for seed in seeds {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));

        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_of(&world).expect("climate reconstructs");
        let sky = sky_of(&world).expect("sky reconstructs");
        let geo = terrain.geosphere();
        let system = sky
            .system()
            .unwrap_or_else(|| panic!("seed {seed} has a generated star system"));
        let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
        let obliquity = system.anchor.obliquity.get();
        let regime = match system.anchor.rotation {
            hornvale_astronomy::Rotation::Spinning { day, .. } => {
                hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
            }
            hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
        };

        let ks = per_species_suitability(
            geo, &terrain, &climate, obliquity, insolation, &regime, &bios, &realm, &affinity,
        );
        let biome_map = climate.biome_map();

        for (tag, k) in &ks {
            let kind = kinds[*tag as usize].0;
            for cell in geo.cells() {
                let v = *k.get(cell);
                *kind_k_total.entry(kind).or_insert(0.0) += v;
                let biome = biome_map.get(cell).name();
                *biome_k_sum.entry((kind, biome)).or_insert(0.0) += v;
                if v >= VIABILITY_FLOOR {
                    occupied_k.entry((kind, biome)).or_default().push(v);
                }
            }
        }
    }

    // `occupied_k` is a `BTreeMap<(&str, &str), _>`, so iterating it already
    // yields rows in ascending (kind, biome) order — no separate sort, and
    // no float ordering anywhere in that order (percentile's internal sort
    // uses `total_cmp` but never influences row order).
    let mut out = String::from("kind,biome,cells_occupied,share_of_kind_k,mean_k,p50_k,p95_k\n");
    for (&(kind, biome), values) in &occupied_k {
        let cells_occupied = values.len() as u32;
        let total_kind = *kind_k_total.get(kind).unwrap_or(&0.0);
        let biome_sum = *biome_k_sum.get(&(kind, biome)).unwrap_or(&0.0);
        let share = if total_kind > 0.0 {
            biome_sum / total_kind
        } else {
            0.0
        };
        let mean = values.iter().sum::<f64>() / values.len() as f64;
        let mut sorted = values.clone();
        sorted.sort_by(|a, b| a.total_cmp(b));
        let p50 = percentile(&sorted, 50.0);
        let p95 = percentile(&sorted, 95.0);
        // Quantize at emit only (constitutional rule): every computation
        // above ran at full precision, and only the rendered string is
        // rounded to 8 significant digits, exactly as `render_csv` does.
        out.push_str(&format!(
            "{kind},{biome},{cells_occupied},{},{},{},{}\n",
            quantize(share),
            quantize(mean),
            quantize(p50),
            quantize(p95),
        ));
    }
    out
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn occupancy_readout_is_current() {
    let rendered = render_occupancy_readout(1..=30);
    let committed = include_str!("fixtures/occupancy.csv");
    assert_eq!(
        rendered, committed,
        "occupancy readout drifted - if this is intended, rewrite the fixture \
         in the SAME commit as the change that drifted it"
    );
}

#[test]
// Deliberately NOT a `heavy:` reason. The heavy tier is what `make gate-full`
// runs, and this test WRITES the fixture — running it there would have CI
// silently rewrite the artifact the drift check above exists to check, so a
// drifted readout would self-heal instead of failing. Run it by hand when a
// change is meant to move the readout. Same rationale as the census fixtures'
// non-heavy reasons.
#[ignore = "regenerates the committed occupancy fixture; run by hand - the drift check above is the gate"]
fn regenerate_occupancy_readout() {
    let rendered = render_occupancy_readout(1..=30);
    std::fs::write(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/occupancy.csv"),
        rendered,
    )
    .expect("write occupancy.csv fixture");
}

/// **Preregistered, and NOT met in any region — the campaign's honest
/// headline, corrected.**
///
/// The Vacancy's exit criterion 6 asked that hot-arid, savanna and boreal each
/// gain at least one kind *centred* there. Measured against the committed
/// readout, regenerated 2026-08-09 (The Range, fix wave — the regeneration that
/// threaded the live `biome_affinity` store into this readout for the first
/// time). **Every row in the table below is byte-identical to the 2026-08-08
/// (The Assize) fixture**, checked rather than assumed: that regeneration moved
/// exactly 24 of 386 rows, all of them `gnoll` or `woolly-mammoth`, and none of
/// EC6's subjects is either.
///
/// | region | new kinds present | top occupant |
/// |---|---|---|
/// | hot-arid (desert) | giant-scorpion, carrion-crawler, shrieker | **otyugh** (0.0470) — NOT met |
/// | savanna | rhinoceros, giant-hyena, dire-wolf, gnoll, +5 | **treant** (0.0822) — NOT met |
/// | boreal (taiga) | carrion-crawler, rhinoceros, dire-wolf, +6 | **treant** (0.0545) — NOT met |
///
/// **EC6 is met in ZERO of three regions, not one — and it always was.** The
/// previous version of this table claimed `giant-scorpion` (0.0177) topped
/// desert. That figure matches nothing in either fixture: `giant-scorpion`'s
/// desert `mean_k` is **0.035982774** (`≈0.0359`), it is not the top occupant
/// there (`otyugh` at 0.0470 is), and it is not even the best newly-authored
/// kind in the region — `carrion-crawler` is, at rank 2 of 29 kinds present
/// in desert. (A naively-inherited "rank 2 of 26" would be wrong here: 26 was
/// desert's kind count in the *pre-regeneration* fixture, before this
/// regeneration's three new dwarves — see below — each added a desert row,
/// taking it to 29. Re-derived against the fixture this test actually reads,
/// not copied.)
///
/// **All three region rows above are byte-identical before and after this
/// regeneration** — `otyugh,desert`, `treant,savanna`, `treant,taiga` and
/// `giant-scorpion,desert` all carry the exact same `mean_k` in the fixture at
/// HEAD and in the regenerated one. This regeneration's drift (below) never
/// touched EC6's subject, so the 2026-08-05 "witnesses refreshed, verdict
/// unchanged" pass quoted above did not re-read the quantity this test
/// actually computes — it refreshed the printed numbers without re-ranking
/// them against the fixture's other occupants, and so missed that desert's
/// verdict was already NOT MET at the values it was quoting. This is the
/// **third** under-checked attribution recorded against this one file.
///
/// **The 2026-08-09 regeneration (The Range, fix wave) has exactly one cause,
/// and its blast radius is the arithmetic of the mechanism.** The row count did
/// not move (386 → 386) and **24 rows changed, every one of them `gnoll` (12) or
/// `woolly-mammoth` (12)** — the two and only two occupants of
/// `biome_affinity_registry`. The other 362 rows are byte-identical. That is
/// what a per-kind multiplier should do, and it is the check that the store is
/// reaching this path rather than being silently dropped: had the readout still
/// been fed an all-`None` slice, the regeneration would have produced a
/// byte-identical file and the drift check would have stayed green *because
/// nothing moved*, not because nothing should have.
///
/// Two internal consistencies worth naming, because they are what distinguishes
/// "the affinity arrived" from "some number changed":
///
/// - **A `1.00` stronghold leaves `mean_k` alone and moves only the share.**
///   `gnoll,desert` and `woolly-mammoth,ice` carry the identical `mean_k`,
///   `p50_k` and `p95_k` before and after, while their `share_of_kind_k` rises
///   (gnoll's desert share `0.0052262188 → 0.017680314`). The factor there is
///   1.0, so the kind's absolute capacity on that ground is untouched; what
///   changed is that every *other* biome was scaled down beneath it.
/// - **Every non-stronghold share moves by the ratio of its factor to 0.25.**
///   `gnoll,shrubland` (0.70) and `gnoll,savanna` (0.45) rise; the eight biomes
///   taking the 0.25 default fall together, by an identical 0.846×, and hold
///   their order exactly —
///   `temperate-forest / tropical-seasonal-forest` reads 1.0773 both before and
///   after, to five figures.
///
/// **This regeneration (2026-08-08, The Assize) has two disjoint causes of its
/// own**, measured rather than inherited from the campaign brief that
/// commissioned it. The row count went 350 → 386:
///
/// - **+36 rows are three new kinds** — `desert-dwarf`, `gully-dwarf` and
///   `hill-dwarf` (C2c, The Delvers), each contributing rows across biomes
///   that did not exist before.
/// - **24 rows *changed*, and they are exclusively `rust-monster` (12) and
///   `xorn` (12)** — the realm gate (`643d3c68`), whose own commit message
///   names precisely these two kinds: "A sparse two-row store. Its occupants
///   are the two kinds The Deep Realm re-authored for darkness and damp and
///   then left being scored against sunlight." (`rust-monster,alpine` alone
///   falls 83081 → 5586 occupied cells.)
/// - **326 of the 350 rows shared between the two fixtures are
///   byte-identical.** Attributing this drift to the three new dwarves alone
///   would be the +36 only, and would miss the 24 changed rows entirely — the
///   two causes are disjoint and both real.
///
/// **Two campaigns drifted this fixture, not one, and neither is the campaign
/// that re-pinned it.** The row count went 300 → 330, and the thirty new rows
/// decompose cleanly:
///
/// - **11 are `human,*`** — the kind was absent from this fixture entirely
///   (zero rows), because *The Generalist* (`6fef04fc`) authored human's
///   biosphere row after this fixture was last written. `KindId("human")` has
///   grep count **0** in `domains/species/src/lib.rs` at `d75cfea4` and **5**
///   at the merge-base. A roster addition, not a capacity change.
/// - **20 are `*,desert`** — desert went 6 → 26 rows, because *The Keeping*
///   (`448a203d`) decomposed `CarryingInput.habitable` into `is_land` and
///   opened the biome to the capacity field.
/// - The two sets **overlap in exactly one row**, `human,desert`, which needs
///   both causes: the kind has to exist *and* the biome has to be reachable.
///   11 + 20 − 1 = 30.
///
/// Of the 300 pre-existing rows, **90 are byte-identical** in the new fixture —
/// including taiga's `treant` reading, which is why the third row of the table
/// above is unchanged. So The Keeping moved *most* capacity readings, not all
/// of them.
///
/// The first version of this note named only The Keeping. That was an
/// under-checked attribution replacing an under-checked attribution — the exact
/// failure the retrospective for this campaign names as its own lesson, in the
/// correction to the correction. Recorded here rather than smoothed over,
/// because a committed test doc is where the next reader takes a cause on
/// faith.
///
/// **Zero of three regions gained a top-ranked occupant** — corrected above
/// from the "one of three" this doc previously claimed. All three gained real
/// presence and none gained dominance, and in all three the top slot belongs
/// to a *sessile autotroph* (`treant`, twice) or a *detritivore* (`otyugh`),
/// never to a predator or to a kind newly authored for that climate. That
/// **strengthens** the diagnosis rather than weakening it: `K = supply × Π
/// condition` multiplies a supply term spanning orders of magnitude by a
/// condition product bounded in `[0, 1]`, so the condition niche can only ever
/// modulate the NPP signal, never select against it. A photosynthate kind rides
/// that signal everywhere it is green; a predator authored for a specific
/// climate cannot outrank it there. That is BIO-supply-drowns-niche, and it is the named
/// prerequisite for this test.
///
/// The gnoll is the sharpest case, and it is also where this doc has been wrong
/// most often. **Every figure in the two bullets below is re-derived from the
/// fixture as it stands after the 2026-08-09 regeneration**, not carried
/// forward; the prior version of each is kept beside it, because the pattern of
/// how they went stale is the more useful thing.
///
/// - *Presence, still not dominance.* A people authored explicitly for hot-arid
///   desert once had **zero** desert occupancy. It holds **5498** desert cells
///   now, and declaring its affinity lifted desert's share of its world total K
///   from **0.0052262188 to 0.017680314** — a 3.38× rise, the single largest
///   proportional move in the file. It is still not dominance: desert is
///   gnoll's **third-smallest share of the twelve biomes it reaches** (only
///   `ice` and `alpine` are lower), and the region's top occupant by `mean_k` is
///   `otyugh` (0.04704871), then `carrion-crawler` and `shrieker`, with gnoll at
///   **0.0036231586** — an order of magnitude below. Desert is gnoll's 8th biome
///   by `mean_k`, a different column; the two rankings disagree and both are
///   quoted here rather than one.
///
///   *What the prior version said:* "3793 desert cells", share "0.0097",
///   "smallest share of any biome it reaches — 11th of 11", and
///   "`giant-scorpion` still tops the region". All four were true of a fixture
///   two regenerations back. `giant-scorpion` has not topped desert since The
///   Assize — the table at the head of this doc already said `otyugh` did, in
///   the same comment, four paragraphs up.
/// - *"Its largest share is temperate-forest" was right, and the correction of
///   it was wrong.* A previous version of this bullet called that claim "never
///   right" and asserted the largest share was **tropical-seasonal-forest
///   (0.3006)**. That was true when written — `75a0f450`'s fixture reads
///   `tropical-seasonal-forest 0.3005768` against `temperate-forest 0.27692454`
///   — but The Tense's regeneration (`979508f8`) reversed the pair, to
///   `temperate-forest 0.25118309` over `tropical-seasonal-forest 0.23314924`,
///   and the prose was not swept. After the affinity landed, both fall by the
///   same 0.25 default and the order is unchanged: **`temperate-forest`
///   (0.21243829)**, then `tropical-seasonal-forest` (0.19718615).
///
///   So the sentence this doc "corrected" had become true again by the time it
///   was corrected. A drift check pins output against *change*, never against
///   being *wrong* — which is what the prior version said, and is exactly how it
///   itself went wrong. The lesson it drew was right; it just did not apply it
///   to its own replacement text.
///
/// Its niche was deliberately left untuned — fitting the world to a
/// preregistered criterion is the one move that would invalidate the result.
///
/// This test is expected to fail until BIO-supply-drowns-niche lands. Its failure is the record.
#[test]
#[ignore = "PREREGISTERED, not met: awaits BIO-supply-drowns-niche (supply magnitude drowns the condition niche)"]
fn each_target_region_gains_a_top_ranked_occupant() {
    let rendered = render_occupancy_readout(1..=30);
    let mut top: std::collections::BTreeMap<String, (String, f64)> =
        std::collections::BTreeMap::new();
    for line in rendered.lines().skip(1) {
        let f: Vec<&str> = line.split(',').collect();
        let (kind, biome, mean_k) = (f[0], f[1], f[4].parse::<f64>().unwrap_or(0.0));
        let e = top
            .entry(biome.to_string())
            .or_insert((kind.to_string(), 0.0));
        if mean_k > e.1 {
            *e = (kind.to_string(), mean_k);
        }
    }
    let newly_authored = [
        "giant-scorpion",
        "giant-hyena",
        "dire-wolf",
        "rhinoceros",
        "giant-constrictor-snake",
        "carrion-crawler",
        "shrieker",
        "gnoll",
    ];
    for region in ["desert", "savanna", "taiga"] {
        let (kind, k) = top.get(region).expect("region is occupied");
        assert!(
            newly_authored.contains(&kind.as_str()),
            "{region}'s top occupant is {kind} ({k}), not a kind this campaign \
             authored for it - see BIO-supply-drowns-niche"
        );
    }
}
