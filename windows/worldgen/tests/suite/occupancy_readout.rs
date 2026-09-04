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
    SettlementPins, WorldComponents, build_world, climate_of, per_species_suitability, sky_of,
    terrain_of,
};
use std::collections::BTreeMap;
use std::ops::RangeInclusive;

/// The viability floor below which a vertex's K is ecological noise rather
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
    // `occupied_k`: the K values of vertices at/above the viability floor (the
    // presence distribution mean_k/p50_k/p95_k are computed over).
    // `biome_k_sum`: this biome's total K for the kind, unfiltered by the
    // floor (the numerator of `share_of_kind_k` — a biome's fraction of the
    // kind's total carrying capacity, not merely its occupied-vertex count).
    // `kind_k_total`: the kind's world total K, unfiltered (the denominator).
    let mut occupied_k: BTreeMap<(&'static str, &'static str), Vec<f64>> = BTreeMap::new();
    let mut biome_k_sum: BTreeMap<(&'static str, &'static str), f64> = BTreeMap::new();
    let mut kind_k_total: BTreeMap<&'static str, f64> = BTreeMap::new();

    for seed in seeds {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
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
                hornvale_climate::RotationRegime::Spinning {
                    day_std: day.as_std_days(),
                }
            }
            hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
        };

        let ks = per_species_suitability(
            geo, &terrain, &climate, obliquity, insolation, &regime, &bios, &realm, &affinity,
        );
        let biome_map = climate.biome_map();

        for (tag, k) in &ks {
            let kind = kinds[*tag as usize].0;
            for vertex in geo.vertices() {
                let v = *k.get(vertex);
                *kind_k_total.entry(kind).or_insert(0.0) += v;
                let biome = biome_map.get(vertex).name();
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
// RESTORED TO `heavy:` BY THE GOVERNOR'S FINAL REVIEW (2026-08-28), reversing
// its own adjudication. The DEMOTE verdict read this test's failure message
// ("rewrite the fixture in the SAME commit as the change that drifted it") as
// the report branch. That reasoning proves too much: it applies word for word
// to `fixture_staleness::census_fixtures_match_a_probe_of_live_seeds`, which
// was KEPT and which decision 0426 builds a section on. This is not a pinned
// historical number — it is an exact `assert_eq!` of a live render against a
// committed byte golden, the same change-detector shape, and the sibling
// regenerator below calls it "the gate" in its own words. Decisive:
// `windows/worldgen/tests/fixtures/occupancy.csv` is NOT declared in
// `docs/generated-paths.txt`, so this test is the only automated path in the
// tree that ever observes that artifact at all. See
// `docs/audits/heavy-tier-adjudication.md` (its row, flipped to KEEP) and
// decision 0086's third amendment.
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn occupancy_readout_is_current() {
    let rendered = render_occupancy_readout(1..=30);
    let committed = include_str!("../fixtures/occupancy.csv");
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
/// readout, regenerated twice on 2026-08-10 (The Radiation task 3, and again in
/// its fix round once the ladder's LEVEL was derived) and before that 2026-08-09
/// (The Range, fix wave — the regeneration that threaded the live
/// `biome_affinity` store into this readout for the first time). **Every row in
/// the table below is byte-identical to the fixture at `d7719e27`**, three
/// regenerations back, checked rather than assumed: `otyugh,desert`,
/// `treant,savanna`, `treant,taiga` and `giant-scorpion,desert` were compared
/// field by field between that fixture, `834fee5c`'s and the live one, and all
/// four rows are equal in every column. Nothing since has moved a row that is
/// not `gnoll`, `woolly-mammoth` or an elf, and none of EC6's subjects is any
/// of those.
///
/// | region | Vacancy kinds present | kinds present | top occupant (`mean_k`) |
/// |---|---|---|---|
/// | hot-arid (desert) | all 8 | 35 | **otyugh** (0.04704871) — NOT met |
/// | savanna | all 8 | 35 | **treant** (0.082202295) — NOT met |
/// | boreal (taiga) | all 8 | 35 | **treant** (0.05448946) — NOT met |
///
/// ("all 8" is `giant-scorpion`, `giant-hyena`, `dire-wolf`, `rhinoceros`,
/// `giant-constrictor-snake`, `carrion-crawler`, `shrieker`, `gnoll` — every kind
/// The Vacancy authored for these regions carries a row in every one of the
/// three, and has in all three fixtures checked here. The column previously held
/// partial lists with `+5`/`+6` suffixes that summed to neither 8 nor 35 and
/// whose denominator was never stated; it is replaced by the two counts, both
/// obtained by filtering the live fixture's rows on `biome`.)
///
/// **Re-read against the 2026-08-10 regenerations, and the verdicts are
/// unchanged.** Six elves entered every region, and none of them displaced a top
/// occupant or came close: in desert the best elf is drow at **rank 9 of 35**
/// (0.010847939) against otyugh's 0.04704871; in savanna it is desert-elf at
/// rank 17 of 35 (0.017667515) against treant's 0.082202295; in taiga it is drow
/// at rank 12 of 35 (0.023019544) against treant's 0.05448946. Every one of
/// those six figures was obtained the same way — load the fixture this test
/// reads *as it stands after this commit's regeneration*, group its rows by
/// biome, sort each group descending on `mean_k`, and read off the rank and the
/// value — not carried forward from the previous version of this paragraph.
///
/// Carrying them forward is precisely what went wrong. The previous version of
/// this paragraph read "drow at rank 14 of 35 (0.006319)", "desert-elf at
/// (0.014963)" and "drow at rank 12 (0.019443)". Those are the abandoned `0.25`
/// arm: `cda3e3c4` derived the ladder's level from each kind's
/// `sovereignty_floor`, which raised every elf's default and every elf's
/// `mean_k` with it, and the paragraph was not re-read. One of the three was
/// wrong in **rank** and not merely in digits — drow moved five places up the
/// desert ranking, from 14th to 9th. The verdicts survive only because the
/// numerator moved and the three top occupants did not.
///
/// **EC6 is met in ZERO of three regions, not one — and it always was.** The
/// previous version of this table claimed `giant-scorpion` (0.0177) topped
/// desert. That figure matches nothing in either fixture: `giant-scorpion`'s
/// desert `mean_k` is **0.035982774** (`≈0.0359`), it is not the top occupant
/// there (`otyugh` at 0.04704871 is), and it is not even the best
/// newly-authored kind in the region — `carrion-crawler` is, at **rank 2 of 35**
/// kinds present in desert (0.046748017). (The denominator has now gone stale
/// twice, which is why it is stated with its provenance every time: it was 26
/// before The Assize's three new dwarves each added a desert row, 29 after them,
/// and 35 now that all six elves carry one. Each figure was true when written
/// and none was re-derived by the campaign that inherited it. Counted here by
/// filtering the live fixture's rows on `biome == "desert"`, not copied.)
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
/// **The 2026-08-10 regenerations move exactly the kinds that carry an affinity
/// row, and nobody else.** Against `834fee5c`, the last fixture written before
/// this campaign touched the store, the live file decomposes as **80 rows added,
/// 24 changed, 362 byte-identical**, 386 → 466. Obtained by loading both files,
/// keying every row on `(kind, biome)`, and comparing the sets and then the
/// surviving rows field by field:
///
/// - the **80 added** rows are the six elves and nothing else — `sea-elf` 20,
///   because it is the one occupant reaching the marine classes, and 12 each for
///   `desert-elf`, `drow`, `high-elf`, `snow-elf` and `wood-elf`;
/// - the **24 changed** rows are `gnoll` (12) and `woolly-mammoth` (12) — the two
///   pre-existing occupants of `biome_affinity_registry`, neither of which this
///   campaign edited.
///
/// An earlier version of this paragraph called the regeneration **PURELY
/// ADDITIVE** — "not one of the 386 pre-existing rows changed a single byte".
/// That was true of `59904d50`, which only authored six new rows, and it stopped
/// being true one commit later at `cda3e3c4`, which replaced the ladder's
/// authored fourth step with each kind's `sovereignty_floor`. **A derivation is
/// registry-wide where an authored row is not**: it relevels every row in the
/// store, so The Range's two kinds moved without being touched, and the two
/// commits could not both be described by the same sentence. The dead claim is
/// kept beside its replacement because the shape of the mistake is the reusable
/// part — it was a true statement about one commit's diff, left standing over
/// the next commit's, in a doc that still read as though it had measured the
/// file it sits beside.
///
/// The argument that claim was serving does survive, and is worth keeping
/// because the obvious expectation is the opposite. An affinity row redistributes
/// *placement* violently: `range_readout.rs`'s P1″ arms, re-run on this tree,
/// take gnoll's seed-42 settlements from **13 to 40** and its arid share from
/// **0.000000 to 0.825000** between the affinity-absent and affinity-shipped
/// worlds. So one might expect this fixture to move everywhere. It does not,
/// because this readout is not placement. It renders `per_species_suitability`,
/// and a kind's suitability field is a function of **that kind's own** biosphere
/// row, realm and affinity against the world's fields — no other kind appears in
/// it. Competition enters at the bake, one layer down from here. So a new kind
/// can only ADD rows to this file, and an affinity row can only move the kind
/// that declares it — which is exactly why a registry-wide *relevel* is the one
/// edit that moves rows nobody wrote.
///
/// Two internal consistencies confirm the store reached this path rather than
/// being dropped, using the same discriminating check the 2026-08-09
/// paragraph below relies on:
///
/// - **`high-elf` and `wood-elf` are identical in every numeric column of all
///   twelve of their biome rows** (taiga `0.013465794`, savanna `0.0098947532`,
///   desert `0.002177014`, …). Checked, not sampled: both kinds' twelve rows
///   were pulled from the live fixture, sorted by biome, and compared column by
///   column with the `kind` field dropped — the biome lists match and **zero**
///   rows differ in any of `cells_occupied`, `share_of_kind_k`, `mean_k`,
///   `p50_k`, `p95_k`. They carry the same biosphere row and the same affinity
///   row and differ only in psyche, society and language — none of which this
///   path reads — so spec §3.6's MIND control is confirmed at the field level
///   here, which is P3(a)'s primary arm.
/// - **`drow` diverges from both on all twelve** (taiga `0.023019544` against
///   their `0.013465794`). **Do not read that as the realm gate**, and do not
///   read it as its mass either. Its affinity row IS wood's, byte for byte,
///   level included. Two other inputs this path reads do differ: its resource
///   vector (`DETRITUS`-dominant against wood's `PLANT_FORAGE`) and its
///   `HabitatRealm`. Its mass differs too (52.0 kg against 55.0), and an earlier
///   version of this bullet named that as a third cause "which moves the
///   sovereignty floor 0.424802 vs 0.429202". **`drow`'s OWN mass is not a cause
///   here** — but the reason is narrow, and a first attempt at this correction
///   got it inverted. Three arms, each run against the fixture at HEAD and then
///   reverted:
///
///   ```text
///     drow        52 →  500 kg   GREEN — readout byte-identical
///     desert-elf  50 →  500 kg   RED   — 12 rows move, all its own
///     wood-elf    55 →  550 kg   RED   — 36 rows move: wood 12, high 12, DROW 12
///   ```
///
///   **The drow arm is green because drow's affinity row is `wood.clone()`**, so
///   drow's affinity *level* is a function of wood-elf's mass and never of its
///   own — not because a mass cannot reach this readout. It plainly can. The
///   correction this bullet first carried said mass "reaches nothing but
///   `sovereignty_floor`, that floor reaches nothing but `tolerance_liebig`'s
///   floored axes … so the floor is computed and discarded", and generalised
///   that to mass as such. Half of it is right and the half that is wrong is the
///   consequential one:
///
///   - **True.** The floor computed *inside* `per_species_suitability` is
///     discarded. Every occupant of this registry has `elevation.devotion` below
///     its floor, so the unfloored elevation term is `tolerance_liebig`'s
///     minimum at every vertex and the floor never enters the product.
///   - **False as a statement about mass.** The same `sovereignty_floor` sets
///     each affinity row's LEVEL — `biome_affinity_registry` builds every row as
///     `BiomeAffinity::from_preferences(floor_of(kind), …)` — and the affinity
///     multiplies **outside** that minimum. So mass reaches this field through
///     the affinity for every kind whose row is self-derived: six of the eight
///     occupants. Drow and high-elf are exempt only because they take wood's row
///     entire. The desert-elf arm above is the positive control the first
///     attempt never ran, and it fires.
///
///   The desert-elf arm also shows the signature cleanly: its **stronghold** row
///   (`desert`) keeps `mean_k`, `p50_k` and `p95_k` to the byte under a tenfold
///   mass change — a stronghold maps to exactly `1.00` for any floor — while its
///   `share_of_kind_k` and all eleven other biomes move.
///
///   One thing the drow arm did **not** hold fixed, and which the first
///   correction reported as a clean null: it is not world-neutral. `hornvale new
///   --seed 42` under drow at 500 kg differs from the unmutated world in exactly
///   **one fact of 12,797** — the world's own name, because `dominant_people_in`
///   weights candidates by `flagship.population × bio.mass.kilograms()`. Counts
///   are unmoved (230 settlements, 474 ruins, gnoll's 40), so *placement* is
///   untouched; "byte-identical" is a claim about this readout only.
///
///   The apportionment is therefore two-way, not three-way, and this readout
///   still cannot perform it; nothing here should be quoted as the gate's
///   magnitude. `warren_readout.rs` is the file that isolates the realm
///   question, by holding every other input fixed and emptying one store.
///
/// What the divergence *does* establish is the discriminating half: had the six
/// elf rows been silently dropped from this path, wood and drow would still
/// differ (their resource vectors and realms alone would do it — *not* their
/// masses, since drow takes wood's row and its own mass therefore reaches
/// nothing here), so drow is not the check. The check is that all twelve elf
/// rows appear at all, and that wood and high come out identical rather than
/// merely close.
///
/// **The 2026-08-09 regeneration (The Range, fix wave) has exactly one cause,
/// and its blast radius is the arithmetic of the mechanism.** *This paragraph
/// and its two bullets describe the `d7719e27` → `834fee5c` pair, not the file
/// as it stands* — both were re-measured from git for this fix round rather than
/// left as inherited prose, precisely because two regenerations have landed
/// since and every unqualified number in this doc has been read as current at
/// least once. The row count did not move (386 → 386) and **24 rows changed,
/// every one of them `gnoll` (12) or `woolly-mammoth` (12)** — the two and only
/// two occupants of `biome_affinity_registry` at the time. The other 362 rows
/// are byte-identical. That is what a per-kind multiplier should do, and it is
/// the check that the store is reaching this path rather than being silently
/// dropped: had the readout still been fed an all-`None` slice, the regeneration
/// would have produced a byte-identical file and the drift check would have
/// stayed green *because nothing moved*, not because nothing should have.
///
/// Two internal consistencies worth naming, because they are what distinguishes
/// "the affinity arrived" from "some number changed":
///
/// - **A `1.00` stronghold leaves `mean_k` alone and moves only the share.**
///   `gnoll,desert` and `woolly-mammoth,ice` carry the identical `mean_k`,
///   `p50_k` and `p95_k` before and after, while their `share_of_kind_k` rises
///   (gnoll's desert share `0.0052262188 → 0.017680314`). The factor there is
///   1.0, so the kind's absolute capacity on that ground is untouched; what
///   changed is that every *other* biome was scaled down beneath it. **This one
///   holds through the derivation as well**, which is the stronger statement:
///   those three columns are still byte-identical in the live file
///   (`gnoll,desert` `mean_k 0.0036231586`, `woolly-mammoth,ice`
///   `0.00065375959`, `woolly-mammoth,tundra` `0.012307025`), because a
///   stronghold maps to exactly `1.0` under the derived level too.
/// - **Every non-stronghold share moves by the ratio of its factor to 0.25.**
///   `gnoll,shrubland` (0.70) and `gnoll,savanna` (0.45) rise; the eight biomes
///   taking the 0.25 default fall together, by an identical 0.846× (0.845751,
///   recomputed row by row across that fixture pair), and hold their order
///   exactly — `temperate-forest / tropical-seasonal-forest` reads 1.0773 both
///   before and after, to five figures. **The same test applied to the
///   derivation passes with different constants**, which is what a relevel
///   should look like: from `834fee5c` to the live file gnoll's eight default
///   biomes all rise by an identical 1.092317×, desert (the stronghold) falls to
///   0.551248×, shrubland and temperate-grassland (the `near` step) to
///   0.668282×, savanna (the `marginal` step) to 0.885011× — four distinct
///   ratios for four ladder rungs and no fifth — and the `temperate-forest /
///   tropical-seasonal-forest` ratio is *still* 1.0773 to five figures, in all
///   three fixtures.
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
///   falls 83081 → 5586 occupied vertices.)
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
/// fixture as it stands after this commit's regeneration**, not carried forward;
/// the prior version of each is kept beside it, because the pattern of how they
/// went stale is the more useful thing.
///
/// - *Presence, still not dominance.* A people authored explicitly for hot-arid
///   desert once had **zero** desert occupancy. It holds **5498** desert vertices
///   now — a figure unmoved by either 2026-08-10 regeneration, since the
///   viability floor is far below every factor in play. Declaring its affinity
///   lifted desert's share of its world total K from **0.0052262188** (the
///   pre-affinity fixture, `d7719e27`) to **0.0097462358** in the live file, a
///   1.86× rise (1.8648733; the paragraph read "1.87×" until this fix round
///   divided it out — every other figure in this bullet is exact to the digit,
///   which is what made the one rounded-the-wrong-way ratio worth catching. All
///   of them were re-divided here, not just the one reported: the companion
///   3.38× below is 3.3830030 and stands). It is still not dominance: desert is gnoll's
///   **second-smallest share of the twelve biomes it reaches** (only `ice`, at
///   0.0070751272, is lower), and the region's top occupant by `mean_k` is
///   `otyugh` (0.04704871), then `carrion-crawler` and `shrieker`, with gnoll at
///   **0.0036231586** — rank 20 of the region's 35 kinds, an order of magnitude
///   below. Desert is gnoll's **9th** biome by `mean_k`, a different column; the
///   two rankings disagree and both are quoted here rather than one.
///
///   *What the version written one commit ago said:* share "0.017680314 — a
///   3.38× rise, the single largest proportional move in the file", desert
///   "third-smallest … (only `ice` and `alpine` are lower)", and "8th biome by
///   `mean_k`". All three were true of the `0.25` arm and all three moved when
///   the level was derived: the mask is shallower now, so gnoll's desert share
///   **fell** from the `0.25` arm's 0.017680314 even while remaining well above
///   its pre-affinity value, `alpine` (0.018064616) rose past it, and the
///   `mean_k` ordering shifted by one place. A number can go stale by the world
///   improving.
///
///   *What a still earlier version said:* "3793 desert vertices", share "0.0097",
///   "smallest share of any biome it reaches — 11th of 11", and
///   "`giant-scorpion` still tops the region". All four were true of a fixture
///   four regenerations back. **Note the trap in the second of them**: that
///   long-dead "0.0097" and the live 0.0097462358 agree to two significant
///   figures by coincidence — a pre-affinity world and a derived-level world
///   happening to cross — and a reader checking the old prose against the
///   current file to two places would conclude it had been right all along. It
///   was not; it was measured on a fixture in which gnoll had no affinity row at
///   all. Quote the digits the file carries, and say which file. `giant-scorpion` has not topped desert since The
///   Assize — the table at the head of this doc already said `otyugh` did, in
///   the same comment, four paragraphs up.
/// - *"Its largest share is temperate-forest" was right, and the correction of
///   it was wrong.* A previous version of this bullet called that claim "never
///   right" and asserted the largest share was **tropical-seasonal-forest
///   (0.3006)**. That was true when written — `75a0f450`'s fixture reads
///   `tropical-seasonal-forest 0.3005768` against `temperate-forest 0.27692454`
///   — but The Tense's regeneration (`979508f8`) reversed the pair, to
///   `temperate-forest 0.25118309` over `tropical-seasonal-forest 0.23314924`,
///   and the prose was not swept. After the affinity landed both fell by the
///   same default and the order was unchanged (`temperate-forest 0.21243829`
///   over `tropical-seasonal-forest 0.19718615` at `834fee5c`); after the level
///   was derived both rose by the same default and the order is *still*
///   unchanged: **`temperate-forest` (0.23204999)**, then
///   `tropical-seasonal-forest` (0.21538981) in the live file. The pair has now
///   survived two relevelings with its ratio fixed at 1.0773, which is the point
///   — a uniform factor cannot reorder a kind's own biomes, only rescale them.
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
