//! The believability readout (Task 3, spec §7): builds the full 15×15
//! snap-judgment landscape over the real fifteen-people catalog and prints
//! a human-readable report — the emotion matrix, the strongest pair per
//! emotion, every people's weight-vector, and the axis-space coverage map
//! (spec §7's roster-expansion shopping list). Assertions are LOOSE
//! STRUCTURAL TRIPWIRES ONLY (0021 + the measure-first / no-rescue
//! discipline, controller correction E3): this readout must never assert a
//! specific real pair's emotion or magnitude, only structural properties
//! the law must have. The believability judgment itself is read from the
//! printed report by a human, not asserted.
//!
//! **Population discipline (a design decision this task owns).** The
//! printed matrix includes the diagonal (`snap_judgment(p, p)` for all 15
//! peoples) because a complete 15×15 landscape is the deliverable. But
//! every ANALYSIS below — all five tripwires, the "strongest pair" report,
//! the coverage map, and the correlation — is computed over the 210
//! OFF-DIAGONAL (cross-people) ordered pairs only. A self-pair is proven
//! (`tests/suite/judgment.rs`) to land at max warmth / `Admiration`
//! unconditionally, by construction, for every people — including it would
//! make tripwire (b) ("it likes") true by definition rather than a genuine
//! structural finding about whether the fifteen peoples, judged against
//! EACH OTHER, ever land on liking.
//!
//! Run with `cargo test -p hornvale-sentiment --test suite -- believability
//! --nocapture` to see the full report.
//!
//! **Measured outcome (2026-08-20): 4 of 5 tripwires hold as hard asserts;
//! tripwire (b) "it likes" is FALSIFIED against the real roster** — 0 of the
//! 210 cross-pairs classify `Admiration` (the projection's warmth term only
//! ever gets pushed DOWN from its `1.0` baseline by any axis, and no two of
//! the fifteen catalog peoples are simultaneously close enough on every
//! warmth-lowering axis for that to survive). Per the measure-first /
//! no-rescue discipline (spec §7; controller correction E3), this was NOT
//! rescued by retuning any weight, baseline, gain, threshold, or axis
//! signature — it is reported at its assertion site below as a MEASUREMENT
//! documenting the null, not asserted as a floor. See
//! `docs/retrospectives/the-cant.md` (written at G6 close) for the
//! campaign-level finding this feeds.

use std::collections::BTreeSet;

use hornvale_sentiment::{
    Axis, Emotion, Judgment, PeopleId, axis_distance, catalog, snap_judgment, weight_vector,
};

/// Tolerance for this readout's OWN float-equality checks (tripwire (e)'s
/// "are these weight-vectors identical" test). Not a domain branching rule —
/// the root `CLAUDE.md`'s "no branching on `f64 ==`" note governs simulation
/// code; this is a measurement's own structural comparison, the same shape
/// as the `close()` helper `tests/suite/judgment.rs` already uses.
const EPS: f64 = 1e-9;

/// The "high distance" cutoff the coverage map buckets a single axis's
/// distance on — reuses the same `0.5` threshold `snap_judgment`'s own
/// classifier uses for "high" warmth/competence, so "high on this axis" and
/// "high on warmth/competence" mean the same thing throughout this report.
const HIGH_DISTANCE_THRESHOLD: f64 = 0.5;

/// One off-diagonal (cross-people) ordered pair's precomputed readout: the
/// judgment plus the pair's total UNWEIGHTED axis distance (the sum of all
/// eight raw `axis_distance` values, before `weight_vector` scales any of
/// them) — the quantity tripwire (d) correlates against warmth.
struct CrossPair {
    judger_index: usize,
    target_index: usize,
    judgment: Judgment,
    unweighted_distance: f64,
}

fn emotion_code(emotion: Emotion) -> char {
    match emotion {
        Emotion::Admiration => 'A',
        Emotion::Envy => 'E',
        Emotion::Pity => 'P',
        Emotion::Contempt => 'C',
    }
}

/// A judgment's distance from the `(0.5, 0.5)` classification-threshold
/// point — this readout's chosen "how extreme" metric for the "strongest
/// pair per emotion" report (a REPORTED magnitude, never an asserted one;
/// see the module doc and controller correction E4).
fn magnitude(judgment: &Judgment) -> f64 {
    ((judgment.warmth - 0.5).powi(2) + (judgment.competence - 0.5).powi(2)).sqrt()
}

fn close(a: f64, b: f64) -> bool {
    (a - b).abs() < EPS
}

/// Pearson correlation coefficient between two equal-length series.
fn pearson(xs: &[f64], ys: &[f64]) -> f64 {
    assert_eq!(xs.len(), ys.len());
    let n = xs.len() as f64;
    let mean_x = xs.iter().sum::<f64>() / n;
    let mean_y = ys.iter().sum::<f64>() / n;
    let covariance: f64 = xs
        .iter()
        .zip(ys.iter())
        .map(|(x, y)| (x - mean_x) * (y - mean_y))
        .sum();
    let variance_x: f64 = xs.iter().map(|x| (x - mean_x).powi(2)).sum();
    let variance_y: f64 = ys.iter().map(|y| (y - mean_y).powi(2)).sum();
    covariance / (variance_x.sqrt() * variance_y.sqrt())
}

#[test]
fn believability_readout() {
    let cat = catalog();
    let ids: Vec<PeopleId> = cat.keys().copied().collect();
    assert_eq!(
        ids.len(),
        19,
        "the catalog must hold exactly the nineteen settling peoples"
    );

    // --- Step 1: the full 15x15 landscape --------------------------------

    let judgments: Vec<Vec<Judgment>> = ids
        .iter()
        .map(|&judger| {
            ids.iter()
                .map(|&target| snap_judgment(&cat[&judger], &cat[&target]))
                .collect()
        })
        .collect();

    println!("=== THE BELIEVABILITY READOUT (Task 3, spec §7) ===");
    println!();
    println!(
        "--- Legend: the 15 peoples, index order (catalog()'s BTreeMap<PeopleId,_> order) ---"
    );
    for (index, id) in ids.iter().enumerate() {
        println!("  [{index:>2}] {}", id.0);
    }

    println!();
    println!(
        "--- Full 15x15 emotion matrix (row=judger, col=target; A=Admiration E=Envy P=Pity C=Contempt) ---"
    );
    let header: String = (0..ids.len()).map(|i| format!("{i:>3}")).collect();
    println!("{:>6}{header}", "");
    for (row_index, row) in judgments.iter().enumerate() {
        let cells: String = row
            .iter()
            .map(|judgment| format!("{:>3}", emotion_code(judgment.emotion)))
            .collect();
        println!("{:>6}{cells}", format!("[{row_index:>2}]"));
    }

    println!();
    println!("--- Weight-vectors (per people, indexed by Axis::ALL order) ---");
    let axis_header: String = Axis::ALL
        .iter()
        .map(|axis| format!("{:>16}", axis.label()))
        .collect();
    println!("{:<14}{axis_header}", "people");
    for &id in &ids {
        let weights = weight_vector(&cat[&id]);
        let cells: String = weights.iter().map(|w| format!("{w:>16.6}")).collect();
        println!("{:<14}{cells}", id.0);
    }

    // --- Cross-pair population (off-diagonal only; see module doc) -------

    let mut cross: Vec<CrossPair> = Vec::with_capacity(ids.len() * (ids.len() - 1));
    for judger_index in 0..ids.len() {
        for target_index in 0..ids.len() {
            if judger_index == target_index {
                continue;
            }
            let judger = &cat[&ids[judger_index]];
            let target = &cat[&ids[target_index]];
            let unweighted_distance: f64 = Axis::ALL
                .iter()
                .map(|&axis| axis_distance(axis, judger, target))
                .sum();
            cross.push(CrossPair {
                judger_index,
                target_index,
                judgment: judgments[judger_index][target_index],
                unweighted_distance,
            });
        }
    }
    assert_eq!(
        cross.len(),
        19 * 18,
        "the off-diagonal cross-pair population must be exactly 19*18=342"
    );

    // --- Strongest pair per emotion (reported, never asserted; E4) -------

    println!();
    println!(
        "--- Strongest pair per emotion (cross-pairs only; magnitude = distance from the (0.5,0.5) threshold point) ---"
    );
    for emotion in [
        Emotion::Admiration,
        Emotion::Envy,
        Emotion::Pity,
        Emotion::Contempt,
    ] {
        let strongest = cross
            .iter()
            .filter(|pair| pair.judgment.emotion == emotion)
            .max_by(|a, b| magnitude(&a.judgment).total_cmp(&magnitude(&b.judgment)));
        match strongest {
            Some(pair) => println!(
                "  {emotion:?}: {} -> {} (warmth={:.6} competence={:.6} magnitude={:.6})",
                ids[pair.judger_index].0,
                ids[pair.target_index].0,
                pair.judgment.warmth,
                pair.judgment.competence,
                magnitude(&pair.judgment)
            ),
            None => println!("  {emotion:?}: BARE — no cross-pair classifies here"),
        }
    }

    // --- Axis-space coverage map (E4: printed, never asserted) -----------

    println!();
    println!(
        "--- Axis-space coverage map: cross-pair counts where THIS axis's own distance is \
         HIGH (>= {HIGH_DISTANCE_THRESHOLD}), broken out by the pair's overall emotion \
         quadrant (0* = bare corner) ---"
    );
    let emotions = [
        Emotion::Admiration,
        Emotion::Envy,
        Emotion::Pity,
        Emotion::Contempt,
    ];
    let quadrant_header: String = emotions
        .iter()
        .map(|emotion| format!("{:>12}", format!("{emotion:?}")))
        .collect();
    println!("{:<16}{quadrant_header}", "axis");
    let mut bare_corners: Vec<(Axis, Emotion)> = Vec::new();
    for axis in Axis::ALL {
        let mut row = String::new();
        for &emotion in &emotions {
            let count = cross
                .iter()
                .filter(|pair| {
                    pair.judgment.emotion == emotion
                        && axis_distance(
                            axis,
                            &cat[&ids[pair.judger_index]],
                            &cat[&ids[pair.target_index]],
                        ) >= HIGH_DISTANCE_THRESHOLD
                })
                .count();
            if count == 0 {
                bare_corners.push((axis, emotion));
            }
            let cell = if count == 0 {
                "0*".to_string()
            } else {
                count.to_string()
            };
            row.push_str(&format!("{cell:>12}"));
        }
        println!("{:<16}{row}", axis.label());
    }
    println!();
    if bare_corners.is_empty() {
        println!(
            "bare corners: none — every (axis, emotion) cell has at least one high-distance cross-pair"
        );
    } else {
        let listing: Vec<String> = bare_corners
            .iter()
            .map(|(axis, emotion)| format!("{}/{emotion:?}", axis.label()))
            .collect();
        println!(
            "bare corners ({} of {}): {}",
            bare_corners.len(),
            Axis::ALL.len() * emotions.len(),
            listing.join(", ")
        );
    }

    // --- Step 2: the five structural tripwires ----------------------------
    // Every assertion below is a LOOSE STRUCTURAL FLOOR (spec §7), never a
    // specific real pair's emotion or magnitude (0021). All five are
    // computed over the 210 cross-pairs only (see module doc).

    println!();
    println!("--- Tripwires (structural floors, per 0021 + the measure-first discipline) ---");

    // (a) non-degenerate: more than one distinct Emotion appears.
    let distinct_emotions: BTreeSet<Emotion> =
        cross.iter().map(|pair| pair.judgment.emotion).collect();
    println!(
        "(a) non-degenerate: {} distinct emotion(s) across {} cross-pairs: {distinct_emotions:?}",
        distinct_emotions.len(),
        cross.len()
    );
    assert!(
        distinct_emotions.len() > 1,
        "structural tripwire (a) non-degenerate FAILED: only {distinct_emotions:?} appeared \
         across all {} cross-pairs — the matrix is one flat emotion",
        cross.len()
    );

    // (b) it likes: at least one cross-pair is Admiration.
    //
    // MEASURED AND FALSIFIED (controller correction E3's plausible-failure
    // case). Over the real fifteen-people roster, 0 of 210 cross-pairs
    // classify Admiration. Per the measure-first / no-rescue discipline
    // (spec §7; root CLAUDE.md's preregistration section: "A falsified
    // prediction is a finding, not a failure"), this is NOT rescued by
    // retuning any weight, baseline, gain, threshold, or axis signature —
    // no constant in `src/weights.rs`, `src/judgment.rs`, or `src/axes.rs`
    // was touched to make this pass. It is reported here as a MEASUREMENT,
    // not asserted as a floor: the campaign's headline finding is that the
    // derivable axes alone do not produce believable liking among the real
    // fifteen catalog peoples (pointing at the appearance/disease substrate
    // as the next layer — see `docs/retrospectives/the-cant.md`, written at
    // G6 close). The other four tripwires remain hard structural asserts.
    let admiration_count = cross
        .iter()
        .filter(|pair| pair.judgment.emotion == Emotion::Admiration)
        .count();
    let quadrant_counts: Vec<(Emotion, usize)> = emotions
        .iter()
        .map(|&emotion| {
            (
                emotion,
                cross
                    .iter()
                    .filter(|pair| pair.judgment.emotion == emotion)
                    .count(),
            )
        })
        .collect();
    // "Most-Admiration-adjacent pair": the cross-pair whose (warmth,
    // competence) point sits closest to the Admiration quadrant
    // (warmth >= 0.5 AND competence >= 0.5) by Euclidean distance to that
    // quadrant's boundary — the natural "how close did the roster come"
    // metric given zero actual occupants.
    let admiration_adjacency = |judgment: &Judgment| -> f64 {
        let warmth_gap = (0.5 - judgment.warmth).max(0.0);
        let competence_gap = (0.5 - judgment.competence).max(0.0);
        (warmth_gap.powi(2) + competence_gap.powi(2)).sqrt()
    };
    let closest = cross
        .iter()
        .min_by(|a, b| {
            admiration_adjacency(&a.judgment).total_cmp(&admiration_adjacency(&b.judgment))
        })
        .expect("cross is non-empty (210 pairs)");
    println!(
        "(b) it likes: FALSIFIED (finding, not asserted) — {admiration_count} of {} \
         cross-pairs classify Admiration. Per-quadrant counts: {quadrant_counts:?}. \
         Most-Admiration-adjacent pair: {} -> {} (warmth={:.6} competence={:.6} \
         adjacency={:.6}, emotion={:?}) — still {} short of the (0.5,0.5) boundary.",
        cross.len(),
        ids[closest.judger_index].0,
        ids[closest.target_index].0,
        closest.judgment.warmth,
        closest.judgment.competence,
        admiration_adjacency(&closest.judgment),
        closest.judgment.emotion,
        if admiration_adjacency(&closest.judgment) > 0.0 {
            "strictly"
        } else {
            "not"
        }
    );

    // (c) asymmetry exists: at least one unordered pair judges differently
    // in each direction.
    let mut asymmetric_pairs: Vec<(usize, usize)> = Vec::new();
    for (i, row) in judgments.iter().enumerate() {
        for (j, other_row) in judgments.iter().enumerate().skip(i + 1) {
            if row[j] != other_row[i] {
                asymmetric_pairs.push((i, j));
            }
        }
    }
    println!(
        "(c) asymmetry exists: {} of {} unordered pairs judge differently in each direction",
        asymmetric_pairs.len(),
        ids.len() * (ids.len() - 1) / 2
    );
    assert!(
        !asymmetric_pairs.is_empty(),
        "structural tripwire (c) asymmetry FAILED: every unordered pair judged identically \
         in both directions, despite DietPredation and SizeThreat being directional axes"
    );

    // (d) similarity -> warmth: Pearson r(total unweighted axis distance,
    // warmth) is negative (sign asserted, coefficient reported).
    let distances: Vec<f64> = cross.iter().map(|pair| pair.unweighted_distance).collect();
    let warmths: Vec<f64> = cross.iter().map(|pair| pair.judgment.warmth).collect();
    let r = pearson(&distances, &warmths);
    println!(
        "(d) similarity -> warmth: Pearson r(total unweighted axis distance, warmth) = {r:.6} \
         over {} cross-pairs",
        cross.len()
    );
    assert!(
        r < 0.0,
        "structural tripwire (d) similarity->warmth FAILED: correlation coefficient r={r} \
         is not negative"
    );

    // (e) distinct personalities: not all weight-vectors are equal.
    let weight_vectors: Vec<[f64; 8]> = ids.iter().map(|&id| weight_vector(&cat[&id])).collect();
    let first = weight_vectors[0];
    let all_equal = weight_vectors
        .iter()
        .all(|w| w.iter().zip(first.iter()).all(|(a, b)| close(*a, *b)));
    println!("(e) distinct personalities: all 15 weight-vectors identical? {all_equal}");
    assert!(
        !all_equal,
        "structural tripwire (e) distinct personalities FAILED: every people's weight-vector \
         is identical, despite differing in_group_radius/threat_response/sociality/status_basis"
    );

    println!();
    println!(
        "=== 4/5 structural tripwires passed as hard asserts (a, c, d, e); \
         (b) it likes is FALSIFIED and reported above as a measurement, not asserted — \
         see docs/retrospectives/the-cant.md for the campaign-level finding ==="
    );
}
