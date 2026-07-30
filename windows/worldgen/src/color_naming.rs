//! Naming a colour through a speaker's own lexicon.
//!
//! The sample and every candidate exemplar are pushed through the same
//! illuminant and the same observer before anything is compared. Because
//! exemplars are reflectances rather than finished colours, this works
//! unchanged for an observer with any channel count.
//!
//! **Two axes, not one — and this is a correction the spec did not have.**
//! The spec proposed "nearest exemplar in signal space." Task 6 measured
//! the seven exemplars and that design cannot work: raw signal distance is
//! dominated by brightness, so `brown` comes out nearest neighbour to four
//! of the seven terms and the whole dim corner {dark, red, brown, green,
//! blue} collapses inside d < 0.87 while `light` and `yellow` sit 2.1–5.2
//! away. A raw-distance namer would say "brown" for almost everything and
//! would essentially never say "light" or "yellow".
//!
//! Chromaticity alone fails the other way: `dark` and `light` have the
//! *same* neutral chromaticity by construction (0.0210 apart, the
//! numerical floor), so nothing chromatic can separate them.
//!
//! The fix was already in the repo's data model. `color_pack`'s hue ladder
//! puts `dark`/`light` at **rank 1** — Berlin & Kay's stage I is
//! achromatic, macro-black against macro-white — and `PackDepths` carries
//! `hue` and `luminance` as separate fields. Naming therefore decides on
//! the axis the term actually lives on: luminance for the achromatic pair,
//! chromaticity for the five hue terms. Collapsing both into one metric was
//! the mistake.
//!
//! The lexicon filter is `in_ladder`, unmodified. This module adds no gate
//! of its own — a gate at the point of use would change nothing, because
//! the lexicon has already filtered.

use hornvale_kernel::color::{Illuminant, Observer, Reflectance, Signal};
use hornvale_language::{PackDepths, color_pack, hue_exemplar, in_ladder};

/// The achromatic terms — Berlin & Kay's stage I, `color_pack`'s hue-ladder
/// rank 1. These are *luminance* terms: `dark` and `light` have the same
/// neutral chromaticity by construction (measured 0.0210 apart, which is the
/// numerical floor), so no chromaticity metric can ever separate them and
/// they are decided on brightness instead.
/// type-audit: bare-ok(identifier-text)
const ACHROMATIC: [&str; 2] = ["dark", "light"];

/// How far a sample's chromaticity must sit from neutral before it earns a
/// hue name rather than an achromatic one.
///
/// **Derived from measurement, not tuned to taste.** Task 6 sensed all seven
/// exemplars under Sol daylight and measured each chromatic exemplar's
/// chromaticity distance from neutral: green 0.0852, yellow 0.1558, brown
/// 0.1668, blue 0.2616, red 0.2701. Green is the closest, so half of green's
/// distance cleanly admits every authored hue term while still rejecting a
/// genuinely grey surface. Changing this number changes which surfaces get
/// hue names at all — treat it as a threshold with a stated derivation, and
/// re-derive it rather than nudging it if the exemplars ever move.
/// type-audit: bare-ok(ratio)
const ACHROMATIC_THRESHOLD: f64 = 0.04;

/// Chromaticity: the signal normalized to unit sum, which strips brightness
/// and leaves only the *proportion* between channels.
///
/// Returns `None` for a signal with no energy — a surface in total darkness
/// has no chromaticity, and inventing one would be a division by zero
/// dressed up as a colour.
fn chromaticity(signal: &Signal) -> Option<Vec<f64>> {
    let total = luminance(signal);
    if total <= 0.0 {
        return None;
    }
    Some(signal.get().iter().map(|v| v / total).collect())
}

/// Euclidean distance between two chromaticities, or `f64::INFINITY` if they
/// come from observers of different arity and are not comparable.
/// type-audit: bare-ok(ratio: return)
fn chromatic_distance(a: &[f64], b: &[f64]) -> f64 {
    if a.len() != b.len() {
        return f64::INFINITY;
    }
    let mut sum = 0.0;
    for (x, y) in a.iter().zip(b) {
        let d = x - y;
        sum += d * d;
    }
    sum.sqrt()
}

/// Total sensed energy — the brightness axis, used to split `dark` from
/// `light`.
/// type-audit: bare-ok(ratio: return)
fn luminance(signal: &Signal) -> f64 {
    let mut total = 0.0;
    for v in signal.get() {
        total += *v;
    }
    total
}

/// The word this speaker reaches for, given what it can see and what its
/// lexicon holds.
///
/// Ties break by ladder rank first (the earlier-acquired term wins, which
/// is what a shallower lexicon would have said anyway), then by concept id,
/// so the result is deterministic without depending on iteration order.
///
/// Every lexicon holds rank-1 terms (`dark` and `light` are the first stage
/// of the ladder), so there is always at least one candidate.
/// type-audit: bare-ok(identifier-text: return)
pub fn name_color(
    sample: &Reflectance,
    light: &Illuminant,
    observer: &Observer,
    depths: &PackDepths,
) -> &'static str {
    let seen = observer.sense(sample, light);

    // Partition the terms this speaker actually holds into the two axes the
    // ladder already distinguishes.
    let mut achromatic: Vec<&'static str> = Vec::new();
    let mut chromatic: Vec<&'static str> = Vec::new();
    for entry in color_pack() {
        if !in_ladder(entry, depths) || hue_exemplar(entry.concept).is_none() {
            // Not held, or a luminance-ladder term (gloom/shadow/starlit),
            // which describes ambient darkness rather than a surface.
            continue;
        }
        if ACHROMATIC.contains(&entry.concept) {
            achromatic.push(entry.concept);
        } else {
            chromatic.push(entry.concept);
        }
    }

    // The achromatic decision, used both as the fallback and as the answer
    // for a genuinely grey surface. Self-calibrating: the split sits at the
    // midpoint between the `dark` and `light` exemplars sensed under THIS
    // light, so it needs no absolute constant and moves correctly at dusk.
    let achromatic_answer = || -> &'static str {
        let held_dark = achromatic.contains(&"dark");
        let held_light = achromatic.contains(&"light");
        match (held_dark, held_light) {
            (true, false) => "dark",
            (false, true) => "light",
            _ => {
                let d = luminance(
                    &observer.sense(&hue_exemplar("dark").expect("dark is a hue term"), light),
                );
                let l = luminance(
                    &observer.sense(&hue_exemplar("light").expect("light is a hue term"), light),
                );
                if luminance(&seen) < (d + l) / 2.0 {
                    "dark"
                } else {
                    "light"
                }
            }
        }
    };

    if chromatic.is_empty() {
        return achromatic_answer();
    }

    let (Some(seen_chroma), Some(neutral)) = (
        chromaticity(&seen),
        chromaticity(&observer.sense(&hue_exemplar("dark").expect("dark is a hue term"), light)),
    ) else {
        // No energy at all: nothing is visible, so the honest answer is the
        // achromatic one rather than an invented hue.
        return achromatic_answer();
    };

    if chromatic_distance(&seen_chroma, &neutral) < ACHROMATIC_THRESHOLD {
        return achromatic_answer();
    }

    let mut best: Option<(&'static str, u8, f64)> = None;
    for concept in chromatic {
        let exemplar = hue_exemplar(concept).expect("chromatic terms have exemplars");
        let Some(exemplar_chroma) = chromaticity(&observer.sense(&exemplar, light)) else {
            continue;
        };
        let distance = chromatic_distance(&seen_chroma, &exemplar_chroma);
        let rank = color_pack()
            .iter()
            .find(|e| e.concept == concept)
            .map(|e| e.ladder_rank)
            .unwrap_or(u8::MAX);
        let candidate = (concept, rank, distance);
        best = Some(match best {
            None => candidate,
            Some(current) => {
                if is_better(candidate, current) {
                    candidate
                } else {
                    current
                }
            }
        });
    }

    best.map(|(concept, _, _)| concept)
        .unwrap_or_else(achromatic_answer)
}

/// Whether `candidate` beats `current`: nearer wins; on an exact tie the
/// lower ladder rank wins; on a further tie the lexicographically smaller
/// concept id wins. Distances are compared with `total_cmp`, never `<`, so
/// there is no NaN ambiguity.
fn is_better(candidate: (&'static str, u8, f64), current: (&'static str, u8, f64)) -> bool {
    match candidate.2.total_cmp(&current.2) {
        std::cmp::Ordering::Less => true,
        std::cmp::Ordering::Greater => false,
        std::cmp::Ordering::Equal => match candidate.1.cmp(&current.1) {
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Greater => false,
            std::cmp::Ordering::Equal => candidate.0 < current.0,
        },
    }
}
