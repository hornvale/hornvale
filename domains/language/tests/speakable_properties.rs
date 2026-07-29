//! The Speakable's structural battery (spec §6): across a 64-seed sweep
//! of drawn phonologies, glossed names audibly contain the words their
//! gloss claims, and per-salt settlement names stay distinct.

use hornvale_kernel::Seed;
use hornvale_language::{
    CascadeRegime, Envelope, ExoticSeg, ExposureClass, LexEntry, MorphOptions, NameCorpus,
    NameKind, Namer, Phonology, SiteConcepts, build_lexicon, draw_phonology, render_views,
};
use std::collections::BTreeMap;

/// An envelope swept from seed bits so the 64-seed battery crosses the
/// full phonotactic regime space — including the cluster-heavy draws that
/// caused the collapse (spec §6). The same 5-line body as the module-level
/// battery in `naming.rs`: small enough that duplication beats exporting a
/// test-only helper across the crate boundary.
fn swept_envelope(seed: u64) -> Envelope {
    let f = |k: u64| ((seed >> k) & 3) as f64 / 3.0;
    Envelope {
        labiality: f(0),
        vowel_space: (f(2)).max(0.2),
        voicing: f(4),
        // Bits 6/8 are always zero for seed < 64, so the last two dims
        // reuse overlapping windows — correlated with the others, but
        // genuinely varying (a coverage sweep needs variation, not
        // independence).
        sibilance: f(1),
        voice_loudness: f(3),
        tonality: 0.0,
        exotic: ExoticSeg::None,
    }
}

/// Every reflex a glossed morpheme may legitimately surface as: its
/// citation form, and the two images the positional reduction (The
/// Wearing, Task 9) can leave — the word-initial one, whose stressed first
/// nucleus is spared, and the unstressed one, where every nucleus falls to
/// the shortest length `nucleus_floor` the language admits.
///
/// Written as **string surgery on the rendered citation form**, not by
/// calling the reduction: a romanized vowel is exactly one of `aeiou` plus
/// an optional combining tone mark, so a maximal run of them in the string
/// is exactly one nucleus, and this can be checked without sharing a line
/// of code with what it is checking. `nucleus_floor` comes from the drawn
/// `Phonology`'s own `nuclei` set, which is data, not machinery.
///
/// A near-twin lives in `cli/tests/branches_identity.rs` (same rule, floor
/// fixed at 1 because a generated world's phonologies always admit a single
/// vowel). The duplication is deliberate — a shared helper would have to be
/// exported from the crate under test, and independence from the production
/// rule is the whole point — but the two must not drift, and they are on
/// Task 11's sweep list for that reason.
fn admissible_reflexes(citation: &str, nucleus_floor: usize) -> Vec<String> {
    let shorten = |spare_first: bool| {
        let mut out = String::new();
        let mut kept = 0usize;
        let mut in_run = false;
        let mut spared = spare_first;
        // Whether the most recent vowel was emitted, so a combining mark
        // after it can follow it or be dropped with it.
        let mut emitted_last = false;
        for c in citation.chars() {
            let vowel = "aeiou".contains(c);
            if !vowel && !c.is_ascii_alphabetic() && in_run {
                // A combining tone mark belongs to the vowel before it — so
                // it is emitted only if THAT vowel was kept. Pushing it
                // whenever anything in the run survived would move a dropped
                // vowel's mark onto a kept one and build a reflex the rule
                // never produces (inert here, since this sweep's envelope is
                // atonal, and it would cost a false failure rather than a
                // false pass — but the doc above says the mark stays with
                // its own vowel, so the code has to).
                if emitted_last {
                    out.push(c);
                }
                continue;
            }
            if !vowel {
                if in_run {
                    spared = false;
                }
                in_run = false;
                out.push(c);
                continue;
            }
            if !in_run {
                in_run = true;
                kept = 0;
            }
            kept += 1;
            emitted_last = spared || kept <= nucleus_floor;
            if emitted_last {
                out.push(c);
            }
        }
        out
    };
    vec![citation.to_string(), shorten(true), shorten(false)]
}

/// A fixed, maximally permissive proto phonology — the family-level draw
/// source every swept daughter phonology below diverges from, so the
/// lexicon always descends from a DIFFERENT proto phonology than the
/// daughter's own drawn one (the exact mismatch that caused the collapse).
fn permissive_proto() -> Phonology {
    draw_phonology(
        &Seed(37),
        "proto",
        &Envelope {
            labiality: 1.0,
            vowel_space: 1.0,
            voicing: 1.0,
            sibilance: 1.0,
            voice_loudness: 1.0,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        },
    )
}

/// The Speakable's §6 audible-containment property, **armed with a real
/// name corpus** — i.e. exercised under toponymic wear, the feature that
/// can break it.
///
/// The unworn sweep below is not enough on its own, and shipping only that
/// was a real defect: wear breaks attestedness, `repair_phonotactics` is
/// the identity only for attested words, and a worn form that no template
/// can host is DELETED. Nine seed-42 settlements committed a `name-gloss`
/// naming a morpheme their name did not contain before the survival rule
/// existed. This is the test that pins the fix — with every concept driven
/// to a corpus share of 1.0, so wear is attempted on every morpheme of
/// every name, across the same 64-seed sweep.
///
/// The property asserted is deliberately the ORIGINAL one: the name must
/// audibly contain each glossed concept's own word. Wear may reduce a
/// morpheme, so a worn name is allowed to contain the **worn** reflex
/// instead of the citation form — but it must contain one of them. It may
/// never contain neither, which is what erasure looks like.
///
/// **How this sweep absorbed The Wearing's positional reduction, and how
/// that differs from its sibling.** The unworn sweep below was widened
/// deliberately, with [`admissible_reflexes`] — a helper that derives the
/// admissible set by string surgery on the rendered citation and shares no
/// code with the rule it admits. This one was **not edited at all**. It
/// widened implicitly, because its `worn_form` is
/// `namer.wear(&derivation.modern, 1.0)` — production code, which now runs
/// the reduction. So half of this test's acceptance set is computed by the
/// very machinery it checks, and it would not catch a reduction that was
/// wrong in the same way on both sides. Recorded rather than repaired: the
/// erasure it exists to catch is a *disagreement* between the name and both
/// reflexes, and the disarming check confirms it still fires — it reds under
/// a reduction mutation as well as under a containment-guard stub. The
/// independent check on the reduction itself is the unworn sweep and the
/// unit battery in `naming.rs`, not this one.
#[test]
fn glossed_names_audibly_contain_their_words_under_a_saturated_corpus() {
    let mut worn_names = 0usize;
    let mut checked = 0usize;
    for seed in 0..64u64 {
        let proto = permissive_proto();
        let ph = draw_phonology(&Seed(seed), "swept", &swept_envelope(seed));
        let mut exposures = BTreeMap::new();
        for c in ["water", "fire", "moon", "shadow"] {
            exposures.insert(c.to_string(), ExposureClass::Steeped);
        }
        let lex = build_lexicon(
            &Seed(seed),
            "fam",
            "swept",
            &ph,
            &proto,
            &exposures,
            &[],
            CascadeRegime::SETTLED,
        );
        let namer = Namer::new(&Seed(seed), "swept", &ph);
        let site = SiteConcepts {
            concepts: &["water", "fire", "moon", "shadow"],
        };
        // An even three-way shape profile: this battery is about
        // phonotactic containment, not shape, so every NameShape is drawn
        // equally often and the sweep covers one-, two- and
        // three-morpheme compounds alike.
        let morph = MorphOptions {
            honorifics: false,
            shape_weights: [1.0, 1.0, 1.0],
            shape_beta: 1.0,
        };
        // Every concept in every name of this culture: the maximum wear
        // pressure the mechanism can ever be under.
        let frequencies: BTreeMap<String, f64> = ["water", "fire", "moon", "shadow"]
            .iter()
            .map(|c| (c.to_string(), 1.0))
            .collect();
        let corpus = NameCorpus {
            frequencies: &frequencies,
        };

        for kind in [NameKind::Settlement, NameKind::Deity] {
            for salt in 0..6u64 {
                let (name, gloss) = namer.glossed_name(kind, salt, &morph, &site, &lex, &corpus);
                let (plain, _) =
                    namer.glossed_name(kind, salt, &morph, &site, &lex, &NameCorpus::none());
                if name.roman != plain.roman {
                    worn_names += 1;
                }
                for concept in gloss.split('-').filter(|c| !c.is_empty()) {
                    let citation = match lex.entry(concept) {
                        Some(LexEntry::Root { derivation, .. }) => {
                            render_views(&derivation.modern).roman.to_lowercase()
                        }
                        other => panic!(
                            "seed {seed} salt {salt} {kind:?}: gloss concept {concept} \
                             must be a root, got {other:?}"
                        ),
                    };
                    let worn_form = match lex.entry(concept) {
                        Some(LexEntry::Root { derivation, .. }) => {
                            render_views(&namer.wear(&derivation.modern, 1.0))
                                .roman
                                .to_lowercase()
                        }
                        _ => unreachable!("checked above"),
                    };
                    let surface = name.roman.to_lowercase();
                    checked += 1;
                    assert!(
                        surface.contains(&citation) || surface.contains(&worn_form),
                        "seed {seed} salt {salt} {kind:?}: name {:?} contains NEITHER \
                         {concept}'s citation form {citation:?} nor its worn reflex \
                         {worn_form:?} — the gloss names a morpheme the name does not say",
                        name.roman
                    );
                }
            }
        }
    }
    assert!(
        checked > 0,
        "non-vacuity: the sweep must have checked some glossed morpheme"
    );
    assert!(
        worn_names > 0,
        "non-vacuity: a saturated corpus must have actually worn some name, \
         or this sweep is the unworn one again under a different name"
    );
}

#[test]
fn glossed_names_audibly_contain_their_words_across_the_seed_sweep() {
    for seed in 0..64u64 {
        let proto = permissive_proto();
        let ph = draw_phonology(&Seed(seed), "swept", &swept_envelope(seed));
        let mut exposures = BTreeMap::new();
        for c in ["water", "fire", "moon", "shadow"] {
            exposures.insert(c.to_string(), ExposureClass::Steeped);
        }
        // ph is the daughter's own drawn phonology (the evolution target
        // and the phonology names are drawn under); proto is the DIFFERENT
        // permissive family-level proto phonology roots are drawn from.
        let lex = build_lexicon(
            &Seed(seed),
            "fam",
            "swept",
            &ph,
            &proto,
            &exposures,
            &[],
            CascadeRegime::SETTLED,
        );
        let namer = Namer::new(&Seed(seed), "swept", &ph);
        let site = SiteConcepts {
            concepts: &["water", "fire", "moon", "shadow"],
        };
        // An even three-way shape profile: this battery is about
        // phonotactic containment, not shape, so every NameShape is drawn
        // equally often and the sweep covers one-, two- and
        // three-morpheme compounds alike.
        let morph = MorphOptions {
            honorifics: false,
            shape_weights: [1.0, 1.0, 1.0],
            shape_beta: 1.0,
        };

        // The per-salt distinctness this loop used to assert is gone with
        // the drawn settlement stem (The Wearing, decision 0024: uniqueness
        // is a reference-time property and no future work fixes the
        // collision rate by adding entropy). Two settlements over the same
        // site concepts now legitimately share a name. The property this
        // test exists for — The Speakable's audible containment — is
        // unaffected and still checked on every salt.
        for kind in [NameKind::Settlement, NameKind::Deity] {
            for salt in 0..6u64 {
                let (name, gloss) =
                    namer.glossed_name(kind, salt, &morph, &site, &lex, &NameCorpus::none());
                for concept in gloss.split('-').filter(|c| !c.is_empty()) {
                    let word = match lex.entry(concept) {
                        Some(LexEntry::Root { derivation, .. }) => {
                            render_views(&derivation.modern).roman.to_lowercase()
                        }
                        other => panic!(
                            "seed {seed} salt {salt} {kind:?}: gloss concept {concept} \
                             must be a root, got {other:?}"
                        ),
                    };
                    // The citation form, or one of the two images the
                    // positional reduction leaves. Widened by Task 9: the
                    // property is still "the name says the morpheme", but a
                    // reduced nucleus is a reflex, not a loss — the same
                    // widening Task 6 made for the worn reflex.
                    let floor = ph.nuclei.iter().copied().min().unwrap_or(1);
                    let reflexes = admissible_reflexes(&word, floor);
                    assert!(
                        reflexes
                            .iter()
                            .any(|r| name.roman.to_lowercase().contains(r)),
                        "seed {seed} salt {salt} {kind:?}: name {:?} must audibly \
                         contain {concept} = {word:?} or one of its reduced reflexes \
                         {reflexes:?}",
                        name.roman
                    );
                }
            }
        }
    }
}
