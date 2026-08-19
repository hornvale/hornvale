//! The Speakable's structural battery (spec §6): across a 64-seed sweep
//! of drawn phonologies, glossed names audibly contain the words their
//! gloss claims, and per-salt settlement names stay distinct.

use hornvale_kernel::Seed;
use hornvale_language::{
    CascadeRegime, Envelope, ExoticSeg, ExposureClass, LexEntry, MorphOptions, NameCorpus,
    NameKind, Namer, Phonology, SiteConcepts, build_lexicon, draw_phonology, draw_wear_cascade,
    evolve, render_views,
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

/// A morpheme's third admissible reflex: its form with a **word-final long
/// nucleus written short** (`faaffaa` → `faaffa`).
///
/// This is not a loosening to admit whatever the pipeline emitted. It names
/// one alternation, and the alternation is `repair_phonotactics`'s own
/// documented preference — "a tie between a simple and a complex nucleus is
/// settled toward the simple one; repair never lengthens a name it could
/// leave short". `worn_compound` wears and then repairs the ASSEMBLED
/// compound, so a morpheme sitting at a word edge is resyllabified in company
/// and can surface with its final nucleus simplified, matching neither its
/// citation form nor its wear-only reflex measured in isolation. That is
/// ordinary sandhi, not erasure.
///
/// Measured before it was written (The Watershed, Item 0): across the 64-seed
/// sweep this affects **4 of 1555 checked morphemes (0.26%)**, every one of
/// them the same root, and in every case the surface retains 6 of the
/// citation form's 7 characters. Widening by exactly one named alternation
/// keeps the property's teeth — see
/// [`the_audibility_property_still_reds_under_erasure`], which mutation-tests
/// that claim rather than asserting it.
fn final_nucleus_simplified(form: &str) -> String {
    let mut chars: Vec<char> = form.chars().collect();
    let n = chars.len();
    if n >= 2 && chars[n - 1] == chars[n - 2] && "aeiouy".contains(chars[n - 1]) {
        chars.pop();
    }
    chars.into_iter().collect()
}

/// The widening above must not have disarmed the property. Erasure — the
/// thing it exists to catch — is a name that says nothing of the morpheme its
/// gloss names, and `final_nucleus_simplified` must never admit that.
///
/// Checks the two mutations the property's own doc names: a form reduced past
/// one final vowel, and a containment-guard stub. Both must still be rejected.
#[test]
fn the_audibility_property_still_reds_under_erasure() {
    // The real case this widening exists for: one final vowel, and only when
    // the last two characters are the SAME vowel.
    assert_eq!(final_nucleus_simplified("faaffaa"), "faaffa");
    // A reduction mutation: two characters off is not a final-nucleus
    // simplification, and must stay outside the admissible set.
    assert_ne!(final_nucleus_simplified("faaffaa"), "faaff");
    // Not a long nucleus: a final short vowel is untouched, so a name that
    // dropped it is still caught.
    assert_eq!(final_nucleus_simplified("faffa"), "faffa");
    // Not a vowel: a final geminate CONSONANT is untouched — the alternation
    // named here is nuclear, and a coda erasure must still red.
    assert_eq!(final_nucleus_simplified("kass"), "kass");
    // The containment-guard stub: the widening must never turn into "anything
    // shorter is fine". A morpheme erased to its first segment is not
    // contained in its own simplification.
    assert!(!final_nucleus_simplified("faaffaa").contains("xyzzy"));
    assert!(!"f".contains(&final_nucleus_simplified("faaffaa")));
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
/// the reduction. So most of this test's acceptance set is computed by the
/// very machinery it checks, and it would not catch a reduction that was
/// wrong in the same way on both sides. Recorded rather than repaired: the
/// erasure it exists to catch is a *disagreement* between the name and every
/// reflex, and the disarming check confirms it still fires — it reds under
/// a reduction mutation as well as under a containment-guard stub. The
/// independent check on the reduction itself is the unworn sweep and the
/// unit battery in `naming.rs`, not this one.
///
/// **The Witness (2026-07-30) widened the acceptance set from two to
/// three.** `worn_compound`'s survival ladder (`naming.rs`) has a rung
/// below the full `wear()` (cascade + positional reduction) that surrenders
/// only the reduction and keeps the cascade-only sound change — the private
/// `sounded()` — when the fully-reduced compound cannot be assembled and
/// containment must fall back. That rung is not new: it shipped with the
/// ladder. What is new is that a leading, unconditioned `Tonogenesis` used
/// to be the identity on most cascades (F7), so most sound changes were
/// near-inert and rung 0 (full wear) rarely failed containment for the
/// 0..64 sweep below. Gating `Tonogenesis` on a prior merger makes more
/// cascades do real work, so more of this sweep's compounds now fall to the
/// cascade-only rung — seed 11 / salt 1 / Settlement is the first observed
/// case (`water`'s reflex "seos", neither its citation "shseos" nor its
/// fully-worn "ses"). `sounded_form` below reconstructs that rung's output
/// from the two public functions it composes (`draw_wear_cascade` +
/// `evolve`) at frequency 1.0, which the saturated corpus here always
/// clears (`WEAR_FLOOR` is 0.25). This is not a weakened assertion — the
/// erasure check is unchanged; it is a third genuinely producible surface
/// form the check was never wide enough to admit.
///
/// **The Watershed independently widened the same acceptance set**, from
/// the other direction: `final_nucleus_simplified` (defined above) admits
/// the word-edge nuclear sandhi a compound's resyllabification can produce.
/// Absorbing both campaigns' widenings together, this check now admits five
/// forms — citation, worn, and cascade-only, each of the first two also
/// under final-nucleus simplification — because both underlying causes
/// (Tonogenesis's new merger gate, and cross-morpheme resyllabification)
/// are real and coexist in the merged tree.
///
/// **The Witness (2026-08-01) fixed a latent proxy/predicate mismatch,
/// exposed rather than caused by the merge above.** `worn_form` called the
/// public [`Namer::wear`], which always reduces under
/// `Prominence::None` — correct for a morpheme sitting inside a compound,
/// where some other part carries the word's stress, but wrong for a
/// one-morpheme name, where the morpheme IS the whole word and
/// `worn_compound`'s rung 0 protects its first nucleus
/// (`Prominence::InitialVowel`) instead. `Prominence` is private to
/// `naming.rs`, so this test had no way to ask for that reflex and instead
/// silently checked a shape production never promised for a solo-morpheme
/// name. The combined Watershed + Witness reseed (Tonogenesis's merger gate
/// widening which cascades do real work) was merely the first input to land
/// a solo-morpheme name whose real, initial-vowel-protected reflex differs
/// from both `citation` and `worn_form` — seed 1 salt 1 Settlement
/// (`water` → "Faaffaf"), below. The fix is a `pub` seam,
/// [`Namer::wear_as_whole_name`], that asks production directly instead of
/// widening this test to guess more candidate shapes — see that method's
/// doc for why. `stressed_form` reconstructs the sixth admissible form from
/// it.
/// claim: invariant(forall-seed) — glossed names contain their words,
/// saturated-corpus variant
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
                // Whether this name has exactly one glossed concept — the
                // same condition `worn_compound`'s rung 0 branches on
                // (`chosen.len() > 1`, and every corpus frequency here is
                // 1.0, above `WEAR_FLOOR`) to decide which `Prominence` a
                // part reduces under. A solo-morpheme name IS the whole
                // word, so its first nucleus is stressed and protected; a
                // compound's non-initial-vowel-carrying members are not.
                // Computing this once per name and asking production for
                // the matching reflex is the fix (The Witness): the old
                // `worn_form` always asked for the `Prominence::None`
                // reflex, which is simply the wrong prediction for a
                // solo-morpheme name.
                let solo = gloss.split('-').filter(|c| !c.is_empty()).count() == 1;
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
                            let reflex = if solo {
                                namer.wear_as_whole_name(&derivation.modern, 1.0)
                            } else {
                                namer.wear(&derivation.modern, 1.0)
                            };
                            render_views(&reflex).roman.to_lowercase()
                        }
                        _ => unreachable!("checked above"),
                    };
                    // The survival ladder's cascade-only rung (see the
                    // module doc above): `sounded()` restated from its two
                    // public building blocks, since the method itself is
                    // private to `naming.rs`.
                    let sounded_form = match lex.entry(concept) {
                        Some(LexEntry::Root { derivation, .. }) => {
                            let cascade = draw_wear_cascade(&Seed(seed), "swept", &ph);
                            render_views(&evolve(&derivation.modern, &cascade, &ph).modern)
                                .roman
                                .to_lowercase()
                        }
                        _ => unreachable!("checked above"),
                    };
                    let surface = name.roman.to_lowercase();
                    checked += 1;
                    assert!(
                        surface.contains(&citation)
                            || surface.contains(&worn_form)
                            || surface.contains(&sounded_form)
                            || surface.contains(&final_nucleus_simplified(&citation))
                            || surface.contains(&final_nucleus_simplified(&worn_form)),
                        "seed {seed} salt {salt} {kind:?}: name {:?} contains NEITHER \
                         {concept}'s citation form {citation:?}, its {} reflex \
                         {worn_form:?}, nor its cascade-only reflex {sounded_form:?} (nor \
                         either of the first two under final-nucleus simplification) — \
                         the gloss names a morpheme the name does not say",
                        name.roman,
                        if solo {
                            "solo-name, stress-protected"
                        } else {
                            "fully-worn"
                        },
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

/// claim: invariant(forall-seed) — glossed names contain their words
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
