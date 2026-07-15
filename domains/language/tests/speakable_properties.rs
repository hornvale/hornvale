//! The Speakable's structural battery (spec §6): across a 64-seed sweep
//! of drawn phonologies, glossed names audibly contain the words their
//! gloss claims, and per-salt settlement names stay distinct.

use hornvale_kernel::Seed;
use hornvale_language::{
    Envelope, ExoticSeg, ExposureClass, LexEntry, MorphOptions, NameKind, Namer, Phonology,
    SiteConcepts, build_lexicon, draw_phonology, render_views,
};
use std::collections::{BTreeMap, BTreeSet};

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
        let lex = build_lexicon(&Seed(seed), "fam", "swept", &ph, &proto, &exposures, &[]);
        let namer = Namer::new(&Seed(seed), "swept", &ph);
        let site = SiteConcepts {
            concepts: &["water", "fire", "moon", "shadow"],
        };
        let morph = MorphOptions { honorifics: false };

        for kind in [NameKind::Settlement, NameKind::Deity] {
            let mut settlement_names: BTreeSet<String> = BTreeSet::new();
            for salt in 0..6u64 {
                let (name, gloss) = namer.glossed_name(kind, salt, &morph, &site, &lex);
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
                    assert!(
                        name.roman.to_lowercase().contains(&word),
                        "seed {seed} salt {salt} {kind:?}: name {:?} must audibly \
                         contain {concept} = {word:?}",
                        name.roman
                    );
                }
                if kind == NameKind::Settlement {
                    settlement_names.insert(name.roman.clone());
                }
            }
            if kind == NameKind::Settlement {
                assert!(
                    settlement_names.len() >= 5,
                    "seed {seed}: settlement names over salts 0..6 must stay distinct \
                     (the per-salt stem keeps spreading them) — got {} distinct of 6: \
                     {settlement_names:?}",
                    settlement_names.len()
                );
            }
        }
    }
}
