//! Every `RuleKind` must be witnessed CHANGING a word.
//!
//! A rule that is drawn, applied, and never changes anything is
//! indistinguishable from a rule that works — `AppliedRule.changed` records
//! it, and nothing read that record. `Tonogenesis` was in that state for
//! every cascade that drew it before a merger (F7). This is the rule-shaped
//! twin of `domains/terrain/tests/hydro_witness.rs` (The Witness, Task 6):
//! it derives its checklist from [`RuleKind::ALL`] (the type itself), then
//! sweeps a small fixed seed set looking for a real derivation that changes
//! a word with each kind — the same property `windows/worldgen/tests/
//! exposure.rs`'s concept sweep already has, generalised from a
//! hand-written concept list to an enum's own variants (spec §4, guard 2).
//!
//! # Two different properties, and this guard is the second one
//!
//! Task 7 fixed the **inert-position** case: `Tonogenesis` drawn *before*
//! any merger is provably the identity (`evolve` opens with `pending =
//! None`), so `draw_rule` now only offers it once a merger has already been
//! seen. This guard covers the **inert-kind** case instead — a kind that
//! never fires *anywhere*, position notwithstanding — which is a different
//! property Task 7's fix does not, by itself, guarantee. Verified against
//! `46baaf07^` (the commit immediately before Task 7's fix landed): this
//! guard **already passes on the pre-fix tree** — see "Step 2 finding"
//! below for why, and for what that implies about what the guard is
//! actually protecting against here.
//!
//! # Step 2 finding: NO currently shipped species can witness `Tonogenesis`
//!
//! `RuleKind::Tonogenesis` writes a tone onto a stranded nucleus, and the
//! write only takes if the toned vowel is in the phonology's inventory
//! (`domains/language/src/etymology.rs`'s `apply_tonogenesis`) — the
//! codomain constraint every rule obeys. `draw_phonology` only ever admits
//! a toned vowel when the species' `Envelope::tonality` is above zero
//! (`domains/language/src/phonology.rs`'s `draw_tone_inventory`), and
//! **every currently shipped kind's `ArticulationVector` carries
//! `tonality: 0.0`** (`domains/language/src/lib.rs`'s
//! `articulation_registry` — goblin, kobold, hobgoblin, bugbear, all three
//! dragons, and gnoll, without exception; the doc comment on
//! `ArticulationVector::tonality` says so explicitly: "the shipped
//! humanoids stay 0.0; the value earns its keep as the bestiary grows"). So
//! for every species this world currently places, `Tonogenesis` is the
//! identity **unconditionally** — not merely rarely, and not only before a
//! merger. Measured directly (a throwaway instrumented copy of
//! `windows/lab/tests/wear_funnel.rs`, over `FUNNEL_SEEDS` widened to 40
//! seeds on both `46baaf07^` and this tree): across 2041 wear-eligible
//! morphemes on EACH tree, `Tonogenesis` fired zero times, both before and
//! after Task 7 — Task 7 only cut how often it is *drawn* pre-merger (445
//! draws pre-fix → 26 post-fix, over the same 40 seeds), not whether it can
//! ever *change* anything for a real placed species. That is a real,
//! reportable finding, not a defect in this guard.
//!
//! # A second, independent finding of the same shape: `VowelShift`
//!
//! Writing this guard with only the three shipped species above (no probe)
//! reds it naming **both** `[VowelShift, Tonogenesis]` unwitnessed — not
//! Tonogenesis alone. `vowel_shift` only lands when the raised/lowered
//! output is one of the 5 canonical vowels (`domains/language/src/
//! phonology.rs`'s `vowel_order`: i/e/a/o/u), which requires the phonology's
//! inventory to admit two *adjacent* heights at the same backness (i+e or
//! o+u). `vowel_band_count` grows that band outward from `a`
//! (`vowel_space`-sized, centered on index 2), and the band only reaches an
//! adjacent i/e or o/u pair once `vowel_space` is roughly ≥ 0.7. **Every
//! currently shipped species' `vowel_space` is ≤ 0.5** (goblin/hobgoblin
//! 0.5, bugbear/the three dragons 0.4, gnoll 0.35, kobold 0.3) — so
//! `VowelShift`, like `Tonogenesis`, is structurally inert for the entire
//! current bestiary, for an unrelated reason (a vowel-space ceiling, not a
//! tonality floor) that has nothing to do with F7 or Task 7 at all. This
//! was not anticipated by the plan; it surfaced only because the checklist
//! is derived from the type and therefore checks kinds the plan's own
//! sketch (`["goblin", "kobold", "gnoll"]`) did not think to doubt.
//!
//! Given both findings, a checklist honestly **derived from the type**
//! cannot be satisfied by shipped species alone — the sweep below includes
//! one synthetic probe (`TONE_PROBE_SPECIES`, `tonality: 1.0`,
//! `vowel_space: 1.0`) purely so the real `draw_phonology` → `draw_cascade`
//! → `proto_root` → `evolve` mechanism has a channel to exercise both
//! structurally-shut-out kinds at once. This mirrors `windows/lab/src/
//! roster.rs`'s `serpent_tonal_solo_components`, built for the tonality
//! half of the same reason ("the shipped peoples stay atonal by authoring
//! \[spec §9\]; tone is for the future bestiary"). Removing the probe and
//! running only shipped species reds this test permanently, on every
//! commit, regardless of any future cascade fix — that is the inert-kind
//! failure this guard cannot distinguish from "no wide-voweled, tone-capable
//! species has shipped yet" without the probe, so the probe is load-bearing,
//! not decoration.
//!
//! Checked *by injection*, the dangerous direction: temporarily removing
//! `TONE_PROBE_SPECIES` from [`species_envelopes`] (leaving `RuleKind::ALL`
//! and the sweep logic untouched) reds this test, naming exactly
//! `[VowelShift, Tonogenesis]` unwitnessed — confirmed by actually running
//! it, not merely asserted here — evidence the sweep is doing real work
//! rather than trivially passing; restoring the probe returns it to green.

use hornvale_kernel::Seed;
use hornvale_language::{
    Envelope, ExoticSeg, RuleKind, draw_cascade, draw_phonology, evolve, proto_root,
    universal_stratum,
};
use std::collections::BTreeSet;

/// A shipped species' envelope, transcribed from
/// `domains/language/src/lib.rs`'s `articulation_registry` (the language
/// crate cannot import `hornvale-species`, so these are hand-copied — the
/// same transcription `domains/language/src/etymology.rs`'s own
/// `gob_env`/`goblinoid_family` test helpers already do).
fn shipped_env(
    labiality: f64,
    vowel_space: f64,
    voicing: f64,
    sibilance: f64,
    voice_loudness: f64,
    exotic: ExoticSeg,
) -> Envelope {
    Envelope {
        labiality,
        vowel_space,
        voicing,
        sibilance,
        voice_loudness,
        tonality: 0.0,
        exotic,
    }
}

/// The synthetic tone-capable species this guard needs to make
/// `Tonogenesis` witnessable at all — see the module doc's "Step 2
/// finding". Never placed in a real world; exists only here, the same way
/// `windows/lab/src/roster.rs`'s `serpent_tonal_solo_components` exists
/// only for its own crate's tests.
const TONE_PROBE_SPECIES: &str = "tone-probe";

/// The species/envelope pairs this guard sweeps: three shipped, atonal
/// species (their real production envelopes) plus the one synthetic
/// tone-capable probe, in that order.
fn species_envelopes() -> Vec<(&'static str, Envelope)> {
    vec![
        (
            "goblin",
            shipped_env(0.5, 0.5, 0.5, 0.5, 0.5, ExoticSeg::None),
        ),
        (
            "kobold",
            shipped_env(0.1, 0.3, 0.6, 0.9, 0.2, ExoticSeg::Trill),
        ),
        (
            "gnoll",
            shipped_env(0.35, 0.35, 0.6, 0.55, 0.85, ExoticSeg::None),
        ),
        (
            TONE_PROBE_SPECIES,
            Envelope {
                tonality: 1.0,
                vowel_space: 1.0,
                ..shipped_env(0.5, 1.0, 0.5, 0.5, 0.5, ExoticSeg::None)
            },
        ),
    ]
}

#[test]
fn every_rule_kind_is_witnessed_changing_a_word() {
    let mut fired: BTreeSet<RuleKind> = BTreeSet::new();
    'seeds: for seed in 0u64..32 {
        for (species, env) in species_envelopes() {
            let ph = draw_phonology(&Seed(seed), species, &env);
            let cascade = draw_cascade(&Seed(seed), species);
            for concept in universal_stratum() {
                let proto = proto_root(&Seed(seed), species, concept.concept, &ph);
                let derivation = evolve(&proto, &cascade, &ph);
                for step in &derivation.steps {
                    if step.changed {
                        fired.insert(step.rule.kind);
                    }
                }
            }
        }
        if fired.len() == RuleKind::ALL.len() {
            break 'seeds;
        }
    }

    let missing: Vec<&RuleKind> = RuleKind::ALL
        .iter()
        .filter(|k| !fired.contains(k))
        .collect();
    assert!(
        missing.is_empty(),
        "no rule of kind {missing:?} ever changed a word across the sweep (seeds 0..32, \
         species {:?}) — the kind is inert in practice, whatever its implementation says",
        species_envelopes()
            .iter()
            .map(|(s, _)| *s)
            .collect::<Vec<_>>()
    );
}
