//! The Wearing (LANG-55): later-epoch proto-roots are drawn from a reserved
//! region of the SAME-LENGTH form space, so a new concept cannot collide with
//! an established one — additivity by construction of the codomain rather than
//! by the assignment ORDER (which is what The Accession bought, at Zipf's
//! expense).
use hornvale_kernel::Seed;
use hornvale_language::{
    Envelope, ExoticSeg, assign_proto_roots_with_epoch_for_test, draw_phonology,
};

/// A permissive envelope — every dimension maxed except tonality — so the
/// drawn phonology's coda inventory is as rich as possible and the
/// weighty/closed-coda carve this test exercises has real templates to draw
/// from rather than degrading to the open-coda fallback. Mirrors
/// `etymology::tests::test_phonology` and
/// `speakable_properties::permissive_proto`; `draw_phonology` takes an
/// `Envelope` (not just a species name), so the brief's two-argument call is
/// adapted here rather than reproduced verbatim — see the Task 2 report.
fn permissive_envelope() -> Envelope {
    Envelope {
        labiality: 1.0,
        vowel_space: 1.0,
        voicing: 1.0,
        sibilance: 1.0,
        voice_loudness: 1.0,
        tonality: 0.0,
        exotic: ExoticSeg::None,
    }
}

/// A later-epoch concept never receives a root already assigned to an
/// epoch-0 concept, and never receives a LONGER one merely for being later.
#[test]
fn later_epoch_roots_are_disjoint_but_not_longer() {
    let seed = Seed(42);
    let ph = draw_phonology(&seed, "goblin", &permissive_envelope());
    let concepts = ["water", "stone", "fire", "hill", "river", "ford"];
    let epoch_of = |c: &str| u32::from(matches!(c, "hill" | "river" | "ford"));

    let assigned =
        assign_proto_roots_with_epoch_for_test(&seed, "goblinoid", &ph, &concepts, &[], epoch_of);

    let old: Vec<_> = ["water", "stone", "fire"]
        .iter()
        .map(|c| assigned[*c].clone())
        .collect();
    for late in ["hill", "river", "ford"] {
        let form = &assigned[late];
        assert!(
            !old.contains(form),
            "{late} collided with an epoch-0 root by construction"
        );
        let longest_old = old.iter().map(Vec::len).max().expect("non-empty");
        assert!(
            form.len() <= longest_old + 1,
            "{late} is {} segments against an epoch-0 max of {longest_old} — \
             the carve spent the LENGTH axis, which is the one axis The \
             Wearing must not spend",
            form.len()
        );
    }
}
