//! The Wearing (LANG-55): later-epoch proto-roots are drawn from a reserved
//! region of the SAME-LENGTH form space, so a new concept cannot collide with
//! an established one — additivity by construction of the codomain rather than
//! by the assignment ORDER (which is what The Accession bought, at Zipf's
//! expense).
//!
//! Both tests below fix a `(seed, species)` pair whose drawn coda inventory
//! is verified (by an asserted precondition, not assumed) to be the shape
//! each test needs — mirroring `etymology::tests::test_phonology`'s own
//! documented seed-search convention. Neither uses the once-obvious "assert
//! disjointness and non-longer-ness" pair alone: `assign_proto_roots_with_epoch`'s
//! pre-existing open-addressing loop already guarantees no two concepts ever
//! share a form regardless of any coda carve, so a test built only on that
//! guarantee would pass with the carve deleted. These assert directly on the
//! one thing the carve actually controls — the drawn forms' final-coda
//! shape — instead.
use hornvale_kernel::Seed;
use hornvale_language::{
    Envelope, ExoticSeg, Segment, assign_proto_roots_with_epoch_for_test, draw_phonology,
};

/// A permissive envelope — every dimension maxed except tonality — so the
/// drawn phonology's coda inventory is as rich as the species/seed pair
/// allows. Mirrors `etymology::tests::test_phonology` and
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

/// Whether `form`'s final segment is a consonant — i.e. the word ends on a
/// closed syllable, the region the carve reserves for epoch-≥1 concepts.
fn ends_closed(form: &[Segment]) -> bool {
    matches!(form.last(), Some(Segment::Consonant { .. }))
}

/// A later-epoch concept's root always ends closed, and does not grow into
/// a longer syllable tier than the epoch-0 forms drawn alongside it — but
/// only when the underlying phonology actually distinguishes open from
/// closed. `Seed(13)`/`"goblin"` is searched (by the same technique
/// `test_phonology` documents) to draw `codas: [[Approximant], []]` — one
/// non-empty AND one empty template — asserted below as a precondition so a
/// future re-search that lands on a phonology where the two cases collapse
/// (e.g. every template closed, the vacuous case the brief's original
/// version of this test silently fell into: at `Seed(42)`/`"goblin"`,
/// `ph.codas` is `[[Stop]]`, a single always-closed template, so every form
/// ends closed whether or not the carve runs) fails loudly rather than
/// passing for the wrong reason.
#[test]
fn later_epoch_roots_end_closed_when_the_phonology_admits_both() {
    let seed = Seed(13);
    let ph = draw_phonology(&seed, "goblin", &permissive_envelope());
    assert!(
        ph.codas.iter().any(|t| t.is_empty()),
        "fixture precondition: the phonology must admit an OPEN coda \
         template, or an epoch-0 form ending open is impossible and the \
         closed/open contrast this test asserts is vacuous — re-search the \
         seed/species pair"
    );
    assert!(
        ph.codas.iter().any(|t| !t.is_empty()),
        "fixture precondition: the phonology must admit a CLOSED coda \
         template, or the carve has nothing to reserve — re-search the \
         seed/species pair"
    );

    let concepts = ["water", "stone", "fire", "hill", "river", "ford"];
    let epoch_of = |c: &str| u32::from(matches!(c, "hill" | "river" | "ford"));
    let assigned =
        assign_proto_roots_with_epoch_for_test(&seed, "goblinoid", &ph, &concepts, &[], epoch_of);

    let old: Vec<_> = ["water", "stone", "fire"]
        .iter()
        .map(|c| assigned[*c].clone())
        .collect();
    assert!(
        old.iter().any(|form| !ends_closed(form)),
        "every epoch-0 form ended closed on this phonology — the fixture \
         no longer exercises the open/closed contrast this test depends on \
         (a vacuous pass is the exact bug this test was written to catch), \
         re-search the seed/species pair"
    );

    for late in ["hill", "river", "ford"] {
        let form = &assigned[late];
        assert!(
            ends_closed(form),
            "{late} = {form:?} did not end closed — the epoch-≥1 carve is \
             not restricting the coda to the reserved region"
        );
        assert!(
            !old.contains(form),
            "{late} collided with an epoch-0 root by construction"
        );
        // A closed form can be exactly one segment longer than the SAME
        // draw would be open (the coda consonant itself); it must never be
        // a whole syllable tier longer, which is what `PROBE_BUDGET`
        // lengthening (the axis this campaign must not spend) would look
        // like against six concepts that never collide.
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

/// Where the phonology admits no closed coda at all, the carve must
/// degrade to the identity: an epoch-≥1 concept is assigned EXACTLY the
/// form it would receive at epoch 0, not merely a same-length one.
/// `Seed(1)`/`"goblin"` is searched to draw `codas: [[], []]` — every
/// template empty, so `Namer::choose_coda_template`'s `weighty` branch can
/// never find a non-empty template to restrict to and falls through to the
/// shared, unrestricted pick (verified by reading
/// `naming.rs::choose_coda_template`; see the Task 2 report). This is the
/// "correctness before marking" clause of the carve, and — unlike a
/// coda-less phonology built by hand — this one is a genuine
/// `draw_phonology` output, so the test exercises the real fallback path
/// rather than a constructed stand-in for it.
#[test]
fn later_epoch_roots_degrade_to_epoch_zero_when_no_closed_coda_exists() {
    let seed = Seed(1);
    let ph = draw_phonology(&seed, "goblin", &permissive_envelope());
    assert!(
        ph.codas.iter().all(|t| t.is_empty()),
        "fixture precondition: the phonology must admit NO closed coda \
         template, or this is no longer the degradation case — re-search \
         the seed/species pair"
    );

    let concepts = ["water", "stone", "fire", "hill", "river", "ford"];
    let later_epoch = |c: &str| u32::from(matches!(c, "hill" | "river" | "ford"));
    let all_epoch_zero = |_: &str| 0u32;

    let with_carve = assign_proto_roots_with_epoch_for_test(
        &seed,
        "goblinoid",
        &ph,
        &concepts,
        &[],
        later_epoch,
    );
    let without_epochs = assign_proto_roots_with_epoch_for_test(
        &seed,
        "goblinoid",
        &ph,
        &concepts,
        &[],
        all_epoch_zero,
    );

    assert_eq!(
        with_carve, without_epochs,
        "a coda-less phonology must assign every concept identically \
         regardless of epoch — the carve degraded to something other than \
         the identity"
    );
}
