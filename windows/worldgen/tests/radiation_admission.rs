//! THE RADIATION (C2d): the six elves' structural admission.
//!
//! Every claim here is a precondition for a LATER task, asserted now so that a
//! bad authored value fails on the value rather than on the measurement three
//! tasks downstream. Nothing in this file measures the world; it reads the
//! registries.

use hornvale_kernel::KindId;
use hornvale_language::ArticulationVector;
use hornvale_species::{HabitatRealm, SocialForm};

/// The six, in ascending `KindId` order — the order every `ComponentStore`
/// iterates in, so a slice built from this list is index-aligned with one
/// built from `wc.biosphere`.
const ELVES: [&str; 6] = [
    "desert-elf",
    "drow",
    "high-elf",
    "sea-elf",
    "snow-elf",
    "wood-elf",
];

/// The whole peopled cluster, in one place. `WorldComponents::assemble()`
/// already enforces the lattice and would fail the whole workspace if a
/// component were missing — this test exists to say WHICH one, on a kind
/// nobody has run a world for yet, instead of leaving an implementer to read
/// `"Settled kind KindId(\"sea-elf\") is missing a peopled component"` and
/// guess.
#[test]
fn every_elf_carries_the_full_peopled_cluster() {
    let wc = hornvale_worldgen::WorldComponents::assemble()
        .expect("canonical registries are well-formed");
    for name in ELVES {
        let k = KindId(name);
        assert!(wc.biosphere.contains(&k), "{name}: no biosphere row");
        assert!(wc.psyche.contains(&k), "{name}: no psyche row");
        assert!(wc.society.contains(&k), "{name}: no society row");
        assert!(wc.perception.contains(&k), "{name}: no perception row");
        assert!(wc.articulation.contains(&k), "{name}: no articulation row");
        assert!(wc.lexicon.contains(&k), "{name}: no lexicon row");
        assert!(wc.family_of.contains(&k), "{name}: no family_of row");
        assert_eq!(
            wc.family_of.get(&k),
            Some(&"elf"),
            "{name} must carry the family label \"elf\" — the six share one \
             proto, and `family_proto` is keyed by the LABEL"
        );
        assert_eq!(
            wc.biosphere.get(&k).expect("checked above").social_form,
            SocialForm::Settled,
            "{name} must be Settled: only Settled kinds enter the bake's \
             roster, and a non-Settled elf is authored and inert (P1' would \
             correctly read it as byte-neutral)"
        );
    }
    assert!(
        wc.family_proto.contains(&KindId("elf")),
        "a family label carried by six kinds requires a `family_proto` entry; \
         `check_integrity` makes this mandatory at the SECOND row"
    );
}

/// **The affinity precondition, asserted per elf BEFORE Task 3 may proceed.**
///
/// `tolerance_liebig` floors temperature/moisture/insolation by
/// `sovereignty_floor(mass, potency)` and floors elevation by `0.0`. A floored
/// axis never reads below its floor; the unfloored one peaks at its own
/// `devotion`. So `elevation.devotion < sovereignty_floor` makes elevation the
/// Liebig minimum at every cell of every world, and the other three curves
/// contribute exactly nothing — which is what makes a biome-affinity row a
/// preference the model was THROWING AWAY rather than one it already applies.
///
/// The spec (§3.1) records the satisfiable band: 0.4133 at 45 kg and 0.4477 at
/// 70 kg, both at potency 0. Floors are computed LIVE here, never copied from
/// a plan — The Delvers' plan table was wrong in the fourth decimal for two of
/// three.
///
/// This duplicates, deliberately, what
/// `range_readout.rs::every_occupant_has_climate_curves_the_minimum_currently_discards`
/// will assert once the rows exist. That one iterates the affinity registry and
/// is therefore silent about a kind with no row yet; this one names the six and
/// fires before Task 3 has written a line.
#[test]
fn every_elf_clears_the_affinity_precondition() {
    let biosphere = hornvale_species::biosphere_registry();
    for name in ELVES {
        let bio = biosphere
            .get(&KindId(name))
            .unwrap_or_else(|| panic!("{name} has no biosphere row"));
        let floor = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
        let devotion = bio.condition_niche.elevation.devotion;
        println!(
            "   {name:<12} mass {:>6.1} kg  potency {:.2}  floor {floor:.6}  \
             elev devotion {devotion:.2}",
            bio.mass.kilograms(),
            bio.potency,
        );
        assert!(
            devotion < floor,
            "{name} may NOT take a biome affinity: its elevation devotion \
             ({devotion}) is at or above its sovereignty floor ({floor}), so \
             the Liebig minimum does NOT discard its temperature/moisture/\
             insolation curves. An affinity row on top of them is the \
             DOUBLE-COUNT the spec's §3.1 forbids, and the movement it \
             produced would be unattributable. Lower the devotion or raise \
             the mass — do not add the row."
        );
    }
}

/// Drow is the ONLY elf in the sparse realm store, and every other elf must be
/// absent from it (absence means `Surface`). Asserted in both directions
/// because the Wood/Drow contrast — the family's realm isolate — is only a
/// single-variable contrast if exactly one of the two carries a realm row.
#[test]
fn drow_alone_is_subterranean() {
    let realm = hornvale_species::habitat_realm_registry();
    assert_eq!(
        realm.get(&KindId("drow")),
        Some(&HabitatRealm::Subterranean),
        "drow must be Subterranean: the realm gate is its ONLY authored \
         separation from the surface elves (spec §3.5)"
    );
    for name in ELVES.into_iter().filter(|n| *n != "drow") {
        assert!(
            realm.get(&KindId(name)).is_none(),
            "{name} appears in the habitat-realm store; every elf but drow \
             must be absent from it (absence means Surface), or Wood/Drow \
             stops isolating the realm variable"
        );
    }
}

/// **Drow encodes NO depth in its elevation curve.** Depth below the surface
/// and height above sea level are different quantities: a deep chamber under a
/// mountain sits high above the sea, a shallow cave in a marsh sits low. The
/// Delvers committed exactly this fake and caught it — duergar authored at a
/// 300 m optimum to mean *deep* selected lowland marshes, and its toponymy came
/// back as an emergent finding until one question dissolved it. **The toponymy
/// was reporting the authoring.**
///
/// Pinned as an equality with wood-elf rather than as a range, because a range
/// invites the next author to argue their number is inside it. Drow's elevation
/// curve is Wood's curve, and if it ever stops being Wood's curve that is a
/// decision to record.
#[test]
fn drows_elevation_curve_is_woods_and_says_nothing_about_depth() {
    let biosphere = hornvale_species::biosphere_registry();
    let drow = biosphere.get(&KindId("drow")).expect("drow row");
    let wood = biosphere.get(&KindId("wood-elf")).expect("wood-elf row");
    assert_eq!(
        drow.condition_niche.elevation, wood.condition_niche.elevation,
        "drow's elevation response must be IDENTICAL to wood-elf's. The \
         elevation axis is metres above sea level and cannot say \"deep\"; \
         encoding depth into it is The Delvers' withdrawn duergar fake, and \
         the finding it produced was the authoring read back."
    );
}

/// The sea elf must actually be able to eat at sea. `marine_forage_supply_field`
/// keys productivity off the biome class, and a kind with no `MARINE_FORAGE`
/// weight draws zero supply on every water cell no matter what its affinity
/// says — it would be authored, admitted, and void.
#[test]
fn the_sea_elf_draws_on_the_marine_supply_axis() {
    let biosphere = hornvale_species::biosphere_registry();
    let sea = biosphere.get(&KindId("sea-elf")).expect("sea-elf row");
    assert!(
        sea.niche.weight(hornvale_kernel::MARINE_FORAGE) > 0.0,
        "sea-elf has no MARINE_FORAGE weight, so it draws zero supply on every \
         ocean cell and its shelf affinity multiplies zero. Killer whale, \
         giant squid and reef shark are the authoring precedent (all 1.0)."
    );
    for name in ELVES.into_iter().filter(|n| *n != "sea-elf") {
        let bio = biosphere.get(&KindId(name)).expect("elf row");
        assert_eq!(
            bio.niche.weight(hornvale_kernel::MARINE_FORAGE),
            0.0,
            "{name} weights MARINE_FORAGE; only sea-elf may, or Sea stops \
             being the family's marine isolate"
        );
    }
}

/// The five authored scalar dimensions of an [`ArticulationVector`], in the
/// order the registry rows write them. `tonality` is excluded deliberately —
/// every shipped humanoid is 0.0, so counting it would only ever add zero and
/// would make the "differs on at least three" claim below weaker than it
/// reads. `exotic` is excluded for the same reason in reverse: it is a
/// categorical innovation the divergence claim already covers separately.
fn differing_scalar_dimensions(a: &ArticulationVector, b: &ArticulationVector) -> usize {
    usize::from(a.labiality != b.labiality)
        + usize::from(a.vowel_space != b.vowel_space)
        + usize::from(a.voicing != b.voicing)
        + usize::from(a.sibilance != b.sibilance)
        + usize::from(a.voice_loudness != b.voice_loudness)
}

/// **P5's divergence clause has a PRECONDITION in the authoring, and this is
/// it.** The spec (§5, P5) measures whether some concept rooted in all six
/// daughters has ≥ 2 distinct present-day forms — descent proven by shared
/// *innovations*, not by a shared ancestor alone. The cascade consumes the
/// daughters' articulation vectors, so six daughters authored identically to
/// the `elf` proto (or to each other) would be six names for one tongue, and
/// P5 would correctly read false. The campaign would then have measured a null
/// it authored into existence.
///
/// The `articulation_registry` block comment already asserts this in prose —
/// "each row below DIVERGES from `family_proto`'s `KindId("elf")` vector … on
/// at least three dimensions". This test is that sentence made able to fail,
/// on the file's own standard: **a precondition that cannot fire is worse than
/// a comment**, because a later editor tidying two vectors toward the family
/// mean would take P5 down silently.
///
/// Asserted in both of the shapes the clause needs: no daughter equals the
/// proto (there is a family, not an alias), and no two daughters are equal
/// (there are six tongues, not fewer). Compared on the authored `f64`s exactly
/// — these are hand-written literals, never computed, so exact equality is the
/// right predicate and a near-miss is a different (and permitted) thing.
#[test]
fn every_elf_diverges_from_the_proto_and_from_its_siblings() {
    let wc = hornvale_worldgen::WorldComponents::assemble()
        .expect("canonical registries are well-formed");
    let proto = wc
        .family_proto
        .get(&KindId("elf"))
        .expect("the elf family carries a proto vector");
    for name in ELVES {
        let v = wc
            .articulation
            .get(&KindId(name))
            .unwrap_or_else(|| panic!("{name} has no articulation row"));
        let n = differing_scalar_dimensions(v, proto);
        println!("   {name:<12} differs from proto-Elvish on {n}/5 scalar dimensions");
        assert_ne!(
            v, proto,
            "{name}'s articulation vector IS the `elf` proto vector. A daughter \
             that has not moved at all is a proto with a second name: its \
             cascade has nothing to consume, and P5's divergence clause would \
             read false for a reason the author created rather than the model."
        );
        assert!(
            n >= 3,
            "{name} differs from proto-Elvish on only {n} of five scalar \
             dimensions; the `articulation_registry` block comment claims at \
             least three for every row. Either the row drifted toward the \
             proto or the comment is now false — fix whichever is wrong, do \
             not lower this bound."
        );
    }
    for (i, a) in ELVES.into_iter().enumerate() {
        for b in ELVES.into_iter().skip(i + 1) {
            let va = wc.articulation.get(&KindId(a)).expect("checked above");
            let vb = wc.articulation.get(&KindId(b)).expect("checked above");
            assert_ne!(
                va, vb,
                "{a} and {b} carry IDENTICAL articulation vectors, so the \
                 family has fewer distinct tongues than daughters. P5's \
                 divergence clause is stated over SIX draws precisely because \
                 six have more room to differ than three; two aliases spend \
                 that room without measuring anything."
            );
        }
    }
}

/// Wood and High are the family's MIND isolate (spec §3.6): same realm, same
/// biomes, differing only in psyche, society and language. That contrast is
/// only single-variable if their BIOSPHERE rows agree exactly — mass moves the
/// sovereignty floor, and the resource vector moves supply, either of which
/// would silently make P3 a two-variable comparison.
#[test]
fn wood_and_high_differ_in_mind_and_not_in_body() {
    let biosphere = hornvale_species::biosphere_registry();
    let wood = biosphere.get(&KindId("wood-elf")).expect("wood-elf row");
    let high = biosphere.get(&KindId("high-elf")).expect("high-elf row");
    assert_eq!(
        wood.mass.kilograms(),
        high.mass.kilograms(),
        "wood-elf and high-elf must share a mass: mass sets the sovereignty \
         floor, and a differing floor makes P3(a) a comparison of two \
         variables"
    );
    assert_eq!(wood.potency, high.potency, "same potency, same floor");
    assert_eq!(
        wood.condition_niche, high.condition_niche,
        "wood-elf and high-elf must share a condition niche — §3.6 says High \
         diverges in MIND and SOCIETY, not in environment"
    );
    assert_eq!(
        wood.niche, high.niche,
        "wood-elf and high-elf must share a resource vector, or supply \
         separates them and P3 measures diet rather than mind"
    );

    let psyche = hornvale_species::psyche_registry();
    assert_ne!(
        psyche.get(&KindId("wood-elf")),
        psyche.get(&KindId("high-elf")),
        "wood-elf and high-elf carry IDENTICAL psyche rows, so High is not a \
         control — it is a duplicate. §3.6's whole claim is that the two \
         differ in mind."
    );
    let society = hornvale_species::society_registry();
    assert_ne!(
        society.get(&KindId("wood-elf")),
        society.get(&KindId("high-elf")),
        "wood-elf and high-elf carry identical society rows; High's identity \
         lives in psyche, society and language and nowhere else"
    );
}
