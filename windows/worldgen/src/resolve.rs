//! Cursor resolution: turn a vertex into the name of the most specific
//! feature there, or (The Portolan part II, Task 4) the full containment
//! chain. See `docs/superpowers/specs/2026-08-19-the-portolan-design.md`
//! §3 and `docs/superpowers/specs/2026-08-20-the-portolan-world-map-
//! design.md` §5 — the terminal client's free-roaming cursor query and the
//! map strip that reports it.
//!
//! This is deliberately a thin function, not a new kind of index: the real
//! work (declaring salience, building `Vertex -> Vec<FeatureId>` once at
//! world load) lives in `hornvale_terrain::landscape` — `FeatureClass::
//! salience` and `VertexFeatureIndex` — because it needs nothing this crate
//! adds (`domains/terrain` may not depend on `hornvale-language` or
//! `windows/worldgen`, by the kernel -> domains -> windows layering). What
//! belongs here is the one step that DOES need this crate: turning a
//! resolved [`FeatureId`] into a drawn name via [`crate::feature_name`], and
//! (for the chain) each class's prose noun via
//! [`hornvale_almanac::gazetteer::class_words`] — already this crate's own
//! dependency, reused rather than a second copy of the class-to-noun table.

use hornvale_kernel::{Seed, Vertex};
use hornvale_language::{MorphOptions, Phonology};
use hornvale_terrain::landscape::{FeatureClass, FeatureId, VertexFeatureIndex};

/// The most specific feature's name at `vertex`, or `None` if `vertex` carries
/// no individuated feature (real terrain below every class's individuation
/// floor — seed 42 measured 377 of 40,962 vertices like this, 0.92%).
///
/// `index` must already be built (`VertexFeatureIndex::build`, which sorts
/// each vertex's stack most-specific-first), so this is an `O(1)` lookup —
/// `index.at(vertex).first()` — plus one name draw. The caller supplies the
/// world's seed and one people's phonology/morphology; a different people
/// at the same vertex draws a different name from the same [`FeatureId`], the
/// same multi-name shape [`crate::gazetteer_class_entries`] already gives
/// the gazetteer page.
/// type-audit: bare-ok(identifier-text: species), bare-ok(identifier-text: return)
pub fn resolve_at(
    index: &VertexFeatureIndex,
    vertex: Vertex,
    seed: Seed,
    species: &str,
    ph: &Phonology,
    morph: &MorphOptions,
) -> Option<String> {
    let id = *index.at(vertex).first()?;
    Some(crate::feature_name(seed, id, species, ph, morph).roman)
}

/// One resolved link in a containment chain (Task 4, F8): a feature's
/// real drawn name and class, plus whether the
/// possession has discovered it. `discovered` is decided per-link, never
/// inferred from a sibling link's own state — see [`format_chain`]'s doc
/// for why an outer link's discovery says nothing about an inner one's
/// (spec Amendment 1, §A4b: "co-location is not discovery" applies in
/// both directions along the chain, not just from vertex to feature).
///
/// **Task 5 wired the real gate.** [`resolve_chain_at`] takes a caller-
/// supplied `is_discovered` predicate and asks it per link, so this field
/// carries the possession's own true discovery state — never a hardcoded
/// `true`. `windows/worldgen` still owns no notion of a *session's*
/// discovery history (that lives in `clients/game/bin`, behind the same
/// containment `driver.rs`'s module doc states: no `Session`/`Knowledge`
/// value may cross out of `Driver`), so this crate stays a pure function of
/// whatever the caller already knows, exactly as it was before Task 5 —
/// only the caller's answer changed from a constant to a real predicate.
/// type-audit: bare-ok(identifier-text: name), bare-ok(flag: discovered)
#[derive(Debug, Clone)]
pub struct ChainLink {
    /// The feature's real, drawn name — never withheld or replaced here;
    /// [`format_chain`] is what decides whether the reader sees it.
    pub name: String,
    /// Which kind of feature this link is.
    pub class: FeatureClass,
    /// Whether the possession has discovered this specific feature (spec
    /// Amendment 1 §A4b): an extent feature (volcano/landmass/sea/salt
    /// lake/river) is discovered by entering any vertex of its extent.
    pub discovered: bool,
}

/// The preposition joining a chain link to the one before it, chosen by
/// the OUTER (containing) link's own class: land-like classes (a
/// landmass, or a river's banks) take "on"; water-like classes (a sea, a
/// salt lake) take "in". `Volcano` never actually appears as an outer
/// link — it is always the most specific class
/// ([`FeatureClass::salience`]), so this arm is included only to keep the
/// match exhaustive and total, not because real data reaches it.
fn preposition_for(class: FeatureClass) -> &'static str {
    match class {
        FeatureClass::Sea | FeatureClass::SaltLake => "in",
        FeatureClass::Volcano | FeatureClass::Landmass | FeatureClass::River => "on",
    }
}

/// One link's own drawn text: `"Name (a class)"` when discovered, or —
/// spec Amendment 1 §A3's "undiscovered landmarks render as terrain and go
/// unnamed" rule, applied to the strip exactly as it applies to the plate
/// — `"an unnamed class"` when not. The class itself is never withheld: a
/// player can SEE it is a landmass (it is drawn, always — §A3 again),
/// only the feature's proper identity is gated.
fn describe(link: &ChainLink) -> String {
    let (singular, _) = hornvale_almanac::gazetteer::class_words(link.class);
    if link.discovered {
        format!("{} (a {singular})", link.name)
    } else {
        format!("an unnamed {singular}")
    }
}

/// Format a resolved containment chain into the strip's own prose (design
/// spec §5): every link, most specific first, each joined to the next by
/// [`preposition_for`] — `"Name (a class), on Name (a class)"` — with an
/// undiscovered link's name replaced by [`describe`]'s honest placeholder
/// rather than either fabricating one or dropping the link from the chain
/// entirely.
///
/// **F8 — the semantics this function pins, ahead of
/// Task 5's real discovery gate:** an outer link being undiscovered never
/// hides an inner, more specific link's own name, and vice versa.
/// Discovery is a per-FEATURE fact (§A4b), not a property of the chain as a
/// whole, so "standing in a named valley inside an unnamed landmass" shows
/// exactly that — the valley's real name, and "an unnamed landmass" for the
/// landmass — never suppresses the valley's name because its container is
/// unknown, and never fabricates the landmass's name because the valley IS
/// known. The chain link itself is never removed: the ground is drawn
/// either way (§A3), so the strip states what class of thing is there even
/// when it cannot yet say which one.
///
/// `None` iff `links` is empty (mirrors [`resolve_at`]'s `None` for a vertex
/// with no individuated feature at all).
///
/// **`prose`, not `identifier-text`: this returns composed, punctuated,
/// multi-clause text** ("Vngashngatva (a volcano), on Kxsokxkxzhakx (a
/// landmass)"), not a bare name — the same class this crate's own
/// `sky_phrase`/`biome_lines`/`settlement_lines`/`culture_lines` already
/// carry `bare-ok(prose: return)` for. `resolve_at`'s `identifier-text`
/// tag is correct where it sits (it returns one bare drawn name); copying
/// it here named the wrong class (fix round 1, reviewer finding 2) —
/// `type-audit check` is syntax-only and cannot catch a well-formed tag
/// naming the wrong class, so this was reclassified by hand.
/// type-audit: bare-ok(prose: return)
pub fn format_chain(links: &[ChainLink]) -> Option<String> {
    let (first, rest) = links.split_first()?;
    let mut text = describe(first);
    for link in rest {
        text.push_str(", ");
        text.push_str(preposition_for(link.class));
        text.push(' ');
        text.push_str(&describe(link));
    }
    Some(text)
}

/// The full containment chain at `vertex`, most specific first, formatted as
/// the strip's own prose ([`format_chain`]) — every entry [`VertexFeatureIndex::
/// at`] returns, not only the first ([`resolve_at`]'s own scope). `None`
/// iff `vertex` carries no individuated feature at all (same 0.92% case
/// [`resolve_at`]'s doc measures).
///
/// **The discovery gate (Task 5).** `is_discovered` is asked once per link,
/// in the SAME most-specific-first order [`VertexFeatureIndex::at`] already
/// sorts them — never inferred from a sibling link, matching [`ChainLink`]'s
/// own doc on why co-location along the chain never leaks either direction.
/// This crate holds no session state of its own (the containment
/// `clients/game/bin/src/driver.rs` states: no `Session`/`Knowledge` value
/// crosses out of `Driver`), so the caller — `bin`'s own `Discovered` —
/// supplies the answer; this function stays a pure fold over whatever it is
/// told.
///
/// `return` is `prose`, not `identifier-text` — see [`format_chain`]'s own
/// doc (fix round 1, reviewer finding 2) for why: this returns the same
/// composed, punctuated chain text, not a bare name.
/// type-audit: bare-ok(identifier-text: species), bare-ok(prose: return)
pub fn resolve_chain_at(
    index: &VertexFeatureIndex,
    vertex: Vertex,
    seed: Seed,
    species: &str,
    ph: &Phonology,
    morph: &MorphOptions,
    is_discovered: &dyn Fn(FeatureId) -> bool,
) -> Option<String> {
    let ids: &[FeatureId] = index.at(vertex);
    let links: Vec<ChainLink> = ids
        .iter()
        .map(|id| ChainLink {
            name: crate::feature_name(seed, *id, species, ph, morph).roman,
            class: id.class,
            discovered: is_discovered(*id),
        })
        .collect();
    format_chain(&links)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;
    use hornvale_language::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_terrain::{GeneratedTerrain, TerrainPins};

    /// The mesh level `windows/worldgen`'s own gazetteer/volcano tests use
    /// (see `gazetteer.rs`'s `LEVEL` const), so this exercises real,
    /// seed-42-shaped data without paying `GLOBE_LEVEL`'s full cost.
    const LEVEL: u32 = 6;
    const PEOPLE: &str = "aeldrin";

    fn test_phonology() -> (Phonology, MorphOptions) {
        let ph = draw_phonology(
            &Seed(7),
            PEOPLE,
            &Envelope {
                labiality: 1.0,
                vowel_space: 1.0,
                voicing: 1.0,
                sibilance: 1.0,
                voice_loudness: 1.0,
                tonality: 0.0,
                exotic: ExoticSeg::None,
            },
            &hornvale_language::typology::concatenative(),
        );
        let morph = MorphOptions {
            honorifics: false,
            shape_weights: [1.0, 1.0, 1.0],
            shape_beta: 1.0,
        };
        (ph, morph)
    }

    /// A vertex inside a real feature's extent resolves to `Some` name; the
    /// name matches what [`crate::feature_name`] draws directly for that
    /// feature's identity, so `resolve_at` is not doing anything to the
    /// name besides the lookup.
    #[test]
    fn a_covered_vertex_resolves_to_the_most_specific_features_name() {
        let seed = Seed(42);
        let geo = Geosphere::new(LEVEL);
        let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        let features = crate::gazetteer_features(seed, &geo, &terrain);
        let index = VertexFeatureIndex::build(&features);
        let (ph, morph) = test_phonology();

        let covered = features
            .first()
            .and_then(|f| f.extent.iter().next().copied())
            .expect("seed 42 has at least one feature with a nonempty extent");
        let want_id = *index.at(covered).first().expect("covered vertex resolves");
        let want = crate::feature_name(seed, want_id, PEOPLE, &ph, &morph).roman;

        assert_eq!(
            resolve_at(&index, covered, seed, PEOPLE, &ph, &morph),
            Some(want)
        );
    }

    /// An empty index (no features at all) resolves every vertex to `None`,
    /// not a panic.
    #[test]
    fn an_empty_index_resolves_to_none() {
        let (ph, morph) = test_phonology();
        let index = VertexFeatureIndex::build(&[]);
        assert_eq!(
            resolve_at(&index, Vertex(0), Seed(42), PEOPLE, &ph, &morph),
            None
        );
    }

    // -- Task 4, Step 1: the containment chain -----------------------------

    /// A real vertex whose stack is at least two deep — the same search shape
    /// `landscape.rs`'s own `at_orders_most_specific_first` test pins
    /// against a hand-built fixture, run here against real seed-42 data so
    /// the chain-formatting prose is exercised against a genuine multi-
    /// feature stack, not only the single-feature case
    /// `a_covered_vertex_resolves_to_the_most_specific_features_name` already
    /// covers.
    fn multi_feature_vertex(index: &VertexFeatureIndex, geo: &Geosphere) -> Vertex {
        geo.vertices().find(|&c| index.at(c).len() >= 2).expect(
            "seed 42 at LEVEL has at least one multi-feature vertex (measured 99.84% \
                     of multi-feature vertices form a proper containment chain -- spec §3.2)",
        )
    }

    /// The containment chain names every feature at a multi-feature vertex,
    /// most specific first — the property `at_orders_most_specific_first`
    /// (`domains/terrain::landscape`) already pins on `VertexFeatureIndex`
    /// itself; this pins that `resolve_chain_at` carries that same order
    /// through into the drawn prose, unbroken.
    #[test]
    fn the_chain_names_every_feature_most_specific_first() {
        let seed = Seed(42);
        let geo = Geosphere::new(LEVEL);
        let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        let features = crate::gazetteer_features(seed, &geo, &terrain);
        let index = VertexFeatureIndex::build(&features);
        let (ph, morph) = test_phonology();

        let vertex = multi_feature_vertex(&index, &geo);
        let stack = index.at(vertex);
        assert!(
            stack.len() >= 2,
            "sanity: the fixture vertex must be multi-feature"
        );

        let text = resolve_chain_at(&index, vertex, seed, PEOPLE, &ph, &morph, &|_| true)
            .expect("a multi-feature vertex resolves to Some chain");

        // Every link's own name and class-noun must appear, in the SAME
        // order `VertexFeatureIndex::at` already sorted them (most specific
        // first, `FeatureClass::salience` ascending).
        let mut last_pos = 0usize;
        for id in stack {
            let name = crate::feature_name(seed, *id, PEOPLE, &ph, &morph).roman;
            let (singular, _) = hornvale_almanac::gazetteer::class_words(id.class);
            let needle = format!("{name} (a {singular})");
            let pos = text
                .find(&needle)
                .unwrap_or_else(|| panic!("{needle:?} missing from chain {text:?}"));
            assert!(
                pos >= last_pos,
                "link {needle:?} is out of order in {text:?}"
            );
            last_pos = pos;
        }
    }

    // -- Task 4, Step 5 / F8: an outer undiscovered link never hides an
    //    inner discovered one's name, and never fabricates its own -------

    /// F8 — the hand-built fixture: standing on a
    /// named, discovered volcano inside an unnamed, undiscovered landmass.
    /// The volcano's real name must appear; the landmass's must not, and
    /// its class must still be named honestly (spec Amendment 1 §A3: the
    /// ground is drawn either way, only the identity is gated).
    #[test]
    fn an_undiscovered_outer_link_keeps_its_class_but_loses_its_name() {
        let links = vec![
            ChainLink {
                name: "Vngashngatva".to_string(),
                class: FeatureClass::Volcano,
                discovered: true,
            },
            ChainLink {
                name: "Kxsokxkxzhakx".to_string(),
                class: FeatureClass::Landmass,
                discovered: false,
            },
        ];
        let text = format_chain(&links).expect("a nonempty chain resolves");
        assert!(
            text.contains("Vngashngatva (a volcano)"),
            "the discovered inner link's real name must show, got {text:?}"
        );
        assert!(
            !text.contains("Kxsokxkxzhakx"),
            "the undiscovered outer link's real name must NOT leak, got {text:?}"
        );
        assert!(
            text.contains("an unnamed landmass"),
            "the undiscovered outer link's CLASS is still honest (it is drawn either way \
             -- spec Amendment 1 §A3), got {text:?}"
        );
    }

    /// The symmetric case, for completeness: an undiscovered INNER link
    /// (standing near, but not on, a named landmass's own undiscovered
    /// volcano) must not borrow the outer link's discovered name either —
    /// discovery is per-feature, never inherited from a neighbour in
    /// either direction along the chain (§A4b).
    #[test]
    fn an_undiscovered_inner_link_does_not_borrow_the_outer_links_name() {
        let links = vec![
            ChainLink {
                name: "Vngashngatva".to_string(),
                class: FeatureClass::Volcano,
                discovered: false,
            },
            ChainLink {
                name: "Kxsokxkxzhakx".to_string(),
                class: FeatureClass::Landmass,
                discovered: true,
            },
        ];
        let text = format_chain(&links).expect("a nonempty chain resolves");
        assert!(
            text.contains("an unnamed volcano"),
            "the undiscovered inner link's class is still honest, got {text:?}"
        );
        assert!(
            !text.contains("Vngashngatva"),
            "the undiscovered inner link's real name must NOT leak, got {text:?}"
        );
        assert!(
            text.contains("Kxsokxkxzhakx (a landmass)"),
            "the discovered outer link's real name must still show, got {text:?}"
        );
    }

    /// An empty chain resolves to `None` — mirrors `an_empty_index_resolves_to_none`.
    #[test]
    fn format_chain_of_no_links_is_none() {
        assert_eq!(format_chain(&[]), None);
    }

    // -- Task 5: the real gate, wired through `resolve_chain_at` itself,
    //    not only `format_chain`'s hand-built fixture ---------------------

    /// `resolve_chain_at` genuinely asks its `is_discovered` predicate per
    /// link, against REAL seed-42 data — a predicate that always refuses
    /// must produce a chain with no real name anywhere in it (every link
    /// falls back to `describe`'s "an unnamed <class>" placeholder), and a
    /// predicate that always accepts must produce the exact same text the
    /// existing `the_chain_names_every_feature_most_specific_first` test
    /// already pins for `&|_| true`. This is what distinguishes "the gate
    /// exists" from "the fixture that hand-builds `ChainLink`s exists" —
    /// the two are not the same claim, and only this test exercises the
    /// former.
    #[test]
    fn resolve_chain_at_genuinely_asks_its_predicate() {
        let seed = Seed(42);
        let geo = Geosphere::new(LEVEL);
        let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
            .expect("default pins generate");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        let features = crate::gazetteer_features(seed, &geo, &terrain);
        let index = VertexFeatureIndex::build(&features);
        let (ph, morph) = test_phonology();

        let vertex = multi_feature_vertex(&index, &geo);
        let stack = index.at(vertex);

        let none_discovered =
            resolve_chain_at(&index, vertex, seed, PEOPLE, &ph, &morph, &|_| false)
                .expect("a multi-feature vertex still resolves to Some chain when undiscovered");
        for id in stack {
            let name = crate::feature_name(seed, *id, PEOPLE, &ph, &morph).roman;
            assert!(
                !none_discovered.contains(&name),
                "a real name leaked through an `is_discovered` predicate that always \
                 refuses: {none_discovered:?} contains {name:?}"
            );
        }

        let all_discovered = resolve_chain_at(&index, vertex, seed, PEOPLE, &ph, &morph, &|_| true)
            .expect("a multi-feature vertex resolves to Some chain when fully discovered");
        for id in stack {
            let name = crate::feature_name(seed, *id, PEOPLE, &ph, &morph).roman;
            assert!(
                all_discovered.contains(&name),
                "a real name failed to appear through an `is_discovered` predicate that \
                 always accepts: {all_discovered:?} missing {name:?}"
            );
        }

        // The two renders must actually differ — otherwise this test could
        // pass vacuously (e.g. if `stack` were empty, both loops above would
        // be no-ops and the assertions above would prove nothing).
        assert_ne!(
            none_discovered, all_discovered,
            "sanity: the predicate must actually change the drawn text"
        );

        // The predicate is asked PER LINK: a predicate that discovers only
        // the most-specific (first) link must show that link's real name
        // and no other link's.
        let first_id = stack[0];
        let only_first = resolve_chain_at(&index, vertex, seed, PEOPLE, &ph, &morph, &|id| {
            id == first_id
        })
        .expect("a multi-feature vertex resolves to Some chain");
        let first_name = crate::feature_name(seed, first_id, PEOPLE, &ph, &morph).roman;
        assert!(
            only_first.contains(&first_name),
            "the discovered link's real name must show: {only_first:?}"
        );
        for id in &stack[1..] {
            let name = crate::feature_name(seed, *id, PEOPLE, &ph, &morph).roman;
            assert!(
                !only_first.contains(&name),
                "an undiscovered link's real name must not leak: {only_first:?} contains \
                 {name:?}"
            );
        }
    }
}
