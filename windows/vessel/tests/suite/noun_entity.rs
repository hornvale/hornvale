//! The Chattel, Task 10 — the name → entity lookup.
//!
//! A catalog entry used to end at a `datum`: `examine water jar` printed a
//! String and the caller learned nothing it could act on. `Noun.entity` is
//! the other end of that lookup, and this file is its standing demonstration
//! — a typed word resolving to the entity a promotion actually minted, not to
//! an id the test invented.
//!
//! **The catalog here is built by the test, and that is a finding about the
//! seam rather than a convenience.** No production site claims an entity
//! today. Four construct a `Noun`: `focalize::TemplateFocalizer::render`
//! catalogs walk-band content (biome, regime, village, sky), `Session::
//! underground_nouns` catalogs rock, footing and a derived resident,
//! `Session::lens_nouns` unions in chart-legend marks, and `Session::examine`
//! builds a throwaway `Noun` for the sole purpose of running `matches`
//! against a chart-legend entry before returning `e.datum.clone()` — the
//! name → string dead end this campaign exists to close, in one line.
//!
//! **The blocker is semantic, not structural, and the difference decides
//! where the later tasks should look.** The plumbing is already there: a
//! `Vantage`'s `locale` carries `face: u8` and `path: Vec<u8>`, which are
//! exactly `Facet`'s two public fields, and `Session::position` returns a
//! `Facet` outright; `Session::underground_resident` already yields the same
//! `hornvale_kernel::KindId` an anchor carries, and the
//! resident branch already passes `kind.0` into `Noun::new`. What is missing
//! is a **promotion**. `thing::promote` has zero production callers
//! workspace-wide — every call is in `thing.rs`'s own tests, and
//! `passage.rs` calls `promote_role` instead — so no production site holds a
//! *minted* entity to claim, and deriving one from `thing_id` at a catalog
//! site would assert an identity the ledger never committed: exactly the
//! placeholder `Noun.entity`'s own doc says not to invent.
//!
//! So wiring the first production caller of `with_entity` is the later
//! tasks' work (the verbs that act on a resolved thing, and the wire), and
//! it needs a promotion at a catalog site rather than a signature or
//! plumbing change. The nearest thing in the tree to a real name → entity
//! lookup is `Session::examine_chamber`, which builds no `Noun` at all: it
//! resolves a typed word against a live anchor's own kind through
//! `chamber_prose::noun`. That is the shape a resolved thing wants, missing
//! only a minted identity to answer with. What this file pins is that the
//! field carries a **real** identity when a site does claim one, so the
//! caller that arrives has something true to inherit.
//!
//! The kind spelling is `hornvale_thing::kinds`' own handle — the same
//! roster the rest of the campaign resolves anchors through — so the word a
//! player types and the kind the entity was minted under come from one
//! source and cannot drift apart inside this test. It was
//! `affordance::thing_kind_of`'s until The Wicket deleted that mapping; the
//! source is one step nearer now, not one step further.

use hornvale_kernel::{ConceptRegistry, Facet, INSTANCE_OF, Ledger, WorldTime};
use hornvale_thing::kinds;
use hornvale_vessel::Noun;
use hornvale_vessel::thing::{promote, thing_id};

fn at(days: f64) -> WorldTime {
    WorldTime::from_std_days(days).expect("a small day count is in range")
}

/// A typed word resolves to the entity, not merely to a string. Before this
/// campaign the catalog yielded a `datum` and stopped, which is the whole of
/// the "no name -> entity lookup" gap The Offer named.
///
/// The `assert_ne!` is the one that makes the claim's *negative* half
/// visible. Two rooms' water jars carry the **same** display, the same datum
/// and the same words — a resolver that answers with strings cannot tell them
/// apart, and would report success either way. Only the entity separates
/// them, so `assert_ne!` on the entities beneath equal strings is the
/// statement "this lookup reaches identity" rather than "this lookup reaches
/// text".
///
/// The `assert_eq!` on `(display, datum, words)` immediately before it is a
/// guard on this test's OWN FIXTURE, not a probe of the system, and no change
/// to `src/` can make it fail: both `Noun`s are built from byte-identical
/// literals a few lines above and `Noun::new` is pure over its arguments, so
/// no production mutation can separate them. It earns its place by making the
/// `assert_ne!` mean something — without it a difference in the strings could
/// be carrying the inequality — and not by testing anything.
///
/// MUTATION THIS MUST FAIL AGAINST: make `with_entity` discard its argument
/// (`self.entity = Some(entity)` -> `self`). The first assertion goes red on
/// `None` and the test panics there, so that is the whole of the observed
/// failure: one failure at the first `assert_eq!`. The `assert_ne!` would
/// also be false under this mutation (two discarded arguments are equal) but
/// is never reached, and claiming both go red would be describing evidence
/// the run does not produce.
#[test]
fn a_typed_word_resolves_to_a_things_entity() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(INSTANCE_OF, false, "the kind a thing is an instance of")
        .expect("a fresh registry accepts the instance-of predicate");
    let mut ledger = Ledger::default();

    let here = Facet {
        face: 0,
        path: vec![1],
    };
    let elsewhere = Facet {
        face: 0,
        path: vec![2],
    };
    let kind = kinds::VESSEL.0;

    // Real promotions, in two rooms. The ids are what the ledger MINTED, not
    // values this test chose: a test that stores an invented `EntityId` and
    // reads it back proves the field holds a value, which is a weaker claim
    // than the one the campaign needs.
    let here_jar =
        promote(&mut ledger, &registry, &here, kind, 0, at(1.0)).expect("a shallow facet packs");
    let there_jar = promote(&mut ledger, &registry, &elsewhere, kind, 0, at(1.0))
        .expect("a shallow facet packs");

    let here_catalog =
        [Noun::new(kind, kind, "A wide-mouthed jar, standing half full.").with_entity(here_jar)];
    let there_catalog =
        [Noun::new(kind, kind, "A wide-mouthed jar, standing half full.").with_entity(there_jar)];

    let resolved = here_catalog
        .iter()
        .find(|n| n.matches(kind))
        .expect("the catalog resolves the word it was built from");
    assert_eq!(
        resolved.entity,
        Some(here_jar),
        "a typed word must resolve to the entity the promotion minted"
    );
    assert_eq!(
        resolved.entity,
        Some(thing_id(&here, kind, 0).expect("a shallow facet packs")),
        "and that entity is the one `thing_id` derives for the room and kind \
         — the lookup reaches the derivation, not a stored surprise"
    );

    let other = there_catalog
        .iter()
        .find(|n| n.matches(kind))
        .expect("the catalog resolves the word it was built from");
    assert_eq!(
        (&resolved.display, &resolved.datum, &resolved.words),
        (&other.display, &other.datum, &other.words),
        "the two entries must be string-identical, or the assertion below \
         could pass on a difference the entity had no part in"
    );
    assert_ne!(
        resolved.entity, other.entity,
        "two rooms' water jars share every string and are two things: the \
         entity is the only thing that separates them"
    );

    assert_eq!(
        Noun::new(kind, kind, "A wide-mouthed jar, standing half full.").entity,
        None,
        "a site with no entity to claim says nothing, rather than inventing a \
         placeholder"
    );
}
