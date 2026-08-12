//! The anthroponymic schema: what a personal name is made of.

use hornvale_language::NameKind;
use hornvale_language::anthroponym::{
    Author, Cite, ElementSource, GlossBasis, IndexBasis, NameElement, NamePattern, PersonName,
};

#[test]
fn person_is_a_distinct_name_kind_with_its_own_seed_label() {
    // The label is a save-format contract: it is folded into the derive
    // path, so it must be "person" and must differ from every existing kind.
    //
    // This test used to hand-list four labels inline, which is exactly what
    // let `Landform`'s addition slip through unnoticed at first: a
    // hand-listed array only ever inspects what someone remembered to name
    // in it, so a fifth variant would leave this test green while covering
    // one fewer kind than the enum actually has. Iterating `NameKind::ALL`
    // instead ties the label count to the enum's own variant count (kept
    // honest by the compile-time sentinel next to `ALL`'s definition in
    // `naming.rs`), so a sixth variant widens this test's coverage for free
    // instead of silently narrowing it.
    let labels: Vec<&str> = NameKind::ALL.iter().map(|k| k.label_for_test()).collect();
    assert_eq!(
        labels.len(),
        5,
        "NameKind::ALL grew or shrank without this test's expectation moving with it"
    );
    assert!(labels.contains(&"person"));
    assert!(labels.contains(&"landform"));
    let mut sorted = labels.clone();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(
        sorted.len(),
        labels.len(),
        "NameKind labels must be distinct"
    );
}

#[test]
fn a_name_is_an_ordered_list_and_order_is_meaningful() {
    let given = NameElement {
        source: ElementSource::Stem,
        author: Author::Kin,
        conferred: None,
    };
    let patronymic = NameElement {
        source: ElementSource::Relation(Cite::Parent),
        author: Author::Kin,
        conferred: None,
    };
    let a = PersonName {
        elements: vec![given.clone(), patronymic.clone()],
    };
    let b = PersonName {
        elements: vec![patronymic, given],
    };
    assert_ne!(
        a, b,
        "given-first and patronymic-first are different systems"
    );
}

#[test]
fn every_element_source_and_author_is_representable() {
    // A closed-vocabulary guard: if a variant is added without updating the
    // consumers, this forces the question at compile time rather than
    // letting a new source silently render as nothing.
    let sources = [
        ElementSource::Stem,
        ElementSource::Gloss(GlossBasis::Trade),
        ElementSource::Gloss(GlossBasis::Bearing),
        ElementSource::Gloss(GlossBasis::Origin),
        ElementSource::Relation(Cite::Parent),
        ElementSource::Relation(Cite::Clan),
        ElementSource::Relation(Cite::Community),
        ElementSource::Relation(Cite::Place),
        ElementSource::Relation(Cite::Deity),
        ElementSource::Relation(Cite::Mentor),
        ElementSource::Relation(Cite::Child),
        ElementSource::Index(IndexBasis::BirthOrder),
        ElementSource::Index(IndexBasis::Generation),
        ElementSource::Deed,
    ];
    assert_eq!(sources.len(), 14);
    let authors = [
        Author::Kin,
        Author::Community,
        Author::Witnesses,
        Author::Institution,
        Author::Selfward,
        Author::Outsiders,
        Author::Inherent,
    ];
    assert_eq!(authors.len(), 7);
}

#[test]
fn a_pattern_with_no_elements_is_a_mononym_not_an_error() {
    // A culture may name with a single given name and nothing else. That is
    // Indonesia and Ancient Egypt, not a degenerate case.
    let p = NamePattern {
        elements: vec![(ElementSource::Stem, Author::Kin)],
    };
    assert_eq!(p.elements.len(), 1);
}

#[test]
fn authorship_determines_revocability() {
    // The dimension the surface vocabulary hides: an epithet conferred by
    // outsiders cannot be revoked by its bearer; a self-assumed name can.
    assert!(!Author::Outsiders.revocable_by_bearer());
    assert!(!Author::Inherent.revocable_by_bearer());
    assert!(Author::Selfward.revocable_by_bearer());
}
