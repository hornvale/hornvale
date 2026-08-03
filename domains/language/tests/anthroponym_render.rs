//! The shortest-prefix render rule (decision 0024, generalized off
//! settlements): a name is uttered as the shortest element prefix that
//! disambiguates at the scope of the utterance.

use hornvale_language::anthroponym::{Rendered, render};

fn r(parts: &[&str]) -> Rendered {
    Rendered {
        parts: parts.iter().map(|s| s.to_string()).collect(),
    }
}

#[test]
fn alone_in_its_scope_a_name_renders_as_its_given_name() {
    let grushak = r(&["Grushak", "Bolgson", "Ironhand"]);
    assert_eq!(render(&grushak, &[]), "Grushak");
}

#[test]
fn a_collision_on_the_given_name_extends_by_exactly_one_element() {
    let a = r(&["Grushak", "Bolgson", "Ironhand"]);
    let b = r(&["Grushak", "Nardson", "Redeye"]);
    assert_eq!(render(&a, std::slice::from_ref(&b)), "Grushak Bolgson");
    assert_eq!(render(&b, &[a]), "Grushak Nardson");
}

#[test]
fn a_collision_two_deep_extends_twice() {
    let a = r(&["Grushak", "Bolgson", "Ironhand"]);
    let b = r(&["Grushak", "Bolgson", "Redeye"]);
    assert_eq!(render(&a, &[b]), "Grushak Bolgson Ironhand");
}

#[test]
fn two_identical_names_render_identically_rather_than_looping() {
    // Real anthroponymy collides and that is correct (decision 0024 accepts
    // a measured collision rate for settlements). Two figures with the same
    // full name are genuinely ambiguous; the renderer must terminate and
    // return the full stack, not spin looking for a distinguishing element.
    let a = r(&["Grushak", "Bolgson"]);
    let b = r(&["Grushak", "Bolgson"]);
    assert_eq!(render(&a, &[b]), "Grushak Bolgson");
}

#[test]
fn an_empty_name_renders_empty_rather_than_panicking() {
    assert_eq!(render(&r(&[]), &[]), "");
}

#[test]
fn a_competitor_that_shares_no_prefix_forces_no_extension() {
    let a = r(&["Grushak", "Bolgson"]);
    let b = r(&["Nard", "Vekson"]);
    assert_eq!(render(&a, &[b]), "Grushak");
}
