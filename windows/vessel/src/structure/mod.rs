//! A STRUCTURE: the sparse set of chamber-band places standing at one built
//! locale, and how they connect. Existence below the walk band is a predicate
//! (metaplan §1b.3 law 1) and this module is that predicate.
//!
//! Connectivity is the structure's OWN graph, never mesh adjacency — a deep
//! address is identity, not shape (law 3), so two chambers being triangle
//! neighbours means nothing and is not consulted.

pub mod grammar;
pub mod role;

use crate::brief::Brief;
use crate::depth::chamber_depth;
use crate::streams::{ROOM_CHAMBERS, ROOM_CHAMBERS_BUILT};
use hornvale_kernel::{Facet, Seed};
pub use role::{EVERY_ROLE, Role};

/// The most chambers one structure may have in v1. A bound, not a target: the
/// point of law 1 is that deep addresses are SPARSE, and an unbounded count
/// would make "every deep address is a place" true by accident.
/// type-audit: bare-ok(count)
/// plumb: universal(a v1 structural bound, not a physical or biological quantity)
pub const MAX_CHAMBERS: usize = 4;

// The collision scan in `structure_at` varies only the LAST base-4 digit
// (`Facet.path` holds child indices 0..4; `pack` rejects anything else), so
// it can only guarantee a free value while `MAX_CHAMBERS <= 4`: with at most
// `MAX_CHAMBERS - 1` prior chambers sharing a prefix, pigeonhole leaves one of
// the four digit values open. Raising `MAX_CHAMBERS` past 4 without widening the
// scan reintroduces an unbounded loop — so the coupling is asserted at compile
// time rather than left as a coincidence of two independent `4`s.
const _: () = assert!(MAX_CHAMBERS <= 4);

/// The sparse set of chambers standing at one built locale.
/// type-audit: bare-ok(index: links)
///
/// The fields are public and this type has no validating constructor, so the
/// invariants [`structure_at`] establishes are stated here — code in other
/// files depends on them, and a reader of that code cannot see them from there:
///
/// 1. **`threshold == chambers[0]`**, always. `structure_at` takes the first
///    drawn chamber as the threshold, so the entry point is index 0.
/// 2. **`links` is a rooted TREE at index 0**, every link `(parent, child)`
///    with `parent < child`; the path graph is the special case where every
///    chamber has one child. `Session::further_in` reads `children`.
/// 3. **`roles[i]` is the role of `chambers[i]`**; for a BUILT structure no
///    role appears twice, because the grammar admits each role once
///    ([`grammar::walk`] skips a role it already holds). A WILD structure is
///    the documented exception: its roles come from [`index_role`] alone, so a
///    three- or four-chamber cave reads `[Threshold, Hearthroom, Store]` or
///    `[Threshold, Hearthroom, Store, Store]` — the duplicate is the wild
///    reading exactly as it stood before The Cruck (spec §3.5), kept
///    byte-for-byte rather than "fixed", since no brief axis reaches a cave and
///    inventing a fourth wild role would move a derivation this campaign
///    promised not to touch. Consumers that need role uniqueness — `enter`'s
///    role-noun resolution — get it from the built path; on the wild path a
///    repeated noun simply matches more than one aperture and refuses, which is
///    the same answer an ambiguous prose noun gets.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Structure {
    /// The chamber `enter` arrives in from the locale.
    pub threshold: Facet,
    /// Every chamber, threshold included. Length is `1..=MAX_CHAMBERS`.
    pub chambers: Vec<Facet>,
    /// Undirected apertures as index pairs into `chambers`. Connected, so
    /// every chamber is reachable from `threshold`.
    pub links: Vec<(usize, usize)>,
    /// What each chamber is FOR, index-aligned with `chambers`. For a BUILT
    /// site these are [`grammar::frame_for`]'s roles, in the grammar's own
    /// admission order; for a WILD one they are [`index_role`]'s reading of the
    /// index and nothing else. See invariant 3 above for what that difference
    /// costs.
    pub roles: Vec<Role>,
}

impl Structure {
    /// The chamber this one hangs off: its unique lower-indexed neighbour, or
    /// `None` at the threshold. Invariant 2 makes "lower index" and "toward
    /// the door" the same direction.
    /// type-audit: bare-ok(index: i), bare-ok(index: return)
    pub fn parent(&self, i: usize) -> Option<usize> {
        self.links
            .iter()
            .find_map(|&(a, b)| (b == i).then_some(a).or((a == i && b < i).then_some(b)))
    }
    /// The chambers that hang off this one, ascending.
    /// type-audit: bare-ok(index: i), bare-ok(index: return)
    pub fn children(&self, i: usize) -> Vec<usize> {
        let mut out: Vec<usize> = self
            .links
            .iter()
            .filter_map(|&(a, b)| {
                if a == i && b > i {
                    Some(b)
                } else if b == i && a > i {
                    Some(a)
                } else {
                    None
                }
            })
            .collect();
        out.sort_unstable();
        out
    }
    /// Chambers in `i`'s subtree, `i` included — the share of a plan it needs.
    /// type-audit: bare-ok(index: i), bare-ok(count: return)
    pub fn subtree_size(&self, i: usize) -> usize {
        1 + self
            .children(i)
            .iter()
            .map(|&c| self.subtree_size(c))
            .sum::<usize>()
    }
}

/// The structure at `locale`, or `None` where there is no site to enter.
///
/// The draw is keyed to the locale's own seed — under `room/chambers/v1` for a
/// WILD site and `room/chambers/built/v1` for a BUILT one — so the same locale
/// in the same world always yields the same structure, and no other locale's
/// draw can perturb it.
///
/// **The brief is the gate for every site and the GRAMMAR's input for a built
/// one; it never touches which facets are drawn.** `brief.site` is the gate
/// (decision 0666) and `brief.built` is the METHOD selector — the same reading
/// `lattice::embed_with` takes of it. On the built path the brief reaches
/// [`grammar::frame_for`], which fixes the chamber COUNT, the ROLES and the
/// LINKS with no draw at all; the stream is spent only on which facets those
/// chambers stand at. On the wild path the brief is consulted for nothing past
/// the gate, so a cave and an exotic site at one locale draw identically.
///
/// Form follows use for a built site: the grammar says what stands here and the
/// seed places it. Use follows form for a wild one: the rock made the chain and
/// a people reads it (spec §1) — that path is byte-for-byte The Lintel's, under
/// the unchanged `room/chambers/v1`, and
/// `the_wild_path_is_pinned_before_the_cruck` is what holds it there.
/// type-audit: bare-ok(count: walk_depth)
pub fn structure_at(
    locale: &Facet,
    brief: &Brief,
    seed: Seed,
    walk_depth: u32,
) -> Option<Structure> {
    // `extra` below is an unchecked `u32` subtraction that underflows if
    // `locale` is ever deeper than `chamber_depth(walk_depth)`. It was
    // unreachable while nothing called this; the session's `enter` is the first
    // caller, so state the precondition where it can be caught in a debug run
    // instead of leaving it as a comment. Callers truncate (`truncate_to_walk`)
    // before calling.
    debug_assert_eq!(
        locale.depth(),
        walk_depth,
        "structure_at takes a WALK-band locale"
    );
    // Decision 0666: the gate is the SITE, not `built`. `built` still means
    // "a structure stands here" and is one property of a settlement; a cave
    // and an exotic site are enterable and were never built.
    brief.site.as_ref()?;
    let depth = chamber_depth(walk_depth);
    let extra = (depth - locale.depth()) as usize;
    let (count, roles, links, mut stream) = if brief.built {
        // The grammar fixes the shape and spends nothing: a count draw here
        // would put the facets of every built structure at the mercy of how
        // many rooms the brief admitted.
        let frame = grammar::frame_for(brief);
        let links = (1..frame.roles.len())
            .map(|i| {
                (
                    frame.parents[i].expect("every non-root chamber has a parent"),
                    i,
                )
            })
            .collect::<Vec<_>>();
        let stream = locale.seed(seed).derive(ROOM_CHAMBERS_BUILT).stream();
        (frame.roles.len(), frame.roles, links, stream)
    } else {
        let mut stream = locale.seed(seed).derive(ROOM_CHAMBERS).stream();
        // How many chambers: 1..=MAX_CHAMBERS, one draw.
        let count = 1 + (stream.next_u64() as usize) % MAX_CHAMBERS;
        let roles = (0..count).map(index_role).collect();
        // A path graph rooted at the threshold: minimal, connected, and honest
        // about being minimal.
        let links = (1..count).map(|i| (i - 1, i)).collect();
        (count, roles, links, stream)
    };
    // Which descendants: one draw per chamber, rejecting repeats by scanning
    // forward deterministically rather than re-drawing (a re-draw loop would
    // consume a variable number of draws and make the stream position depend
    // on collisions).
    let mut chambers: Vec<Facet> = Vec::with_capacity(count);
    for _ in 0..count {
        let draw = stream.next_u64();
        let mut candidate = child_path(locale, draw, extra);
        // Deterministic forward scan on collision: bump the last digit.
        while chambers.contains(&candidate) {
            let last = candidate.path.len() - 1;
            candidate.path[last] = (candidate.path[last] + 1) % 4;
        }
        chambers.push(candidate);
    }
    let threshold = chambers[0].clone();
    Some(Structure {
        threshold,
        chambers,
        links,
        roles,
    })
}

/// The wild reading: threshold, hearthroom, then stores — what `role_for` gave
/// a brief with no business and no notability, and the WHOLE of what a wild
/// site's roles are since The Cruck, Task 3. No brief axis reaches a cave
/// (spec §3.5), so a four-chamber cave reads `Store` twice; see [`Structure`]'s
/// invariant 3 for why that duplicate is kept rather than fixed. Also used
/// directly by fixtures that have no brief to consult (`lattice::render`,
/// `lattice::mod`'s own `structure_of`, and the `path_structure` test helper).
pub(crate) fn index_role(i: usize) -> Role {
    match i {
        0 => Role::Threshold,
        1 => Role::Hearthroom,
        _ => Role::Store,
    }
}

/// Extend `locale`'s path by `extra` child digits taken from `draw`, two bits
/// at a time. Integer only.
fn child_path(locale: &Facet, draw: u64, extra: usize) -> Facet {
    let mut path = locale.path.clone();
    for i in 0..extra {
        path.push(((draw >> (2 * i)) & 0b11) as u8);
    }
    Facet {
        face: locale.face,
        path,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::housemark::{AuthorityMark, Housemark, ThresholdPosture};
    use crate::site::{Site, SiteKind};
    use hornvale_history::record::{Function, Notability};
    use hornvale_kernel::Seed;

    const WALK: u32 = 13;

    fn locale() -> hornvale_kernel::Facet {
        hornvale_kernel::Facet {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        }
    }

    /// A structure built BY HAND from a link list and a role list — not a
    /// draw — so `parent`/`children`/`subtree_size` can be exercised over a
    /// chosen shape rather than whichever one a brief happens to derive.
    fn tree(links: &[(usize, usize)], roles: &[Role]) -> Structure {
        let chambers: Vec<Facet> = (0..roles.len())
            .map(|i| {
                let mut f = locale();
                f.path.extend(std::iter::repeat_n(
                    0u8,
                    crate::depth::CHAMBER_DEPTH_OFFSET as usize,
                ));
                let last = f.path.len() - 1;
                f.path[last] = i as u8;
                f
            })
            .collect();
        Structure {
            threshold: chambers[0].clone(),
            chambers,
            links: links.to_vec(),
            roles: roles.to_vec(),
        }
    }

    #[test]
    fn parent_children_and_subtree_read_the_link_graph_as_a_rooted_tree() {
        use Role::*;
        // T{ H{ W, S } } — the "deep" shape.
        let s = tree(
            &[(0, 1), (1, 2), (1, 3)],
            &[Threshold, Hearthroom, Loomroom, Store],
        );
        assert_eq!(s.parent(0), None);
        assert_eq!(s.parent(1), Some(0));
        assert_eq!(s.parent(3), Some(1));
        assert_eq!(s.children(0), vec![1]);
        assert_eq!(s.children(1), vec![2, 3]);
        assert_eq!(s.children(2), Vec::<usize>::new());
        assert_eq!(s.subtree_size(0), 4);
        assert_eq!(s.subtree_size(1), 3);
        assert_eq!(s.subtree_size(3), 1);
    }

    #[test]
    fn a_chain_is_the_tree_where_every_chamber_has_one_child() {
        use Role::*;
        let s = tree(&[(0, 1), (1, 2)], &[Threshold, Hearthroom, Store]);
        for i in 0..2 {
            assert_eq!(s.children(i), vec![i + 1]);
        }
        assert_eq!(s.children(2), Vec::<usize>::new());
    }

    /// A living, warm, communal, plain-postured agrarian dwelling: the BUSH
    /// shape, four chambers, `T{ H, W, S }`. The fullest frame the grammar
    /// derives without a Seat, so it is what the built path's general
    /// properties (distinctness, depth, connectivity, the collision scan) are
    /// asserted over — a thinner brief would derive two chambers and leave the
    /// scan unexercised.
    fn bush_brief() -> Brief {
        Brief::from_parts(
            Some(Function::Agrarian),
            None,
            Some(Notability::Common),
            None,
            Some(Housemark {
                authority: AuthorityMark::Common,
                threshold: ThresholdPosture::Plain,
            }),
            0,
            true,
            false,
            Some(Site::placed(SiteKind::Settlement, None)),
            None,
        )
    }

    /// The same dwelling with `cold: true` — the DEEP shape, `T{ H{ W, S } }`.
    /// One fire heats what opens off it, so the grammar nests the rooms on the
    /// hearth (spec §3.1).
    fn deep_brief() -> Brief {
        let mut b = bush_brief();
        b.cold = true;
        b
    }

    /// A waypoint: `Trade` keeps a store instead of a workroom, so three
    /// chambers, `T{ H, S }`.
    fn trade_brief() -> Brief {
        let mut b = bush_brief();
        b.function = Some(Function::Trade);
        b
    }

    /// A built site whose brief names no business at all: two chambers,
    /// `T{ H }`. The grammar's floor, and the built counterpart to the
    /// single-chamber caves the wild draw produces.
    fn no_business_brief() -> Brief {
        Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            true,
            true,
            Some(Site::placed(SiteKind::Settlement, None)),
            None,
        )
    }

    /// A brief carrying a cave: UNBUILT, but a site. This is the shape a real
    /// cave or exotic facet presents (`brief_of` builds exactly it), and it is
    /// the case that distinguishes the post-0666 gate from the one it replaced.
    fn cave_brief() -> Brief {
        Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            false,
            true,
            Some(Site::placed(SiteKind::Cave, None)),
            None,
        )
    }

    /// **The gate is the SITE, not `built`** (decision 0666), and a test that
    /// varies both together cannot say which one it reads.
    ///
    /// This was `an_unbuilt_locale_has_no_structure` and it passed a brief
    /// with `built: false` AND `site: None` — so it held under the old gate and
    /// the new one identically, and could not have caught the four doc comments
    /// this campaign left asserting the old precondition (see
    /// `lattice::anchor_cells`' module doc). The two cases are separated here:  // lexicon: a MODULE NAME, and the cells it names are chamber floor squares (areas), not mesh vertices
    /// no site is refused whatever `built` says, and an unbuilt SITE is
    /// admitted, which is the half that used to be impossible.
    #[test]
    fn the_site_gates_the_structure_and_built_does_not() {
        let no_site = Brief::from_parts(None, None, None, None, None, 0, false, true, None, None);
        assert!(
            structure_at(&locale(), &no_site, Seed(42), WALK).is_none(),
            "a facet with no site has nothing to enter"
        );
        // The discriminating case: unbuilt, and enterable anyway.
        assert!(
            structure_at(&locale(), &cave_brief(), Seed(42), WALK).is_some(),
            "a cave is unbuilt and IS a site — decision 0666 hangs the gate on \
             `Brief.site`, so this must derive a structure"
        );
    }

    /// **The wild path is ONE derivation for every wild kind**: a cave and an
    /// exotic site at one locale draw the same structure, because neither
    /// consults the brief for anything past `brief.site.as_ref()?`.
    ///
    /// This replaces `the_structure_drawn_is_the_same_whatever_kind_of_site_gates_it`,
    /// whose own doc predicted The Cruck would break it. It asserted
    /// settlement == cave; a settlement is BUILT now, so it runs the grammar
    /// under `room/chambers/built/v1` and its structure is deliberately
    /// different from a cave's at the same locale. What survives — and is
    /// what `lattice::anchor_cells`' grown corpus actually rests on — is that  // lexicon: a MODULE NAME, and the cells it names are chamber floor squares (areas), not mesh vertices
    /// the WILD half is uniform: the corpus derives its grown structures from
    /// a wild brief and embeds them with `grow`, so it represents production's
    /// caves and exotic sites exactly, whichever kind gated them.
    ///
    /// claim: invariant(forall-seed) — over 0..64 at one locale; the property
    /// is that no wild brief axis reaches the draw, so a single disagreeing
    /// seed falsifies it.
    #[test]
    fn a_cave_and_an_exotic_site_draw_the_same_structure() {
        let exotic = Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            false,
            true,
            Some(Site::placed(SiteKind::Exotic, None)),
            None,
        );
        for s in 0u64..64 {
            assert_eq!(
                structure_at(&locale(), &cave_brief(), Seed(s), WALK),
                structure_at(&locale(), &exotic, Seed(s), WALK),
                "seed {s}: the wild draw moved with the KIND of site gating it, so the \
                 grown anchor corpus no longer represents production"
            );
        }
    }

    /// **A built site takes its SHAPE from the brief and its FACETS from the
    /// seed** — the split The Cruck exists to make.
    ///
    /// Two briefs differing in one axis (`cold`) admit the same four rules, so
    /// the roles are equal and the TREE is not: warm hangs the workroom and the
    /// store off the door (the bush), cold nests them on the hearth (the deep).
    /// Then the converse, over the same brief at two seeds: the tree does not
    /// move and the facets do. Neither half alone would show the split —
    /// together they say the brief reaches the shape and nothing else, and the
    /// seed reaches the placement and nothing else.
    #[test]
    fn a_built_site_takes_its_shape_from_the_brief_and_its_facets_from_the_seed() {
        let warm = bush_brief();
        let cold = deep_brief();
        let a = structure_at(&locale(), &warm, Seed(42), WALK).expect("built");
        let b = structure_at(&locale(), &cold, Seed(42), WALK).expect("built");
        assert_eq!(a.roles, b.roles, "same rules admitted");
        assert_ne!(a.links, b.links, "cold nests the rooms on the hearth");
        assert_eq!(a.links, vec![(0, 1), (0, 2), (0, 3)]);
        assert_eq!(b.links, vec![(0, 1), (1, 2), (1, 3)]);
        // Facets: one draw per chamber under the built label — different seeds,
        // different facets, same tree.
        let c = structure_at(&locale(), &warm, Seed(7), WALK).expect("built");
        assert_eq!(a.links, c.links);
        assert_ne!(a.chambers, c.chambers);
    }

    /// **The built draw is exactly one per chamber, under the built label, and
    /// the count draw is gone.** Reproduced by hand rather than described: `n`
    /// draws under [`ROOM_CHAMBERS_BUILT`], through the same collision scan,
    /// must equal what `structure_at` returns. A count draw anywhere — or a
    /// draw taken under the wild label — shifts every facet and this fails.
    #[test]
    fn the_built_draw_is_one_per_chamber_under_the_built_label_and_none_under_the_wild_one() {
        let brief = bush_brief();
        let s = structure_at(&locale(), &brief, Seed(3), WALK).expect("built");
        let mut stream = locale()
            .seed(Seed(3))
            .derive(crate::streams::ROOM_CHAMBERS_BUILT)
            .stream();
        let extra = (chamber_depth(WALK) - WALK) as usize;
        let mut want: Vec<Facet> = Vec::new();
        for _ in 0..s.chambers.len() {
            let mut c = child_path(&locale(), stream.next_u64(), extra);
            while want.contains(&c) {
                let last = c.path.len() - 1;
                c.path[last] = (c.path[last] + 1) % 4;
            }
            want.push(c);
        }
        assert_eq!(s.chambers, want);
    }

    /// The threshold is still index 0 on the built path — the grammar's first
    /// admitted rule IS the threshold — and `roles` is the frame's own list,
    /// not a second reading of it. Pinned because `chamber_interior_of` and
    /// `enter`'s role-noun resolution both trust `roles[i]` directly.
    #[test]
    fn the_threshold_is_index_zero_and_the_roles_match_the_grammar() {
        let brief = bush_brief();
        let s = structure_at(&locale(), &brief, Seed(42), WALK).expect("built");
        assert_eq!(s.threshold, s.chambers[0]);
        assert_eq!(s.roles, crate::structure::grammar::frame_for(&brief).roles);
    }

    /// **Invariant 3, asserted on `structure_at`'s own output for BOTH paths**
    /// (Task 5a review note). The BUILT path inherits role-uniqueness from the
    /// grammar, which skips a role it already holds; the WILD path does NOT
    /// have it, and that is the documented exception rather than a defect —
    /// `index_role` yields `Store` at every index past 1, so a four-chamber
    /// cave reads `[Threshold, Hearthroom, Store, Store]`. Asserting the
    /// duplicate rather than describing it is what stops someone "fixing" the
    /// cave and moving `the_wild_path_is_pinned_before_the_cruck`'s derivation
    /// (which pins chambers and links, not roles, so it would NOT catch it).
    ///
    /// claim: invariant(forall-brief) over the four built shapes, plus a
    /// reachability probe for the four-chamber cave.
    #[test]
    fn no_role_repeats_on_the_built_path_and_the_wild_path_keeps_its_duplicate_stores() {
        for brief in [
            no_business_brief(),
            trade_brief(),
            bush_brief(),
            deep_brief(),
        ] {
            let s = structure_at(&locale(), &brief, Seed(42), WALK).expect("built");
            let distinct: std::collections::BTreeSet<Role> = s.roles.iter().copied().collect();
            assert_eq!(
                distinct.len(),
                s.roles.len(),
                "a built structure admitted a role twice: {:?}",
                s.roles
            );
        }
        let four = (0..64u64)
            .find_map(|seed| {
                let s = structure_at(&locale(), &cave_brief(), Seed(seed), WALK)?;
                (s.chambers.len() == MAX_CHAMBERS).then_some(s)
            })
            .expect("some seed in 0..64 draws a four-chamber cave");
        assert_eq!(
            four.roles,
            vec![Role::Threshold, Role::Hearthroom, Role::Store, Role::Store],
            "the wild reading is `index_role` alone and repeats Store — spec §3.5"
        );
    }

    #[test]
    fn a_built_locale_has_a_bounded_chamber_set() {
        let s = structure_at(&locale(), &bush_brief(), Seed(42), WALK).expect("built");
        assert!(
            (1..=MAX_CHAMBERS).contains(&s.chambers.len()),
            "sparseness: got {} chambers",
            s.chambers.len()
        );
    }

    #[test]
    fn every_chamber_sits_at_the_chamber_depth_under_this_locale() {
        let s = structure_at(&locale(), &bush_brief(), Seed(42), WALK).expect("built");
        for c in &s.chambers {
            assert_eq!(c.depth(), chamber_depth(WALK));
            assert_eq!(c.face, locale().face);
            assert_eq!(c.path[..WALK as usize], locale().path[..]);
            assert!(c.pack().is_ok(), "a chamber must pack: {c:?}");
        }
    }

    #[test]
    fn chambers_are_distinct() {
        let s = structure_at(&locale(), &bush_brief(), Seed(42), WALK).expect("built");
        let ids: std::collections::BTreeSet<u64> =
            s.chambers.iter().map(|c| c.pack().unwrap().0).collect();
        assert_eq!(ids.len(), s.chambers.len(), "no chamber may repeat");
    }

    /// **An agrarian brief's own business stands at chamber index 2, and the
    /// store at index 3** — the fact `interior::pattern`'s
    /// `a_key_is_drawn_where_no_strongbox_is` points here for, restated over
    /// the grammar.
    ///
    /// This was `chamber_two_differentiates_on_the_briefs_business_at_the_index_role_for_used`,
    /// asserted against the interim `chamber_role` shim (The Cruck, Task 5a).
    /// The shim is gone: `structure_at`'s built path now takes `roles` from
    /// [`grammar::frame_for`] whole. The CLAIM is unchanged and is still worth
    /// its own test here rather than only in `grammar.rs`, because what
    /// `pattern.rs` and `chamber_interior_of` actually read is
    /// `structure_at`'s output, not a `Frame` — and the two could come apart
    /// (a `roles` built by a second reading of the index, say) with every
    /// grammar test still green.
    ///
    /// It no longer needs a seed SEARCH: the built count is the grammar's, so
    /// an agrarian brief derives four chambers at every seed. That is itself
    /// the change, stated as an assertion.
    #[test]
    fn an_agrarian_brief_stands_its_loomroom_at_index_two_and_its_store_at_index_three() {
        let s = structure_at(&locale(), &bush_brief(), Seed(42), WALK).expect("built");
        assert_eq!(
            s.chambers.len(),
            MAX_CHAMBERS,
            "the count is the grammar's now, so an agrarian brief derives four \
             chambers at every seed rather than at some of them"
        );
        assert_eq!(
            s.roles[2],
            Role::Loomroom,
            "an agrarian brief's own business must reach chamber 2, exactly as \
             `role_for` gave it before The Cruck moved the read to the grammar"
        );
        assert_eq!(
            s.roles[3],
            Role::Store,
            "the store stands deeper than the business it keeps for, which is \
             what makes a possession walk THROUGH the key's room to the lock"
        );
    }

    /// claim: invariant(forall-seed) — determinism over a sweep, own doc: "a
    /// single seed could be pure by accident"
    #[test]
    fn derivation_is_pure() {
        // Spec §8 asks for purity over a SWEEP, not one case: a single seed
        // could be pure by accident of which draws it happens to take.
        for s in 0..8u64 {
            let a = structure_at(&locale(), &bush_brief(), Seed(s), WALK);
            let b = structure_at(&locale(), &bush_brief(), Seed(s), WALK);
            assert_eq!(a, b, "seed {s} derived two different structures");
        }
    }

    #[test]
    fn a_different_locale_gives_a_different_structure() {
        // NOTE the honest name. This asserts only LOCALE-keying, and it is
        // near-tautological: `child_path` clones the locale's path before
        // appending drawn digits, so two locales differ at the inherited prefix
        // whatever the draw does. Kept because the prefix-inheritance property
        // is itself worth pinning — but it does NOT show the seed is read.
        // `the_draw_is_keyed_to_the_world_seed` below is what covers that.
        let here = structure_at(&locale(), &bush_brief(), Seed(42), WALK).expect("built");
        let mut elsewhere_path = locale().path;
        elsewhere_path[0] = (elsewhere_path[0] + 1) % 4;
        let elsewhere = hornvale_kernel::Facet {
            face: 3,
            path: elsewhere_path,
        };
        let there = structure_at(&elsewhere, &bush_brief(), Seed(42), WALK).expect("built");
        assert_ne!(
            here.chambers, there.chambers,
            "a structure is keyed to its own locale"
        );
    }

    /// claim: reachability(seed: 0..8) — non-degeneracy: the seed is read at all
    #[test]
    fn the_draw_is_keyed_to_the_world_seed() {
        // Hold the locale FIXED and vary only the seed. Asserting that two
        // arbitrary seeds differ would be a coin flip on a small space (a count
        // in 1..=4 plus 18 drawn bits), so assert the weaker non-flaky property
        // that actually matters: the seed is read at all.
        let l = locale();
        let b = bush_brief();
        let structures: Vec<_> = (0..8u64)
            .map(|s| structure_at(&l, &b, Seed(s), WALK).expect("built"))
            .collect();
        assert!(
            structures.iter().any(|s| *s != structures[0]),
            "eight seeds at one locale produced identical structures — the draw ignores the world seed"
        );
    }

    #[test]
    fn the_threshold_is_a_chamber_and_the_graph_is_connected() {
        let s = structure_at(&locale(), &bush_brief(), Seed(42), WALK).expect("built");
        assert!(s.chambers.contains(&s.threshold));
        // Every chamber reachable from the threshold by `links`.
        let ti = s.chambers.iter().position(|c| *c == s.threshold).unwrap();
        let mut seen = std::collections::BTreeSet::from([ti]);
        let mut frontier = vec![ti];
        while let Some(i) = frontier.pop() {
            for &(a, b) in &s.links {
                for (x, y) in [(a, b), (b, a)] {
                    if x == i && seen.insert(y) {
                        frontier.push(y);
                    }
                }
            }
        }
        assert_eq!(
            seen.len(),
            s.chambers.len(),
            "closure (§1b.8 rule 3): every chamber reachable from the threshold"
        );
    }

    /// One pinned row: the seed drawn, the packed chamber ids in draw order,
    /// and the links among them.
    type PinnedRow = (u64, Vec<u64>, Vec<(usize, usize)>);

    /// H5's instrument: a byte-literal pin of what today's `structure_at`
    /// draws for a wild (cave) site, taken before The Cruck changes anything.
    /// Task 3 must keep this green without editing it.
    ///
    /// claim: structural(seed: 0..16, locale: 2) — byte-identity pin, not a
    /// swept property.
    #[test]
    fn the_wild_path_is_pinned_before_the_cruck() {
        let mut elsewhere_path = locale().path;
        elsewhere_path[0] = (elsewhere_path[0] + 1) % 4;
        let elsewhere = hornvale_kernel::Facet {
            face: 3,
            path: elsewhere_path,
        };
        let locales = [locale(), elsewhere];

        let mut got: Vec<PinnedRow> = Vec::new();
        for locale in &locales {
            for s in 0u64..16 {
                let st =
                    structure_at(locale, &cave_brief(), Seed(s), WALK).expect("a cave is a site");
                got.push((
                    s,
                    st.chambers.iter().map(|c| c.pack().unwrap().0).collect(),
                    st.links.clone(),
                ));
            }
        }
        let want: Vec<PinnedRow> = vec![
            (0, vec![622556418861347], vec![]),
            (
                1,
                vec![622556417883875, 622556418639907, 622556416105859],
                vec![(0, 1), (1, 2)],
            ),
            (2, vec![622556422406691, 622556416456931], vec![(0, 1)]),
            (3, vec![622556416918147], vec![]),
            (
                4,
                vec![
                    622556420144419,
                    622556420537443,
                    622556422376035,
                    622556418657251,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (5, vec![622556420530115, 622556417236227], vec![(0, 1)]),
            (
                6,
                vec![
                    622556417499939,
                    622556418160099,
                    622556419399779,
                    622556422222531,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (
                7,
                vec![622556422055587, 622556416185987, 622556419588739],
                vec![(0, 1), (1, 2)],
            ),
            (8, vec![622556420192355, 622556417625091], vec![(0, 1)]),
            (
                9,
                vec![622556417534339, 622556422982499, 622556423548867],
                vec![(0, 1), (1, 2)],
            ),
            (
                10,
                vec![622556419920579, 622556416539331, 622556418059139],
                vec![(0, 1), (1, 2)],
            ),
            (
                11,
                vec![
                    622556419311843,
                    622556422395555,
                    622556416461027,
                    622556423367491,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (12, vec![622556420211043, 622556421830723], vec![(0, 1)]),
            (
                13,
                vec![
                    622556417056163,
                    622556417343363,
                    622556422633955,
                    622556419365923,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (
                14,
                vec![
                    622556416540067,
                    622556423589411,
                    622556419984387,
                    622556423613027,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (15, vec![622556421578435], vec![]),
            (
                0,
                vec![763293907238819, 763293904956963, 763293910378787],
                vec![(0, 1), (1, 2)],
            ),
            (1, vec![763293907873411], vec![]),
            (
                2,
                vec![
                    763293904954467,
                    763293906376259,
                    763293906421411,
                    763293912219427,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (
                3,
                vec![
                    763293909329667,
                    763293908784867,
                    763293909697091,
                    763293911552451,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (4, vec![763293907690403], vec![]),
            (
                5,
                vec![763293911364451, 763293910621603, 763293904469283],
                vec![(0, 1), (1, 2)],
            ),
            (6, vec![763293906041027, 763293909991363], vec![(0, 1)]),
            (7, vec![763293909943011], vec![]),
            (
                8,
                vec![763293905323555, 763293909732899, 763293907215395],
                vec![(0, 1), (1, 2)],
            ),
            (
                9,
                vec![
                    763293906233123,
                    763293905286851,
                    763293909277251,
                    763293909213187,
                ],
                vec![(0, 1), (1, 2), (2, 3)],
            ),
            (10, vec![763293909420067], vec![]),
            (11, vec![763293909569507], vec![]),
            (12, vec![763293908689891, 763293912247267], vec![(0, 1)]),
            (
                13,
                vec![763293911048931, 763293906544835, 763293909724131],
                vec![(0, 1), (1, 2)],
            ),
            (
                14,
                vec![763293907857603, 763293904792099, 763293905285187],
                vec![(0, 1), (1, 2)],
            ),
            (15, vec![763293907626787, 763293904150691], vec![(0, 1)]),
        ];
        assert_eq!(got, want, "the wild draw under room/chambers/v1 moved");
    }
}
