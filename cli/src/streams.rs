//! Render the stream manifest: every seed-derivation label in the project,
//! and stamp a world with the versioned ones it was derived under.
//!
//! Both live here, over one roster ([`label_sources`]), because `cli` is the
//! only crate that can see every other crate's labels — which is why
//! `render_streams` was already here. A second hand-written list of labels is
//! exactly the drift `stream_labels!` was built to prevent, so the manifest
//! page and the stamp read the same one.

use hornvale_kernel::World;
use std::collections::BTreeMap;

/// Every registered crate's stream labels, in the order the manifest prints
/// them (alphabetical by crate).
///
/// Domain sections come from the single composition-root roster (`DOMAINS`),
/// which stores domains in registration order. Crates that draw streams but
/// are not domains — the kernel (substrate: room addressing) and the windows
/// with their own draws (locale, vessel) — are listed explicitly alongside
/// them.
fn label_sources() -> Vec<(&'static str, Vec<(&'static str, &'static str)>)> {
    let mut sources: Vec<(&'static str, Vec<(&'static str, &'static str)>)> =
        hornvale_worldgen::DOMAINS
            .iter()
            .map(|d| (d.crate_name(), d.stream_labels()))
            .collect();
    sources.push(("hornvale-kernel", hornvale_kernel::stream_labels()));
    sources.push(("hornvale-locale", hornvale_locale::stream_labels()));
    sources.push(("hornvale-vessel", hornvale_vessel::stream_labels()));
    sources.push((
        "hornvale-worldgen",
        hornvale_worldgen::streams::stream_labels(),
    ));
    sources.push(("hornvale-chronicle", hornvale_chronicle::stream_labels()));
    sources.sort_by(|a, b| a.0.cmp(b.0));
    sources
}

/// Render every registered crate's stream labels as the book's generated
/// reference page. Labels are permanent save-format contracts.
/// type-audit: bare-ok(artifact: return)
pub fn render_streams() -> String {
    let mut doc = String::new();
    doc.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->\n\n");
    doc.push_str(
        "Labels are permanent save-format contracts; regeneration uses epoch \
         suffixes (e.g. `settlement/name/v2`), never renames.\n\n",
    );
    for (crate_name, labels) in label_sources() {
        doc.push_str(&format!("### {crate_name}\n\n"));
        if labels.is_empty() {
            doc.push_str("*(no seed-derivation streams)*\n\n");
            continue;
        }
        doc.push_str("| Label | Meaning |\n|---|---|\n");
        for (label, meaning) in labels {
            doc.push_str(&format!("| `{label}` | {meaning} |\n"));
        }
        doc.push('\n');
    }
    doc.push_str("### hornvale-kernel (internal)\n\n");
    doc.push_str("| Label | Meaning |\n|---|---|\n");
    doc.push_str("| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |\n");
    doc
}

/// True for a `vN` path segment (`v1`, `v12`) and nothing else — notably not
/// for `vessel`, which also starts with `v`.
fn is_version_segment(s: &str) -> bool {
    s.len() > 1 && s.starts_with('v') && s[1..].bytes().all(|b| b.is_ascii_digit())
}

/// Split a stream label on its `/vN` segment, into the label without that
/// segment and the version number. `None` for an unversioned label.
///
/// The version can sit anywhere: `room/furnishing/v1` versions a leaf, while
/// `room/layout/v1/rectilinear` versions the family a leaf hangs under. Both
/// reduce to a stable stem plus a version, which is the whole point — see
/// [`World::derived_under`] for why a bump must be a value change rather than
/// a key change.
///
/// Panics on a label carrying two version segments, because there is then no
/// answer to which epoch it names, and a silently-picked one would be recorded
/// into every world written afterwards. The label roster is authored source,
/// so this is a declaration error, and the golden in
/// `the_stamp_is_exactly_this_roster` exercises the whole roster every run.
fn split_version(label: &str) -> Option<(String, u32)> {
    let mut versions = label.split('/').filter(|s| is_version_segment(s));
    let version = versions.next()?;
    assert!(
        versions.next().is_none(),
        "stream label {label} carries more than one /vN segment, so which epoch \
         it names is ambiguous"
    );
    let stem: Vec<&str> = label
        .split('/')
        .filter(|s| !is_version_segment(s))
        .collect();
    let n = version[1..]
        .parse()
        .expect("is_version_segment guarantees the rest is ascii digits");
    Some((stem.join("/"), n))
}

/// Every LIVE versioned seed-derivation label in the project, as
/// `label-without-version -> version`. The stamp a world carries.
///
/// Unversioned labels are omitted on purpose: they are structural and must
/// never move (decision 0073's argument for `room/face` and `room/child`), so
/// recording one adds a row that can never differ.
///
/// **The highest version for a stem wins, and that is load-bearing.** The
/// manifest roster keeps RETIRED labels as rows, marked only in their prose
/// (`language/<family>/lexicon/root/v3/<concept>` sits beside the live
/// `.../root/v4/<concept>`), so a stem can appear more than once. Taking
/// whichever the roster happened to list last would have recorded the retired
/// epoch as the current one in every world written from then on, and diffed
/// the first real bump against a wrong baseline. Epoch suffixes are monotonic
/// by construction — 0073 bumps and never renames — so the highest N for a
/// stem is the live one and the lower ones are the retirement record the
/// manifest keeps for readers.
/// type-audit: bare-ok(identifier-text: return)
pub fn versioned_labels() -> BTreeMap<String, String> {
    let mut highest: BTreeMap<String, u32> = BTreeMap::new();
    for (_crate_name, labels) in label_sources() {
        for (label, _meaning) in labels {
            if let Some((stem, n)) = split_version(label) {
                let slot = highest.entry(stem).or_insert(n);
                *slot = (*slot).max(n);
            }
        }
    }
    highest
        .into_iter()
        .map(|(stem, n)| (stem, format!("v{n}")))
        .collect()
}

/// Record `labels` on `world` as what it was derived under. Called at save
/// time by the composition root; takes the roster as an argument so a test can
/// hand it a synthetic one.
/// type-audit: bare-ok(identifier-text: labels)
pub fn stamp(mut world: World, labels: &BTreeMap<String, String>) -> World {
    world.derived_under = labels.clone();
    world
}

/// Which labels a world's stamp and the current roster disagree about, named.
///
/// Only labels present in BOTH sets, with different versions, count. A label
/// the stamp lacks is one that did not exist when the world was written, so
/// nothing it derives can have *moved*; and an empty stamp (a world written
/// before stamping) must therefore report nothing rather than everything.
/// Ordering is `BTreeMap`'s, so the answer is deterministic.
/// type-audit: bare-ok(identifier-text: then), bare-ok(identifier-text: now), bare-ok(identifier-text: return)
pub fn what_moved(then: &BTreeMap<String, String>, now: &BTreeMap<String, String>) -> Vec<String> {
    then.iter()
        .filter(|(label, version)| now.get(*label).is_some_and(|current| current != *version))
        .map(|(label, _)| label.clone())
        .collect()
}

/// The one line a reload prints before the first turn when an epoch has moved
/// something under the world's feet — or `None` when nothing moved, which is
/// the common case and must stay silent.
///
/// Amendment 1 §1a.5 wanted the consequence *stated*: the parenthetical is
/// derived from the diff, so the message names the label rather than issuing a
/// generic warning about an unspecified rearrangement. The subject varies with
/// it — claiming the rooms rearranged when a deity-naming stream moved would
/// be a new falsehood in place of the old vagueness.
///
/// # THE PAVEMENT'S EPOCH IS INVISIBLE TO THIS FUNCTION, AND THAT IS CORRECT
///
/// The Pavement (2026-08-31, decision
/// [0506](../../docs/decisions/0506-the-occupancy-lattice-is-a-cube-sphere.md))
/// replaced the occupancy lattice's triangular icosphere faces with
/// cube-sphere quads and moved the walk band from `globe_level + 6` to
/// `globe_level + 7` (decision 0511). Every `Facet` in the repository changed
/// MEANING. `reload_notice` reports **nothing** about it, and a reader who
/// expects otherwise has the mechanism backwards twice over:
///
/// 1. **This function diffs seed-derivation LABELS, and not one moved.** That
///    is the campaign's load-bearing claim rather than an oversight: the
///    occupancy lattice consumes no randomness, so terrain, climate and
///    settlement all still generate on the icosphere from the same streams in
///    the same order. Verified rather than asserted — `hornvale new --seed 42`
///    at `origin/main` and on the branch produced BYTE-IDENTICAL world files
///    (sha256 `e70ca3d0…`, 21,635 facts). A notice here would have been a
///    falsehood, because the world genuinely did not move; only the addressing
///    of where you stand in it did.
/// 2. **A world file therefore reloads cleanly, because a genesis ledger
///    carries no walk-band room ids.** What does NOT survive is anything that
///    recorded a PLACE: a `vessel/session/v2` snapshot's `room/<id>` knowledge
///    keys decode through `FacetId::unpack`, which refuses any face `>= 6`, so
///    a pre-epoch session fails loudly with the key named
///    (`windows/vessel/src/knowledge.rs`). That is decision 0189's shape and is
///    the real protection; this function was never it.
///
/// The note is here rather than in a chronicle because this is where a future
/// reader will come looking, having reasoned — correctly, from the code — that
/// an epoch ought to produce a notice, and needing to learn that an epoch which
/// moves no label is a real thing this project has now shipped once.
/// type-audit: bare-ok(identifier-text: then), bare-ok(identifier-text: now), bare-ok(prose: return)
pub fn reload_notice(
    then: &BTreeMap<String, String>,
    now: &BTreeMap<String, String>,
) -> Option<String> {
    let moved = what_moved(then, now);
    if moved.is_empty() {
        return None;
    }
    let subject = if moved.iter().any(|l| l.starts_with("room/")) {
        "The rooms are not as you remember."
    } else {
        "The world is not as you remember it."
    };
    Some(format!(
        "You have been away. {subject} ({})",
        moved.join(", ")
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    #[test]
    fn manifest_lists_every_crate_and_label() {
        let doc = render_streams();
        for expected in [
            "<!-- GENERATED FILE",
            "| `settlement/name` |",
            "| `settlement/placement` |",
            "| `terrain/plate-count` |",
            "| `language/<species>/name/settlement` |",
            "octave-{n}",
            "### hornvale-paleoclimate",
            "*(no seed-derivation streams)*",
            "### hornvale-kernel",
            "| `room/face` |",
            "### hornvale-worldgen",
            "### hornvale-chronicle",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn manifest_is_deterministic() {
        assert_eq!(render_streams(), render_streams());
    }

    #[test]
    fn manifest_sections_are_alphabetical_by_crate() {
        let doc = render_streams();
        // The domain section headers, in document order, must be sorted.
        let headers: Vec<&str> = doc
            .lines()
            .filter(|l| l.starts_with("### hornvale-") && !l.contains("kernel"))
            .collect();
        let mut sorted = headers.clone();
        sorted.sort_unstable();
        assert_eq!(
            headers, sorted,
            "manifest domain sections must be alphabetical"
        );
        // paleoclimate sorts between language and religion.
        let pos = |s: &str| headers.iter().position(|h| *h == s).unwrap();
        assert!(pos("### hornvale-language") < pos("### hornvale-paleoclimate"));
        assert!(pos("### hornvale-paleoclimate") < pos("### hornvale-religion"));
    }

    #[test]
    fn a_world_records_what_it_was_derived_under() {
        let stamped = stamp(World::new(Seed(42)), &versioned_labels());
        let json = serde_json::to_string(&stamped).unwrap();
        let back: World = serde_json::from_str(&json).unwrap();
        assert_eq!(back.derived_under, stamped.derived_under);
        assert!(
            back.derived_under
                .keys()
                .any(|k| k.starts_with("room/layout/")),
            "the stamp omits the labels this campaign declared, which is the \
             failure mode a genesis-time stamp would have had silently: {:?}",
            back.derived_under
        );
        assert!(
            back.derived_under.values().all(|v| v.starts_with('v')),
            "every recorded version is a /vN segment: {:?}",
            back.derived_under
        );
    }

    #[test]
    fn the_stamp_records_only_versioned_labels() {
        let stamp = versioned_labels();
        // `room/face` and `room/child` are structural and unversioned (0073):
        // recording them would add rows that can never differ.
        assert!(!stamp.contains_key("room/face"), "{stamp:?}");
        assert!(!stamp.contains_key("room/child"), "{stamp:?}");
        // Every stem the manifest declares a versioned label for is recorded,
        // and nothing else is. Counted off the roster rather than a second
        // hand-written list.
        let versioned_stems: std::collections::BTreeSet<String> = label_sources()
            .into_iter()
            .flat_map(|(_, labels)| labels)
            .filter_map(|(label, _)| split_version(label).map(|(stem, _)| stem))
            .collect();
        assert_eq!(
            stamp.keys().cloned().collect::<Vec<_>>(),
            versioned_stems.into_iter().collect::<Vec<_>>(),
            "{stamp:?}"
        );
    }

    #[test]
    fn the_stamp_is_exactly_this_roster() {
        // A golden, so that a new versioned label, a bump, or a retired label
        // colliding with its successor is a review decision rather than a
        // silent change to what every world written afterwards claims. It also
        // walks the whole roster through `split_version`'s ambiguity assert.
        //
        // The three naming stems read v3 as of The Wearing, and this line is
        // that review decision being taken rather than deferred. The bump is
        // real, deliberate and already declared with the epoch-suffix
        // discipline the Constitution requires (never a rename): see
        // `domains/language/src/lib.rs`, where `name/settlement/v2` is marked
        // retired and `name/settlement/v3` states what changed — the wear pass
        // over each morpheme, plus the RETIREMENT of v2's per-salt drawn stem
        // under decision 0024. This golden was authored on main while the
        // campaign's bump was in flight on its branch, so the two met for the
        // first time at the close merge; the roster is updated to record the
        // bump, and the assertion is left exactly as strong as it was.
        //
        // The Witness (F7, 2026-07-30) adds `language/<species>/lexicon/
        // cascade` at `v2` (and its `v2/wear` child): `draw_rule` becomes
        // position-aware, offering `Tonogenesis` only once a merger has been
        // drawn, so every cascade reseeds. See
        // `domains/language/src/streams.rs`'s `CASCADE_V2` doc for the full
        // reasoning, including why this is the leg that owes the bump and
        // not `name/settlement/v4` or `lexicon/root/v4`.
        //
        // The Contour (position-aware conflict, decision 0096) adds
        // `history/bake` at `v2`: the mechanism consumes no new draw, but it
        // changes every generated world's committed history, so the label
        // takes an epoch suffix per decision 0006 (an epoch suffix, never a
        // rename). See `domains/history/src/streams.rs`'s `BAKE` doc.
        //
        // The Granary (sub-year phase placement) bumps it to `v3`: raid
        // outcomes move when raids fire at store-trough phases, so committed
        // history changes while stream consumption order is untouched — the
        // same decision-0006 epoch suffix. See `domains/history/src/
        // streams.rs`'s `BAKE` doc.
        let rows: Vec<String> = versioned_labels()
            .into_iter()
            .map(|(k, v)| format!("{k} {v}"))
            .collect();
        assert_eq!(
            rows,
            vec![
                // The Deep Realm: the underworld chamber derivation. Born at
                // v1, versioned from birth like `settlement/disposition v1`
                // below, because its key (a ChamberAddr's vertex, entrance,
                // branch, band NAME and floor — see `windows/worldgen/src/
                // chamber.rs`'s `chamber_key`) is a save-format contract
                // the moment anything commits a chamber fact, which that
                // campaign deliberately did not do (spec §3.1/§3.3).
                //
                // **The Underworld bumps it to v2**, and this line is that
                // review decision being taken rather than deferred. The key
                // spells its band by NAME, and `ChamberAddr.band` stopped
                // naming a stratigraphic band (`regolith`/`cover`/…) and
                // started naming a delve rung (`undercroft`/`shallows`/…)
                // when spec §4.1 re-pointed the lattice's depth axis at the
                // heat-spaced ladder. Every chamber in every world therefore
                // re-derives — the exact case `chamber_key`'s own doc named
                // as "an epoch, not a fix to that assertion". `chamber/v1` is
                // retired and never reused.
                //
                // **The Stope bumps it to v3**, for three changes that each
                // re-key every chamber and therefore ride one epoch: the
                // address gained a `floor` (a band used to be one
                // interior-less point per column), `slot` was renamed
                // `branch`, and the deepest delve rung was renamed
                // `Sunless` -> `Nadir` — which matters here because the key
                // spells the rung's NAME. `chamber/v2` joins `chamber/v1` in
                // retirement; neither is ever reused.
                "chamber v3",
                // The Drift, Task 6 (spec §4.5): which branches of an
                // adjacent band a branch connects to — the edges descent
                // actually travels, which nothing drew before this campaign.
                // A NEW label, ADDITIVE at v1: it derives its own independent
                // stream and perturbs none of the ones around it, so no
                // existing key moves and no world's existing draws change.
                //
                // Versioned from birth like every sibling here, because its
                // KEY is a save-format contract: vertex, branch, band NAME and
                // the ROLE word (`child`/`parent`) — see
                // `windows/worldgen/src/chamber.rs`'s `descent_key`. The role
                // is in the key because one place in the lattice answers two
                // independent questions ("who is below me", "who am I below"),
                // and giving each its own stream is what keeps the two
                // surjections whose union guarantees connectivity from
                // sharing draws in a fixed order.
                "chamber/band-descent v1",
                // The Stope, Task 2: how many floors one RUN — the floors of
                // one branch within one band — realizes. A NEW label,
                // ADDITIVE at v1: it derives its own independent stream and
                // perturbs none of the ones above it, so `chamber/v3` stays
                // and no chamber address relocates. What it changes is which
                // addresses EXIST — before it, every in-budget run admitted
                // all `FLOORS_PER_RUN_CEILING` floors, and the lattice
                // ceiling was standing in for a distribution.
                //
                // Versioned from birth for the same reason `chamber` and
                // `entity/identity` are: its KEY is a save-format contract
                // (a RunAddr's vertex, entrance, branch and band NAME — see
                // `windows/worldgen/src/chamber.rs`'s `run_key`), and
                // re-shaping it re-decides how long every run in every world
                // is.
                //
                // **Its own leg, rather than a key shape under `chamber`,**
                // so a run draw and a chamber draw are separated by their
                // PARENT and not by the observation that one key is a strict
                // prefix of the other. See `windows/worldgen/src/streams.rs`'s
                // `RUN_FLOORS`.
                //
                // **The Drift bumps it to v2** (spec amendment A.3/A.6):
                // `entrance` left `RunAddr` entirely — an entrance is which
                // aperture a player used, not a coordinate in a system's own
                // lattice — so the key dropped a whole segment
                // (`vertex/branch/band` instead of `vertex/entrance/branch/band`).
                // Unlike `chamber v3` (a display formatter with no production
                // reader, per A.6), this IS a live production leg —
                // `levels_in_branch` is `chamber_exists`'s own drawn-length
                // gate — so the re-keying rides a real epoch. `chamber/
                // run-floors/v1` is retired and never reused.
                // **The Drift Task 5 bumps these three to v2** (spec
                // amendment A.3): `entrance` left each key entirely and
                // `band` moved in — a branch's character, barrier and count
                // are now facts about `(system, band)` rather than the
                // system as a whole, which is what lets one system be two
                // branches wide in the Undercroft and one wide in the
                // Shallows. All three are live production legs
                // (`chamber_exists`'s own branch gate reads `BRANCH_COUNT`
                // directly), so the re-keying rides a real epoch, same
                // discipline as `RUN_FLOORS`'s own v2 bump above.
                // `chamber/branch-barrier/v1`, `chamber/branch-character/v1`
                // and `chamber/branch-count/v1` are retired and never
                // reused. **`chamber/branch-root/v1` is GONE** (The Drift
                // Task 7, spec §4.6): `root_floor_of` retired with it, so
                // the label is not merely unchanged but absent — a leg
                // nothing derives from must not sit in a stamp claiming a
                // world reads it. Retired and never reused, like every
                // superseded label above.
                "chamber/branch-barrier v2",
                "chamber/branch-character v2",
                "chamber/branch-count v2",
                // The two Task 5 entrance legs (amendment C.3): how many
                // apertures a system opens, and which branch each opens on.
                // Both were additive NEW labels at v1, both keyed on stable
                // lattice places (vertex; vertex + entrance index). See
                // `windows/worldgen/src/streams.rs`'s ENTRANCE_COUNT /
                // ENTRANCE_MOUTH.
                //
                // **The Drift Task 7b bumps BOTH to v2** (spec amendment
                // E.2/E.4). §4.5's "every branch has a parent" is vacuous at
                // the TOP band, which has no band above it, so a top-band
                // branch no door landed on was orphaned along with
                // everything hanging beneath it — 120 of seed 42's 123
                // unreached levels. E.2 closes it by construction: every
                // top-band branch is named by an aperture. That costs both
                // legs a real discontinuity, and they are different ones:
                //
                //   - `entrance-count` keeps its key and changes its ANSWER
                //     — it now supplies the size of the FREE aperture set,
                //     which the shipped count raises to the top band's
                //     branch width. One key, two different quantities across
                //     the boundary.
                //   - `entrance-mouth` changes its KEY as well, gaining a
                //     `share`/`free` role word, because the sharing-out draw
                //     indexes a shrinking pool of unspoken-for branches
                //     while the free draw names any side branch. One key
                //     would have served two questions at two widths.
                //
                // Neither is an empty epoch (A.6's refusal): both are live
                // production legs every world derives every aperture from.
                // `chamber/entrance-count/v1` and `chamber/entrance-mouth/v1`
                // are retired and never reused.
                "chamber/entrance-count v2",
                "chamber/entrance-mouth v2",
                "chamber/run-floors v2",
                // The Toponym: a vertex's characteristic variant, what a
                // settlement there is named for. Additive — a new label
                // perturbs no existing stream.
                "climate/variant/cell v1",
                "climate/weather/phase v1",
                // The Weft, Task 7: the derived erratic/scatter surface's
                // noise root — the negative control, deliberately LOW
                // contextuality. A NEW label, additive at v1 —
                // position-keyed, never vertex-keyed — see
                // `windows/worldgen/src/streams.rs`'s `WEFT_ERRATIC`.
                "derived/erratic v1",
                // The Weft, Task 7: the derived overhang/hollow surface's
                // noise root — medium contextuality, not enterable but
                // affording shelter and fire. A NEW label, additive at v1 —
                // see `windows/worldgen/src/streams.rs`'s `WEFT_OVERHANG`.
                "derived/overhang v1",
                // The Weft: the derived spring/seep surface's noise root
                // (Task 5). A NEW label, additive at v1 — position-keyed,
                // never vertex-keyed — see
                // `windows/worldgen/src/streams.rs`'s `WEFT_SPRING`.
                "derived/spring v1",
                // The Weft, Task 7: the derived thicket/brake surface's
                // noise root — high contextuality, texture aimed at the
                // biome-monotony defect. A NEW label, additive at v1, same
                // posture as `derived/overhang` above — see
                // `windows/worldgen/src/streams.rs`'s `WEFT_THICKET`.
                "derived/thicket v1",
                // The Signet: the leg every entity id derives through
                // (`derive(parent, entity/identity/v1)` then the role
                // label). A NEW label, additive at v1 — it perturbs no
                // existing stream — but versioned from birth because it IS
                // the save-format contract for identity: bump it and every
                // entity in every world renumbers. See
                // `kernel/src/streams.rs`'s `ENTITY_IDENTITY`.
                "entity/identity v1",
                // The Repose: the per-vertex hazard-event draw. Additive at
                // v1 — a NEW label, perturbing no existing stream, and C0
                // commits nothing (spec §3.3: an event is recomputed on
                // demand and never stored, like the volcano below).
                // Versioned from birth because its KEY is a contract the
                // moment anything narrates an event: the vertex, the process,
                // and the index of a fixed 1,000-year block of world time —
                // NOT the window a caller asked about. Keying on the window
                // would make a narrower query draw an unrelated set rather
                // than a subset of a wider one, which is the defect
                // `windows/worldgen/tests/repose_laws.rs`'s sub-window
                // property exists to hold shut. See
                // `windows/worldgen/src/hazard.rs`'s `event_key`.
                "hazard/event v1",
                "history/bake v3",
                // The Salt re-keys the flesh seed from the occupation's
                // entity id onto its material core, so residue and
                // structures stop moving when an id moves. Taking the
                // review decision here rather than deferring it, because
                // this row sits at a genuine tension with decision 0084
                // and the reasoning should be legible later:
                //
                // 0084 DECLINED an epoch for `room/furnishing`, and one of
                // its three stated reasons was that the chamber composer
                // "is read only by the chamber renderer, which commits
                // nothing" — which describes `history/flesh` exactly. On
                // that reading this label should stay unversioned and this
                // row should not exist.
                //
                // It is recorded anyway, and the difference from 0084 is
                // the one that matters: there, the measurement came back
                // byte-identical — *nothing moved at all*, which is the
                // empty epoch 0089 warns against writing into the
                // manifest. Here the derivation genuinely moved (measured:
                // vertex 1400's rendered layer draws Seed(11388647889657673426)
                // under the old key and Seed(10641468697408252209) under
                // the new one), and a reader re-rendering `hornvale history
                // --site` off an older save under newer code gets different
                // prose with no other record of why. The stamp is that
                // record. It claims nothing about the save's BYTES, and
                // says so.
                "history/flesh v2",
                // The Burr: an alveolar trill is no longer gated behind the
                // exotic-consonant capability (a decision recorded at this
                // campaign's close). `assign_proto_roots` itself is
                // unchanged, but the phonology it draws
                // candidates from now offers a trill to any species, which
                // inserts extra candidate-consonant draws ahead of every
                // species' inventory — so every family's root assignment
                // reseeds, not only families whose daughters end up with a
                // trill. `ROOT_EPOCH` bumps to `v4` per decision 0089's
                // riding precedent: this branch's later tasks make further
                // consumption changes to the same draw before merge, so one
                // epoch suffix covers the whole campaign rather than one per
                // task.
                "language/<family>/lexicon/root/<concept> v4",
                "language/<species>/lexicon/cascade v2",
                "language/<species>/lexicon/cascade/wear v2",
                "language/<species>/name/deity v3",
                "language/<species>/name/epithet v3",
                "language/<species>/name/settlement v3",
                "religion/deity v2",
                "room/chambers v1",
                "room/furnishing v1",
                // The Sighting: where a chamber's ANCHORS land in its cells,
                // a third sibling in the layout family. Additive at v1 — a
                // NEW label, so it perturbs no existing stream — and it is
                // the label in this roster with the SMALLEST blast radius:
                // an anchor placement is FRAME-tier (decision 0069), never
                // serialized and never a fact's object, so bumping it could
                // not corrupt a saved world even in principle. Versioned
                // from birth anyway, because 0073 fixes epoch granularity at
                // declaration and an unversioned label can only gain a
                // version by a rename.
                "room/layout/anchors v1",
                "room/layout/grown v1",
                "room/layout/rectilinear v1",
                // The Winze, Task 4 (spec §4.3): whether one epoch's advance
                // of a working breaks through. Additive at v1 — a NEW label,
                // so it perturbs no existing stream and no world that exists
                // today consumed a draw under it — and versioned from birth
                // for the same reason `settlement/disposition v1` below is:
                // the draw shape (`1 - exp(-metres_cut / BREACH_FREE_PATH_M)`,
                // one draw per living working per epoch) and the
                // (vertex, band, year) key are both save-format contracts, and
                // changing either re-rolls which delvings broke through in
                // every world ever generated. See
                // `windows/worldgen/src/streams.rs`'s `SETTLEMENT_BREACH` for
                // why it is a keyed leg rather than a draw on
                // `history/bake/v3`.
                "settlement/breach v1",
                // The Tolerance: the per-settlement disposition draw, a
                // people's authored mind perturbed by its authored
                // dispersion. Additive at v1 — a NEW label, so it perturbs
                // no existing stream, but it is versioned from birth
                // because it will one day want an epoch: the draw shape
                // (uniform on ±√3σ, per-dimension independent, clamped to
                // [0, 1]) and the (site, founded-year) key are both
                // save-format contracts, and changing either changes every
                // settlement's mind. See
                // `windows/worldgen/src/disposition.rs`.
                "settlement/disposition v1",
                // The Roll, Task 3: the per-resident draw — a name salt, an
                // age and three MindVector dial deviations, on one fresh
                // stream per resident. Additive at v1 — a NEW label, so it
                // perturbs no existing stream — and versioned from birth for
                // the same reason `settlement/disposition v1` above is: the
                // draw shape (mirroring `disposition::perturb`) and the
                // (site, ordinal) key are both save-format contracts. See
                // `windows/worldgen/src/residents.rs`.
                "settlement/resident v1",
                // The Winze, Task 2: whether one expansion out of
                // `Bake::grow` is a WORKING rather than a farm — the
                // second siting objective that makes `Function::Mine`
                // reachable at all (spec §B.3). Additive at v1: a NEW
                // label on its own leg, so it consumes nothing from
                // `history/bake v3` and a world moves only where a
                // working is founded. Versioned from birth for the same
                // reason `settlement/disposition v1` is — the rate
                // (the site's own prospectivity) and the
                // (vertex, band, year) key are both save-format
                // contracts. See `windows/worldgen/src/streams.rs`.
                "settlement/working v1",
                // The Prospect, Task 5: where a placed site lands inside its
                // geosphere vertex's territory. Versioned from birth like the
                // other entries in this golden, and NOT an epoch for any
                // existing world — nothing derived from `site/placement/*`
                // before this campaign, so no world consumed a draw under it
                // and no existing stream's consumption order moves. What the
                // row records is that a `Brief` now reports an exotic site at
                // one facet per placed vertex instead of at none. See
                // `windows/worldgen/src/streams.rs`'s `SITE_PLACEMENT` doc for
                // why a draw rather than a threshold on the coarse mesh.
                "site/placement v1",
                // The Adit, Task 3: the cellular-automata cave content
                // generator (Karst-biased leaves). Versioned from birth
                // like the other entries in this golden. See
                // `windows/vessel/src/underworld_level/carve.rs`.
                // The Brattice: which cycle pattern a realm draws from the
                // frozen inventory. Additive at v1 — a NEW label, so it
                // derives its own stream and perturbs none of the four plan
                // legs. Versioned from birth like its siblings: the key IS a
                // save-format contract, because a descent key's identity is
                // a plan position (spec §5). Sorts before the
                // `underworld/level/*` block, not beside `underworld/plan/*`.
                // See `windows/worldgen/src/streams.rs`.
                "underworld/gate/pattern v1",
                "underworld/level/cellular v1",
                // The Adit, Task 6: deriving each rung's own seed within
                // one descent, drawn once per rung so two rungs never
                // restart their own generation from the same stream state.
                // Additive at v1 — a NEW label, so it perturbs no existing
                // stream. See
                // `windows/vessel/src/underworld_level/mod.rs`'s
                // `generate_descent`.
                "underworld/level/descent v1",
                // **underworld/level/partition v1 is GONE** (The Crosscut,
                // Task 3): the partition tree it drew is retired, nothing
                // derives from it, and a leg nothing reads must not sit in
                // a stamp claiming a world reads it — retired and never
                // reused, as chamber/branch-root/v1 was.
                // The Adit, Task 3: the partitioned-rooms content
                // generator, shared by the angular (Fracture-biased) and
                // rooms-and-corridors (worked) leaf styles — one label
                // because the underlying technique is one technique tuned
                // two ways, not two independent algorithms. See
                // `windows/vessel/src/underworld_level/carve.rs`.
                "underworld/level/rooms v1",
                // The Adit, Task 4: a leaf's worked-vs-natural and
                // algorithm-family draw, keyed to `CaveKind`/
                // `ChamberOrigin`. Additive at v1 — a NEW label, so it
                // perturbs no existing stream. See
                // `windows/vessel/src/underworld_level/mod.rs`'s
                // `choose_leaf_style`.
                "underworld/level/style v1",
                // The Adit, Task 3: the drunkard's-walk tunnel content
                // generator (LavaTube-biased leaves). See
                // `windows/vessel/src/underworld_level/carve.rs`.
                "underworld/level/tunneler v1",
                // The Crosscut, Task 1: the descent plan's four growth
                // legs — the spine (entrance and per-level stair cells), a
                // cycle's attachment/segment-length/floor choice, which
                // passage a series extension lengthens, and the shared
                // coordinate a stairway's two ends land on. All four are
                // NEW labels, additive at v1: each derives its own
                // independent stream and perturbs none of the others, so no
                // existing world's draws move. Versioned from birth like
                // every sibling here, because each key is a save-format
                // contract the moment growth reads it. See
                // `windows/worldgen/src/streams.rs`.
                "underworld/plan/cycle v1",
                "underworld/plan/extend v1",
                "underworld/plan/spine v1",
                "underworld/plan/stair v1",
                // The Repose: the volcano-identity derivation. Additive at
                // v1 — a NEW label, so it perturbs no existing stream, and
                // C0 commits nothing at all (spec §3.2 puts the whole object
                // in the phenomenon register: a volcano is recomputed on
                // demand and never stored). Versioned from birth for the
                // same reason `chamber v1` and `settlement/disposition v1`
                // above are: its KEY is a contract the moment anything
                // narrates or commits a volcano — the edifice's SOURCE
                // CONTACT vertex, never the query vertex (an edifice spans 1-2
                // vertices, so keying on the query vertex would give the two
                // halves of one mountain two identities and two names). See
                // `windows/worldgen/src/volcano.rs`'s `volcano_key`.
                "volcano v1",
            ]
        );
    }

    #[test]
    fn a_retired_label_never_outranks_its_live_successor() {
        // The manifest keeps `language/<family>/lexicon/root/v3/<concept>` as a
        // row (retired, superseded by root/v4) and lists it AFTER v4, so
        // last-write-wins would have recorded the retired epoch as current in
        // every world written from then on.
        assert_eq!(
            versioned_labels().get("language/<family>/lexicon/root/<concept>"),
            Some(&"v4".to_string())
        );
    }

    #[test]
    fn a_version_segment_anywhere_in_a_label_splits_off() {
        assert_eq!(
            split_version("room/furnishing/v1"),
            Some(("room/furnishing".to_string(), 1))
        );
        assert_eq!(
            split_version("room/layout/v1/rectilinear"),
            Some(("room/layout/rectilinear".to_string(), 1))
        );
        assert_eq!(split_version("room/face"), None);
        // A leading `v` is not a version segment without digits after it.
        assert_eq!(split_version("vessel/walk"), None);
        // Numeric, not lexicographic: v10 outranks v9.
        assert_eq!(split_version("a/v10"), Some(("a".to_string(), 10)));
    }

    #[test]
    fn a_changed_label_is_named_not_merely_noticed() {
        let mut then = versioned_labels();
        let k = then.keys().next().unwrap().clone();
        then.insert(k.clone(), "v0".to_string());
        let moved = what_moved(&then, &versioned_labels());
        assert_eq!(
            moved,
            vec![k],
            "the diff must name the label, not just report one"
        );
    }

    #[test]
    fn nothing_moved_when_the_rosters_agree() {
        assert!(what_moved(&versioned_labels(), &versioned_labels()).is_empty());
        assert_eq!(
            reload_notice(&versioned_labels(), &versioned_labels()),
            None
        );
    }

    #[test]
    fn an_unstamped_world_claims_nothing_moved() {
        // A world written before stamping existed has no basis for the claim,
        // so a reload must stay silent rather than warn about everything.
        let unstamped = World::new(Seed(42));
        assert!(what_moved(&unstamped.derived_under, &versioned_labels()).is_empty());
        assert_eq!(
            reload_notice(&unstamped.derived_under, &versioned_labels()),
            None
        );
    }

    #[test]
    fn a_label_the_stamp_never_knew_about_has_not_moved() {
        let then = BTreeMap::from([("room/furnishing".to_string(), "v1".to_string())]);
        let now = BTreeMap::from([
            ("room/furnishing".to_string(), "v1".to_string()),
            ("room/layout/grown".to_string(), "v1".to_string()),
        ]);
        assert!(what_moved(&then, &now).is_empty());
    }

    #[test]
    fn the_notice_names_the_rooms_when_a_room_label_moved() {
        let then = BTreeMap::from([("room/furnishing".to_string(), "v1".to_string())]);
        let now = BTreeMap::from([("room/furnishing".to_string(), "v2".to_string())]);
        assert_eq!(
            reload_notice(&then, &now).unwrap(),
            "You have been away. The rooms are not as you remember. (room/furnishing)"
        );
    }

    #[test]
    fn the_notice_does_not_blame_the_rooms_for_a_non_room_epoch() {
        let then = BTreeMap::from([("religion/deity".to_string(), "v2".to_string())]);
        let now = BTreeMap::from([("religion/deity".to_string(), "v3".to_string())]);
        let notice = reload_notice(&then, &now).unwrap();
        assert_eq!(
            notice,
            "You have been away. The world is not as you remember it. (religion/deity)"
        );
    }

    #[test]
    fn the_notice_lists_every_moved_label_deterministically() {
        let then = BTreeMap::from([
            ("room/layout/grown".to_string(), "v1".to_string()),
            ("room/layout/rectilinear".to_string(), "v1".to_string()),
        ]);
        let now = BTreeMap::from([
            ("room/layout/grown".to_string(), "v2".to_string()),
            ("room/layout/rectilinear".to_string(), "v2".to_string()),
        ]);
        assert_eq!(
            reload_notice(&then, &now).unwrap(),
            "You have been away. The rooms are not as you remember. \
             (room/layout/grown, room/layout/rectilinear)"
        );
    }
}
