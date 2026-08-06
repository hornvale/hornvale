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
/// (`language/<family>/lexicon/root/v2/<concept>` sits beside the live
/// `.../root/v3/<concept>`), so a stem can appear more than once. Taking
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
        let rows: Vec<String> = versioned_labels()
            .into_iter()
            .map(|(k, v)| format!("{k} {v}"))
            .collect();
        assert_eq!(
            rows,
            vec![
                // The Deep Realm: the underworld chamber derivation. A NEW
                // label, additive at v1 — it perturbs no existing stream —
                // but versioned from birth like `settlement/disposition v1`
                // below, because its key (a ChamberAddr's cell, entrance,
                // band NAME and slot — see `windows/worldgen/src/
                // chamber.rs`'s `chamber_key`) is a save-format contract
                // the moment anything commits a chamber fact, which this
                // campaign deliberately does not do (spec §3.1/§3.3).
                "chamber v1",
                // The Toponym: a cell's characteristic variant, what a
                // settlement there is named for. Additive — a new label
                // perturbs no existing stream.
                "climate/variant/cell v1",
                "climate/weather/phase v1",
                "history/bake v2",
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
                // cell 1400's rendered layer draws Seed(11388647889657673426)
                // under the old key and Seed(10641468697408252209) under
                // the new one), and a reader re-rendering `hornvale history
                // --site` off an older save under newer code gets different
                // prose with no other record of why. The stamp is that
                // record. It claims nothing about the save's BYTES, and
                // says so.
                "history/flesh v2",
                "language/<family>/lexicon/root/<concept> v3",
                "language/<species>/lexicon/cascade v2",
                "language/<species>/lexicon/cascade/wear v2",
                "language/<species>/name/deity v3",
                "language/<species>/name/epithet v3",
                "language/<species>/name/settlement v3",
                "religion/deity v2",
                "room/chambers v1",
                "room/furnishing v1",
                "room/layout/grown v1",
                "room/layout/rectilinear v1",
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
            ]
        );
    }

    #[test]
    fn a_retired_label_never_outranks_its_live_successor() {
        // The manifest keeps `language/<family>/lexicon/root/v2/<concept>` as a
        // row (retired, superseded by root/v3) and lists it AFTER v3, so
        // last-write-wins would have recorded the retired epoch as current in
        // every world written from then on.
        assert_eq!(
            versioned_labels().get("language/<family>/lexicon/root/<concept>"),
            Some(&"v3".to_string())
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
