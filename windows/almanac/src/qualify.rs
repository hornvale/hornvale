//! Render-time disambiguation of co-occurring same-named settlements — the
//! remedy decision 0024 names and defers.
//!
//! 0024 holds that **uniqueness is a property of a reference, not of a
//! name**: a committed settlement name is a pure per-`(seed, species, kind,
//! salt)` draw that may collide with another, exactly as Earth carries
//! forty-one Springfields, and *"any surface that would be ambiguous (a
//! document naming two same-named settlements) disambiguates at render time
//! from the entities' own site facts."* That is what this module does, and
//! all it does. It is a **view**: it reads the ledger, commits nothing,
//! derives nothing, draws nothing, and touches no save-format contract. No
//! stream, no epoch, no fact.
//!
//! # Lazy qualification
//!
//! The ambiguity scope is **one rendered document** — the roster of sites a
//! single `render_*` call will name in the string it returns — never the
//! world. Newcastle is "Newcastle" until a second Newcastle is in the room.
//! Two same-named settlements that never appear in the same document need no
//! qualifier, and get none; a document naming only one of a name's holders
//! pays nothing for the others' existence.
//!
//! The scope choice is what makes this affordable, and the gap is large. At
//! seed 42, 129 of the world's 169 settlements (76%) share their name with
//! another — but the `connections` documents name a mean of 3.95 sites each,
//! and 110 of the 169 carry no qualifier at all. Across those documents 301
//! of 667 named sites (45%) are qualified and 366 are left bare, at a mean
//! cost of +7.9 characters per named site. Qualifying every named site
//! unconditionally — the obvious alternative — would cost +15.4 per site,
//! roughly doubling every place name in the window to solve an ambiguity
//! that, in most documents, is not there. That is the length win The
//! Wearing exists for, and this module must not spend it.
//!
//! The same principle applies one step further out for documents that print
//! each site on a line carrying more than the name. The almanac's Land list
//! and the REPL's `settlements` already show the biome (and, for the REPL,
//! the population), so two same-named entries there are ambiguous only when
//! the whole line coincides — see [`SiteLabels::for_lines`], which those two
//! callers use and which is why the REPL qualifies 13 of 169 lines at seed
//! 42 rather than 129.
//!
//! # What a qualifier is made of
//!
//! Only the settlement's own committed site facts, per 0024 — its people
//! ([`hornvale_species::PEOPLED_BY`]), its biome
//! ([`hornvale_settlement::BIOME`]), and its coordinate
//! ([`hornvale_settlement::LATITUDE`]/[`hornvale_settlement::LONGITUDE`]).
//! Never a counter, an ordinal, or a disambiguating suffix: `Ice-Home-2` is
//! precisely what 0024 forbids, because it says nothing about the place.
//!
//! For each group of same-named sites in the roster, the first [`Rung`] that
//! *fully* separates the group wins. The ladder is ordered by how much the
//! qualifier tells a reader, not by how many characters it costs: "of the
//! kobolds" and "(taiga)" answer *which* place in the world's own terms,
//! while a coordinate answers only *where*, so the coordinate is the last
//! resort rather than the cheap default. Length discipline here comes from
//! not qualifying at all where there is no ambiguity — that is the whole
//! design — not from picking the shortest available qualifier; a shorter
//! qualifier that says less is a worse qualifier, not a better one. A
//! cost-ordered ladder was built first, measured, and discarded: across all
//! 169 of seed 42's `connections` documents it saved **69 characters** in
//! total (5210 against this ladder's 5279) and in exchange dropped the biome
//! rung from 17 firings to **zero**, because this world's biome names
//! ("temperate-rainforest") run longer than a coordinate does. Trading
//! "Xoxa (tropical-rainforest)" for "Xoxa (15.3°N, 106.1°W)" to save four
//! characters is a bad trade.
//!
//! A rung that separates only *some* of a group is rejected outright rather
//! than applied partially: a qualifier that leaves two of three "Roa"s still
//! identical has cost characters and bought nothing.
//!
//! Different groups in one document may therefore land on different rungs,
//! which is what natural languages do too — "Newcastle upon Tyne" beside
//! "Springfield, Illinois".
//!
//! **Which rung fires depends on how wide the document's roster is, and
//! that was measured rather than guessed.**
//!
//! [`Rung::People`] does **not** fire in the `connections` document at any
//! seed sampled (42, 1, 7, 21, 99, 404, 777 — 406 ambiguous groups, every
//! one of them single-people). That is structural rather than unlucky: a
//! route neighbourhood is one people's regional cluster, so its same-named
//! sites always agree on `peopled-by`. There the biome rung fires 17/301 at
//! seed 42 (2–18 elsewhere) and the coordinate rung carries the rest.
//!
//! It **does** fire in the two world-wide listings ([`for_lines`]'s
//! callers), whose roster spans every people at once: 2 of seed 7's 134
//! qualified Land entries, and 4 of seed 7's 18 qualified `settlements`
//! lines — plus 4 of 10 in the constant-sky seed-42 world, where two `Nee`s
//! of the same population and biome are separated as `Nee of the goblins` /
//! `Nee of the hobgoblins`. So the rung earns its keep; it is the *route
//! graph*, not the ladder, that never asks for it.
//!
//! [`for_lines`]: SiteLabels::for_lines
//!
//! # Determinism
//!
//! The roster is a [`BTreeSet`]; groups are a [`BTreeMap`] keyed by name;
//! rung choice walks a fixed-order array and stops at the first that
//! separates. No float comparison, no hashing, no wall clock. Same world and
//! same roster ⇒ same labels, byte for byte.

use hornvale_kernel::{CellId, EntityId, Value, World};
use std::collections::{BTreeMap, BTreeSet};

/// The disambiguating site facts this module reads for one settlement.
/// Every field is `Option` because a legacy save, or a hand-built fixture,
/// may carry a settlement without one of them; a [`Rung`] that needs a fact
/// no member of its group has simply is not a candidate.
struct SiteFacts {
    /// The settlement's committed name — the thing that may collide.
    name: String,
    /// The species that peoples it, if committed.
    people: Option<String>,
    /// The biome it sits in, if committed.
    biome: Option<String>,
    /// Its latitude/longitude in degrees, if both are committed.
    coordinate: Option<(f64, f64)>,
}

/// One way to qualify a name from its site facts. The two prose shapes are
/// 0024's own examples ("Ice-Home (taiga)", "Ice-Home of the kobolds");
/// [`Rung::Coordinate`] is the last resort for twins that agree on both.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Rung {
    /// "Ice-Home of the kobolds".
    People,
    /// "Ice-Home (taiga)".
    Biome,
    /// "Ice-Home of the kobolds (taiga)" — for a group where neither fact
    /// alone separates every member but the pair does.
    PeopleAndBiome,
    /// "Ice-Home (61.5°N, 12.2°W)".
    Coordinate,
}

/// The ladder, most-telling first — the first rung that separates a group
/// wins. People before biome because a settlement's people is the fact a
/// reader can act on ("go to the kobolds' Ice-Home") where a habitat gloss
/// only narrows; the pair before the coordinate because a coordinate says
/// where and nothing about what.
const RUNGS: [Rung; 4] = [
    Rung::People,
    Rung::Biome,
    Rung::PeopleAndBiome,
    Rung::Coordinate,
];

impl Rung {
    /// This rung's rendering of `facts`, or `None` when `facts` lacks the
    /// fact the rung is made of.
    fn apply(self, facts: &SiteFacts) -> Option<String> {
        let name = &facts.name;
        match self {
            Rung::People => Some(format!("{name} of the {}", plural(facts.people.as_ref()?))),
            Rung::Biome => Some(format!("{name} ({})", facts.biome.as_ref()?)),
            Rung::PeopleAndBiome => Some(format!(
                "{name} of the {} ({})",
                plural(facts.people.as_ref()?),
                facts.biome.as_ref()?
            )),
            Rung::Coordinate => {
                let (latitude, longitude) = facts.coordinate?;
                Some(format!(
                    "{name} ({})",
                    coordinate_phrase(latitude, longitude)
                ))
            }
        }
    }
}

/// Pluralize a people label naively (`kobold` → `kobolds`), matching
/// `history::pluralize`: the biosphere roster has no irregular plurals.
fn plural(people: &str) -> String {
    if people.ends_with('s') {
        people.to_string()
    } else {
        format!("{people}s")
    }
}

/// A coordinate as prose: `"61.5°N, 12.2°W"`. One decimal degree, chosen
/// against a measurement rather than a guess: over seeds 42, 1, 7, 21, 99,
/// 404 and 777 (953 settlements) no two settlements share a cell, and the
/// closest pair anywhere differs by 0.764° on at least one axis — 7.6× the
/// grain rendered here. So this rung separates in practice, and did so in
/// every one of those seeds' documents.
///
/// "In practice" is the honest word: nothing *enforces* it. If two sites did
/// render alike, their group would fail to separate and stay bare (see
/// [`SiteLabels::for_document`]) rather than gain a meaningless suffix.
///
/// Determinism: `f64::abs` is a sign-bit mask, not a libm call, and Rust's
/// float formatting is implemented in `core` — both are platform-identical.
/// The values themselves are read back from the ledger, so they are already
/// quantized (decision 0033).
fn coordinate_phrase(latitude: f64, longitude: f64) -> String {
    let north_south = if latitude < 0.0 { 'S' } else { 'N' };
    let east_west = if longitude < 0.0 { 'W' } else { 'E' };
    format!(
        "{:.1}°{north_south}, {:.1}°{east_west}",
        latitude.abs(),
        longitude.abs()
    )
}

/// The prose labels for every site one document names, qualified only where
/// that document is genuinely ambiguous.
///
/// Build one per rendered document from the roster of cells it will name
/// ([`SiteLabels::for_document`]), then ask it for each cell's label
/// ([`SiteLabels::label`]). Building it once per document rather than
/// resolving each cell independently is what makes the qualification
/// document-scoped at all — and it also costs one ledger scan instead of one
/// per named cell.
pub struct SiteLabels {
    /// Cell → the label that document should print for it. A cell holding no
    /// settlement is absent; [`SiteLabels::label`] renders it as a bare cell
    /// id.
    labels: BTreeMap<CellId, String>,
}

impl SiteLabels {
    /// Resolve the labels for a document that will name `cells`.
    ///
    /// `cells` is a roster, not a sequence: duplicates are collapsed, because
    /// naming one place twice (a neighbour reachable by both a sea-lane and a
    /// land route, say) is not an ambiguity. Order does not matter.
    ///
    /// Cells holding no settlement are left out — they render as bare cell
    /// ids and cannot collide. Where two settlements claim the same cell, the
    /// first in ledger commit order wins; that has not been observed in any
    /// generated world sampled (953 settlements over seven seeds, all on
    /// distinct cells), so this is defence, not a path anything walks.
    pub fn for_document(world: &World, cells: &[CellId]) -> SiteLabels {
        let lines: Vec<(CellId, String)> = cells.iter().map(|&c| (c, String::new())).collect();
        SiteLabels::for_lines(world, &lines)
    }

    /// Resolve the labels for a document that prints each site on its own
    /// line, with `also_shown` beside the name.
    ///
    /// Some documents already distinguish two same-named sites without any
    /// help: the almanac's Land list prints `- **Xoxa** — temperate-forest`,
    /// and the REPL's `settlements` prints `Xoxa — population 8 —
    /// temperate-forest`. Two entries there are ambiguous only when the name
    /// *and* the rest of the line coincide, so grouping is by the pair. This
    /// is the same principle one rung further out — spend a qualifier only
    /// where the document is actually ambiguous.
    ///
    /// Measured at seed 42, not estimated: 129 of the Land list's 169
    /// entries sit in a *name*-colliding group but only **120** sit in a
    /// (name, biome) one, so grouping by name alone would qualify **9**
    /// entries a reader could already tell apart from the line. The REPL's
    /// `settlements`, whose line also carries the population, needs only
    /// **13** of its 169 — population separates most colliding names
    /// outright.
    ///
    /// A rung whose fact is already inside `also_shown` needs no special
    /// handling and gets none: every member of such a group agrees on that
    /// fact by construction, so the rung renders identical labels and
    /// [`separating_rung`] rejects it. That is why the Land list can never
    /// produce `- **Roa (taiga)** — taiga`.
    ///
    /// A cell appearing more than once keeps its first `also_shown`; that
    /// would be a caller printing one place on two different lines, which
    /// neither caller does.
    /// type-audit: bare-ok(prose: lines)
    pub fn for_lines(world: &World, lines: &[(CellId, String)]) -> SiteLabels {
        let roster: BTreeSet<CellId> = lines.iter().map(|(c, _)| *c).collect();
        let facts = site_facts(world, &roster);
        let mut context: BTreeMap<CellId, &str> = BTreeMap::new();
        for (cell, shown) in lines {
            context.entry(*cell).or_insert(shown.as_str());
        }

        // Group by (name, rest of the line). `BTreeMap` keyed by that pair
        // gives a deterministic group order and a deterministic membership
        // order (the roster is already ascending by cell).
        let mut groups: BTreeMap<(&str, &str), Vec<CellId>> = BTreeMap::new();
        for (cell, site) in &facts {
            let shown = context.get(cell).copied().unwrap_or("");
            groups
                .entry((site.name.as_str(), shown))
                .or_default()
                .push(*cell);
        }

        let mut labels = BTreeMap::new();
        for ((name, _), group) in groups {
            if group.len() == 1 {
                // Unambiguous in this document: pay nothing.
                labels.insert(group[0], name.to_string());
                continue;
            }
            match separating_rung(&facts, &group) {
                Some(rendered) => {
                    for (cell, label) in group.iter().zip(rendered) {
                        labels.insert(*cell, label);
                    }
                }
                // No rung separates this group — every member agrees on
                // every site fact this module reads. Leaving them bare is
                // the honest outcome: a qualifier that does not qualify only
                // costs characters, and 0024 forbids the counter that would.
                None => {
                    for cell in &group {
                        labels.insert(*cell, name.to_string());
                    }
                }
            }
        }
        SiteLabels { labels }
    }

    /// The label this document should print for `cell`: its settlement's
    /// name, or `"cell N"` when the cell holds no settlement.
    ///
    /// The name carries a qualifier only if this document names another
    /// settlement of the same name — and, deliberately, not always even
    /// then: a group whose members agree on every site fact this module
    /// reads has no separating rung and stays bare, because 0024 forbids the
    /// counter that would separate it and a qualifier that does not qualify
    /// is only length. (Not the same as "unreachable": see
    /// `twins_alike_in_every_fact_have_no_separating_rung`. It does not
    /// arise in the worlds sampled, where the coordinate rung always
    /// separates.)
    ///
    /// A cell outside the roster the labels were built for also renders as a
    /// bare cell id — a caller that names a site it did not declare gets an
    /// honest, unqualifiable label rather than a silently unqualified name.
    /// type-audit: bare-ok(prose: return)
    pub fn label(&self, cell: CellId) -> String {
        self.labels
            .get(&cell)
            .cloned()
            .unwrap_or_else(|| format!("cell {}", cell.0))
    }
}

/// The rendering of `group` under the first [`Rung`] of [`RUNGS`] that gives
/// every member a distinct label, or `None` when no rung does.
///
/// A rung whose fact some member lacks yields `None` for that member and is
/// skipped entirely — a half-applied rung would leave part of the group
/// unqualified. A rung whose renderings are not all distinct is skipped for
/// the same reason: a qualifier that separates two of three "Roa"s has spent
/// characters and bought nothing.
fn separating_rung(facts: &BTreeMap<CellId, SiteFacts>, group: &[CellId]) -> Option<Vec<String>> {
    RUNGS.iter().find_map(|rung| {
        let rendered: Vec<String> = group
            .iter()
            .map(|cell| rung.apply(&facts[cell]))
            .collect::<Option<Vec<String>>>()?;
        let distinct: BTreeSet<&String> = rendered.iter().collect();
        (distinct.len() == group.len()).then_some(rendered)
    })
}

/// Read the site facts of every settlement standing on a cell in `roster`,
/// in one pass over the settlement roster rather than one pass per cell.
fn site_facts(world: &World, roster: &BTreeSet<CellId>) -> BTreeMap<CellId, SiteFacts> {
    let mut out: BTreeMap<CellId, SiteFacts> = BTreeMap::new();
    for settlement in hornvale_settlement::all_settlements(world) {
        let Some(Value::Number(n)) = world
            .ledger
            .value_of(settlement.id, hornvale_settlement::CELL_ID)
        else {
            continue;
        };
        let cell = CellId(*n as u32);
        if !roster.contains(&cell) || out.contains_key(&cell) {
            continue;
        }
        out.insert(
            cell,
            SiteFacts {
                name: settlement.name,
                people: hornvale_species::species_of(world, settlement.id),
                biome: text(world, settlement.id, hornvale_settlement::BIOME),
                coordinate: number(world, settlement.id, hornvale_settlement::LATITUDE)
                    .zip(number(world, settlement.id, hornvale_settlement::LONGITUDE)),
            },
        );
    }
    out
}

/// A `Text`-valued fact about `entity`, if committed.
fn text(world: &World, entity: EntityId, predicate: &str) -> Option<String> {
    match world.ledger.value_of(entity, predicate) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

/// A `Number`-valued fact about `entity`, if committed.
fn number(world: &World, entity: EntityId, predicate: &str) -> Option<f64> {
    match world.ledger.value_of(entity, predicate) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn facts(name: &str, people: &str, biome: &str, lat: f64, lon: f64) -> SiteFacts {
        SiteFacts {
            name: name.to_string(),
            people: Some(people.to_string()),
            biome: Some(biome.to_string()),
            coordinate: Some((lat, lon)),
        }
    }

    #[test]
    fn a_coordinate_names_its_hemispheres() {
        assert_eq!(coordinate_phrase(61.5, -12.25), "61.5°N, 12.2°W");
        assert_eq!(coordinate_phrase(-30.0, 44.5), "30.0°S, 44.5°E");
        assert_eq!(coordinate_phrase(0.0, 0.0), "0.0°N, 0.0°E");
    }

    #[test]
    fn a_rung_whose_fact_is_missing_renders_nothing() {
        let bare = SiteFacts {
            name: "Ice-Home".to_string(),
            people: None,
            biome: None,
            coordinate: None,
        };
        for rung in RUNGS {
            assert_eq!(rung.apply(&bare), None, "{rung:?} needs a fact it lacks");
        }
    }

    /// The ladder walks in [`RUNGS`] order and stops at the first rung that
    /// separates — deliberately NOT at the shortest one. Here the people rung
    /// separates and is much longer than the biome rung that also would; the
    /// people rung still wins. Reversing `RUNGS` reds this.
    /// claim: structural(seed: none) — false-positive seed-loop flag; `s` binds a
    /// &String, single hand-built fixture, no world seed
    #[test]
    fn the_ladder_stops_at_the_first_separating_rung_not_the_shortest() {
        let both_separate = BTreeMap::from([
            (CellId(1), facts("Ice-Home", "hobgoblin", "bog", 11.0, 21.0)),
            (CellId(2), facts("Ice-Home", "bugbear", "fen", 12.0, 22.0)),
        ]);
        let chosen = separating_rung(&both_separate, &[CellId(1), CellId(2)]).unwrap();
        assert_eq!(
            chosen,
            vec!["Ice-Home of the hobgoblins", "Ice-Home of the bugbears"],
        );
        // Non-vacuity: the rung it passed over really would have separated
        // them, and really is shorter.
        let biome: Vec<String> = [CellId(1), CellId(2)]
            .iter()
            .map(|c| Rung::Biome.apply(&both_separate[c]).unwrap())
            .collect();
        assert_eq!(biome, vec!["Ice-Home (bog)", "Ice-Home (fen)"]);
        assert!(
            biome.iter().map(|s| s.chars().count()).sum::<usize>()
                < chosen.iter().map(|s| s.chars().count()).sum::<usize>(),
            "the skipped rung was the cheaper one: {biome:?} vs {chosen:?}"
        );
    }

    /// A rung that separates only part of a group is rejected, not applied
    /// partially: two of these three share a people, so the people rung is
    /// out and the group falls through to the next rung that separates all
    /// three.
    #[test]
    fn a_partially_separating_rung_is_rejected() {
        let three = BTreeMap::from([
            (CellId(1), facts("Roa", "kobold", "taiga", 11.0, 21.0)),
            (CellId(2), facts("Roa", "kobold", "desert", 12.0, 22.0)),
            (CellId(3), facts("Roa", "gnoll", "steppe", 13.0, 23.0)),
        ]);
        let chosen = separating_rung(&three, &[CellId(1), CellId(2), CellId(3)]).unwrap();
        assert!(
            chosen.iter().all(|label| !label.contains("of the kobolds")),
            "the people rung cannot separate two kobold Roas: {chosen:?}"
        );
        assert_eq!(chosen, vec!["Roa (taiga)", "Roa (desert)", "Roa (steppe)"]);
    }

    /// The pair rung exists for the group no single fact separates: these
    /// three share a people pairwise and a biome pairwise, but no two share
    /// both.
    #[test]
    fn the_pair_rung_catches_what_neither_fact_catches_alone() {
        let three = BTreeMap::from([
            (CellId(1), facts("Roa", "kobold", "taiga", 11.0, 21.0)),
            (CellId(2), facts("Roa", "kobold", "desert", 12.0, 22.0)),
            (CellId(3), facts("Roa", "gnoll", "taiga", 13.0, 23.0)),
        ]);
        let chosen = separating_rung(&three, &[CellId(1), CellId(2), CellId(3)]).unwrap();
        assert_eq!(
            chosen,
            vec![
                "Roa of the kobolds (taiga)",
                "Roa of the kobolds (desert)",
                "Roa of the gnolls (taiga)"
            ]
        );
    }

    /// A group agreeing on every readable site fact has no separating rung.
    /// The caller leaves it bare rather than inventing a counter.
    #[test]
    fn twins_alike_in_every_fact_have_no_separating_rung() {
        let identical = BTreeMap::from([
            (CellId(1), facts("Roa", "kobold", "taiga", 1.0, 1.0)),
            (CellId(2), facts("Roa", "kobold", "taiga", 1.0, 1.0)),
        ]);
        assert_eq!(separating_rung(&identical, &[CellId(1), CellId(2)]), None);
    }
}
