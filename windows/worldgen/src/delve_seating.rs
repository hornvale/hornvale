//! Where in the delve ladder a people lives, and what a chamber there is worth
//! to it (The Underworld, Task 8; spec §4.6 and §4.2.1 clause 2).
//!
//! Three things live here, and they are one derivation read at three grains:
//!
//! 1. [`chamber_fit`] — how well one kind's [`EnvironmentNiche`] suits the
//!    underworld communities that occur in a given cave formation at a given
//!    depth class. Task 7 shipped `environment_fit` with no production
//!    consumer; this is that consumer.
//! 2. [`seat_at`] — which rung of one cell's column a people would settle, and
//!    the factor its capacity there is scaled by.
//! 3. [`seating_for`] — the whole map of both, one per people, which is what
//!    the deep-history bake keys its node index on.
//!
//! # The founding circularity, and why capacity does not gate on dryness
//!
//! Task 3 measured that the deepest rungs are dry on **0%** of cave-bearing
//! columns in every seed, and that no scale constant opens them: reach rises
//! with `induration` while porosity falls with it, so a deep cave is in rock
//! that cannot shed water (`underworld_water_table_probe.rs`). Spec §4.2.1
//! clause 2's answer is that a *made* chamber is dry regardless of the table.
//!
//! That creates a trap worth naming, because walking into it would leave the
//! drainage rule unreachable in exactly the case it was written for: **if
//! founding required the chamber to be dry already, no community could ever
//! found deep, so no community would ever make a chamber, so no chamber would
//! ever be dry.** The rule would be live, correct, and permanently
//! unreachable.
//!
//! So founding is **not** gated on dryness. It is priced on it. A rung whose
//! chamber is phreatic as *found* is still settleable; its capacity is scaled
//! by [`UNDERWORLD_WORKS_COST`], which is what the adits, wheels and drainage
//! levels cost the people that cut them. Dryness is a consequence of settling,
//! not a precondition for it — which is the same sentence §4.2.1 clause 2 uses
//! to say that a dwarven hall is something a people *does*.
//!
//! [`crate::chamber::is_sump`] is called here with `ChamberOrigin::Found`,
//! deliberately: the question capacity asks is "will this need works", not
//! "is this wet", and the `Made` answer is trivially `false` because the
//! settler is the one who will make it. This is that function's first
//! production caller — its own doc records that it had none.

use hornvale_climate::underworld::{DelveZone, underworld_assignment};
use hornvale_kernel::{CellId, CellMap, Geosphere};
use hornvale_species::{EnvironmentNiche, environment_fit};
use hornvale_terrain::{
    Cave, CaveKind, DelveRung, GeneratedTerrain, GeothermalGradient, delta_t_range_of, rungs,
    water_table_depth_m,
};

use crate::chamber::{
    ChamberAddr, ChamberOrigin, ChamberOverrides, SLOTS_PER_BAND, chamber_exists, is_sump,
    rung_rank,
};

/// What a settled people's capacity keeps, on a rung whose chamber is
/// phreatic as found — the price of the works that make it habitable.
///
/// **AUTHORED, not measured, and the number is a judgement about play rather
/// than a reading off any field.** Two things constrain it and neither fixes
/// it: it must be strictly greater than zero, or the deep is unreachable and
/// the drainage rule is dead on arrival (see this module's own docs); and it
/// must be strictly less than one, or drainage is free and a people has no
/// reason to prefer a dry seat to a flooded one. `0.5` is the midpoint of
/// that open interval — half of what the place would otherwise yield goes to
/// keeping it dry.
///
/// Spec §4.2.1 clause 3 is what licenses authoring it rather than deriving it:
/// this is a fantasy underworld, its scale is a design choice, and a constant
/// chosen for playability says so in its own doc. What would *measure* it is a
/// model of how much labour a working depth costs to dewater against how much
/// it yields, which is a mining-economy campaign this one is not.
/// type-audit: bare-ok(ratio)
pub const UNDERWORLD_WORKS_COST: f64 = 0.5;

/// The depth class one habitation rung names, in the vocabulary the underworld
/// corpus states its communities in.
///
/// The two rosters are a **mirrored pair** under decision 0094 — same names,
/// same order, different owners — and `cli/tests/delve_roster_mirror.rs`
/// already fails if they drift. This is the one place the correspondence is
/// spelled, and it is exhaustive on `DelveRung`, so a sixth rung fails to
/// compile here rather than silently scoring against the wrong depth class.
///
/// `Surface` is `None` for exactly the reason [`DelveZone`] has no `Surface`
/// variant: no underworld community is at the surface, so there is nothing for
/// a surface rung to be scored against.
fn zone_of(rung: DelveRung) -> Option<DelveZone> {
    match rung {
        DelveRung::Surface => None,
        DelveRung::Undercroft => Some(DelveZone::Undercroft),
        DelveRung::Shallows => Some(DelveZone::Shallows),
        DelveRung::Deeps => Some(DelveZone::Deeps),
        DelveRung::Underdeep => Some(DelveZone::Underdeep),
        DelveRung::Sunless => Some(DelveZone::Sunless),
    }
}

/// The corpus's spelling of one cave formation — the third leg of decision
/// 0094's duplicate roster, and the one nobody had joined.
///
/// **This exists because the two spellings are genuinely different words.**
/// `hornvale_terrain::CaveKind::name` answers `"karst"` / `"lava-tube"` /
/// `"fracture"` (its scene-emission legend), while
/// `hornvale_climate::underworld`'s `genera` carry the *surface corpus's*
/// formation names from `climate::axes` — `"karst-cave"` / `"lava-tube"` /
/// `"fracture-cave"` — because an underworld community is a community *of* one
/// of the formations The Axes already named. One of the three agrees by
/// coincidence, which is exactly why the mismatch survived: `lava-tube` matched
/// and looked like the rule working.
///
/// Exhaustive over `CaveKind` with no wildcard, following
/// `cli/tests/cave_kind_correspondence.rs`'s own enforcement pattern: a fourth
/// formation fails to compile here rather than silently falling through
/// [`chamber_fit`]'s genus-blind branch.
///
/// See [`chamber_fit`]'s "Genus first" paragraph for what went wrong while this
/// function did not exist, and `every_cave_kind_matches_a_corpus_genus` for the
/// assertion that would have caught it.
fn genus_of(cave: CaveKind) -> &'static str {
    match cave {
        CaveKind::Karst => "karst-cave",
        CaveKind::LavaTube => "lava-tube",
        CaveKind::Fracture => "fracture-cave",
    }
}

/// How well `niche` suits the underworld communities that occur in a `cave`
/// formation at `rung`'s depth class, in `[0, 1]` — or `None` where the corpus
/// describes nothing at that depth at all.
///
/// **The mean is taken over FITS, never over vectors, and that is a
/// correctness requirement rather than a preference.** `SUBSTRATE` is
/// `AxisValence::Nominal`: its six values index unordered classes, so the mean
/// of `S_SOIL` and `S_ORGANIC` is `S_EVAPORITE`, a rock type neither community
/// is made of. Averaging the corpus's vectors would compute exactly that
/// number, on the one axis the basis's own docs warn about. Averaging the
/// fits instead scores every community against its own authored point on the
/// authored grid and then asks how well, on average, this kind suits the
/// communities this kind of cave carries at this depth — which is a question
/// with an answer.
///
/// **Genus first, then depth alone.** The corpus's rows carry the cave
/// formations they are a community *of*, so a `karst-cave` row is the right
/// reading for a karst column. Where a formation has no row at a depth the
/// fallback is the same depth in *any* formation: the depth class is described
/// even where that formation's version of it is not, and refusing the rung
/// instead would make a cave kind's coverage gap look like uninhabitable rock.
/// `None` is reserved for a depth class the corpus does not describe at all,
/// which today never happens and is kept total rather than asserted.
///
/// **CORRECTED 2026-08-17, Task 9, before H2 was measured.** As Task 8 shipped
/// it, this function filtered on `cave.name()` — `"karst"` and `"fracture"` —
/// against corpus genera spelled `"karst-cave"` and `"fracture-cave"`, so the
/// genus branch matched for **one** formation of three and the other two fell
/// silently through to the genus-blind fallback. Karst and fracture columns
/// were therefore scored against *every* community at their depth, including
/// lava-tube ones, and the two formations returned bit-identical fit tables.
/// Nothing objected: this module's own `every_formation_and_depth_class_scores`
/// asks only that a fit *exists*, and the fallback always produces one. The
/// join now goes through [`genus_of`], and
/// `every_cave_kind_matches_a_corpus_genus` asserts the thing that was actually
/// false — that each formation's genus string occurs in the corpus at all.
///
/// **Unassigned rows are excluded, not scored as zero.** The two resisters
/// (`breakdown-fall`, `flood-pulse`) carry the empty vector, and
/// `environment_fit` correctly answers `0.0` for a place sharing no axis with
/// the niche. Folding that into the mean would read "the axes could not place
/// this community" as "this community suits nobody", which is the difference
/// between absence of evidence and evidence of absence.
///
/// The whole function ranges over `(3 formations × 5 depth classes)`, so a
/// caller may — and [`seating_for`] does — evaluate it once per people rather
/// than once per cell.
/// type-audit: bare-ok(ratio: return)
pub fn chamber_fit(niche: &EnvironmentNiche, cave: CaveKind, rung: DelveRung) -> Option<f64> {
    let zone = zone_of(rung)?;
    let genus = genus_of(cave);
    let assigned = || {
        underworld_assignment()
            .iter()
            .filter(move |n| n.zone == zone && !n.vector.is_unassigned())
    };
    let mut total = 0.0;
    let mut counted = 0usize;
    for name in assigned().filter(|n| n.genera.contains(&genus)) {
        total += environment_fit(niche, &name.vector);
        counted += 1;
    }
    if counted == 0 {
        for name in assigned() {
            total += environment_fit(niche, &name.vector);
            counted += 1;
        }
    }
    if counted == 0 {
        return None;
    }
    Some(total / counted as f64)
}

/// One people's seat in one cell's column: which rung it would settle, and the
/// factor its capacity there is scaled by.
/// type-audit: bare-ok(ratio: multiplier), bare-ok(flag: works), bare-ok(ratio: fit)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RungSeat {
    /// The rung settled.
    pub rung: DelveRung,
    /// The factor capacity at this cell is multiplied by — [`chamber_fit`],
    /// times [`UNDERWORLD_WORKS_COST`] where the chamber needs dewatering.
    pub multiplier: f64,
    /// Whether the chamber at that rung is phreatic **as found**, and so
    /// costs its settlers the works that keep it dry.
    pub works: bool,
    /// The undiscounted [`chamber_fit`] at that rung — the quantity the rung
    /// was **chosen** on, kept beside the quantity capacity is **scaled** by
    /// so a reader can tell the two apart. See [`seat_at`] for why they are
    /// two questions and not one.
    pub fit: f64,
}

/// The rung a people with this `niche` would settle in a column with this
/// `cave`, gradient and water table — the rung its niche **fits best** among
/// those the cave's own depth budget reaches.
///
/// "Best" is the largest [`chamber_fit`], tie-broken toward the **shallower**
/// rung: two rungs a people values equally are not equally cheap to reach, and
/// nothing else in the model prices descent, so the tie-break is where that
/// asymmetry is stated. `total_cmp`, never `>`, so the fold is total.
///
/// # The choice is made on fit, and the cost is charged afterwards
///
/// **This was measured, not assumed, and the other way round was tried
/// first.** The obvious alternative is to rank rungs by the whole multiplier —
/// fit *times* [`UNDERWORLD_WORKS_COST`] — so that a people weighs a good seat
/// against the price of draining it. Run over seeds 42 / 7 / 1234 that rule
/// seated **0 of 23** underworld communities below the water table, on any
/// rung, in any world: `Undercroft` is dry in 100% of cave-bearing columns
/// (`underworld_water_table_probe.rs`), so a dry seat is *always* available,
/// and at a flat halving no deeper rung's discounted score could beat it. The
/// drainage rule would then have had a producer that never once produced the
/// case it exists for — the fourth dangling seam this campaign is trying not
/// to create, arrived at by a route that looked like careful economics.
///
/// So the two questions are kept apart, which is also the better statement of
/// what a people is: **where it belongs is its niche; what that seat costs it
/// is the hydrology.** A kind whose identity is depth (spec §4.7's Mountain
/// and Duergar) then reaches the deep by being authored deep, rather than by
/// out-bidding a drainage constant nobody calibrated.
///
/// `None` only where the cave reaches no scorable rung, which cannot happen
/// for a real cave — `Undercroft` begins at ΔT = 0, so its top is 0 m in every
/// column and every cave reaches it — and is kept total rather than asserted.
///
/// The depth a rung is judged at is its own **top** (`delta_t_range_of(rung).0`
/// over the gradient), the same depth `chamber.rs` places a chamber at, so
/// this and the lattice cannot disagree about where a rung is.
/// type-audit: bare-ok(diagnostic-value: water_table_m)
pub fn seat_at(
    niche: &EnvironmentNiche,
    cave: &Cave,
    gradient: GeothermalGradient,
    water_table_m: f64,
) -> Option<RungSeat> {
    let mut best: Option<RungSeat> = None;
    for &rung in rungs().iter().filter(|r| **r != DelveRung::Surface) {
        let top_m = 1000.0 * delta_t_range_of(rung).0 / gradient.get();
        if top_m > cave.depth_reach_m {
            continue;
        }
        let Some(fit) = chamber_fit(niche, cave.kind, rung) else {
            continue;
        };
        // The question is "will this need works", asked of the chamber AS
        // FOUND. Asking it of a `Made` chamber would answer `false` by
        // definition — the settler is the maker — and price nothing.
        let works = is_sump(ChamberOrigin::Found, top_m, water_table_m);
        let multiplier = fit * if works { UNDERWORLD_WORKS_COST } else { 1.0 };
        let candidate = RungSeat {
            rung,
            multiplier,
            works,
            fit,
        };
        // Ranked on FIT, never on the multiplier — see this function's own
        // docs for the measurement that decided it. Shallower-first iteration
        // plus a strict `is_gt` keeps the first (shallowest) rung of any tie,
        // which is the stated tie-break.
        let better = match best {
            None => true,
            Some(b) => candidate.fit.total_cmp(&b.fit).is_gt(),
        };
        if better {
            best = Some(candidate);
        }
    }
    best
}

/// One people's seating over the whole globe: its rung at every cell, and the
/// factor its capacity there is scaled by.
///
/// Two `CellMap`s rather than one of pairs because the bake reads them at
/// different moments — the rung on every index lookup, the multiplier once,
/// when the capacity fields are built.
/// type-audit: bare-ok(ratio: multiplier)
pub struct Seating {
    /// The rung this people occupies at each cell. `Surface` everywhere for a
    /// surface people.
    pub rung: CellMap<DelveRung>,
    /// The factor this people's capacity at each cell is scaled by. `1.0`
    /// everywhere for a surface people, which is an IEEE-754 no-op.
    pub multiplier: CellMap<f64>,
}

impl Seating {
    /// The seating of a people that lives overhead: the `Surface` rung
    /// everywhere, at an untouched capacity.
    ///
    /// This is what makes the re-key inert for a surface people — one rung,
    /// one community per cell, and a multiplier that is exactly `1.0` rather
    /// than approximately so.
    pub fn all_surface(geo: &Geosphere) -> Seating {
        Seating {
            rung: CellMap::from_fn(geo, |_| DelveRung::Surface),
            multiplier: CellMap::from_fn(geo, |_| 1.0),
        }
    }
}

/// Where a people seats itself everywhere, given its niche (or the lack of
/// one) — the map the bake keys its node index on.
///
/// **A people with no authored [`EnvironmentNiche`] seats at `Surface`,
/// whatever realm it is in, and that absence is the campaign's positive
/// control.** A kind that cannot score a chamber cannot choose a rung, so
/// emptying `hornvale_species::environment_niche_registry` reverts the seating
/// — and only the seating — to the pre-campaign behaviour, without touching a
/// single capacity field. It is also the honest fallback: `rust-monster` and
/// `xorn` are subterranean and settle nothing, so there is nothing to seat.
///
/// A cell with no cave gets `Undercroft` at multiplier `0.0`. The rung there
/// is unobservable — the realm gate already zeroes a subterranean kind's
/// capacity on a caveless cell, so no community is ever opened at one — and
/// naming the shallowest habitation rung beats naming `Surface`, which would
/// put a subterranean people into the overworld's index at a cell it cannot
/// live in.
pub fn seating_for(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    niche: Option<&EnvironmentNiche>,
) -> Seating {
    let Some(niche) = niche else {
        return Seating::all_surface(geo);
    };
    let sea = terrain.sea_level().get();
    let seats: Vec<Option<RungSeat>> = geo
        .cells()
        .map(|cell| {
            let cave = terrain.cave_at(cell)?;
            let table = water_table_depth_m(
                terrain.drainage_at(cell),
                terrain.material_at(cell).porosity,
                terrain.elevation_at(cell).get() - sea,
            );
            seat_at(niche, &cave, terrain.geothermal_gradient_at(cell), table)
        })
        .collect();
    let at = |cell: CellId| seats[cell.0 as usize];
    Seating {
        rung: CellMap::from_fn(geo, |c| {
            at(c).map_or(DelveRung::Undercroft, |seat| seat.rung)
        }),
        multiplier: CellMap::from_fn(geo, |c| at(c).map_or(0.0, |seat| seat.multiplier)),
    }
}

/// The chambers a settled subterranean community makes — spec §4.2.1 clause
/// 2's **producer**, which the campaign bound to this task as an acceptance
/// criterion rather than deferring a fourth time.
///
/// Every address in the lattice at a community's own `(cell, rung)` that
/// exists at all resolves to [`ChamberOrigin::Made`]: keeping a working depth
/// dry is what settling underground *is*, so a people's own halls are cut,
/// not found. Consequently [`is_sump`] answers `false` for every one of them,
/// whatever the water table does — which is the whole point of the rule.
///
/// **The rung is re-derived rather than carried.** An [`Occupation`] records
/// its site and its people and nothing about depth, and adding a rung to it
/// would put a new field on a serialized type for information that is already
/// a pure function of `(niche, terrain, cell)`. Re-deriving it here through
/// the same [`seating_for`] the bake was handed is exact, not approximate:
/// same inputs, same function, same answer.
///
/// [`Occupation`]: hornvale_history::Occupation
///
/// Every record, alive or ended, contributes. An excavated extent survives its
/// maker — only the *claim* lapses — which is the persistence asymmetry
/// [`crate::chamber::resolve_origin`]'s docs name as what a future dig
/// campaign will be reading. A hall does not refill because its people died.
///
/// # NOTHING IN THE SHIPPED PATH CALLS THIS, AND THAT IS THE WHOLE DISCLOSURE
///
/// The only caller anywhere in the tree is `underworld_capacity_probe.rs`
/// (`#[ignore]`d); this module's own tests do not call it either.
/// `bake_history_from` does not call it; `windows/vessel`'s
/// `delve_at` — the one production caller of [`crate::chamber::chamber_at`]
/// anywhere in the tree — hands it a freshly-constructed **empty**
/// [`ChamberOverrides`]. **So in every world a player can reach, every chamber
/// still resolves `Found`.** Spec §4.2.1 clause 2 required this task to *write*
/// `Made`, and what shipped writes it into a value no shipped code path
/// constructs. That is a WRITER WITH NO CALL SITE, which is one level worse
/// than a value with no reader, and it is stated here rather than left to be
/// discovered.
///
/// **The vacuity argument that used to stand here is now HALF FALSE, and the
/// half that fell is the measurement.** `delve_at` still enters at a hardcoded
/// `band: 0` (`Undercroft`), `describe_underground_here` still renders
/// `"Ways on: out."`, the only other underground verb is still `climb`, and
/// [`crate::chamber::passages_from`] still has no production caller — so a
/// player still reaches exactly one chamber per column and it is always the
/// shallowest rung. All of that is unchanged.
///
/// What has changed is the number the argument rested on. Task 8 measured that
/// **every** settled underworld column in the campaign's three seeds seated at
/// `Shallows` (band 1) — 14 / 8 / 7 occupied columns, band histogram `{1: N}`,
/// band 0 holding ZERO in all three — and concluded that passing the real
/// overrides into a band-0 lookup would hand it an empty map in every measured
/// world.
///
/// **Task 9 repaired [`chamber_fit`]'s genus join and band 0 stopped being
/// empty.** Re-measured on the same three seeds with
/// `underworld_capacity_probe`: occupied-column band histograms `{0: 1, 1: 4}`
/// / `{0: 1, 1: 11}` / `{0: 15, 1: 10}` on seeds 42 / 7 / 1234 — so band 0
/// holds 1, 1 and 15 columns rather than none, and seed 1234 seats a clear
/// majority of its underworld columns at the one band a player can reach.
/// Drow's fit table now scores karst and fracture columns against their own
/// corpus rows instead of the genus-blind fallback, and its argmax in a
/// fracture column is `Undercroft`.
///
/// **The disclosure below therefore stands, but for a weaker reason than it
/// was given.** Wiring `delve_at` is no longer provably vacuous — it would now
/// resolve `Made` for a real, if small, set of columns. It is still not done
/// here, because the rest of the pricing is unchanged and unpaid: without a
/// descent verb the seam is closed for exactly one band out of five, which is
/// a partial closure presented as a whole one. A campaign that wants this
/// should read the paragraph below and do all of it.
///
/// **What closing it actually needs**, so the next campaign can price it: a
/// descent verb walking [`crate::chamber::passages_from`]; chamber state that
/// tracks an *address* rather than one `Chamber`; prose that distinguishes a cut
/// hall from a found void; and a home for the overrides themselves — either
/// derived per column off the committed ledger (`occupations_at` + [`seat_at`],
/// ~30 lines, cheap enough for one verb) or committed as dig facts, which
/// [`ChamberOverrides`]'s own doc defers. That is a `windows/vessel` campaign,
/// not a capacity task.
pub fn made_chambers(
    seed: hornvale_kernel::Seed,
    terrain: &GeneratedTerrain,
    history: &crate::history_bake::History,
    seating: &std::collections::BTreeMap<hornvale_kernel::KindId, Seating>,
) -> ChamberOverrides {
    let mut overrides = ChamberOverrides::new();
    for record in &history.records {
        let Some(seating) = seating.get(&record.core.people) else {
            continue;
        };
        let cell = record.core.site;
        let rung = *seating.rung.get(cell);
        let Some(band) = rung_rank(rung) else {
            continue; // a surface community cuts no chamber
        };
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        let gradient = terrain.geothermal_gradient_at(cell);
        for slot in 0..SLOTS_PER_BAND {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                band,
                slot,
            };
            if chamber_exists(seed, &cave, gradient, addr) {
                overrides.insert(addr, ChamberOrigin::Made);
            }
        }
    }
    overrides
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_species::environment_niche_registry;
    use std::collections::BTreeSet;

    fn drow() -> EnvironmentNiche {
        environment_niche_registry()
            .get(&hornvale_kernel::KindId("drow"))
            .cloned()
            .expect("drow carries an authored environment niche")
    }

    /// The corpus describes every formation at every depth class, or the
    /// genus-blind fallback covers it — either way a fit exists for all 15
    /// combinations, so no cave kind is silently unscorable.
    #[test]
    fn every_formation_and_depth_class_scores() {
        let niche = drow();
        for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            for &rung in rungs().iter().filter(|r| **r != DelveRung::Surface) {
                let fit = chamber_fit(&niche, kind, rung)
                    .unwrap_or_else(|| panic!("{kind:?} at {rung:?} has no fit"));
                assert!(
                    (0.0..=1.0).contains(&fit),
                    "{kind:?} at {rung:?} scored {fit}, outside [0, 1]"
                );
            }
        }
    }

    /// **The join decision 0094 leaves to a reader, asserted.** Every
    /// `CaveKind`'s corpus spelling ([`genus_of`]) must name at least one row
    /// of `underworld_assignment()`; a spelling that names none makes
    /// [`chamber_fit`]'s genus branch dead for that formation and silently
    /// promotes the genus-blind fallback into the answer.
    ///
    /// This is the assertion that was missing when Task 8 shipped, and it fails
    /// on the exact defect Task 9 found: two of the three formations were
    /// filtered on a name no corpus row carries.
    ///
    /// **It is NOT sufficient on its own, and neither is its sibling.** Both
    /// were written against the defect that was found, and a *transposed*
    /// mapping — `Karst => "fracture-cave"` — satisfies both: it names a real
    /// row, and karst still scores differently from fracture. That is this
    /// campaign's own theme (a guard that catches the instance rather than the
    /// class) landing on the fix for this campaign's theme, and it matters here
    /// because [`genus_of`] decides where every underworld people sits.
    /// `the_genus_extends_the_cave_kinds_own_name` below closes it.
    #[test]
    fn every_cave_kind_matches_a_corpus_genus() {
        for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            let genus = genus_of(kind);
            let rows = underworld_assignment()
                .iter()
                .filter(|n| n.genera.contains(&genus))
                .count();
            assert!(
                rows > 0,
                "{kind:?} is joined to the underworld corpus as {genus:?}, which no \
                 row carries — every column of this formation would fall through to \
                 the genus-blind fallback and be scored against other formations' \
                 communities"
            );
        }
    }

    /// **The mapping is RIGHT, not merely non-empty** — the guard that catches
    /// a transposition, which neither of its two siblings can.
    ///
    /// The corpus spells a cave formation as the `CaveKind`'s own name plus an
    /// optional `-cave` suffix (`karst` → `karst-cave`, `lava-tube` →
    /// `lava-tube`, `fracture` → `fracture-cave`), so the prefix relation is
    /// the naming convention itself and holds for all three. Every one of the
    /// five non-identity permutations of the mapping breaks it.
    ///
    /// **Why this rather than a structural join through
    /// `hornvale_climate::Formation`**, which would be better and was
    /// considered first: `Formation` carries no name. The corpus's `genera` are
    /// raw `&'static str` literals in `climate::axes`, so joining structurally
    /// means adding a `const fn Formation::name`, re-pointing three corpus
    /// constants at it, and deciding where a second `CaveKind` → `Formation`
    /// map may live given that decision 0094 puts the canonical one in
    /// `cli/tests/cave_kind_correspondence.rs` precisely to stop it being
    /// duplicated. That is a domain-crate API change and an 0094 question, not
    /// a guard; it is the right shape and it is recorded here rather than done
    /// under a fix round.
    #[test]
    fn the_genus_extends_the_cave_kinds_own_name() {
        let mut seen: BTreeSet<&'static str> = BTreeSet::new();
        for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            let genus = genus_of(kind);
            assert!(
                genus.starts_with(kind.name()),
                "{kind:?} is joined to the underworld corpus as {genus:?}, which does \
                 not extend its own name {:?} — the mapping is TRANSPOSED, and every \
                 column of this formation would be scored against another \
                 formation's communities while both sibling guards stayed green",
                kind.name()
            );
            assert!(
                seen.insert(genus),
                "{kind:?} is joined to {genus:?}, which another cave kind already \
                 claims — two formations sharing one genus would collapse them"
            );
        }
    }

    /// The genus branch must actually **discriminate**: two formations whose
    /// fit tables are identical everywhere are evidence that neither is being
    /// filtered. A companion to the test above, because a genus string can
    /// match rows and still not be the one being used.
    #[test]
    fn two_formations_do_not_score_identically() {
        let niche = drow();
        let differs = [
            DelveRung::Undercroft,
            DelveRung::Shallows,
            DelveRung::Deeps,
            DelveRung::Underdeep,
            DelveRung::Sunless,
        ]
        .into_iter()
        .any(|rung| {
            chamber_fit(&niche, CaveKind::Karst, rung)
                != chamber_fit(&niche, CaveKind::Fracture, rung)
        });
        assert!(
            differs,
            "karst and fracture score identically at every depth class, which \
             means the genus filter is matching neither and both are reading the \
             genus-blind fallback"
        );
    }

    /// `Surface` is not a depth class any underworld community occupies, so it
    /// has no fit — the total-ness that stops the overworld being scored
    /// against a cave.
    #[test]
    fn the_surface_rung_has_no_chamber_fit() {
        assert_eq!(
            chamber_fit(&drow(), CaveKind::Karst, DelveRung::Surface),
            None
        );
    }

    /// A niche that states no preference at all scores `0.0` everywhere, so
    /// the multiplier is a real term rather than a constant: the fit reaches
    /// the seat.
    #[test]
    fn an_indifferent_niche_scores_zero() {
        let blank = EnvironmentNiche::new(&[]).expect("the empty niche is legal");
        assert_eq!(
            chamber_fit(&blank, CaveKind::Karst, DelveRung::Deeps),
            Some(0.0)
        );
    }
}
