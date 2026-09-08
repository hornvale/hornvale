//! Where in the delve ladder a people lives, and what a chamber there is worth
//! to it (The Underworld, Task 8; spec §4.6 and §4.2.1 clause 2).
//!
//! Three things live here, and they are one derivation read at three grains:
//!
//! 1. [`chamber_fit`] — how well one kind's [`EnvironmentNiche`] suits the
//!    underworld communities that occur in a given cave formation at a given
//!    depth class. Task 7 shipped `environment_fit` with no production
//!    consumer; this is that consumer.
//! 2. [`seat_at`] — which rung of one vertex's column a people would settle, and
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
//! settler is the one who will make it. **This is that function's first
//! production caller**, and its own doc now records this call site rather than
//! the absence of one.

use hornvale_climate::underworld::underworld_assignment;
use hornvale_kernel::{Band, Geosphere, Vertex, VertexMap, World};
use hornvale_species::{EnvironmentNiche, environment_fit};
use hornvale_terrain::{
    Cave, CaveKind, GeneratedTerrain, GeothermalGradient, delta_t_range_of, rungs,
    water_table_depth_m,
};

use crate::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, ChamberOrigin, ChamberOverrides, chamber_exists, is_sump,
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
/// plumb: pending(wave-1)
pub const UNDERWORLD_WORKS_COST: f64 = 0.5;

/// The depth class one habitation band names, in the vocabulary the underworld
/// corpus states its communities in.
///
/// The delve ladder and the corpus's own `UnderworldName::zone` field are
/// both `hornvale_kernel::Band` (decision 0044 clause (a) put the shared
/// roster in the kernel; the two used to be a **mirrored pair** under
/// decision 0094, guarded by the now-retired `cli/tests/suite/
/// delve_roster_mirror.rs`, and `hornvale_terrain::delve` re-exported the
/// kernel type as `DelveRung` until The Drift deleted that alias). This
/// function translates a `Band` into itself, minus the one band no
/// underworld community occupies — kept as an exhaustive match rather than
/// collapsed to an identity, so a future `Band` variant fails to compile
/// here rather than silently scoring against the wrong depth class.
///
/// `Surface` is `None` for the reason `domains/climate::underworld` states on
/// `UnderworldName::zone`: no underworld community is at the surface, so
/// there is nothing for a surface band to be scored against.
fn zone_of(rung: Band) -> Option<Band> {
    match rung {
        Band::Surface => None,
        Band::Undercroft => Some(Band::Undercroft),
        Band::Shallows => Some(Band::Shallows),
        Band::Deeps => Some(Band::Deeps),
        Band::Underdeep => Some(Band::Underdeep),
        Band::Nadir => Some(Band::Nadir),
    }
}

/// The corpus's spelling of one cave formation. Until The Hallmark
/// (2026-09-02) this was the third leg of decision 0094's duplicate roster —
/// the one nobody had joined, alongside `hornvale_terrain::CaveKind` and a
/// second, hand-written `CaveKind` → `Formation` variant map. Task 14 made
/// that second map unnecessary by making the join STRUCTURAL
/// (`Formation::Cave(CaveKind)`, ledger #14/#16), which deleted decision
/// 0094's canonical map at `cli/tests/suite/cave_kind_correspondence.rs`
/// outright — two different `CaveKind`s wrapped in `Formation::Cave` are
/// unequal by construction, so there is no longer a second hand-written
/// variant map to duplicate. **The roster now has two legs, not three**:
/// the kernel's `CaveKind`, embedded directly in `Formation::Cave`, and the
/// corpus's spellings, joined by this function. What this function still
/// guards is that second leg — the spelling join, which stays hand-written
/// because nothing structural could carry it (see below).
///
/// **This exists because the two spellings are genuinely different words.**
/// `hornvale_terrain::CaveKind::name` answers `"karst"` / `"lava-tube"` /
/// `"fracture"` (its scene-emission legend), while
/// `hornvale_climate::underworld`'s `genera` carry the *surface corpus's*
/// formation names from `climate::axes` — `"karst-cave"` / `"lava-tube"` /
/// `"fracture-cave"` — because an underworld community is a community *of* one
/// of the formations The Axes already named. One of the three agrees by
/// coincidence, which is exactly why the mismatch survived: `lava-tube` matched
/// and looked like the rule working. Embedding `CaveKind` inside `Formation`
/// changed nothing about this: `Formation` carries no name of its own (the
/// corpus's `genera` are raw `&'static str` literals in `climate::axes`, not a
/// derived reading of the enum), so the spelling correspondence is still a
/// fact about the *strings*, joinable only by a function like this one.
///
/// Exhaustive over `CaveKind` with no wildcard, following this module's own
/// enforcement pattern: a fourth formation fails to compile here rather than
/// silently falling through [`chamber_fit`]'s genus-blind branch, and
/// `every_cave_kind_matches_a_corpus_genus` asserts every emitted genus
/// actually occurs in the corpus.
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
/// than once per vertex.
/// type-audit: bare-ok(ratio: return)
pub fn chamber_fit(niche: &EnvironmentNiche, cave: CaveKind, rung: Band) -> Option<f64> {
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

/// One people's seat in one vertex's column: which rung it would settle, and the
/// factor its capacity there is scaled by.
/// type-audit: bare-ok(ratio: multiplier), bare-ok(flag: works), bare-ok(ratio: fit)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RungSeat {
    /// The rung settled.
    pub rung: Band,
    /// The factor capacity at this vertex is multiplied by — [`chamber_fit`],
    /// times [`UNDERWORLD_WORKS_COST`] where the chamber needs dewatering.
    pub multiplier: f64,
    /// Whether the chamber at that rung is phreatic **as found**, and so
    /// costs its settlers the works that keep it dry.
    ///
    /// **Structurally `false` whenever `rung` is `Undercroft`, and that is an
    /// identity rather than a measurement** — see [`seat_at`]'s own docs for
    /// the derivation. A rung is judged at its top, `Undercroft`'s top is
    /// `0.0` m in every column, the water table is floored at zero, and
    /// [`hornvale_terrain::is_phreatic`] is strict; so no world can produce a
    /// seat with `rung: Undercroft, works: true`. A reader counting works over
    /// a seating is counting over ranks 1–4 only, and a per-rung share for
    /// rank 0 says nothing. This mirrors the rank-0 degeneracy
    /// [`crate::chamber`]'s `stratum_at` discloses for `Chamber::stratum`; it
    /// is disclosed here for the same reason and was not, for one campaign.
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
/// against the price of draining it.
///
/// ## Half of that argument is an IDENTITY, disclosed here as a measurement
///
/// **`works` is `false` at `Undercroft` in every possible world**, and no
/// probe can report otherwise. A rung is judged at its own **top**; the ΔT
/// range of `Undercroft` begins at 0 K, so its top is `0.0` m in every column
/// under every gradient; [`hornvale_terrain::water_table_depth_m`] is
/// `.max(0.0)`-floored and so is never negative; and
/// [`hornvale_terrain::is_phreatic`] is a **strict** `depth > table`. So
/// `is_phreatic(0.0, table)` is `false` for every table this crate can
/// produce — including the `0.0` of a drowned column, which the strictness is
/// there to make vadose. [`RungSeat::works`] is therefore structurally
/// unreachable at rank 0.
///
/// This paragraph previously cited *"`Undercroft` is dry in 100% of
/// cave-bearing columns (`underworld_water_table_probe.rs`)"* as **evidence**.
/// The probe does print that row, and the row is correct; but it is reporting
/// an identity rather than a contingent fact, and a reader entitled to think
/// it might have come back 99% was being misled about what had been tested. It
/// is the same rank-0 degeneracy [`crate::chamber`]'s `stratum_at` discloses
/// for `stratum`, in a second quantity — and nothing disclosed it here.
///
/// **The ranking rule's justification survives being restated as an identity,
/// and is stronger for it.** What the identity buys is that the alternative is
/// bad *structurally* rather than bad on three seeds: a dry seat exists in
/// every cave-bearing column of every world by construction, so under
/// multiplier ranking a **phreatic** rung — the only kind the drainage rule
/// exists for — can win only by beating that dry seat on fit by a factor of
/// `1 / UNDERWORLD_WORKS_COST`. (A deeper rung that is itself dry competes at
/// full fit and is unaffected, which is why the v1 arm below still seats at
/// `Shallows`: 41.8 / 24.9 / 26.1% of reached `Shallows` columns are vadose.)
/// So the condition that decides whether the rule is ever reachable is a
/// condition on the **authored fit table**, not on the hydrology — the
/// alternative makes the rule's reachability a property of a constant nobody
/// calibrated, in the one place the campaign wanted hydrology to speak.
///
/// ## The counterfactual, RE-MEASURED 2026-08-18 against the repaired join
///
/// Both arms were first measured before Task 9 repaired [`chamber_fit`]'s
/// genus join (`"karst"` against `"karst-cave"`), which moved every seating in
/// every world; the pre-repair figures are not evidence about this tree. Re-run
/// by the same method —
/// `underworld_capacity_probe::where_underworld_communities_found_and_what_they_cut`,
/// with the fold below temporarily ranking on `candidate.multiplier` for the v1
/// arm and restored afterwards:
///
/// ```text
///                                    seed 42   seed 7   seed 1234   total
///   v1  rank on the multiplier
///     underworld occupations               7       21          53      81
///     founded below the table            0/7     0/21        0/53    0/81
///     `Made` chambers below the table      0        0           0       0
///   v2  rank on fit (SHIPPED)
///     underworld occupations               7       22          64      93
///     founded below the table            0/7     1/22        2/64    3/93
///     `Made` chambers below the table      0        3           4       7
/// ```
///
/// **Every number moved; the conclusion did not.** The figures this doc used
/// to carry were **0 of 23** (v1) and **2 of 28 on seed 42** (v2). Post-repair
/// the counterfactual is 0 of **81** — a larger denominator and the same zero —
/// while the shipped rule fires on 3 of **93**, having moved off seed 42
/// entirely and onto seeds 7 and 1234. So the design claim is unchanged in kind
/// and better evidenced: under multiplier ranking the drainage rule would have
/// a producer that never once produces the case it exists for — not one
/// community founded under water, and not one `Made` chamber below the table,
/// on any seed — the fourth dangling seam this campaign is trying not to
/// create, arrived at by a route that looked like careful economics.
///
/// **What it costs to say honestly: the rule now fires on 3 of 93, on two
/// seeds of three.** That is thinner than "2 of 28 on seed 42" sounded and it
/// is the real margin; it is reachable rather than unreachable by
/// construction, which is the whole of the claim, and nothing here should be
/// read as calibration.
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
    for &rung in rungs().iter().filter(|r| **r != Band::Surface) {
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

/// One people's seating over the whole globe: its rung at every vertex, and the
/// factor its capacity there is scaled by.
///
/// Two `VertexMap`s rather than one of pairs because the bake reads them at
/// different moments — the rung on every index lookup, the multiplier once,
/// when the capacity fields are built.
/// type-audit: bare-ok(ratio: multiplier)
pub struct Seating {
    /// The rung this people occupies at each vertex. `Surface` everywhere for a
    /// surface people.
    pub rung: VertexMap<Band>,
    /// The factor this people's capacity at each vertex is scaled by. `1.0`
    /// everywhere for a surface people, which is an IEEE-754 no-op.
    pub multiplier: VertexMap<f64>,
}

impl Seating {
    /// The seating of a people that lives overhead: the `Surface` rung
    /// everywhere, at an untouched capacity.
    ///
    /// This is what makes the re-key inert for a surface people — one rung,
    /// one community per vertex, and a multiplier that is exactly `1.0` rather
    /// than approximately so.
    pub fn all_surface(geo: &Geosphere) -> Seating {
        Seating {
            rung: VertexMap::from_fn(geo, |_| Band::Surface),
            multiplier: VertexMap::from_fn(geo, |_| 1.0),
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
/// A vertex with no cave gets `Undercroft` at multiplier `0.0`. The rung there
/// is unobservable — the realm gate already zeroes a subterranean kind's
/// capacity on a caveless vertex, so no community is ever opened at one — and
/// naming the shallowest habitation rung beats naming `Surface`, which would
/// put a subterranean people into the overworld's index at a vertex it cannot
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
        .vertices()
        .map(|vertex| {
            let cave = terrain.cave_at(vertex)?;
            let table = water_table_depth_m(
                terrain.drainage_at(vertex),
                terrain.material_at(vertex).porosity,
                terrain.elevation_at(vertex).get() - sea,
            );
            seat_at(niche, &cave, terrain.geothermal_gradient_at(vertex), table)
        })
        .collect();
    let at = |vertex: Vertex| seats[vertex.0 as usize];
    Seating {
        rung: VertexMap::from_fn(geo, |c| at(c).map_or(Band::Undercroft, |seat| seat.rung)),
        multiplier: VertexMap::from_fn(geo, |c| at(c).map_or(0.0, |seat| seat.multiplier)),
    }
}

/// The chambers a settled subterranean community makes — spec §4.2.1 clause
/// 2's **producer**, which the campaign bound to this task as an acceptance
/// criterion rather than deferring a fourth time.
///
/// Every address in the lattice at a community's own `(vertex, rung)` that
/// exists at all resolves to [`ChamberOrigin::Made`]: keeping a working depth
/// dry is what settling underground *is*, so a people's own halls are cut,
/// not found. Consequently [`is_sump`] answers `false` for every one of them,
/// whatever the water table does — which is the whole point of the rule.
///
/// **The rung is re-derived rather than carried.** An [`Occupation`] records
/// its site and its people and nothing about depth, and adding a rung to it
/// would put a new field on a serialized type for information that is already
/// a pure function of `(niche, terrain, vertex)`. Re-deriving it here through
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
/// `band: 0` (`Undercroft`) and [`crate::chamber::passages_from`] still has
/// no production caller — those are unchanged. **Two of this paragraph's own
/// clauses are not**: The Gallery gave `describe_underground_here` a real
/// ways-on report over the level's own neighbours (it no longer renders a
/// fixed `"Ways on: out."`) and added `down`/`up` beside `climb`, so a player
/// now walks and climbs through a whole generated descent. Neither change
/// reaches this disclosure, because the two systems never touch: The
/// Gallery's movement is over `underworld_level::Level`'s cells, never over
/// [`Chamber`]/[`ChamberOverrides`], and `delve_at` still calls
/// [`crate::chamber::chamber_at`] exactly once, at the entrance, with an
/// empty override map (`windows/vessel/src/session.rs`'s own Gallery-era
/// comment there: the chamber it resolves "only gates whether this cave
/// mouth leads anywhere at all… it is not itself where the possession
/// stands"). So the conclusion survives on its own terms: a player still
/// never resolves a `Made` chamber through this path, and it is still always
/// the shallowest rung *of this system* — the cave the player now walks
/// through has depth, but this settled-chamber machinery still is not
/// wired to it.
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
/// tracks an *address* rather than one `Chamber`; and prose that
/// distinguishes a cut hall from a found void. The fourth thing this
/// paragraph used to list as missing — a home for the overrides themselves,
/// derived per column off the committed ledger — now exists: [`column_origins`]
/// reads exactly that, per column, and [`ledger_overrides`] folds it into the
/// [`ChamberOverrides`] shape [`crate::chamber::chamber_at`] reads, pinned to
/// agree with this function (`underworld_capacity_probe`).
/// `windows/vessel`'s `Session::delve_at` has been the production caller of
/// [`column_origins`] since The Plat — but for a DIFFERENT purpose than this
/// paragraph priced: it derives the walked descent's own per-rung
/// [`ChamberOrigin`]/[`Tenancy`]
/// (`hornvale_worldgen::circuit::plan_descent_with_origins`), not this
/// module's `Chamber`/`ChamberOverrides` lattice — `delve_at`'s own
/// `chamber_at` call, the one this doc's disclosure is about, still receives
/// an empty override map. The remaining three needs above are still a
/// `windows/vessel` campaign, not a capacity task.
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
        let vertex = record.core.site;
        let band = *seating.rung.get(vertex);
        if band == Band::Surface {
            continue; // a surface community cuts no chamber
        }
        let Some(cave) = terrain.cave_at(vertex) else {
            continue;
        };
        let gradient = terrain.geothermal_gradient_at(vertex);
        // **`level: 0` is a deliberate narrowing, not the whole run** (The
        // Stope). A settled community occupies the levels its run realizes,
        // and how many those are is `chamber::levels_in_branch`, which landed
        // in Task 2 — so the reason for the narrowing has changed and the
        // narrowing has not. Widening it now would be *possible* (walk
        // `0..levels_in_branch(seed, addr.run())` instead of pinning zero) and
        // it would be a claim this campaign has not measured: that a
        // community fills every level of its run rather than some part of it.
        // This function still has no production call site (see its own doc),
        // so the narrowing costs nothing a player can reach, and the widening
        // belongs with whatever campaign decides how much of a run a people
        // occupies.
        for branch in 0..BRANCHES_PER_SYSTEM {
            let addr = ChamberAddr {
                vertex,
                branch,
                band,
                level: 0,
            };
            if chamber_exists(seed, &cave, gradient, addr) {
                overrides.insert(addr, ChamberOrigin::Made);
            }
        }
    }
    overrides
}

/// Whether the people who cut a chamber are still there (spec §3.2, §3.4).
/// `Made` is read from the ledger's occupation whether or not it is alive —
/// a cut hall outlives its people — and this decides only the tense.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tenancy {
    /// No people ever seated this rung: a found cave.
    Wild,
    /// A seated occupation on this rung is still alive.
    Inhabited,
    /// Every occupation that seated this rung has ended.
    Abandoned,
}

/// The chamber origins of ONE column, per rung, read off the committed ledger
/// at the walk (spec §3.2) — the per-column twin of [`made_chambers`], which
/// reads a baked `History`. For each occupation at `vertex` whose people
/// carries an environment niche, [`seat_at`] names the rung it settled, with
/// the same three inputs `made_chambers` and the capacity probe use; that
/// rung is `Made`, `Inhabited` if any seating occupation `is_alive()`, else
/// `Abandoned`. Every other rung, and every rung of a column with no cave or
/// no occupation, is `(Found, Wild)` — the answer every column gave before
/// The Plat.
///
/// Today exactly one people carries a niche (`drow`,
/// `environment_niche_registry()`), so it is the only people that can seat
/// underground; a people with no niche is a surface people
/// (`Seating::all_surface`) and cuts nothing.
pub fn column_origins(
    world: &World,
    terrain: &GeneratedTerrain,
    vertex: Vertex,
    rungs: &[Band],
) -> Vec<(ChamberOrigin, Tenancy)> {
    let mut out = vec![(ChamberOrigin::Found, Tenancy::Wild); rungs.len()];
    let Some(cave) = terrain.cave_at(vertex) else {
        return out;
    };
    let niches = hornvale_species::environment_niche_registry();
    let gradient = terrain.geothermal_gradient_at(vertex);
    let porosity = terrain.material_at(vertex).porosity;
    let height_asl_m = terrain
        .elevation_at(vertex)
        .above(terrain.sea_level())
        .get();
    let water_table_m = water_table_depth_m(terrain.drainage_at(vertex), porosity, height_asl_m);
    for occupation in crate::history_emit::occupations_at(world, vertex) {
        let Some(niche) = niches.get(&occupation.core.people) else {
            continue;
        };
        let Some(seat) = seat_at(niche, &cave, gradient, water_table_m) else {
            continue;
        };
        let Some(i) = rungs.iter().position(|r| *r == seat.rung) else {
            continue;
        };
        let tenancy = match (out[i].1, occupation.is_alive()) {
            (_, true) => Tenancy::Inhabited,
            (Tenancy::Inhabited, false) => Tenancy::Inhabited,
            (_, false) => Tenancy::Abandoned,
        };
        out[i] = (ChamberOrigin::Made, tenancy);
    }
    out
}

/// Every `Made` chamber the committed ledger implies, as the
/// [`ChamberOverrides`] map [`crate::chamber::chamber_at`] reads — the same
/// shape [`made_chambers`] writes (every branch of the seated rung, `level:
/// 0`), derived from the ledger instead of a baked `History`. The two are
/// pinned to agree (`underworld_capacity_probe`), which is what keeps this
/// pair one writer at two grains rather than two writers that can drift.
pub fn ledger_overrides(world: &World, terrain: &GeneratedTerrain) -> ChamberOverrides {
    let rungs: Vec<Band> = rungs()
        .iter()
        .copied()
        .filter(|r| *r != Band::Surface)
        .collect();
    let mut overrides = ChamberOverrides::new();
    for vertex in terrain.geosphere().vertices() {
        let Some(cave) = terrain.cave_at(vertex) else {
            continue;
        };
        let gradient = terrain.geothermal_gradient_at(vertex);
        for (i, (origin, _)) in column_origins(world, terrain, vertex, &rungs)
            .iter()
            .enumerate()
        {
            if *origin != ChamberOrigin::Made {
                continue;
            }
            for branch in 0..BRANCHES_PER_SYSTEM {
                let addr = ChamberAddr {
                    vertex,
                    branch,
                    band: rungs[i],
                    level: 0,
                };
                if chamber_exists(world.seed, &cave, gradient, addr) {
                    overrides.insert(addr, ChamberOrigin::Made);
                }
            }
        }
    }
    overrides
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_species::environment_niche_registry;
    use std::collections::{BTreeMap, BTreeSet};

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
            for &rung in rungs().iter().filter(|r| **r != Band::Surface) {
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
    /// `hornvale_climate::Formation`, and why that is still true after The
    /// Hallmark (2026-09-02) made the *variant* half of this join
    /// structural.** Task 14 folded the three-variant `KarstCave`/
    /// `LavaTube`/`FractureCave` roster into `Formation::Cave(CaveKind)`
    /// (ledger #14/#16), which deleted decision 0094's canonical map at
    /// `cli/tests/suite/cave_kind_correspondence.rs` — that file asserted
    /// only that `CaveKind`'s three values reach three distinct `Formation`
    /// values, a claim the embed now makes true by construction (two
    /// different `CaveKind`s wrapped in `Formation::Cave` are unequal by
    /// construction), so nothing there needed re-homing. **This test guards
    /// a different join that the embed left untouched: the *spelling*, not
    /// the *variant*.** `Formation` still carries no name — the corpus's
    /// `genera` are raw `&'static str` literals in `climate::axes`, sitting
    /// beside `Formation::Cave(CaveKind)` rather than derived from it — so
    /// there is no `const fn Formation::name` for a structural join to route
    /// through, embed or no embed. `genus_of` is still the one hand-written
    /// correspondence between a `CaveKind` and the corpus's string for it,
    /// and this test is still what would catch that correspondence being
    /// transposed.
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
            Band::Undercroft,
            Band::Shallows,
            Band::Deeps,
            Band::Underdeep,
            Band::Nadir,
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
        assert_eq!(chamber_fit(&drow(), CaveKind::Karst, Band::Surface), None);
    }

    /// A niche that states no preference at all scores `0.0` everywhere, so
    /// the multiplier is a real term rather than a constant: the fit reaches
    /// the seat.
    #[test]
    fn an_indifferent_niche_scores_zero() {
        let blank = EnvironmentNiche::new(&[]).expect("the empty niche is legal");
        assert_eq!(chamber_fit(&blank, CaveKind::Karst, Band::Deeps), Some(0.0));
    }

    /// The fixture world (decision 0607) plus a re-derived terrain: no build
    /// site, so no roster row (decision 0606). Every rung of a column with no
    /// cave, or no occupation, is `(Found, Wild)` — today's answer.
    // Named construction site (decision 0092): re-derives the fixture's
    // terrain rather than rebuilding the world.
    #[allow(clippy::disallowed_methods)]
    #[test]
    fn a_column_nobody_settled_is_found_and_wild_on_every_rung() {
        let world = crate::fixture::seed_42_world();
        let terrain = crate::terrain_of(&world).expect("seed 42 sculpts");
        let rungs: Vec<Band> = rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
            .collect();
        // The first cave-bearing vertex with no occupation at all.
        let vertex = terrain
            .geosphere()
            .vertices()
            .find(|&v| {
                terrain.cave_at(v).is_some()
                    && crate::history_emit::occupations_at(&world, v).is_empty()
            })
            .expect("seed 42 has an unsettled cave");
        let origins = column_origins(&world, &terrain, vertex, &rungs);
        assert_eq!(origins.len(), rungs.len());
        assert!(
            origins
                .iter()
                .all(|o| *o == (ChamberOrigin::Found, Tenancy::Wild))
        );
    }

    /// Seed 42's Murrain world holds 5 historical cave-bearing columns with an
    /// occupation record for an underworld people, including records whose
    /// occupations have ended. The pre-Murrain `underworld_capacity_probe`
    /// measured 26 on 2026-09-03; the difference is real history movement,
    /// not a reclassification between population layers. Every current column
    /// is seated at the top or second rung, so at least one column reads
    /// `Made` at rung 0 or 1 and exactly one rung per column is Made (a people
    /// has one seat). This historical column witness is distinct from the
    /// present living-occupation layer: only tenancy follows `is_alive()`.
    ///
    /// **Grouped by vertex once, rather than calling [`crate::history_emit::
    /// occupations_at`] per vertex of the globe.** That function's
    /// `occupation_records(world)` re-derives every occupation in the ledger
    /// from scratch on every call — a naive, un-indexed scan
    /// (`Ledger::ensure_index` only ever runs on a WRITE path, and this
    /// fixture is loaded read-only) — so calling it once per one of this
    /// terrain's 40,962 vertices was measured at 155.8 ms/call, ~1.8 h total.
    /// [`crate::history_emit::occupation_records`] once, grouped by
    /// `core.site` here, gives the identical per-vertex answer this test
    /// still gets (nothing about `occupations_at`'s own sort order matters
    /// to any assertion below) in well under a second. [`column_origins`],
    /// the function actually under test, is still called exactly as the
    /// production path calls it — once per settled, cave-bearing column.
    // Named construction site (decision 0092): re-derives the fixture's
    // terrain rather than rebuilding the world.
    #[allow(clippy::disallowed_methods)]
    #[test]
    fn a_historically_settled_column_is_made_at_its_seated_rung_and_nowhere_else() {
        let world = crate::fixture::seed_42_world();
        let terrain = crate::terrain_of(&world).expect("seed 42 sculpts");
        let rungs: Vec<Band> = rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
            .collect();
        let niches = hornvale_species::environment_niche_registry();
        let mut by_vertex: BTreeMap<Vertex, Vec<hornvale_history::record::OccupationRecord>> =
            BTreeMap::new();
        for o in crate::history_emit::occupation_records(&world) {
            by_vertex.entry(o.core.site).or_default().push(o);
        }
        let empty: Vec<hornvale_history::record::OccupationRecord> = Vec::new();
        let mut historical_made_columns = 0usize;
        for vertex in terrain.geosphere().vertices() {
            let occ = by_vertex.get(&vertex).unwrap_or(&empty);
            let historically_settled = occ.iter().any(|o| niches.get(&o.core.people).is_some());
            if terrain.cave_at(vertex).is_none() || !historically_settled {
                continue;
            }
            let origins = column_origins(&world, &terrain, vertex, &rungs);
            let made: Vec<usize> = origins
                .iter()
                .enumerate()
                .filter(|(_, o)| o.0 == ChamberOrigin::Made)
                .map(|(i, _)| i)
                .collect();
            assert_eq!(made.len(), 1, "vertex {vertex:?}: one seat per people");
            assert!(
                made[0] <= 1,
                "vertex {vertex:?}: seated at rung {}",
                made[0]
            );
            let presently_occupied = occ
                .iter()
                .any(|o| niches.get(&o.core.people).is_some() && o.is_alive());
            let expected = if presently_occupied {
                Tenancy::Inhabited
            } else {
                Tenancy::Abandoned
            };
            assert_eq!(origins[made[0]].1, expected, "vertex {vertex:?}");
            historical_made_columns += 1;
        }
        assert_eq!(
            historical_made_columns, 5,
            "seed 42 historical occupied-underworld-column witness"
        );
    }
}
