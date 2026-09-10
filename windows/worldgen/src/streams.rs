//! Seed-derivation labels owned by `windows/worldgen` itself (the
//! chorus/schema-selection and religion-naming streams — composition-
//! root concerns, not any one domain's). Save-format contracts; a
//! rename silently corrupts every world.

hornvale_kernel::stream_labels! {
    /// Synthetic cohort-to-person realization. This is opt-in probe state;
    /// no default world derives this leg.
    SOCIAL_PROJECTION = "social/projection/v1" => "synthetic cohort-to-person realization";
    /// The folk causal-schema-selection sub-leg, under a culture's own
    /// `hornvale_language::streams::ROOT` derivation.
    SCHEMA = "schema" => "the folk causal-schema-selection sub-leg";
    /// The sky-domain fact-shape sub-leg, under `SCHEMA`.
    SKY = "sky" => "the sky-domain fact-shape sub-leg";
    /// The lexicalization sub-leg for a chosen schema's rendered sentence.
    LEXEME = "lexeme" => "the lexicalization sub-leg for a chosen schema";
    /// The doctrine (institutional) causal-schema-selection sub-leg — the
    /// doctrine-voice twin of `SCHEMA`.
    DOCTRINE_SCHEMA = "doctrine-schema" => "the doctrine-voice twin of the folk schema-selection leg";
    /// The doctrine-voice twin of `LEXEME`.
    DOCTRINE_LEXEME = "doctrine-lexeme" => "the doctrine-voice twin of the lexeme leg";
    /// The deity-naming stream, epoch v2 (a full flat path, not a composed
    /// leg chain — matches `domains/climate`'s own `WEATHER_PHASE` pattern).
    RELIGION_DEITY_V2 = "religion/deity/v2" => "the deity-naming stream, epoch v2";
    /// The per-settlement disposition draw (The Tolerance). A flat path, like
    /// `RELIGION_DEITY_V2`: a composition-root concern, because the draw needs
    /// `hornvale_species::Dispersion` AND the occupation's own site, and no
    /// domain crate may depend on a sibling.
    ///
    /// **Keyed on the occupation record's `(site, founded-year)` pair** — see
    /// [`crate::disposition`] for why that key, and not the settlement's
    /// `EntityId`, its `BakeId`, or its bare current `cell-id`.
    SETTLEMENT_DISPOSITION = "settlement/disposition/v1" => "the per-settlement disposition draw, keyed on the occupation's (site, founded-year)";
    /// The per-resident draw (The Roll): name salt, age and the three mind
    /// dials, keyed on the settlement's site vertex and the resident's
    /// ordinal — never its `EntityId` — for the reason
    /// [`SETTLEMENT_DISPOSITION`] gives. Flat, like it, because the draw
    /// needs `hornvale_species::Dispersion` and the settlement's own site.
    SETTLEMENT_RESIDENT = "settlement/resident/v1" => "the per-resident draw, keyed on (site, ordinal)";
    /// Whether one expansion out of [`crate::history_bake::Bake::grow`] is a
    /// **working** rather than a farm (The Winze, spec §B.3). A flat path,
    /// like [`SETTLEMENT_DISPOSITION`] beside it and for the same reason: the
    /// draw needs `hornvale_terrain`'s prospectivity field AND the expanding
    /// occupation's own place, and a domain crate may not depend on a sibling,
    /// so no domain can host it.
    ///
    /// **Keyed on the PARENT's place and the year it throws** — vertex, band,
    /// and the epoch year — which is a place in the fixed lattice plus a place
    /// in time, never a generation ordinal (decision 0102, and the same shape
    /// [`HAZARD_EVENT`] uses). The key is unique by construction: at most one
    /// live community occupies a `(vertex, band)`, and `grow` runs at most once
    /// per community per epoch, so a `(vertex, band, year)` names exactly one
    /// throw. The year goes through
    /// [`crate::disposition::occupation_draw_key`] so the two composition-root
    /// keys that spell a year spell it the same way.
    ///
    /// **A SEPARATE LEG RATHER THAN A DRAW ON THE HISTORICAL `history/bake/v3`, AND THAT WAS
    /// MEASURED, NOT ASSUMED.** The first cut of this campaign took the draw
    /// sequentially off the bake's own epoch-dynamics stream, one line below
    /// the `DAUGHTER_PROB` draw it is nested inside. That is the neighbouring
    /// idiom and it is wrong here, because the bake stream carries exactly one
    /// other draw and inserting a second one conditionally re-orders every
    /// world's whole history. Measured over four seeds (42/7/1234/0), holding
    /// the working PLACEMENT out and consuming only the draw: occupation
    /// counts moved 1240→1402, 860→1240, 892→915, 440→284. The mines
    /// themselves number 0/1/2/3. So on the sequential stream ~all of the
    /// world change was the reshuffle and ~none of it was the mechanism, which
    /// would have left every later measurement in this campaign — the breach
    /// rate, the survivorship comparison — sitting on a world re-rolled for
    /// reasons unrelated to delving. Off its own leg, a world moves where a
    /// working is founded and nowhere else.
    SETTLEMENT_WORKING = "settlement/working/v1" => "whether one expansion is a working rather than a farm, keyed on the parent's (vertex, band, year)";
    /// Whether one epoch's advance of a **working** breaks through (The Winze,
    /// spec §4.3). A flat path, like [`SETTLEMENT_WORKING`] directly above and
    /// for the same reason: the draw belongs to a composition-root mechanism
    /// that reads a domain type (`Function::Mine`) against the bake's own live
    /// state, and no domain crate may host it.
    ///
    /// **A NEW LABEL, so nothing is an epoch.** Nothing derived from
    /// `settlement/breach/*` before this campaign, so no world that exists
    /// today consumed a draw under it (`domains/CLAUDE.md`: a new label is
    /// safe, a changed or reused one is an epoch).
    ///
    /// **Keyed on the WORKING's own place and the year it digs** — vertex,
    /// band, and the epoch year, spelled through
    /// [`crate::disposition::occupation_draw_key`] like the two composition-
    /// root keys beside it. Unique by construction for the same reason
    /// [`SETTLEMENT_WORKING`] is: at most one live community occupies a
    /// `(vertex, band)`, and a working is deepened at most once per epoch, so
    /// a `(vertex, band, year)` names exactly one increment of digging. It is
    /// a place in the fixed lattice plus a place in time, never a generation
    /// ordinal (decision 0102).
    ///
    /// **A SEPARATE LEG RATHER THAN A DRAW ON THE HISTORICAL `history/bake/v3`, on the
    /// measurement [`SETTLEMENT_WORKING`] already paid for.** That label's own
    /// doc records what happens when a conditional draw is inserted into the
    /// bake's sequential epoch-dynamics stream: occupation counts across four
    /// seeds moved 1240→1402, 860→1240, 892→915, 440→284, which is the
    /// reshuffle and not the mechanism. This draw fires far more often than
    /// that one — once per living working per epoch rather than once per
    /// daughter throw — so on the sequential stream it would be strictly
    /// worse, and spec §5.2's survivorship comparison would be taken on a
    /// world re-rolled for reasons unrelated to delving. Off its own leg, a
    /// world moves where a working breaches and nowhere else.
    SETTLEMENT_BREACH = "settlement/breach/v1" => "whether one epoch's advance of a working breaks through, keyed on the working's (vertex, band, year)";
    /// The underworld chamber derivation (The Deep Realm). Spelled by
    /// `crate::chamber`'s private `chamber_key`, but **`chamber_key` is a
    /// DISPLAY FORMATTER now, not a derivation key** (The Drift, spec
    /// amendment A.6, correcting an earlier A.4 that got the mechanism
    /// wrong): Task 1 deleted the chamber existence draw, the only
    /// production reader of a stream composed under this label, so nothing
    /// in a shipped world derives from `CHAMBER` any more. What actually
    /// carries the underworld's seed-derivation key is [`RUN_FLOORS`] and
    /// the three per-branch labels in `crate::character`
    /// (`BRANCH_COUNT`/`BRANCH_CHARACTER`/`BRANCH_BARRIER`).
    ///
    /// **Stays at v3 through The Drift, deliberately** (amendment A.6): The
    /// Drift dropped `entrance` from `ChamberAddr` (amendment A.3), which
    /// does change `chamber_key`'s spelling — but since nothing derives from
    /// it, bumping this label would record a discontinuity that never
    /// happened through it, an **empty epoch**. `chamber_key`'s sole
    /// production caller is `underworld_readout.rs`'s witness, which prints
    /// it as a `key` column for a human to read.
    ///
    /// **Epoch v3 (The Stope, spec §3.1 and amendment B.3).** The ADDRESS
    /// changed shape, in three ways that each re-key every chamber and which
    /// therefore ride one epoch rather than three:
    ///
    /// 1. `ChamberAddr` gained a **`floor`** (since renamed `level`), the
    ///    rung the lattice was missing — a band used to be one
    ///    interior-less point per column. A key that did not spell it would
    ///    derive one stream for every floor of a run, which is to say the
    ///    floors would all be one chamber.
    /// 2. `slot` was renamed **`branch`** (spec §3.1: "slot reads as a
    ///    position and it is an identity"), and the key's field order changed
    ///    with it.
    /// 3. The deepest rung was renamed **`Sunless` -> `Nadir`** (amendment
    ///    B.3), and `chamber_key` spells the rung's NAME, so the rename alone
    ///    relocates every chamber that sits at rank 4.
    ///
    /// **Epoch v2 (The Underworld, spec §4.1)** was the previous one:
    /// `ChamberAddr.band` stopped indexing the stratigraphic ladder
    /// (`Horizon`/`Stratum`) and started indexing the **delve ladder**
    /// (`hornvale_terrain::DelveRung`), whose rungs are ΔT thresholds above
    /// the surface datum rather than rock units.
    ///
    /// `chamber/v1` and `chamber/v2` are retired and must never be reused.
    CHAMBER = "chamber/v3" => "a display-only address formatter; the underworld's real derivation key is RUN_FLOORS and the per-branch legs";
    /// How many levels one **run** (a branch's own place within one band)
    /// realizes (The Stope, Task 2; spec §3.1's per-band ranges). Keyed on a
    /// [`crate::chamber::RunAddr`]: vertex, branch and band, a place in the
    /// fixed lattice and never a generation ordinal (decision 0102).
    ///
    /// **Epoch v2 (The Drift, amendment A.3/A.6).** `entrance` dropped out of
    /// `RunAddr` — every entrance of a system now addresses INTO the same
    /// shared lattice rather than realizing its own private sublattice — and
    /// this label is a LIVE production leg (`levels_in_branch` is
    /// `chamber_exists`'s own floor gate), so the re-keying rides an epoch
    /// rather than landing silently under `/v1`. `chamber/run-floors/v1` is
    /// retired and must never be reused.
    ///
    /// **A SEPARATE ROOT LEG, AND SINCE TASK 5 THAT IS THE WHOLE OF THE
    /// COLLISION ARGUMENT RATHER THAN A BELT-AND-BRACES HALF OF IT.**
    ///
    /// This doc used to rest the argument on prefixes: a run key
    /// (`vertex/branch/band`) is a strict prefix of a chamber key
    /// (`vertex/branch/band/level`), so the two strings can never be equal, and
    /// the separate parent was the durable backstop for a later campaign that
    /// made `level` optional. **The prefix half is still true of [`CHAMBER`]
    /// and is no longer the interesting case.** After The Drift's Task 5
    /// re-keyed the per-branch draws on band, `chamber::run_key` and
    /// `character::band_branch_key` both format `vertex/branch/rung_name(band)`
    /// — **byte-identical strings, not a prefix relation.** They were
    /// different shapes before that task; nothing announced that they had
    /// converged.
    ///
    /// So `RUN_FLOORS`, [`BRANCH_CHARACTER`] and [`BRANCH_BARRIER`] are three
    /// legs deriving from the *same* dynamic key, and the only thing keeping
    /// their three draws apart is that they hang off **different parent
    /// seeds**. That is sound — and it means the parent separation is now
    /// load-bearing rather than defensive, so re-parenting any of these legs
    /// is a save-format change even if no key string moves.
    /// `the_run_leg_and_the_chamber_leg_cannot_collide` in `crate::chamber`
    /// asserts the parent property directly, by handing the same string to
    /// both legs; `the_run_draw_travels_the_run_floors_leg_and_not_the_chamber_leg`
    /// is what holds the shipped path to it.
    ///
    /// What else changes with this leg: before it every in-budget run
    /// admitted all [`crate::chamber::LEVELS_PER_BRANCH_CEILING`] levels, and
    /// now it admits the drawn count. That is a world change, carried by the
    /// gate rather than by the key.
    RUN_FLOORS = "chamber/run-floors/v2" => "how many levels one run realizes, keyed on (vertex, branch, band)";
    /// Which [`crate::character::Character`] one branch carries (The Stope,
    /// Task 3; spec B.4/B.5). Keyed on a **branch at a band** — vertex, branch
    /// and band, a place in the fixed lattice and never a generation ordinal
    /// (decision 0102). A SEPARATE root leg from [`CHAMBER`] for the same
    /// collision argument [`RUN_FLOORS`]'s doc states: additive, perturbs no
    /// existing draw.
    ///
    /// **Epoch v2 (The Drift, amendment A.3, Task 5).** `entrance` dropped
    /// out of the key and `band` moved in: before this change a branch's
    /// character was a per-SYSTEM fact (one answer for the whole depth),
    /// after it a per-`(system, band)` fact — a branch can carry a different
    /// character at each band it occupies. This is a LIVE production leg
    /// (`character_at` is `chamber_at`'s own character read), so the
    /// re-keying rides an epoch. `chamber/branch-character/v1` is retired
    /// and must never be reused.
    BRANCH_CHARACTER = "chamber/branch-character/v2" => "which character one branch carries, keyed on (vertex, branch, band)";
    /// How thin the barrier between the underworld and what lies beyond it
    /// is, on one branch (The Stope, Task 3; spec B.5). Same key shape as
    /// [`BRANCH_CHARACTER`] — character and barrier are ONE object per B.5:
    /// same owner, same lattice key — but its own parent leg, so the two
    /// draws cannot collide even by accident.
    ///
    /// **Epoch v2 (The Drift, amendment A.3, Task 5)**, same shape and same
    /// reason as [`BRANCH_CHARACTER`]'s own v2 note: `entrance` out, `band`
    /// in, re-keyed at the identical granularity so character and barrier
    /// stay one object (B.5) rather than splitting across two. `chamber/
    /// branch-barrier/v1` is retired and must never be reused.
    BRANCH_BARRIER = "chamber/branch-barrier/v2" => "the barrier thinness of one branch, keyed on (vertex, branch, band)";
    /// The Crosscut: the descent plan's spine — the entrance cell and each
    /// level's stair cell. Additive at v1; keyed further by vertex at the
    /// call, the way `BRANCH_CHARACTER` is.
    UNDERWORLD_PLAN_SPINE = "underworld/plan/v1/spine" => "entrance and per-level stair cells of a descent's spine";
    /// The Crosscut: which edge a cycle attaches to, how long its existing
    /// segment is, and whether it runs on this floor or the one below.
    UNDERWORLD_PLAN_CYCLE = "underworld/plan/v1/cycle" => "cycle attachment, segment length and same- or cross-floor choice";
    /// The Crosscut: which edge a series extension replaces.
    UNDERWORLD_PLAN_EXTEND = "underworld/plan/v1/extend" => "which passage a series extension lengthens";
    /// The Crosscut: which shared coordinate a stairway's two ends land on.
    UNDERWORLD_PLAN_STAIR = "underworld/plan/v1/stair" => "the coordinate a stairway shares between two floors";
    /// The Brattice: which cycle pattern a realm draws from the frozen
    /// inventory. One draw per realm, made even when zero or one row is
    /// admissible, so the draw count is data-independent (spec §3.2 step 2).
    UNDERWORLD_GATE_PATTERN = "underworld/gate/v1/pattern" => "which cycle pattern a realm draws from the frozen inventory";
    /// How many of the lattice's four branch columns one cave system
    /// realizes (The Stope, Task 3; amendment C.1) — the drawn realization
    /// half of the lattice-ceiling/drawn-realization split, with
    /// `BRANCHES_PER_SYSTEM` as the ceiling. Keyed on the SYSTEM **at a
    /// band**: vertex and band, no branch.
    ///
    /// **Epoch v2 (The Drift, amendment A.3, Task 5).** `entrance` dropped
    /// out and `band` moved in: before this change a system had ONE branch
    /// width for its whole depth; after it, a system may realize a
    /// different width at each band — a system can be two branches wide in
    /// the Undercroft and one wide in the Shallows. `chamber_exists` reads
    /// this leg directly (`addr.branch >= branch_count_of(seed, addr.vertex,
    /// addr.band)`), so it is a LIVE production leg and the re-keying rides
    /// an epoch. `chamber/branch-count/v1` is retired and must never be
    /// reused.
    BRANCH_COUNT = "chamber/branch-count/v2" => "how many branches one cave system realizes, keyed on (vertex, band)";
    /// How many apertures one cave system opens to the surface (The Stope,
    /// Task 5; amendment C.3). Keyed on the SYSTEM's vertex alone — no
    /// entrance index, because the count is a fact about the system as a
    /// whole and an entrance index could not be defined before this draw
    /// answered. Terrain reports one cave per vertex with no aperture count,
    /// so the plural is derived here at the composition root rather than
    /// read off the cave.
    ///
    /// A separate root leg from [`CHAMBER`] for the same collision argument
    /// [`RUN_FLOORS`]'s doc states: additive, perturbs no existing draw,
    /// and the `/v1` epoch discipline applies to any later re-shaping of
    /// the key.
    ///
    /// **Epoch v2 (The Drift, Task 7b, spec amendment E.2/E.4).** The KEY is
    /// unchanged — still the system's vertex alone — and the epoch is real
    /// anyway, because what the leg ANSWERS changed. It used to be the
    /// system's aperture count outright; it is now the size of the FREE
    /// aperture set only, which `crate::chamber::entrance_count` then takes
    /// pointwise-maximum against the top band's drawn branch width so that
    /// every top-band branch is named by a door (E.2). A system with fewer
    /// apertures than top-band branches cannot satisfy that guarantee, so
    /// the count can no longer be independent of the width. One key, two
    /// different quantities across the boundary, is precisely the
    /// discontinuity an epoch records — and unlike `CHAMBER` (amendment A.6,
    /// which stays at v3 because nothing derives from it) this is a live
    /// production leg that every world reads. `chamber/entrance-count/v1` is
    /// retired and must never be reused.
    ///
    /// **Do not generalize this to "any downstream meaning change bumps the
    /// leg".** This was a close call made cheap by circumstance:
    /// `entrance-mouth` moved regardless, so every world's apertures were
    /// moving anyway and the bump cost no additional stability. Had
    /// `entrance-count` been the *only* leg in play, an epoch that reshuffles
    /// every world purely to record a meaning change happening one level
    /// above the label would have deserved a harder look.
    ENTRANCE_COUNT = "chamber/entrance-count/v2" => "how large one cave system's FREE aperture set is, keyed on vertex (the shipped count is this raised to the top band's branch width)";
    /// Which branch of the top habitation band one aperture opens on (The
    /// Stope, Task 5, amendment C.3; re-shaped by The Drift, Task 7b,
    /// amendment E.2). Keyed on the APERTURE's place — vertex and aperture
    /// index, a place in the fixed lattice and never a generation ordinal
    /// (decision 0102) — **plus the role that place is playing**. Same
    /// separate-root-leg discipline as [`ENTRANCE_COUNT`].
    ///
    /// **Epoch v2 (The Drift, Task 7b, spec amendment E.2/E.4)**, and here
    /// both halves moved. The key gained a role word (`share` / `free`, the
    /// discipline [`BAND_DESCENT`] already applies to a band transition),
    /// and the two roles range over different populations: a `share` draw
    /// indexes the branches not yet spoken for by a lower-indexed aperture —
    /// which is what makes apertures `0..width` a bijection onto branches
    /// `0..width`, and E.2 true by construction — while a `free` draw names
    /// any side branch, as every mouth draw did before this task. Whether a
    /// given index asks one question or the other depends on the top band's
    /// width, so ONE key would have served two questions at two widths; the
    /// role word is what keeps them apart. `chamber/entrance-mouth/v1` is
    /// retired and must never be reused.
    ENTRANCE_MOUTH = "chamber/entrance-mouth/v2" => "which top-band branch one aperture opens on, keyed on (vertex, aperture, role)";
    /// Which branches of an adjacent band a branch connects to (The Drift,
    /// Task 6; spec §4.5) — the edges descent actually travels, drawn so
    /// that "every branch above has a child" and "every branch below has a
    /// parent" hold **by construction** rather than by a repair pass.
    ///
    /// Keyed on the branch's own place **plus which of the two questions is
    /// being asked**: vertex, branch, band, role — where role is `child`
    /// (which branch of the band BELOW this one does this branch descend
    /// into?) or `parent` (which branch of the band ABOVE does this one hang
    /// from?). Vertex/branch/band is [`RUN_FLOORS`]'s own field order and
    /// spelling, so every re-keyed leg in this crate agrees; the band is
    /// spelled by its `Band` NAME for the same reason the others are.
    ///
    /// **The role is a question, not an ordinal** (decision 0102). One place
    /// in the lattice answers two independent questions, and giving each its
    /// own key is what keeps them from sharing a stream — the same shape
    /// [`HAZARD_EVENT`] uses when it puts the *process* in the key beside the
    /// vertex and the time block. The alternative — one stream per place, two
    /// draws taken in a fixed order — would make the parent answer depend on
    /// whether a child draw was taken first, which is exactly the
    /// order-dependence spec §4.5 rejects a repair pass for.
    ///
    /// A separate root leg from [`CHAMBER`] for the collision argument
    /// [`RUN_FLOORS`]'s doc states, and additive: it perturbs no existing
    /// draw, so worlds move only through the new edges themselves.
    BAND_DESCENT = "chamber/band-descent/v1" => "which branches of an adjacent band one branch connects to, keyed on (vertex, branch, band, role)";
    /// The volcano-identity derivation (The Repose). Keyed on the edifice's
    /// **source contact vertex** — a place in the fixed geosphere, never a
    /// generation ordinal, and never the query vertex a caller happened to ask
    /// about (an edifice spans 1-2 vertices, so the query vertex would mint two
    /// mountains for one). `crate::volcano`'s private `volcano_key` is the one
    /// place the composed key is spelled. The third time this project has met
    /// the "generation order is never an identity" wall (decision 0102, The
    /// Salt, The Tolerance); nothing here carries an ordinal so that mistake
    /// cannot recur.
    VOLCANO = "volcano/v1" => "the volcano-identity derivation, keyed on the edifice's source contact vertex";
    /// The per-vertex hazard-event draw (The Repose). Keyed on a **place in
    /// space and a place in time** — the vertex, the process (seismic or
    /// eruption), and the index of a fixed 1,000-year block of world time —
    /// and **never on the window a caller asked about**. That is the whole
    /// design: the event sequence of a `(seed, vertex)` exists independently of
    /// who asks, so a narrower query filters the same sequence rather than
    /// drawing an unrelated one. A block index is a coordinate on a lattice
    /// that tiles the timeline before anything is generated into it, exactly
    /// as `ChamberAddr`'s `band`/`branch`/`floor` are — not a generation ordinal
    /// (decision 0102, The Salt, The Tolerance). `crate::hazard`'s private
    /// `event_key` is the one place the composed key is spelled.
    HAZARD_EVENT = "hazard/event/v1" => "the per-vertex hazard-event draw, keyed on (vertex, process, world-time block)";
    /// Where a placed site lands within its geosphere vertex's territory (The
    /// Prospect, Task 5) — the ADDRESS a site has, as against the vertex that
    /// warrants it. Spelled by `crate::placement`'s private `placement_key`.
    ///
    /// **Why a draw and not a derivation.** A site is warranted at a geosphere
    /// vertex, and level-6 vertices are 110-132 km apart; the facet a walker
    /// stands on at the walk band is 1.126 km across. So a predicate that asks
    /// "is a site here?" by thresholding the nearest vertex answers for the
    /// whole 110 km neighbourhood at once. That is not a hypothetical: it is
    /// `CLIM-water-label-resolution-vs-walk-band`, where all 81 facets of seed
    /// 42's 6.4905 km flagship band draw the river glyph because `WaterKind` is
    /// a per-vertex label read nearest-vertex rather than interpolated. The
    /// project has already measured that failure once, so reproducing it for
    /// sites would be adopting it knowingly. A site therefore gets a real
    /// address — and an address the coarse fields cannot derive has to be
    /// drawn. Decision 0667.
    ///
    /// **It serves caves as well as exotic sites, and that is why the label is
    /// `site/` and not `exotic/`.** The campaign's spec claimed a cave needed
    /// no draw because `hornvale_terrain::cave_proneness` is a pure function.
    /// That is true of the function and false of its data: `material_at` and
    /// `cave_proneness_at` are both `Vertex`-bound, so proneness exists only at
    /// the same 110-132 km spacing everything else here does. One placement
    /// mechanism serves both kinds rather than two mechanisms that would have
    /// to agree.
    ///
    /// **Keyed on the VERTEX INDEX and the REASON.** The vertex index is a
    /// fixed position in the geosphere lattice, never the order sites happen to
    /// be enumerated in (decision 0102, and the fourth time this project has
    /// met that wall). The reason — cave or exotic — is a role word, the same
    /// discipline [`ENTRANCE_MOUTH`] and [`BAND_DESCENT`] apply when one place
    /// answers two independent questions: without it a vertex that warrants
    /// both a cave and an exotic site would place both at the identical facet,
    /// every time, which is a regularity of exactly the kind the draw exists to
    /// destroy.
    ///
    /// **A NEW LABEL, so nothing is an epoch under it.** Nothing derived from
    /// `site/placement/*` before this campaign, so no world that exists today
    /// consumed a draw under it, and no existing stream's consumption order
    /// moves (`domains/CLAUDE.md`: a new label is safe, a changed or reused one
    /// is an epoch). What DOES change is downstream of the label, not through
    /// it: a `Brief` now reports an exotic site at one facet per placed vertex
    /// instead of at none.
    SITE_PLACEMENT = "site/placement/v1" => "where a placed site lands within its vertex's territory, keyed on (vertex, reason)";
    /// The derived spring/seep surface's noise root (The Weft, Task 5; spec
    /// §5.1/§5.6). A `WeftKind`'s own prevalence-modulation and occurrence
    /// draws are decorrelated sub-legs derived dynamically under this label
    /// at the call site (`crate::weft`), the same discipline
    /// [`ENTRANCE_MOUTH`]'s `role` word documents — never a second entry
    /// here, since a dynamic leg names no new save-format contract by itself.
    ///
    /// **Position-keyed, not vertex-keyed.** Every other flat label in this
    /// file keys a draw on a discrete lattice coordinate (a vertex, a facet
    /// address); this one keys a *noise field* sampled at a facet's
    /// continuous centre position ([`hornvale_kernel::Facet::centroid`]),
    /// deliberately never at [`hornvale_kernel::Facet::seed`] — an
    /// address-hashed draw would decorrelate geometrically adjacent facets,
    /// which is the exact failure `channel-band-monotonicity` names
    /// ("address-hashed noise leaked into a band edge").
    ///
    /// **A NEW LABEL, so nothing is an epoch under it** — the same posture
    /// [`SITE_PLACEMENT`] states for the same reason: nothing derived from
    /// `derived/spring/*` before this campaign.
    WEFT_SPRING = "derived/spring/v1" => "occurrence of a derived spring at a walk facet, keyed on position";
    /// The derived overhang/hollow surface's noise root (Task 7; spec
    /// §5.6): medium contextuality (induration × slope), short–medium
    /// correlation length. Not enterable, but affords shelter and fire — see
    /// `crate::weft::kinds::WeftKind::Overhang`'s own doc for the affordance
    /// wiring this label's kind still needs (a later task's, per the
    /// layering `windows/vessel` sits on the far side of).
    ///
    /// Position-keyed, additive, a NEW label — the same three properties
    /// [`WEFT_SPRING`] states for itself, for the same reason: a `WeftKind`'s
    /// own prevalence-modulation and occurrence draws are decorrelated
    /// sub-legs derived dynamically under this label at the call site
    /// (`crate::weft`), never a second entry here.
    WEFT_OVERHANG = "derived/overhang/v1" => "occurrence of a derived overhang at a walk facet, keyed on position";
    /// The derived thicket/brake surface's noise root (Task 7; spec §5.6):
    /// high contextuality (productivity — temperature × moisture, Liebig-
    /// combined AFTER each is blended, never a materialized `productivity`
    /// field; see `crate::fieldpack`'s own module doc for why), long
    /// correlation length — texture, aimed directly at the biome-monotony
    /// defect this campaign addresses.
    ///
    /// Position-keyed, additive, a NEW label — see [`WEFT_SPRING`]'s own
    /// doc for the three properties this restates.
    WEFT_THICKET = "derived/thicket/v1" => "occurrence of a derived thicket at a walk facet, keyed on position";
    /// The derived erratic/scatter surface's noise root (Task 7; spec
    /// §5.6): the **negative control** — deliberately LOW contextuality
    /// (near-zero, mostly free noise, uncorrelated with any macro cause),
    /// short correlation length. Exists so Task 9's legibility metric can be
    /// shown to *discriminate*: if springs and erratics score alike, the
    /// metric measures nothing. See
    /// `crate::weft::kinds::WeftKind::contextuality`'s own doc for why this
    /// kind's contextuality must never be raised to tie it to macro state.
    ///
    /// Position-keyed, additive, a NEW label — see [`WEFT_SPRING`]'s own
    /// doc for the three properties this restates.
    WEFT_ERRATIC = "derived/erratic/v1" => "occurrence of a derived erratic at a walk facet, keyed on position";
    /// Skyworld projected-coverage draw, conditioned by the deterministic
    /// environment score and independent from territory properties.
    SKYWORLD_COVERAGE = "skyworld/coverage/v1" => "seeded sky-habitat coverage draw conditioned by the deterministic environment score";
    /// Skyworld's spatial distribution, clustering, and origin-altitude draws,
    /// keyed on the stable surface vertex as `vertex/{id}`.
    SKYWORLD_DISTRIBUTION = "skyworld/distribution/v1" => "sky-habitat distribution and origin altitude, keyed on the stable surface vertex as vertex/{id}";
    /// Skyworld's stable atmospheric baseline and field profile.
    SKYWORLD_ATMOSPHERE = "skyworld/atmosphere/v1" => "stable world-level sky atmospheric fields";
    /// Skyworld territory phenotype and stability attributes.
    SKYWORLD_PHENOTYPE = "skyworld/phenotype/v1" => "sky-territory phenotype and stability traits";
    /// Skyworld territory lineage and lifecycle metadata.
    SKYWORLD_LINEAGE = "skyworld/lineage/v1" => "sky-territory lineage and lifecycle metadata";
    /// Skyworld movement-regime variation. Stage 1 stores this property but
    /// does not derive trajectories from it.
    SKYWORLD_MOVEMENT = "skyworld/movement/v1" => "sky-territory movement regime variation";
    // Waterworld Stage 1 is a pure terrain/climate projection and deliberately
    // earns no stream label. The first stochastic source is Stage 2's vent
    // admission; its label belongs here only after its behavior is tested.
    /// Waterworld vent admission and per-vent source strengths, keyed by seabed vertex.
    WATERWORLD_VENT = "waterworld/vent/v1" => "seeded sparse hydrothermal-vent admission and source strength";
}
