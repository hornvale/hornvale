# The Tableau — decision ledger

Autopilot engaged. G3 (spec review) and G6 (merge) are hard stops.

#0 [Q] — **The premise correction this campaign exists to fix.** ·
The Company reported that `the-orange` is blocked by
`SOC-one-creature-per-settlement`. It is not, or not primarily. The binding
constraint was THE COMPANY'S OWN SPEC section 6, "scenes are found, never
staged", which the controller wrote and recommended and Nathan approved on
that recommendation. The world constraint bites only because of that choice. ·
ROOT CAUSE: two products were forced into one instrument — a scene as a
CAPABILITY AUDIT (must be found; a staged pass proves nothing about the world)
and a scene as A DRAMA YOU WANT TO WATCH (must be stageable; the point is to
see it). One good argument was applied universally. ·
ALSO RECORDED, because it is systemic rather than a slip: the decision log is
append-only and supersede-never-edit, so a record written to be durable READS
as settled whether or not it was. Nothing in the format distinguishes a
ruling from a stopgap-pending-cost. `SOC-one-creature-per-settlement` is a
scale-management compromise and was read as a statement about what the world
may contain. ·
CAPTURE: spec §1, retro; a `PROC-` row for the provisional-decision gap.

#1 [G1] — Is a tableau a WORLD BUILT FROM SCRATCH or a DIFF against a
derived base? · DECISION: **a diff.** A tableau overrides named layers of an
otherwise-derived session, riding `Session::start_in` over a shared
`WorldContext`. ·
WHY: `start_in`'s own doc — "begin a possession from an already-derived
`WorldContext`, **paying none of its cost**" — already provides the sharing,
and the expensive layers (sculpted terrain, fitted climate, species roster,
demography fit) all live in that context. A from-scratch world would have to
synthesize `LocaleContext`, whose fields are private and bound to real
`GeneratedTerrain`/`GeneratedClimate`. Diff-not-scratch also makes PARTIAL
tableaux legal, which is what a variation family (same room, unlit) needs. ·
ALTERNATIVES DISCARDED: synthesize a `LocaleContext` from stipulated field
values (a second way to make a locale, two paths that can disagree — and
unnecessary, since the drama is a CHAMBER scene and chambers do not read it
the way the walk band does).
ideonomy passes: 3 / 1 OVERTURN (pass 1: dimension-identification + negation
over a `periodic-grid`, prompts polarity/scope/modularity — reframed the
object as a diff and OVERTURNED the assumption that the primary artifact is a
data format, see #2; pass 2: cross-domain-reinstantiation + organon-
construction over a `map`, prompts hierarchicalness/discovery-vs-invention/
reversibility — produced #3 via the factory_bot "mystery guest" analogue and
the tableau-extends-tableau shape; pass 3: tree-finding + abstraction-lift
over a `timeline`, prompts decomposability/cardinality/materiality —
CONFIRMING, with one addition (#5), which is where convergence was called) ·
CAPTURE: spec §3.

#2 [G1] — Is the primary artifact DATA (a JSON format) or CODE (a builder)? ·
DECISION: **a builder, in code. The JSON is a front-end over it.** ·
WHY: the two customers want different things. A test wants type-checking,
refactorability and no parse cost; the corpus and a human sketching want a
file. Choosing data-first would make every test pay JSON to build a fixture
and would put the fixture's shape outside the compiler. Choosing code-only
would leave the corpus unable to hold a staged scene. A thin serialization
over a builder serves both and keeps ONE construction path. ·
OVERTURNED: the controller's implicit assumption, inherited from `tropes/`,
`systems/`, `sentences/` and `repertory/`, that a new capability of this kind
arrives as a corpus format. Those are all MEASUREMENTS, whose data-ness is
decision 0011; a tableau is a FIXTURE, and 0011 does not reach it. ·
CAPTURE: spec §4.

#3 [Q] — What does an UNSPECIFIED layer default to: inherit from the derived
world, or empty? · DECISION: **split by whether the layer can be fully
stated.** Stateable layers (cast, things, chamber geometry, light, day)
default to EMPTY. Unstateable layers (terrain, climate, species roster, sky)
INHERIT. ·
WHY, and it is the whole point of the campaign: if an unspecified cast
inherited `derive_npcs`, a tableau that stipulates a chamber but not a cast
would silently depend on what the seed happened to place — which is exactly
the complaint ("without having to rely on seeds, because that suuuuucks").
This is factory_bot's *mystery guest*: a test passes because of something the
factory created that the test never mentioned. Hermetic-by-default for
everything you could have written down is what makes a tableau reproducible
without a seed. ·
COROLLARY: **absent and empty must be distinguishable** in the serialization
(`cast: []` means nobody; `cast` missing means the same, but `things: []` vs
absent must not silently differ) — a negative tableau ("this room but with
nobody in it") is how a shadowcasting test isolates a variable, and JSON's
null-vs-missing is the classic way to lose it. ·
ideonomy passes: 1 (pass 2) / 0 overturns · CAPTURE: spec §5.

#4 [Q] — Where does the injection happen? · DECISION: at the two derivation
calls a session already makes — `derive_npcs` (the cast) and `interior_of`
(the chamber). Things need no new seam: `thing::promote` mints and
`located_in_holder_fact` places, and `Session::place_thing_in_hand` (The
Company) already composes them. ·
WHY (measured by reading): `Interior` is ALREADY a hand-buildable builder
(`Interior::new`, `push(kind, within)`) — arbitrary chamber geometry needs no
new construction machinery, only an injection point. `Body` is a `pub` struct
whose `species` is a plain `String`, so a drow requires nothing but
constructing one. The single genuine gap is that `Session::start` derives its
roster internally and `bodies()` is read-only. ·
CAPTURE: spec §6.

#5 [Q] — Does a tableau invent a new override idiom? · DECISION: **no —
extend `PossessOpts`.** ·
WHY (precedent, three instances): `PossessOpts` is already a record of
optional overrides applied at session start, and `SkyPins`/`TerrainPins`/
`SettlementPins` are the same idiom at the genesis layer. A tableau is that
pattern extended past the point where the world stops being derived. Adding a
field inherits every existing caller and adds no second way to start a
session. ·
ideonomy passes: 1 (pass 3's tree-finding surfaced the sibling family) /
0 overturns · CAPTURE: spec §6.

#6 [Q] — How does the corpus keep a staged scene honest? · DECISION: scenes
gain `provenance: found | staged`, and the verdicts read differently: a
STAGED `AUTHORED` means "the machinery carries this", a FOUND `AUTHORED`
means "the world produces this". ·
WHY: conflating them is #0's original error and must not be re-committed at a
lower level. It also makes `UNWITNESSED` MORE meaningful, not less — it stops
meaning "we could not build it" and starts meaning "nothing assembles this on
its own", which is the gap actually worth closing. ·
CAPTURE: spec §7.

#7 [G5] — Task 2 (the chamber seam) DEFERRED, and not for cost ·
DECISION: defer, with the reason recorded rather than the task quietly
dropped. · WHY: the plan specified the wrong object. It said inject an
`Interior` (a room's furniture: hearth, doorway, bed); shadowcasting runs on
the `Lattice` (the grid of cells), which is DERIVED —
`embed_with(structure, brief, extent, seed)`. Supplying an `Interior` would
let an author say "there is a hearth here" and never "the wall is here, so
the light falls there", which is the whole of a lighting test. ·
AND IT OPENS A FORK THE CAMPAIGN HAD NOT EARNED: a lattice supplied wholesale
(total control, including rooms the world's own rules would never produce)
versus constraints an embedder honours (coherent, less arbitrary) — the same
permissive-vs-safe question settled for creatures, deserving the same
deliberate answer. · NOT ON THE CRITICAL PATH: the drama stages and plays
OUTDOORS, proved by probe before the deferral was taken.

#8 [G5] — The ledger's own home moved mid-campaign · DECISION: this file is
now `docs/superpowers/ledgers/2026-08-31-the-tableau.md`, committed. ·
WHY: decision 0486 (The Cartulary) landed on main during execution — a
campaign's decision ledger is a committed document, not scratch, because the
promote-at-close remedy failed five recorded times. This campaign absorbed it
and moved rather than becoming the sixth.

#9 [G5] — Task 5's conversion target does not exist as planned ·
FINDING: the plan said "convert a test that pays genesis and asserts on an
arrangement it did not choose". The obvious candidate,
`common::world_that_draws_a_creature`, has ZERO live call sites — converting
dead code proves nothing. The live one,
`world_where_an_unsensed_creature_arrives`, is `#[ignore]`d, costs a recorded
233.72 s, FAILS ON ALL 64 SEEDS, and leaves the wait verb's arrival narration
with no witness of any kind. Its own ignore reason names the remedy: "a
day-parameterised placement seam able to pre-stage a same-tick position
change". A tableau is most of that seam and lacks the day parameter. ·
DECISION: report the measurement (3.35 s staged vs 233.72 s hunted, and the
expensive one does not work) and carry the arrival test as a followup rather
than starting research at the close.
