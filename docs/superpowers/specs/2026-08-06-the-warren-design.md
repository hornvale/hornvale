# The Warren — design

**Status:** G3 APPROVED (Nathan, 2026-08-06), **including the census-regen and
golden re-pin carve-out of §8.1**. Planning.
**Date:** 2026-08-06
**Campaign:** C2w of the peoples program
(`2026-08-03-the-peoples-program-design.md`), **inserted 2026-08-06 between
C2b and C2c** at the owner's direction, on the same grounds C2t was inserted:
every dwarf authored before it would be authored in a frame it changes. Runs
after C2b (The Long Age, merged); **blocks C2c (The Delvers).**

The Deep Realm gave the world an inside, gave two of its creatures honest
subterranean niches, and shipped a function that says what a chamber's
environment is. Nothing calls that function.

So the world currently scores a cave-dwelling, light-shunning, damp-loving
creature against the sunlight and the rainfall of the hillside above it. This
campaign makes the placement layer ask which realm a creature lives in before
deciding whether it thrives there.

## 1. What was measured before anything was designed

**`subterranean_substrate` has exactly one consumer in the workspace, and it is
a test.** `windows/worldgen/src/lib.rs` defines it `pub`; the only non-doc
reference outside its own definition is
`windows/worldgen/tests/deep_realm_rehome.rs`.

**The live path scores everything against the surface.**
`demography_report_with_beta_from` collects `wc.biosphere.iter()` — *the whole
component set, fauna included*, in its own comment's words — and hands it to
`per_species_suitability`, which builds one `substrate_field` (surface) and
scores every kind against it.

**So C2a's rehoming is half-connected.** Xorn's and rust monster's niches were
re-authored for true darkness and `SUBTERRANEAN_MOISTURE = 0.90`, and the live
suitability layer evaluates those niches against a sunlit, climate-driven
surface cell. The trait is expressible and unread — **rung 2 of the program's
own probe-validity ladder**, arriving in the campaign immediately before the
one that depends on it.

This is not a criticism of C2a, which measured the rehoming correctly in the
frame it built and shipped a byte-identical world. It is the consumer half that
campaign explicitly did not build, surfacing where the ladder predicts it will.

### 1.1 Caves are rare, and that is the whole shape of the problem

The Deep Realm measured 469,122 land cells holding 55,947 caves — **caves are a
~12 % minority of land cells, and roughly half of those are sealed.** A
subterranean kind scored against subterranean conditions *everywhere* would
draw capacity from cells with no void in them at all.

`terrain.cave_at(cell) -> Option<Cave>` already exists and is already read in
`windows/worldgen/src/lib.rs`'s terrain report. The availability gate is
therefore a read, not a new mechanism.

## 2. What this is not

- **Not the dwarves.** C2c authors the roster. This campaign authors no kind;
  its consumers are xorn and rust monster, which C2a already authored and which
  are waiting.
- **Not chamber-level occupancy.** Capacity stays keyed by **cell**. A cell
  gains a subterranean *reading*; it does not gain a population keyed
  `(cell, chamber)`. The program spec's original `(cell, stratum)` promise was
  superseded by decision 0105's chamber graph and is not revived here.
- **Not a subterranean supply field.** Supply stays surface-fed
  (allochthonous) by design. Chemosynthesis is The Keeping's step D
  (`BIO-chemotrophy`) and must not be smuggled in.
- **Not a realm derived from the niche.** See §3.2 — that would re-bless the
  proxy C2a removed.
- **Not byte-neutral.** See §5. This campaign changes what the world computes,
  and it says so up front rather than hoping.

## 3. The design

### 3.1 A kind declares its realm; the world decides where that realm exists

The keystone, and the cell no simpler option occupies:

> **Authored realm × per-cell availability.** A kind declares that it lives
> underground. The world decides *where* underground is.

Both halves are load-bearing. Without the authored half, realm has to be
inferred from niche values, which is the proxy this program is trying to
delete. Without the availability half, a subterranean kind draws full
subterranean capacity on every land cell, including the ~88 % with no cave in
them — a strictly worse model than the surface-scoring it replaces.

### 3.2 Why realm is authored rather than derived

You *can* predict a kind's realm today from its niche: insolation optimum ≈ 0
and moisture ≈ 0.90 identifies xorn and rust monster exactly. That
predictability is **circular** — it holds only because C2a authored those
values to *mean* subterranean. Deriving realm from them would re-establish the
proxy as the encoding, which is the defect F5 named and C2a spent a campaign
removing.

Authoring realm makes the claim falsifiable in the other direction: a kind can
now be declared subterranean and turn out to fit badly there, which is a
finding rather than a contradiction in terms.

### 3.3 `HabitatRealm`, in a sparse component store

```rust
/// Which realm a kind's carrying capacity is scored in.
pub enum HabitatRealm {
    /// Scored against the surface substrate. Every kind not listed.
    Surface,
    /// Scored against the subterranean substrate, gated by cave availability.
    Subterranean,
}
```

Carried in `habitat_realm_registry() -> ComponentStore<KindId, HabitatRealm>`,
**sparse**: a kind absent from the store is `Surface`. It ships with **two
rows** — `xorn` and `rust-monster`.

**Sparse is right here for exactly the reason a field was right in The Long
Age.** That campaign's D2 was overturned on the rule that a storage precedent
transfers on *consumer count*, not component shape: the life schedule had six
consumers that each already held the biosphere row, so it rode the row. This
has **one** consumer — `per_species_suitability` — which does not hold a row
per kind at all but a `&[&BiosphereTraits]` slice, and is reached through
`demography_report_with_beta_from`, which does hold the `wc`. One consumer,
two occupants, twenty-eight silent defaults: a sparse store, following
`dispersion_registry`.

The same rule, applied honestly, gives the opposite answer to the one it gave
last campaign. That is the rule working, not a contradiction.

### 3.4 What `per_species_suitability` does

It already builds one `substrate_field` and hoists it out of the per-species
loop. It now builds **two** — the surface field, and that field mapped through
`subterranean_substrate` — still hoisted, still computed once.

For each kind it selects the field its `HabitatRealm` names, and for a
`Subterranean` kind multiplies the per-cell result by a **cave-availability
factor**: `1.0` where `terrain.cave_at(cell).is_some()`, `0.0` where it does
not. A void that does not exist offers no habitat.

The gate is deliberately **binary rather than graded by aperture**. The Deep
Realm's aperture scale reserves its lowest rung for a sealed cave — a real void
with no way in — and a sealed cave is still habitat for something that is
already inside it. Grading by reachability would conflate *can a creature live
there* with *can a walker get there*, which are different questions and the
second is `MAP-underworld-reachability`'s, not this campaign's.

### 3.5 The `Surface` path must be bit-identical

Every kind not in the store must be scored exactly as it is today, with the
same operation order, so that any world movement this campaign produces is
attributable to the two kinds it re-homes and to nothing else. The second
substrate field is built but never read for a `Surface` kind.

This is what makes §5's measurement interpretable: without it, a drift could be
the new field's arithmetic rather than the realm change.

## 4. Blast radius

Orientation only — **the compiler is the enumeration.** Never silence a
missing-field, arity, or exhaustiveness error with a wildcard or a stub.

- `domains/species/src/lib.rs` — `HabitatRealm`, `habitat_realm_registry`,
  re-exports, `impl Component`.
- `windows/worldgen/src/lib.rs` — `per_species_suitability` (signature gains
  the realm lookup and terrain access), `demography_report_with_beta_from`.
- `windows/worldgen/src/components.rs` — `WorldComponents` gains
  `habitat_realm`; `from_stores` gains a parameter. **Ten callers** (measured:
  2 in `cli/tests`, 1 in `windows/worldgen/tests`, 2 in `components.rs`, 4 in
  `lib.rs`, 2 in `windows/lab/src/roster.rs`).
- `windows/worldgen/tests/deep_realm_rehome.rs` — C2a's probe; it should now
  assert the *live* path agrees with what it measured by hand.

`per_species_suitability` is `pub` and has **ten call sites in eight files** —
the live one in `lib.rs` plus seven test files:
`generalist_baseline`, `generalist_distinctness`, `waterline_probe`,
`occupancy_readout`, `keeping_probe`, `non_void_roster`, `demesne`. Each gains
an argument.

*(This paragraph first said "five test callers", counted off a grep piped
through `head -5`. Corrected before the plan was written. A grep is only as
complete as the grep — and a truncated one is worse than none, because it
looks like an answer.)*

## 5. Preregistration

Frozen 2026-08-06, before any implementation code. Decision 0016.

**This campaign is not byte-neutral, and its size is a measurement, not a
prediction.** The Deep Realm's retrospective records that re-authoring these
same two niches produced a 1e-4 golden drift on *unrelated* creatures, through
`niche → suitability → the demography coexistence fit → the shared predator /
prey pressure fields → every other creature's affect`. Changing the *frame*
those niches are evaluated in travels the same path.

**P1 — the direction.** Rust monster's mean subterranean suitability over
cave-bearing land cells is **greater** than its current surface suitability.
C2a measured the ratio at ~2.5 by hand; the live path should agree in
direction. Xorn's should be **flat within noise** — its potency buys a
sovereignty floor and its devotions are near-zero on every axis, so no curve
moves it. *That asymmetry is C2a's finding, and reproducing it through a
different code path is the check that this campaign wired the right thing.*

**P2 — the range collapses.** Both kinds' *total* habitat falls, because the
cave gate zeroes ~88 % of land cells. A subterranean creature gains fitness
where it lives and loses everywhere it never should have been scored. **If
total habitat rises, the gate is not working.**

**P3 — world identity moves, and the campaign reports by how much.** The
committed seed-42 world, the gallery almanacs, and both census fixtures are all
expected to move. The deliverable is the *measured* magnitude and its
attribution, not a number predicted here.

**Falsification.** If P1 fails — if scoring these creatures in the frame they
were authored for does **not** improve their fit — then either
`subterranean_substrate` is wrong or C2a's niches are, and the campaign's
premise is broken. Report it; do not tune either to rescue the prediction.

**A carve-out, stated rather than assumed: this needs a census regen and
golden re-pins, and both require the owner's explicit authorization.**
**Granted by Nathan at G3, 2026-08-06.**

**Where the census runs is not negotiable and is not this box.**
`scripts/census-canonical-host.txt` names **`lefford`**; this Mac
(`hostname -s` = `MacBookPro`) is refused outright by
`require_canonical_census_host`. CLAUDE.md's "the sanctioned refresh is local"
means *not AWS* — it does not mean *here*. The refresh is therefore: push the
branch, then `ssh lefford` with `HV_CENSUS_REF=<full-sha>` (a SHA, never a
branch name), and commit and push the result from there.

## 6. The mutation this campaign owes

The program's shared acceptance criterion: a green test proves the code ran;
only a mutation proves the axis is visible.

**M1.** Force `habitat_realm_registry()` to return an empty store — i.e. treat
every kind as `Surface`, the pre-campaign behaviour. Rust monster's live
suitability over cave cells must return to its surface value and the test must
go **RED**.

**M2.** Force the cave-availability gate to `1.0` everywhere. Total
subterranean habitat must jump to all land cells and the range test must go
**RED**.

Both must be run, reddened, reverted, and their output pasted into the commit
message. Note that unlike The Long Age's M2, **both are reachable**, because
this campaign ships with two live occupants rather than none — the registry-
locked-consumer problem that campaign hit does not arise here.

## 7. Definition of done

- §3 implemented; `Surface` path bit-identical (§3.5) asserted, not assumed.
- §5's P1/P2 measured and reported; P3's magnitude measured and attributed.
- §6's two mutations run, reddened, reverted, output recorded.
- `make gate` green; artifact drift reviewed deliberately rather than
  rebaselined reflexively.
- Census regen and golden re-pins **only after explicit authorization**.
- Chronicle, retrospective, freshness sweep, Confidence Gradient check.
- The program spec gains its C2w paragraph and C2c's is amended to say the
  realm is now real.

## 8. Flagged for review

1. **Census regen + golden re-pins are required and are a carve-out.** This is
   the first campaign in this program that moves worlds. Explicit
   authorization needed; not implied by approving this spec.
2. **The cave gate is binary, not graded by aperture** (§3.4). A sealed cave
   counts as habitat. Defensible — a sealed void still houses what is already
   in it — but it is a modelling choice a reader may want the other way.
3. **`per_species_suitability` gains terrain access it did not need before.**
   It already takes `&GeneratedTerrain`, so this is a read rather than a new
   parameter — but it widens what the function is *about*.
4. **Sparse store here, field last campaign.** The same consumer-count rule
   gives opposite answers in consecutive campaigns (§3.3). Stated explicitly
   so it reads as the rule working rather than as inconsistency.

## 9. Decisions

Promoted from the autopilot ledger on G3 approval (Nathan, 2026-08-06).

**D1 — C2c splits in two; C2w is inserted between C2b and C2c.** Nothing in the
placement path reads subterranean conditions, so authoring Mountain and Duergar
dwarves today would mean authoring them with a low-insolation surface curve —
the exact fake C2a's F5 removed. Follows C2t's insertion precedent. Brought to
Nathan because the answers diverged materially and the only precedent was the
program spec's own C2c paragraph, itself written before C2a shipped.

**D2 — Realm is authored, not derived.** Realm *is* predictable from the niche
today, but circularly: it predicts only because C2a authored those values to
mean subterranean. Deriving from them re-establishes the proxy as the encoding.
Authoring also makes the claim falsifiable the other way. *Discarded:* derive
from niche values; per-cell best-of-two (realm becomes emergent, so C2c could
not author Hill-vs-Mountain as a design statement); a list in worldgen
(authoring in the wrong layer).

**D3 — The keystone: authored realm × per-cell cave availability.** The
lattice's one unoccupied cell. Without the gate a subterranean kind draws
capacity on every land cell, including the ~88 % with no void — strictly worse
than what it replaces. *This decision was overturned once during its own
ideonomy pass: the first form had no gate, and the distribution axis is what
surfaced it.*

**D4 — The gate is binary, not graded by aperture.** A sealed void still houses
what is already inside it; grading by reachability conflates *can a creature
live there* with *can a walker get there*. The second belongs to
`MAP-underworld-reachability`.

**D5 — A sparse `ComponentStore`, not a field on `BiosphereTraits`.** The
consumer-count rule from The Long Age, applied honestly, gives the opposite
answer here: one consumer, two occupants, twenty-eight silent defaults.

**D6 — Not byte-neutral; the magnitude is measured, not predicted.** Census
regen and golden re-pins are required; authorized by Nathan at G3. The census
runs on **lefford** — this Mac is refused by the canonical-host guard.

## 10. Readout

Measured 2026-08-06 (plan Tasks 3–4), before any re-pin. Full detail in
`.superpowers/sdd/readout.md` (dies with the worktree); this section is the
durable copy.

**The wiring check (Task 3, seed 42, `deep_realm_rehome.rs`).** Both
re-homed kinds scored through the live `per_species_suitability` path (real
realm slice vs. every kind forced to `Surface`), on cave-bearing land cells
only: rust-monster's live/surface-forced ratio is **2.603** (C2a measured
~2.5x by hand); xorn's is **0.977** (C2a measured 1.02). The xorn case is the
wiring check — reproducing near-flatness through a different code path
proves the right thing got connected.

**P1 — direction, CONFIRMED (25 seeds, `warren_readout.rs`).** Pooled mean
suitability over every cave-bearing land cell across all 25 seeds:
rust-monster before=0.005391 after=0.013783 (**ratio 2.557**); xorn
before=0.002087 after=0.002043 (**ratio 0.979**). Matches §5's prediction and
the single-seed wiring check closely.

**P2 — range collapse, CONFIRMED.** Land cells with non-zero suitability,
pooled over 25 seeds: rust-monster and xorn both fall from 390,813 (100.0%)
to 46,993 (**12.0%**) of all land cells — matching §5's "~88% of land is
cave-free" almost exactly (measured 88.0%). It did not rise; the gate is
working. The two kinds' after-counts are identical, which is structural: a
subterranean kind's non-zero set is exactly "land cells with a cave" (the
availability factor is the only zero-producing term, and the sovereignty
floor keeps fit strictly positive everywhere availability is 1.0) —
independent of which kind it is. Confirmed by reading
`per_species_suitability`'s arithmetic, not inferred from the coincidence.

**P3 — world identity, FALSIFIED.** §5 predicted "world identity moves, and
the campaign reports by how much." Measured instead: **zero of 25 seeds
moved.** `world_after.to_json()` and `world.ledger.len()` matched their
"before" (empty `habitat_realm` store) counterpart at every seed, confirming
over a wider sweep what `cli::tests::lens_purity`'s passing seed-42 fixture
check already showed. Rust-monster and xorn are fauna, not settling peoples —
they never found settlements, so their suitability change never reaches a
committed fact.

**What DID move instead: three goldens, not the world.**
`hornvale-vessel::session_snapshot::the_client_fixtures_are_current`,
`hornvale-vessel::session_snapshot::v1_bytes_are_pinned`, and
`hornvale-lab::affect_trace_golden::seed_42_affect_trace_reproduces_the_pinned_bytes`.
Measured: at seed 42's possessed bugbear's room, the committed vessel-session
fixture shows `"a wild xorn"` then `"a wild carrion-crawler"` as nearby
presences; the live rebuild shows `"a wild carrion-crawler"` then `"a wild
giant-elk"` in the same two slots — no settlement, name, or fact-count
difference (P3 already confirms this). **Hypothesis, not fully traced to the
specific ranking call:** `windows/vessel/src/session.rs:387` and
`windows/lab/src/health.rs:322` both call
`hornvale_worldgen::demography_report_from` at snapshot-read time — a LIVE
re-derivation, not a ledger read — which reaches the now-realm-aware
`per_species_suitability` through `demography_report_with_beta_from`. Since
P1 shows rust-monster's and xorn's suitability changing substantially, the
demography coexistence stack's local density ranking can plausibly reorder
which fauna reads as "present" at a given cell, with no ledger fact
involved — the exact chain the campaign spec and The Deep Realm's
retrospective both name (`niche -> suitability -> the demography
coexistence fit -> the shared predator/prey pressure fields -> every other
creature's affect`). This readout did not walk
`hornvale_demography::coexist::pack`'s ranking for this specific cell/seed
by hand, so the mechanism is reported as a well-evidenced hypothesis, not a
verified cause.

**Bottom line.** Two of the spec's three predictions (P1, P2) held almost
exactly; the third (P3) was falsified — the world does not move, and the
campaign's real footprint is confined to two live, non-ledger read paths.
Task 5 re-pins those three goldens, citing this section.

### 10.1 What the 188-line affect diff actually mixes

The re-pin of `affect-trace-seed-42.txt` moves 188 lines, which reads as a
large drift and is not one. Decomposed by species rather than by line — the
same discipline The Deep Realm's "a compound rate is not a measurement" arrived
at — it is **three different things**:

```
  species           lines differing   of which LABEL changes
  ----------------  ---------------   ----------------------
  bugbear                    0                 0
  kobold                     0                 0
  gnoll                      0                 0
  otyugh                     1                 0
  hobgoblin                  2                 0
  human                      4                 0
  goblin                     4                 0
  rust-monster              40                34     <- the target
  carrion-crawler           36                35     <- a neighbour effect
  ----------------
  only before: xorn      only after: giant-elk       <- the roster swap
```

1. **Six of the nine shared species are effectively untouched** — zero to four
   lines, and **not one label change** among them. The peopled roster's
   emotional life is where it was.
2. **rust-monster is the campaign's target and moved most** (34 of its 40
   differing lines are label changes). Its scoring frame changed; its
   trajectory changed with it. This is the intended effect, arriving where it
   was aimed.
3. **xorn left the trace and giant-elk entered it.** Xorn is gated on cave
   presence now and no longer stands at a cave-free cell. Everything below it
   re-indexed, which is why a naïve label-frequency comparison across the whole
   file reads as a large behavioural shift (`54 Content → 19 Content`) when
   almost all of that is comparing *different creatures at the same index*.

**A caution, stated as a hypothesis rather than a finding.**
`carrion-crawler`'s 35 label changes are the one item not directly explained by
its own scoring — it is not `Subterranean` and its frame did not change. The
evidenced guess is the path The Deep Realm documented: `niche → suitability →
the coexistence fit → the shared predator/prey pressure fields → every other
creature's danger-sense and hunger → its affect`. Its neighbours changed — a
xorn left and a giant elk, which is prey, arrived — so a hunger and danger
profile changing is exactly what that path predicts. **This has not been walked
through `hornvale_demography::coexist::pack` by hand and is not asserted as a
cause.**

### 10.2 The census, authored on lefford

Run at SHA `a1de4635` on the canonical host (640.8 s, 1000 + 1000 rows, 0
refusals). **Exactly two of ~180 metrics moved, in opposite directions:**

```
  per-cell-diversity      mean 3.6998288 -> 3.0603200   (-0.63950889)
    [2,3): 268 -> 535     [3,4): 307 -> 379     [4,6): 425 -> 86
  composition-variance    mean 0.38070089 -> 0.40926278 (+0.028561893)
```

`census-of-the-meeting` did not move at all — its solo/twin rosters are peopled
and carry no subterranean kind.

**Diversity falls** because two creatures stopped being counted across the 88 %
of land with no cave. **Composition variance rises** because a species present
in one cell in eight differentiates places that a ubiquitous one flattens.
The second is the better argument for the change: correctness was the means,
and a more textured world is what it bought.

`make census-check` passes; the golden-pins tripwire is clean.

### 10.3 Amendment — P1 was confirmed, then falsified by an absorption

**Recorded 2026-08-06, after absorbing 72 commits of main (The Tense, The
Tilth) at the close.** P1 held when measured (§10's 2.557× / 0.979). After the
absorption it reads **exactly 1.000 for both kinds, at every one of 25 seeds**.

**The cause is not this campaign's wiring**, and the resolution kept both
sides' changes correctly. The Tilth (stage 5) replaced the product of four
condition tolerances with Liebig's law of the minimum. `tolerance_liebig`
floors temperature, moisture and insolation by the sovereignty floor and calls
elevation with floor `0.0`; **its own doc states the consequence** — "a floored
axis can never bind, so whichever axis is left bare becomes the sole
determinant wherever it dips below the others' floor."

Measured (`windows/worldgen/tests/warren_liebig_probe.rs`, rust-monster, seed
42 cave-bearing cells):

```
                  temperature   moisture   insolation   elevation    min
  surface            0.7327      0.5850      0.4670       0.2498    0.2498
  subterranean       0.7327      0.7865      0.8399       0.2498    0.2498
  sovereignty floor = 0.466288
```

The subterranean substrate improves exactly the two axes it was designed to
improve, and the minimum is elevation in both frames — below the floor, and
passed through unchanged because `subterranean_substrate` inherits
`height_asl_m` from the surface cell (The Deep Realm's deliberate choice: a
real depth coordinate was out of its scope).

**What still holds.** P2 is unaffected — the cave gate zeroes 88 % of land
(390,813 → 46,993 non-zero cells), because a hard mask is not a tolerance and
does not pass through the minimum. P3 remains falsified in the other direction:
world identity moved at zero of 25 seeds.

**What was NOT done, deliberately.** Neither flooring the elevation axis nor
switching on The Tense §3.3's shadow-mode two-tier tolerance. Both would
relitigate a calibration two campaigns had just made, to rescue a prediction
after unblinding — precisely what this project forbids doing quietly.

**The assertion is inverted rather than removed.** `warren_readout.rs` now pins
`ratio == 1.000` as a **tripwire**: when a tolerance model lands in which a
non-lethal preference can bind, that test reddens and tells whoever landed it
that The Warren's other half just came alive. Quieting the assertion instead
would have deleted the finding.

**The generalisable statement**, which is the campaign's most precise result:
*a non-lethal preference cannot matter while an unfloored axis is scarcer.*
That is a sharper account of the tolerance model's current limit than anything
in the tree before, and it was bought by a prediction failing.
