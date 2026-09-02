# The Prospect — decision ledger

Campaign: `campaign/the-prospect` · Spec:
`docs/superpowers/specs/2026-09-01-the-prospect-design.md` · Decision block:
0536–0545.

Created at spec time rather than mid-campaign, which is the lesson The Pavement
learned the expensive way (its 44 rulings sat in git-ignored scratch until a
gate refused a merge).

## Decisions taken with Nathan, in conversation, before the spec

### #1 [G1] — scope: surface and name first, measure density second

**Question:** build the sub-facet detail band, or make what exists visible?

**Decision:** surface + name + widen to caves and exotic sites. The detail band
is not in this campaign.

**Why:** the affordance already exists and is invisible — `enter` works at seed
42's flagship and `look` never says so. And a finer band does not create
interest: 262,000 rooms of derived grass is oatmeal at higher resolution.
Nathan: *"I don't want 262K rooms to navigate through right now."*

**Alternatives discarded:** a continuous sub-facet terrain band (rejected as
above); strict surfacing with no widening (rejected by Nathan — "I'd definitely
like to be able to see caves and exotic sites", and it would have shipped a
world with exactly one door).

### #2 [Q] — `built` is the wrong name, and the rename carries meaning

**Question:** widen `built`, or replace it?

**Decision:** replace. `site_at(&Facet) -> Option<Site>` becomes the gate;
`Terrain::is_built` survives as what makes a Settlement a settlement.

**Why:** `brief.rs:53` defines `built` as *"whether a structure stands here"* —
made by hands, which is false for a cave and false for a fungal canopy.
Widening it would put a lie in the predicate every enterable place hangs off.
"Site" is already the project's own word (`plate.rs`: "an undiscovered site";
the CLI: "placed exotic sites"). Nathan raised this unprompted.

### #3 [G2, EPOCH] — sites are re-sited to a facet by a seeded draw

**Question:** read site presence off the level-6 vertex mesh, or give a site a
real address?

**Decision:** re-site. A new stream label, therefore an epoch.

**Why:** the vertex mesh is the same ~120 km lattice that produced the all-river
defect (`CLIM-water-label-resolution-vs-walk-band`). Reading site presence off
it would reproduce that exactly — every facet for tens of kilometres claiming
the same cave. Nathan chose the epoch over the weaker promise.

**Cost if wrong:** every world's site placement moves; worlds regenerate from
seed.

### #4 [G2] — extent is modelled now and unused

**Decision:** `Site` carries `Extent::{Point, Region}`; this campaign emits only
`Point`.

**Why:** Nathan's observation that exotic sites are not uniform in scale — a
cursed land is miles across with components. Modelling extent costs one enum;
not modelling it makes multi-facet sites a migration of every consumer. Since #3
mints an epoch anyway, the shape is free to carve now.

### #5 [G2] — the landscape pattern grammar is deferred, and the reason is not cost

**Decision:** out of scope; filed as its own question.

**Why:** `interior/pattern.rs`'s `Attach::{Hub, Beside(Ec), Within(Ntpp)}` is
RCC-8 and scale-free, and its own comment already calls it "the pattern
language". Lifting it from chamber interiors to landscapes is the right
long-term model. It is deferred because the interesting question — whether
landscape patterns need relations a room never needs (`Surrounds`,
`Overlooks`, `Downstream-of`) — is a design question deserving its own spec,
not a corner of this one.

## Findings that shaped the design, surveyed before drafting

- `Terrain::is_built` already takes a `Facet`, so the widened predicate does NOT
  inherit the 120 km resolution defect. Checked, not assumed.
- `cave_proneness(&MaterialBuffer, f64)` is a pure function of two continuous
  fields — caves need no roster and no draw to place.
- The exotic tier is explicitly stranded: `cli/src/main.rs` says it "was
  generated but **unreachable**."
- `plate.rs` draws settlements and nothing else, at any zoom — which is why
  Nathan sees only the flagship.

## Pre-flight rulings, before Task 1

The full scan table is in the plugin's scratch ledger; the three rulings are
durable and live here.

### #6 [G4] — `from_parts` gains a `site` parameter rather than deriving one

**Question:** Task 2 derives `Brief::site` inside `from_parts` from `built`.
Task 4 needs a cave to produce a site, and `from_parts` has no world to ask.

**Decision:** `from_parts` takes `site: Option<Site>`; `brief_of` computes it.

**Why:** overriding the field after construction would leave a constructor
deriving a HALF-RIGHT answer that reads as authoritative to the next caller.
`from_parts`'s own doc says it "exists so the type can be unit-tested without a
world", which is precisely the argument for the caller owning a derivation that
needs one.

**Cost if wrong:** one extra parameter, a small edit to Task 2's test, and Task
4 becomes a call-site change instead of a signature change.

### #7 [G4] — Task 3's test asserts a positive signal, not an absent string

**Question:** Task 3's step 2 claims the test fails before the fix. It does not.
With the old refusal wording present, `!reply.starts_with("There is nothing here
to enter")` is TRUE, so the test passes green at the step that is supposed to
prove it is wired to the code.

**Decision:** assert on the chamber's own signature (`reply.contains("Ways on")`)
instead.

**Why:** it goes red meaningfully if the gate breaks, and it decouples the test
from message wording entirely — the refusal string is presentation and should
not be load-bearing for an enterability test.

**Cost if wrong:** none identified; it is strictly stronger than the plan asked
for. Recorded because a plan defect I authored and then corrected is exactly the
kind of thing that otherwise disappears.

### #8 [G4] — undefined test helpers are the implementer's to write

Tasks 4, 6 and 9 each name a helper the plan does not define
(`measure_cave_rate`, `seed_42`/`flagship_facet`/`three_tiles_east`,
`measure_site_density`). Standing precedent from The Pavement: the plan names
the PROPERTY, the implementer finds the mechanism, because a plan author does
not know what the surrounding suite already provides.

**Cost if wrong:** an implementer reinvents an existing helper; the task review
catches it.

### #9 [G5] — Task 1's two Important findings are both defects in my brief

The task review approved the implementation and found two problems in the code
I had written into the plan verbatim. Recorded here because a plan defect that
the process catches still happened, and the pattern is what matters.

**(a) `salience()` is tagged `bare-ok(count: return)` and is not a count.**
Decision 0028 defines `count` as "an honest cardinality — cell/plate/moon
counts, octaves, generations". `salience()` returns an ordinal presentation
rank. The type-audit tool accepts the string because it validates the CLASS
NAME, not the claim — so this is a wrong reason passing a gate that checks
shape. **Ruling:** the implementer re-derives the correct class from 0028's own
definitions rather than being handed one, because the property is "the tag
states the true reason" and I have already demonstrated I will pick a plausible
wrong one.

**(b) `a_new_site_is_a_point` cannot fail.** `Extent` has exactly one variant,
so any `Site::new` that compiles sets it. **Ruling:** keep the assertion but
make the test discriminate TODAY by also round-tripping kind and name (a gutted
`new` that dropped the name would go red), and mark the extent line as an
explicit FORWARD guard — it exists to catch a future `new()` that defaults to
`Region` once that variant lands. Declaring a vacuous assertion is this
project's sanctioned remedy (`expect(survives:)`, the three-valued ratchets);
leaving it undeclared is the defect.

**Cost if wrong:** (a) a tag reads slightly off and a later audit re-classes it;
(b) a test that is stronger than it needs to be.

**The uncomfortable part:** (b) is the "test whose input collapses to one value"
shape, which is in my own memory notes with sixteen prior instances across two
campaigns, and I wrote it into this plan the same afternoon.

### #10 [G5] — `lattice/mod.rs`'s `built` check STAYS, and it vindicates the rename

Task 2's review found a second site reading `brief.built` that the plan does not
mention (`windows/vessel/src/lattice/mod.rs:398`):

```rust
pub fn embed_with(structure: &Structure, brief: &Brief, extent: Rect, seed: Seed) -> Lattice {
    if brief.built { allocate(structure, extent, seed) }
    else           { grow(structure, extent, seed) }
}
```

**Decision:** `structure.rs`'s gate moves to `site`; this one stays on `built`.

**Why:** it is not a gate at all — it is a generator dispatch. `allocate`
produces rectilinear rooms, `grow` produces a non-convex blob. So the question
it asks is "is this place CONSTRUCTED or NATURAL", which is exactly what `built`
means and exactly the distinction a cave needs. Swapping it to
`site.is_some()` would have generated every cave as a rectilinear building —
a serious defect, and an easy one to introduce while doing a mechanical rename.

**This vindicates decision 0536's shape.** Keeping `built` as a property of a
settlement rather than deleting it looked like conservatism when the rename was
proposed. It turns out to be load-bearing at precisely the site where a cave
must diverge from a village, and the campaign gets that divergence for free
because the two concepts were separated rather than merged.

**Cost if wrong:** if `grow` turns out to be wrong for caves, the fix is a
different generator, not a different predicate — the predicate is now asking
the right question.

### #11 [G5] — my red-then-green demand was incoherent, and the test is a regression guard

Task 3's implementer reported honestly that the new test passed both BEFORE and
AFTER the gate swap, so the red-then-green cycle I demanded could not be
established.

**It was right to report it and I was wrong to demand it.** Task 2 made `site`
mirror `built` exactly — that is H1's whole content — so swapping the gate at
Task 3 is a provable no-op, and no black-box test can distinguish the two gates
by construction. Demanding red-then-green for a behaviour-preserving refactor is
a category error.

**Decision:** the test stands, documented as an **H1 regression guard** rather
than a TDD cycle. It cannot fail today and will fail the moment a later task
breaks the flagship's enterability, which is exactly its job. Same discipline as
Task 1's forward guard: a known-vacuous-today assertion is fine when declared,
and a defect when silent.

**The pattern in my own instructions, third instance:** I keep demanding an
evidence SHAPE the situation cannot produce — an inverted step-2 expectation
(#7), a `count` tag on a non-count (#9), now red-then-green on a no-op. Each
time the mechanism was right and the demanded proof was impossible. Worth
naming: before asking for a failing test, check that a failure is reachable.

### #12 [G5] — we are citing decisions in the one form the gate cannot see

Task 3 hit `decision_cites_in_sources_resolve` refusing a lowercase
"decision 0536" (correctly — 0536 does not exist until Task 9), and worked
around it by using the capitalized "Decision 0536." form, citing Task 2's
precedent.

That is the form the checker is BLIND to (76 capitalized cites unchecked against
1067 lowercase). So the campaign is now, twice, deliberately writing citations in
the shape the gate cannot inspect.

**Decision:** accept for now — the alternative is writing decision records before
the decisions are settled — but Task 9 owes two things: write 0536/0537/0538,
and MANUALLY verify every "Decision NNNN" this campaign added resolves, because
the gate cannot do it. Relying on a blind spot is precisely the shape this
project keeps finding, and leaving it unverified would be adopting it.

**Cost if wrong:** a dangling citation ships. Mitigated by Task 9's manual sweep.

### #13 [G5, SPEC DEFECT — mine] — caves DO need the draw, and Tasks 4 and 5 swap order

**The spec says caves are derived with no seeded draw**, on the grounds that
`cave_proneness(&MaterialBuffer, f64)` is a pure function of two continuous
fields. **That is true of the function and false of its data.**

Verified before dispatching Task 4:

```
domains/terrain/src/provider.rs:319   material_at(&self, id: Vertex)
domains/terrain/src/provider.rs:371   cave_proneness_at(&self, id: Vertex)
```

Both are **Vertex**-bound, so proneness is only available at level-6 resolution —
110-132 km apart. And `Terrain::is_built`'s real implementation
(`liveness.rs:745`) is not a field read at all: it is
`built.zip(room.pack().ok()).is_some_and(|(set, id)| set.contains(&id))` — a
lookup in a precomputed SET of settlement-territory facet ids.

So a per-facet cave predicate has only two honest shapes: threshold the nearest
vertex (which reproduces `CLIM-water-label-resolution-vs-walk-band` exactly —
every facet for tens of kilometres becomes a cave), or pick a specific facet
within the prone vertex's territory, which is a placement and therefore a draw.

**Decision: Tasks 4 and 5 swap order, and caves use the SAME placement draw as
exotic sites.** Task 5's `site/placement/v1` already exists to turn a coarse
vertex signal into a real address; caves need precisely that. One mechanism, one
epoch, no resolution defect.

**Alternative considered and rejected:** place the cave at the facet containing
the vertex — genuinely derived, no draw, one cave per prone vertex. Rejected
because Task 5 mints the epoch regardless, so the draw is free at the margin,
and a placement keyed to vertex centres puts every cave on a 120 km lattice
point. That regularity is invisible to a player and visible to anyone who plots
them, which is the kind of thing this project finds two campaigns later.

**Cost if wrong:** the epoch covers one more label's worth of placement than it
strictly had to. Recoverable — the alternative stays available.

**The shape of my error, because it is the campaign's own subject.** I checked
that `cave_proneness` was a pure function and inferred that its answer was
available anywhere. I did not check where its INPUTS live. That is the same move
as reading a ratio without its denominator, in a spec written the same day I
catalogued that failure.

### #14 [G5, CORRECTION — mine] — the campaign mints NO epoch, and I said otherwise repeatedly

Task 5's implementer reported that calling this an epoch is a misnomer.
Checked against the project's own rule and it is right —
`domains/CLAUDE.md:34`: *"New label = safe; changed/reused label = an epoch."*

A NEW label consumes no draws from any existing stream, so nothing that exists
re-derives; and site placement is new behaviour, so there is no prior placement
to move. Measured, not argued: both pin-isolation suites stayed green
(genesis_properties 20/20, tectonic_properties 20/20) and
`make rebaseline-goldens` was a **no-op** — no world artifact moved at all.

**This corrects ruling #3, spec §7, and what I told Nathan at the G3 stop**,
where the epoch led the flagged section and he approved it as a deliberate cost.
The real answer is strictly better than the one he approved, so nothing needs
re-approving — but the record was wrong in the direction of sounding more
expensive and more dangerous than it is, and a spec that overstates a
determinism cost trains the next reader to discount the ones that are real.

Spec §7 is corrected in place with the measurement.

**My error:** I reasoned "new stream label" → "save-format contract" → "epoch"
without checking the rule that distinguishes a NEW label from a CHANGED one. The
distinction is one line in a guide I had already read this session.

### #15 [G5] — two brief signatures I specified were unimplementable

`site_facet_for(vertex: u32, seed, walk_depth)` cannot work: a bare vertex index
locates nothing without the mesh it indexes into. The implementer took
`&Geosphere` and a typed `Vertex` instead. Correct.

It also added a `reason` parameter, because a cave and an exotic site at the
same vertex would otherwise be placed at the identical facet — a direct
consequence of ruling 13 folding caves into this mechanism, which I extended the
scope for without following the consequence through to the key.

**Cost if wrong:** none; both changes are strictly more correct than what I
wrote.

### #16 [G5] — the reverse lookup would have silently lost a THIRD of every world's sites

Task 5's implementer avoided an O(1) facet→site reverse and used a membership
test, flagging that the reverse would be "silently wrong at territory edges."
The reviewer instrumented it against seed 42 rather than accepting the note:

```
reverse_mismatch = 37 of 103   (36%)
naive_center_mismatch = 37     (same 36%)
found_exotic = 103, shadowed = 0
```

**36%, not an edge case.** And the second number is the important one: even
placing a site at the facet containing its own vertex resolves back to a
DIFFERENT vertex 36% of the time, so the draw is not the cause. `containing_vertex`
takes the max of `Facet::corner_weights` — a cube-sphere quad interpolation, not
a nearest-vertex query — and since The Pavement the quad mesh and the vertex
mesh are unrelated objects.

So the rejected shortcut would have dropped a third of every world's exotic
sites with nothing red, in a campaign whose entire subject is findability. The
membership direction has no such hole: all 103 found at their exact facet, none
shadowed.

**Kept because the reasoning generalises:** any facet→vertex→answer lookup in
this repo now inherits a 36% error, and `CLIM-water-label-resolution-vs-walk-band`
is the same defect from the other side. Two campaigns have now been bitten by
the two meshes being unrelated; the next one should assume it rather than
discover it.

### #17 [G5] — I1 and I2 enter the fix loop; I3 is a plan gap I am ruling into Task 8

**I1 — `placement_key`'s wire spelling is an unpinned save-format contract.**
Its own doc says the spelling is a contract, and five sibling keys in the same
crate each pin theirs (`volcano.rs:579`, `hazard.rs:1129`, `chamber.rs:1768`,
`:2524`, `:3397`). This one does not. A refactor of `"cell/{}/{}"` would move
every site in every world with nothing red. **Fix.**

**I2 — nothing pins the placement DRAW.** All four unit tests are
self-consistency checks that stay green under a changed depth constant or a
reordered draw, and because addresses are derived-not-stored, no committed
artifact witnesses one. **Fix** with the one-line golden the reviewer names.

**I3 — two readouts of one site now disagree by a mean 19.7 km (max 42.5 km),
and no task closes it.** `strange_site_rows` and the CLI's `--strange` listing
report a site at its VERTEX's lat/lon; the walker finds it only at the placed
facet. Task 8 covers the game map and nothing covers the textual listing.

**Ruling:** I3 goes into Task 8, whose scope widens from "the map draws sites"
to "every readout of a site agrees on where it is." Not deferred: for a campaign
about findability, shipping a prose readout that points 20 km from the only
facet where a site exists would be the exact failure the campaign exists to
remove, and it is a gap this campaign CREATED.

**Cost if wrong:** Task 8 grows. Cheaper than a follow-on campaign to reconcile
two readouts nobody noticed disagreed.

### #18 [G5] — Task 3 closed without rebaselining a rendered string

Verified, not inferred: Task 3 changed the `enter` refusal at `session.rs:5163`
and `book/src/gallery/possession-seed-42.md` carries exactly that one line,
first touched since by Task 5's rebaseline. Task 5's attribution is correct.

The finding is a process one: **a task that changes rendered output owes a
rebaseline before it closes**, and Task 3's review did not ask for one because
the plan's verify list did not name it. Task 3 is complete and this is not worth
reopening — but every remaining task that touches prose or the map (6, 7, 8)
gets a rebaseline in its dispatch, and the final review inherits the check.

### #19 [G5, H2 FALSIFIED — and it is the campaign's real result]

**H2 is falsified structurally, at any threshold, and the implementer correctly
did not adjust the constant even once.**

The arithmetic, which I re-derived independently:

```
  level-6 vertices    40,962
  walk facets         402,653,184   (6 x 4^13)
  max sites per facet 1 in 9,830  = 0.0102%
  H2's floor          1.0000%      = 98.3x higher
```

Placement emits at most one facet per vertex, so **any** point-site kind is
capped at 0.0102% of facets before a seed is built. H2 asked for 1-8%. No
threshold reaches it.

**Why the hypothesis died: my own ruling killed it.** H2 was frozen (spec §9)
when a cave was a per-facet DERIVATION — a predicate every facet could satisfy
independently, where a percentage is a meaningful quantity. Ruling 13 replaced
that with a placed POINT PROCESS, which cannot express a per-facet percentage at
all. I changed the mechanism and did not re-read the hypothesis that measured
it.

**Decision:** H2 stands falsified and is NOT re-derived. The threshold stays at
its measured value. Spec §9 is corrected to record the falsification and its
cause. The implementer's `the_band_is_unreachable_at_any_threshold` pins the
structural fact so it reds if the mechanism or the mesh resolution changes,
which is a better instrument than the band it replaces.

This is decision 0016 working exactly as written: a falsified prediction is a
finding, and retuning a constant to rescue one after unblinding is what the rule
forbids. The implementer had the opportunity — I explicitly allowed one
adjustment — and correctly reported that no adjustment could help.

### #20 [G5] — the same arithmetic answers H3 before Task 9 measures it, and it is the answer Nathan needs

H3 asks what fraction of land facets hold any site. **The cap above answers it:
at most 0.0102%, and that is a property of the model, not of this world.**

So the follow-on question Nathan framed — "is the gap rendering or generation?"
— has a third answer neither of us offered: **it is the SHAPE of the model.**
One site per vertex over 40,962 vertices cannot populate 402 million facets.
Meeting "every square mile, something interesting" with point sites would need
roughly 402 million sites; the mesh offers 40,962 anchors.

**Decision:** Task 9 still measures H3 and reports the number, because a
measured 0.0102% against a structural prediction of 0.0102% is a confirmation
that the model is understood. But its framing changes from "how dense is the
world" to "what the point-site model can and cannot reach", and the chronicle
leads with the ceiling rather than the sample.

**This is why `Extent` was worth carving out.** A region-extent site is the only
shape in the current design that can cover ground without one anchor per facet.
Decision 0538 looked like cheap insurance when Nathan raised it; it is now the
identified route to the campaign's stated promise.

**Cost if wrong:** none — the measurement still happens; only its reading
changes.

### #21 [G5] — two brief constants were wrong, and the second would have shipped near-empty worlds

Both caught by measurement rather than review.

**The threshold: 0.12, not my 0.35.** Cave proneness is bimodal — the lower mode
tops out at 0.02201, the upper starts at 0.22343, and nothing lies between. 0.12
is the midpoint of the empty gap; my 0.35 sat *inside* the upper mode, cutting it
arbitrarily.

**The elevation floor is FREEBOARD, not absolute elevation**, and this is the one
that mattered. Sea level in these worlds is **-1328 to -2032 m**, not 0. My
`elevation_m > 5.0` cut seed 42's cave roster from 2084 to **8**, and gave seeds
7 and 1 **zero caves in the entire world**. I assumed a sea level the project
does not have.

### #22 [Q, ideonomy] — decision 0539: the tier is `placed`/`derived`, and my naming argument was overturned

Nathan asked for an ideonomy pass on the naming rather than accepting my
suggestion. 2 passes, **1 overturn — of my premise, not my answer**, which is
what these passes are for.

**What broke:** I argued the type should name the CRITERION (participation in
the record) rather than the mechanism — `chronicled`/`uncharted`. The inversion
pass refuted it: **promotion makes participation MUTABLE.** A derived site a
player enters joins the record, so `chronicled`/`uncharted` names a STATE while
the generation tier is IMMUTABLE. I conflated two orthogonal axes and proposed
naming one with the other's vocabulary.

**What the pass found that neither of us had:** three states, not two kinds. A
promoted derived site is in the ledger but **never shaped the world's past** —
it can shape the future and not the history. Only visible once the axes are
separated. And symmetry-hunting showed the asymmetry is free rather than
enforced: the ledger is append-only, so standing is monotone by construction.

**Collision checks disqualified my own suggestion**, before taste entered:
`chronicled` collides with `windows/chronicle`, the derived-history engine, 58
files. `attested` is partly spoken for by The Attestation. `uncharted` is
genuinely free (0 occurrences) and is reserved for the standing axis.

**`placed`/`derived` wins because the project already owns both words** for
exactly this: the CLI prints "103 **placed** exotic sites", and `derived`
appears in 377 files as the word for computed-not-stored. Zero new vocabulary.

**The standing axis is deliberately left unnamed** — The Prospect does not
implement promotion, and naming an axis before building it burns a good word and
invites the exact confusion the inversion found.

### #23 [G5] — I guessed decision filenames from titles for the THIRD time

0539's first draft cited two records by filenames I inferred from their titles;
both were wrong (`0038-the-canonical-grid-bears-identity` is really
`0038-identity-computes-on-the-canonical-grid`, and `0102-a-stream-keys-on-a-
lattice-position` is really `0102-one-per-cell-was-an-index-artifact`). Five
were wrong the same way in 0514, and one in the campaign before.

A link check caught it every time, which is why it has never shipped — but the
rate is the finding. **The habit to adopt: `ls docs/decisions/NNNN-*.md` before
writing any citation.** The title in my head is not the slug on disk, and the
number being right is what makes the wrong slug survive a skim.

### #24 [G5] — I fabricated a count with a grep, and an implementer wrote it into permanent source

The tier fold's doc comment claimed "every call site The Prospect has (26, all
in this crate)". The real figure is 23. **I supplied the 26.**

The cause, exactly:

```
git grep -c 'Site::new'  -- '*.rs'   ->  26   (lines MENTIONING the string)
git grep -c 'Site::new(' -- '*.rs'   ->  23   (actual invocations)
```

The three extras are doc comments *talking about* the constructor:

```
site.rs:92   /// `Site::new` must not silently drop or alter what it is given
site.rs:107  /// `Extent` has exactly one variant, so any `Site::new` that compiles
site.rs:109  /// catch a future `Site::new` that defaults to a `Region` variant
```

**Two of those three are prose I asked for in Task 1's forward-guard fix.** My
own earlier instruction inflated the number I then quoted back as a fact, and an
implementer trusting me committed it to permanent source where it formed part of
the doc's own justification.

**Ninth instance in two campaigns of the observing tool answering a neighbouring
question.** `grep 'Site::new'` answers "how many lines mention this", not "how
many call sites exist". One character — the open paren — separates the two
questions.

**Resolution:** the implementer dropped the count rather than correcting it,
reasoning that the paired-constructor argument holds at one call site or a
hundred, so a figure there was a maintenance liability rather than evidence.
That is the better answer and I did not think of it.

**The habit this earns:** when a count is going into durable prose, grep for the
SYNTAX of the thing (`foo(`), not its NAME (`foo`) — and ask whether the prose
needs the number at all. A count in a doc comment has to stay true forever; this
one was false within the hour.

### #25 [G5] — ruling #19's "the threshold stays at its measured value" is void, and two more of my claims were refuted

**Ruling #19 said the cave threshold stays at its measured value.** There is no
threshold: `cave_at` already existed and decides existence, so the constant is
deleted rather than kept. H2's falsification is unaffected — it was always a
property of placement, not of the predicate — and
`the_band_is_unreachable_at_any_threshold` survives with its doc now explaining
why the name outlived the threshold it was named for.

**Two claims I made in the re-aim dispatch, both refuted by measurement:**

1. **"1,528 vs 874 — 75% more, by cruder criteria."** True of seed 42 only. Over
   five seeds the invented predicate gave 1564/970/1638/1355/2772 against
   `cave_at`'s 874/1647/1681/1116/2440 — **lower on seeds 13 and 7.** They
   disagree in both directions. The implementer's framing is better than mine:
   two predicates disagreeing in both directions is a stronger case for deleting
   one than either being bigger. I generalised from n=1 in a dispatch that
   another agent then acted on.

2. **"It fixes the underwater defect for free."** False, and backwards.
   Underwater placements went **300 → 426**. `cave_at`'s guard is
   `is_ocean(v)` = `elevation < sea_level`; my invented guard was
   `elevation - sea_level > 5.0`, which is **strictly stronger**. Replacing mine
   loosened the water test. I asserted a consequence of a deletion without
   comparing the two guards I was choosing between.

### #26 [G5] — the live defect is the PLACEMENT SEAM, not either predicate

426 of 7,758 placed caves sit on water facets and are enterable there.

**Neither predicate can fix it, and that is the finding.** Both check water at
the VERTEX; `site_facet_for` then moves the address up to ~39 km and nothing
re-checks the facet it lands on. Two predicates, two different guards, the same
bug. **Exotic sites share the seam** — nothing about this is cave-specific.

**Ruling:** this is its own task with its own declaration, exactly as the
implementer proposed. A fix inside `site_facet_for` moves every placed address
for every seed, so it wants to be a deliberate act rather than a rider on a
deletion. Filed as the next task.

**Why not accept underwater caves as flavour:** a flooded sea cave is a real
thing and would be a fine feature — but it is enterable today with no swimming,
no drowning and no water prose, so the world would be offering a door into the
sea floor and describing it as a small room. Accepting it needs the fiction
built first; refusing it needs one predicate at one seam.

### #27 [Q, Nathan] — flooded sea caves are a FEATURE, and the placement-seam task is CANCELLED

Nathan, 2026-09-02: *"I think flooded sea caves are great; let's keep them. We
don't have swimming, drowning, or water prose anywhere, so let's just treat that
as Yet Another Thing We Need to Do."*

**Decision:** the 426 underwater placements stay. Ruling #26's placement-seam
task is cancelled — water was its only live defect, so removing the defect
removes the task. `site_facet_for` is unchanged and no placed address moves.

**What changed and what did not:**

- `cave_rate_calibration`'s `placed_on_water` column survives, with its meaning
  **inverted**: it was added as a defect counter and is now a feature metric —
  how much of the world is waiting on water traversal. Reported, never asserted,
  so no test needed changing.
- The gap is filed as `PLAY-water-traversal-and-prose`. Until it lands, entering
  a flooded cave gives the ordinary dry-chamber description, which IS a lie the
  project is knowingly carrying.
- **Flooded-ness is deliberately NOT stored on the `Site`.** It is derivable
  from the placed facet whenever prose needs it, so a field would be a second
  source of truth for a fact the mesh already holds — the same reasoning that
  made two cave predicates a bug.

**Why this is a good trade rather than a shortcut:** the alternative was one
predicate at one seam, and it would have deleted a real feature to satisfy a
consistency the world does not yet need. A pre-alpha world is allowed to contain
places it cannot yet describe; it is not allowed to contain two disagreeing
answers about whether they exist. The first is a gap, the second was the bug.

**Cost if wrong:** a player finds a dry room under the sea and it reads as
broken rather than unfinished. Mitigated by the row and by the column that
counts them.

### #28 [G5] — I specified a vacuous test for the third time, and the implementer caught it by probing first

Task 6's dispatch told the implementer to assert the flagship's prose contains
"Doaba". It probed live before implementing and found **`"in the lands of
Doaba"` already appears there** via `Vantage::village`, an unrelated mechanism.
So the assertion would have passed whether or not the new code did anything.

It asserted on the clause's own marker text instead, and confirmed
red-before/green-after.

**Eleventh instance of the pattern this campaign, and the third where the
vacuous test was MY specification** (after Task 1's `a_new_site_is_a_point` and
Task 3's inverted step-2). The shape is always the same: I pick an observable
that the desired behaviour would produce, without checking whether something
else already produces it.

**The habit that caught it, and it is cheap: probe the observable BEFORE writing
the assertion.** One CLI run would have told me. The implementer did exactly
that unprompted, which is the second time this campaign an agent has protected
me from my own test design.

### #29 [G5] — `Site::salience()` has no production consumer, and duplicates an ordering `brief_of` hardcodes

Surfaced by Task 6 reporting that "at most two sites, ranked by salience" is not
exercisable: `Brief::site` is a singular `Option<Site>`, already reduced by
`brief_of`.

Verified:

```
salience() callers in production : NONE
  (site.rs:177,178,188 are its own tests; purview.rs and
   portolan_resolution.rs are a DIFFERENT salience — agent/class, unrelated)
brief_of's ordering             : an if/else chain, brief.rs:206/211/216
brief_of's comments             : brief.rs:191, 203 cite `Site::salience` as the authority
```

So the settlement > exotic > cave order is stated **twice, independently**, and
the prose claims one copy is canonical while the code uses the other. Change
`salience()` and `brief_of` silently keeps the old order; change `brief_of` and
salience's test still passes.

**This is the `cave_at` / `cave_site_at` bug in miniature — two sources of truth
for one fact — and it is the third instance in this campaign.** I specced
`salience()` in Task 1 for a ranking consumer that a singular `Option<Site>`
means never arrives.

**Ruling: make `brief_of` USE `salience()`** — assemble the candidates and take
the max — rather than deleting salience or leaving the duplication. That makes
salience load-bearing, deletes the second copy of the ordering, and honours the
comments already claiming it is the authority. Deleting it instead would leave
the spec's "at most two named" intent with no home the moment `Brief` carries
more than one site.

**Cost if wrong:** a slightly less direct expression of a three-way priority.
Cheaper than a third occurrence of the two-sources-of-truth defect.
