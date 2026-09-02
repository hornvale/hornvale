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
