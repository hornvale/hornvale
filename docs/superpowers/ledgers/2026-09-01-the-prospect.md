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
