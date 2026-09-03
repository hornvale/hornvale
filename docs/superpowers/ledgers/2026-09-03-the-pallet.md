# The Pallet — decision ledger

Campaign B of the `MAP-one-kind-model` arc: **a body chooses where to sleep**,
with The Plumb's Group A (the local-day family) folded in per Nathan's triage.

---

#1 [Q] — **Campaign name: The Pallet.** A pallet is the humblest bed — straw on
a floor — which is exactly the span this campaign models: a body that would
prefer a bed, will take bracken, and can pass out in the road. `the-pallet` has
no prior reference in `docs/retrospectives/` or `book/src/chronicle/`;
`the-settle` was the first choice and is unusable (62 substring collisions with
"settlement"). No ideonomy pass — a name is not a design decision.

#2 [Q] — **What may be recorded is settled by decision 0069, and the answer is
the KIND, not the place.** 0069 makes an entity's persisted position its
**room**; anything finer "exists only inside the presence bubble and is never
serialized". `windows/vessel/src/interior/anchor.rs` says the same in its own
words: "an anchor has no coordinate, and its identity within a room is
positional, not persisted."

So "the body slept at anchor #3" is unrecordable **by constitutional design**,
and no campaign should try. But The Wicket already made the durable half
first-class: `Anchor { kind: KindId, within }`. **A `KindId` is a registered
concept and a stable string.** So `slept-on = bed` is fully 0069-legal — it
records *what* a body slept on, never *where*.

Mechanism, checked against `kernel/src/ledger.rs` rather than assumed:
`Value::Text(String)` exists, and `Fact.place: Option<EntityId>` is the
room-granular location 0069 permits. A **new** predicate `SLEPT_ON` carrying
`Value::Text(kind)` with `place: Some(room)` is additive — it does not touch
`SLEPT`'s existing object (the span), so no save-format contract moves.

Precedent: predicates are single-valued here, so a second attribute takes a
second predicate rather than a compound object.

#3 [G1] — **OVERTURNED BY THE IDEONOMY PASS: the choice is THREE RUNGS, not one
preference function, and the body must be allowed to choose badly.**

Tuple drawn: operators `dimension-identification` + `combination`, organon
`lattice`, dimension-prompts `discovery-vs-invention`, `side-effect`,
`autonomy`. One pass, **one overturn** — the pass changed the recommendation
materially rather than enriching it, which is the rarer outcome.

**What I was going to propose:** a single preference function over available
kinds, plus a quality grade feeding the recovery rate.

**What the pass produced.** Combining the sleep decision with The Plumb's own
rung ladder shows the quantity is not one thing but a lattice, and Nathan's
own words already name all three rungs:

| rung | the quantity | Nathan's phrasing |
|---|---|---|
| `per-species` | the GRADE of a kind — how well this body recovers on it | a xorn does not want a bed |
| `per-people` | the PREFERENCE ordering among available kinds | "whatever their people tends to use" |
| `per-individual` | the idiosyncrasy | "some people just really like sleeping bags" |

This makes The Pallet **the first campaign to use the ladder as a design
instrument rather than an audit** — and it sets the scope honestly: the
per-people rung needs kind-to-kind edges (Campaign C) and the per-individual
rung needs `Lineage`-derived values (Campaign D), **neither of which exists.**
So The Pallet ships the `per-species` rung and leaves the other two as
*declared, tagged seams* — `plumb: per-people(...)` / `per-individual(...)`
findings in the roster, not TODO comments — which is the mechanism decision
0586 exists to provide.

**The `autonomy` prompt produced the constraint I would otherwise have violated.**
Nathan: *"a creature sleeping in an unsafe or unrestful place would be a useful
indicator that something needs tuning."* A design that clamps the body to the
best available site **destroys that diagnostic**. The choice must therefore be
sane-by-default and **capable of being wrong**, and a bad choice must be
visible in the ledger rather than prevented in the chooser. This inverts the
obvious implementation (argmax over grade) into something weaker on purpose.

**The `side-effect` prompt found the argument for recording a kind rather than
a grade.** `windows/historiography` is domain-agnostic by construction — it
replays any entity's facts against the registry's predicate docs. So committing
`slept-on: bed` yields narrative **for free**, on a surface nobody has to
write; committing only a number would not. `windows/lab/src/health.rs` already
reads `SLEPT`, so the distress read gains the same for free.

**The `discovery-vs-invention` prompt sharpened one distinction worth keeping:**
a body *knows* innately that a bed out-rests a floor (species-level instinct),
but *cannot know* whether a particular site is safe without being there. So
**preference is innate; safety is situational** — which argues against folding
threat into the grade, and for leaving it as a separate term a later campaign
supplies.

**Discarded, with reasons:** (a) recording a bare quality *number* — loses the
free historiography and the tuning diagnostic, which both need the kind;
(b) making sleep *gated* on finding a site — Nathan ruled this out explicitly
("we don't want things to just wander out of the room to find the nearest
bed"); (c) an anchor-identity record — unconstitutional under 0069.

Capture: Campaign C and D dependencies are recorded here rather than as new
registry rows, since `MAP-one-kind-model` already carries both additions.

#4 [G2] — **The Wicket built more of this than the brief assumed, and checking
before designing halved the campaign.** Grepped the sleep path before writing a
line of spec, on the standing lesson from The Plumb's instance 28 (a task whose
deliverable already existed and had for thirteen days). Found already shipped:
`OfferedVerb::Sleep` gated on `ObjectProperty::SupportsRest`
(`affordance.rs:403,528`), `SiteGrade { Bare, Afforded }` with `gain()`
(`liveness.rs:3139`), `AFFORDED_REST_GAIN = 1.5` multiplying `BoutKind::fall`
with a compile-time `> 1.0` bracket, and the `SLEPT`/`RESTED` predicates.

So The Pallet does **not** build a grade. Three things are genuinely missing —
the CHOICE (today `SiteGrade` is a room-level boolean at `liveness.rs:3247`, so
no body ever picks an anchor and "where did it sleep" has no answer even in
memory), the RECORD, and the per-species RUNG — plus Group A. Five tasks, not
nine. No ideonomy pass: this is a fact about the tree, not a design choice.

#5 [G2] — **`AFFORDED_REST_GAIN`'s existing `plumb` tag is wrong, and wrong in
the shape decision 0586 was written to name.** It reads
`universal(a uniform multiplier on every rest/sleep act's own rate, bounded
rather than derived — not a species property)`. "Bounded rather than derived"
is a **provenance** claim (where the number came from) and "not a species
property" is a **negation** — the two tells 0586 records for a reason that
answers a neighbouring question. A xorn gains nothing from a bed; the quantity
is `per-species`, and this campaign flips it.

Worth stating because the tag was authored by The Wicket *before* The Plumb
existed to name the failure, and it is the first case of the ladder catching a
verdict rather than an untagged constant. No ideonomy pass — an error identified
against a ratified decision's own stated criterion.

#6 [G2] — **Group A's success condition is DELETING a test, and that needs
saying out loud.** The Plumb shipped
`a_rest_still_outlasts_the_sleep_scans_give_up_fallback_at_the_100_hour_legal_extreme`
as a RUNNING test asserting the current wrong ordering — an inverse assertion
chosen over an `#[ignore]`d one precisely so that fixing the defect would go
red. Converting `SCAN_LIMIT` and `ONE_DAY` will redden it, and the correct
response is to delete it, which its own doc instructs. A future implementer
meeting an unexplained red is the exact reader this needs to reach, so it is in
the spec (§4e) rather than only here. No ideonomy pass — a consequence read off
a committed test's own doc.

#7 [G4] — **Plan self-review and the SDD pre-flight scan, with the table the
scan is supposed to produce rather than a verdict.**

**Task-pair rows** (every pair sharing a file or an interface):

| pair | shared | what one produces vs what the other consumes | finding |
|---|---|---|---|
| T1 × T3 | `liveness.rs` | T1 edits `next_awake_day`'s bounds and two file-level spans; T3 adds a predicate beside `SLEPT` | disjoint regions — clean |
| T1 × T4 | `liveness.rs` | T1 the sleep-span constants; T4 `AFFORDED_REST_GAIN` and `SiteGrade::gain` | disjoint — clean |
| T3 × T4 | `liveness.rs` | T3 a fact builder; T4 a gain lookup | disjoint — clean |
| T2 × T3 | `select_sleep_site` | T2 produces it, T3 consumes its `AnchorId` to read `Anchor::kind` | real dependency; ordering T2→T3 is correct |
| T2 × T4 | — | T4 needs only afforded-vs-bare, which `SiteGrade` already derives room-level | **no dependency** — T4 could run before T2; ordering is by risk, not need |

**Per-task self-consistency rows:** T1's tests-vs-code agree (it deletes one
assertion and adds another over the same four constants); T2's three test cases
cover the function's whole return domain; T3's absence-test matches its own
"bare ground commits nothing" rule; T4's ratchet matches the table it guards;
T5 is procedural.

**Two gaps found, both fixed inline rather than left for an implementer:**

1. **The table's KEY was never stated, and both readings are plausible.**
   `sleep_grade_registry() -> ComponentStore<KindId, f64>` — keyed by the
   sleeper's species, or by the site's kind? The spec says "how well this body
   recovers on it", which reads either way. **It is the sleeper's species.**
   The site stays binary (afforded / bare) exactly as `SiteGrade` already is,
   which is what keeps this a one-dimensional table. The `(species, thing)`
   matrix — *is a bed better than bracken for a dwarf* — is the `per-people`
   rung and needs Campaign C's kind-to-kind edges. An implementer who read it
   the other way would have built Campaign C early and badly, and it would have
   looked like progress. This is the defect class this project keeps recording:
   a spec sentence that two readings satisfy.

2. **Spec §4d requires the unbuilt rungs be DECLARED as tagged seams, and no
   step did it.** Added as Task 4 Step 5, with an escape hatch: if there is no
   constant to hang a tag on, say so rather than inventing one to carry it.

No ideonomy pass on either — the first is a disambiguation against a
constraint that already exists (Campaign C's scope), the second a coverage
miss against the spec's own text.

#8 [G5] — **Task 1 review: my headline was right and the reviewer found the
better finding underneath it. A REPLACEMENT TEST CAN COVER A DIFFERENT THING
THAN THE TEST IT REPLACED, and everything stays green.**

My finding — that converting a `const` into a `let` removes the quantity from
`plumb`'s reach — was confirmed and proved rather than argued.
`tools/plumb/src/walk.rs:461-511` overrides only `visit_item_const` /
`visit_impl_item_const` / `visit_trait_item_const`; there is no `visit_local`
and no `visit_expr_lit`. Two probes settle it: an untagged const in
`next_awake_day`'s body is caught (`687 swept, 1 undeclared`, rc=1); the
identical number as a `let` vanishes (`686 swept, 0 undeclared`, rc=0). So
`20`, `3 / 2` and `2 / 5` — the numbers that now govern rest and sleep spans —
are unreachable by the audit, while `plumb report`'s own header claims *"Every
authored constant should declare which axis it varies along."*

**But the sharper finding is I1, and I did not see it.** The reviewer mutated
each of the four conversions separately:

| mutation | vessel | lab |
|---|---|---|
| `sleep_bout` → `SLEEP_BOUT` | **1 failed** | — |
| `wake_scan_step` → `WAKE_SCAN_STEP` | 1009 passed | 1 failed (byte-golden only) |
| `scan_limit` → `SCAN_LIMIT` | 1009 passed | 503 passed |
| `one_day` → `ONE_DAY` | 1009 passed | 503 passed |

**The deleted test used `permanent_night: true`, forcing the give-up-fallback
branch — `ONE_DAY`, the constant the inversion was actually measured on. The
replacement uses `permanent_night: false` plus awake-at-noon, forcing the
`SLEEP_BOUT` floor branch instead.** Both are good tests. They witness
different things, and the one that went away is not the one that came back. The
arm The Plumb deliberately ratcheted is now unpinned, and the whole suite is
green.

**The generalizable form, which is new to this project's record:** deleting a
test and adding a test in the same task reads as *replacement*, and nothing
checks that the new one exercises the same code path. A test's identity is its
**branch**, not its subject or its name — both tests here are "about" the
rest/sleep ordering at the 100-hour extreme, and they take different arms of
the same `match`. The check is not "did we add a test" but "mutate what the old
test pinned and confirm something still reddens."

Two contributing causes worth keeping: almost every fixture uses
`PlantedTerrain::thermal`, which reports no day, so `ticks_per_local_day`
returns the base rate and const ≡ runtime formula — which is why three of four
mutations are invisible; and a byte-golden catching `WAKE_SCAN_STEP` is a
**change detector, not a property witness**, so it should not be counted as
coverage.

Also found, all routed to fix round 1: `SCAN_LIMIT` and `ONE_DAY` are read by
nothing, making them constants whose sole function is to hold a tag (the
`REST_BOUT` idiom is real precedent, but its three siblings are all still read
by tests — delete one and something stops compiling); a latent non-termination
(`local_day / 20` is `0` for `day_ticks()` in `1..=19`, which the old fixed
step made structurally impossible and the conversion makes only situationally
impossible); and three stale doc claims of the class the implementer had
already fixed once elsewhere.

Ruling: keep the idiom, fix the coverage, file the blind spot as a `TOOL-*`
row rather than fixing the tool — auditing every integer literal in a function
body has an obvious false-positive problem and is its own campaign.

#9 [G5] — **Task 1 fix round 1: closed.** All five items from #8 addressed.

I1: added `a_rest_is_shorter_than_a_sleep_through_the_give_up_fallback_at_the_
100_hour_legal_extreme` (same fixture as the deleted falsifier —
`permanent_night: true` — but asserting the CORRECT ordering, not its
inverse) and `a_fast_rotating_world_gives_up_a_search_the_retired_bound_
would_have_finished` for `SCAN_LIMIT` (a small-`L` world where the converted
search bound gives up on a wake the retired anchor's own window would have
found — the property named in #8: the bound only matters where the two
disagree about whether a wake exists at all). Re-mutated all four,
vessel-only (the discriminating suite for three of the four rows):

| mutation | vessel |
|---|---|
| `sleep_bout` → `SLEEP_BOUT` | 1010 passed, **1 failed** (unchanged test) |
| `wake_scan_step` → `WAKE_SCAN_STEP` | 1011 passed (still lab-only; unchanged, not asked for) |
| `scan_limit` → `SCAN_LIMIT` | 1010 passed, **1 failed** (new) |
| `one_day` → `ONE_DAY` | 1009 passed, **2 failed** (new give-up test, and the new `SCAN_LIMIT` test cross-reacts) |

`SCAN_LIMIT` ended up WITNESSED, not declared unwitnessable — the small-`L`
property in #8 was constructible and the mutation table above proves it.

F2: `SCAN_LIMIT`/`ONE_DAY` promoted from `next_awake_day`'s body to file
level, alongside `REST_BOUT`/`SLEEP_BOUT`/`WAKE_SCAN_STEP` (same idiom, same
`#[allow(dead_code)]` shape) — both are now read by the two new tests, so
neither is decoration any more, and neither needed deleting.

F3: filed `TOOL-plumb-walk-blind-to-let-bindings` (`book/src/frontier/
idea-registry.md`), citing `tools/plumb/src/walk.rs:461-511` and this entry
and #8 by ledger reference. Tool itself untouched, per instruction.

F4: `wake_scan_step` floored at `.max(1)` with the invariant stated inline —
`local_day / 20` is `0` for `day_ticks()` in `1..=19`, unreachable through
today's genesis bound but no longer resting on an invariant declared in
`domains/astronomy`.

F5: three doc fixes — the `Action::Sleep` bullet in `act_span`'s own doc now
names `SLEEP_BOUT` as the `L = 1` anchor (parallel to the `Action::Rest`
bullet above it); the two present-tense "`WAKE_SCAN_STEP`, 72 minutes"
mentions (`REST_BOUT`'s and `SLEEP_BOUT`'s docs) now say it is the `L = 1`
value; the awake-at-noon test's own doc now gives the fixture's real
wake-scan rate (`local_day / 20 = 20,833` ticks) instead of citing the
constant (`5,000`) as if it still governed there.

`cargo fmt`, `make gate-commit` (rc=0), `docs_consistency` (28/28, including
the registry-row shape checks), `make rebaseline` (only `plumb-roster.md`
moved — line numbers, no behavior change), byte-goldens by name
(`HV_TEST_OK=1 -p hornvale-lab -p hornvale-vessel -p hornvale`: 1964/1964).

#10 [G5] — **Task 1 fix round 2: closed. `WAKE_SCAN_STEP` witnessed.**

The step is the scan's own RESOLUTION, so it can only matter where the
retired 5,000-tick grid and the converted, finer grid at a small `L`
bracket an actual wake transition. A diurnal wake window is half the day —
wider than either grid's spacing, so nothing brackets it. A CREPUSCULAR
window is narrow (`TWILIGHT_DEG` = 6°, roughly 2,124 ticks wide around dawn
and dusk) — narrower than the retired step, exactly the historical concern
`WAKE_SCAN_STEP`'s own doc already recorded.

Added `a_crepuscular_wake_band_is_bracketed_by_the_retired_steps_grid`: at
`L = 4` standard hours starting tick 8,855 (off-phase), the retired
5,000-tick grid samples four times inside the SAME converted bound
(~25,000 ticks) and lands in the dawn band on none of them; the converted
grid finds it. Re-mutated all four conversions, vessel-only, to check for
cross-reaction the way `SCAN_LIMIT`'s test cross-reacted with `ONE_DAY`'s
in fix round 1:

| mutation | vessel |
|---|---|
| `sleep_bout` → `SLEEP_BOUT` | 1011 passed, **1 failed** (unchanged) |
| `wake_scan_step` → `WAKE_SCAN_STEP` | 1011 passed, **1 failed** (new, clean) |
| `scan_limit` → `SCAN_LIMIT` | 1011 passed, **1 failed** (unchanged, clean) |
| `one_day` → `ONE_DAY` | 1010 passed, **2 failed** (unchanged pair from fix round 1) |

The new test does not cross-react with any of the other three — it never
calls `act_span` or reads `SLEEP_BOUT`, and its own independent bound
(`local_day_ticks * 3 / 2`, computed in the test) is unaffected by a
`scan_limit`/`one_day` mutation inside `next_awake_day` because it never
reaches the give-up branch on real code.

Also fixed in passing: seven assertion strings from fix round 1's two new
tests had literal multi-space runs where a single space belonged — a
non-raw Python heredoc ate the Rust `\`-continuation (the exact trap this
project's own memory already names), collapsing three-line messages onto
one line with the continuation lines' leading indentation surviving as
extra spaces. Cosmetic only (compiled, ran, passed throughout), fixed by
collapsing to single spaces and re-running `cargo fmt`; this round's own
edits used escaped backslashes in the Python source to avoid repeating it.

`cargo fmt`, `make gate-commit` (rc=0), no tag or const moved so the roster
was not regenerated (confirmed by diffing a fresh `plumb report` against
the committed one). Byte-goldens by name (`-p hornvale-lab -p
hornvale-vessel -p hornvale`): 1965/1965.

All four sleep-side conversions now have a dedicated vessel-level property
witness; the byte-golden remains as a secondary change detector, not the
sole coverage for any of them.
