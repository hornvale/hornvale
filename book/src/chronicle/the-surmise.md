# The Surmise

The Foresight gave a routine a planner, but the planner still cheated: `decide`
received the resource room as a ground-truth argument, so an NPC that had
never set foot near water beelined to it anyway, A\* searching a destination
it had no way to know. The reserved `Perceived { position, drive }` view — its
own doc comment already promised this — existed for exactly this moment. This
campaign fills it. An agent now knows only what it has **perceived**; its
model of the world is a **belief**, derived, fallible, and possibly wrong. The
planner plans over belief, not truth.

A surmise is a belief held on incomplete evidence — one that may turn out
false. That is the whole payoff of this slice, named plainly.

## Belief is a cache, not a second ledger

The governing move (decision-ledger #5, an ideonomy abstraction-lift) is to
treat belief as a **lazily-populated cache over the one true ledger**, never
a stored predicate of its own. An agent perceives water at a room by having
stood there — some committed `agent-at(agent, R, d ≤ t)` where `R` is water.
`believed_water(ledger, npc, t)` folds that agent's own `agent-at` history,
keeping the nearest-to-home of every water room it has actually visited, or
`None` if it has never stood in one. Nothing new is committed to make this
work: belief re-derives from a predicate (`agent-at`) that already existed,
exactly as `drive` already folded `drank` events (The Wanting) and `position`
already folded `agent-at` itself (The Quickening). The derived-view
architecture's matrix verdict — a false belief is always *derived*, with
real provenance, never a second ledger — holds here at zero save-format
cost: no new committed predicate, no
epoch, genesis stays byte-identical. The genesis-zero pins that have guarded
every liveness slice since The Quickening (no `agent-at`, no `drank` at
genesis) still hold unmoved.

Water itself became a **region** rather than a single point midway through
design (a Nathan G3 direction): many low-lying sources scattered across the
terrain, not one hand-placed spring. That single change sharpens the whole
campaign's keystone from a binary (`knows` / `doesn't know`) into a genuine
**nearest-known-of-several** — an agent may know a *far* source while a
*nearer* one sits entirely unperceived, and two agents with different
histories can believe different nearest sources and beeline to different
water.

## Ignorance is the demonstrated divergence

With truth held static (water does not move, dry up, or refill this slice),
a populated belief line is never wrong — the only way belief can lag truth is
**ignorance**: a cache miss. The measure-don't-narrate discipline (the
project's own scar tissue: "four tests asserting nothing shipped GREEN")
demanded the tests **force** a genuine divergence rather than merely narrate
one, so the keystones are adversarial by construction:

- **Pure decision divergence.** Two identical `Perceived` views differing
  only in `believed_water` (`Some(w)` vs `None`) drive `decide` to different
  first moves — the believer A\*-steps toward `w`; the ignorant one takes a
  downhill explore step. A `decide` that ignores belief fails this
  immediately.
- **Multi-source destination divergence — the keystone proper.** Two NPCs,
  identical home, thirst, and world, differing *only* in a pre-seeded
  perceived history: one has stood at a near source, the other at a far one,
  and neither has found the truly nearest. Each beelines to its *own* believed
  source. Ignore belief and both collapse onto the same true-nearest — the
  test is built to fail exactly that way.
- **Graded discovery.** A fresh, ignorant NPC explores downhill (the "water
  lies low" prior — reused, not invented, since it is `resource_room`'s own
  old elevation rule), reaches some water, drinks; a later thirst cycle
  beelines straight to it. The discovery trip and the beeline trip are
  observably different journeys from the identical starting ignorance.

`BELIEF ≡ FOLD-OVER-PERCEIVED` joins the project's c-series lineage of
X-≡-reference results (INDEX ≡ SCAN, JOIN ≡ SCAN, SCHEDULE ≡ HAND-ORDER,
DRIVE ≡ FOLD, A\* PATH DETERMINISM): a pure deterministic function of
perceived history that visibly changes behavior.

## The seam did its job

`decide(view, home, params, budget) -> Intent` needed no new parameter and no
new shape to receive belief — `Perceived` grew two fields (`believed_water`,
`explore_step`) and the ground-truth `water` argument dissolved into the
first of them. This is the body-swap The Wanting's decision #9 and The
Foresight's decision #10 explicitly reserved: nothing about the tick's
dependence on `Intent`, or the planner's A\* machinery, needed to change for
the view it reads to stop being ground truth. The reserved seam is the
entire reason this campaign touches only `windows/vessel/` — kernel and
domains are untouched (The Walk §11).

## The re-wire: fresh water, not the sea

T4 measured a real problem before this campaign could close: on the actual
seed-42 world, "water" meant *below sea level* — the salt ocean — and the
ocean sat thousands of BFS hops from any inland settlement. Worse than
merely unreachable, it was the wrong referent: nothing drinks the sea. Rather
than ship a campaign that quietly modeled agents surviving on seawater, the
campaign **parked** (decision-ledger #9) to wait for a real salt/fresh
distinction. [The Freshet](./the-freshet.md) supplied exactly that seam:
`WaterKind::is_fresh()`, true only for rivers, exposed through
`LocaleFields.water` at zero save-format cost of its own.

T5 re-points the resource at it. The `Terrain` trait's `water_level()` method
and the sea-level derivation it fed (`world_sea_level`, the fragile
cross-window string coupling on `"sea-level-m"`, and the tripwire test that
existed only to catch that coupling drifting) are gone entirely — vestigial
the moment the referent changed. In their place, one new trait method:
`is_fresh_water(room) -> bool`, backed in a live session by
`LocaleFields.water.is_fresh()` and, in tests, by a planted set of fresh
rooms. Elevation survives unchanged as the *exploration prior* — "water lies
low" still steers the downhill search, because rivers genuinely are the
downhill drainage channels The Freshet's own classification follows — but it
no longer *defines* water. Two different questions, two different fields,
cleanly separated.

## Measured, not assumed: the demo does not yet fire

The campaign's discipline — measure the mechanism, don't narrate around a
gap — applied one more time at the close, and it is worth stating plainly
rather than papering over: on the real seed-42 world, the possessed agent's
own home settlement's greedy-downhill exploration still does **not** reach
fresh water. Rivers are far closer than the old, unreachable sea (a BFS
search finds one roughly 93,000 expansions out, three orders of magnitude
nearer than the ocean's "not within 500,000"), but that is still two orders
of magnitude past the budget any real call site uses. More to the point, the
actual walking mechanism — a greedy walk that steps to the lowest-elevation
neighbor it has not yet visited *this call*, never backtracking — explored
2,592 distinct rooms over an artificially enormous wait and still walled
itself into a riverless drainage basin before finding one. Checked against
all three of a real session's derived NPCs (the home settlement plus its two
next-most-populous neighbors): none of the three currently drink.

The gap is not universal — it is **settlement-placement-dependent**. A
sweep of ten of this seed's settlements found the same mechanism working
fine elsewhere: one starts already beside a river, another reaches one in
448 moves. The reason the home settlement's own walk fails is structural,
not a defect this task introduces: a walker that never revisits a room
within one call is not a global search. It can wall itself into a basin
whose every boundary room's neighbors are already visited, even though
unvisited, lower, river-bearing ground exists elsewhere in the same
connected mesh — the walker simply has no way to see it from where it
stands. This is exactly the risk the campaign's own decision #6 named and
knowingly accepted when it chose a greedy local prior over a frontier search
("more machinery than a greedy prior... a followup"). Widening the search
budget does not help; the walk is topologically stuck, not budget-limited.
Smarter, info-gain-directed exploration is the followup that closes it,
banked in the decision ledger rather than rushed into this close.

The mechanism-level keystones above are untouched by this finding — they
prove belief is correct on terrain built to exercise it. What the close
measured is that the real world's terrain and the exploration *policy*
interact in a way this campaign's chosen policy does not yet handle
everywhere. Pinning that measurement as a real, checkable test — rather than
quietly deleting the old parked assertion — means a future exploration
change that fixes it (or worsens it) is caught, not silently assumed.

## Discovery, narrated honestly

The provenance strings a moved NPC's `why` surfaces now say what actually
happened, not a euphemism for it: a believer's step reads *"went down to the
river it knew (thirst)"*, an ignorant one *"wandered, having found no water
yet (thirst)"*, and drinking itself *"drank from the river (thirst sated)"*.
The gallery's over-time possession transcript is re-baselined against this
honest reality — its NPC, on the real seed-42 world, wanders rather than
returning, and the transcript's framing prose says so plainly rather than
promising a round trip the mechanism cannot currently deliver for that
settlement.

## What's reserved

Ignorance is the only divergence this slice models, because truth is static.
Two harder polarities wait on the same seam:

- **Staleness** — the false-positive twin of ignorance. Give water
  truth-volatility (a source runs dry after drinking, or moves seasonally)
  and a belief that was once true can lag a world that changed; the agent
  walks to remembered water and finds it gone.
- **The credulity threshold** — deception, the immune model's own L2 tier.
  This slice has exactly one truth-to-belief channel: the agent's own
  perception.
  A second agent's claim — a lie, an illusion, a forgery — is a second,
  untrusted channel weighted by a signal-detection threshold (credulous vs.
  paranoid, one mechanism at two settings). Reserved as the next campaign,
  **The Discernment** — discrimination is, literally, the mechanic.

Belief for the *possessed* player (a `know`/`recall` verb surfacing the
player's own cache), a richer believed-map re-anchored to the moving agent
rather than home, probabilistic perception, and smarter exploration all wait
their turn, banked in the followup register rather than smuggled into this
close.
