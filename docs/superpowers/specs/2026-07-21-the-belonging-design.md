# The Belonging — social affiliation, the sixth drive

**Status:** shipped (merged 2026-07-21) — see the
[chronicle](../../../book/src/chronicle/the-belonging.md) and
[retrospective](../../retrospectives/the-belonging.md). One model refinement
during build: loneliness is *reachability-gated* (dormant when home is beyond
homing range, never chronic-social-distress) — see the retro.
**Date:** 2026-07-21
**Campaign:** The Belonging (Temperament followup #3, sixth & last drive: social)
**Precedes:** implementation plan → execution → merge

## Problem

Creatures thirst, shiver, tire, sleep, hunger, and fear — but they are alone.
Nothing pulls a creature toward its own kind; a foraging goblin wanders as far
from its village as any need drives it and feels nothing for the distance. A
world observed through text needs its creatures to *belong*: to feel the pull of
home and company, to grow uneasy in isolation, to drift back toward their people
when their bellies and throats are satisfied. This is the last of the six
Temperament drives, and the one that first turns a creature's attention outward,
from the self and the ground to the *others*.

## The core result (from ideonomy)

Two findings shape the design.

- **Social is the first drive whose field is *other agents*.** The five drives
  before it read the self (thirst, hunger, fatigue — internal stocks) or the
  environment (thermal, danger — fields over terrain). Affiliation reads a third
  thing: the presence of one's own kind. It is the root of a whole subtree —
  attachment to *specific* others, status, conformity, mating — of which v1 takes
  only the root: **affiliation**, the pull toward the group.
- **Sociality has an *optimum*, so social is shaped like thermal.** Company is
  not monotone "more is better": too isolated is lonely, too crowded is stress.
  That is exactly thermal comfort's shape — deviation from a setpoint — with the
  field being *how much of my kind is near* rather than temperature. So social
  reuses the flow-drive template: a deviation urgency and a gradient step toward
  better company. Thermal seeks a comfortable *cell*; social seeks a comfortable
  *crowd*.

## Design

### 1. The company field (Tier-0: home-proximity)

A creature's *company* is the presence of its people. The honest coarse rung —
and the one that dodges a real landmine — proxies company by **proximity to
home** (a creature's home is its village, its people). Loneliness is a normalized
**angular distance from home**:

```
loneliness(pos, home) = clamp( angular_distance(pos, home) / LONELY_SCALE , 0, 1 )
```

Angular distance (the angle between the two rooms' centroid directions) is an
O(1) geometric proxy for hop-distance — urgency is read several times per
arbitration, so it must be cheap (the hot-path lesson from `threat_at`). `0` at
home, rising as the creature strays.

**Why not perceive individual agents?** Reading other agents' positions directly
is (a) a determinism hazard — mid-tick reads of others' positions are
order-dependent — and (b) a null-control hazard — NPCs homed at scattered
settlements would each read chronic loneliness and despair, breaking the health
metric. Home-proximity dodges both: **loneliness is always serviceable** (home is
always reachable, so the drive reads *Searching* — heading home — never distress),
so a natural world stays calm; only a *stranded* creature, cut off from home,
reads chronic social-Frustrated. The real **population field** (affiliate at *any*
village, not just home) and **attachment** (perceive *specific* others) are the
reserved Tier-1, exactly as The Slumber reserved the true solar terminator.

### 2. The social drive (flow, Drive #6)

Urgency is `loneliness(pos, home)`. Its affordance is to **step toward home** (the
first step of the A* plan, `plan_to_room` — the same planner thirst uses for
water, computed once), so a lonely creature beelines back to its people; `None`
only when home is unreachable within budget (stranded → Frustrated). Its ceiling
is **comfort** (below survival, like thermal and fatigue — loneliness is not
lethal), so a thirsty, hungry, or frightened creature attends to survival first
and drifts homeward only once those are met. It is **silent while asleep** (a
sleeping creature does not seek company) with no survival override. Reuses
`MoveTo` — **no new predicate, no new `Action`**.

### 3. Affect

Social reuses the circumplex with `object = DriveKind::Social`: heading to the
known home reads **Eager** (a known, reachable target — like fatigue beelining
home); arrived, no drive active, it reads **Content**; cut off from home it reads
**Frustrated** (lonely, and cannot reach its people). The narration seam colours
the prose ("drifts homeward, missing its people").

### 4. Gregariousness (reserved)

v1 is uniform-moderate: every creature is mildly gregarious (consistent with all
current species being `Hierarchic`/`Communal`), lonely-when-isolated, one-sided.
The per-species **gregariousness niche** — the setpoint sliding solitary ↔
eusocial, where the *solitary* pole flips the sign (others *repel*, crowding is
stress, like danger) — is reserved, the sociality twin of hunger's diet niche and
danger's threat niche. The metabolic gate carries over: an **Ametabolic** creature
does not affiliate (a construct is not lonely — its own drives, e.g. a swarm-node
seeking its cluster, are the reserved seam).

## Architecture

`loneliness` is a pure read over (position, home) — no stream draw, no stored
state, no epoch, no new predicate. Social is a flow drive (`Social` holding
`home`), a sixth `Drive` implementor, `DriveKind::Social`. Wired into
`affect_of`, the tick's `arbitrate` drive set, the session felt-phrase prose, and
the health metric's by-cause tally.

## Determinism

- **No stream draw, no epoch, no new predicate** — reads position + home.
- **Genesis byte-identical.** The possession galleries **may drift**: a sated
  creature away from home now homes as a *motivated* drive (felt loneliness,
  a social provenance) rather than the default `Homing`, so its provenance/affect
  prose changes — regenerated and accepted at close. Near home (loneliness below
  `act`) behaviour is unchanged.
- The single-drive `decide` path (thirst only) is unaffected (social is not in
  its set), so it stays byte-identical.

## Model card

Social is thermal comfort over a company field, with an optimum (loneliness ↔
crowding). v1 proxies company by home-proximity (Tier-0), one-sided
(lonely-below). **Deferred:** the real **population field** (affiliate at any
populated place); **attachment** (perceive and seek *specific* others — the first
true multi-agent perception, its determinism handled deliberately); the
**gregariousness niche** (solitary ↔ eusocial, the sign-flip at solitary);
**status/dominance** (the hierarchical dimension, reading the authored
`Sociality`); **out-group aversion** (a stranger as a mild threat — where social
meets danger); and **construct/hive affiliation** (the ametabolic own-drive).

## Test plan

- **Unit:** `loneliness` = 0 at home, rising with distance, clamped; the social
  drive's affordance steps toward home / `None` when stranded; comfort-tier
  ceiling (a thirsty creature attends thirst first, drifts home only when sated);
  the metabolic gate (ametabolic → not lonely); a sated creature far from home
  homes with felt loneliness (the promotion of `Homing` to a drive).
- **Harness (`lab::synthetic`):** a creature stranded from home (home past the
  plan budget) reads chronic social-Frustrated (by-cause social); a creature at
  home reads Content.
- **Calibration:** the health null-control holds (loneliness is always
  serviceable — heading home is *Searching*, not distress — so a natural world is
  not chronically lonely).
- **Artifacts:** the possession galleries regenerated.

## Deferred (captured)

To a new **PSY** row (the affiliation/belonging engine): the population field,
attachment, the gregariousness niche, status/dominance, out-group aversion, and
construct affiliation. Plus the standing `arbitrate` `Disposition`-struct tidy,
now **six** drives deep — the strongest candidate for the next consolidation
campaign.
