# Decision ledger — The Shudder (PSY-11, the visceral felt phobia)

Autopilot overlay engaged. Entries auto-resolved from precedent + ideonomy
convergence; G3 (spec review) and G6 (merge) are Nathan's.

Ideonomy: three passes over the whole decision cluster —
(1) dimension-identification + tree-finding, rendered as a *scale* of
"how visceral" (cardinality / scope / polarity prompts);
(2) organon-construction, rendered as the phobia's *cycle*
(purpose / distribution prompts);
(3) abstraction-lift over a *timeline* (animacy / direction prompts).
Pass 3 changed no answer and produced only capture, so the cluster is
converged. **0 overturns; two reframings** (see #1 and #2).

---

#1 [G1] — **Does felt dread win arbitration (flee) or merely colour affect
(unease)?** · **Decision: both — dread enters the Danger drive's `urgency`
(so it colours `Affect`) AND its `serviceability`/`affordance` (so the
creature steps off the ground).** · *Why (precedent):*
`loneliness_from_plan` (The Belonging) makes the social drive go **dormant**
when there is no actionable pull — "an unreachable home is not a distress
but a relocation." That is the module's settled discipline: *a felt state
with no affordance must not be allowed to register as distress.* Dread sits
on **now-safe** ground, which by construction has no terrain gradient to
flee down (unlike The Alarm's halo, which always lies within one hop of
genuinely frightening terrain — which is exactly why the alarm needs no
serviceability term and dread does). A dread term in `urgency` alone would
therefore produce `Hold` → `Lost` → a distress tick in `lab/health.rs` —
arming the regression alarm for a feature that is meant to be a feeling,
not a pathology. · *Alternatives discarded:* affect-only unease capped below
`DANGER_ACT` (invisible in behaviour; a fear that can never act is not a
drive term); urgency-only without serviceability (the distress trap above).
· *ideonomy 3 passes / 0 overturns* — the scale organon reframed the fork:
"merely feel" is not a *lesser* dread but a *worse-valenced* one (same
magnitude, no outlet), and the health metric is where that polarity becomes
measurable. · *capture:* none new.

#2 [G1] — **Contact dread (own cell) or anticipatory dread (cell +
neighbours, mirroring `threat_field`)?** · **Decision: contact dread — read
at the creature's OWN cell only, symmetric with The Alarm's borrowed term.**
· *Why:* the cycle organon reframed this as a **persistence** question, not
a spatial one. The Phantom's most-recent-visit rule extinguishes a phantom
the moment the creature stands on it (the visit commits an `agent-at` fact
whose verdict, re-derived with today's roster, is safe), so contact dread is
**bounded in duration**: felt on arrival, discharged by the flee step,
disproven next tick — the loop closes. Anticipatory dread is felt from
*outside* the cell, so avoidance prevents the contact that would disprove
it: the loop never closes and the dread is unbounded in duration and in
health-metric exposure. *Registry precedent:* PSY-11 already reserves "a
longer-reach GRADIENT (distance-decay repulsive field, a wide berth not just
the exact cells)" — anticipatory dread **is** that reserved row, not this
one. · *Alternatives discarded:* anticipatory dread (reserved — and it must
ship WITH time-decay, which is the only thing that can close its loop; see
capture). · *ideonomy 3/0.*

#3 [Q] — **Does a creature's own felt dread feed back into
`believed_hazard` (a self-reinforcing phobia)?** · **Decision: never. The
memory reads only EXTERNAL danger — terrain plus other creatures'
re-derived alarm.** · *Why:* the same closure argument as #2. The Phantom
ships `believed_hazard_clears_a_disproven_phantom`; a self-feeding term
makes the memory unfalsifiable and reds that test. It is also the recursion
break — `frightened_at`'s alarm replay must not read a quantity computed
from `frightened_at`. Clinically, avoidance-prevents-extinction is *already*
modelled, by the planner routing around; self-reinforcement would
double-count it. · *ideonomy 3/0.* · *capture:* generalized into the spec's
named design principle (**the fear must stay falsifiable**) — flagged at G3
as a possible decision-record.

#4 [Q] — **Magnitude or binary, and what dread scale?** · **Decision:
magnitude — a `BTreeMap<RoomAddr, f64>` of the remembered alarm at each
transient cell — summed with the live borrowed alarm into the SAME additive
slot. No new constant: the dread *is* an alarm term, so it rides
`ALARM_SCALE`.** · *Why:* one source of truth. The transient subset is by
definition {cells where `(terrain + alarm) × mettle ≥ act` while `terrain ×
mettle < act`}; feeding the same remembered alarm back through the same
`feels_frightening` reproduces exactly the verdict that created the memory —
*the memory and the feeling agree*, the invariant `liveness.rs` already
states in three doc comments. Any discount < 1 breaks the identity and
silently creates cells the planner shuns but the creature cannot feel.
Precedent: `ALARM_SCALE = 1.0`, justified identically ("the field is already
the emitter's felt-threat magnitude"). · *Alternatives discarded:* a binary
set plus an authored `DREAD_SCALE` (a tuning knob with no evidence that
breaks the agreement invariant). A fading discount is reserved, paired with
time-decay. · *ideonomy 3/0.*

#5 [Q] — **Where does the dread map live — a new `Perceived` field or a
`Danger` drive field?** · **Decision: a `Danger` field, `dread:
Option<&BTreeMap<RoomAddr, f64>>`, mirroring `alarm` exactly; `None` ⇒
byte-identical.** · *Why:* precedent — The Alarm added its per-tick field
this way. `Perceived` has **42** literal constructions in `liveness.rs`
(verified: `grep -c "Perceived {" windows/vessel/src/liveness.rs` → 42), so
a new view field is broad churn for no gain. The provenance line is honest
too: `Perceived` carries the belief the *planner* reads
(`believed_hazard`); the drive carries the fields it *senses*, and dread is
sensed-as-if-present. · *ideonomy 3/0.*

#6 [Q] — **Does a dread-afraid creature EMIT an alarm (superstition
contagion)?** · **Decision: no — and impossible by construction, not by a
guard.** · *Why:* `alarm_field_memo` builds emission from `emitter_arousal`
→ `affect_of(band = &[])`; an empty band yields `believed_hazard(roster =
&[])`, whose emitter scan is empty, whose transient subset is therefore
empty — so the **emission read is dread-free automatically**. The same
empty-band fact that breaks The Phantom's recursion also blocks contagion,
and the cheap terrain gate in `alarm_field_memo` stays an *exact* necessary
condition for that read (its doc comment is sharpened to say so, so a future
edit cannot open contagion silently). Contagious superstition stays reserved
(PSY-11: "SUPERSTITION → collective TABOO"). · *ideonomy 3/0.*

#7 [Q] — **Is the felt dread observable?** · **Decision: yes —
`affect_of` (the narration + health-metric read) is dread-aware; only the
bandless emission read is not.** · *Why:* the purpose prompt in pass 2: a
fear that never reaches `Affect` is not "the felt half", it is a second
behavioural term. PSY-11 already carries one instance of this wart ("v1's
mover-only signal leaves a fleeing creature narrated 'calm but moving'");
this campaign must not add a second. Safe because the emission read is
bandless (#6). · *ideonomy 3/0.*

#8 [Q] — **How is the transient subset isolated?** · **Decision: the
existing fold returns both sets — `HazardMemory { shunned: BTreeSet, dread:
BTreeMap }` from a new `hazard_memory_memo`, with `believed_hazard` /
`believed_hazard_memo` retained as thin wrappers over `.shunned`.** ·
*Why:* the split already exists structurally in `believed_hazard_memo` —
the **terrain shortcut** branch (`feels_frightening(terrain, 0.0, boldness)`
⇒ shunned, `continue`) separates the static set from the alarm-tipped set at
zero extra cost, and the emitter-free fast path returns before any dread can
be recorded (which *is* the byte-identity proof). Keeping the old entry
points untouched leaves both existing callers and ~8 unit tests unchanged. ·
*ideonomy 3/0.*

---

## Capture manifest

**Idea registry (`book/src/frontier/idea-registry.md`, PSY-11) — at close:**
- flip "the VISCERAL FELT phobia … the next campaign" → SHIPPED (The Shudder).
- new reserved clauses:
  - **anticipatory dread and time-decay must ship together** — the
    longer-reach gradient opens a loop that only a forgetting half-life can
    close (pass-2 cycle finding).
  - **entity-keyed dread** — fear of a remembered *individual* rather than a
    place; the seam where PSY-11's memory meets SOC-9's enmity edges
    (pass-3 animacy finding).
  - **kind-keyed / generalized dread** — one bad cliff → all cliffs; the
    transfer that makes a phobia portable and is the seed of learned,
    category-level fear.
  - **the sanctuary** — the proseasis of the phantom: a cell remembered as
    where fear *lifted*, read as comfort; the memory form of The Alarm's
    reserved reassurance/calm-contagion, and a member of PSY-11's already-
    reserved experiential-memory family (`believed-comfort`).

**Followup register (`.superpowers/sdd/followups.md`):** narration wording
for dread in `felt_phrase` (a dread-driven Danger currently narrates like
any other fear); greedy-drive dread (foraging/fleeing gradients ignoring
remembered ground).

**Rejected branches:** affect-only unease (#1); anticipatory dread (#2,
reserved not rejected); self-reinforcing dread (#3); binary dread +
`DREAD_SCALE` knob (#4); a `Perceived` field (#5).

**Chronicle seed (pass 3, abstraction-lift):** the structural form is
*negative caching* — a judgment about a place, valid when written, never
re-checked at read time, invalidated only by re-observing the place. DNS
NXDOMAIN caching, an HSTS pin, a roadworks sign left up after the works are
done. The phobia is a stale cache entry that only a visit can evict.

---

#9 [G4] — **Plan shape: 4 tasks, no docs task.** · *Decision:* T1 split fold ·
T2 drive term · T3 wiring · T4 e2e + evidence; the chronicle / registry flip /
retro are left to `closing-a-campaign` at G6. · *Why:* matches The Phantom's
plan (3 tasks + close-owned docs) and the project's Definition of Done, which
routes book work through the close skill. Self-review against the spec found
full coverage of §1–§4, determinism, cost and all nine success criteria. ·
*Flagged-item #4 disposition:* G3 approved as written, so "the fear must stay
falsifiable" stays a spec principle rather than a `docs/decisions/` record; it
is enforced in the plan by making `believed_hazard_clears_a_disproven_phantom`
the named tripwire in T1.4, T3.4 and T4's assertion (3). · *ideonomy 0 passes*
(G4 is a mechanical self-review, not a decision point).

#10 [G5] — **The spec's cost hypothesis is FALSIFIED.** The health battery
went 364 s (T2 tree) → 522 s (T3 tree), measured back-to-back on the same
machine by stashing only `liveness.rs`. The emitter-free null controls moved
+3.3% (one added `.shunned.clone()` in `affect_of_memo`); the remaining
~158 s is entirely on the emitter-bearing scenarios, where creatures that
remember an alarm now feel it and *step off*, so more sim steps run — and a
longer walk grows the ledger the belief folds are quadratic in. · *Decision:*
keep the behaviour, record the falsification, and re-score the spec's Cost
section at close rather than leaving the hypothesis standing. Not a fidelity
cut (nothing was approximated away) — an intended behaviour change that costs
what behaviour costs. **Leads the G6 digest**: the commit gate's critical
path moves from `walker_battery` (434 s) to `health_calibration` (522 s). ·
*ideonomy 0 passes* (a measurement, not a design choice; the design question
it raises — whether the new behaviour is *sane* rather than merely intended —
is discharged by T4's by-cause/prevalence comparison).

#11 [G5] — **Two plan-fixture bugs, corrected by the implementer, not
worked around.** (a) The Task-3 test as written gave the rememberer a *safe
revisit* after the emitter left — which is precisely the disproof The
Phantom's staleness rule performs, so the memory was empty and the test was
inert. (b) Its days fell at NIGHT for a Diurnal creature, whose sleeping
emitter pursues Fatigue, not Danger, so no alarm was ever emitted; and at the
plan's later day thirst had saturated to 1.0 and won the arbitration outright.
Fixed by keeping the creature standing where it was frightened (no revisit,
so no self-disproof) and moving the fixture into daylight at a low-thirst
hour, with explicit red/green verification against `dread: None`. The
implementer also hardened `a_dread_afraid_creature_raises_no_alarm`, which as
planned passed *before* the wiring existed (an empty field for want of an
empty memory) — it now asserts the dread is non-empty first. · *capture:*
retrospective — a "felt" test must not write the extinction into its own
fixture, and liveness fixtures carry a day/night trap.

#12 [G5] — **Correction to #10: the cost falsification was overstated.** T4
re-measured and the picture changed. On the REAL worlds the campaign is a
strict no-op: `health_report` is **bit-identical** pre/post on seeds 0, 1, 2,
7, 42 — every field to the last printed digit, including the whole `by_cause`
map — because those worlds are emitter-free and the fold's fast path returns
before any dread is recorded. The probe timings that looked like a 1.5×
regression (352.9 s pre → 527.9 s post) did not reproduce: a confirming re-run
on the post tree read **330.7 s**, faster than pre. This box runs parallel
campaign sessions, so wall-clock under load is not a reliable signal at this
resolution. · *Standing:* #10's *measurement* is retracted as noise-dominated;
what survives is the *structural* claim that emitter-bearing scenarios simulate
more steps because creatures now flee what they remember, which is the
behaviour the campaign exists to add. The spec's "no measurable change"
hypothesis is upheld for every real world and unmeasured for the synthetic
harness. Re-score the Cost section accordingly at close — do not carry #10's
1.43× figure into the chronicle.

#13 [G5] — **Inherited red commit gate — NOT this campaign's, and it blocks
close.** 32 failures, all one error: `rows.csv header does not match study
'the-census' schema` (missing `vestige-density`, `forgotten-fraction`,
`dominant-hazard`, `mean-warning-legibility`). Attributed to **The Vestige**
(`1a7d4378`, which registered those four census metrics); the fixture was last
re-pinned at `a634848f` (The Lode). Verified three ways: the implementer
reproduced the identical 32 at HEAD with its working tree stashed; `origin/main`
has not moved since the branch point (0 commits); and the controller reproduced
the failures in the main checkout with no campaign code present at all. The
Shudder's diff touches `windows/vessel/src/liveness.rs` and the regenerated
type-audit report — neither can reach a census schema. · *Decision:* the fix is
`HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`, which is a **carve-out
requiring Nathan's explicit authorization** — escalated at G6, not taken
unilaterally. The resulting value drift belongs to The Vestige's chronicle, not
The Shudder's. This is the recurrence the memory `all-metrics-census-owes-a-
regen-per-added-metric` predicts: nothing in the gate re-runs the census, so
the debt accrues silently until the next campaign trips over it.
