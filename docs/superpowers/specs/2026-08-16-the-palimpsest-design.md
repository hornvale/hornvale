# The Palimpsest — memory gets a unit, and the unit varies by who remembers

**Program:** Myth, campaign 3 of 4. **Predecessor:** The Retelling
(`2026-08-14-the-retelling-design.md`).

A palimpsest is a manuscript scraped and rewritten with the earlier text
still showing through. Campaign 2 built a boundary model where a claim is
damaged at most once; this campaign is about accumulation — many rewritings,
each leaving a trace.

## 1. What this campaign produces

- A **generational amplitude** for distortion: a retelling damages a claim in
  proportion to the number of generations it spans, where a generation is the
  teller's people's own allometric generation length.
- A **ladder extended with two derived social rungs** — generation and
  lifespan — so the resolution hierarchy is no longer purely astronomical.
- **Three co-equal preregistered accumulation rules**, all reported. This
  campaign deliberately does not choose between them; §6.4 says why.
- A readout on the census seed panel, not one world.

## 2. Non-goals

- **Corroboration.** Still blocked, still for campaign 1's stated reason: it
  needs a notion of belief that changes on confirmation, which does not exist.
  Unchanged by anything here.
- **Contact.** No mechanism added for a claim to cross a people boundary.
  `KNOW-mismatch-needs-contact` stands.
- **The signed / directional amplitude.** §7.
- **Non-monotone distortion.** Reachable now (§7) but not built: varying the
  accumulation rule and the monotonicity at once measures neither.

## 3. Substrate, measured before the model was frozen

All figures seed 42, on `campaign/the-palimpsest` at `1e92c152`, from
`windows/hearsay/tests/probe_teller_relations.rs`. Reported as substrate, not
outcome — the precedent is campaign 2 §3, and the discipline is load-bearing
here because **this campaign's first two designs were killed by measurements
taken after they were proposed** (§8).

### 3.1 Positive control

The probe reproduces campaign 2's published held-claim ceiling exactly:
**3,237 held claims one rung below finest**, matching the chronicle's "3,237
one rung below" on a world that has since moved 165 commits. A zero or a null
below is therefore a real one.

### 3.2 The ceiling's cause, corrected

Campaign 2's chronicle attributes the one-rung ceiling to stance being an
absorbing partition. That is right about held claims and wrong as a general
statement about paths, and the sharper version matters because it tells you
what to change:

```
transmission is strictly parent -> child on a single-parent founding tree,
so ANY predicate over the teller/hearer LINEAGE RELATION is constant across
every step by construction.
```

Measured: **209 paths cross stance twice** — `Bystander -> Perpetrator` is
reachable when the attacker sits inside a witness's subtree. Those paths carry
no held claim, because `variants_about` retains the least-corrupted route per
holder, which is why the published ceiling of 1 is correct for held claims.

An earlier draft of this spec asserted the two-crossing case was structurally
impossible. Its own positive control went red at 2. Recorded because the
argument was persuasive and wrong.

### 3.3 The retention rule is not a second ceiling

`variants_about` keeps `min lossy_steps` per holder, which could in principle
mask compounding. Measured, it does not: for every candidate axis the retained
maximum equals the all-paths maximum. It binds only for stance, where it
collapses those 209 two-flip paths to zero held claims. **Predicted a second
ceiling; found none.**

### 3.4 Which candidate axes vary at all

Over 93,729 carrying steps and 13,930 paths:

| axis | fires | max flips/path | retained max |
|---|---|---|---|
| stance (campaign 2) | 4.3% | 2 | 1 |
| coeval (was the teller alive) | 15.9% | 6 | 6 |
| raider (teller's own history) | 51.6% | 15 | 15 |
| same-cause | 51.4% | 14 | 14 |
| same-function | **0.0000** | 0 | 0 |

`same-function` is **inert on 100% of steps** — the same shape as the
species-keyed filter campaign 2 killed in its §3.2, found the same way and
before it reached a spec.

The ~51% axes are near-balanced labels on adjacent nodes, i.e. close to a coin
flip; that is the "global noise knob in costume" objection made numeric.

### 3.5 Generation length varies by people, 6.75x

```
goblin      21.65 yr     gully-dwarf  117.83     desert-elf 139.57
kobold      30.24        desert-dwarf 119.68     drow       140.95
human       30.36        hill-dwarf   121.46     high-elf   142.94
hobgoblin   30.87                                wood-elf   142.94
bugbear     35.58                                sea-elf    144.85
gnoll       35.86                                snow-elf   146.08
```

Three clean tiers, nothing authored — allometric, from body mass and metabolic
class. A lifespan is derivable for **15 of 15** peoples (goblin 49.2 yr, human
69.0, snow-elf 332.0).

### 3.6 The amplitude's scale, and the unit error it exposed

The amplitude frozen in §5.1 is the generational span of one retelling:

```
generations spanned by ONE retelling
  p10=0.000  p50=0.804  p75=3.215  p90=8.036  p99=22.502  max=76.769
```

Measured in **days** instead, the same quantity has p50 = 9,131 against a
ladder topping out at one year (368.054 std days) — 25x past the coarsest rung
on a single step, i.e. total saturation. **The unit was the defect, not the
mechanism**, and the unit that fixes it is the one that varies by people.

### 3.7 What no ladder of natural durations can hold

```
accumulated over a whole path:  additive   p50=12.86  p90=45.00  max=77.91
                                quadrature p50= 8.27  p90=23.19  max=76.78
```

The longest natural duration in this world is a lifespan, at ~2.2 generations.
The median transmission path spans 8–13 generations. **So a ladder built only
from durations the world contains will saturate at the median under any
accumulating rule.** This is a fact about the world — chains outlive every
natural unit — and §6.4 is the response.

## 4. Where the code lives

- `windows/hearsay/src/ladder.rs` — the two new rungs.

  **A layering problem the first draft of this section got wrong, and it
  constrains the API.** The allometric lookup goes through
  `WorldComponents::assemble()`, which lives in `windows/worldgen` — the
  composition root. `windows/hearsay`'s own `Cargo.toml` states that its
  library must *not* depend on the composition root at runtime ("a read-only
  window must not depend on the composition root"), and worldgen is a
  dev-dependency there precisely for that reason. So `PrecisionLadder::of`
  cannot call `generation_length_of` itself.

  The resolution is to **pass the durations in**: `PrecisionLadder::of` gains a
  parameter carrying per-people generation and lifespan spans, which the
  composition root (or the lab/CLI caller that already builds the world)
  supplies. The window keeps reading only what it is handed, the derivation
  stays at the root, and no layering rule bends. This is a signature change to
  an existing constructor and therefore touches every call site.
- `windows/hearsay/src/stance.rs` — unchanged. Campaign 2's boundary model
  keeps its tests and its meaning.
- `windows/hearsay/src/derive.rs` — `variants_about` gains the amplitude and
  the three accumulation rules behind one enum.
- `windows/worldgen/src/descent.rs` — `generation_length_of` is public *within
  a private module*, so it is unreachable from outside the crate (confirmed:
  `error[E0603]: module descent is private`). Whether it needs widening depends
  on where ladder construction lands: if the composition root builds the
  ladder, no change; if `windows/lab` or `cli` does, the module must be
  re-exported. **Decide this in the plan, not at implementation time** — it is
  the difference between a no-op and a public API change.
- `kernel/` — untouched. `Precision` stays a bare rung index; all duration
  arithmetic stays in the window, for campaign 2's stated layering reason.

## 5. The derivation

### 5.1 The amplitude

```
gen_span(teller, hearer) =
    | occ-founded(hearer) - occ-founded(teller) |
    ---------------------------------------------
      generation_length(people(teller)) * year_days
```

One axis, both parties on it, a magnitude rather than a category. Read as: *a
story handed down across three generations blurs more than one handed across
half a generation.*

**Why the teller's generation length and not the hearer's.** They are always
the same, because fission never crosses a people boundary — campaign 2 §3.1
measured **zero of 658** typed inheritance edges, and its chronicle restates
the same zero as **zero of 780** after absorbing main moved the world. Both
numbers are that campaign's; they differ because the world does, not because
the finding does. Campaign 2's zero is load-bearing here in the opposite
direction from usual: it makes the choice free of consequence, and it means a
lineage has one stable memory resolution.

**Why this is not the species-keying campaign 2 forbade.** §3.2 of that spec
rejects a *filter* keyed on `occ-people`, because comparing teller's people to
hearer's people is inert on 100% of edges. Nothing here compares two peoples.
The people sets the **unit**; being constant along a lineage is harmless for a
unit and is arguably the point. The distinction is comparison versus
parameterisation, and it is the whole reason this axis is admissible.

**What this buys that campaign 2 could not have.** The same gap in days is
0.18 generations for an elven lineage and 0.70 for a gnoll one, so **two
peoples remember the same event at different resolutions** with no contact
between them. That is a source of divergence the world can actually supply,
where corroboration is still blocked for want of contact.

**Decision 0021.** The functional form is authored, as every model's is; 0021
governs *inputs*, and every input here is a committed world fact — founding
days from history, generation length from allometry over body mass and
metabolic class. No parameter was handed to the world.

### 5.2 The ladder gains two derived rungs

Campaign 2's rungs are read from committed astronomy and sorted by real span.
This campaign appends two read from committed allometry:

```
day -> moon(s) -> year -> GENERATION -> LIFESPAN
```

Per people, so an elven ladder is longer in absolute time than a goblin one.
Both new rungs are `Option`: a people with no mass-derived life history (an
`Ametabolic` kind) contributes neither, exactly as a moonless world
contributes no lunar rung. **The non-nesting principle is preserved and
strengthened** — a generation does not divide a year any more than a synodic
month does.

This is a genuine change to campaign 2's design, which said rungs are read
from astronomy. What it preserves is the principle underneath: **rungs are
derived from the world, never authored.** An invented "century" rung would
fail that test; a generation does not. Those two look adjacent and are not,
and an earlier draft of this spec wrongly treated them as one option.

### 5.3 Precision is computed at emit

Accumulated damage is a continuous width carried with the claim. The reported
rung is the coarsest rung whose span does not exceed that width, computed when
the claim is read — never snapped into the accumulator.

This is the project's own quantize-at-emit-only discipline applied to a
non-float quantity, and it is what decouples firing *rate* from firing *depth*:
campaign 2 spends a whole rung per firing, which welds them together and makes
the one-rung ceiling and saturation the same defect seen from opposite ends.

**The surviving invariant is precision-rank monotonicity**, unchanged from
campaign 2: width only grows, so the rung index only rises. No statement about
error is made or implied — a claim is an interval that widens, not a point
that moves.

## 6. Preregistration

Frozen before the code that would move it. Every readout is on the **census
seed panel**, not seed 42 alone — campaign 2's readout measured one world and
recorded that as a deferred limitation.

### 6.1 Disclosure: this freeze is contaminated, deliberately

The author of this spec has read campaign 2's retrospective and chronicle, and
therefore knows 0.662, 11.9%, the median of 2, and the one-rung ceiling.
Campaign 2's handoff asked that this campaign be frozen by someone who had
not. **Nathan was asked and accepted the contamination explicitly.**

Binding consequence, in the shape of campaign 2's own `0.8848` disclosure: **no
campaign-2 figure may be used as a threshold anywhere below.** Where a
hypothesis needs a bar, it is stated as a direction or as a comparison
internal to this campaign's own data.

### 6.2 H1 — species-differentiated retention

*Do long-generation peoples retain finer precision for events of comparable
age?*

The mechanism predicts yes, and it is the cleanest consequence of §5.1: the
same elapsed time is fewer generations for an elf than a gnoll.

- **Confirmed** — retained precision rank correlates negatively with the
  holder's people's generation length, in the same direction across the panel.
- **Falsified** — no consistent relationship, or the opposite direction.
- **No verdict** — direction consistent but the panel disagrees on sign.

This is the campaign's headline candidate because it is new, it is derived
rather than authored, and it does not depend on which accumulation rule wins.

### 6.3 H2 — does the ceiling actually break?

*Does the retained rung distribution spread beyond one rung?*

Campaign 2 retained a maximum of 1. **Stated as a direction, not a bar** (§6.1):
confirmed if the retained distribution puts mass at three or more distinct
rungs under at least one accumulation rule; falsified if it collapses to two.

Note the honest risk, stated before the readout: §3.7 says accumulation exceeds
the ladder at the median, so the plausible failure is not a ceiling but
**saturation** — everything at the coarsest rung. That would falsify H2 in a new
way and is a reportable result, not a defect.

### 6.4 The three accumulation rules are co-equal, and none is primary

```
additive        w  = w + span
quadrature      w  = sqrt(w^2 + span^2)
multiplicative  w  = w * (1 + span)
```

All three are implemented, all three are reported, **and this spec designates
no primary.** The reason is disclosure, not indecision: substrate for all three
was measured (§3.7) before this section was written, so nominating one now
would be selection on data already seen. Quadrature is the standard
error-propagation rule for independent contributions; multiplicative is the
one commensurate with an approximately geometric ladder; additive is the naive
baseline. Each has an argument, none has evidence.

**What the campaign may not do:** pick a favourite after the readout and
present it as the model. If one rule is adopted for campaign 4, that adoption
is a separate, dated decision citing this campaign's numbers.

### 6.5 H3 — does divergent structure still predict divergence?

Campaign 2 found Spearman rho 0.662 between maximum-antichain width and variant
count. With content varying more, the relationship should survive.

- **Confirmed** — positive monotone relationship across the panel.
- **Falsified** — no relationship or a negative one.

Per §6.1 the value 0.662 is **not** a threshold; H3 is a direction test only.

## 7. Carried forward

- **The signed amplitude.** `gen_span` is symmetric — it does not care which
  party is further from the event. A signed rule degrades a claim more flowing
  *toward* the more-removed party. This is the ingroup/outgroup direction
  decision 0021 requires be derived rather than authored, and it costs nothing
  to compute. Held back only because varying it and the accumulation rule at
  once measures neither.
- **Non-monotone distortion is reachable, and campaign 2's reason for calling
  it impossible is wrong.** Its §7 says *"`Claim` carries no time, so `hops` is
  the only clock"*. The guard takes the ledger, and the ledger carries founding
  and ending days — a second clock in world-time. Whether the campaign wants
  regularisation-then-re-embellishment is open; that it is unreachable is not.
- **Blind reconstruction** (`KNOW-lectio-difficilior`), **subject-drift
  misattribution** (`KNOW-misattribution-drift`) — unchanged from campaign 2 §7.
- **`KNOW-descendants-with-path`** was reported by campaign 2's handoff as
  landing post-merge and **does not exist in the repo**. Flagged, unresolved.

## 8. Process record — three designs died to measurement

Kept in the spec rather than only the retrospective, because each death was
caused by the same omission and the pattern is the deliverable:

1. **Teller-history-alone** (raider keying) — killed by argument: it never
   consults the claim, so it cannot be told apart from a per-community noise
   knob. Later confirmed numerically at 51.6%, near a coin flip.
2. **Distance-from-event as amplitude, in days** — killed by §3.6. Off-scale by
   25x on a single step. Proposed, approved, and dead within one measurement.
3. **A ladder of natural durations under an accumulating rule** — killed by
   §3.7. Chains span 8–13 generations; the longest natural unit is 2.2.

Each was measured *after* being proposed. The standing rule this campaign
adopts: **measure a candidate quantity's scale against the scale it must be
commensurate with, before writing it into a design.** Two of the three would
have been caught by one cheap probe run earlier.

## 9. Definition of done

- The amplitude, the two derived rungs, and the three accumulation rules in
  `windows/hearsay`; `worldgen::descent` made public; `#![warn(missing_docs)]`
  clean; `type-audit:` tags on every pub-boundary primitive and the report
  regenerated **in the same commit**.
- `hornvale-species` promoted from the probe's dev-dependency to whatever the
  runtime path actually needs, or removed if the composition root supplies it.
- The §6 readouts on the census seed panel, reported against their decision
  tables including NO VERDICT, and all three rules reported.
- Campaign 2's tests keep their meaning: `claims_about` and `stance` untouched,
  the maximum-antichain test untouched.
- The probe promoted to a permanent battery or deleted, decided explicitly.
- **A roster check**: `hornvale-hearsay` sits in `docs/timings/subfloor-roster.tsv`
  with 48 entries; any new crate or test target must not silently fall out of
  the commit gate the way that crate did for a whole campaign.
- Chronicle entry, freshness sweep, Confidence Gradient re-score if moved,
  retrospective, registry rows with resolving **Where** cells.

## 10. Open for Nathan at G3

1. **The ladder change is the fidelity call.** Adding social rungs alters a
   merged campaign's stated design (§5.2). The principle survives; the letter
   does not. Recommendation: proceed. The call is yours.
2. **Saturation may be the result.** §3.7 says the plausible outcome is that
   everything lands at the coarsest rung. That is a publishable finding and
   this spec is written to report it rather than to avoid it — but it is worth
   knowing before the work starts that the headline could be a null.
3. **Three rules, no primary** (§6.4) means the campaign ships three numbers
   where a reader may want one. That is the honest consequence of having
   measured substrate first, and I would rather carry it than pretend.
