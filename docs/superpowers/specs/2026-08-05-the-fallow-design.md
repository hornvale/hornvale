# The Fallow — the land has a capital account, and it can be spent

**Status:** Draft for review (2026-08-05) · **Campaign:** the-fallow ·
**Follows:** [The Tilth](2026-08-04-the-tilth-design.md), whose stage 1 is now
visibly *half* of a change · **Registry:** `BIO-land-capital`,
`BIO-reliability-not-mean`, `BIO-subsistence-feeds-capacity`

## 1. The provocation

Nathan, on being shown that adopting Lieth flattened deep history from sixteen
stacked steadings to one: *"The Dust Bowl leads me toward #3. Perhaps Lieth is
correct, but we're using it for the wrong thing. Perhaps there are multiple
eviction pressures and we're not considering the right ones."*

The Dust Bowl is the right lens because **NPP was not zero there.** Grass grew;
crops had grown for decades. What evicted roughly two and a half million people
was drought *variance*, soil *degradation* from ploughing, and the interaction of
the two. None of that is mean annual productivity.

## 2. The diagnosis: Lieth is right and is being asked the wrong question

Lieth's Miami model answers **"what grows here?"** The bake reads its output as
**"can people live here, and will they stay?"** Those differ by three terms, and
all three are missing or disconnected:

```
  productivity      what the land GROWS per year (mean).  Lieth. Correct. Present.
  reliability       how DEPENDABLE that is year to year.          NO REFERENT
  land capital      a STOCK that extraction can draw down.        NO REFERENT
  subsistence mode  farming / herding / foraging.        COMMITTED, NEVER CONSULTED
```

Verified: `subsistence` and `occ-tech` are committed facts that appear **nowhere**
in `domains/demography` or `history_bake.rs`. So the same productivity supports the
same population however a people extracts from it.

**The Dust Bowl in one clause:** the plains had adequate productivity for grazing
and inadequate resilience for ploughing. Same land, same NPP, different mode,
opposite outcome. Hornvale cannot express it — not because a number is wrong, but
because two terms have no referent and the third is unwired.

### 2.1 Income without capital

Cross-domain re-instantiation converges from four directions — fisheries
(standing stock versus recruitment), the Ogallala aquifer (positive recharge,
negative balance), soil organic matter (centuries to build, a decade to spend),
and finance (income versus capital):

> **Hornvale models only income.** Annual NPP is income; soil is capital. A model
> with no capital account cannot represent living beyond the land's means.

That is why **every eviction pressure in the bake is natural** — ice, famine,
war — and **not one is anthropogenic.** A place can never be ruined by its own
occupants.

### 2.2 The state machine, and two missing states

```
  TODAY     Unoccupied ──> Settled ──(ice | famine | war)──> Ruin ──> Unoccupied

  MISSING   Settled ──(extraction > regeneration)──> DEGRADING
            DEGRADING ──(stock spent)──> EXHAUSTED
            Ruin ──(stock refills)──> RECOVERING ──> Unoccupied
```

- **No edge Settled → Exhausted**, so anthropogenic pressures are not merely
  absent but structurally **unreachable**.
- **No Recovering state**, so a re-settled cell is identical to a virgin one — the
  land has no memory in *either* direction. (`paleoclimate` already ships
  **refugia**, which is the same idea for climate; this is its soil cognate.)
- **Slow in, fast out** is what makes collapse dramatic, and degradation is
  **latent** — invisible until it crosses a threshold. A model carrying only
  visible mean productivity *cannot* produce surprise, which is exactly why the
  Dust Bowl surprised people.

## 3. Design

### 3.1 A land-capital field, and a flow that draws it down

Add a per-cell **stock** — call it `tilth`, the worked condition of ground —
alongside the existing productivity flow. Its shape, deliberately borrowed from
the drainage/flow machinery already proven in `terrain`:

```
  regeneration(cell)  = f(productivity, climate)        slow, always positive
  extraction(cell)    = g(population, subsistence, tech)
  d tilth/dt          = regeneration - extraction
  effective capacity  = productivity x mode_factor x h(tilth)
```

`h(tilth)` is the multiplier that makes degradation bite: full stock is neutral,
a drawn-down stock reduces what the land yields, and an exhausted stock reduces it
sharply. **The asymmetry is the point** — `regeneration` is small and `extraction`
can exceed it, so a community can run a deficit for generations before the
consequence arrives.

### 3.2 Subsistence mode enters capacity

`mode_factor` is where `subsistence` finally connects. Herding extracts less per
head and degrades less; farming extracts more and degrades more, especially on
marginal ground. This is the term that lets the same cell be sustainable for one
people and ruinous for another, and it needs **no new authored data** — the facts
are already committed.

### 3.3 Reliability: a bad year, not a bad average

Productivity gains a variance term, so marginal ground fails *intermittently*
rather than never. This is what restores the churn: a community on thin ground
survives good decades and is evicted by a bad one. Crucially it must ride the
**paleoclimate era series the bake already steps**, not a fresh per-epoch draw —
otherwise it is noise rather than climate.

### 3.4 What this restores, and why it is better than what it replaces

The old hard zero below 2 °C produced sixteen stacked steadings on one cell,
every layer ending *"the cold drove them on."* That churn was real texture
generated by a **modelling error**. This design regenerates it from causes the
world can explain: a bad decade, exhausted ground, a people farming land that
would only bear grazing. Same texture, honest mechanism — and the history page
gets a better story than ice, sixteen times.

## 4. Preregistration (decision 0016)

Frozen before implementation. Instruments: `tilth_phase_diagram.rs` (extended with
a time axis) and the seed-42 history page.

**H1 — deep history returns.** At least one cell on seed 42 carries a column of
**≥ 4 layers**, against the **1** that stages 1+4 leave. Restored from degradation
and variance, with no return of the hard zero.

**H2 — eviction causes diversify.** Ruins attribute to more than one cause, with a
non-trivial share (**≥ 20%**) anthropogenic (exhaustion), against **0%** today.

**H3 — mode matters.** At least one cell is sustainable under herding and ruinous
under farming, holding climate fixed. The phase diagram measures this directly.

**H4 — the null.** If degradation produces columns only where the old hard zero
did, then variance was doing all the work and the capital account is decoration —
report it and cut §3.1. And if columns *never* return, the churn was not
climate-driven at all but an artifact of a specific eviction path, which would
send this back to `nearest_dest` rather than forward.

## 4a. Feasibility measured BEFORE implementation (2026-08-05)

`windows/worldgen/tests/fallow_feasibility.rs` models one cell over 80 epochs with
no world at all, and sweeps extraction rate against climate variance. Two results,
and the second is a defect in the probe rather than the design.

### §3.1 works, but needs a timescale separation the spec did not state

```
  extr\var    0.0    0.2    0.4    0.6    0.8       column depth (H1 wants >= 4)
      0.00      1      1      1      1      1
      0.04      1      1      1      1      1
      0.08      1      1      1      1      1
      0.16      1     14     17     17     16
```

A cliff, not a gradient — and it has a closed form. At equilibrium the capital
account settles at `tilth_eq = regen / (regen + extraction)`, so

```
  extraction 0.04 -> tilth_eq 0.333, eff_eq  6.67   persists (graceful decline)
  extraction 0.08 -> tilth_eq 0.200, eff_eq  4.00   CYCLES
  extraction 0.16 -> tilth_eq 0.111, eff_eq  2.22   CYCLES
```

The mechanism has **two attractors**: *persistent-degraded*, where a community
shrinks gracefully onto worn-out ground and stays forever, and *cyclic*, where it
overshoots and collapses. Columns exist only in the second. Since `GROWTH_RATE` is
0.2 per epoch, a population absorbs a ~20%-per-epoch fall in capacity simply by
shrinking — so **collapse requires degradation to outrun population adjustment.**

That is a real design constraint and §3.1 must state it: extraction has to be fast
relative to `GROWTH_RATE`, or the model produces dignified decline instead of
collapse-and-refound. It is also reassuring that the cyclic side lands at **14–17
layers** against the historical **16** — the mechanism reproduces the observed
texture rather than merely producing *some* texture.

### The variance axis was under-powered — that verdict does not stand

The sweep appears to show variance contributing nothing. It does not, because the
probe's climate stand-in is too gentle:

```
  amplitude 0.8 -> multiplier range [0.501, 1.681]   nominal claim was [0.2, 1.8]
```

Two incommensurate sines rarely align, so the widest setting only ever tested a
mild climate. **Variance is unresolved, not refuted**, and a successor probe should
drive it from the real paleoclimate era series rather than a beat function. Recorded
because the temptation is to read the flat rows as a finding; they are a limit of
the instrument.

### What this changes

- §3.1 gains the timescale requirement above as an explicit design constraint.
- H4's first null (*"variance does all the work, cut §3.1"*) is **not** answered —
  the instrument could not answer it. Its second form (*"columns never return"*) is
  answered no: they return, at 14–17 layers.
- The bifurcation is itself a risk worth carrying into §5: texture that depends on
  which side of a sharp boundary a constant lands is fragile, which is the trap the
  old hard zero set once already. Either the boundary is widened, or the constants
  are derived from it deliberately rather than chosen near it.

## 5. Risks

- **A stock is state, and state is a save-format contract.** `tilth` must either
  be re-derivable from the seed plus the era series (preferred: it is a function of
  history, and the bake already replays history) or committed, which is an epoch.
  **Re-derivable is the design; committing it is the failure mode.**
- **Two new fields over 40,962 cells × 80 epochs**, and the census multiplies by
  ~1000 worlds. `regeneration` is cheap; `extraction` needs population, so it is
  computed inside the bake's existing loop rather than as a separate pass.
- **The variance term must not become a new draw** if that can be avoided — new
  seeded draws are the epoch-triggering additions (`windows/worldgen/CLAUDE.md`).
  Deriving variance from the era series keeps the stream contract intact.
- **`h(tilth)` and the extraction rates are three new constants**, and decision
  0104 binds them: each declares its kind, and none may be calibrated against
  Hornvale's own census if it is an Earth-contingent claim. Soil-degradation rates
  are Earth-contingent, so they are **cited or authored, never census-fitted.**

## 6. Open questions

1. **Does `tilth` reduce productivity or only capacity?** Reducing productivity is
   more physical (degraded soil grows less) and couples into the whole food web;
   reducing capacity only is cheaper and more surgical. Recommendation: productivity,
   because the food web is the point of `BIO-detritus-derived`.
2. **Is exhaustion permanent within a world's 2,000-year span?** Real severe
   degradation outlasts that. Recommendation: recovery on a centuries timescale, so
   ruins genuinely heal but not within one community's memory — which is also what
   makes a *relict* place meaningful.
3. **Does this subsume The Tilth's sovereignty-floor finding, or sit beside it?**
   The floor flattens *who wins*; this flattens *whether anyone stays*. They look
   orthogonal, but both were surfaced by the same phase diagram and should be
   spec'd knowing about each other.
