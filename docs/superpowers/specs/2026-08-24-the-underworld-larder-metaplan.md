# The Underworld Larder — the program for giving the underworld something to eat

**Four campaigns.** This document records the sequencing and the reason for
it, so a later rung does not relitigate the split. Each rung gets its own
spec, plan, and merge.

## 1. What occasioned it

Nothing feeds the underworld. Carrying capacity is computed from
**insolation**, so a people seated 800 m down is, in the model, fed by
sunlight. The same gap keeps hydrothermal vents barren: `marine_forage_supply_field`
(`windows/worldgen/src/lib.rs`) deliberately gives `Biome::HydrothermalVent`
near-zero productivity, and says why in its own doc —

> a real vent community is CHEMOTROPHIC, which is a metabolic class the enum
> does not have (BIO-chemotrophy), so making it productive here would feed
> vent biomass to photosynthesis-based consumers.

That refusal is correct and it is the whole program in one sentence: the type
system cannot express eating rock, so nothing anywhere can do it.

**The occasion** is The Winze's amendment C (2026-08-24). Nathan's ruling
there is that a breach's consequence MAY persist and travel — evil spreading
through several underworld systems is wanted — bounded by something that
varies world to world, so that not every world becomes *DOOM* at the same
hour. The bounding quantity is the underworld's own productive base, which
does not exist. Amendment C defers all of it here.

## 2. Keystone

**A thing underground can eat the rock, and how much rock there is to eat
decides what the underground can hold.**

## 3. The findings this program rests on

### 3.1 The underworld is 3.264x the surface and every chamber is reachable

Measured post-Drift, seeds 42/7/1234, pooled, per land cell
(`windows/worldgen/tests/suite/winze_scale_probe.rs`): chambers **3.264x**,
reachable chambers **3.264x**, runs 0.502x. `BIO-underworld-has-no-energy`'s
size clause is falsified; its energy clause is what this program addresses.
**That is how much space rung 3 has to feed.**

**Re-run 2026-08-26 (The Sources, harvest task), against `main` post-Glasshouse
(commit `7576eca00`):** all three figures reproduced bit-for-bit. The probe
had been authored 403 commits earlier, before the temperature epoch merged;
whether it still agreed with current `main` was unmeasured until this
re-run.

### 3.2 The six energy sources are reachable from lithology

`BIO-subterranean-energy-sources` names six candidates, each keyed on a
shipped lithology axis. Whether those axes carry independent signal — or all
move together, collapsing six sources into one — was measured and initially
answered STOP. **That answer was an instrument artifact**: the probe sorted
each field vector independently before computing `pearson`, which by the
rearrangement inequality computes the maximum correlation over any pairing of
two multisets rather than the correlation of the data. Corrected, the axes are
genuinely decoupled (`silica`x`porosity` -0.0279, `silica`x`carbonate` 0.1983,
`grain`x`metamorphic_grade` 0.0988), and what IS coupled is exactly what the
source defines as arithmetic on its neighbours (`induration`x`metamorphic_grade`
0.9818). `winze_energy_probe`'s M4 assertions pin the corrected verdict.

**Re-run 2026-08-26 (The Sources, harvest task), against `main` post-Glasshouse:**
all cited correlations reproduced bit-for-bit. This section's conclusion — the
axes carry independent signal — held.

### 3.3 The blocking type is small, and the estimate everyone was carrying was wrong

`BIO-chemotrophy` says "widening that enum makes every reader a blast radius",
and the figure in circulation was 94 sites. Measured by stubbing the change and
running `cargo check --workspace --all-targets`:

```
adding a fifth variant           3 exhaustive match arms   (probe ran to a
                                 CLEAN workspace — this figure is sound)
splitting into two fields      221 occurrences, 34 files, 7 crates,
                                 3 struct carriers, 80 construction sites
                                 (all compiler-found)
```

**The second row was first reported as "~100, all compiler-found" and that was
low.** The probe stopped at "14 errors in 5 files", which was the first wave —
a compile error in an early crate means every downstream crate is never
checked at all. Re-measured by grep, which cannot fail early. The
compiler-found property still holds (no serde on the carriers, no macro
construction); only the count was wrong.

The 94 was 94 *uses of the `Endotherm` value*, nearly all in a Rust-authored
kind table and in test fixtures. Those do not break. `MetabolicClass` derives
no `Serialize` and appears in no authored JSON, so **there is no save-format
or epoch consequence anywhere in rung 1**.

### 3.4 The enum conflates a demand axis with a supply axis

Its doc says its job is to select B0 and the pace multiplier. What
`basal_metabolic_rate_w` does:

```rust
Endotherm | Autotroph => B0_ENDOTHERM,
Ectotherm             => B0_ENDOTHERM * ECTOTHERM_METABOLIC_FRACTION,
Ametabolic            => return 0.0,
```

`Autotroph` is grouped with `Endotherm` because it is a **supply** value in a
**demand** enum and allometry had nothing else to do with it. The doc admits
the result: the shipped autotrophs "are computed exactly as endotherms of the
same mass". The axis this program needs is the one the enum handles worst.

## 4. The campaigns

### Rung 1 — THE GOSSAN: chemotrophy becomes expressible

Split `MetabolicClass` into `ThermalStrategy` and `TrophicMode`; add
`Chemotrophic` as a `Declared` variant no kind carries. Behaviour-preserving
by construction. Spec: `2026-08-24-the-gossan-design.md`.

**Deliberately changes nothing.** Its whole output is that a later campaign
can say the word.

### Rung 2 — THE SOURCES: an energy field over the rock — **DONE**

**`BIO-subterranean-energy-sources` does not exist on `main`.** It is added by
`campaign/the-winze`, unmerged as of this writing, and verified absent from
this branch's registry. Every reference to it here is a forward reference; if
The Winze is abandoned, rung 2 must author the row itself rather than assume
it. Nothing mechanical catches this — the drift check resolves LINKS, and a
registry ID named in prose under `docs/` resolves against nothing.

The six sources of §3.2, keyed on lithology, producing a per-place energy
quantity. Moves `Chemotrophic` from `Declared` to `Witnessed`. **Unblocks
hydrothermal vents at the same time** — the surface half of the same defect,
and free evidence that the mechanism is not underworld-special-cased.

**Shipped by The Sources (merged 2026-08-27); see
[the chronicle](../../../book/src/chronicle/the-sources.md).** Landed as a
seven-term sum (the six named here plus a drainage-gated detrital-import
term the shallow arm needs), evaluated per rung rather than once per column.
This paragraph's own "free evidence" framing does not survive unqualified —
the chronicle corrects it: the terrain model gives every vent the same
chemistry and gradient as the open ocean floor around it, so the evidence is
that the whole ocean floor sits in the productive regime, not that a vent
does specifically. Three of §7's questions came back FALSIFIED (no trough at
the corpus's middle depth; worlds do not differ from each other by the
frozen bar; a single rung's own chambers do not spread out by it either) and
one CONFIRMED (composition — which source dominates — varies far more than
magnitude does), which is why §7 below is now marked measured rather than
left open.

### Rung 3 — THE CEILING: what that field can support

Carrying capacity that does not trace back to the star. Where "not every world
is *DOOM*" is either derived or fails to be. **The open design question lives
here**, recorded now so rung 3 inherits it rather than rediscovering it: The
Winze's amendment C.3 argues the budget should be SYMMETRIC — a spreading
horror and a spreading ecology eat the same rock, so one budget with two kinds
of consumer is a mechanism where a special-case cap on monsters is a knob. Its
cost is that "barren and deadly" and "lush and safe" both become unreachable
world-types. Nathan has not ruled.

**Inherited diagnosis (The Sources, §7 above):** do not design this rung
against lithology carrying the variety budget. Rock chemistry underground
was measured as roughly **three near-constant categorical states** (which
kind of chamber a place is — karst, fracture, lava tube, …), not a
continuum: within one kind the rock reads almost the same value everywhere,
and the three chemical bands the terrain model draws from are wide enough
that almost no sampled value leaves every energy mechanism unproductive.
That is why magnitude compressed at every scale rung 2 measured, and it will
compress the same way here if `Not every world is *DOOM*` is built to read
off lithology-derived magnitude. What rung 2 found instead, and what this
rung inherits as its live lead: variety survives in *composition* — which
mechanism dominates, not how much arrives — and per-world **presence** of an
axis at all (a world can lack a chemistry outright, the way it can lack a
mineral), neither of which this rung's original framing anticipated.

### Rung 4 — THE TENANT: something that eats the budget and spreads

Needs 1-3, and needs naming, which The Winze currently refuses (`thaumic`
stays 0.0, nothing named).

## 5. What is deliberately NOT in this program

- **No fix to `BIO-autotroph-physics`.** It is exposed more sharply by rung 1
  and fixed by neither. Bundling it destroys its attribution, which its own
  doc says in as many words.
- **No unification with `windows/locale`'s `EnergySource`.** Locale already
  has `{ Sunlit, Chemosynthetic, Geothermal }` — "what powers a room's
  ecology" — one layer over, and `Serialize`. Unifying is attractive and
  premature: there is one consumer on each side and no shared use, so naming a
  common vocabulary now is guessing. Revisit when rung 2 knows what it needs.
- **No transit realms, no metaphysics, no named horror.** Rung 4's problem.

## 6. Relationship to live work

The Winze is **not blocked by this program and must not wait for it**. Its
§4.1-4.5 — the derived function, the committed depth, the per-increment
hazard, the knowledge that decays — are independent of everything here. The
graph of the two is joined at exactly one node, the breach, which is why this
is a separate program rather than a fifth Winze task.

## 7. What is unvalidated — **MEASURED (The Sources)**

**Whether lithology-derived energy varies BETWEEN worlds.** Rung 3's entire
promise is that different worlds get different ceilings without anyone tuning
one. What is measured is that the six axes vary WITHIN a world (the tightest
of 18 field x seed combinations is `grain` on seed 42 at IQR/range 0.1740,
over 17x prospectivity's floor). Whether seed 42's rock is systematically
richer than seed 7's is a different statistic and **nobody has measured it**.

The precedent for taking this seriously is in this campaign's own history:
The Winze Task 1 assumed ore prospectivity varied usefully across the map, and
measured 75% of all land inside a band 0.0067 wide. A near-constant field
everyone assumed had structure. **Rung 2 must measure the between-worlds
statistic before rung 3 designs against it.**

**Answer: it does not, at a magnitude rung 3 can build on.** The Sources froze
this exact statistic before writing the derivation (twelve worlds, the
between-world spread of world medians over the typical within-world spread,
bar 0.25) and measured **0.145** — worlds differ by little more than half
the frozen bar, and by less than three-fifths of what one world's own
chambers already vary by internally. The same campaign also found the
per-rung spread within a single world falling short of the same 0.25 bar at
every depth (0.09–0.20), so the shortfall is not particular to the
between-world statistic; magnitude is compressed at every scale this program
measured. What did vary, robustly, is *composition* — which of the seven
candidate mechanisms supplies the largest share at a given place — so rung
3's ceiling cannot lean on lithology-derived magnitude the way this section
originally hoped, and should look to composition, and to which environmental
axes a given world even has, instead.

## 8. Provenance

Nathan, 2026-08-24, after The Winze's amendment C: "let's start that campaign
arc". The four-rung split, the blast-radius figures, and §3.4's demand/supply
reading are this document's contribution; the keystone and the *DOOM*
constraint are his.
