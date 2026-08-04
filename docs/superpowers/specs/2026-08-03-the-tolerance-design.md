# The Tolerance — design

**Status:** spec, awaiting G3 review.
**Date:** 2026-08-03
**Program:** the peoples program
(`2026-08-03-the-peoples-program-design.md`). **Runs immediately after C2-0,
before C2a and C2b** (§7).

A manikin is the reference figure a tailor fits cloth against. A *tolerance* is
the spread the tailor is allowed around it. Hornvale has the first and not the
second: every people is authored as a single point, and the model cannot say
that one people spreads further around its point than another.

## 1. Why — the finding that exposed it

The Generalist authored humans with `threat_response = 0.5`, on the argued
ground that humans genuinely both flee and stand. A review then found that
`RAID_DISPOSITION_MIN = 0.6` (`windows/worldgen/src/history_bake.rs`) gates
raiding on that dimension — so **no human raids in any world's deep history**.

The sentence is the tell. *"Humans do not raid"* is only **sayable** because
every human in every world is one human. The finding is a symptom; the disease
is that a people has no interior.

## 2. What is wrong

**The lift: Hornvale has a representative agent.** A type is being used as its
own only instance — the species row is at once the type description and the sole
exemplar. Macroeconomics named this and its standing critique is *predictive*:
it says in advance which phenomena a one-agent model cannot produce, instead of
letting us find them one accident at a time.

- **Already bitten** — raiding, and a `RAID_DISPOSITION_MIN` disclosure whose
  wording only parses if peoples are uniform.
- **Silently absent, no failing test** — deviance; the exceptional individual;
  **sorting** (risk-takers to the frontier, the cautious to the interior, so two
  settlements of one people differ); lineage drift.
- **Blocks C2d** — a radiation splits a population under selection, selection
  acts on a distribution, a point has no tails.

**The weld: one number serves five consumers wanting different scopes.** This is
The Manikin's "four things welded into one word" recurring one level down.

```
  consumer                     wants          reads today
  ---------------------------  -------------  -----------
  placement / ecology          species mean   species  OK
  language + culture drift     species mean   species  OK
  settlement behaviour (raid)  POPULATION     species  WRONG
  individual NPC behaviour     INDIVIDUAL     species  WRONG
  narration ("humans are...")  species mean   species  OK
```

**The keystone: variability is itself a species trait.** A eusocial insect has
near-zero behavioural variance; a generalist has high variance. So the thing
that makes humans generalists is *psychological* breadth — not the ecological
breadth C2-0 measures.

## 3. What already exists

`hornvale_species::instance_biosphere` implements prototype inheritance — an
instance's effective trait is *its own latest override fact, else its kind's
authored default* — and `tests/instance_lens.rs` proves overrides survive a kind
change. **It is the workspace's only instance lens**, covering `SPECIES_MASS_KG`
and `SPECIES_POTENCY`. The body varies per individual; the mind cannot. This
campaign completes a pattern already committed to.

`domains/culture/src/structure.rs:103` already crosses environment with
psychology (`env.threat > 0.4 * (1.5 - psych.threat_response)`). That is a crude
**elective affinity** and the right shape; §4 generalises it.

## 4. What determines what

The owner's question: are a people's beliefs downstream of its disposition, or
its disposition downstream of its beliefs?

**Today the answer is neither, and that is the deeper bug.** Beliefs derive from
perceived phenomena (`religion::genesis`, per decision 0003's appearances-not-
sources); culture derives from environment × psychology. But **nothing
determines psychology.** It is authored, static, and upstream of everything — an
unmoved mover.

### What the discipline says

Both directions are attested, and the argument is 150 years old:

- **Marx; Harris's cultural materialism** — infrastructure → structure →
  superstructure. Harris explained warfare materially. One-way, and what
  Hornvale half-implements.
- **Weber** — the deliberate counter-case (belief → economic order), but his
  actual position is *elective affinity* (**Wahlverwandtschaft**): ideas and
  material interests **select each other**, neither causing the other.
- **Durkheim** — social morphology → collective representations; the *form* of a
  belief mirrors the *form* of the society. A hierarchic people should get a
  hierarchical pantheon by derivation, not by authoring.
- **Geertz** — religion as a "model **of**" reality and a "model **for**" it.
  Bidirectional by construction.

**On war specifically**, the cross-cultural literature offers a finding built
for a world that already has a climate model: **Ember & Ember** found warfare
predicted not by chronic scarcity but by **unpredictable resource shocks and the
fear of them** — war tracks *variance*, not *level*. **Ibn Khaldun** and Turchin
supply the cyclic half: asabiyyah forged at frontiers, decaying over roughly
four generations of comfort.

### The resolution: it is not a direction, it is a rate difference

The question presumes one arrow. The honest answer is a coupled system whose
components move at different speeds — and Hornvale already models exactly this
shape in climate. **Beliefs are the ocean; behaviour is the weather.** Nobody
asks whether the atmosphere causes the ocean; the ocean is the slow variable
setting boundary conditions the fast one fluctuates around. And a warm ocean
keeps throwing storms after the season that warmed it has passed.

```
  TIMELINE — one people, 600 years, which variable leads

  yr 0    drought variance rises        STRUCTURAL   (fast, reversible)
  yr 40   raiding begins                BEHAVIOUR    lags conditions
  yr 90   martial display earns rank    ORGANIZATION status_basis shifts
  yr 160  doctrine sanctifies the raid  IDEATIONAL   (slow)
  yr 300  climate stabilises            STRUCTURAL   the cause is GONE
  yr 300+ they keep raiding             <-- HYSTERESIS
  yr 480  asabiyyah decays in comfort   the cycle turns
```

Sample at year 100 and conditions cause beliefs. Sample at year 320 and beliefs
cause behaviour. Both readings are correct; they differ in where the ruler is
put. The loop closes by something evolution already named — the **Baldwin
effect**: what a population learns under pressure becomes constitutive of it
over deep time.

**So: do not author the arrow. Author the loop, and let the time constants
produce the arrow.** That is sim-first, and it is MAP-69's own rule — a
mechanism whose output *resembles* the trope, never an authored trope.

## 5. Design decisions

**D1 — A trait is authored as a location *and* a dispersion.** The existing
value becomes the location; a per-species, per-vector dispersion is added beside
it.

**D2 — The species value's meaning must be stated.** Mean, median, or modal
typical member? Today it is ambiguous, which is the bug The Manikin removed one
level up: a datum whose frame is unstated drifts as the model grows. Pick one,
state it in the vector's doc, make the draw consistent with it.

**D3 — An individual's deviation must NOT be derived from its `EntityId`.**

The trap, visible only because The Salt already ratified the rule: an
`EntityId` may be stored, compared and looked up, but **never read for its
value**. `Ledger::mint_entity` assigns sequentially, so an id-keyed draw means
inserting one earlier entity silently reshuffles the psychology of every
individual in the world — deterministic, reproducible, and catastrophic. Key on
a **stable semantic identity**. Resolution order:

```
  own override fact            (exists today)
    else derived draw           keyed on a STABLE SEMANTIC key
    else kind location          (today's behaviour)
```

**D4 — Ship the POPULATION tier; defer the INDIVIDUAL tier.** The raid consumer
wants population scope, and a population draw is **one draw per settlement**.
That fixes the presenting symptom, produces the sorting phenomenon, and clears
C2d's blocker at the level selection acts on anyway.

**D5 — Warlikeness becomes derived, time-varying and place-specific.** It stops
being a species constant:

```
  per settlement, per era:
    structural pressure = f(resource VARIANCE, strife field, frontier)
    disposition         = draw from the people's distribution   (D1)
    organization        = grid/group quadrant                   (D6)
    warlike             = f(pressure, disposition, organization)
```

Resource **variance**, not level — the Ember & Ember finding, and the reason
this is falsifiable against the climate model rather than merely plausible.
MAP-32's strife field is the already-designed structural term.

**D6 — `SocietyVector` IS a grid/group instrument, and the spec says so.**

```
              LOW GROUP            HIGH GROUP
  HIGH GRID   fatalist             hierarchy
  LOW  GRID   individualist        egalitarian / sect

  grid  = how rule-bound a life is   ~=  SocietyVector.sociality
  group = how bounded "us" is        ~=  SocietyVector.in_group_radius
```

Douglas's four biases each carry published predictions about cosmology, risk
perception, and stance toward outsiders. Adopting the framework means those are
**derived from the quadrant** rather than authored per people — falsifiable
structure for free, and adding a people becomes a matter of placing it on two
axes rather than inventing its culture. This is a real theoretical commitment,
taken deliberately.

**D7 — The slow feedback edges are deferred, but their shape is stated here so
the successor is designed for rather than retrofitted.** Sustained raiding
should shift `status_basis`; doctrine should ratchet and **not** decay when
pressure does. That hysteresis is what gives belief *autonomy* — the capacity to
outlive its cause and be wrong about its own world. It needs deep-time history
bake to integrate over and is its own campaign. §8 files it.

**D8 — The raid gate reads the wrong axis, and this campaign fixes it as a
consequence of D5 rather than by touching the constant.** `threat_response` is
*defensive* (flee ↔ stand); raiding is *proactive*. Once warlikeness is derived
from pressure × disposition × organization, the gate stops borrowing a defensive
axis and the mismatch dissolves. The owner's earlier disclose-don't-fix ruling
was made when the alternative was editing the constant in a roster campaign;
D5 supersedes it by removing the constant's role.

## 6. Preregistration

Frozen before implementation.

**H1 — the mean survives.** Each people's *mean* behaviour over many settlements
matches its pre-Tolerance point behaviour. If the mean moves, the draw is biased.

**H2 — the variance appears where authored.** Between-settlement variance in the
gated behaviours is high for high-dispersion peoples and near-zero for
low-dispersion ones.

**H3 — raiding becomes a fraction, not a flag.** Humans raid at a rate strictly
between goblin's and hobgoblin's, rather than at 0 or 1.

**H4 — WITHDRAWN at G3 (2026-08-04), before any code was written.** It read:
"across settlements, raiding frequency correlates more strongly with the
*interannual variance* of the local resource supply than with its *mean*" —
Ember & Ember's cross-cultural finding as a prediction about a simulated world.

**It cannot run, and preregistering it anyway would have been the exact trap
this program exists to catch.** Two independent reasons, both checked against
the code rather than assumed:

1. **There is no variance to measure.** `hornvale_demography::carrying_capacity`
   and `windows/worldgen`'s `forage_supply_field` both return
   `CellMap<f64>` and take **no time parameter**. The supply the packer reads is
   one static value per cell.
2. **The nearest available quantity measures a different construct.** The Mire
   landed a per-cell daily substrate trajectory, but `spin_up` runs a *periodic*
   year to convergence, so it is identical every year by construction — seasonal
   *amplitude*, not interannual *unpredictability*. Ember & Ember draw exactly
   that distinction: predictable seasons are adapted to; it is the surprise that
   drives conflict.

The ingredients do exist — `hornvale_climate::weather_phase` is a function of
**absolute** day, so successive years genuinely differ — but nothing aggregates
that into a per-cell unpredictability field, and carrying capacity does not read
it. Building it is real scope this campaign does not budget.

Filed against `SOC-war-variance` with the blocker named. This is the fifth
instance of the program's own probe-validity ladder biting, and the first caught
at spec review rather than after the code was written.

**The falsification.** If no measured outcome moves once dispersion is authored,
the layer is decorative and should not ship — rung 2 of the program's own
probe-validity ladder.

**The mutation step.** A test that goes RED if the dispersion parameter is
ignored: setting a people's dispersion to zero must collapse its
between-settlement variance.

## 7. Sequencing

```
    C2-0  THE GENERALIST   human x1              <- in flight
    C2t   THE TOLERANCE    dispersion + derived  <- THIS, moved earlier
                           warlikeness
    C2a   THE DEEP REALM   the Underdark
    C2b   THE LONG AGE     lifespan curve
    C2c   THE DELVERS      dwarf x5
    C2d   THE RADIATION    elf x6 + LANG-53      <- HARD dependency
```

**Moved ahead of C2a and C2b at the owner's direction (2026-08-03).** The
argument: if dispersion is what "generalist" means, every people authored before
this campaign is authored in a frame it changes — so authoring five dwarves and
six elves first would mean authoring eleven peoples twice.

**It does not stop C2-0.** Human-as-a-point is a defensible first approximation,
and finishing it keeps its census diff attributable to "a sixth people was
added" rather than mixing in a new psychological layer.

## 8. Non-goals

- **The individual tier** (D4) and **the slow feedback edges** (D7) — the
  organization drift and the doctrine ratchet. Filed as their own campaign; the
  hysteresis is the payoff and deserves a preregistration spanning centuries of
  simulated time rather than a corner of this one.
- **Per-dimension dispersion.** One dispersion per vector to start; per-dimension
  spread should be argued from a measured need.
- **Widening any vector**, or adding dimensions.
- **Re-authoring existing peoples' locations.** Only dispersions are added.
  Moving goblin remains its own deferred campaign.
- **Deriving the full Douglas cosmology.** D6 adopts the framework and derives
  outsider-stance; the complete "model of / model for" build-out of pantheon
  shape from quadrant is a religion-domain campaign.

## 9. Flagged for review

1. **An epoch and a census regen.** Population draws consume a new stream label
   and warlikeness becomes derived, so every world with settlements moves.
   Carve-out; authorization requested at that campaign's close.
2. **A new stream label is a permanent save-format contract** — a `pub const` in
   the owning crate's `streams` module, published via `stream_labels()`, never
   renamed.
3. **D3 is the highest-risk decision.** Keying on `EntityId` is the obvious
   implementation and is catastrophic. The stable semantic key must be chosen
   and defended in the plan.
4. **D6 imports a theoretical framework.** Grid/group is well-established but
   has its own contested literature, and adopting it constrains how every future
   people is authored. Taken deliberately at the owner's direction; flagged
   because it is the kind of commitment that is expensive to reverse.
5. **D8 supersedes an earlier owner ruling** (disclose-don't-fix on the raid
   gate). That ruling was correct given its alternatives; D5 changes the
   alternatives. Called out so the reversal is visible rather than silent.
6. **H4 may falsify against our own ecology.** If raiding tracks mean supply
   rather than its variance, either the climate model does not produce the
   right kind of shock or the anthropological finding does not transfer. Both
   are findings; neither is a defect.
7. **The name is reversible.** "Tolerance" reads as engineering (permitted
   spread around a nominal) and continues The Manikin's tailoring lineage, but
   also reads as social tolerance.
