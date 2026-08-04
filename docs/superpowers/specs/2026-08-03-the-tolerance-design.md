# The Tolerance — design

**Status:** spec, awaiting G3 review.
**Date:** 2026-08-03
**Program:** the peoples program
(`2026-08-03-the-peoples-program-design.md`). Inserted after this document's
§6 argument; **must precede C2d**.

A manikin is the reference figure a tailor fits cloth against. A *tolerance* is
the spread the tailor is allowed around it. Hornvale has the first and not the
second: every people is authored as a single point, and the model has no way to
say that one people spreads further around its point than another.

## 1. Why — the finding that exposed it

The Generalist authored humans with `threat_response = 0.5`, on the argued
ground that humans genuinely both flee and stand. A code review then found that
`RAID_DISPOSITION_MIN = 0.6` (`windows/worldgen/src/history_bake.rs`) gates
raiding on that dimension — so **no human raids in any world's deep history**.

The owner's reaction was that this is shocking, and it is. But the sentence
itself is the tell. *"Humans do not raid"* is only a **sayable** sentence
because every human in every world is one human. The finding is a symptom; the
disease is that a people has no interior.

## 2. What is wrong

**The lift: Hornvale has a representative agent.** Strip the specifics and the
shape is that a type is being used as its own only instance — the species row is
simultaneously the type description and the sole exemplar. That shape has a name
in another field. Macroeconomics' *representative agent* draws exactly one
standing critique: a model with one agent cannot produce the phenomena that come
from heterogeneity. Hornvale has a representative-goblin.

The critique earns its keep by being **predictive**. It says in advance which
phenomena are unreachable, instead of letting us discover them one accident at a
time, which is how raiding surfaced:

- **Already bitten** — raiding; and a `RAID_DISPOSITION_MIN` disclosure whose
  wording ("exactly one of the four settling peoples declines to raid") only
  parses if peoples are uniform.
- **Silently absent, with no failing test** — deviance (crime, dissent,
  heresy: behaviour off the modal value); the exceptional individual (a leader,
  a prophet, a traitor drawn from the tail); **sorting** (risk-takers to the
  frontier, the cautious to the interior — which would make two settlements of
  one people differ from each other, something the model cannot currently
  express); lineage drift within a people.
- **Blocks a planned campaign** — **C2d, the elf radiation.** A population
  splits and adapts under selection, and selection acts on a *distribution*. A
  point has no tails, so there is nothing to select on and nothing to diverge.
  The campaign the owner is most invested in is blocked by this, and neither the
  program metaplan nor its author noticed.

**The weld: one number serves five consumers that want different scopes.** This
is The Manikin's "four distinct things welded into one word" recurring exactly
one level down, which is itself worth noticing.

```
  consumer                     wants          reads today
  ---------------------------  -------------  -----------
  placement / ecology          species mean   species  OK
  language + culture drift     species mean   species  OK
  settlement behaviour (raid)  POPULATION     species  WRONG
  individual NPC behaviour     INDIVIDUAL     species  WRONG
  narration ("humans are...")  species mean   species  OK
```

Three of five are correct. The two that are wrong are precisely the two that
produced the shock.

**The keystone: variability is itself a species trait.** The owner's phrasing —
humans are *capable of tremendous variability* — is not the claim that humans
vary and others do not. A eusocial insect has near-zero behavioural variance; a
generalist has high variance. Dispersion is a per-species, per-trait authored
quantity.

That reframes the campaign this one interrupts. **The Generalist measures
ecological breadth; the wide *psychological* niche is the part that is actually
human**, and it is the part nothing models.

## 3. What already exists, which makes this far cheaper than it looks

`hornvale_species::instance_biosphere` already implements prototype
inheritance — an instance's effective trait is *its own latest override fact,
else its kind's authored default* — and `tests/instance_lens.rs` proves
overrides even survive a kind change.

**It is the only instance lens in the workspace.** It covers `SPECIES_MASS_KG`
and `SPECIES_POTENCY`. The body can vary per individual; the mind, society and
perception vectors cannot. This campaign completes a pattern the codebase
already committed to rather than introducing one.

## 4. Design decisions

**D1 — A trait is authored as a location *and* a dispersion.** Not a scalar. The
existing authored value becomes the location; a new per-species, per-vector (not
necessarily per-dimension — see §7) dispersion is added beside it.

**D2 — The species value's meaning must be stated, not assumed.** Mean, median,
or modal typical member? Today it is ambiguous, and the ambiguity is exactly the
bug The Manikin removed one level up: a datum whose frame is unstated drifts in
meaning as the model grows. Pick one, state it in the vector's doc, and make the
draw consistent with it.

**D3 — An individual's deviation must NOT be derived from its `EntityId`.**

This is the trap, and it is only visible because The Salt already ratified the
rule: an `EntityId` may be stored, compared, and looked up, but **never read for
its value**. `Ledger::mint_entity` assigns sequentially, so seeding a
personality draw on the id means inserting one earlier entity silently
reshuffles the psychology of every individual in the world. It would be
deterministic, reproducible, and catastrophically wrong — the exact failure
class The Salt was fought over.

The draw is therefore keyed on a **stable semantic identity**, never a
positional one. The resolution order becomes:

```
  own override fact           (exists today; authored or event-driven)
    else derived draw          keyed on a STABLE SEMANTIC key
    else kind location         (today's behaviour)
```

**D4 — Ship the POPULATION tier; defer the INDIVIDUAL tier.** This is the
campaign's main scoping call and it falls out of §2's matrix. The raid consumer
wants population scope, and a population draw is **one draw per settlement**,
not one per inhabitant. That fixes the presenting symptom, gives settlements of
one people genuine character (the sorting phenomenon), costs a settlement-keyed
stream label, and needs no per-individual storage at all.

The individual tier is real and wanted — the exceptional person, the deviant —
but it belongs with the game layer that has individuals to vary, and it can
reuse the same authored dispersions when it arrives. Shipping the population
tier first also means the elf radiation's blocker clears at the population
level, which is the level selection acts on anyway.

**D5 — The raid gate reads the wrong axis, and this campaign says so without
fixing it.** `threat_response` is *defensive* (flee ↔ stand); raiding is
*proactive*. Gating a proactive behaviour on a defensive disposition would
misclassify any people, not just humans. Fixing it moves raid behaviour for
every people at once and belongs in its own campaign; this one documents the
mismatch and files it.

## 5. Preregistration

Frozen before implementation.

**H1 — the mean survives.** With dispersion authored and population draws live,
each people's *mean* behaviour over many settlements matches its pre-Tolerance
point behaviour. If the mean moves, the draw is biased and the layer is wrong.

**H2 — the variance appears where authored.** Settlements of a high-dispersion
people differ measurably from one another; settlements of a low-dispersion
people do not. Measured as between-settlement variance in the gated behaviours,
per people.

**H3 — raiding becomes a fraction, not a flag.** Some proportion of human
settlements raid, and that proportion is a function of dispersion and the
distance from the mean to the gate. The specific prediction: **humans raid at a
rate strictly between goblin's and hobgoblin's**, rather than at 0 or 1.

**The falsification.** If no measured outcome moves once dispersion is authored,
the layer is decorative and should not ship. That is a real possibility worth
naming: if every consumer downstream of these vectors happens to threshold in a
way that swallows the spread, we will have added a parameter nobody reads —
rung 2 of the peoples program's own probe-validity ladder.

**The mutation step** (the program's shared criterion): a test that goes RED if
the dispersion parameter is ignored — i.e. if setting a people's dispersion to
zero does not collapse its between-settlement variance.

## 6. Sequencing

```
    C2-0  THE GENERALIST   human x1              <- in flight, nearly done
    C2t   THE TOLERANCE    dispersion + pop tier <- THIS
    C2a   THE DEEP REALM   the Underdark
    C2b   THE LONG AGE     lifespan curve
    C2c   THE DELVERS      dwarf x5
    C2d   THE RADIATION    elf x6 + LANG-53      <- HARD dependency on C2t
```

**This does not stop The Generalist.** Human-as-a-point is a defensible first
approximation, Tasks 5–7 measure ecological breadth honestly, and the raid
disclosure is a fair interim statement. Finishing it also keeps its census diff
attributable to "a sixth people was added" rather than mixing in a new
psychological layer.

**It must precede C2d** — the radiation has nothing to select on without it.

**There is a case for placing it before C2a and C2b**, stronger than it looked
an hour ago: if dispersion is what "generalist" actually means, then The
Generalist's own headline is incomplete until this lands, and every people
authored in the meantime is authored in a frame this campaign changes. Against
that: it is a new epoch on top of one just paid, and C2a/C2b are already
specified. **Flagged for the owner rather than decided here.**

## 7. Non-goals

- **The individual tier.** D4.
- **Fixing the raid gate's axis mismatch.** D5 — documented and filed.
- **Per-dimension dispersion.** Start with one dispersion per vector; a
  per-dimension spread is a refinement that should be argued from a measured
  need, not assumed at the outset.
- **Widening any vector**, or adding dimensions. Each vector type's doc reserves
  widening to its own campaign; that stands.
- **Culture and language consumers.** They read the species mean and are correct
  to (§2's matrix). This campaign does not touch them.
- **Re-authoring existing peoples' locations.** Only dispersions are added.
  Moving goblin remains its own deferred campaign.

## 8. Flagged for review

1. **An epoch and a census regen.** Population draws consume a new stream label,
   which moves every world with settlements. Carve-out; authorization requested
   at that campaign's close, not here.
2. **A new stream label is a permanent save-format contract.** Declared as a
   `pub const` in the owning crate's `streams` module, published through
   `stream_labels()` into the generated manifest, and never renamed — an epoch
   suffix if it is ever regenerated.
3. **D3 is the campaign's highest-risk decision.** Keying a draw on `EntityId`
   would be deterministic, reproducible, and catastrophically wrong, and it is
   the obvious implementation. The Salt's rule is what catches it; the stable
   semantic key must be chosen and defended explicitly in the plan.
4. **H1 is a byte-identity-adjacent claim** and should be checked by measurement,
   not argued. The mean surviving is what makes this a refinement rather than a
   contradiction of every world that exists today.
5. **The placement question in §6** — before or after C2a/C2b — is the owner's.
6. **The name is reversible.** "Tolerance" reads as engineering (permitted spread
   around a nominal) and continues The Manikin's tailoring lineage, but it also
   reads as social tolerance. Surfaced deliberately, as The Manikin surfaced its
   own name risk.
