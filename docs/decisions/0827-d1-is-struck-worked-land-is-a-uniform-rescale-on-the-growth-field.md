# 0827. D1 is struck: on the bake's growth field, worked land is a uniform rescale in disguise

**Status:** Accepted (2026-09-06) · **Decider:** Nathan (G6 / autopilot) ·
**Campaign:** The Hidage · **Relates:**
[0826](0826-a-dynamics-probe-falsifies-on-a-count-against-an-existing-ceiling.md)
(the criterion this verdict is read off),
[0145](0145-one-community-per-place-not-per-cell.md) (one community per
place — the settlement unit `N_s` counts),
[0102](0102-one-per-cell-was-an-index-artifact.md) (the one-per-cell rule
0145 re-keyed; the metaplan cites both at §1.3)

In the context of The Staple's dynamics arc — D1, *a settlement has worked
land* — we decided that **D1 as specified is struck**: on the deep-history
bake's own growth field, growing a community toward the summed capacity of
its catchment instead of the capacity of its own vertex would lift every
settlement over the hamlet ceiling, which is a uniform rescale that
`SETTLERS_PER_CAPACITY` already performs.

**The readout** (`20f48585e`, `hidage_probe`, five worlds at default pins,
seeds 42 / 7 / 13 / 100 / 1234). For each settling people, the probe rebuilds
the bake's present-era growth field `K_p(v)`, runs `hornvale_demography::flow`
over it, and takes the top-`N_p` attractors by accumulation — "if this world's
current settlement count sat at its largest catchments, what would they be
worth". `c_s` counts those clearing `HAMLET_POPULATION_CEILING` (150):

```
  seed    c_s / N_s   ratio   c200_s / N_s   median attainment a
  ----    ---------   -----   ------------   -------------------
  42      390 / 390   1.00    388 / 390      0.66
  7       250 / 250   1.00    250 / 250      0.82
  13      262 / 262   1.00    262 / 262      0.65
  100      60 /  60   1.00     60 /  60      0.40
  1234     44 /  44   1.00      44 /  44     0.11
```

Every top-N catchment on every seed clears the ceiling, and all but two also
clear `LONGHOUSE_POPULATION_FLOOR` (200). The rule's RESCALE clause
(`c_s / N_s > 0.5` on every seed) fires at its extreme value.

**The attainment caveat, worked rather than waved.** The bar is on summed
*capacity*; a community's realized peak sits at or below it, so RESCALE is
the less certain pole and spec §4 says MIXED applies if RESCALE fires with
median attainment below 0.5. Two seeds are below (100 at 0.40, 1234 at 0.11),
so the caveat is live. Dividing the 150 bar by each seed's own median
attainment gives corrected bars of 227 / 183 / 231 / 375 / 1364, and the
count still clearing them is `>= 378/390`, `250/250`, `262/262`, `60/60`,
`35/44` — a majority on every seed, tightest at 35 of 44. The caveat is a
correction to the bar, not a fifth clause, and the corrected bar still fires.
The verdict is RESCALE.

**Disclosed, because the step is an interpretation.** This campaign's
implementation plan (Task 3 Step 1) froze the clause more strongly than the
spec did: *"if the verdict is RESCALE and median `a < 0.5`, the branch is
MIXED and the entry says why."* Read literally, seeds 100 and 1234 put this
campaign on MIXED — no strike, the table back to the metaplan — rather than on
RESCALE. It was read as a bar correction because every other §4 clause is
quantified "on every seed" and the caveat's preamble is a statement about what
the bar measures, and the corrected bar fires on every seed; but that is a
post-unblinding reading of preregistered text by the party it favours, and it
was put to Nathan with the MIXED alternative named rather than absorbed.
**Ratified at G6.**

**The four characterizations** (spec §4.1), stated before the probe was
written so being wrong would be visible:

- **S1 (Gini of accumulation) ≥ 0.25 — HELD** (0.352, 0.262, 0.446, 0.328,
  0.320). The hydrology argument behind 0826 stands; flatness was never a
  live falsifier.
- **attr/N < 0.5 — HELD** (0.13, 0.18, 0.17, 0.40, 0.39). Most bake sites are
  not attractors of their own people's field.
- **S3 (Spearman of accumulation against vertex capacity) ≥ 0.7 — FAILED** on
  3 of 5 (0.398, 0.614, 0.542, 0.754, 0.813). D1 would not only resize
  settlements, it would RE-ORDER them: the biggest vertex is not reliably the
  biggest basin, and D5's "comparative notability" inherits a different
  question than the metaplan planned for.
- **median attainment in [0.5, 1.0] — FAILED** on 2 of 5 (0.40, 0.11). On the
  two small worlds the binding ceiling today is not capacity at all — a D6
  observation, recorded and not acted on.

**What this does NOT decide, and the record is explicit about it.** The
catchment field *does* carry an apex: max-over-median accumulation is 2.9–11.2
and the Gini is 0.26–0.45, so spec §4's RESCALE response text ("it makes no
apex") is wrong about the field. What dies is D1 **as specified** — wiring the
catchment in as the growth ceiling *at today's scale*. Whether a RESCALED
catchment, normalised so the walkable settlement band holds, would produce a
differentiated apex is metaplan §6's open `SETTLERS_PER_CAPACITY` question. It
is not a rung, this record does not answer it, and it moves into D2's probe
brief as an open item rather than being lost.

**Consequences.** The Staple's dynamics arc re-plans from **D2** (split people
from subsistence; add a voluntary exchange beside the coercive one), whose own
Task 0 probe is the next campaign under standing rule 1. The metaplan's §4 D1
entry carries the per-seed table and this verdict; §2.1's dependency diagram
and §3 consequence 3 — which argued D1 must precede D2 — are annotated rather
than rewritten, because the argument they make (climate reaches capacity but
not a city; one mechanism, two payoffs) is still the reason the link is
wanted, and only the proposed link is struck.

**Accepted cost.** No mechanism was built and no epoch taken, so the campaign
ships a null. The catchment code (`domains/demography/src/flow.rs`,
`condense.rs`) stays exactly as it is — a Lab instrument, still tested, read
by nothing on the production path (The Hidage design §2.3, correcting the
metaplan's "genesis uses it").

**See also.** [The Hidage design](../superpowers/specs/2026-09-06-the-hidage-design.md)
§§2, 3.4, 4, 4.1, [chronicle](../../book/src/chronicle/the-hidage.md),
[campaign ledger](../superpowers/ledgers/2026-09-06-the-hidage.md) #1, #9 and
the Task 1 readout.
