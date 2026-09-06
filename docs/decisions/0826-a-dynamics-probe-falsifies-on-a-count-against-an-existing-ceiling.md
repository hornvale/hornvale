# 0826. A dynamics-rung probe falsifies on a count against an existing ceiling, with a floor and a ceiling, never on flatness

**Status:** Accepted (2026-09-06) · **Decider:** Nathan (G6 / autopilot) ·
**Campaign:** The Hidage · **Relates:**
[0016](0016-studies-preregister-hypotheses.md) (the freeze this states the
shape of), [0093](0093-seed-hunting-is-not-a-test-mechanism.md) (a readout
quantifies a distribution; a ratchet would freeze whatever it found)

In the context of The Staple's standing rule 1 — no dynamics rung starts
without a preregistered Task 0 probe that could falsify it — we decided that
**such a probe states its death criterion as a COUNT with a denominator,
taken against a bar the code already carries, and names TWO dead poles**,
accepting that this costs an authored band (what fraction is "a minority
apex") in exchange for a criterion the mechanism can actually produce.

The rule that was replaced was a dispersion test. D1's brief said: *if
catchment accumulation is spatially flat, every catchment sums alike and this
is a uniform rescale in disguise.* Re-instantiated in hydrology — `flow` is
the drainage algorithm with the gradient flipped, and drainage-basin areas
are heavy-tailed by construction — a flatness statistic **could never fire**
on any field the algorithm is run over. It is the guard-that-cannot-go-red
shape this project has recorded before: a preregistered falsifier that the
mechanism under test cannot produce is not a falsifier, and it reads exactly
like coverage until someone re-derives it. The Hidage's own readout then
confirmed the argument rather than merely asserting it: Gini of accumulation
was 0.352 / 0.262 / 0.446 / 0.328 / 0.320 on the five seeds, above the 0.25
floor on every one.

**What a count buys.** The property a dynamics rung exists to deliver is
DIFFERENTIATION — some places above the ceiling every place sits under today,
and *not all of them*. That is two bars, not one, and both dead poles are
real findings:

- **nothing clears the bar** — the rung cannot make the thing it was
  proposed for;
- **almost everything clears it** — the rung is a uniform rescale in
  disguise, which some existing constant already performs.

The live band is what sits between them. The Hidage's frozen rule (spec §4)
is the exemplar: `c_s == 0` on every seed is NO CITY, `c_s / N_s > 0.5` on
every seed is RESCALE, `1 <= c_s` with `c_s / N_s <= 0.25` on every seed is
LIVES, anything else is MIXED and goes back to the metaplan. It fired
RESCALE (decision [0827](0827-d1-is-struck-worked-land-is-a-uniform-rescale-on-the-growth-field.md)).

**The bar comes from the code, not from the occasion.** `HAMLET_POPULATION_CEILING`
(150) and `LONGHOUSE_POPULATION_FLOOR` (200) already exist and already decide
what a settlement is; a bar authored for the probe would be a number chosen
after the question, which is the metric-chasing preregistration exists to
contain. Only the BAND fractions (0.25 / 0.5) are authored, and they are
authored from the meaning of "a minority apex" before any data is seen.

**Consequence for D2–D6.** Each remaining dynamics rung's probe states its
death criterion as a count with a denominator against a bar the code already
has, and names both dead poles in its own terms. Where the count is on a
quantity that only bounds the observable — capacity bounding population, say
— the probe carries the attainment ratio beside the count and says which pole
the slack makes less certain, as spec §4's own caveat does.

**Accepted cost.** A count discards information a distribution statistic
would keep, so a probe under this rule must also print the characterization
statistics it is NOT deciding on, as predictions that can be visibly wrong
(spec §4.1). Two of The Hidage's four failed, and both failures are findings
the arc carries forward.

**See also.** [The Hidage design](../superpowers/specs/2026-09-06-the-hidage-design.md)
§§3.4, 4, 4.1 and [campaign ledger](../superpowers/ledgers/2026-09-06-the-hidage.md)
#2 (four ideonomy passes, one overturn) and the Task 3 verdict section.
