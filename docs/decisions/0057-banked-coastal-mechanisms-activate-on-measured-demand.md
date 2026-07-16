# 0057. Banked coastal mechanisms activate on measured demand, not by default

**Status:** Accepted (2026-07-15/16) · **Decider:** Nathan (Sculpting tuning
season; decision ledger #7, #9)

In the context of the Sculpting spec preregistering two coastal mechanisms —
wave-cut differential erosion and barrier islands/lagoons — as explicitly
contingent ("spits and barrier lagoons... built only if the shoreline band
needs the help," spec §5/§12), we decided to hold that discipline literally
through the tuning season: **neither mechanism was built until measurement
demonstrated the band it targets could not close without it**, and each
one's tuning constant was chosen from a direct data sweep against the worst
measured seeds rather than an analytically-derived or blindly-guessed value.

**Context.** Wave-cut coastal erosion activated only after iteration 1 (a
spec-faithful fluvial-only fix — restricting incision's slope term to land
neighbors) falsified its own prediction: shoreline-development and
shelf-fraction both moved *further* out of band, and a Nathan-ruled gate (the
single-craton `shelf_land_ratio > 0.05` floor) broke outright. The diagnosis
— the fluvial fix had inadvertently removed an accidental wave-erosion proxy
the old (incorrect) slope term had been supplying — made wave-cut's trigger
condition concrete, not speculative, and it landed in the same commit as the
fluvial fix per the adjudication discipline (ledger #7). Its depth-scale
constant, `wave_cut_m`, was set to **1000** (not the initially authorized 60)
from a probe sweep against the worst single-craton sweep seed, documented in
code as an epoch's planation capacity at ~10²-km cell scale rather than a
literal wave height. Barrier islands and lagoons activated only after a
dedicated shoreline-development diagnostic
(`.superpowers/sdd/shoreline-diagnostic.md`) proved the estimator was not
saturated but specifically rewards single-hex-scale land/ocean alternation —
and iteration 3's relief-frequency fix, though correct on its own merits,
moved the band a bare +0.02 against a +0.5 threshold. Its supply constant,
`barrier_supply_per_cell`, was set to **400.0** from a three-point sweep
(50.0 and 400.0 tied exactly; 5000.0 visibly gated barriers back down),
chosen as the more physically legible of the two non-binding settings with
demonstrated headroom above it, not the cheapest-looking number.

**Consequence.** Both mechanisms are gated on real per-cell physical fields
(exposure and erodibility for wave-cut; per-mouth sediment supply from
passive-margin coasts for barriers), keeping the spec's global-knobs
principle intact — heterogeneity comes from fields, never from a regionally
jittered parameter. Both activations are real, measured, positive movers
(shelf-fraction closed its band on wave-cut's activation; shoreline-
development gained its season's largest single movement, +8.85%, on
barriers') and both are also honestly incomplete: shoreline-development's
band remains open even with every banked mechanism now built (decision
0056), and barrier cells classify through the generic lithology buffer today
rather than a true unconsolidated-sand lithology, a named follow-up rather
than an in-season build.

**See also.** `docs/superpowers/specs/2026-07-14-sculpting-design.md` §5/§12;
[Sculpting chronicle](https://github.com/hornvale/hornvale/blob/main/book/src/chronicle/sculpting.md);
[The Census of Coasts III](https://github.com/hornvale/hornvale/blob/main/book/src/laboratory/census-of-coasts-iii.md)
(the tuning-iteration table and magnitude notes); decision
[0056](0056-sculpting-is-terrain-epoch-v3.md).
