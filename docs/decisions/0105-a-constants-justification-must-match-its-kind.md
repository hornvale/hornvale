# 0105. A constant's justification must match its kind

**Status:** Accepted (2026-08-05) · **Decider:** Nathan · **Relates:**
[0009](0009-models-author-dice-roll.md),
[0016](0016-studies-preregister-hypotheses.md),
[0097](0097-assert-the-robust-half-measure-the-fragile-half.md),
[0103](0103-suitability-and-headcount-are-distinct-types.md)

In the context of discovering that `carrying_capacity` cited a published
ecological model it did not implement, and that the census "calibration" which
validated it could not have failed, we decided that **every physical constant
declares what *kind* of truth it is, and that its justification must match that
kind** — accepting a classification pass over ~219 constants as follow-on work.

## The two axes

A constant has a **kind** (what makes it true) and a **justification** (how the
code defends it). Crossed, they enumerate the space:

```
                      | cited to a  | measured against | authored | unsourced
                      | source      | Hornvale's census| prior    |
  --------------------+-------------+------------------+----------+-----------
  UNIVERSAL PHYSICS   |  correct    |  RED FLAG (1)    | RED FLAG | RED FLAG
  EARTH'S BIOSPHERE   |  correct    |  CIRCULAR (2)    | honest(3)| gap
  HORNVALE'S CHOICE   |  n/a (4)    |  VALID (5)       | correct  | gap
```

Five readings, and the two numbered pathologies are the ones that bit:

1. **Physics measured internally** is a red flag: it means something derivable was
   sampled instead. Any occupant of that cell should be re-derived.
2. **An Earth-contingent value measured against Hornvale's own census is
   CIRCULAR** — and this is the general form of the specific defect The Keeping
   found. If a constant is true because *Earth's biosphere* is a certain way, the
   only admissible evidence is *Earth data*. Measuring it against Hornvale worlds
   can only establish that Hornvale is self-consistent, never that it resembles
   Earth. `carrying_capacity`'s temperature constants and
   `capacity-by-abs-latitude`'s floor both sit here: both were "calibrated against
   the real biomass-by-latitude gradient" using measurements of Hornvale.
3. **An Earth-contingent value that is *authored* is honest**, not a gap, when no
   Earth data is at hand. `sovereignty_floor`'s *"AUTHORED biological prior (not
   census-calibrated)"* is therefore in the **right** cell — and its explicit
   refusal of census calibration is exactly the disclosure this record wants.
4. **A Hornvale-arbitrary constant cannot be cited**; there is no external source
   for our own choice, so that cell is correctly empty.
5. **A Hornvale-arbitrary constant measured internally is VALID** — the same
   method as (2), opposite verdict, because the row differs. `K_m` and `V_max`
   (The Tilth) are internal scale factors with no Earth counterpart, so measuring
   them against Hornvale's own distributions is the *right* evidence, not circular.

**The rule in one line: internal measurement validates internal choices and
cannot validate external facts.**

## A fourth justification: gauge

Negating "every constant needs provenance" found the one case that legitimately
needs none. `carrying_capacity`'s `BASE: f64 = 1.0` occurs exactly once, as a
multiplicative factor, and any change to it is absorbed downstream by
`SETTLERS_PER_CAPACITY`. It is a **gauge** — its value is unobservable and only
its existence matters. A gauge needs no source, only a note saying it is one, so
that nobody later "calibrates" a quantity that cannot be measured.

## Consequence

- **A wrong citation is worse than none.** An unsourced constant is a gap a reader
  can see; a wrongly-sourced formula *defends itself*, because the citation stops
  the reader checking. That asymmetry is why `carrying_capacity`'s defect survived
  four campaigns with a green calibration, and it is the strongest reason this
  record exists.
- **New constants declare their kind.** One of `physics` / `earth-biosphere` /
  `hornvale-gauge` / `hornvale-choice`, beside the value, with justification of the
  matching form. This binds new constants; it is not a retroactive sweep.
- **The ~219 existing constants** across `kernel/`, `domains/climate/`,
  `domains/demography/` and `domains/terrain/` are not swept here. The known gap is
  the moisture budget (`EVAP`, `OROG_K`, `CONVECTIVE`, `DECAY`, `UPLIFT_SCALE_M`):
  each says what it *is* and none says where its number came from.
- **This does not forbid Earth-contingent constants.** Hornvale's biosphere is
  water-carbon life around Sun-like stars and may stay so; what the record forbids
  is *pretending internal measurement establishes an external fact*.
- **`make ci`-style mechanisation is deliberately not proposed.** Which kind a
  constant is, is a judgment; a checker could enforce that a *declaration exists*
  but never that it is the right one. A tag whose correctness nobody checks would
  reproduce exactly the false-citation failure this record is about.

## See also

`The Tilth` spec §§2.1, 2.2, 5a, 5b, 5c
(`docs/superpowers/specs/2026-08-04-the-tilth-design.md`); The Keeping chronicle
(the false-citation discovery); decision 0016 (preregistration, whose
"frozen only after the measurement confirmed it" standard the circular cell can
satisfy in form while failing in substance).
