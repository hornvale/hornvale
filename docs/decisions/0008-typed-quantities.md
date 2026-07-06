# 0008. Typed quantities at API boundaries

**Status:** Accepted (2026-07-06) · **Decider:** Nathan

In the context of physical simulation with many coherent units (solar masses,
AU, megameters, light-years, standard vs local days, degrees), facing the class
of bugs where a bare `f64` in the wrong unit crosses an API boundary
undetected, we decided that **coherent physical quantities crossing API
boundaries are hand-rolled newtypes with validating constructors and named
conversions; dimensionless ratios stay bare `f64`**, accepting the boilerplate
and declining any dimensional-analysis crate (per [0004](0004-no-new-dependencies.md)).

**Context.** The Campaign 2 retrofit proved its worth twice: a golden test
showed the newtype migration was behavior-free, and the final review's fuzzing
found bugs precisely in the places where raw numbers still leaked. Where the
types ruled, those bugs could not exist.

**Consequence.** `SolarMasses`, `Au`, `Mm`/`Megameters`, `LightYears`,
`StdDays`, `LocalDays`, `Degrees`, and similar are validating newtypes.
Dimensionless ratios (brightness, obliquity fractions) remain bare. The
approach applies beyond Hornvale, in and out of this crate.

**See also.** Campaign 2 spec design principle 5; `CLAUDE.md` "Typed
quantities".
