# The Weir

The Shuttle closed with a warning dressed as a lesson: fixing the callers
leaves the pulpit standing. The proof arrived before the merge cooled. The
very next flamegraph found `Session::start` spending 62 % of a vessel test
running the demography fit **three times** — `predator_pressure`,
`prey_pressure`, and `wild_concentrations` each privately assembling the
component set, sculpting the terrain, deriving the climate, and fitting
the same coexistence stack on identical inputs. All three functions were
written *after* the artifact-taking idiom existed. Nobody chose the slow
path; the API offered it, and the API was taken.

Nathan's directive named the disease rather than the symptom: make
inefficient operations unrepresentable. A weir forces the whole flow
through one measured channel.

## Three layers

**Absence.** Thirteen convenience readouts died — the chorus `_of` family,
`lexicon_of`, `exposure_of`, and the bare pressure/report forms — with
~110 call sites migrated to the artifact-taking twins and, by review's
mechanical check, zero changed assertions, seeds, or expected values
anywhere in the diff. A deleted function cannot be called slowly.

**The lint.** The surviving derivation entry points — `terrain_of`,
`climate_from`, `demography_report_from` — joined the `clippy.toml`
`disallowed-methods` list that already enforces the HashMap and libm bans.
Every sanctioned construction site carries a scoped `#[allow]` with a
one-line justification; the attributes *are* the sanctioned-site list,
greppable. A scratch function calling `terrain_of` without one now fails
the gate with a message citing decision 0092.

**The record.** Decision 0092 states the principle — derivation happens at
named construction sites; readouts take artifacts — and decision 0093
states its testing corollary, Nathan's own formulation: seed-hunting is
not a test mechanism. A desirable but non-deterministic property is a
census question; a behavior of that property is a synthetic question.

## The guard that nearly blinded a stronger guard

The first lint implementation put one crate-level
`#![allow(clippy::disallowed_methods)]` on the composition root.
`disallowed-methods` is a single lint with one switch per scope — so that
attribute also turned off the constitutional platform-libm bans (decision
0041) for eleven thousand lines of the one crate that routes seven
transcendentals through `kernel::math`. The reviewer caught it, proved the
semantics in a throwaway crate, and the fix replaced the blanket with
thirty-one function-scoped allows — then proved the ban live again by
watching a scratch `f64::sin()` fail the gate. The near-miss is written
into 0092's consequences, because the next campaign to add a lint entry
will face the same switch.

## The synthetic arm, and the drift that proved the rule

The soc1 doctrine test had been building **61 full worlds** — 169 seconds
— to find eleven folk flagships for its negative arm; its own panic
message had prescribed the alternative for weeks. The replacement
hand-builds a world through the real registry and `Ledger::commit`,
asserts its preconditions before the gate assertion, and was proven by
mutation: flip the committed cult-form to organized and the test goes red
exactly on the gate. Zero world builds; one inert sculpt kept for
call-site parity, shrunk to 60 ms by a globe-level pin.

Then the plan's own live-smoke constant demonstrated the principle it
shipped under. Seed 56 — folk when the spec was written — had drifted
all-organized under The Wearing's lexicon re-draw before this campaign
merged. The bounded re-find that decision 0093 prescribes ran for the
first time and landed on seed 57's bugbear flagship. The decision record
carries the correction and the moral: the constant is epoch-sensitive, the
mechanism for that is a ten-seed scan, and the sweep does not come back.

## The numbers

On lefford, solo: the grievance test 53.6 s → **23.7 s**; the sky-follows
walk 42.9 s → **34.8 s**; the health null-control 86.1 s → **63.2 s**; the
soc1 gate 169.3 s → **7.5 s**, its whole battery 172 s → **7.9 s**.
`Session::start` performs exactly one sculpt and one fit, verified by
instrumented count, not asserted. Byte-identity held on the total route:
the seed-42 world, all three book lenses, and a scripted possession
transcript reproduce byte-for-byte against the pre-campaign binary, and
the rebaseline drift was exactly the predicted artifact set.

What remains, honestly: about sixteen worldgen `(world)`-shaped readouts
(the almanac/book lens family — `seas_lines`, `firmament_lines`,
`sky_report`, and kin) survive behind sanctioned allows on cold paths.
They are the remaining half of the weir, named in the follow-up register
rather than silently normalized. The next census regen will rewrite two
`schema.json`s whose doc strings were corrected through the backfill
mechanism — expected, not drift. And three renamed tests spend their
silent first-run pass in the Timekeeper's baseline knowingly.
