# The Type Audit

**July 2026 · outcome: complete — every primitive at a public boundary now
carries a verdict**

## What was attempted

A world is built from quantities: masses and distances, days and degrees,
temperatures and fractions. The project had long ago decided that the
coherent ones should not travel as bare `f64`s — a distance is an `Au`, a
mass a `SolarMasses`, a span of time a `StdDays` — because a bare number
carries no memory of what it measures, and two bare numbers of different
kinds look identical right up until one is used where the other belongs. The
house exhibit is a real bug: a sidereal period once stood in for a synodic
one, a confusion a newtype would have made unrepresentable.

But the convention was only a convention. Astronomy honoured it; other
domains, growing under deadline, let bare primitives accumulate at their
public edges — dozens of them in climate, in terrain, in the kernel itself.
Nothing watched the boundary, so the boundary drifted. This campaign builds
the watcher, and in building it, forces the project to say out loud, for
every primitive crossing every public boundary, *why* it is allowed to be
bare — or that it is not.

## What landed

**A checker that reads the source, not the sim.** The tool lives in
`tools/type-audit/`, deliberately outside the Cargo workspace. It uses a Rust
parser to walk every crate's public surface — function parameters and
returns, public struct and enum fields, constants, type aliases — and finds
each place a tracked primitive (`f64`, `u32`, `bool`, `String`, and their
kin) crosses a `pub` boundary. The parser it depends on would be unwelcome in
the simulation's own build graph, and stays out of it: the tool is a
committed, offline instrument whose outputs are checked for freshness, the
same posture the project already takes toward anything an author runs but the
sim never links.

**A verdict at every crossing, written where the code is.** Each boundary
primitive carries one line in its doc comment — `type-audit: bare-ok(ratio)`,
or `waiver(decision-0014)`, or `pending(wave-2)`. The verdict travels with the
code: it is reviewed in the same diff that changes a signature, and it renders
in the documentation, because the reason a number is allowed to be bare *is*
documentation. The checker is bidirectional and default-deny: an untagged
boundary primitive is an error, and so is a tag that no longer points at
anything. Neither the code nor its verdicts can rot silently.

**A rubric, argued into existence.** The audit could not proceed without
answering, class by class, which bare primitives are *permanently* fine. Eight
classes were proposed up front — a dimensionless `ratio`, an honest `count`,
an `index` into a structure whose type already carries the meaning, the
sanctioned raw edge of a newtype, the deliberately-dumb trace-protocol
`envelope`, plain-text `identifier-text`, screen-space `render-internal`, an
unambiguous `flag`. The audit's first pass, over astronomy and the kernel,
found the eight insufficient and forced three more into the open: `prose` for
free-form human text, distinct from a key; `artifact` for a finished produced
output, a rendered chart or a serialized document, distinct from the math that
made it; and `diagnostic-value` for the raw rejected number an error echoes
back. The `constructor-edge` class widened to admit opaque handles and raw
randomness alongside physical newtypes. The eleven-class rubric is now a
ratified decision, extended only by superseding it.

**The measure of the debt.** Six hundred and sixty-eight primitives crossed a
public boundary; every one now carries a verdict. Five hundred and thirty-one
are permanently bare — honest counts, dimensionless ratios, plain-text keys,
finished artifacts. Eleven are waivers, each citing the ratified choice that
sanctions it. And one hundred and twenty-six are `pending`: coherent physical
quantities — meters, days, Kelvin, degrees — still travelling bare, sorted
into remediation waves. The committed report tallies them by class, by crate,
and by wave, so the remaining work is not a vague unease but a number that can
only go down. Not one line of simulation code changed in the counting: the
campaign added the tool, the verdicts, the report, and nothing else. The
conversion of the pending quantities is the work of later campaigns, and each
will watch its wave's tally shrink.
