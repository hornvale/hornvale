# The Escapement

An escapement is the part of a clock that converts continuous motion into
countable discrete beats — a mainspring's smooth unwinding, broken by an anchor
into a tick a wheel can count and a face can show. Hornvale's own clock had no
such part. Time was stored as a floating-point day, rounded the same way every
other measurement in the world was rounded, and nothing in the tree said that
time was not like every other measurement.

## The argument the campaign opened with

Every serialized float in Hornvale passes through `quantize` at eight
*significant* digits (decision 0033), so that two platforms computing the same
world agree to the last committed byte. For a bounded quantity — an elevation,
a temperature, a population share — eight significant digits buys effectively
constant *absolute* precision, because the magnitude never grows without
bound. Time is the one quantity in the system with no ceiling, and precision
proportional to magnitude means precision that decays as a world ages. Measured
against the real `quantize`, the full spacing between two storable instants
widens with world age:

| world age | spacing between storable instants |
|---|---|
| 100 yr | 86.4 s |
| 2,000 yr | 14.4 min |
| 20,000 yr | 2.4 h |
| 200,000 yr | **24 h** |
| 4.5 Myr | 100 days |

The 200,000-year row is not a hypothetical horizon reached only in an
appendix's arithmetic: `windows/worldgen/src/hazard.rs` constructs a
`WorldTime` at exactly that depth, where two committed facts a full day apart
could collapse to the same stored instant — not day from night, but any two
moments inside the same day.

## The better argument, found by someone who was not looking for it

That table is real, and it is also remote — nobody's world has yet run to
200,000 years and noticed. The argument that actually mattered arrived from
outside, from a campaign (`campaign/the-hand`) that had never heard of this
one and hit the same root cause at world-day **0.0117**, the first hour a
world exists.

`Ledger::commit` quantized a fact's day on the way in, and rounding goes
upward about as often as down. A fact committed at exactly `t` could be stored
as a value strictly *greater* than `t` — verified directly, not asserted:

```
quantize(0.011719999738288106) = 0.01171999999999999952   strictly greater
```

Read the ledger back with the filter `d <= t` that a "most recent fact at or
before now" query naturally writes, and the fact that was just committed fails
its own filter. The Hand's `latest_committed_position` hit exactly this: a
possessed body's location, written one line earlier, read back as "nothing
committed yet" and the body fell to its home coordinates instead. Precision
never had anything to do with that bug. Exactness did — a quantity that is
*supposed* to compare exactly against itself cannot afford a representation
that sometimes disagrees with its own value.

Two arguments, two very different registers — one from the deep end of a
world's life, one from its first hour — and both traced to the same line:
`fact.day = fact.day.map(|d| WorldTime::new(quantize(d.day())))`.

## The design: an instant is an exact tick count

The fix does not make the old encoding more precise. It replaces the encoding.
`WorldTime` is now `{ ticks: i64 }` — an exact count of ticks since genesis,
at **100,000 ticks per standard day** (one tick = 0.864 s). The constant is
not new: it is `windows/vessel`'s own `BASE_TICKS_PER_STD_DAY`, already tested
and already the granularity the action clock runs on. Promoting it means
vessel's clock becomes the kernel's clock, and the lossy bridge that used to
sit between them is deleted rather than moved. A year is exactly 36,525,000
ticks — no repeating fraction — and `i64` ticks stay exactly representable in
`f64` out to about 2.47×10⁸ years, so converting a tick count back to a
standard-day float loses nothing at any horizon the project has ever
constructed.

An exact integer needs no quantization at any magnitude, so `WorldTime`
leaves the quantize contract entirely (decision 0188) — `Ledger::commit`'s
day-rounding block is deleted outright, not merely skipped. Every *other*
quantized surface decision 0033 named — a committed fact's numeric fields, the
lab CSV, the scene/ephemeris `f64` fields — is unchanged; the carve-out is
scoped to the one quantity whose magnitude has no bound, which is exactly why
it was the one quantity whose precision was decaying. Being an exact integer
also means `WorldTime` gets `Ord`, `Eq`, and `Hash` for the first time, which
a `f64`-backed type structurally cannot have. Every place that used to fake an
ordering — a `total_cmp` call paired with an artificial tie-break, four memo
keys built from `.day().to_bits()` — collapses to the value itself, and
`WorldTime` becomes a legal `BTreeMap` key.

**The lattice reaches the compute path, and that is a deliberate, ratified
departure from decision 0033's letter, not an oversight.** `hazard.rs` draws
an event's day continuously, filters it continuously against a window, and
then *stores* the survivor as a `WorldTime` — which now rounds to the nearest
tick. Read that stored day back out and compare it against the same window a
second time, and a raw continuous draw is being checked against a
tick-rounded bound; the window's own half-open property can flip depending on
which way the rounding went. Decision 0033 says quantization never touches
the compute path, in as many words, and this is quantization touching the
compute path. The ratified answer (decision 0186) is that the lattice is not
a quantization *of* the time domain laid on top of it — the lattice *is* the
time domain, and any code that draws a continuous time converts to ticks once,
at the draw, and compares ticks exactly from then on. This is a determinism
improvement rather than merely an accepted cost: which side of a boundary an
event falls on now depends on an exact integer comparison instead of an `f64`
ULP accident, the same on every host, forever.

## Repricing itself: precision becomes constant, not uniformly better

The table above argues entirely from the deep end, and taken alone it
overstates the case. Comparing the old lattice's full step against a tick's
full step at the same world-day tells a different story near genesis:

| world-day | old (8 sig-digits) | new (1 tick) | finer |
|---|---|---|---|
| 7.8 | 1.0×10⁻⁷ day | 1.0×10⁻⁵ day | **old, by 100×** |
| 50 | 1.0×10⁻⁶ day | 1.0×10⁻⁵ day | **old, by 10×** |
| 100 | 1.0×10⁻⁵ day | 1.0×10⁻⁵ day | coincide |
| 365.25 | 1.0×10⁻⁵ day | 1.0×10⁻⁵ day | coincide |
| 3,652.5 | 1.0×10⁻⁴ day | 1.0×10⁻⁵ day | new, by 10× |
| 36,525 | 1.0×10⁻³ day | 1.0×10⁻⁵ day | new, by 100× |

The old lattice's step is exactly `10^(⌊log₁₀ day⌋ − 7)`, which equals a tick
for the entire decade `[100, 1000)` and is *finer* than a tick for the whole
of a world's first hundred days. So the honest description of this campaign
is not "time got more precise" — below roughly world-day 100, it got less
precise, by up to two orders of magnitude. What it bought instead is a
resolution that no longer *decays*: constant across the world's whole life
rather than best at the start and worst at the depth where the read-back bug
and the hazard boundary actually live. And the sub-tick precision the old
encoding briefly offered was never usable — a tick, 0.864 s, is already the
finest granularity anything in the simulation resolves, since it is the
action clock's own quantum. Nothing in the sim could have read a value finer
than that even when the encoding was capable of storing one.

## What the flip found looking for something else

The migration itself surfaced findings the spec did not anticipate, each
worth recording because each is a small lesson in what a verified fact does
and does not license you to assume next.

**A domain the plan never assigned to anyone.** The migration staged itself in
two phases — a rename that added the tick-shaped surface as an accessor over
the still-`f64` field, then a flip of the field itself — precisely so that
every commit in between still compiled under an unscoped workspace `clippy`.
When the flip's shim-deletion gate finally ran, it surfaced twenty-two
`WorldTime::new` call sites inside `domains/astronomy/src/provider.rs` that no
task had ever ported. An earlier review had reported "survivors only in
astronomy — out of scope" and that was read, uncritically, as "owned
elsewhere." There was no elsewhere. The migration shims had hidden the gap for
four commits, and the lesson generalizes past this campaign: a reviewer
reporting a survivor is reporting a fact, not a disposition, and "out of
scope" answers a question nobody asked about who owns the fix.

**A frozen fixture that had to be re-encoded, not re-derived.** One committed
world snapshot, with a single commit in its whole history and nothing that
regenerates it, held 714 day values that needed to move onto the new lattice.
The distinction that mattered was between *re-encoding* each stored value
(`round(day × 100,000)`, preserving exactly what the frozen file already said)
and *re-deriving* the fixture by regenerating a fresh world under today's
code, which would have quietly replaced a historical pin with a new one that
merely looked frozen. A frozen artifact's entire value is recording what the
world used to emit; the fixture was re-encoded.

**Two committed artifacts moved, and the difference between reporting a
mechanism and establishing one turned out to matter more than the moved
bytes.** The gallery's seed-42 walk fixture committed one additional fact
after the flip — 190 where it had committed 189 — and the first explanation
offered was a shift at a window boundary the spec's own §2.1 had already
ratified as an accepted consequence of the lattice reaching the compute path.
The explanation was plausible, cited the ratified clause, and was checked by
arithmetic before it was allowed to stand: reconstructing the window's two
ends from the seed's committed day length showed both rounding to the
*identical* tick, across the full uncertainty band the old eight-digit
encoding could have produced. The named mechanism moved nothing.

An instrumented run — dumping every fact a homeostatic drive tick commits on
each side of the flip — found the real cause, and it defeated the second
guess too (a `d <= t` read-back mismatch, the same class of bug The Hand had
found independently). Neither position-path explanation moved: `agent-at`
facts were identical on both sides. What moved was a *feedback loop*: a
drive's committed day now rounds onto the tick lattice at the moment it is
written rather than at eight significant digits, the homeostatic system reads
its own committed days back as `last_drank`/`last_rested`/`last_ate`, and each
read-modify-write cycle compounds the rounding — half a tick at first, then a
few dozen, then thousands, until by day 38 the accumulated drift is 0.41 days
and a sixth rested event fits inside a window that used to hold only five.
Both of the guesses that preceded this measurement were specific and
checkable, and both were wrong; the magnitude of the true divergence, worked
out independently by two people reasoning from different starting points,
converged on the same order as the campaign's own headline finding — The Hand's
upward-rounding read-back defect — without either of them being the actual
mechanism at work. Volunteering a plausible cause is not the same act as
establishing one, and the gap between
them was a specific, checkable number that nobody had checked until asked
twice.

**The save format breaks, deliberately, and says so loudly.** A world file
written before the flip does not deserialize afterward — `serde_json`'s `i64`
visitor refuses a JSON number token carrying a decimal point outright, so the
first committed fact with a day produces an explicit type error rather than a
silent misread. No float-tolerant bridge was added to soften that. A bridge
that accepted either encoding would have kept the lossy representation this
campaign exists to remove, silently rounding every old file's timestamps on
load with no signal that anything had happened — including files carrying the
exact upward-rounding defect this campaign fixes. A world is a seed plus a
ledger; every world this project has produced is re-derivable byte for byte
from its seed and pins, so losing the ability to load an old file costs
nothing that losing the ability to regenerate the identical world would not
already cost. Every load path in the tree was traced rather than assumed
sound, and each one propagates the error to a non-zero exit with the
`serde_json` message intact — there is no silent-fallback branch anywhere for
this decision to worry about.

## The two independent defects the flip made free to fix

Every float-to-integer time boundary in the tree mishandled negative time,
and negative days are constitutionally legal — a community founder can be
born before the history record begins (decision 0126). `local_day`'s
`local as u64` cast **saturated**: every negative local day collapsed to
zero, silently, with no test anywhere exercising the branch. Verified rather
than assumed: `local = -5.3` cast to `0`, not `-6`. Tracing every caller found
the defect was **latent, not live** — the sole `WorldTime → StdDays` funnel
(`GeneratedSky::t`) clamps to genesis first, so nothing reachable today could
hand `local_day` a negative value — but the fix was nearly free once the
surrounding code was being rewritten anyway, and it is now a `div_euclid`/
`rem_euclid` pair with the first test in the tree that passes a negative
`StdDays` to any calendar method.

That same clamp — `time.day().max(0.0)`, previously justified only by a code
comment — is now a decision rather than an inherited assumption (decision
0187). A pre-genesis sky query answers as the sky at genesis, because the
public `StdDays` type cannot receive a negative value to answer honestly in
the first place; the choice was between a total function with no case to
defend and a case the type system had already foreclosed one layer up. The
tension worth stating plainly: `Calendar::local_day`, fixed the same commit,
now answers correctly for negative time on the strength of the very same
precedent (decision 0126) that the sky's clamp seems to decline. The
resolution is structural, not a disagreement about what a pre-genesis moment
means — the calendar's negative path is reachable in principle and the sky's
is not, because nothing outside `domains/astronomy` can construct a negative
`StdDays` to ask the sky the question at all.

## The epoch, and what stays put

Every internal representation of time moves: `World`/`Ledger` JSON, the
committed almanacs and session fixtures, the lab study CSVs, the Domesday
survey, the census goldens. Nothing about generation itself moves — no
seed-derivation label changed, no draw was added, and stream consumption
order is exactly what it was before, which is the load-bearing claim the
pin-isolation property tests exist to hold. A controller re-parse of the
seed-42 world on both sides of the flip found 12,534 facts on each side, zero
non-day differences, and every one of the 12,502 dated facts satisfying
`old == quantize(new / 100,000)` — the same instants, some now up to 495
ticks (7 minutes 8 seconds) better resolved than the old encoding could have
placed them.

The one thing that does *not* move is the cross-repo contract.
`scene/eclipses/v1`'s `day`, `from_day`, and `until_day` fields stay exactly
as they were — bare `f64` standard days, still quantized to eight significant
digits, because an external consumer of that wire format gets the same
cross-platform stability every other quantized float on it already has. What
was added instead is a sibling field beside each one — `day_ticks`,
`from_day_ticks`, `until_day_ticks` — carrying the same instant exactly, for
any client able to read a bare integer. Scene schemas are
additive-or-versioned-only; adding a field beside an unchanged one costs
nothing and breaks nothing, where bumping to `v2` for a precision fix most
consumers do not need would have broken every one of them for free.

## What this cost, precisely

The physical magnitude of everything in this campaign is nothing — 0.864
seconds of jitter on events drawn across ten thousand to two hundred thousand
years, in a simulation whose calendar does not resolve finer than that
already. What justified four decision records and an internal save-format
epoch was never the magnitude. It was that a rule written as constitutional
(decision 0033: quantize never touches the compute path) turned out to have
an unstated exception the moment a truly unbounded quantity tried to live
inside it, and that the same defect had already cost a different campaign a
correctness bug in a possessed character's very first hour of existence,
independently, before anyone had connected the two. An escapement does not
make a clock's spring wind any more precisely. It makes the clock's face
agree with itself, at every hour it will ever show.
