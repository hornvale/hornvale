# Common Ground

**July 2026 · outcome: complete — the same seed yields the same bytes on
every platform**

## What was attempted

Determinism was the project's founding promise: a world is a seed, and the
same seed must yield the same world, byte for byte. The tests asserted it,
and CI enforced it by regenerating every committed artifact and diffing.
For months the promise held — and then, quietly, it stopped. CI went red
and stayed red: the seed-42 world no longer matched its own committed
fixture. Nothing in the simulation had changed. What had changed was
*where* the fixture was frozen.

The promise, it turned out, had always carried an unstated clause. "Same
seed → same bytes" was true on a single machine. But a world is built from
transcendental arithmetic — the sines and cosines of orbital mechanics, the
power laws of terrain — and Rust computes those by calling out to the
operating system's math library. Apple's library and Linux's do not agree
to the last bit. They round the sixteenth digit differently, and that
difference, invisible to any physical instrument, is loud to a byte
comparison. The fixtures were frozen on a Mac; CI runs on Linux. Every
committed golden was a photograph of one platform's rounding, checked
against another's.

## What landed

**A single primitive, at the boundary.** The fix is one function,
`quantize`, that rounds a float to eight significant digits — but rounds it
using Rust's own number-formatting, which is written in pure Rust and is
identical on every platform, rather than the operating system's. Two values
that differ only in their platform-specific noise round to the same number.
The noise is erased before it can reach a committed byte.

The subtle discipline is *where* it runs. `quantize` touches a number only
as it is written down — a fact entering the ledger, a metric entering a
census row, an elevation entering a scene file. It never touches a number
in flight. The noise fields, the sculpting, the orbital integrations all
run at the machine's full precision; what is canonicalized is only the
record they leave behind. The distinction matters because the alternative —
rounding values mid-computation and feeding them back — would be a different
and worse thing.

**Four boundaries, measured not guessed.** Before choosing where to
intervene, the drift surface was measured directly: every committed
artifact was searched for high-precision floats. The result was decisive.
The almanacs, the maps, the reference tables — all coarse-formatted or
textual — carried none, and were left untouched. Four surfaces carried the
divergence: the world ledger, the two lab censuses (tens of thousands of
full-precision numbers), and the scene and ephemeris exports. Each got the
same primitive at its serialization seam. Every committed world, census,
scene, and ephemeris was then regenerated: a deliberate save-format epoch,
recorded rather than hidden.

## The Lorenz clause

A save format that rounds is a lossy save format, and for a determinism-first
world that should provoke suspicion. If a future system ever *resumed* a
chaotic simulation from these eight-digit numbers, the discarded digits
would not stay discarded: sensitive dependence on initial conditions —
Lorenz's butterfly — would amplify them into a different history. The
project is safe from this, but for a specific reason worth stating aloud.
Reloading a world does not resume from its stored floats; it re-derives
every provider from the world's seed, which is an integer, stored without
loss. The quantized numbers are a *record* of what was computed, never the
*state* a simulation runs forward from. The lossless path back to full
precision is, and must remain, the seed. The rule this campaign banks for
its successors: never seed a chaotic integrator from a quantized fact.

## Where it leaves the project

The founding promise is now honestly stated and actually true: same seed,
same pins, same bytes — on a Mac, on a Linux runner, on whatever comes next.
The guarantee is no longer a photograph of one machine. It is a property of
the world.
