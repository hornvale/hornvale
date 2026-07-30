# The Pyx

Since 1282 the Royal Mint has submitted a random sample of its coins to the
Trial of the Pyx: an assay, before an independent jury, against a reference
standard. The trial exists not because mints are dishonest but because a mint
that never checks its own output against a standard will not notice the day it
stops matching.

Hornvale has authored its census goldens on one machine since decision 0063,
and had never assayed them. On 30 July the machine was tried against three
standards — its own past, a second build of itself, and a different
architecture. It passed all three.

## The ruling nobody had re-measured

0063 ruled a single canonical platform on the strength of a single
observation. On 19 July, lefford and an AWS box disagreed by one unit on
roughly a tenth of a percent of census values — `divergence-magnitude`-class
discrete counts, the sort settled by a comparison in the compute path, where
quantization at the emit boundary arrives too late to help. Quantization
absorbs a last-bit float difference when the number is serialized; it cannot
un-flip an integer that a `>` has already decided.

The observation was recorded. The *mechanism* never was.

Eight days after that measurement, on 27 July, the workspace raised its x86
codegen baseline to `x86-64-v2`. The change was made for speed, and the
commit's own notes explain why it was worth 4.6% of a census: on the default
baseline LLVM cannot emit the `roundsd` instruction, so every `f64::floor()`
compiled to a *library call* — a bare `floor` symbol that a profiler charged
4.62% of the run's own time — and `Fbm::sample`, the innermost primitive of
all world generation, floors twice per sample. Raising the baseline turned
those calls into single instructions.

So between the divergence measurement and the present, the hottest
floating-point path in the project stopped calling into a shared library that
differs per host and started executing a fixed instruction. Nobody went back
to look.

## Repeatability is not reproducibility

Underneath sat something larger than one stale measurement.

Metrology distinguishes two things a laboratory can claim. **Repeatability**
is the same apparatus, the same operator, a short interval apart.
**Reproducibility** is a *different* apparatus reaching the same value. They
are not degrees of the same virtue; they are different claims, and the second
does not follow from the first.

Every determinism guarantee Hornvale enforces is a repeatability guarantee.
The drift check regenerates on lefford and compares against a golden authored
on lefford. That is a real and valuable check — it catches a change in the
code. It is structurally incapable of noticing that the *machine* has moved,
because the machine is on both sides of the comparison.

Which left the project in the position the SI system occupied until 2019.
Le Grand K, the platinum-iridium cylinder in Sèvres, *was* the kilogram by
definition, so it could not be wrong — and yet its official copies drifted
against it by tens of micrograms over a century, and the only reason anyone
knew was that the copies existed and were periodically compared. Hornvale had
the artifact and no copies.

## Three assays

The audit was built cheapest-first, each layer able to make the next
unnecessary.

The first re-ran the full census on lefford at the exact commit whose goldens
it had authored eleven days earlier, and diffed. This is the control that had
never been run, and the only one whose failure would have outranked the
campaign entirely: had it come back dirty, every drift check in the repository
would have been comparing against an artifact its own author could no longer
reproduce.

The second asked a question the project had never asked at all — whether the
binary is a function of the source. Two clean release builds of the same
commit, in different directories, hashed.

The third compared platforms: forty worlds, every registered metric, run on
lefford and on an Apple-silicon Mac. The window was centred on seed 681,
because 0063 had named it — its `divergence-magnitude-hobgoblin` read 5 on
lefford and 6 in the AWS golden, and that single integer is the most specific
surviving trace of the divergence.

## The results

The census reproduced exactly. Not merely the thousand-row table — the whole
generated tree: 520 charts, both study summaries, the schemas, the gallery
renders, the type-audit report. Zero bytes different, in fifteen minutes of
recomputation, eleven days on.

The two binaries were identical: `fb8c368c…` twice, not one byte apart.

The two platforms agreed completely. Both probe tables hash to `ddb999ff…`
— x86_64 Linux against aarch64 Darwin, forty worlds, every metric, no
exceptions. And seed 681 reads `divergence-magnitude-hobgoblin = 5` on
lefford, on the Mac, and in the committed golden. The one value AWS disputed
is now agreed on by two architectures and two operating systems.

## The prediction that missed

The campaign froze its predictions before running anything, and one of them
was wrong.

The binary comparison was expected to *fail*, benignly. Rust embeds absolute
source paths in its output unless told otherwise, and two builds in two
directories should therefore differ in a way that says nothing about
correctness — a confirmed nuisance, closing off binary hashing as a useful
tool and leaving the expensive output comparison as the only real instrument.

Instead the hashes matched, and the reason is a default nobody had chosen
deliberately. The workspace declares no release profile of its own, so cargo's
default applies, and that default builds release binaries without debug
information — and a binary with no debug information carries no source paths
to differ over.

The consequence is larger than the layer it came from. Qualifying a new
machine no longer requires generating a single world: build the binary there,
compare one hash, and only if it matches spend a census confirming it. An
assay that was budgeted in tens of minutes costs seconds.

It comes with a condition, and the condition is now load-bearing. The property
holds exactly while release builds carry no debug information. The workspace's
*profiling* profile does enable it, and is correspondingly not reproducible
across directories. Anyone who adds debug information to the release profile
would silently revoke the guarantee, and nothing at present would notice.

## What was and was not established

The divergence 0063 measured does not reproduce between two current, pinned
hosts spanning two architectures and two operating systems. That is the
finding, at exactly that width.

It is not a finding about the Kubernetes node that prompted the audit; that
machine was never run. It does not license a second authoring host, and the
enforcement that keeps goldens coming from one box is deliberately untouched.
What changed is that the single-platform rule now rests on a premise that has
been re-measured rather than inherited — and the premise has moved beneath it.

Nor was the mechanism found. The codegen story is consistent with every
result and remains unproven; confirming it would mean rebuilding without the
raised baseline and showing the disagreement return. IEEE-754 floor is exactly
representable, so a conforming library should not have diverged in the first
place. Either the cause lies elsewhere, or one of those two machines was doing
something it should not have been. The observation of 19 July stands as an
observation, and its cause is still unknown.

A mint that assays itself and passes has not proven its dies will never wear.
It has established that today they have not, and that it owns an instrument
capable of telling it when they do.
