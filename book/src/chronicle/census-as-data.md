# Census as Data

**July 2026 · outcome: merged — the laboratory's measurement record is one
canonical census, self-describing at the byte level, with a live/frozen
split and a queryable analysis surface (decisions 0045, 0046)**

## What was attempted

The project's richest dataset was also its least legible. Every study built
a complete world, start to end — star system through tectonics through
climate through species through language through religion — and measured
some slice of it, but five studies had accumulated that all measured
overlapping slices at mismatched seed counts: five hundred here, a thousand
there, two thousand, ten thousand, each a plausible answer to "how big a
sample do we need" asked at a different moment in the project's life. A
committed `rows.csv` named its columns but nothing committed said what any
of them meant, what type they held, or what precision their floats had
survived. Cross-metric questions — which tidally locked worlds also grow
large pantheons — had no home to be asked from but a spreadsheet a human
opened by hand. Longitudinal questions — how a distribution moved when a
physics change landed — had no home at all.

Generation was never the problem. The fragmentation was entirely in the
record: how the same underlying population of worlds got sliced, named, and
described across five overlapping instruments. So the campaign left the
generators alone — one seed still builds one whole world, exactly as it
always has — and rebuilt the record around it: one canonical census, a
manifest that makes every table describe itself, and a query surface a
maintainer can actually sit down and use.

## Consolidation, not deletion

The project's oldest continuously-run census — the five-hundred-seed
instrument CI had been regenerating and diffing on every build since the
tectonic globe first shipped — became **the census**: doubled to a thousand
seeds, walking every metric the registry now knows, sky and land and
society alike. It carries its lineage forward rather than starting fresh; a
chapter written when it was still five hundred seeds and answering to an
older name still describes today's instrument, because the git history
underneath it never broke. A second study, the solo-roster null control
that closed out the prior year's comparative work, stood next to it
unchanged — genuinely different worlds, not a subset of anything, so it
never had cause to fold in.

One study did fold in, and its ending is the part of this campaign that
reads as loss but isn't. The goblinoid-language-family battery — the
thousand-world run that proved out sound change, monophyly, and the
homophony ledger — turned out to be, column for column, an exact projection
of the canonical population once the canonical population caught up to it
in size. Its rows were verified byte-identical against the merged run across
every one of its thousand seeds before anyone touched a calibration that
depended on them. Once verified, the standalone run retired: not deleted,
but **frozen**. Its committed numbers stay exactly as they were the day they
were measured, cited by every chapter and chronicle entry that already
quotes them, and never regenerated again — regenerating a frozen study under
moved physics would falsify the very evidence it exists to preserve. What
keeps a frozen study from becoming an inert fossil is a single recorded
fact: the commit it was produced at. Anyone who wants to know it is still
reproducible needs only check out that commit and run the study again.

## A table that describes itself

Every published study now emits a second file beside its data: a manifest
that types every column, names its source in the registry, records the
histogram buckets its chart was built from, and states outright the
precision floor every float on the page was quantized to before it was
ever written down — the fact that used to live only in a comment days-old
in someone's memory. A content hash binds the manifest to the exact bytes
of the data it describes, so the two files can never quietly drift apart;
if one changes without the other, the mismatch is loud, not silent. Studies
that predate this convention — the frozen ones, the coastline surveys
authored before anyone thought to ask what a schema for a census would even
look like — got a one-time manifest built after the fact from the registry
as it stands today, honestly flagged as a later annotation rather than part
of the original record.

None of this changed a single measured number. It is entirely a statement
*about* the numbers: what they are, what they mean, and how tightly they
are bound to the file sitting next to them.

## A query surface, and a memory that survives a rename

The census used to be read by skimming prose. Now it can be asked
questions. A small, deliberately unglamorous tool outside the simulation's
own build — the same posture the project already takes toward instruments
an author runs but the world itself never links — turns every committed
study into a typed view in an ordinary SQL database, one command away.
Every mounted dataset is validated at the moment it loads: row counts,
content hashes, column order, all checked before a single query can run
against it, so a silently wrong answer is not a failure mode this
instrument has. A unified view stacks every study's columns into one long,
queryable table regardless of which wide file they came from, and a second
path walks the data's entire git history and reassembles it the same way —
so a question like "what did this metric look like just before the
elevation datum landed" is a `WHERE` clause, not an afternoon of checking
out old commits by hand. Because the census carries its lineage across its
one rename, that history runs unbroken through the consolidation itself: a
single seed's biography — its pantheon, its settlements, its languages,
across every commit that ever touched it — reads as one continuous story,
not two disconnected ones stitched at a rename.

One canned query earns a name of its own: it recomputes, straight from the
committed fixture, every calibration constant currently pinned in the test
suite, and checks each one against the value the tests assert. It exists as
a second, independent path from raw data to pinned number — not because the
Rust tests are doubted, but because a duplicate derivation that agrees is
worth more than a single derivation trusted twice. Run it, and the project's
entire pinned-constant provenance is a query result, not an assertion buried
across a dozen files a reader has to already know to look for.

## What follows

The record now has a shape that scales past the working tree it currently
lives in: the manifest and its content hash are already written as the
contract that survives the day the raw rows themselves outgrow git for
object storage, needing no format change when that day arrives, only new
paths in a small file that already knows how to describe what it points at.
Nearer term, the same manifest that types every column also records the
depth at which each metric is measured — sky, land, society — which is
exactly the piece a shallower, wider companion census would need if the
project ever wants ten thousand worlds measured at terrain depth instead of
a thousand measured all the way down. Both are named, neither is built; what
this campaign leaves behind is a record honest enough, and legible enough,
that building them later is a matter of asking the right query and noticing
what it cannot yet answer.
