# 0466. Authored evidence is read through its own schema, not the live registry

**Status:** Accepted (2026-08-30) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0028](0028-the-bare-ok-rubric.md)

In the context of committed laboratory fixtures that were read through the
**live** metric registry with an exact header match, facing a deadlock in which
registering any new census metric made every committed fixture in the repository
unreadable at once, we decided that **a committed measurement fixture is read
through the `schema.json` beside it, resolving each column by name at that
column's position in the fixture's own header**, accepting that "is this fixture
current?" now needs a different, deliberately strict reader.

## Context

A committed `rows.csv` is a historical measurement record. Under the old
arrangement its readability depended on a registry that only ever grows, so
adding one column to the registry invalidated every past measurement
simultaneously. That is not a stale-fixture problem; it is a category error
about what the bytes on disk are.

It deadlocks in practice, not merely in principle. Census goldens and the
injection arms can only be re-authored on the canonical box at a pushed
reference, and the commit gate would not admit the commit that registers the
metric until they had been. The observed cost: it was hit twice in one week, and
the first campaign broke it by bypassing the commit hook for one commit with
Nathan's explicit authorization. This campaign was told to fix the cause
instead.

The seam already existed. Every generated study directory carries a
`schema.json` beside its `rows.csv`, emitted from the same result in the same
column order, precisely so this is possible — and the Domesday reader has read a
census that way, registry-independently, for some time.

## What was decided

`load_authored` resolves a fixture's columns **by name** against the registry and
then reads each at the position the **fixture's** header gives it, returning a
study narrowed to exactly those columns. Its verdict is three-valued, the same
ratchet shape as the trope check, the type audit's waivers, and seam-guard:

| verdict | condition | result |
| --- | --- | --- |
| `CURRENT` | fixture columns equal live study columns | green, silent |
| `PREDATES` | live has columns the fixture lacks; every shared column keeps its kind and relative order | **green, printed loudly**, naming the columns |
| `DIVERGED` | a fixture column is gone from the registry, a shared column's kind changed, or the shared columns are reordered | red — re-author |

`PREDATES` is green because a purely additive registry change cannot invalidate
a past measurement. It prints on every read so nobody mistakes silence for
currency. A gate that reddened on the mere existence of an older fixture would
be red from the day it shipped.

## The trap that makes the naive fix wrong

The row parser indexes **positionally**, by the study's metric list. Merely
relaxing the header check would make a fixture missing one column read every
subsequent field into the wrong metric — and the trailing refusal column as a
metric value — silently, with plausible output. The decision is therefore
"resolve from the fixture's own schema", never "tolerate a header mismatch".
The same trap was live in the census sentinel's own comparison loop, which
zipped live metric names against committed values positionally; it resolves by
name now.

## Consequence

**What stays strict, deliberately.** The exact-match reader is unchanged and
still refuses a shifted schema, because *is this fixture current?* is a real
question with a right answer and four tests ask it. The authored reader is a
**sibling, never a replacement**, and refuses to diff two fixtures whose column
lists differ.

**What is given up.** A `PREDATES` fixture is read successfully while being out
of date, and the only thing saying so is a printed line that a test harness
shows on failure. The loud print is the whole mitigation; there is no gate on
how far behind a fixture may fall.

**What it buys.** Registering a census metric is a local act again. The
schema-backfill path also moves to the authored reader, which is what lets the
whole artifact-regeneration script survive a metric registration instead of
aborting and silently skipping everything downstream of it.

## See also

- `windows/lab/src/authored.rs`; `windows/lab/CLAUDE.md` ("Registering a metric
  is not a local act", corrected in place).
- The Winze chronicle and retrospective; spec
  `docs/superpowers/specs/2026-08-19-the-winze-design.md`.
