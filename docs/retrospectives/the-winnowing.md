# The Winnowing — retrospective

Process lessons, not product. A three-task campaign that let a caller choose
which per-tile layers cross the wire, hit its preregistered byte number, and
was saved twice by an instruction to distrust the plan.

## A plan's transcription of another repository is a claim, and it was wrong twice

The specification asserted which per-tile layers the Orrery reads. It was
derived by reading that repository's `parseTiles` once, by eye, and it was
wrong: it counted `features` (document metadata, always emitted, not a
selectable layer) and `water` (not referenced anywhere in the client's `src/`)
among the ten layers the client extracts. The real figure is eight, and eleven
layers go unread rather than nine.

The plan's own `ALL_NAMES` list was wrong too — not in content but in *order*,
placing `moisture` before `current_east`/`current_north` where the source
declares the currents first. Same nineteen names, same spellings, wrong
sequence, and the sequence is the contract in a positional flag vector.

Neither error caused a defect, and the reason is a single sentence that
appeared in both task briefs: *verify this list against the source rather than
trusting the plan*, with the file to check named. Both implementers did, and
both reported the correction back. The plan's self-review had even flagged
these two lists as transcribed by eye — it knew they were the weak spots and
said so.

What makes this uncomfortable is the direction of the field-list error. It was
in the campaign's favour: the predicted saving was 52% and the measured one is
53.7%, so a bigger-than-expected number came out and nothing looked wrong.
There was no red test, no surprising output, no moment where the discrepancy
announced itself. Had the instruction not been there, the campaign would have
shipped a chronicle stating a client's read set incorrectly and celebrating the
extra bytes as luck.

**Lesson:** when a plan transcribes a fact from outside the repository — a
client's field list, another crate's declaration order, an external schema —
mark it as transcribed and instruct the implementer to re-derive it from the
source. It costs one grep, and it is the only check that exists: nothing in a
gate can see that a *cross-repository* claim is false.

## A pin dropped from a plan is invisible; a pin dropped from a plan's self-review is worse

The specification's testing story had three pins. The plan shipped two — an
equality test between the full projection and serde's derive, and a per-layer
independence test — and silently dropped the third, a committed golden of a
representative subset. The plan's self-review then mapped the spec's section to
"the two in-memory tests plus the existing full golden" and declared coverage
complete. The omission was not noticed because it had been paved over by a
sentence claiming it had not happened.

The consequence was found by the Task 1 reviewer, who gutted `TileFields::
contains` to return `true` unconditionally — defeating the campaign's entire
purpose — and watched both tests stay green. The equality test only exercises
`all()`, where over-emission is correct. The independence test only inspects
emitted bytes, and its locator helper *panics* on absence, so it structurally
could not observe a layer that should not have been present. Two genuine,
non-tautological tests, one of them explicitly mutation-checked by its
implementer, and between them nothing tested that the projection projects.

The pin the spec asked for is exactly the pin that catches it: an absolute
golden of a subset is the only artifact that can be red when a *superset* is
emitted.

**Lesson:** a plan's self-review is the wrong instrument for checking the plan
against the spec, because it is written by the party that would have to admit
the gap. Check spec-to-plan coverage by enumerating the spec's pins as a list
and pointing each at a plan step by name — an unpointed pin is then a missing
row rather than a paragraph nobody re-read. And when a spec asks for both a
property test and a golden, that is usually not redundancy: the property test
checks presence and the golden checks absence.

## Two green tests can leave the campaign's own thesis untested

This is the standing lesson (*mutation-test the deliverable tests*), but the
shape it took here is new enough to record. Both existing tests were about
*what is emitted*. Neither could be about *what is not*, because the helper
that finds a layer in a document fails loudly when the layer is missing — the
right behaviour for a locator, and precisely what makes it blind to
over-emission.

The same weakness existed in miniature on the wasm side, where the drive
script asserted that one withheld layer of sixteen was absent. That check
would have passed against a projection that spared only the sampled name. It
now filters the full name list and checks all sixteen, which cost one line and
was verified live by mutation.

**Lesson:** absence is the harder half of a projection, a filter, or a
redaction to assert, and the natural helpers for such code are all built to
find things. When the deliverable is *fewer* of something, write the assertion
over the complement set explicitly, and mutate the implementation to
over-produce rather than under-produce.

## Print the assumption's residual, not just the result

The specification's predicted serialize time assumed cost was proportional to
bytes, said in as many words that this was an assumption rather than a
measurement, and instructed that a departure be reported as the finding. The
profiler was therefore written to print the time/byte ratio as its own line —
1.000 meaning serialize fell exactly with bytes — instead of leaving a reader
to divide two percentages.

The same discipline paid better on the composition table. Each layer's bytes
are measured as the difference between a one-layer document and a zero-layer
one, which is only that layer's true contribution *if* layers are independent
— the design's load-bearing assumption. So the profiler sums the nineteen
contributions plus the metadata and prints the residual against the full
document. It came out zero. The assumption the whole design rests on is now
checked by the same run that exploits it, at production width, every time
anyone runs the profiler.

**Lesson:** when a measurement rests on a modelling assumption, make the
instrument print the assumption's residual next to the result. A number that
would be wrong if the assumption failed, printed beside a number that shows it
did not, is a much better artifact than either alone — and it costs a
subtraction.

## Follow-ups

### type-audit is blind to associated consts — a silent hole in a default-deny tool

**Owner decision 2026-07-29: note it as a quick followup.**

`tools/type-audit/src/extract.rs:64` handles `syn::Item::Const` — module-level
constants only. A `pub const` declared inside an `impl` block is never walked, so
the audit reports success on it **without having looked**.

That matters more than a normal gap because the audit is **default-deny**: any
untagged pub-boundary primitive is supposed to FAIL. Here it passes. The failure
mode is silence, not noise, which is the wrong direction for a tool whose whole
value is refusing to be quiet.

Found via `TileFields::ALL_NAMES` (`windows/scene/src/lib.rs`), a
`pub const &[&str]` at a public boundary. It is tagged by hand
(`bare-ok(identifier-text: ALL_NAMES)`) so the surface is honestly annotated if
the tool ever grows the case. **The tool was deliberately not changed** — a fix
belongs in its own change, not smuggled into a scene-layer campaign.

**Scope of the fix:** add `syn::ImplItem::Const` alongside the existing
`ImplItem::Fn` handling at `extract.rs:82`. Then regenerate
`docs/audits/type-audit-report.md` and expect the counts to rise across the
workspace — every associated const that has been invisible will surface at once,
and each will want a verdict tag. That second half is the real work, and is why
this is its own followup rather than a one-liner.

**Cheap first step before committing to it:** count how many associated consts
exist at pub boundaries workspace-wide, to size the tagging job.

### The rest

- **An unexplained ~1.8× in a committed profiler's absolute figure.** The
  spec's serialize baseline of ~553 ms traces to The Sextant's spec (567.0 ms
  for a 17,313 KB document). This campaign measured 1029.1 ms for the same
  document size on a quiet box with an unchanged serialization path; The
  Cistern's committed chronicle records 1053.6 and 1062.9 ms. Two of three
  agree and The Sextant's is the outlier, but the cause is not known. The
  campaign's claim is a within-run ratio and is unaffected. Worth an hour with
  `git bisect` on the profiler rather than a campaign — an instrument whose
  absolute numbers move 1.8× for unknown reasons is a weaker instrument than
  its committed status suggests.

- **`hw_new_pinned` clears `WORLD` before its `-1`/`-2` returns**, so a caller
  reading `OUT` after a refused pinned call gets a document belonging to a
  world that no longer exists. Not a defect today — every caller in the
  repository checks its return code — but it makes that export the outlier now
  that `hw_scene_tiles_selected` sets an error envelope on every non-zero
  return. Changing the clearing order is a separate change with its own
  byte-identity story.

- **The `_ => -5` catch-all in the new export's parse arm.** Honest today,
  since `TileFields::parse_json` yields exactly two error variants, but it
  would misreport a future third. Named in the code comment rather than fixed,
  because the alternative (an exhaustive match on a non-exhaustive error enum)
  trades a silent misreport for a compile break in an unrelated crate.

- **Re-encoding remains the larger prize.** Base64 typed arrays or a binary
  side-channel would take ~4 bytes per float against JSON's ~10 — about 60%
  off *every* layer rather than 54% off the unread ones — and would largely
  remove `JSON.parse`'s array-building cost rather than reducing it. That
  needs `scene/tiles/v2`, a catalog version bump, and coordinated client work.
  Projection composes with it rather than competing, which is why it went
  first. The `ocean` array's 5.3 bytes per element to carry one bit (~430 KB
  of `true,`/`false,`) is the clearest single instance.
