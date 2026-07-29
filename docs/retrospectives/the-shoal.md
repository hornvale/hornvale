# The Shoal — retrospective

Process lessons, not product.

## The book had no page where the bug could appear

After fixing 79% of the world's prose, regenerating every committed artifact
produced **zero** drift. No page in the gallery sampled a marine room — every
published artifact was on land.

That is the real finding. The defect was not subtle; it was unobserved. No
reader of the book could have seen it, and no drift check could have caught it,
because the drift check compares generated output against a fresh run and every
generated page happened to look at the same third of the planet.

**Lesson:** artifact coverage is a distinct property from artifact freshness,
and only the second one is tested. A campaign that adds a capability should ask
whether any committed artifact *exercises* it — and if the answer is no, the
capability is untested in the one place that would let a human notice. The fix
here was one gallery page; the general form is "does the book have a page where
this could go wrong in public?"

Related to the existing memory that the deliverable is prose and must be read.
This sharpens it: reading is necessary, but a page that gets read *every time*
is better than a discipline of remembering to look.

## A pure re-key with zero drift is a strong contract

Task 1 changed the grammar's key from the flat `Biome` to the faceted
expression and changed nothing else. The acceptance criterion was that
regenerating every artifact in the book produce no diff at all.

That is a much stronger check than a unit test, and it costs one command. It
caught nothing this time — which is the point: it converted "I believe the arms
are mapped correctly" into "the whole system agrees they are", before a single
new word of content was written. Separating the mechanical re-key from the
authoring, and demanding zero drift from the first, made the second free of
suspicion.

**Reusable shape:** when a change has a mechanical half and a judgement half,
land the mechanical half alone under a zero-drift assertion. Anything that
moves afterwards is unambiguously the judgement.

## Capture the oracle before you change the thing

The land-descriptor table in the tests was captured by running the *current*
code and pasting its output, before any edit. Written afterwards it would have
been a transcription of the new behaviour wearing the old behaviour's name.

Second campaign running to need this (The Formations transcribed the old
classifier for the same reason). It is becoming a pattern worth naming:
**an equivalence test needs the old behaviour recorded somewhere the new code
cannot reach.**

## Splitting the epoch out was right

The Stratum specced this campaign as prose *plus* the name-gloss epoch. Splitting
them meant the zero-risk, highest-visibility change shipped without waiting on
a census-regeneration authorization, and the epoch will be reviewable on its own
terms rather than buried under a large prose diff.

**Lesson:** when a specced campaign bundles a free change with a carve-out, the
default should be to split, not to bundle. The spec is a design authority, not a
work-breakdown authority.

## Precedent did its job

`scene/surrounds/v1` goldens moved, and its test names the moved bytes as an
epoch decision point. Rather than escalating, the campaign applied the ruling
Nathan gave at The Occlusion's G6 for `vessel/session/v1`: shape unchanged, one
prose value corrected, so it is a rebaseline rather than a `v2`. Here it was
even clearer, since the contract explicitly names *field order* and field order
was untouched — verified by a key-set diff, not by eye.

**Lesson:** ruling on a *class* of decision, not just an instance, is what makes
the second instance cheap. The Occlusion's ledger entry was written to be
reusable, and it was reused four campaigns later without a second interruption.

## Follow-ups

- **`Ice` renders with land clauses** — "wind-carved sastrugi sun-warmed dry".
  Correct in the sense that permanent land ice *is* land, but "sun-warmed dry"
  on an ice sheet is the same category smell this campaign fixed for the sea.
  A cold-medium clause set is the obvious next slice.
- **The remaining Stratum sequence:** the name-gloss epoch (split out of this
  campaign, still resolved yes), exotic commensurability, and the depth band.
- **Variants proper are still not built.** This campaign gave each *formation*
  a pool; the spec's `variant` facet — named, concept-bearing sub-types like
  cerrado under savanna — remains the epoch-bearing campaign.
