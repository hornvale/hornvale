# The Compendium

A compendium is a *weighing together* — Latin *compendere*, the root that also
gives *compensate*. It abridges by putting unlike things on one scale, which
means its whole value rests on the scale being honest about what it can and
cannot weigh.

[The Repertoire](./the-repertoire.md) built a capability probe against Georges
Polti's dramatic situations, and [The Collation](./the-collation.md) gave it a
second column. Both ask one question: can a **world** represent a situation?
This campaign opens a second family asking a different question of a different
subject — does a **program** implement a capability? — and reads its first
catalogue, Herbert Wolverson's *Roguelike Tutorial — In Rust*, backwards, as
seventy-four numbered pages of things a roguelike is expected to do.

## Why the existing resolver could not take it

The trope resolver scores requirement bundles against the **concept registry**:
predicates, concepts, phenomena — the sim's ledger vocabulary. That substrate
cannot carry this catalogue, and the failure would have been quiet rather than
loud.

**Eight of the tutorial's seventy-four items are pure renderer work** — two
user-interface passes, wall-glyph selection from a neighbour bitmask,
bloodstains, particle effects, a camera and viewport, a coloured message log,
console text layers. None of it has a concept-registry token and none of it
ever will, because
[decision 0022](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0022-sim-emits-data-clients-render.md)
puts rendering outside the ledger deliberately. Resolved against the registry,
every one of those pages would have read *blocked by a dangling requirement* —
a number that would have been plausible, near zero, and a category error.

That count is about the *substrate*, and this campaign spent one whole review
round learning that it is not about the *verdict*. Having no registry token is
not a reason to call a capability refused — see **The asymmetry**, below.

So the two families are siblings, never members of one another, and
[decision 0134](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0134-a-capability-corpus-is-a-sibling-to-a-trope-corpus.md)
settles it: different subject, different resolution substrate, one resolver
each.

## An unmet capability is not one fact

The trope family needed three verdicts. This one needs five, and the split is
the entire design rather than a refinement of it. An item Hornvale does not do
may be one of three completely different things:

```
  present       Hornvale does this            -> cites a MECHANISM anchor
  refused       Hornvale deliberately won't   -> cites a DECISION anchor
  deferred      planned, not built            -> cites a REGISTRY anchor
  absent        a genuine hole, nothing claimed-> cites NOTHING
  inapplicable  about the tutorial's toolchain -> cites a REASON
```

Chapter 2.6, *Dealing Damage*, is the case that forced it.
[Decision 0070](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0070-wounds-commit-health-folds.md)
rules that no stored, mutable health value may exist anywhere — wounds commit
and vitality folds over them — and closes by ordering combat explicitly *after*
that model, because combat built first would invent the counter. An instrument
with a two- or three-valued vocabulary files that as *missing*. It is not
missing. It is refused, on the record, with a reason and a sequence.

## The anchor is the evidence

Every verdict except `absent` cites an anchor, and the resolver checks that the
anchor still resolves. All three kinds resolve without building anything: a
decision anchor is membership in the generated in-force index, a registry
anchor is a parsed row, a mechanism anchor is a test name or a path found by
scanning sources. Four conditions turn the artifact red:

```
  DANGLING        the anchor stopped resolving — a decision superseded,
                  a registry row deleted or renamed
  STALE-DEFERRED  the verdict says `deferred`, but the cited row now reads
                  `shipped`. The verdict has become a lie.
  UNJUSTIFIED     a non-`absent` verdict with no anchor — a parse error
  NOVELTY         the `absent` count rose against the committed artifact
```

None of the four is new machinery. Superseded decisions are absent from the
in-force index by construction. Registry IDs already parse, for the drift check
that guards the registry itself. `STALE-DEFERRED` is the mutation guard's
stale-declaration verdict, exactly — a one-directional acknowledgement can only
ever be satisfied, so it rots, and the fix is a check that fails the moment
somebody adds the missing thing. `NOVELTY` is the ratchet shape the trope
audit, the type audit and the duration baseline all already use.

What this buys is stated most concretely as a prediction about the future. The
registry row admitting cellular automata, drunkard's walks, mazes and wave
function collapse as *derivations* rather than as authored layout is planned
and unbuilt; the day somebody builds them and flips it to `shipped`, all seven
pages deferred against it go red until a human re-reads each one. When decision
0070 is superseded by a combat decision, chapter 2.6's refusal goes red the
same day. A hand-authored coverage table does not go stale so much
as begin to **lie**; this one cannot, because each cell points at something
checkable instead of asserting.

[Decision 0135](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0135-a-coverage-verdict-cites-a-checked-anchor.md)
ratifies that rule and states its price plainly: the idea registry stops being
prose that humans read and becomes a gated interface, where a row's identifier
and status are load-bearing for a committed artifact.

## The reading

Seventy-four numbered pages, four sections, one column, rendered to
[`docs/audits/system-coverage-wolverson-2021.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/system-coverage-wolverson-2021.md)
by `hornvale systems report` and ratcheted by a whole-file byte comparison
against its committed copy:

| verdict | count | share |
|---|---|---|
| present | 24 | 32% |
| refused | 5 | 7% |
| deferred | 28 | 38% |
| absent | 12 | 16% |
| inapplicable | 5 | 7% |

**The first page Hornvale cannot replicate is 2.1 — the tutorial's first page —
and it reads `absent`.**

That page asks for entities carrying their own appearance: a `Renderable` of a
glyph, a foreground colour and a background one, drawn to a screen. Hornvale
has the entity half in depth — an entity is a ledger subject and its components
are the facts committed about it, contradiction-checked against a concept
registry that no ECS tutorial has an analogue for. **What it cannot do is tell
one entity from the ground.** In the shipped character-grid client's walk band
there is one glyph for *here* and one for everything else in view, terrain and
marks alike, so a creature and a boulder draw the same character on every seed.
Colour is the second half of the same failure and not the first: no per-entity
colour channel exists either — a mark on the wire carries a noun, a kind, a
datum and a salience, and nothing else.

The reading before this one said 2.6, *Dealing Damage*, and it was wrong for a
reason worth more than the correction: see **The asymmetry** and **The colour
question**, below. What survives is that the ladder's next four rungs are met —
a walked map that is a sculpted planet, symmetric shadowcasting for field of
view, creatures derived from the world's own demography and drawn as marks —
and that the first *refusal* is still 2.6, still 0070, still on the record with
a reason and a sequence.

That sentence — *the first page this project cannot replicate is page one* —
is the single most useful thing this catalogue produces, and it is only
available because the tutorial declares itself **ordered**. Its items
form a pedagogical ladder where each chapter assumes the one before it. A
feature list has no such order, and *first unmet item* asked of one would be an
artefact of sorting. So the corpus states its own ordering and the resolver
refuses ordinal claims for a catalogue that does not, rather than silently
ranking by identifier.

**Every remaining refusal is decision 0070.** Five pages — dealing damage,
ranged scrolls, bloodstains, a boss fight, ranged combat — and all five are
downstream of the same sentence: no stored, mutable health value may exist
anywhere. Against a catalogue of seventy-four things a roguelike is expected to
do, this project has exactly one ratified refusal, applied five times. That is
a sharper statement of where Hornvale actually stands than any percentage on
this page, and it only became visible once the asymmetry below was fixed.

The rest of the reading is mostly a map of where the project's positions live.
Twenty-eight deferrals cite twelve distinct registry rows, and they
concentrate hard: admissible derived map builders, an object genus, underground
settlement, and the fact that **the underworld has no chart of its own** carry
most of them between them. Not one new registry row had to be minted to explain
a deferral, across two scorings — a stronger result than it sounds, because it
means the backlog already knew about every gap the catalogue found that anybody
had planned for.

## The asymmetry

The reading above is the second one. The first was wrong, and the way it was
wrong is the most useful thing this campaign produced.

**A verdict has to measure one subject, and this instrument silently changed
subject halfway through the corpus.** Pages that were mostly simulation had
their sim half scored `present` and their render half waved away as decision
0022's business — page 2.1's own note said so in as many words, that drawing an
`@` "is decision 0022's, not the sim's". Pages that were mostly rendering had
the *whole* page scored `refused` under that same decision. Same decision,
opposite treatments, and which one a page received tracked which produced the
more favourable verdict.

**Five reviews did not catch it, because every anchor resolved.** They were
real tests over real sim mechanisms. The resolver checks that a citation still
points at something; it has no way to notice that the citation is evidence for
a different question than the one the row is answering. An instrument can be
fully anchored, fully green, and measuring the wrong thing — and the anchor
discipline, which is this family's whole safeguard, is blind to exactly that
failure. Nathan caught it by reading the client.

The rule that closes it is one sentence, now
[decision 0135](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0135-a-coverage-verdict-cites-a-checked-anchor.md)'s
second clause: **the corpus scores the whole program, clients included, and a
page spanning the sim and the client takes its weakest half.** Decision 0022 is
an *assignment of responsibility, not a refusal* — it says the sim emits data
and clients render, and never that rendering does not count. What it genuinely
forbids is narrow enough to state in one line: the sim carrying a picture. The
tutorial's `Renderable { glyph, fg, bg }` has no counterpart here and never
will. Pictures existing was never forbidden at all.

Re-auditing all seventy-four pages under the rule moved twelve verdicts **in
both directions** and re-anchored two more, which is the part worth insisting
on — it was not a downgrade pass:

- Six pages left `refused` for something honest. A user interface exists (a
  two-page journal spread: a plate, an entry, an endpaper identity strip);
  particle effects are *planned* rather than refused, licensed by name in the
  registry; a viewport that decouples map size from screen size ships in the
  atlas viewer, pan, zoom and all; text composited independently of the map
  ships in the Casement.
- Five pages left `present` for something honest. You can descend into the
  caves and **no renderer can draw them** — the underground band folds into the
  walk band, so the pane and the verb agree on a chart of the country
  *overhead*. Three pages about going underground rest on that one gap. The
  hunger clock, richer than the tutorial's counter as a fold over committed
  meals, applies only to derived creatures: the possession never gets hungry
  and no channel carries the number. And page 2.1, a round later and for the
  reason the next section is about.
- One page went from `refused` to `absent`, which is the least comfortable
  move and the most honest one. Wall glyphs selected from a neighbour bitmask
  are not refused by anything. Every wall in every renderer simply draws `#`,
  and nobody has planned otherwise.

The count of pages refused under 0022 went from eight to **zero**.

## The colour question

Fixing the asymmetry meant scoring the render half, and scoring the render half
meant answering a question this campaign got wrong three times in one session:
does Hornvale draw its world in colour?

**Colour is on.** The interactive session takes the lantern lens; only
`--script` runs unlensed, deliberately, so committed transcripts stay
byte-stable. Enter a building at seed 42 and the map emits four hundred and
fourteen escape bytes. Stand outside at that same seed and it emits none — and
the chart's own disclosure line says why, in the program's voice: *nought
tinted, thirty-one withheld — water, a mark, or you*. Seed 42's flagship stands
on water, and the lens withholds tint from water, from marks, and from the
observer, because a tint describes bedrock and none of those three is bedrock.
Move to a land seed and thirty of thirty-one cells tint.

So the wrong answers, in order, were: colour is a client-side deferral; colour
is absent; colour is on, *inferred from finding the truecolor code*; colour is
off by default, *inferred from finding the unlensed default and never opening
the interactive arm*. **Each was true about something narrower than what it
claimed.** The fourth arrived as the correction of the third and was evidenced
by counting escape bytes in the committed possession scripts — which are files
of typed commands, `look` and `map` and `examine sky`, and could not have
contained an escape byte whatever the renderer did. A campaign about guards
that report safety they do not have produced, as the proof of a correction, a
control that could not have failed.

What the headline actually turns on is not colour at all, and it is
unconditional. Nathan's sentence was *everything in the wilderness is a `+`*,
and the shipped character-grid client says it verbatim: one glyph for here, one
for everything else in view, **terrain and marks alike**. A creature and a
boulder draw the same character, on every seed, under every flag. The render
half of page 2.1 does not fail on a missing colour attribute — it fails one
step earlier, on whether an entity is distinguishable from the ground at all.
Colour is only the second finding: no per-entity colour channel exists either,
so both halves of `Renderable { glyph, fg, bg }` are unanswered.

Page 2.3 survives all of this because every distinction its map draws is
carried by a **glyph** — five relief glyphs and three water glyphs outdoors,
wall, floor and threshold indoors, a faded twin for everything remembered
rather than seen — and a glyph is not lens-gated and does not depend on the
seed.

## The surplus, and what it does not say

The trope matrix's most valuable table is its *demand* read — the catalogues
scored against each other, where disagreement is the finding a single column
cannot carry. With one column here, the analogue runs the other way: what does
the catalogue never think to ask for?

Enumerate the domains and the presenting windows; any subsystem no `present`
verdict cites is **surplus**, and
[`docs/audits/system-matrix.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/system-matrix.md)
prints the list beside every corpus's tally. Twenty of twenty-five. Astronomy, paleoclimate,
language, religion, culture, history, demography, settlement — the tutorial has
no vocabulary for any of it, and the five subsystems it does reach are the ones
a walked, rendered game touches.

The read is derived on every run from the corpora and the live directory tree,
never authored, so it moves on its own as domains land. It is also coarse, and
the artifact prints that limitation directly above the list rather than
below it: the most-cited subsystem carries ten anchors and therefore does
not appear as surplus, which reads as full coverage and is nothing of the kind.
Ten anchors are not ten anchors' worth of a crate's surface, and this
instrument does not measure that surface at all.

## What the instrument is least entitled to

In the trope family, requirements were mapped onto the bundle vocabulary by a
model reading wiki prose blind — a weak authority, but an *independent* one.
Here the verdicts are authored by us, about ourselves, which invites
self-flattery structurally rather than accidentally.

The anchor requirement mitigates it unevenly, and the report says so above its
own headline. `refused` and `deferred` are strongly checked: a decision must be
in force, a row must exist and must not read `shipped`. `present` is weakly
checked — a path that exists is not a working feature, and a resolvable test
name is not proof this page's capability is met. Preferring a test name over a
path narrows the gap without closing it. So `present` is the verdict this
instrument is least entitled to, printed as such immediately before the tally
it most affects, and seven verdicts carry an explicit *arguable* qualification
that travels into the rendered table rather than living only in the source.

The strongest conceivable version of this instrument would run each chapter as
a scenario against a live session and read the verdict off a transcript —
behaviour rather than assertion. It is refused here on two grounds: it costs
more than the rest of the campaign combined, and it still cannot cover those
eight renderer items, which have no transcript to read. It is recorded as a follow-up
scoped to the pages where a transcript would actually discriminate, not as a
vague ambition.

## What it does not claim

This is **reach against one catalogue**, and the catalogue is a teaching
sequence for building one particular game on one particular library — not a
specification of what a roguelike is. Its later sections are one game's content
design. A second column is the point of the exercise and is deliberately not
this campaign's work; the corpus format, the resolver and the matrix all take N
columns from the first commit, because a format that could not accept a second
catalogue is the thing this family would most regret shipping.

Thirty-two per cent is therefore not a score. It is one reading through one
declared bias, and the number that matters on this page is not the percentage
but the identifier: the first page this project cannot replicate is **2.1**,
the reason is written down, and it took three wrong answers and a human
playing the game to find it.
