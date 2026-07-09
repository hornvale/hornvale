# Campaign 18: The Meeting

**July 2026 · 11 commits · outcome: complete, merged — the Year-2 capstone:
the two peoples proven to diverge by their vectors and nothing else, and the
book closed on the year**

## What was attempted

Year 2 built two peoples one vector at a time. Campaign Y2-1 gave them
different minds, Campaign 15 different eyes, Campaign 16 different mouths, and
Campaign 17 let those mouths be heard. Each campaign proved its own layer:
the ladder that tops out at *elders* where a goblin's tops out at *chief*, the
pantheon that crowns a moon where a goblin's crowns the sun, the name that
hisses and rolls where a goblin's clusters and stops. But a layer proved is
not a year proved. The metaplan set one exit criterion for all of Year 2:
**one world, two peoples that differ only in their authored parameter vectors,
yielding legibly different languages and religions from the same sky** — and
it demanded that difference be shown to be a reading of the vectors, not an
accident of the dice. A skeptic could still ask whether the divergence the
prior campaigns measured was authored structure at all, or merely two
different draws off one seed wearing the costume of culture. The Meeting set
out to answer that skeptic and to close the year. It adds almost no new
machinery; its whole job is to **prove, measure, and present** what the four
prior campaigns built.

## What landed

**One new piece of machinery, and it never ships.** The Meeting introduces
exactly one generative addition: a **study-scoped species roster** that lets
the Laboratory build a world from a chosen set of species without touching
the path shipped worlds take. The default roster is exactly today's
`{goblin, kobold}`; every shipped world, almanac, and census regenerates
byte-identical, verified rather than assumed. The roster lives entirely at the
composition root — no domain was edited, the layering untouched — and it
resolves against a small closed set the Lab knows, not an open
species-injection surface. It exists to stand up one thing: the null control.

**The null control, run on solo rosters.** The proof the year still owed is a
**distributional twin** — a third people, `goblin-twin`, carrying the goblin's
*exact* vectors, every scalar at its midpoint and every enumeration at its
goblin variant. A vector identical to the baseline cannot move a formula
constructed to reduce to identity there, so the twin must come out
structurally indistinguishable from the goblin. If it diverged anyway, the
divergence the prior campaigns measured would be noise. The subtlety is that
the twin cannot simply join the shipped world as a third people: two species
with identical placement vectors tie on every cell, and placement breaks ties
by species order, so the twin would place nothing at all. The control
therefore runs each identical-vector species **alone** — two solo rosters,
`[goblin]` and `[goblin-twin]`, over one shared seed range. Placed alone, each
lands in the same cells, builds a pantheon from the same phenomena, and grows
the same ladder; the only thing that can differ is the one thing that should,
the names, drawn from streams salted by the species name. The solo design
isolates exactly that noise and nothing else.

**Both teeth bit.** Run a blind structural rule — domain, cyclic share,
pantheon size, no lexical channel — on each seed's goblin/twin pair, and of
the 500 solo pairs **all 500 are indistinguishable**: the rule never once
decides, so it never once picks the twin. Compare the two populations
distribution by distribution, and head-deity domain, cult form, and pantheon
size are byte-identical — total-variation and standardized-mean distances of
**exactly zero** — with only name length diverging, at a standardized mean
difference of about **−0.118**, well inside a conservative sampling-theory
bound and precisely the axis the name-salt noise was designed to move. Read
against the standard roster, where the same blind rule recovers the real
kobold **0.875** of the time and a *mooned* kobold every time, the contrast is
total: decisive separation where the vectors differ, at-chance
indistinguishability where they do not. A 10,000-seed author-time run confirms
the picture holds at scale.

**The suite, gathered into one ledger.** The comparative claims that prove the
two peoples differ were already built, scattered across four campaigns'
calibrations. The Meeting formalizes them: [Study 009, the Census of the
Meeting](../laboratory/study-009.md), presents every hypothesis — head-deity
domain by lock, social verticality by stratification, the slave rung as an
exact gate on rank and surplus and scale, the epithet honorific keyed to
status basis, phonotactic validity, blind attribution at 0.875 — as **one
preregistration ledger**, each row carrying its preregistered direction, its
measured result, and the test that guards it, the null control among them. A
reader sees the whole falsifiability structure of the year in a single table.

**The capstone, one seed read whole.** [The Meeting of Seed
42](../gallery/the-meeting-seed-42.md) puts the two peoples side by side on
one fixed sky and traces their divergence across all three vectors at once:
they settle different cells because their psychology prices land differently,
grow ladders that top out at *chief* against *elders* because their status
basis differs, crown a sun against a moon because their lenses weigh the same
sky differently, and tell those gods' stories in a formal honorific voice
against a repeating descriptive one. Every divergence is recountable by `why`
to a named dimension of a named vector — never to an author's instinct, never
to a coincidence of the dice.

**No new predicates, confirmed.** Closing the year included a review of the
campaign's predicate vocabulary. The Meeting shipped **none**: the concept
registry is byte-identical to its pre-Meeting form, verified against the
committed registry dump. The null control is measurement, not new ontology —
the twin is authored data, its distances are arithmetic, and nothing it
produces is ever serialized into a shipped world.

## What was learned

- **A proof of divergence needs a proof of non-divergence beside it.** Showing
  that two peoples differ decisively is only half an argument; the other half
  is showing that a people which *should not* differ does not. The 0.875
  separation means something only because the twin scores at chance. A
  campaign that had measured only the divergence would have left the skeptic's
  question — structure or noise? — formally open.
- **The control's design carried its own subtlety.** The obvious null control
  — three peoples in one world — is unbuildable: identical vectors tie, and
  the tie-break silently erases the twin. Recognizing that the honest control
  had to isolate each identical-vector species *alone*, so that seed and cell
  and phenomena are shared and only the name-salt differs, was the campaign's
  one genuine design problem. Once solo, the noise the control measures is
  exactly the noise it means to.
- **The safe direction of a bound is worth stating out loud.** Because the two
  solo builds share seed, cell, and phenomena, their signatures are positively
  correlated, so the independent-two-sample envelope is a conservative *upper*
  bound — the true distances are smaller than independence predicts, never
  larger. A distance outside the bound would have been a real finding, not a
  bound to widen. Naming which way the conservatism runs turned the pass
  criterion from a tuned threshold into a claim sampling theory underwrites.

## Year 2, closed

Year 2 set out to give the world a second people and to prove that a people is
its authored vectors and nothing more. Four campaigns built the vectors —
mind, eye, mouth, voice — and this one cashed the exit criterion the year was
staked on: one world, two peoples, one sky, every difference between them
traceable home to a parameter and none of it to the dice. The book is closed
on the year with two model cards typing every species and language parameter,
this chronicle, a freshness sweep of the pages that name the exit criterion,
and the registry review that found nothing to add. The world now carries two
peoples who diverge because of who they are, and can say, of every way they
differ, exactly why.

## Deferred, deliberately

No new domain and no widened vector — any new dimension of mind, eye, or mouth
needs its own campaign that argues for it, not a struct with room to spare. No
society across the full settlement scatter, only each species' flagship still
runs a culture. No diffusion, no relatedness, no pidgin or creole or
language-shift between two peoples who have now been *shown* to differ but have
not yet met — contact dynamics want an event ledger and deep time under them.
No lexicon and no syntax: a name still has a sound and no meaning, and the
no-lexicon line stays bright. And no per-individual variation — the vectors
describe a people, not yet a person. Each is real work for Year 3 and beyond,
none a quiet loosening of what Year 2 made tight.

## Artifacts

[Study 009: The Census of the Meeting](../laboratory/study-009.md) — the
preregistration ledger of the whole comparative suite and the null control's
two teeth, at-chance blind attribution and within-bound distributions. [The
Meeting of Seed 42](../gallery/the-meeting-seed-42.md) — the two peoples side
by side on one sky, every divergence recountable across all three vectors.
[The Species chapter](../domains/species.md) and [the Language
chapter](../domains/language.md) — each now carrying a model card typing every
parameter as derived, approximated, drawn, or authored. `census-of-the-meeting`
— the committed 500-seed solo null-control census, re-run and drift-checked on
every build beside `census-lands-drift`. `windows/lab/tests/calibration.rs` —
the null-control calibrations that hold the twin to at-chance attribution and
within-bound distributions on every build, and the study-scoped roster that
builds the solo populations without the shipped path ever moving.
