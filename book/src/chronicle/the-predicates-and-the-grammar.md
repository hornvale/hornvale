# The Predicates & the Grammar

*The world learns to say more than one thing at a time.*

The First Sentence taught the world to classify itself: *Vebe is a planet.*
One frame, one fact, one sentence. This campaign — the second of the
Self-Writing Book program — teaches it to *aggregate*: to gather every fact
it holds about a subject and fold them into a single dry clause, and to
classify not just the world but the peoples on it. The deliverable, rendered
from nothing but the Fact graph:

> Vebe is a planet with two moons, orbiting a yellow-white dwarf (F); its
> day lasts about 1.5 standard days.
>
> The Vavako are goblins. · The Babako are hobgoblins.

No human or model wrote those sentences. The first is the planet's ledger —
`is-a`, `moon-count`, `star-class`, `day-length-std` — passed through a
construction table; the second is a people's own word for *person* standing
as their name.

## The construction table is the document plan

The Reiter–Dale pipeline calls the step this campaign builds *aggregation*:
same-subject facts become modifiers on one head clause instead of a drumbeat
of simple sentences. The machinery is deliberately small. A construction
table in the book window maps each renderable predicate to a fragment
("moon-count 2" → *with two moons*; "star-class F" → *orbiting a
yellow-white dwarf (F)*; "day-length-std 1.55" → *its day lasts about 1.5
standard days*), and the clause realizer in the language domain gained a
modifier tail, number words (*two*, not *2*, up to twelve — the
newspaper-style convention), and a `quantity` helper that renders *about
1.5* for 1.5507. The table's **authored order** governs the sentence — moons,
then star, then day — not the ledger's commit order, which happens to put
the star first. Both orders are deterministic; the difference is who the
sentence is for. Commit order narrates provenance; the table is a document
plan, and a gazetteer reads better when the plan, not the pipeline, chooses
the sequence.

Referring expressions arrived in the same motion: the volume tracks first
mention, so a subject re-mentioned within a volume reduces to a pronoun.
Today no volume exercises it — after aggregation each subject is named
exactly once — but the mechanism is the Dale–Reiter reduction rule, wired
and tested against the day the volumes grow long enough to need it.

## Two classification predicates, on purpose

The kernel now carries two ways to say what a thing is, and a decision
(0061) ratifying that the split is permanent rather than transitional.
`is-a` is functional and immutable: the world *is* a planet, written once
at genesis, contradiction-checked forever. `instance-of` — shipped by the
ECS program's Individuation campaign in deliberate shadow, with genesis
minting zero instances — is non-functional and latest-wins: a roster kind
that can change (a goblin can become a lich) without rewriting history,
because each reassignment is a new fact. This campaign ends the shadow on
owner ruling: genesis now mints one collective entity per placed people,
`instance-of` its species kind, named by the autonym. The Book reads both
predicates in one pass, and neither stands in for the other.

The autonym is the campaign's quiet centerpiece. A `person` concept joined
the universal stratum — every culture lexicalizes it — and each people's
collective is named by *their own word for person*. This is how
ethnonyms actually work: *Diné*, *Inuit*, *Bantu* all mean "the people" in
the language of the people so named. So "The Vavako are goblins" contains
two voices at once: *Vavako* is the goblins' word, *goblins* is the
gazetteer's.

The campaign also repaired a C1 wart while it was in the neighborhood: the
planet's `is-a` and `name` had been committed onto a freshly minted,
otherwise-empty entity, while every astronomical and terrain fact lived on
the world root. The classification now lands on the root fact-holder
itself, so aggregation finds the planet's facts where the planet is.

## What the measurements said

Two findings worth the record. First, the campaign falsified its own spec's
determinism note. Adding a concept is additive *in the streams* — no
existing draw moves — but not additive *in lexeme space*: `person` sorts
alphabetically before `sibling`, claimed its candidate root in the
merger-aware assignment, and kobold's word for sibling re-probed to a new
form. Deterministic, contract-documented, honestly re-pinned — but the
lesson stands: a routine concept-add can displace existing lexemes, so
census-cleanliness is verified per add, never assumed. (This one came home
clean: the full gate's census probes passed untouched.)

Second, absorbing the parallel Ordination campaign mid-flight surfaced a
limit in its new capability schema. Declaring this campaign's genesis tail
truthfully (`planet` and `peoples` both write `name`) manufactures a false
cycle against `religion` (which reads `name` — place names, disjoint
subjects), because the schema's dependency edges are predicate-granular but
subject-blind, and the tail's real ordering constraint lives in memory, not
in the ledger. The tail is therefore scoped *out* of the schema behind a
documented boundary, and subject-aware edges are filed with the ECS program.
The schema still validates the eight core systems; it now says so precisely
instead of implicitly.

The whole campaign moves no seeded draw, triggers no epoch, and leaves
every world byte-identical under its seed — the ledger simply holds two
more entities per world and the grammar knows what to do with them.
