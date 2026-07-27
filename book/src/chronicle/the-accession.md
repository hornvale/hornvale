# The Accession

A dictionary is supposed to be the kind of thing you can add a word to. You
append the entry, the other entries stay where they were, and nothing that was
already true stops being true. That expectation is so basic it rarely gets
written down, which is roughly how a world can spend two weeks quietly failing
it.

This campaign began as an audit question — is the world's vocabulary still
matched one-to-one to the world's things? — and the audit turned up twelve
creatures with no name. Treants, mammoths, three dragons, an owlbear: all
simulated, all placed, all narrated to anyone walking the world, and none of
them holding a concept in the registry. The obvious repair was to register the
missing twelve. Priced before it was performed, the repair moved seventy place
names across the world and churned eleven committed artifacts.

That price was the discovery. Not the twelve missing names — those were only
bookkeeping — but the fact that naming a thing was expensive at all.

## Why a word cost seventy names

A language's words are not drawn independently. Each concept in a family's
vocabulary is assigned a distinct ancestral root, and *distinct* is a global
property: the assignment walks the concept universe in order, draws a candidate
form for each, and re-draws whenever the form it wanted is already spoken for.
Core vocabulary is assigned first so the short forms go to the words a people
says most; the rest take what remains. Where a cascade of sound changes would
collapse two core words onto one modern form, the assignment rejects that
candidate too, so no two core concepts are ever homophones in any daughter
tongue.

The consequence is that a concept's word depends on every concept assigned
before it, and on nothing assigned after. Order is destiny. A concept that
sorts last takes whatever is left and disturbs nobody; a concept that sorts in
the middle can take a form some later concept would have had, sending that one
back to draw again — and every name built from its word moves with it.

The code knew this. Beside the sort it says, in as many words, that *a concept
added to the universe later slots in without reshuffling the words already
assigned ahead of it*, and a test stands behind the claim. But the sort key was
the concept's own name, so a new concept landed wherever the alphabet put it,
which is hardly ever last. The test passed because it only ever tried the
position where the promise holds — a newcomer named `zzz-late`, which sorts
after everything. The property was real, the guarantee was partial, and the
gap between them was invisible because the only witness ever called had been
chosen from the safe side.

Measured one at a time, ten of the twelve creatures were free. `treant` cost
five names. `otyugh` cost sixty-five. Additivity was not a property of the
system; it was a coin flip nobody was watching.

## Accession

The repair is a single idea borrowed from libraries, which have been adding
books to fixed shelves for a very long time. A library does not re-shelve its
collection when an acquisition arrives; it assigns an *accession number*, in
order of arrival, and the number never changes. What makes the collection
growable is not that new books are unimportant but that arrival order is
recorded and respected.

So each concept now carries the generation in which it joined the registry.
Everything present when this campaign landed is generation zero; everything
added afterwards belongs to a later cohort. The assignment sorts by generation
before anything else, which puts every newcomer strictly last — the one
position that provably displaces nothing, and the exact position the existing
test had already proved was safe. What had been a coin flip became a
construction.

Registering the world's twelve nameless creatures now leaves every world
byte-identical. `otyugh`, which cost sixty-five names in the morning, costs
nothing.

## What it costs

Sorting by arrival before importance means importance no longer comes first.
Core vocabulary still wins short forms *within* a generation, but a word that
arrives later takes what is left, however common it turns out to be. For a
language this is not obviously wrong — real tongues do hand their newest
coinages their clumsiest forms, and a borrowed word usually sounds borrowed.
It is nonetheless a real trade, made deliberately: guaranteed additivity
bought with the freedom to give a latecomer a short name.

There is a way to have both, and it comes from the same shelf. Dewey's decimal
notation slots a new subject between two existing ones without renumbering a
single book, because the space of call numbers is dense enough to admit
insertion anywhere. The linguistic equivalent is to reserve a shape of word
that established roots cannot occupy and draw later coinages from it — then a
newcomer cannot collide with an incumbent at all, additivity holds by the
construction of the sound-space rather than of the queue, and the oldest words
keep their short forms. Its cost is that new words are audibly new, which is
not a cost so much as a description of how languages actually sound. That
remains ahead of the world, registered and wanted.

## The shape of the lesson

The defect was not that anyone had reasoned badly. The property was named, the
mechanism was documented, and a test was written to hold the line. What failed
was subtler: the test could only fail in a way the design already prevented.
Its witness was drawn from the safe position, so the assertion it made was true
and the guarantee it implied was not.

The repair carries the same hazard, and is guarded accordingly. A concept's
generation lives in a table someone has to remember to update, and a forgotten
entry would quietly read as generation zero — restoring exactly the churn just
removed, silently, in the manner of the original defect. So the table is held
against the registry in both directions, and a concept registered without a
generation now fails loudly rather than drifting. The check found a real fault
the first time it ran, before the code it guards had executed once.

The world can be added to again.
