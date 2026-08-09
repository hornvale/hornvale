# The Handle

The world named a forest and then refused to let you look at it. It named two
moons and refused those. It named the rock you were standing inside, underground
in the dark, and refused that too.

A handle is what you grab a thing by, and in a parser it is the noun. This
campaign is about the things the world had described but left without one.

## The shape of it

`examine` resolved by whole-string equality against a catalog whose keys were
the phrases the prose had used, verbatim. A player types the head noun. So
`examine tropical seasonal forest` answered and `examine forest` did not.

Measured at the starting room, over the five entries in its catalog, taking
every word longer than two characters that was not a stopword: **six of seven
failed to resolve.** The one that worked did so by coincidence — the settlement
was separately its own entry, so nothing about the *phrase* had made its words
reachable.

Four surfaces carried it, not the two the bug report named. The chart legend's
NPC marks turned up while sizing the check — `examine bugbear` failed on a
creature standing in the room, whose full name the `map` verb had printed one
turn earlier. And the underworld named "the rock" while `examine` resolved
against the *surface* locale's catalog, because the dispatch special-cased
being indoors and nothing else.

## Every generator already knew the answer

The root cause is the most useful thing here, and it is one line of existing
code admitting it. `windows/locale/src/grammar.rs` builds a room's descriptor
and says so in its own comment:

> The variety and its substrate detail are one noun phrase ("erg dunes of
> shifting sand"); the habitat and exotic clauses are qualifiers.

Then it joins them into a string and returns the string. The head noun was
*computed*, named in prose, and discarded a line later. Astronomy does the same
with its moon epithets, composing each clause inline; the underworld does it
with its stratum word.

So the fix is not to recover the noun by parsing — the direction this campaign
exists to avoid — and not to guess with a last-word rule, which gets `a stream
gully, shaded, in a hollow` exactly wrong by naming the hollow. The fix is to
*keep what was already computed*. Prose is composed from declared parts, and
the catalog is those parts.

That principle has a consequence worth stating on its own: **"mentioned but
undeclared" stops being possible by construction rather than by checking**,
because a generator cannot put a word in the sentence except by declaring it
first.

## A name has one face and many handles

A catalog entry became a display name plus a set of resolution words. The split
is not decoration. The catalog is not private to `examine` — it rides the
session snapshot to the browser client, which shows it as a legend. Aliases in
that list would spray "forest, tropical, seasonal" across a client-facing
surface beside the real entry.

So the words are process-internal and never serialize. The wire's shape did not
move; only its content grew, by the entries the sky and the underworld had
never had.

Qualifiers do not resolve. `shaded` and `in a hollow` are things the room is,
not things in it, and the descriptor declares only its noun phrase. That
position had to be defended twice, because `examine` has **two** matchers — the
prose catalog and the chart legend — and teaching one did not teach the other.
Both times the second was found only after the first was fixed.

## Two grains, one question

That pair is the campaign's recurring shape. The legend keys its ground mark on
the whole raw descriptor, so deriving words from it re-admitted every qualifier
the prose entry had just declined: `examine hollow` was refused by one matcher
and answered by the other, in the same room, about the same thing, with
different wording.

The rule already written for that collision — *a noun named by both grains
resolves to the prose datum* — had been written for the case where both grains
agree the word is a noun and only the datum differs. It now also covers them
disagreeing about whether it is a noun at all.

## The check, and the direction it cannot see

The gate is mechanical and needs no parser: **every significant word of every
catalog entry's display name must resolve to an entry.** No curation, nothing to
maintain, and it fails six-of-seven against the pre-campaign world. A
hand-curated list of "words a player might try" was considered and rejected for
the reason such lists always fail — they are maintained at human rate and go
stale in silence, whereas a rule derived from data the catalog already holds
cannot.

It has a blind spot, found by mutating it. Reverting the descriptor's declared
noun phrase back to its display name makes *more* words resolve, and the gate —
which asserts only that declared words are reachable — stayed green. What caught
it was a separate regression test, aimed at the opposite direction. So the
protection here is two tests facing opposite ways, not one gate facing both, and
the spec's claim that the mechanical and structural halves covered the space was
too tidy.

## What it cost the world

Nothing. No seeded draw moved, no epoch, no census. The seed-42 keystone
fixtures are byte-identical to main's, which is the evidence rather than the
assurance: a campaign that touches only how a world is *described* should move
no world byte, and this one demonstrably did not.
