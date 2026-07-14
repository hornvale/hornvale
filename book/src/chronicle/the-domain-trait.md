# The Domain Trait

**July 2026 · outcome: complete — the nine domains' shared registration
ritual became a single trait and a single list, and building that list
proved a load-bearing assumption about the concept registry false**

## What was attempted

Nine domains had grown to the same silent shape. Each one, when a world is
born, registers its concepts and predicates into the shared vocabulary, and
each one that draws on the seed declares its stream labels — the permanent
names of its random draws. Two places gathered those contributions: the
composition root called every domain's registration in turn, and the tool
that renders the stream manifest listed every domain again, by hand. Nothing
tied the two lists together, and they had already drifted — the deep-time
layer sat in one and was quietly missing from the other. The convention was
real but unwritten, and an unwritten convention is a drift waiting to
happen.

The aim was to make the shape explicit: a single trait in the kernel naming
the members every domain already provides, and a single roster in the
composition root that both registration and the manifest read from, so the
two can no longer disagree. The trait was kept deliberately narrow. It
carries only the *declarative* surface — what a domain registers and what it
names — and pointedly not genesis, because how a domain is generated is
composition work whose inputs differ from every other domain's, and folding
that into one interface would rebuild the very monolith the trait was meant
to dissolve. Each domain's crate name, the manifest key, is read from the
crate itself rather than typed by hand, so it cannot be mistyped and cannot
fall out of step with the crate it names.

## What the roster proved

The design assumed registration order was invisible — that because the
vocabulary is a sorted set, the sequence in which domains register could not
be observed in any artifact. Building the roster proved that false, and
loudly. Ordered alphabetically, registration failed outright: the language
layer, alone among the nine, names concepts it does not own — the stone of
the terrain, the gods of the pantheon, the hearth of a settlement — so that
its lexicon can speak of them, and it had always claimed any such concept
that was not yet present. When language ran last, as the old hand-written
order happened to place it, every owner had already spoken and language
found nothing to claim. Run earlier, it seized the terrain's stone under its
own name, and the terrain's later, rightful registration collided with it.
The order had been load-bearing all along; only luck had kept it working.

The narrow fix was to store the roster in that same registration order —
owners before borrowers — and let the manifest impose its own alphabetical
order at the moment it renders, so one list of members serves two readers
each in the order it needs. But the collision exposed something deeper than
an ordering. The act of registering a concept was doing two jobs at once:
*claiming ownership* of it and *ensuring it is present*. Language wanted only
the second and was forced to perform the first. Split the two — a claim that
one domain makes, a reference that any number may make without claiming —
and the order stops mattering entirely; a reference to a concept no one owns
becomes a loud error instead of the silent false claim it is today. That
separation is a change to the registry itself, with its own weight, and it
was set down as a recorded direction rather than taken up here. The world
kept the small fix and named the larger one for later.

## What landed

One trait, implemented by nine unit types that each defer to the
registration the domain already had. One roster, in the composition root
that alone is permitted to know every domain, read by both registration and
the manifest. The vocabulary a world is born with is byte-for-byte what it
was. The manifest gained a single section: the deep-time layer, restored to
the list it had been arbitrarily absent from, now declaring plainly that it
draws on no seed of its own.

What the change does not do is worth stating as plainly as what it does.
Adding a domain is now one line in the roster and one trait implementation
beside the domain's own code — but it is *one* line, not zero. Nothing reads
the domains off the filesystem; a new domain that no one adds to the roster
still will not register itself, and the drift check that guards every
generated artifact can catch a domain removed but not a domain never added.
The list went from many hand-kept copies to one. Closing the gap from one to
none would mean machinery the project has deliberately refused, and so the
one line stays, with a note beside it naming exactly what to write.
