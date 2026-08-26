# The Reticence

[The Confidant](./the-confidant.md) gave a possessed host a voice, and made
that voice unreliable in two ways it could not help: no perception of what
its own arbitration suppressed, and no word in its culture for some of what
it feels. Both are incapacities — things the host cannot do anything about.

This campaign adds the third unreliability, and it is the first one that is a
choice. The host knows how it feels, has a word for it, and decides what you
get. You ask the body you are wearing how it feels. Whether it answers, and
whether the answer is true, depends on what its people believes you *are* and
on what you have made it do.

## What a people believes a rider is

Nobody authored a table of who thinks what. The world already commits the
two things the question needs.

A settled people already carries words for `god` and `spirit` in its
vocabulary, and its beliefs already carry a `cult-form` — organized or folk.
An organized cult has apparatus, precedent, a prescribed response for
something inexplicable moving through a body; a folk cult has a word for it
and nothing behind the word. A people with no settlement has neither concept
at all, so it reaches for whichever body-state word its own tongue does
carry when asked what is wrong with you.

No culture in this world holds an actual rider-doctrine — the idea of
possession as a named, understood thing — because no such concept is
registered anywhere in the concept registry yet. So every people currently
falls on the *improvising* side of the line, reaching for the nearest
metaphor its religion already gives it: fifteen peoples measured at one
seed, nine organized (the `god` reading), six folk (the `spirit` reading),
none holding the ceremony that would let it name you correctly.

The direction this gives the willingness is not the one it looks like at
first. A people with the doctrine "knows what to do about" a rider — it has
apparatus, and apparatus makes it guarded. A people with nothing to invoke
has nothing to guard, and is the most open of the three. The host who cannot
name what is riding it is the host most willing to talk to you about it.

## What this rider has actually done

The second input is not about the people at all. It is about you.

Every tick, arbitration ranks this body's drives and pursues the winner. The
ranks it does not pursue were already being computed and thrown away before
this campaign — The Confidant kept them for one tick at a time, as the
residue a host cannot introspect about itself. When the player is driving,
that per-tick residue is precisely the record of what the rider has made
this body ignore, and this campaign keeps it across the *whole* possession
instead of discarding it every tick, one running count per drive.

The axis you override is the axis the host goes quiet on. A rider who has
made this body ignore its thirst eleven times finds thirst is the one
subject this host stops volunteering anything about — not because the
culture withholds it, but because the conduct has earned that particular
silence.

## The two are never added together

A people whose doctrine calls you an ancestor-spirit, ridden by someone who
has overridden its thirst past patience, should still call you by the warm
word *and* still refuse to say where the water is. Summing the doctrine and
the conduct into one number the moment a possession starts makes that host
unreachable — a blended average can never disagree with itself.

So the two stay two values all the way to the point where the host actually
answers. What the culture believes decides how much patience the host
starts with; what you have done decides how much of it is spent. The
disagreement between them — the doctrine's opinion and the conduct's own
account — is the entire point of keeping the pair apart, and it is this
campaign's own version of the gap The Confidant shipped one level up: that
campaign's deliverable was the gap between what arbitration computed and
what the host said; this one's is the gap between what a culture says a
rider is and what this particular rider has actually been.

## Refusal, a lie, and a costly truth are one mechanism

Three behaviors, and they turn out to be one filter with a direction, not
three separate things to build. A host past its patience either says nothing
about a drive at all, or claims to feel fine when it does not — the one lie
this campaign ships is flat and always the same claim, because the
deliverable is that a host *can* lie, not a whole taxonomy of ways to do it.
And a host with almost no patience left does something that looks, at first,
like the opposite of hostile: it answers *accurately*, and volunteers the
very residue an ordinary answer would have kept to itself, because naming
what you have made this body ignore is exactly what costs you.

That third behavior is the one that shows the mechanism is genuinely single
rather than three code paths wearing one name. A hostile host telling the
truth because the truth is what hurts is not a fourth case bolted onto the
other three — it is the same filter, with the salience the culture would
normally suppress inverted instead of withheld.

A deliberate lie needed one careful decision. A culture with no word for the
truth substitutes its nearest neighbour on the same felt-state wheel — that
was already built, and it is a different phenomenon from a host that has the
word and chooses not to use it. Folding the lie into the same representation
as that lexical substitution would have let a committed measurement of
vocabulary coverage start silently counting chosen falsehoods as gaps in a
culture's tongue. So a lie is carried by its own wrapper around the lexical
answer rather than a new case inside it — which makes that measurement
*unable* to see a lie as anything else, rather than merely filtered to avoid
seeing one that way.

## Does the doctrine actually move what you hear?

This is the question the whole model was built to answer, and it was frozen
before any of the mechanism above existed: does a people's belief about what
you are ever change what you actually get told, once a rider's own conduct
is also in play?

Structurally, the drive a host's answer is *about* and the drive its conduct
record has accumulated history for are disjoint by construction — the drive
currently winning arbitration is, by definition, never a drive the override
record has history for at that tick — so the doctrine prior cannot move
observable testimony on the current wiring. This was verified both by
reading the exact call sites that establish it and by a long, direct probe
that never once saw the asked-about drive carry an override. A separately
preregistered sweep, seventy observable points across six sessions, measured
zero divergences, and every one of those seventy points was the case where
agreement between any two peoples' doctrines was already guaranteed before
the sweep ran at all — so the sweep corroborates the structural finding
rather than testing it independently. A positive control, run against the
identical comparison the sweep itself uses, confirms that comparison is not
simply incapable of detecting a divergence: fed a case where it should
disagree, it does. The discriminating state is not reachable through any
currently-shipped way of interacting with a possessed body — a fact about
what the game currently lets you ask, not about the willingness mechanism
underneath it, which remains genuinely sensitive to a people's doctrine at
override counts a player cannot currently reach.

So the honest headline splits down the middle. The conduct half shipped
working and measurable: overriding a drive moves the host's stance on that
drive and only that drive, and a single host refuses on some subjects while
answering freely on others in the same conversation. The doctrine half
shipped measurably inert — decorative, on the only path a player can
currently interrogate — for a structural reason a later campaign could
change by widening what a host can be asked about, not by tuning any
threshold in the mechanism that already exists.

Ask a host what the water tastes like far past its patience, and you may get
silence, a claim that everything is fine, or — worse for you than either —
the truth, freely given, because the truth is what costs you to hear.
