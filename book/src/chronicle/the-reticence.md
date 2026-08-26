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
starts with; what you have done decides how much of it is spent.

**Those starting amounts are the campaign's one hand-picked quantity, and it
is worth naming rather than letting the prose imply everything was derived.**
A people with apparatus tolerates two overrides before its stance worsens a
step, a people with only a word tolerates four, a people with neither
tolerates eight. Nothing derives 2, 4 and 8; they were chosen. What keeps
them honest is that they are thresholds on a count nobody chose — the record
of what this rider has actually made this body ignore — so the *quantity*
being judged is earned even though the step size is picked. They are not
retuned to rescue a measurement, and the test that reports the campaign's
null says so in its own header.

The
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

The measured answer is no, and the reason is more interesting than the
answer. A host's testimony is about whichever drive is currently winning
arbitration, and its willingness is read from how often *that* drive has
been overridden. Across a preregistered sweep of seventy observable points
in six sessions, that count was zero at every single point — so all three
doctrines agreed at every point, unconditionally, and the sweep measured
zero divergences.

The tempting explanation is that the two quantities are disjoint by
construction: a drive is never recorded as overridden on the very tick it
wins, so the winner can never carry a record. That is true per tick and it
does not settle the question, because the count the host actually reads is
not a per-tick quantity — the override record is accumulated across the
whole possession and never reset. A drive that piled up overrides while
losing would carry all of them with it if it later won.

What actually holds the count at zero is that **arbitration is sticky**. A
drive holds the topic only during an opening stretch in which it has not yet
lost anything, and once a drive starts losing it is never observed to take
the topic back. Seed 42 shows the shape plainly: fatigue is the subject of
the first answer while the record is still empty, thirst takes over one
sample later, and thirst then holds it for the rest of the possession while
fatigue and hunger climb past a hundred overrides each. The pursued drive
genuinely does change — in three of the six sampled sessions — which is
exactly why "impossible" is the wrong word for this null. Pushed to a much
longer horizon, seven hundred and twenty observed points across the same six
seeds, the count was still zero every time.

So this is a contingent fact that keeps holding, not a structural
impossibility, and that makes it a stronger result rather than a weaker one:
an impossibility would merely restate the wiring, whereas a fact that
survives two very different sampling regimes is something the world is
actually doing. A positive control, run against the identical comparison the
sweep itself uses, confirms that comparison is not simply incapable of
detecting a divergence: fed a case where it should disagree, it does. Two
separate things would make the mechanism live — widening what a host can be
asked, or anything that lets a drive regain the topic after a spell of
losing. The willingness model underneath remains genuinely sensitive to a
people's doctrine at override counts nothing currently reaches.

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
