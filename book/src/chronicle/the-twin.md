# The Twin

A controlled experiment needs one variable. For four months this project had
a disagreement between two computers and no way to say what about them
disagreed, because everything about them did: different hardware, different
operating systems, different C libraries, possibly different compilers, and
different directories to build in. Five candidate causes and one observation
is not a measurement. It is an anecdote with a number attached.

A container closes four of the five.

## What was being asked

Two questions survived [The Pyx](the-pyx.md).

The first was whether the C library explained the old disagreement. When a
program asks for the largest whole number below some value, the compiler can
answer in one of two ways: emit a single instruction that the processor
executes directly, or call out to a routine in the system's C library. Which
one it does depends on how old a processor the compiler is told to assume.
Until late July this project assumed an old one, so every such call left the
program and entered the system library — and the system library is exactly
the thing that differs between two machines. Eight days after the
disagreement was recorded, the assumption was raised for unrelated
performance reasons and those calls became instructions. Nobody went back to
check whether that had been the cause.

The second question was whether a second machine could reproduce this world
at all — the question that started the whole enquiry, when a spare
twenty-four-core computer became available.

## The instrument

Both questions were asked with the same tool, an off-the-shelf image
containing the exact compiler this project pins. Nothing was built for the
occasion. The image supplies a fixed compiler, a fixed C library, and — the
part that turns out to matter most — a fixed directory to build in, because
a program compiled here records the path it was compiled at, and two
different paths are enough on their own to make two correct builds disagree.

The first question then needs no second machine at all. The image carries a
*newer* C library than the machine hosting it, so one computer can hold both,
and the comparison reduces to running the same build twice in two different
libraries. The Pyx had abandoned this experiment as impossible, having
written down that it required two machines. It required two C libraries. The
distinction had gone unnoticed, and it was the difference between an
experiment needing a cluster and one needing an afternoon.

Before any of it ran, the diagnostic build was checked for being diagnostic.
Compiled the shipping way, the program contains a hundred and forty-seven of
the single-instruction floors. Compiled the old way, it contains none. Had
that check been skipped, the entire first half might have compared the
shipping configuration against itself and reported a reassuring and
meaningless agreement.

## What came back

Four builds — two C libraries, each with the assumption on and off — produced
the same forty worlds, to the byte. The same forty worlds The Pyx had already
produced on a different processor architecture running a different operating
system.

So the C library is not the mechanism. It cannot be: the operation in question
has exactly one correct answer, which is why the hypothesis had always carried
a caveat saying so. Both libraries give the correct answer. The best available
explanation for the original disagreement, held for four months, is simply
wrong.

The second question came back the same way. The two machines — different
processors, different kernels, different operating systems entirely, one a
conventional Linux server and the other a stripped-down appliance that cannot
even be logged into — produced **the same binary**, byte for byte, once the
compiler, the library, and the directory were held fixed.

## The accident

The most useful thing the campaign produced was a mistake in its own
instrumentation.

A routine step recorded which compiler each build had used. For two of the
four builds it reported the wrong one — an old version, three years stale.
The builds were fine; the *probe* was wrong, because it had asked the question
from a directory outside the project, and the pin that selects this project's
compiler only applies inside the project's own tree. Step outside it and the
machine quietly answers with whatever it considers its default.

That is worth more than the result it nearly corrupted. It means the canonical
machine has been carrying a compiler pin that is conditional on where you are
standing when you invoke it. Whether that caused the original disagreement
cannot now be established — the other machine was decommissioned when the
enquiry that needed it was abandoned — but it is a far more plausible cause
than the one just eliminated, and unlike that one it is a live hazard today,
independent of any history.

## What is settled and what is not

The disagreement of nineteen July remains unexplained, and the honest form of
that sentence has not changed in four months. What has changed is the size of
the space it hides in. The C library is eliminated. The compiler's processor
assumption is eliminated. The bare machine is eliminated. What remains is how
a build chooses its compiler, and the possibility that the original comparison
was confounded in a way no longer recoverable.

The second machine can produce this project's binaries exactly. It has not
been shown to produce its *worlds* exactly, because no world was generated
there, and the rule that exactly one machine may publish the measured corpus
is deliberately untouched. Qualifying a machine is now a cheap and known
procedure rather than a research question, which is the campaign's practical
result; whether to use it on this one is a separate decision, and a
conservative answer remains available.

The container, it should be said, is an instrument and not a destination.
Nothing about how this project builds or ships changed. It was borrowed for an
afternoon because it holds four things still, and put down again.
