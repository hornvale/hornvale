# The Hearth

Until now a room in this world has been a point. It has a place on the mesh, it
has neighbours, it can be described in prose — but it has no inside. A creature
standing in a hall and a creature standing in a cave are, to the simulation,
standing in the same kind of nothing: a node with a label. Everything the world
knows about where anyone is stops at the doorway.

This campaign gives a room an interior. Not a floor of squares — that was the
expected answer and it turned out to be answering a question nobody had asked.
The interface this world is building exists so that creatures can be spoken to,
letters read, artefacts taken apart, languages learned by standing near people
who speak them. Walk those activities one at a time and the demand each makes of
space is the same, and it is never metric. Who else is present. Who can hear.
Whether you are alone. What you can reach. Whether there is light. Not one of
them asks how many paces away a thing is.

So the inside of a room is a handful of named places — the fire, the doorway, the
alcove, the water jar — and the relations among them. The vocabulary for those
relations was not invented here; it was borrowed. There is a settled calculus of
how regions can stand to one another, eight relations that between them cover
every case exactly once, with a table saying what follows when two are chained.
Taking it rather than improvising one bought correctness that no amount of care
would have produced, and bought it in an afternoon.

What the room is made of is authored; what any particular room *is* is not. There
is one small inventory of patterns — a fire, a threshold, a recess, a bed by the
fire — and a room draws from it according to what that room already is: built or
wild, cold or warm. This is the same move the language engine makes when it draws
a people's sounds from the space of possible sounds, and it has the same
consequence: houses of one people resemble each other, and that resemblance *is*
the culture, visible without anyone having authored a culture's architecture.

The distinction that mattered most is the one easiest to lose. A pattern is
smaller than a room. A fire with seating turned toward it is a pattern; a floor
plan is not. Author whole rooms and you have a catalogue of solutions, which is
what happened when software last borrowed these ideas from architecture and is
generally agreed to be when they died. Author fragments and rules for combining
them, and you have a grammar — something that can produce rooms nobody wrote,
that are nonetheless right. The test of which one this is has nothing to do with
how many patterns there are. It is whether the rules for combining them carry any
weight. Here they do two things: a pattern declares where it attaches, and a
pattern may declare another that it completes and without which it may not
appear. A bed *by the fire* cannot be drawn into a room that has no fire.

That second rule is what gives a room depth, and depth is what makes the rest
mean anything. An early version attached everything to the middle, which is
tidy and wrong: every place one step from every other, so a fire warms the whole
room equally and crossing to it is a single stride. The room had no far end. With
attachment rules the intended shape appears on its own — threshold, then the open
ground, then the recess, then the fire within it, then the bed beside — and the
fire's warmth, falling by half at each step, spans sixteenfold from the hearth to
the corner by the door. There is now somewhere in a room that is cold.

Two errors surfaced late and both were instructive. The graph could report itself
whole while being, for a creature trying to cross it, in pieces: the check for
wholeness counted a fire inside an alcove as connected to it, and the route
planner did not, so the validator was cheerfully approving rooms with an
unreachable hearth. The fix was to agree that being inside something is a way of
being next to it — you can step into the alcove and out again — and the deeper
lesson was that two functions had been answering the same question differently
for long enough that a test could pass by walking a path nobody intended. The
other error was inherited and invisible: a note on the temperature drive claimed
a kind of checking that does not exist, and had gone unread for want of anything
to check. Giving the drive one new number made the note legible and the tool
rejected it instantly.

Nothing in the world has changed. No creature stands at an anchor yet; no room
derives an interior; every place the new machinery could be consulted passes
nothing and gets the old answer, exactly as before, to the byte. That is not a
shortfall but the shape of the work: the substrate is proved and inert, and the
campaign that makes it live is already written and waiting. What exists now is a
world where the inside of a room is a thing that can be reasoned about — and
somewhere in it, not yet felt by anyone, a fire that is warmer than the door.
