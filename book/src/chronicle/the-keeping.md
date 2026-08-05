# The Keeping

A world's settlements sat in a handful of tight clusters, and the question was
why they crowded. The answer is that they do not crowd. They are a thousand times
too sparse, and every explanation offered for the crowding — including the one
written down in the project's own notes — was wrong.

This campaign changed almost nothing about how worlds are generated. What it
produced instead is six settled decisions, one design killed before it was built,
and a measured account of why a world that models climate, terrain, carrying
capacity, deep history and six peoples nevertheless comes out mostly empty. It is
a campaign whose product is knowledge, and the knowledge cost less than the
implementation would have.

## The floor nobody chose

Settlements on any seed sit at a minimum separation that never varies: on four
probe worlds the nearest-neighbour distance was identical to the decimal. A figure
that repeats exactly across unrelated worlds is not a distribution's tail. It is a
floor, and a floor is a rule.

The rule turned out to be a data structure. The deep-history engine indexes live
communities by the cell they occupy, and the index is a map from cell to
community — a shape that structurally cannot hold two. One community per cell
means no two settlements closer than one cell, and the grid's cell is about a
hundred kilometres across. Nobody designed a hundred-kilometre separation; it fell
out of a lookup optimisation meeting a grid resolution, and the two constants live
in different crates and never meet.

A week after that index shipped, the emergent behaviour was written down as though
it were a design position, complete with a rationale it never had. It read as
settled. It would have been defended. This is the second time the project has
caught the same pathology — a size that is the product of two constants nobody
compares — and the first time, the collision produced a wrong design that no test
could have caught.

## Sparse, not crowded

A cell of that grid covers roughly twelve thousand square kilometres. At medieval
density it would hold something like a thousand villages. A world carries a few
hundred settlements in total, occupying under one percent of its cells. Each
committed settlement is not a village crowding its neighbour; it is a lonely
placeholder for a region.

So the question inverts, and with it the whole shape of the work. The interesting
problem is not how to spread settlements apart. It is why a world's *habitable*
ground is so nearly unused — and the answer to that runs through what the model
means by habitable.

## Habitability is a relation

The word was doing two jobs. A single flag marked a cell habitable if it was dry
land *and* within a temperate band *and* above a moisture floor: three predicates
fused into one boolean, then used as a hard gate on productivity. But whether
ground is dry is a fact about the ground, while whether ground is *livable*
depends entirely on who is asking. Habitability is a relation between a species
and a location, not a property of the location alone — and a model that stores it
as a property of the location has already decided, for every species at once, what
counts as a good place to live.

Decomposing that flag so only *land* remains a hard gate opened the arid and
very-hot bands the old conflation forbade outright. It also demonstrated the limit
of the fix: of all the newly reachable ground, almost none could support even a
minimal community, and none anywhere could support a community large enough to
send out a daughter. Opening ground is not the same as making it livable.

## The model was not the model it cited

Underneath sat a productivity field documented as a proxy for a standard
climate-to-vegetation model from the ecological literature. Only one feature of
that model survived the translation: the rule that the scarcer of temperature and
moisture sets the limit.

The published model rises monotonically with temperature and saturates. It has no
optimum, and it never reaches zero — near freezing it still predicts about a fifth
of its maximum. The implemented version is a symmetric tent peaking at a
comfortable temperature and falling to exactly zero a little above freezing. Above
the peak the two curves move in opposite directions. Below freezing one predicts
diminishing but real productivity and the other predicts none at all.

That exact zero is why the frozen wastes are empty, and it is a departure from the
cited model rather than a consequence of it. A tent with an optimum is not a
productivity curve at all; it is a *tolerance* curve — and tolerance curves already
exist in this world, one per species, authored with their own optima and widths. So
the base field carried a hidden species-blind preference for temperate ground that
no species had chosen, which is the same relation-mistaken-for-a-property error one
layer further down.

It also explains something previously only observed. Two of the six settling
peoples win essentially all the best ground and three win none, despite optima
spread widely across temperature, moisture, light and altitude. If the ground
itself is shaped like a temperate generalist, then every species is scored against
an incumbent nobody authored, and whoever most resembles that incumbent wins
everywhere. The roster was never the problem.

And the cold, it turns out, has a claimant already. Evaluating the authored curves
rather than reading their optima, one of the six prefers ground below freezing to
temperate ground, and retains meaningful tolerance far colder. It does not live
there because it cannot eat there.

## What a self-checking claim could not check

The capacity field's headline validation is a ratio: how much more the tropical
band supports than the polar band, measured across a thousand worlds and
preregistered before the sweep. The polar figure is exactly zero often enough that
the metric floors it at one percent of a baseline unit to avoid dividing by zero.
A ratio computed against a floored zero is largely a statement about the floor. The
validation could not have failed, and its own documentation records the floor
without noticing what it implies.

The same measurement is also contaminated in a way its coordinate system hides.
Roughly one world in twenty is tidally locked, and a locked world's warmth is
organised around the point beneath its star rather than by latitude. On those
worlds a tropical-versus-polar comparison cuts across the physics and samples hot
and cold ground alike; they report almost no gradient at all. They sit inside the
pinned average, in precisely the failure mode the chapter claims clearance from.
Obliquity, checked in the same pass, is harmless — this world-builder never draws
a tilt steep enough to invert the gradient.

## What the campaign kept

Two changes to how worlds work, both small. Land became the only hard gate on
productivity. And a dimensionless suitability and a headcount capacity became
distinct types, because a spec had already been written on the belief that one was
the other — a mistake that would have rescaled every world's carrying capacity by
one to two orders of magnitude, and that nothing in the toolchain objected to,
since both were the same anonymous container of numbers.

Everything else the campaign produced is a decision, a measurement, or a design
that was stopped. The stopping is the part worth keeping. A preregistered probe,
written to answer five questions before any of the planned work began, found that
the planned work targeted the wrong one of two gates and would have compiled,
passed, held byte-identity, and done nothing at all — while producing a null
result that looked exactly like a hypothesis the same document had preregistered.
That is the failure a measurement-first discipline exists to catch, and it caught
it for the price of one throwaway test.
