# The Coercion

[The Hand](./the-hand.md) built the seam and left it empty. A body's decision
procedure became a parameter of the tick — GOAP by default, a player's typed
commands when driven — and its own spec said the shape "generalises to a
controller map in Arc III without the body type changing." This campaign is
that generalisation: a body driven by something other than the player, and a
gate that refuses your own in-character acts on it while it lasts.

The founding image, from the program that named this arc: a mind flayer takes
your body. Your commands refuse. You are still there, watching. **The ledger
must not be able to tell.**

## One word, and the count that settled it

The metaplan's own gate table called this row `dominated`. Nothing shipped
used that word in this sense — every existing `dominat*` in the tree turned
out to be unrelated ("dominant species", a coastline "ice-dominated" two
thirds over, a colour "dominating the night sky"). Measured against the
incumbent before choosing: `possess` already ran 747 times through shipped
code and 1,642 through prose; `dominated` ran zero. `usurp` was proposed
during the same brainstorm that objected to `dominated`, and rejected on the
same evidence it argued from — the count decided it, not the taste that
proposed it (decision 0336).

The reason the word matters is not lexical. Taking a body by force and taking
one by invitation are the *same mechanism* wearing two occasions: both replace
a decision procedure with an outside one, both leave the original mind
co-present rather than displaced (The Hand, decision 0226), and both make the
body's acts its own rather than the driver's (The Deed, decision 0168).
Nothing about the type changes when the hand on the tiller changes; only who
holds it does. A second word for the second occasion would have manufactured
a distinction the mechanism itself does not draw.

Charm and command are not this mechanism at all, and settling why closes a
question the metaplan's own taxonomy had left open. Possession *replaces* a
decision procedure. Command *constrains* its outputs — a gate a creature still
runs its own arbitration behind. Charm *modifies* its inputs — a creature that
genuinely wants to help because its valuations moved, refused nothing, gated
nowhere. Building charm into the same table this arc extends would refuse acts
that should succeed; building it into the controller stack would strip a
charmed creature of the very arbitration charm depends on running. Three
mechanisms, three subsystems, and no trait unifies them because the only thing
they share is one English sentence, not a shape (decision 0337).

## A fact that opens and a fact that closes

The ledger never retracts anything, which is usually a virtue and here was
briefly a trap. Sleep, the gate's only existing row, sidesteps the problem by
being *derived* — a wake day compared against the current one, so it
self-terminates with no fact to clean up. A held body has no such luck:
possession needs an event to end it, because Nathan's own ruling on how it
ends — release at the possessor's own option, or the possessed creature's
death — named two *events*, not a schedule. A fixed span was considered and
set aside on purpose; the natural next idea, a duration that scales inversely
with the strength of whoever is held, is a real one and is deferred rather
than built (spec §6).

So possession is an open/close pair, `possessed-by` and `possession-ended`,
read by a fold in ledger order rather than by any stored flag: the last of the
two to appear wins, exactly the shape the occupation predicates already use
for the same reason. A same-day release-then-reseize was the sharpest test of
the fold, and it exposed something about the ledger nobody had needed to know
before this arc: committing the *identical* fact twice — same subject,
predicate, object, place, day, and provenance — is a silent no-op, because the
commit path deduplicates on the whole envelope. A naive reopen would have
looked like it worked and committed nothing. The fix reaches only as far as
`possess`'s own provenance string, which now carries the turn count, and nowhere
else in the ledger needed to change — but the finding travelled: it is exactly
the trap a later verb pair with the same shape would fall into blind.

**Whose possession is this fact about, and the question that seemed obvious
and was not.** The first draft of the model asked the derivation to compare
the possessor against whoever was asking — the natural reading of "held by
*another*." There is no *asker* to compare against. The player has no ledger
identity apart from the body it is driving; asking `Session` who the player is
returns the driven body's own entity, so the comparison would have been
degenerate, refusing the player's own acts on a body it possesses in the
ordinary sense, on every request, forever — precisely the bug this design
exists to prevent, reappearing inside its own remedy. The repair removes the
asker entirely: an open `possessed-by` fact means someone other than the
player holds the body, full stop, because nothing else could put one there.
The variant name, `BodyState::PossessedByAnother` rather than a flatter
`Possessed`, is what carries the relation the signature no longer needs to
(decision 0338).

## The gate, and an ordering that is not incidental

`BodyState` gained a third row beside `Awake` and `Asleep`, joining an
exhaustive match with no wildcard arm — the same discipline that already
turns a forgotten verb into a compile error rather than a silent gap. A held
body refuses every in-character act and permits every out-of-character one,
which is the whole of what the founding image asked for: your commands
refuse, and you can still watch.

Possession is checked *before* sleep, and the order was a real decision rather
than an accident of match-arm sequence. A body can in principle be both
asleep and held at once, and reporting "you are asleep" to a player whose body
has been taken under them names the wrong condition — the possession is what
actually stops the act, and the refusal should say so. A cross-product sweep
over every body state and both moods pins the count this arc's whole gate
change is answerable to: over the six combinations of three states and two
moods, exactly **two** refuse — asleep in-character, and possessed
in-character — and the sweep is mutation-proved in both directions: flipping
the new row's own verdict reddens it, and so does silently dropping the row
from the state roster the sweep iterates, which closed a gap the roster's
own hand-written list had carried since sleep was the only row in it.

## The seam that opens it is a confession, not a mechanism

No creature can take another's body yet. `domains/species` holds no aboleth,
no mind flayer, nothing that decides to possess — the metaplan always deferred
that biology to its own later campaign, and the acceptance test's aboleth was
a placeholder for a capability this program had not built. So the seam that
opens a possession is an out-of-character verb, `!possess`, which the same
design already argues for on independent grounds: out-of-character is what
lets you observe your own condition, whichever condition put you there.
`!unpossess` closes it at the holder's own option, idempotent on whether a
possession is even open rather than on the ledger's own deduplication, because
the two predicates carry no single latest value to compare against.

The death terminator is written and cannot fire. `Body` carries no life
state anywhere in the tree the session touches; the only death on record
belongs to founders baked into deep history, never a body a live session
drives. So `possession-ended`'s `"died"` arm exists in code and is asserted,
by name, to be unreachable — a sweep of every one of the thirty currently
shipped verbs (eighteen in-character, three that end a session outright, and
nine out-of-character operator instruments) confirms no sequence of them ever
produces it. This is deliberate, not an oversight papered over: the day
mortality ships anywhere in this tree, the assertion is what turns red, and
the arm was already sitting there correct, waiting.

## What the ledger cannot tell, measured rather than assumed

The founding image made a falsifiable claim: an act's facts are the body's,
never the driver's, so nothing in the trace should reveal that a different
mind chose. Measured directly — the same walk, the same seed, the same
39-day span rich enough to touch every predicate the drive layer can commit —
a body under the imposed controller and the same body under its own default
controller commit **byte-identical** fact streams. Not merely the same shape;
the same bytes, subject through provenance, because the imposed controller
does not yet do anything but hand the tick to the default one underneath it.
That is a real and useful regression guard — it reddens the day the imposed
controller grows arbitration of its own — but it is worth being honest about
what kind of result it is: deducible from the two controllers' own code
before either fixture runs, not discovered by running them. The founding
image's claim held; the two preregistered tests that were built to check it
hold trivially, because nothing yet exercises the seam they were built to
exercise.

**The result that was not preregistered, and is the one worth carrying, is
sharper than either of those and cuts the other way.** The controller stack
had never actually been wired into where the player's own body walks — its
outputs were unconditionally discarded at the one call site that runs it, so
an imposed controller had nothing yet to impose on. Wiring it there is
ledger-inert, by the same discard, and *not* behaviour-inert: a free body
holds through most of a wait, because the player's own controller returns
nothing queued unless a verb has queued something; a held body does not hold —
it acts, on its own arbitration, the same way every other creature in the
world does. One `!wait 1` at the reference seed is enough to see it start:
free, the body is `Pursuing(Fatigue)` and `Eager`; held, the same body a
moment later reads `Idle` and `Content`. Nothing committed to the ledger
differs between the two, because that walk's facts are discarded either way —
but the body's own felt state, the read `!ask` narrates from, is not
discarded, and it has already moved.

So the honest headline has two halves, and neither may stand alone.
**Possession is invisible in the ledger** — the committed trail a pursuer
could later read carries no trace of whose will was driving, exactly as
promised. **Possession is visible in testimony** — ask the body how it feels
and the answer differs from what it would have said free, because arbitration
genuinely ran differently under the two drivers. "Possession is undetectable"
is false. What is true, and was the actual design bet from the start, is
narrower and stranger: the world's own record cannot convict you, but the
person you rode can still, without knowing why, sound like someone else.

## What stays open

No creature possesses another yet — that waits on the species domain
deciding who can, and why, which this program has always treated as somebody
else's question. A fixed possession span was rejected in favor of two events;
the interesting alternative, a hold whose maximum length falls as the
possessed creature's own strength rises, is recorded rather than built. And
`rider`/`ridden` already live in shipped prose as the narrative register for
a possessor — a register split this campaign found and did not touch, left
for whoever decides whether the field is as clean as one word implies.
