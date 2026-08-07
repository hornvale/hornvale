# The Confidence Gradient

Not everything in Hornvale is equally understood, and this book would mislead
if it presented the settled and the speculative in the same voice. This
chapter is the standing map of which is which — and, unlike the world it
describes, it is meant to be **re-scored as campaigns resolve its questions**,
not written once and left.

The honest axis is not *how established the pattern is* — precedent was never
the thing at risk. It is **whether the world can grade itself on the claim, or
whether the claim rests on a human's judgment.** A bet the Laboratory can
score — generate the evidence, measure it, drift-check the number — is a bet
the project can drive to a verdict on its own. A bet that resolves only
against taste stays open for a structural reason, not for lack of effort.
Confidence still runs bottom-up; but the gradient it runs along is
*checkability*, and any claim about the top of the stack made with the
assurance of the bottom should be distrusted.

One sharpening, learned the hard way (The Named, 2026-07-16): **the
drift-check is not the checkable part — the anchor is.** A drift check pins
output against change and has no opinion about whether the output was ever
right, so a wrong number drift-checks green forever and every regeneration
re-ratifies it. The bets below are self-scorable because their metrics are
measured against something outside the generator (Earth's shoreline
development index, a null control, a preregistered threshold); the
drift-check only stops the answer moving once found. Where a committed
artifact has no external anchor — a rendered page, say — drift-checking it
buys stability and nothing else. The Named's defect sat in a drift-checked
artifact, in plain English, for eight days of green runs.

A second sharpening, and a different failure than the first (The Siding,
2026-07-29): **a check is worth only the configurations it actually runs in.**
The Named's lesson was that a drift check has no anchor; this one is that a
drift check can have an anchor and still never fire, because the command that
*regenerates* an artifact and the command that *verifies* it are never invoked
together. The census sat stale for 139 commits — wrong in three missing
columns and twenty-one drifted metrics — while every gate ran green, because
`make gate` `#[ignore]`s the tier that rebuilds it and `make rebaseline` never
touches it. The Sounding's wall-clock timings sat inside the drift-checked
tree for the mirror-image reason: nothing that rewrites them is ever followed
by the diff. Both artifacts were anchored and both were unobserved. The same
campaign wrote two checks into its own spec that could not have failed — a
zero-diff over a tree containing nanosecond timings, and a claim-status probe
that answered "no" from the wrong machine — and caught both only by running
them. So the gradient has a floor beneath checkability: a claim is only as
checkable as the *pairing* of its generator with its verifier, and an unpaired
check scores as unchecked no matter how good its anchor.

The successor campaign (The Timekeeper, 2026-07-30) built the instrument that
was supposed to close that gap and produced eight more instances of the same
shape *from its own plan text*, four of them inside the machine built to
detect them — including a duration alarm that compared each run against itself
and so could never fire. The count is now sixteen across the two campaigns, so
the floor needs stating as a practice rather than an observation: **the only
thing that reliably distinguishes a check that fires from one that does not is
making it fail on command.** A mutation step — corrupt the input, require the
red — found the never-firing alarm; five further findings came from a final
review that ran the system instead of reading it. Reviewing a check against
its specification cannot catch a specification that disagrees with itself, and
one of these did: the contention guard was wired backwards against a rationale
written three lines above it, and passed review as faithful to the plan.

A third campaign extends the tally in a way that narrows the diagnosis. The
Repertoire (2026-07-31) built a capability probe that touches no world state,
draws no seed and commits no fact — and produced the same family anyway, from
its own plan text: a coverage ratchet that read `REBASELINE=0` and an empty
`REBASELINE=` as permission to rewrite the artifact it was guarding, and a
resolver whose unknown-requirement branch returned *satisfied* rather than
*blocked*, inverting the default-deny posture its own spec had set. Both were
found by mutation — tamper with the input, require the red — and neither by
reading. So the pattern is not a property of measurement code, or of
determinism-critical paths, or of instruments that watch themselves. It is a
property of **plans written as literal code listings**, which get reviewed for
faithful transcription and not for whether the predicate they contain is the
one the spec asked for.

A fourth campaign puts the sharpening where the *repair* is. The Collation
(2026-08-06) produced the same shape from its own plan text — a spec promising
one test asserting a generated matrix's per-column figures equal the per-corpus
reports' own, and a plan listing that asserted only that the rendered document
*contains* each corpus id and each denominator, which any cell reading
`(217/409)` satisfies. Review caught that. The replacement was a whole-file
byte comparison of the matrix against its own committed copy, **proved to fire
by a mutation**, and it passed a second review on that proof — while still
being unable to fail for the reason the spec named, because the two figures
came from duplicated tally code and rebaselining accepts both documents in the
same pass. The whole-branch review caught the second one, and settled it by
running the finding's own scenario: perturb one renderer, rebaseline, and watch
the byte check go green while a cross-derivation test stays red. So making a
check fail on command is necessary and is **not** sufficient: the mutation has
to be the failure the check was promised against, and a check written to
replace one that could not fire inherits the burden of proof rather than the
credit.

## What the world can already check itself on (high confidence)

**The kernel substrate.** Hash-based seeding, coherent noise, append-only
event-sourced storage, deterministic serialization, triple-shaped facts.
Decades of precedent, thousands of implementations, and now stress-tested
under nine domains and five windows without cracking. This layer was
deliberately chosen to be boring, and the choice paid. The substrate has
since grown a shared typed-unit vocabulary — the elevation datum and the
temperature pair, ratified as standing doctrine (decision
0044) — and the way it landed belongs at this
confidence tier for the same structural reason: each migration's central
claim, *this changed nothing*, was scored by the world itself — every
committed artifact regenerated byte-identical — rather than by anyone's
judgment ([The Datum](./chronicle/the-datum.md),
[Temperature](./chronicle/temperature.md)).

That confidence is well placed and slightly too narrow, and
[The Benchmark](./chronicle/the-benchmark.md) says where. "This changed
nothing" is a strong claim about a migration, scoreable by the world — and it
is silent about whether the migration was *complete*. The elevation wave
introduced `ReferenceElevation` and left its sea-level-relative sibling
unbuilt on a stated condition; the condition was later met, nothing watched
for it, and the gap held a real defect that banded three quarters of a world's
land into a marine relief class and published it. A vocabulary can be
byte-identically correct at every step and still be missing the term that
would have made a wrong reading unsayable. So the tier is right about what it
measures and should not be read as covering coverage: the substrate scores
*changes* against itself honestly, and does not yet score its own gaps.

The same substrate now scores its
own *completeness*: [The Correspondence](./chronicle/the-correspondence.md) made
every modeled concept account for its manifestation across the lexical,
perceptual, and cognitive ledgers or record a typed void, so a drift-checked
trial balance — not a reviewer's memory — reports what the world models but
cannot yet name, perceive, or think.

That instrument sat at zero for its whole life until
[The Vernacular](./chronicle/the-vernacular.md) put the first entries in it: nine
concepts declaring, in the registry rather than in a comment, that a star's
spectral class is real and that no culture here can name it. The sharpening is
worth the tier it sits at, because it is about the *instrument* and not the
reading. A trial balance that can report a class and never has is not yet known
to work — and this one, once exercised, immediately found that its own claim
evaporated across a save boundary and that the language layer was minting words
for concepts the registry had just declared unnameable. Confidence here rests on
a ledger having been *made to answer*, not on its having been built.

**The divergence method** — once the year-one research bet, now the project's
own instrument of proof. Generate two worlds differing in a single pin, hold
everything else, and measure whether the downstream culture differs *legibly*.
Year 1 varied the sky: the same land and society under a spinning sky crowned
the cyclic [Wheel-Turner](./gallery/the-gods-seed-42.md), and under a tidally
locked twin crowned the eternal Still Crown — a world with no seasons to
mythologize ([Campaign 5](./chronicle/campaign-5.md); a near-upright locked
world still has none, but [The Wandering Sun](./chronicle/the-wandering-sun.md)
later gave a *tilted* locked world its own libration season, so the Still
Crown reads now as the zero-obliquity limit rather than the whole locked
case). Year 2 inverted it,
varying the observer and holding the sky: two species differing only in their
authored parameter vectors grew different languages and religions, verified by
a 500/500 null control and a blind-attribution metric pinned honest at 0.875
([The Meeting](./chronicle/18-the-meeting.md)). This is no longer *the actual
research*; it is how the research checks itself, and it is applied afresh to
every new layer. [The Pigment](./chronicle/the-pigment.md) applied it to
colour and got the sharpest instance yet, because the observer parameter is a
single scalar: two peoples differing only in night vision descend Berlin &
Kay's ladder to different depths, so the same iron-rich outcrop under the
same light is *yellow* to a goblin and *red* to a kobold — neither holding
the word *brown* that is actually nearest. Self-scorable, and already scored:
the census pins mean hue-depth at 4 and 2 respectively, and flattening the
derivation reddens the claim.

**Population has a physically-grounded, self-checking prior.** Every
settlement used to carry a population number a formula handed it, with no
account of what the land could support and nothing to catch an absurd
total. A carrying-capacity field, closed-form and seed-free, now stands
under every settlement in every world, and its central claim — that
supported capacity tracks the real biomass-by-latitude gradient — is
exactly the kind of bet this chapter cares about: preregistered before the
sweep, measured across two hundred generated worlds (the tropical-and-
temperate band supports roughly 27× the polar band), and frozen only after
the measurement confirmed it, not before. A second guarantee is checked
even more tightly: settlements condense as attractors of a population flow
over that field, so the sum of every settlement's population equals the
sum of the field exactly, by construction, not by tuning ([The
Gathering](./chronicle/the-gathering.md)). This is a genuine promotion, not
a full resolution — see "The standing horizon," below, for the half that
is still ahead.

*Re-scored by [The Vacancy](./chronicle/the-vacancy.md).* The self-checking
half got sharper and the modelling half got narrower, and both belong in the
score. Sharper: a committed readout of where each kind actually lives, plus a
rule that no kind may have zero capacity everywhere, caught four species — the
three chromatic dragons and the owlbear — that had zero carrying capacity on
every cell of every world and had been in the registry, absent from creation,
for four campaigns. A prior that can catch that about itself is doing the work
this section credits it for. Narrower: capacity is a supply term spanning orders
of magnitude multiplied by a condition product bounded in the unit interval, so
an authored ecological niche can only modulate the primary-production signal,
never select against it. A species authored for a particular climate can be
genuinely present there and still rank below species with no affinity for it —
measured, on a people authored for desert that has no desert at all. The
gradient claim and the conservation guarantee are untouched; what is now known
to be beyond the prior is *placing a species where its traits say it belongs*.

*Re-scored down by [The Keeping](./chronicle/the-keeping.md), which contradicts
the sentence immediately above.* The gradient claim is **not** untouched — not
because the gradient is wrong, but because the measurement offered for it could
not have disconfirmed it. The polar term of that ratio is exactly zero often
enough that the metric floors it at one percent of a baseline unit to avoid a
division by zero, so a ratio computed against a floored zero is largely a
statement about the floor. The figure is recorded in the metric's own
documentation, one line from the claim it undermines. Two further problems ride
along: roughly one world in twenty is tidally locked, and a locked world's warmth
is organised around the point beneath its star rather than by latitude, so a
tropical-versus-polar comparison on those worlds samples hot and cold ground
alike and reports almost no gradient — they sit inside the pinned average, in
exactly the failure mode this section claims clearance from. And the productivity
field is not the published model its own documentation cites: that model rises
monotonically with temperature and never reaches zero, while the implementation
is a symmetric tent that reaches zero a little above freezing, which is why no
world is inhabited cold.

What survives untouched is the **conservation** guarantee — the sum of every
settlement's population equalling the sum of the field is by-construction
arithmetic, not a measured bet, and nothing here touches it. What is demoted is
the *evidential standing* of the gradient claim, which is a subtler and more
uncomfortable thing than being wrong: the reading itself is plausible, sitting
inside the band the published model predicts from theory alone. It was the
evidence that was not evidence. This chapter's own standard — preregistered,
measured, frozen only after confirmation — was met in form and not in substance,
and the campaign that found it was looking for something else entirely.

The Vacancy's re-score deserves credit here for seeing the symptom first: it
recorded that capacity is *"a supply term spanning orders of magnitude multiplied
by a condition product bounded in the unit interval,"* so an authored niche *"can
only modulate the primary-production signal, never select against it."* That is
the same defect, named a campaign early. The Keeping supplies the cause — the base
field takes the scarcer of its two limits while the layer above it multiplies four
tolerances together, so one half of the model obeys the law of the minimum and the
other half does not — and measures the resulting compression at roughly fourfold.

**The phenomena interface generalizes.** The bet that one salience-ranked
observation interface could serve religion, perception, and historiography
without any consumer learning which system produced a phenomenon has held
across every domain that has tested it. One caveat corrects the original
forecast: *room description* was expected to ride the phenomena channel too,
and instead took a cleaner road — the semantic query surface, where the sim
emits quantities and the client renders them ([The Scene
Window](./chronicle/21-the-scene-window.md)). The interface is more general
than feared; it is also not the only interface, and that turned out to be the
right shape. The bet has now been confirmed on the *producer* side as well:
[The Elements](./chronicle/the-elements.md) added a wholly new source class —
climate's felt weather — through a `Domain`-trait roster that lets any domain
contribute observations without editing the composition root or a sibling, so
the stream is no longer sky-bound and religion can grow weather-gods where the
land is harsh.

The bet has now also been tested on the channel's *payload*, and the original
shape was wrong in one respect. A phenomenon carried a `description` string
alongside its salience, and the forecast treated that as harmless — prose the
consumer could ignore. It was not harmless, for a reason the interface's own
design implies: an observer context deliberately carries no species, so a
producer cannot know who is looking, and a stored sentence could only ever be
culture-neutral or wrong. The field's *type* guaranteed a leak that no amount of
producer discipline could close. [The Vernacular](./chronicle/the-vernacular.md)
deleted it and moved rendering to the windows, where a speaker is known — and
found the string had been serving as a **sort key**, so its removal reordered
tied phenomena and, through a positional join, moved two deities' periods. The
test written to prove the description was not load-bearing had compared the
gloss *after* the ordering ran, and so had never looked at order at all.

What the bet gets right is confirmed and sharpened: the channel generalizes
because it carries *what was observed*, not *how to say it*. What it got wrong
was assuming a description could ride along inertly. It could not, and the
correction is that a phenomenon now carries a referent and no text. One
qualification stands unresolved: `SkyReport` and `ClimateReport` still carry
domain-resident prose of the same shape, so the guarantee is currently true of
the phenomena channel rather than of the simulation.

The scene seam has since crossed a repository boundary: an
external client now consumes the same documents through a versioned wasm
catalog, byte-identical across platforms
([Goldengrove](./chronicle/goldengrove.md), decision 0055). It has also
started carrying *parameterized* quantities the client evaluates over time —
per-tile temperature elements a viewer reconstructs across the year, with
the seasonal evaluator documented normatively in one place and pinned by a
producer-sourced contract test on both sides of the boundary
([The Isotherm](./chronicle/the-isotherm.md)). The seam holds not just for
static quantities but for the small closed-form functions of them, which is
the more demanding form of the same bet — and
[The Wandering Sun](./chronicle/the-wandering-sun.md) added a second such
function (a locked world's librating-substellar temperature) and, in doing
so, sharpened what "the same function on both sides" requires: not just the
same formula but the same *point of evaluation*. Every earlier cross-seam
value the client read pre-computed off a scene layer, so the producer's
nearest-cell snapping was baked in and invisible; the first value the client
*recomputed* from position diverged from the golden by up to a degree,
because it evaluated at the tile centre while the golden had snapped to a
mesh cell. A closed-form function crosses the seam faithfully only when both
sides sample it at the same coordinate — the fix was a position-based
producer evaluator sampled at the tile centres the client uses, and the
lesson is that the golden must pin the client's computation, geometry
included, not merely its arithmetic. A refinement arrived from the
consumer's side, and it sharpens what the bet does *not* buy: **the seam
holding is not the same as the data being drawn.** A rendering debt had
accumulated silently across three producer campaigns — four layers shipped
and parsed and, in the sharpest case, fully evaluated, with
`circulation_bands` feeding a tested, normatively specified wind evaluator
that rendered no pixels at all. The seam was working perfectly while a
quarter of what crossed it went unseen. That the sim emits a quantity a
client faithfully receives says nothing about whether anyone ever looks at
it, and the discipline this bet still lacks is the check — at a producer
campaign's close, not a campaign later — that some consumer draws what was
just shipped ([The Lens](./chronicle/the-lens.md)).

The mirror image of that caveat arrived next, and it closes the pair: a
consumer can also draw a distinction the producer never made. *The Faces*
shipped `scene/moons/v1` with `bright-icy` as a surface class, selected off a
hash-derived albedo — the client rendering the **word** for an icy moon while
the model held no concept of ice, and deriving every moon's radius from an
**assumed** constant lunar density because composition was never drawn. *The
Reckoning* then drew composition for real, and for the length of one campaign
the repository held **two answers for one quantity**: an icy captured body is
~28% larger at its true density than the contract reported. Unified rather
than deferred ([The Reckoning](./chronicle/the-reckoning.md)). So the bet's
honest statement now has two failure modes on the same axis, and neither is a
seam failure: **the seam holding says nothing about whether the data is drawn
(The Lens), and nothing about whether the data is grounded (The Reckoning).**
A schema is a contract about *shape*, and both campaigns found that shape is
the easy half — a field can be faithfully transported, correctly parsed,
beautifully rendered, and still refer to nothing. The check this bet lacks is
therefore larger than The Lens made it look: not only *does some consumer draw
this?* but *does the producer actually know what it is asserting?*

[The Shadow Track](./chronicle/the-shadow-track.md) took the interface across
a fifth layer — `scene/eclipses/v1`, a *parameterized temporal* query in the
shape of tiles-region: a client asks for a day window and receives that
world's dated eclipses with their solar ground tracks. Two of the bet's open
disciplines got exercised rather than merely restated. The Lens's "does some
consumer draw this?" check was run at the *producer* campaign's own close, and
it earned its keep: the shadow band shipped, parsed, and unit-passed, yet
rendered nothing a viewer could see — it sat at a radius just above the sphere,
beneath the globe's sixty-times-exaggerated mountains, occluded from every
camera angle. jsdom and the geometry unit tests could not see an occlusion; a
screenshot could, and the fix (lift the band above the tallest exaggerated
peak) is a change no non-visual gate would have prompted. And the golden
discipline sharpened in the other direction: the campaign's plan mandated a
committed producer-sourced golden by reflex, but the whole-branch review found
it pinned nothing and contradicted the client's own documented convention. The
distinction the earlier campaigns had blurred is that a golden is the right
instrument only for a value the client *recomputes* (the climate and ephemeris
re-derivations); for a scene document the client merely *parses*, the
end-to-end fixture that reads the real wasm binary **is** the contract, and a
second committed copy only adds a thing to drift. The seam generalizes; the
check that it is *seen* is now practiced at the source; and the golden is
calibrated to the one case that needs it.

[The Turning](./chronicle/the-turning.md) sharpened the *seen* check one turn
further, and in the harder direction. Its diurnal temperature crosses the seam
as another recomputed closed-form function (the client reads a per-cell
amplitude and re-derives the waveform, golden-pinned at the tile centre it
evaluates) — but the finding was about review, not transport. The waveform
shipped physically wrong: it keyed the day/night phase to the *global* fraction
of the day, so the whole planet pulsed in unison instead of a warm band
sweeping per longitude. It passed the Task-1 implementer, its per-task reviewer
(who verified the formula matched the brief *line by line* — and it did), and
three further reviews, because **every one of them checked the code against the
spec, and the spec's formula was itself the error.** Only the producer
campaign's own visual pass — the globe run forward, the lens pulsing the entire
hemisphere at once — caught it. So the check the bet still lacks is larger than
"does some consumer draw this?": it is *does the drawn thing look like the
phenomenon?* A formula can be internally consistent, faithfully transported,
correctly parsed, and **physically wrong**, and no review that treats the
specification as ground truth will see it. The visual pass is the only reviewer
that checks the model against reality rather than against the plan — which makes
it, for physical fields, not a courtesy at the end but the gate that closes the
loop.

[The Gyre](./chronicle/the-gyre.md) carried the seam across a boundary it had
not yet crossed: the first **vector** field (an ocean current, two tangent
components per tile, where every prior layer was a scalar) and the first the
client does not merely colour but **advects** — particles swept along the field
as motion, the living globe's first real animation over a
deterministic keyframe. The interface held: the client's tangent frame is the
exact inverse of the producer's, so the flow points where the sim says. And the
*seen* check earned its keep a second time, in a gentler register — not a
physics error this time but a legibility one: the field shipped correct and
nearly invisible (faint one-pixel specks), and only the screenshot showed that
"transported and parsed" is still not "read." The bet's honest statement now
carries three failure modes on one axis — the data can go undrawn (The Lens),
ungrounded (The Reckoning), or drawn-but-illegible (The Gyre) — none a seam
failure, all invisible to everything but a human looking at the picture.

[The Selvage](./chronicle/the-selvage.md) added a fourth, and it is the one
that reaches furthest back toward the producer. A map tile's samples crossed
the seam correctly, were parsed correctly, and were drawn correctly — and the
client still assembled two adjacent tiles at the wrong edges, because the
*geometric convention* that makes them assemblable appears nowhere in the
document they arrive in. The producer walks a parameter across a tile to lay
out its rows, and that same parameter counts the tiles; so a tile's row axis
and the tile grid's own axis must run the same way, and a tile's last row of
samples is bit-identically its neighbour's first. Every word of that is true
of the contract and none of it is *in* the contract: the client had to
re-derive it from the producer's source, got the sign backwards on one axis,
and produced a discontinuity that could not exist on the real planet. So the
data can also go **drawn-but-mis-assembled** — each document faithful, the
composition of two documents wrong. The bet is unharmed (nothing crossed the
seam incorrectly) but its scope is now clearer: a versioned scene document
carries values and says nothing about the geometry that relates one document
to the next, and a consumer holding several at once is re-deriving that
relationship whether or not anyone wrote it down. The check this suggests is
cheap and not yet practiced — a contract that ships more than one tile should
state its own adjacency convention, rather than leaving each consumer to
reconstruct it from the generator.

[The Snapshot](./chronicle/the-snapshot.md) moved the seam onto an axis it had
never been tested on. Every layer before it was a **query**: a client asks the
world about a place or a day window and receives a document describing what is
there. `vessel/session/v1` is an **emit** — one document per committed turn of
an interactive session, and not a view *of* the world but a view *from* an
agent inside it, which means the interesting part is what it must withhold. It
carries the redaction boundary in its own shape: channels grouped by how the
agent came to hold what it holds, rather than fields tagged with provenance, so
a pane that reads one channel does not decline to look outside it but cannot.
The seam held on the new axis, and was proved the honest way — by moving the
one pane that already existed off the prose interface and onto the document,
where it printed the same bytes it printed before, rather than by adding a
second pane that nothing could contradict.

The campaign also refines the golden rule [The Shadow
Track](./chronicle/the-shadow-track.md) had just sharpened, and the tension is
worth stating rather than smoothing: that campaign concluded a committed golden
is the right instrument only for a value the client *recomputes*, and this one
committed a golden for a document its client merely *parses*. The distinction
that survives both is not who reads the artifact but what the artifact pins.
The Shadow Track's dead golden pinned a *client contract* the end-to-end wasm
fixture already pinned better. This one pins something no client-side test can
reach: `vessel/session/v1` is declared save-format-class, so a change in what
its bytes *mean* is an epoch event, and the committed fixture is the tripwire
that makes such a change arrive as a reviewable diff instead of arriving
silently — its own failure message names the epoch decision. An in-process
determinism test catches nondeterminism; only a golden held across code changes
catches a meaning that moved. It earned its keep on the day it was written, by
exposing a negative zero that had been folded into every unprovoked NPC's
grievance for months and was invisible to the equality test that guarded it.
And it carries a tie no end-to-end fixture could express: the newest channel in
the newest schema asserted byte-identical against the *oldest* committed golden
in the book, the published seed-42 possession transcript. The end-to-end check
exists too — the wasm smoke driver asserts the schema tag, every channel, and
narration equality against the real binary — so the campaign holds two
instruments rather than one duplicated. The cost The Shadow Track warned of was
still paid: the fixture needed a rebaseline during the final fix wave, and any
second copy of a document must be regenerated whenever the document
deliberately changes, with a rubber-stamped regeneration as the standing
hazard. Here the regeneration *was* the instrument working, and what forced it
belongs to this bet's own tally. A schema is a contract about shape, and shape
does not include **range**: the agent id was faithfully declared, faithfully
transported, faithfully parsed, and quietly truncated on arrival, because
JSON's one numeric type is a float and the producer's identifier is a
full-width 64-bit integer — wrong by 296 for seed 42, and wrong for nearly
every world. It printed correctly, which is why it survived three separate
per-task reviews: the integer, the WebAssembly boundary, and the TypeScript
annotation each sat in a different file, and no single-file review holds two at
once. The three failure modes above are invisible to everything but a human
looking at the picture; this fourth one is invisible to anything smaller than
the whole seam — and unlike them it is *mechanizable*, since nothing in the
ladder yet checks that an integer crossing into JSON fits a double. So the rule
the two campaigns jointly support: **a golden for a value the client
recomputes, or for a save-format-class document whose meaning changes are epoch
events; the end-to-end fixture for a document the client merely parses.** Where
neither holds, a second committed copy is only a thing to drift.

**Re-scored by [The Panes](./chronicle/the-panes.md) (2026-08-06): the bet
The Snapshot deliberately declined to take has now been taken.** That campaign
proved the emit seam by moving an *existing* pane onto the document, and said
plainly why — a second pane that nothing could contradict would have proved
nothing. The Panes added the second pane. It is the harder direction, because
the redaction boundary is only as real as the first consumer that could have
violated it and did not: a map pane is precisely the pane most tempted to
reach outside its channel for world truth, and the shape of the schema is what
stops it. The channel carries **semantic content, never a picture** — cells,
not glyphs — so the sim never learns how anything is drawn, and the client
renders from one document rather than from two sources that could disagree.
Both panes are now pure functions of one snapshot, which is the structural
form of the claim rather than a discipline anyone must keep.

Two things sharpen the score rather than merely confirming it. First, the cost
was **measured, not asserted**: a session-level benchmark this campaign built
prices the emit at 1.249 ms against a 0.173 ms baseline, and the payload
growth is band-dependent in a way a single figure hides — 2.73× out of doors,
1.17× indoors. The bet's premise is that the emit is cheap enough to pay every
turn; that is now a number rather than an expectation, and it paid down a
re-measurement another campaign had left owed. Second, the bet's *weakest*
seam showed itself at the merge, not during the work. A tagged union over
bands is an enumeration of another part of the sim's state space, and a
parallel campaign added a band to that space while this one ran. The merge was
textually clean; nothing in either campaign's documents mentions the other's
surface; the two agreed only because both happened to guard on the same
condition. **The generalisable lesson is that an emit whose shape mirrors
sim state inherits that state's growth, and no gate asks whether the mirror is
still total.** The seam held, and it held for a reason no test had stated —
which is the kind of pass worth recording as a narrower confidence, not a
wider one.

**Re-scored again by [The Sighting](./chronicle/the-sighting.md) (2026-08-07):
the redaction boundary stopped being merely structural and started
withholding.** Both prior tests proved the boundary by building panes that
*could* have reached outside their channel and did not; nothing had yet
required the sim to remove something a pane would otherwise have shown. This
campaign does, and the score improves for a reason that is not the one the bet
anticipated. The withholding turned out to be far harder to make **total** than
to make correct: the narrowing predicate was right in its first commit and
still leaked four times, through `examine`, a needs report, a provoke line, and
a tick's motion narration. Every leak was a surface that *narrated* a creature
rather than one that *returned* one, so none of them appeared in any
enumeration of the channel's readers. The generalisable form is that
**introducing an invariant silently promotes every existing reader of the
underlying data into a potential violation of it, and nothing in the repository
enumerates that set** — the emit seam's own shape does not help, because the
last mile of every channel is prose, and prose cannot be audited for what it
happens to mention. The structural claim survives and is now load-bearing; what
narrows is the confidence that a structurally-correct boundary is
automatically an *observed* one.

Two smaller corrections the same campaign forces on this chapter's arithmetic.
The bet's cost premise has been priced through the **actual** wasm boundary for
the first time — a turn measures 1.57–1.78× native, not the 3.6–3.8× every
derived browser figure here was multiplied by, so the seam is roughly twice as
cheap in the browser as this chapter had assumed. And the payload growth this
campaign added is eleven bytes, against a per-turn derivation cost of about
3.7 ms in release: the bytes were never the term worth watching, the derivation
is.

Every test of this bet so far has pushed on the *producer* side — new source
classes, new layers, new document shapes. [The Vigil](./chronicle/the-vigil.md)
pushed on the **observer** side instead, and the interface took it without
modification. The observation machinery — the lens, the characteristic hour,
the salience ranking — was authored for settling peoples, every one of which
has a place, a society, and neighbours. A dragon has none of these: it never
settles, so the exposure classifier that feeds it finds no biome, no
neighbouring kind, no hearth. It nonetheless observes through exactly the same
path, and the ranking it gets back is visibly its own — lunar eclipses outrank
the sun for a crepuscular creature with a dark-adapted eye. That the consumer
never learns which system produced a phenomenon is the half of this bet that
was already well tested; that the *observer* need not be a member of a society
for the interface to describe it is the half that had never been exercised,
because until now every observer was.

[The Purview](./chronicle/the-purview.md) added the sixth scene kind and the
first **egocentric** one — and with it the first document that carries an
*epistemic* field, since a situated scene describes what an observer knows and
not merely what is there. The structural news is not the schema, though, but
the scheduling: this is the first layer where the producer and a consumer that
draws it shipped in the **same campaign**, which made the "does some consumer
draw this?" check the bet has been asking for since The Lens not a discipline
to remember but one that could not be deferred. It paid immediately and in the
direction nobody was watching. The chart resolved a biome by matching the
climate domain's kebab-case name against the locale window's spaced one, and
every multi-word biome had been quietly resolving to index zero — on seed 42
all thirty-one cells reported *ice* for a tropical seasonal forest. Single-word
biomes matched by coincidence, which is why months of green tests had said
nothing. That is a **producer-side** error in a seam that had already shipped,
found only because something finally drew it; the repair was to compare enums
rather than strings, deleting the round-trip that was the defect class. The
visual pass earned its keep a second time in the same campaign, on the render
rather than the data: the chart passed every assertion while drawing a leaning
parallelogram, because the screen projection did not cancel the lattice's row
offset. So the tally of ways a faithful seam still fails gains a sixth, and it
is the mildest-sounding and the most general — **the picture can misstate the
geometry of a document that is entirely correct**, and no test written against
the document can see it, because the document is not what is wrong.

The bet itself is unmoved and, if anything, better supported: six kinds across
cartographic, temporal, orrery, session-emit, and now situated poles, none of
which required the interface to change. What keeps sharpening is the ledger of
things the seam holding does *not* buy — and the one lesson that now recurs
often enough to be a rule rather than an anecdote is that every entry on that
ledger was found by a human looking at output, never by a test.

[The Occlusion](./chronicle/the-occlusion.md) confirms that rule from outside
the scene-document program entirely, and in the plainest register available:
its four defects were found not in a picture but in **prose**, by building the
CLI and reading what it said. The almanac had been opening for most of the
project's life by naming five stars beneath a flat overcast — a sentence that
contradicts itself inside its own span — and `possess` printed `Ways on: SE,
N, SW.` and then answered `No verb 'se'`. Neither is visible in a diff, because
in both cases each half is correct in isolation: the compass parser accepted
the token and the dispatch arm that reaches it was simply absent; the weather
was computed correctly and appended after the sky was already described. A
codebase with zero TODOs across a hundred thousand lines, a default-deny type
audit, and a 2,319-test gate said nothing about either. So the visual pass
generalizes to a **legibility pass**: for a project whose deliverable is prose
about a world, reading the output is a distinct instrument from testing it, and
the same one that catches a leaning parallelogram catches a sky that argues
with itself.

The campaign also put a sharper edge on what "verified" buys. Its spec claimed
the change could not reach the save format, and *checked* that claim — the sky
report carries no serializer, confirmed by reading the derive rather than
assuming it. The check was sound and the conclusion was false, because the
exposure did not run through serialization but through genesis, where a
people's gods are derived from the sky they observe. Wiring occlusion into the
observation path cost seed 42 twenty-three of its forty-eight deities while
every gate stayed green, since the gate pins facts against the current build
and not against history. What caught it was the cheapest possible instrument,
and one no schema discipline implies: build the world before and after, and
compare the bytes. The ledger's entries were all *found by looking*; this one
adds that a determinism claim is only as good as the route it was checked
along, and that the total check — same seed, both binaries, `cmp` — costs
ninety seconds and subsumes the clever ones.

[The Sextant](./chronicle/the-sextant.md) adds a seventh entry to that ledger,
and it is the first one an instrument found rather than a human looking. A
scene document can be faithful, grounded, drawn, legible, and correctly
assembled with its neighbours — and still be **ruinously expensive to ask
for**, because a versioned schema is a contract about a document's *contents*
and says nothing about the cost of producing one. Every terrain-facing entry
point in the scene window re-derived terrain and climate from the world and
kept neither, so each such document carried about 638 ms of fixed setup;
measured against the
Orrery's real call pattern, which requests one regional document per
level-of-detail tile, **91.6% of a scene call was the planet being rebuilt**
and a single camera move spent roughly fifteen seconds generating the same
world two dozen times. ([The Cistern](./chronicle/the-cistern.md) closed that
the following day — the derivation now happens once per world, and a region
patch measured 11.1× cheaper. The entry stays on the ledger because the
*failure mode* is what it records, not the defect's lifetime. [The
Winnowing](./chronicle/the-winnowing.md) then took the residual The Cistern
named: the globe document's cost is no longer redundant *derivation* but sheer
*volume*, and the schema said nothing about that either. A caller may now name
the per-tile layers it will read — the eight the Orrery's parser actually
extracts are 46.3% of the bytes — which is the same lesson one turn on. A
contract about contents says nothing about the cost of producing a document,
and it says nothing about which parts of one a consumer will use.) The bet is
untouched — nothing crosses the seam incorrectly,
and the interface required no change to be measured. What sharpens is the
same scope lesson The Selvage drew about geometry, transposed to cost: a
document describes itself and not its relationship to the *other* documents a
consumer holds, and a consumer's calling pattern is exactly such a
relationship. So this entry is also the ledger's counter-example to its own
recurring rule. Six failure modes were found by a human reading output; this
one is invisible to reading — every document is correct — and visible only to
a fixture shaped like the consumer's session, since redundancy is a property
of a sequence of calls and cannot appear in any one of them.

[The Pyx](./chronicle/the-pyx.md) adds an eighth entry, and it generalizes the
ledger's own recurring rule one step further. The Cartographer's lesson was
that a determinism claim is only as good as the *route* it was checked along.
The Pyx's is that it is only as good as the **apparatus** it was checked on:
every check in this repository regenerates on the canonical box and compares
against a golden authored on the canonical box, so the machine sits on both
sides of the comparison and cannot be what the comparison detects. In the
vocabulary metrology uses for exactly this distinction, the project had been
enforcing *repeatability* and describing it as *reproducibility*. The audit
that closed the gap found nothing wrong — a full census reproduced on its
authoring host eleven days later with zero bytes different, two clean builds
of one commit hashed identically, and a forty-world all-metric probe was
byte-identical between x86_64/Linux and aarch64/Darwin, including the one
seed whose count decision 0063 had recorded two
machines disagreeing on. The bet on deterministic serialization is
**strengthened, and for the first time by evidence from outside the machine
that authors the goldens**. What sharpens is the scoring instrument rather
than the claim: the cheapest sufficient check turned out to be a comparison of
*binaries* rather than of outputs, which nobody had tried and which the
campaign's own frozen prediction said would not work.

That instrument then needed a correction of its own, and the correction
belongs on this ledger as much as the entry does. Two builds of one commit in
two directories hashed identically on the canonical box, and the campaign
generalized from that single host to a property of the toolchain. Repeating
the comparison on the second machine produced two *different* binaries, each
carrying the absolute path it was built in — a path written in deliberately by
ordinary code asking where its own source tree is, not by debug information as
first supposed. So the oracle is real but conditional: it holds when both
machines build at the same absolute path, which an image supplies for free and
an ad-hoc checkout does not. The entry's shape is therefore the ledger's rule
turned on the ledger's own author — a claim verified on one apparatus is a
fact about that apparatus, and the campaign that had just finished saying so
in prose went on to forget it in a decision record within the hour.

[The Twin](./chronicle/the-twin.md) closes that correction and sharpens the
bet itself. Holding the compiler, the system library, and the build directory
fixed, two machines that share almost nothing else — different processors,
kernels, and operating systems, one of them an appliance that cannot be logged
into — produced the same binary to the byte; and two *different* system
libraries produced the same forty worlds, even compiled the old way, where the
operation everyone suspected leaves the program and enters the library. So the
long-standing explanation for the one recorded cross-machine disagreement is
eliminated, and with it the machine itself. **What determines the output is
the environment, not the host** — which is the strongest form this bet has
been stated in, and the first version of it supported by a comparison in which
only one thing varied. Every earlier cross-machine check in this project moved
five things at once, which is why the disagreement of nineteen July was
observable for four months and diagnosable for none of them. It is still
unexplained. The space it can hide in is now small enough to name: how a build
chooses its compiler, given that this project's pin is silently conditional on
the directory you invoke it from.

**Re-scored by [The Blocking](./chronicle/the-blocking.md) (2026-07-28): one
entry on that ledger is now mechanized, and the move that mechanized it is
worth more than the check.** The entries above are all forms of *the drawn thing
does not match the thing* — undrawn, ungrounded, illegible, mis-assembled,
misstated geometry — and the standing lament is that only a human noticed. The
Blocking's parity contract turns one sub-class into a test: **every noun the
render depicts must answer to `examine`, and every destination it depicts must
be reachable by a named command.** That is precisely the class of defect that had
shipped one campaign earlier, where `look` named a water jar and `examine`
denied it, and it is checkable because the render and the command language are
required to derive from *one* model rather than to agree by vigilance. The
structural half is what makes the tested half possible: a pane input
**synthesizes a command** — an arrow key emits `go n` and the existing verb runs
— so there is one implementation and no second path to drift from. The accepted
cost is permanent and is the reason this is a bet moving rather than a feature
landing: any future pane capability must first be a verb, so nothing will ever be
expressible only by pointing.

The honest scope: this does not close the ledger, it converts one row. A plan
whose every glyph answers can still be *ugly*, and legibility remains
taste-checked — the campaign's own render had to be reworked once because a model
that was faithful drew no walls at all, which no assertion caught and a human
reading the picture did. What changed is that "the render depicts something the
command language denies" has stopped being a thing a human must remember to look
for.

**Terrain shape has Earth-anchored, self-checking acceptance bands, and the
one that stayed open resolved by superseding its own instrument rather than
closing under it.** The Measured Coast preregistered six Earth-anchored
shape metrics (shoreline development, hypsometric bimodality, shelf
fraction, continent count, largest-continent share, plate-size Gini) before
any generator change, exactly the kind of bet this chapter cares about: the
Laboratory generates the evidence, measures it, and drift-checks the number,
with no human judgment call about whether a continent "looks right." Crust's
epoch closed four of six by direct measurement, refuting two of its own
predictions along the way (the tanh-lobing pinch-off hypothesis, the craton-
repulsion hypothesis) before a read-only probe found the actual cause
(sea level sitting in the abyssal plain). Sculpting's epoch closed a fifth —
shelf-fraction, via a wave-cut coastal-erosion mechanism the tuning season
built only once measurement showed the band demanded it — and left the
sixth, shoreline-development, on an honest open verdict: every mechanism the
spec banked for it is now built, the metric moved a real +8% under
measurement, and it still sat below its floor. A dedicated diagnostic
instrument established *why* the floor was hard to reach — the estimator is
not saturated, but the floor's own anchor was partly built on coastline
noise a since-removed generator had produced — and handed the open band
forward, with its evidence, to a named future campaign.

That campaign, rift-and-fit, resolved the question, but not by closing the
band. Its own fitted, continental-scale rift moved the metric the wrong way
(lower than Sculpting's, not higher), which turned out to confirm the
diagnostic rather than refute it: the estimator rewards single-hex-scale
coastline texture almost exclusively, so a large-scale geometric fit was
never going to move it much, and the one lever that does move it (cell-scale
texture) hits a hard fit-verification ceiling before it can close the gap.
The campaign then measured the real planet through the exact same,
unchanged estimator for the first time — and Earth's own coastline scored
*below* the floor every generated world had been held to. A floor no real
planet clears is not a floor; it is the contaminated anchor the diagnostic
had already flagged, now proven. The band was superseded, not closed: an
Earth-anchored range now serves as a sanity floor rather than an acceptance
gate, and single-scale coastline complexity stepped down from this
project's headline shape criterion to a tripwire against degenerate output.
The more interesting bet this surfaced is banked, not built — a coastline
score is the wrong shape for a pass/fail band in the first place, because a
coast is a variable meant to *vary and drive something else* (habitat edge,
harbor geometry, a maritime-versus-continental cultural split) rather than
sit at a constant the Laboratory checks once and forgets. This is the
chapter's discipline working exactly as intended, one step further out: a
bet stays open for a structural reason, and when it resolves, it can resolve
by convicting the instrument instead of the world. See
[Crust](./chronicle/crust.md), [Sculpting](./chronicle/sculpting.md),
[Rift-and-Fit](./chronicle/rift-and-fit.md), and [The Census of Coasts
IV](./laboratory/census-of-coasts-iv.md).

[The Threshold](./chronicle/the-threshold.md) exercised this discipline on a
bet of its own and got the answer the discipline exists to make possible: **no**.
It froze, before a line of code, the claim that a cold creature with a fire in
its house would suffer the cold measurably less than one without — and then
failed to find it, four times over. What makes that worth recording is not the
failure but its shape. Each null was designed to kill one candidate explanation
and did: the fire was too faint (so it was recalculated from an energy balance —
envelope, infiltration, hearth power, the radiant crowding that is why people
sit close — the argument written down and committed *before* anything was
measured again); then the creature never reached the fire (so it was taught to
cross the room, and did); then the instrument could not see where the creature
stood (so it was taught to look). Warmth, walk, and witness each eliminated in
turn, the remaining explanation is not about the machinery at all: **the
creatures who live where it is cold are either already within their own
tolerance, or forty to eighty degrees beyond anything a domestic fire could
offer.** There is nobody in between for a hearth to save.

Two things follow for this chapter. The first is that a preregistered
prediction is only as good as the *sequence* of measurements behind it — a null
that eliminates nothing is a wasted run, and four that each eliminate something
are a result. The second is subtler and concerns the anchor: this bet's own
warning is that a drift check pins output against change and has no opinion
about whether the output was ever right. Here the analogue bit at the level of
the *instrument* — an acceptance protocol verified byte-identity with a command
that could not, by construction, reflect the layer being changed, and four
stages of evidence were vacuous before anyone noticed. The check that a
measurement can move at all belongs beside the measurement, not after it.

[The Mire](./chronicle/the-mire.md) exercised the same discipline on a bet
about weather and world structure that no earlier chapter entry had staked,
and it too came back **no** — a double falsification rather than a single
one. It froze, before any code existed, that a weather-gated modifier on the
connection graph's edge conductance (mud and snowpack lowering it, frozen
ground raising it back) would move the passable fraction of the world's
connection graph by a global, latitude-graded amount: at least a 5% median
swing across two hundred generated worlds (the systemic-effect bet), growing
toward the poles where weather is harshest (the where-it-shows-up bet).
Neither held. The measured median swing is **0.95%**, an order of magnitude
under the floor, and the swing that does exist runs backward: equatorial
cells swing furthest (0.0224), temperate cells less (0.0021), and polar
cells swing **exactly zero** — not merely small, zero on the nose, across
every sampled seed.

The mechanism is the durable part, and it generalizes past this one
measurement: **seasonal variation lives where conditions alternate, not
where they are extreme.** A permanently frozen polar cell has one season,
all year, so it never crosses the conductance threshold in either
direction; an equatorial cell, wet in one season and dry in the next, is
exactly the alternation the instrument can detect. Extremity without
alternation is stasis. Two checks confirmed the null was real rather than
the instrument being blind: a synthetic all-or-nothing probe (every land
edge fully open one day, fully closed the next) registered swings ten to
twenty times the measured median when an effect of that size was
deliberately manufactured, and across a full year only about 4% of real
land edges ever cross the passability threshold at all — most of the graph
is simply always-open or always-closed, regardless of season, which is the
mechanistic reason the systemic swing is small. This null joins the
chapter's growing record of preregistered predictions that came back no —
alongside the fire-warmth bet above and the conflict-cascade criticality
bet below — each recorded as a finding rather than a failure, because a
chapter that only reports confirmations is measuring taste, not the world.

What the null does not settle is stated in the chronicle rather than
smoothed over. The measured quantity is **passability** — whether a route
is open at all — not **cost**, how much slower or harder a route becomes
while it stays open; a large seasonal cost effect could sit entirely
beneath this instrument's threshold-crossing view and be invisible to it.
And the result is a claim about **land only**: water edges were
deliberately left ungated this campaign, so "the poles do not vary" may be
true for land and false for the sea ice that borders it, on coastlines
whose land itself never varies because it is permanently frozen.

## Precedented but nontrivial (moderate confidence)

- **Lazy retrospective generation** — committing detail only on observation,
  consistent with a statistical prior. *Caves of Qud* and *Ultima Ratio
  Regum* prove pieces of this can work; nobody has done it against a
  fields-plus-ledger substrate at this scope, and the observe-then-commit loop
  is not yet built. Its self-scorable half is named below.
  **Re-scored by [The Lintel](./chronicle/the-lintel.md) (2026-07-27): the bet
  moves halfway, and only halfway.** The phrase names two mechanisms, and the
  campaign shipped exactly one of them. *Derive-on-demand* now exists at the
  finest band the world has: a chamber's existence, its interior and its prose
  are a pure function of the derived brief, the address and the seed, computed
  when a player walks in and discarded when they leave. Against ~4^9 candidate
  addresses under a single locale, that is the statistical-prior half working at
  its intended ratio — the overwhelming majority of the space is never
  materialized because existence is a predicate rather than a given, and it is
  asserted by test that it stays that way. *Commit-on-observation* does **not**
  exist and was deliberately excluded: The Lintel commits nothing at all, which
  is precisely what preserves byte-identity — the player's position has never
  been a committed datum, so descent needed no schema change and no epoch.
  Promotion-on-touch — the write half, where an observed detail is *kept* — and
  the delta store it implies remain unbuilt, and are the harder half, since they
  are where a lazily generated world can begin to contradict its own prior. So
  the bet's confidence in *derivation* is materially higher than it was, and its
  confidence in the *loop* is unchanged.
- **Coarse constrains fine.** The design principle — a `ConstantSun` and a
  generated star system are both valid; higher fidelity refines and never
  contradicts lower — *shipped*, and holds from astronomy through religion's
  tiers. Crust sharpened it into a stated contract (decision
  0038): the terrain quantities that are
  *pointwise* — crust thickness and age — are stateless `Field`s any grid may
  resample, while the *mesh-bound* ones (sea level, drainage, placement)
  compute once on the world's canonical grid. So the pointwise half of the
  substrate is now genuinely resolution-free: the render lens samples the
  elevation field below cell scale, and the crust field byte-agrees across
  nested grid levels. The *Dwarf Fortress* move it is sometimes conflated with —
  runtime level-of-detail, refining an *active region* on the fly with the seams
  kept invisible — was the mesh-bound half the field/grid line isolated as the
  remaining work, and [The Room Mesh](./chronicle/the-room-mesh.md) has now laid
  its foundation. A room is a triangular face of the *same* icosphere refined
  deeper, so a level-7 room literally *is* a level-7 triangle: the seam problem
  that made active-region refinement look risky is dissolved structurally, not
  patched, and the dissolution was oracle-validated to `max|Δ| = 0` across all
  327,680 faces of a level-7 globe. Local detail is now summonable per-address
  at arbitrary depth for zero global cost, through an O(1) integer neighbour walk
  and coarse-field inheritance hooks. What is *not* yet built is the layer that
  consumes this substrate: the runtime active-region swap itself, its delta
  store, and the spike-validated adaptive-depth walk that lifts the uniform-depth
  restriction — all deferred, all resting now on a substrate that exists. The bet
  has moved from *no mechanism* to *mechanism shipped, composition pending*.
  [The Region](./chronicle/the-region.md) shipped the first cross-repo
  realization of the pointwise half — a regional tile query
  (`scene/tiles-region/v1`) that samples and barycentrically interpolates the
  continuous fields at arbitrary on-tile density, fed to a client that builds a
  registered globe patch from a tile's address. In doing so it drew the honest
  line the phrase *resolution-free* had blurred: the fields are free to *sample
  and smooth* below cell scale, but carry no sub-cell *information* — the
  ~110 km canonical cell is the physics floor, and interpolation beneath it is
  cosmetic, not fidelity. The active-region swap the bet still awaits inherits
  that boundary: it can refine geometry indefinitely, but never invent physics
  the cells do not hold. [The Massing](./chronicle/the-massing.md) deepened the
  *client* half of that consumption — the globe's level-of-detail ceiling lifted
  (a purely client-side reach for the finer region tiles the substrate already
  served) and the camera's own floor lowered to meet it — and, more pointedly,
  gave the cosmetic-versus-fidelity boundary a *renderer that shows it*. A voxel
  globe draws one block per cell and no gradient between, so the ~110 km floor
  reads as the visible edge of a block rather than hiding inside a smooth slope:
  the honest instrument for the very question of whether the cells' own
  resolution — not the client's — is the next floor worth deepening. That
  producer-side deepening stays deferred; what The Massing added is the
  instrument to judge when it is owed.
  **Re-scored by [The Lintel](./chronicle/the-lintel.md) (2026-07-27):** the
  substrate now has a *second occupied band*. A possessed body can stand at
  nine refinements below the walk band — one address space, a longer path — so
  the uniform-depth restriction is lifted in the narrow case the two-band
  vocabulary defines, and band changes are confined to visible thresholds
  precisely to avoid the thrashing an automatic adaptive-depth walk would
  reintroduce. This does **not** breach the ~110 km physics floor the row draws:
  a chamber's content derives from the *committed occupation history* of its
  walk-band ancestor, not from interpolating fields beneath cell scale. The
  distinction is worth keeping sharp — refining geometry below the floor stays
  cosmetic, while refining *what is recorded to be there* is fidelity the
  ledger already holds. The runtime active-region swap and its delta store are
  still unbuilt.
  **Re-scored by [The Blocking](./chronicle/the-blocking.md) (2026-07-28): the
  principle now has a *number*, at the finest band, and that is the largest
  movement this row has had.** "Higher fidelity refines and never contradicts
  lower" has always been checked by *agreement* — a field resampled at two grid
  levels must byte-agree — which tests that the fine layer does not disagree with
  the coarse one. It says nothing about the fine layer **inventing**. The floor
  plan is the first fine layer whose entire content is a lowering of a coarse
  structure (an anchor graph of chambers and links, itself derived from committed
  history), so the question became answerable in the other direction: how much did
  the fine layer add? The embedder reports its **residual degrees of freedom** and
  the checker compares that number against how much freedom the graph leaves free
  — and it is *exact*, not merely bounded, at every chamber count over two
  thousand seeds. Being **under** budget is a finding too, since it means the seed
  is not filling freedom the graph genuinely left. That converts the principle
  from a design intention into a measured property of one derivation, and it drew
  a line the phrase had left implicit: a plan's extent derives from chamber count
  alone and *spends no draw*, because a coarse constraint that consumes randomness
  is not a constraint, it is another generator.

## Genuinely open — split by whether the world can grade itself

The remaining low-confidence bets do not sit at one altitude. Each has a
**self-scorable half** the Laboratory could close on its own, wrapped around a
**taste-gated half** that waits on a human read. Naming the seam is most of the
progress.

The split is no longer hypothetical: The Chorus drove one bet of exactly
this shape to a verdict. Whether a derived cultural account differs from
ground truth *as a worldview* (not merely in vocabulary) looked
taste-gated until it was decomposed into distinctiveness ×
recoverability; the preregistered known-groups gate then separated the
uncanny pole from the gibberish pole from the shipped voices on every
measured world ([Study 012](./laboratory/study-012.md)). The residue that
stayed taste-shaped is exactly what the decomposition predicted:
*is it pleasant to read* — a far smaller surface than *is the worldview
right*. That is the template the bets below should expect: the
self-scorable half closes by instrument, and the taste half shrinks to
its honest size.

1. **Refinement at scale.** Generating detail consistent with fields *and* a
   large committed ledger, with aesthetic requirements on top, is constraint
   satisfaction plus taste — and the two halves have very different horizons.
   *Consistency* is self-scorable today: a generated detail either violates a
   committed fact or a field prior or it does not, and that is a metric, not a
   judgment. *Aesthetic quality* is taste, and it is the half that is years
   away and may need ideas that don't exist yet. The honest move is to build
   the consistency tier — checkable now — and stop letting the taste half make
   the whole problem look untouchable.
   **Re-scored by [The Wearing](./chronicle/the-wearing.md) (2026-07-29): the
   taste half shrank, exactly along the template this section's preamble
   predicts.** *A generated place name is too long to say and too uniform to
   believe* reads like pure taste, and had been treated that way. Decomposed,
   most of it was not. **Length** was already instrumented and the instrument
   was being ignored — the metric's declared buckets stopped at 10 characters
   and every world in a thousand-seed census overflowed them, silently, for
   several campaigns; a declared bucket range nothing enforces is an intent, not
   a check. **Syllable count** is a second instrument the campaign had to add,
   because character length cannot separate *shorter words* from *the same
   words spelled tighter* and the diagnosis turned on precisely that
   distinction. **Transparency** is the interesting one: the property that made
   the names read as generated was not any name's opacity but the *uniformity*
   of their readability — 650 of 650 names fully glossable, by construction —
   so the metric that closes it is a **distribution witness whose target is
   explicitly not its maximum**, and whose comment records that a drift back
   toward 1.0 is a regression. Three self-scorable readings where the honest
   prior expectation was one human read. What did **not** move is the residue
   the decomposition leaves: whether a given name is *pleasant*, and whether a
   world's toponymy reads as inherited rather than issued, are still a human's
   call, and the campaign's own success criterion for that half was written
   down as the owner's judgement rather than as a number. So this row's
   confidence in *scoring aesthetic constraints* is materially higher than "the
   half that is years away" allowed; its confidence in *closing* them is
   unchanged. The complementary lesson is a caution for the whole gradient: an
   instrument only scores a bet if something reads it. This one existed,
   drift-checked green, and measured a failure nobody was told about.

   **The Watershed sharpened that caution into its harder form (2026-07-31).**
   There, the instrument was read constantly — and was *wrong*.
   `exposure-sound` reported false on roughly three quarters of all worlds
   because the Laboratory's deliberately hand-maintained duplicate of the
   exposure rules had not learned a rule an earlier commit added. The worlds
   were correct throughout. It was the second such lapse in eleven days, and
   the campaign least able to notice was the one whose central mechanism the
   metric measures. So the caution generalizes: *an instrument scores a bet
   only if something reads it AND the instrument is itself current*, and
   nothing in this repo reddens when a deliberate duplicate falls behind.

   That campaign also moved the self-scorable half in both directions at once.
   Sonority sequencing made pronounceability a property held **by
   construction** rather than measured after the fact — reverse-sonority
   onsets no language uses are no longer drawable, at zero entropy cost, since
   ordering a template consumes the same draws as picking one. But the
   transparency witness *fell* over 1000 worlds (0.816 → 0.793) while rising
   at the reference seed, which is the distribution witness earning its keep:
   a single-world reading would have recorded the opposite. Neither movement
   touches the taste half, which is unchanged.

2. **Emergent economics that don't degenerate.** The mermaid-bone-farm
   problem: static value tables meeting exploitable production collapse into
   absurdity, and most game economies are faked precisely because real ones
   misbehave. Here too the bet splits. *Degeneracy* is self-scorable — an
   exploit detector is a Lab study: run the production loops, measure whether
   any yields unbounded value divergence. Whether prices flood, crash, and
   recover *legibly* is the partly-taste remainder. The economics campaign
   still begins with a literature phase (experimental economics, auction
   theory, virtual-world economics), but that phase now designs the apparatus
   that would falsify the claim, rather than standing between the project and
   knowing how it will grade itself.

3. **Historiography worth reading.** The systems half is no longer
   architecture-less: any entity's committed facts already replay into a
   derivation, physical deep time now lays down glacial strata and fossil
   shorelines ([Deep Time](./chronicle/deep-time.md)), and the past is being
   made queryable so that `why <ghost-town>` can recount the ice age that
   emptied it. Its measurable properties — focalization, sparsity,
   unreliability — are the guardrails. But *worth reading* is honestly
   taste-gated, and this is the one place the project refuses to fake a metric:
   *Dwarf Fortress* generates accurate history that glazes eyes, and no amount
   of architecture guarantees the sparse, focalized, unreliable account with a
   teller that would not. The human read is the real gate, and it is allowed
   to withhold a pass.

## The standing horizon

Year 1 varied the world and held the observer; Year 2 varied the observer and
held the sky. The current research varies **time**: two worlds identical at
genesis but differing only in their deep-time forcing, and a legibly different
present — different glacial strata, fossil shorelines, refugia, ghost-town
lineages, and history-derived myths — every divergence recountable through the
event ledger to its cause in the past. The falsifiability teeth are the same
shape as before: a blind-attribution metric over many thousands of worlds,
against a zero-forcing null control whose present must be indistinguishable
from its own genesis. It is a bet at the top of the checkability gradient, and,
like the two before it, it is allowed to fail.

**A partial rescore, now that population has a field to vary.** The
carrying-capacity field promoted above (see the high-confidence tier) is an
*equilibrium* snapshot — `population = f(carrying capacity)` in closed
form, no iteration, no clock — and equilibrium is not the same claim as
"vary time." What remains open splits cleanly along the same
checkability line this whole chapter runs on. Giving the field a clock —
temporal relaxation, and the founding/growth/fission/abandonment history
that only becomes tellable once population moves — **has now landed**: *The
Living Community* (the living-community engine's first campaign) grows the
present world as the last frame of a coarse forward history run over the
capacity field, now ticked per-era by paleoclimate, so settlements found,
grow, migrate, and end across ~2000 years and leave standing ruins. That is
the *placement* half of the vary-time horizon; the deeper bet above — a
blind-attribution metric over thousands of worlds against a zero-forcing null
control whose present is indistinguishable from its genesis — is not thereby
settled and stays open at the top of the checkability gradient. A second,
narrower piece is not a time question at all: today's
condensation runs each species' field independently, which loosens the old
rule keeping different peoples' settlements apart. Packing multiple
species onto one landscape properly — footprint-scaled home ranges, a
tunable competition temperature, predator-prey coupling — **has now
landed**: the coexistence stack packs a multi-species density stack (with a
frozen competition temperature), and *The Niche* gave each species a
niche-differentiated carrying-capacity field, so composition varies across
space rather than resolving to one global blend — seed-42's
identical-everywhere settlements broke into distinct regions with
structured strife along their ecotones. It resolved to the
moderate-confidence tier as predicted; it was architecture, not taste. The
goblinoid roster also showed the model's honest limit: same-resource
species differentiate only ~two ways on climate alone, so the fuller payoff
(strongholds, refugia, a creature that owns the cold) was thought to wait on
a roster with distinct resource niches. *The Menagerie* (the entity-component
program's first campaign) built that roster — sixteen kinds spanning
photosynthate to apex predator — and found the limit lies deeper than the
roster: carrying capacity is `supply × fitness`, but supply is a **single**
net-primary-productivity field scaled per species by uptake, so a resource
niche changes a species' *magnitude* everywhere, never its *place*. Only the
climate term is spatial, and climate alone differentiates ~two ways however
many creatures compete. The stronghold payoff waited, precisely, on
**per-axis spatial resource fields** (minerals in the mountains, prey where
the prey is) and a way to read dominance as resource captured rather than
headcount. *The Demesne* (Stage 1 of a named Living-Biomes arc) shipped the
**abiotic half** of exactly that: photosynthate, forage, and mineral became
real per-cell supply fields, dot-producted against each kind's uptake vector
at the existing carrying-capacity site, and the rank-restoration paid off
wherever an abiotic specialist could reach it — seed-42's distinct material
dominants rose 2→4, and the pure-mineral xorn went from a noise-level single
settlement to the single largest domain on the world (≈29k cells), a
mineral-eater owning its mountains as the place-identity model intends. Two
pieces stay open,
both measured rather than asserted: the **prey axis** (dragons and predators
still hold no place) waits on Stage 2's trophic food-web field, and — newly
surfaced by the shipped half — the four small **peoples** do not diversify at
all, because their authored niches carry zero weight on any axis this stage
spatialized, leaving them competing on one shared forage number. That last was
named its own open design question (an order-independent territory force — a
field or a fixed point, never a cumulative tally); the preregistered
`≥6`-distinct-dominants test — a *material*-dominance target spanning all
sixteen kinds — stays honestly `#[ignore]`d as its remaining target. **The
peoples half of that question has since been answered, but from the other
axis.** *The Living Community* separates the four near-identical goblinoids not
by a spatial resource force at all but by **history**: history-first placement
grows them at different founding sites and displaces them along different
climate paths, so they end up holding distinct territories (region-of-influence
overlap 0.055 on seed 42, every sampled seed under 0.06) — diversity by *time*
where the spatial axes had none to give. It resolved to the checkable tier;
it was architecture, not taste. So the rescore moves the **abiotic** stronghold
debt from open to **banked and measured**, holds the prey-stronghold debt
against Stage 2, moves the **peoples-territory** bet from open to **answered by
history-first placement** (a time resolution, not the anticipated spatial one),
and — the field having gained its clock — narrows the vary-time debt to its
blind-attribution core, still open. *The Sundering* (the connection graph's
second slice) then gave that peoples-territory resolution a second, *dynamic*
mechanism: routing the history bake over a time-varying connection graph — a sea
that falls with the ice, opening land bridges, and rises to drown them — so
peoples are confined to the landmasses they can reach. Seed-42's four peoples
resolve onto four sea-bound landmasses, three holding only a subset of them, and
the territory overlap held (0.0466). It stays in the checkable tier; what it did
*not* resolve — the *volume* of the diaspora, throttled by the world's ample
vacant land and by peoples settling glacially-stable ground — it handed, with
measurements, to conflict-as-criticality.

**And conflict-as-criticality has now been tested, and the bet lost.** This
chapter has to be able to say that, or its scores are decoration. The wager was
that organised conflict, once it emerged rather than being floored, would
**self-organize to criticality** — that the size distribution of cascading
displacement would be a power law, the signature of a system holding itself at
its own critical point. It was always a bet at the *top* of the checkability
gradient in the good sense: preregistered, instrument-gradable, adjudicable by
the Laboratory without a human read. *The Tumult* built it and graded it.
Conflict does now emerge — seed 42, which never crowds, resolves 76 conquests
driven by coveted value rather than by density, and the map gains population
rather than losing it. The distribution is **not** a power law and is not close
to one: pooled over a hundred seeds and 2974 conquests, nothing chains beyond
size three, the support spans 0.48 decades against a preregistered threshold of
about 1.5, the per-octave decay is roughly 46-fold where a heavy tail falls two-
to fourfold, and the branching ratio measures **σ ≈ 0.051** against a critical
value of 1 — stable to three figures across a 3.3× change of sample. Geometric
with a hard cutoff, deeply sub-critical. No constant was tuned toward the
hypothesis at any point.

The honest rescore is therefore: **the criticality bet moves from open to
falsified for the mechanism as built** — not "partially confirmed", not
"promising". What it does *not* move is the underlying question, and the
distinction is the useful part. Two builds now bracket it from opposite sides.
The first, a crowding sandpile, had a **drive and no dissipation**, so every
avalanche ran to the depth cap — an artifact, not a tail. The second has
**dissipation and no accumulation**: each hop of a cascade costs real
population, every victim is weaker than whoever displaced it, and a chain dies
within a hop or two, with nothing stored *between* relaxations whose release
could make a large event. Criticality needs both terms, and the missing one has
a name and a shape — a standing dominance relation that concentrates value into
a topple-able structure, whose collapse frees a whole subordinate network at
once. So the residual bet is narrower and better armed than the original: not
"does conflict self-organize?" but "does accumulation-plus-dissipation
self-organize, on this world, at this resolution?" — with a measured null
result to beat rather than a prior to defend.

Two notes for the gradient itself. First, this is the chapter's second bet
driven to a verdict by instrument rather than by taste, after The Chorus — and
the **first whose verdict was no**, which is the more informative of the two
outcomes and the one a confidence map exists to be able to record. Second, the
falsification cost roughly one campaign and produced a sharper successor
question; the alternative — shipping the mechanism and narrating it as
criticality — would have cost nothing and taught nothing, and the drift-check
would have re-ratified the narration every time it ran. A bet is only worth
placing at this altitude if losing it is allowed to be published as a loss.

**The successor question has now been asked, and it lost too.** The residual
bet stated just above — *does accumulation-plus-dissipation self-organize, on
this world, at this resolution?* — was the whole mandate of *The Tithe*, which
built the missing term: a raid whose prize is *mobile* subordinates rather than
evicts, a patron collects tribute from a vassal it cannot fully see, and what
it collects banks in a store of wealth that feeds strength without ever
entering the pressure that kills. That is a literal accumulator rather than a
metaphorical one, and it works — the structure forms at volume, patrons survive
collecting, and a dominant grows without moving. **The shape of the violence
did not change.**

Two things moved and they must not be conflated. **σ roughly doubled**, from
≈ 0.051 to **0.109–0.115** pooled over thirty seeds and 7183 conquests, and to
0.103–0.109 over a hundred seeds and 22 255 — the same factor on both samples,
which makes it a real effect of accumulation rather than sample noise. That is
a genuine result and this chapter should say so. But **σ ≈ 0.1 is not σ ≈ 1**,
and every reading of *shape* is unmoved: the support still spans **0.48
decades** against the preregistered ≈ 1.5, the per-octave decay is still
**17.6-fold** where a heavy tail falls two- to fourfold, and **not one cascade
exceeds three displacements in roughly twenty-two thousand conquests**. Still
geometric with a hard cutoff, still deep in the sub-critical regime. No
constant was tuned toward the hypothesis at any point, and the last mechanism
the campaign added had its predictions **written into the spec before its code
existed** — including, explicitly, that revolts firing while the distribution
stayed geometric would be a *stronger* falsification than the standing null.
Revolts fired. The distribution stayed geometric. **That is the branch the
preregistration named as the harder one to explain away, and it is the branch
that happened.**

The honest rescore is therefore: **the criticality bet moves from falsified for
the mechanism as built to falsified a second time, against a mechanism built
specifically to answer the first falsification's diagnosis.** Not "progress
toward"; not "trending". The right way to hold it is that the *diagnosis* has
narrowed, not that the *bet* has improved. Two builds bracketed the answer as
drive-without-dissipation and dissipation-without-accumulation; a third
supplied accumulation and moved the number without moving the family, which
eliminates "nothing is stored" as the explanation. What is left is
**conduction**. A revolt frees exactly one vassal — collapse-release, where a
fallen patron's entire network is freed at once, was a stated non-goal — and
the relation graph is a set of one-level stars, because a vassal may not itself
take a vassal, and depth was the other stated non-goal. A patron's failure has
nowhere to propagate. An avalanche needs a medium, and this world does not yet
have one.

So the residual bet narrows again and is now nearly bare: not "does conflict
self-organize", not "does accumulation self-organize", but **"does a
*connected* accumulating structure self-organize?"** — with two named,
already-specified levers as its remaining content and two measured nulls behind
it. What that costs the gradient is worth stating. A bet that loses twice in a
row, each time to an instrument, each time with the mechanism built rather than
argued about, is more expensive to keep than to drop; the case for asking a
third time rests entirely on each null having eliminated a *different*
candidate, so that the third question is materially different from the first
two rather than a rephrasing of them. **If the connected version also comes
back geometric, the right conclusion is that this world does not sit at a
critical point, and this chapter should record that as settled rather than
open.**

### The third ask is being spent elsewhere (2026-07-29)

That test — *materially different, or a rephrasing?* — has now been applied,
and the answer is that the connected-cascade question **does not clear its own
bar by much**, while a different question clears it easily. This chapter is
therefore rescored: the criticality bet is **not** being asked a third time in
the form above, and the depth-and-collapse-release levers are **deferred, not
refuted**.

The reason is that three campaigns have been measuring the size distribution of
**events** — how long a cascade of displacements runs — while the property the
project actually wants from its history is a distribution over **entities**:
how large the largest polity gets, how unequal holdings become, whether an
empire is a thing a world can produce at all. Those are different variables with
different mechanisms and different literatures. Event-size criticality is
Bak–Tang–Wiesenfeld, and the conduction diagnosis is correct on its own terms.
Entity-size heaviness is Gibrat and Kesten — a random *multiplicative* factor
against a reflecting lower barrier — which is the standard account of Zipf's law
for city sizes and of the empire-area distributions. **Hornvale has never
measured it, and the bake has no empire-size metric at all.**

Reading the mechanism against that second literature explains the two nulls
without appealing to conduction, and the reading was verified in source rather
than reasoned about. A Kesten process needs a per-entity random multiplier that
persists. The bake's strength is `(population + stores × 0.5) × tech_weight`.
Population is logistic, so its growth is *anti*-proportional to its size near
capacity. Stores decay at 0.95 per epoch to a fixed point set by inflow, and are
destroyed on a community's closure. `tech_weight` takes four values capping at
3.0, is driven by absolute year, and its per-people head start is a draw in
[0, 300) years against era boundaries at 400/900/1400 — so **the world's only
irreversible advantage provably converges to zero relative value at year 1400**.
Every multiplier in the model is shared, capped, or mean-reverting, and no two
communities of one people differ in any authored dimension at all. A model with
no persistent per-entity multiplicative heterogeneity cannot produce a heavy
entity-size tail, and would not do so even with a conduction medium added.

*Re-scored in part by [The Tolerance](./chronicle/the-tolerance.md) (2026-08-05),
which voids one clause of the paragraph immediately above and leaves the more
important one standing.* That campaign made a people a distribution rather than
a point: each settlement now draws its own threat response from its
people's authored mean and dispersion, keyed on where and when it was founded
and fixed for the life of the community. So the clause **"no two communities of
one people differ in any authored dimension at all" is no longer true** — two
towns of one people, on different ground in different centuries, hold genuinely
different temperaments, and the between-settlement variance in that dimension
went from exactly zero to 0.010–0.113 depending on the people. The heterogeneity
is persistent and per-entity, which is two of the three properties a Kesten
process wants.

The third it does not have, and that is the part this chapter must not round
away. The drawn quantity enters the model as a **gate on a decision** — a
community above the threshold may take the initiative, one below it may not —
not as a **multiplier on strength**. Strength is still
`(population + stores × 0.5) × tech_weight`, and every term in it is still
shared, capped, or mean-reverting; nothing about the disposition draw multiplies
anything. A heterogeneous *propensity to act* changes which communities move and
therefore how the history branches, but it does not give a community a
persistent random factor on its own growth, which is the specific thing the
literature says a heavy entity-size tail requires. So the correct rescore is
narrow: **the diagnosis loses its "no heterogeneity exists" clause and keeps its
"no multiplicative heterogeneity exists" clause**, and the entity-size
prediction is unchanged. Nothing here was measured against M2 — The Tolerance
preregistered variance and rate hypotheses, not a size distribution — so this is
a correction to the *argument*, not a new reading of the *bet*. A campaign that
wants to test the Kesten account now has a cheaper route to it than it did: the
authoring pattern for per-entity variation exists and is proven, and what
remains is to point one at a multiplicative term instead of a threshold.

One detail sharpens this rather than softening it, and it is the same point
clause 3 above makes about asymptotes. The new heterogeneity is drawn from a
**uniform** on ±√3σ, clamped to the axis — so it is not merely
non-multiplicative, it is *bounded*, and the probability of a settlement
exceeding its people's support is exactly zero rather than small. Per-entity
variation now exists in this world; **rare** per-entity variation still does
not. The build constraint this chapter already owes a successor campaign is
unchanged, and one more mechanism now sits inside its scope.

**What replaces the bet is narrower, and it is a different shape of claim.**
Not a power law: a **sigmoid**. The wager is that annihilation, coexistence and
domination lie on one saturating response, that the middle is where nearly every
world sits, and that both extremes are **reachable but rare** — a world with no
goblins, and a world under one government, each possible and each unusual. This
is preregisterable, it is falsifiable in both directions, and it is a claim
about a distribution the Laboratory can compute over seeds rather than about a
scaling exponent that needs 1.5 decades of support to be well-posed at all.

It also carries a structural requirement the previous framing never surfaced,
recorded as [decision 0096](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0096-diversity-is-terminal-and-rubberbanding-is-multi-axis.md)
clause 3: rare extremes need **asymptotes, not clamps**. Hornvale's saturating
bounds are presently clamps — population against capacity, `tech_weight` against
3.0, `coexist.rs`'s viability `FLOOR` — and the probability of exceeding a clamp
is exactly zero at any input. On the current response forms the tails are not
rare; they are impossible. That is a build constraint, not a tuning target, and
it is the first thing a successor campaign owes this chapter.

**Confidence: low, and deliberately so.** The sigmoid has not been measured, the
claim that per-world conditions vary widely enough to reach either tail is
**unverified**, and this chapter should not be read as predicting the result. The
one thing it does now assert with the same confidence as the two falsifications
above it is the diagnosis: **this world's history evaluates every people on a
single scalar axis, and on one axis weakness is absolute.** That is checkable in
forty lines of source, it is the standing charge decision 0096 opens, and it is
why the third ask is being spent on a second axis rather than on a deeper graph.

One further note the campaign earns a place for, because it bears on how much
any of the above should be trusted. *The Tithe* amended its own specification
**five times, four of them following a disappointing measurement** — and that
count is disclosed in the spec, in the adjudicating test's own documentation,
and in the chronicle, because a reader who meets only the final histogram has
been misled about how it was arrived at. The protection taken was
preregistration of the last amendment. The lesson for this chapter is that a
confidence score is only as good as the disclosure attached to the measurement
under it: the number here is a falsification, which is the direction that
*cannot* be manufactured by adding mechanisms, and that asymmetry is the reason
the rescore is trustworthy despite the amendment count.

### The sigmoid's first axis is measured, and the null is in (2026-08-02)

*The Contour* built the cheapest test of the sigmoid wager's own diagnosis —
a second contest axis, uncorrelated with strength, entering at the raid
dominance test — deliberately touching no authored species data, so that if
it moved nothing the two costlier campaigns behind it (*The Appraisal*,
*The Deviation*) would be worth reconsidering for one campaign's price rather
than three. It moved nothing. Both of the wager's own preregistered halves
are now measured, matched against a frozen thirty-seed baseline, and both are
null:

- **M3 (peoples-alive-at-bake-end) fell, fractionally, rather than rising.**
  The entire thirty-seed delta is one world losing one people; every other
  seed's count is byte-identical to baseline, including the exact set of six
  extinction seeds. The mechanism rescued zero worlds from total extinction
  and caused zero new ones.
- **M2 (the entity-size distribution — the sigmoid's own headline variable)
  stayed geometric.** Mean, median and IQR sit within a few percent of
  baseline at both thirty and a hundred seeds; the one statistic that moved
  cleanly (max/median) moves inside the band a single outlier seed produces,
  not a distributional shift.

Per §4.3 of the spec, both conditions being met is the null the chapter
above already named as the informative branch: **a second contest axis,
uncorrelated with the first and entering at the decision point, is not
sufficient to hold diversity open in this world** — a finding about decision
0096 clause 1's *chosen mechanism*, not about the axiom, and one that sends
the sequence back to design rather than forward to *The Appraisal*.

**The null itself decomposes, and the decomposition is the part this chapter
must not round away.** `peoples-alive-at-bake-end` is discrete and bounded at
five, the roster's own size, and the baseline sits at that ceiling in 76.7%
of worlds already. "M3 rises" was close to unfalsifiable *upward*: in
twenty-three of thirty seeds the metric could not rise, because all five
peoples were already alive. So the null is really two claims of unequal
strength. **"Does not rescue worlds from extinction" is strong** — six
extinction seeds at baseline, six live, the identical seed set, a detectable
effect measured at exactly zero. **"Does not improve diversity in surviving
worlds" is untested**, because the instrument is saturated at its ceiling in
twenty-three of the twenty-four surviving worlds. The spec asked for a second
half of M3 — the effective-diversity reading `coexist.rs` already computes in
space, which would have headroom inside an all-five-peoples world that a bare
count cannot see — and only the count was ever wired up. Building that half
now, immediately after a disappointing count, would have the *shape* of
metric-chasing even with clean logic behind it, so it is deliberately
deferred to whichever campaign answers this chapter next, with its own
headroom declared in the preregistration before any code exists.

**Rescore.** The sigmoid wager's confidence stays **low**, but the character
of the "low" has changed, and the gradient should say so precisely: it was
*unmeasured* when the bet above was struck; it is now *measured on one axis
and null there*, with the other, headroom-bearing axis still unmeasured
rather than merely undiscussed. That is a materially weaker position for the
multi-axis thesis than "unmeasured" was, and a materially stronger one than
"falsified outright" would be — decision 0096 clause 1 is not itself
falsified by one mechanism's failure to move one metric, but it has now spent
its cheapest test and has one clean finding to show for it: position, alone,
is not the term that holds diversity open here.

**Re-measured after the epoch (2026-08-02).** Position-aware conflict draws
no new stream, but it changes every world's committed history, and the
`history/bake` label was bumped to `/v2` to say so honestly (decision 0006).
That re-mints every draw a second time on top of the mechanism's own effect,
so the numbers above were re-measured on a fresh matched pair taken entirely
on the post-epoch derivation rather than trusted to still describe the
shipped world. Neither null moved: M3 is still falsified, M2 still stayed
geometric, and the extinction set is unchanged in both identity and size
across the epoch — the strongest form the "does not rescue from extinction"
half of the decomposition above can take. **This rescore is unchanged and
stands as written.** One thing about *how* the null holds did shift: where
the pre-epoch reading found a single seed accounting for the whole M3 delta,
the post-epoch reading finds two seeds moving in opposite directions that
cancel exactly — the mechanism is visibly live at the individual-world
level, it simply does not net into more diversity. Full numbers:
`docs/superpowers/plans/the-contour-baseline-v2.md`.
