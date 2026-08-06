# The Repertoire

A repertoire is the set of pieces a company can perform without new rehearsal.
Not what it did perform, and not what it should — what it *can*. A company that
has never written its repertoire down cannot tell the difference between a piece
it declines and a piece it could not mount.

Hornvale had roughly six hundred registry rows of mechanism and no way to say
what the world can **stage**. "Narrative richness" was unmeasurable, and the
only available proxy was a count of shipped features, which rewards adding
machinery whether or not anything can use it. This campaign built the missing
instrument, ran it once, and shipped the reading.

## Reading the trope backwards

Forwards, a trope is content to generate, and that does not transfer: a world
that requires nothing of its inhabitants has no plot node it must deliver.
Backwards, the same artifact is a coverage probe.

> *An ambitious drow frames a superior for murder* is not a thing to generate.
> It is a conjunction of required mechanisms — ambition, a hierarchy worth
> climbing, killing classified as murder, forged evidence, others who credit it,
> a sanction, a vacancy to inherit — and whether the world supports each is
> answerable **without generating anything.**

So the campaign froze a corpus and asked it of the concept registry. The corpus
is Georges Polti's thirty-six dramatic situations (1895), decomposed with
Greimas' actantial roles into **40 capability bundles** over **217
requirement edges**, each bundle expanding to concrete registry tokens —
`predicate:`, `phenomenon:`, `concept:` — and never to prose. A requirement
either names something the registry could hold or it names a gap carrying a
proposed name. Default-deny throughout: an unknown bundle expands to itself,
which no registry token can match, so a typo blocks its situation rather than
silently freeing it.

Three verdicts, not two. **Stageable** and **blocked** are the obvious pair;
the third is **inapplicable**, for a situation presupposing something the world
deliberately lacks. Polti is 1895 French dramaturgy, an instrument of known
bias, not an inventory of the possible — and a probe that cannot say *different,
not deficient* scores a design choice as a hole.

The output is one committed, byte-ratcheted artifact per corpus. This campaign
produced the first,
[`docs/audits/trope-coverage-polti-1895.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/trope-coverage-polti-1895.md),
in four sections: provenance, demand (every situation and its verdict),
leverage (missing bundles ranked by fan-in), supply (registered tokens no
situation requires). It regenerates from `hornvale tropes report` and is
rebuilt by the ordinary artifact script. Everything below is a reading taken
through that one instrument; a second came later, and the last section says
what it changed.

## The four predictions

The spec froze four before the first run, and the corpus commit lands before
the first report commit so the git history proves the ordering. Two held; two
did not.

**P1 — under 15% of the 36 will be fully stageable. HOLDS.** Zero of thirty-six
are stageable: 0%. Thirty-five are blocked and one is inapplicable. The spec
expected red and said so; the ranked misses are the deliverable, not the score.
A green reading here would have meant the corpus was drawn wrong.

**P3 — the median bundle will be required by at least three situations.
HOLDS,** at a median fan-in of four across all forty bundles, with 217 edges
and no orphan bundle. This is the cheapest kill the campaign had, and it was
spent the day the corpus froze — before a line of resolver code existed. Had
the lattice come back flat, situations sharing almost no bundles, there would
be no leverage signal and the instrument would have been void; nothing built
afterwards could have rescued it. Preregistration's value is mostly in which
prediction you can afford to check first.

**P2 — the top three missing bundles will be deception, status-succession and
norm-violation. FALSIFIED**, and this is the campaign's most interesting
result.

**P4 — at least four of the 36 will resolve `inapplicable`. FALSIFIED.**
Exactly one does.

Nothing was retuned to rescue either. The corpus is byte-identical to its
freeze commit apart from two grain defects corrected before any measurement,
both found by review and both recorded at the time.

## P2: the catalogue does not bottom out at narrative machinery

The three bundles the spec named are the machinery of *plot* — lying,
climbing, transgressing. The measured top three are not:

| rank | bundle | fan-in over the 35 blocked |
|---|---|---|
| 1 | `individual-persons` | 35 |
| 2 | `intent` | 17 |
| 3 | `felt-affect` | 16 |

The predicted three place seventh (`norm-and-transgression`, fan-in 8),
seventeenth (`deception`, fan-in 4) and twenty-ninth (`succession`, fan-in 2)
in a ranking of thirty-one rows. "Status-succession" has no single counterpart
in the decomposition — the corpus splits it into rank, office, reputation and
succession — and under the most generous reading available, taking whichever
half places best, it reaches eleventh. There is no mapping under which the
prediction is close.

What the instrument actually says is that the catalogue does not bottom out at
narrative machinery. It bottoms out at **there being no individual people, no
representation of intent, and no felt affect**. Thirty-five of thirty-five
blocked situations require at least one of those three. Corpus-wide, the three
carry 70 of 217 requirement edges — 32% of all demand — by themselves; in the
narrower frame the first clause uses, the missing edges over blocked
situations, it is 68 of 197, or 34%. The world has settlements,
populations, biomes, phenomena, deities-as-mythologisations and a deep-time
history; eight of forty bundles resolve satisfied and every one of them is
about places, communities, weather or the sky. The demand side is about
persons, and the supply side is about the world they would live in.

That reorders the backlog this instrument exists to produce. A prioritisation
argued from `deception` would have been an argument about a feature. The
measurement instead says the next tier is ontological, and that no amount of
plot machinery is reachable before it.

The report is careful not to overclaim the consequence. Fan-in is **not** an
unlock count: the closest blocked situation is still missing four bundles, and
landing all three of the top rows would leave all thirty-five still blocked —
the best case falls to one bundle short, the median to four. The ranking says
where the leverage is, not where the finish line is.

## P4: the third verdict may be earning its place zero times

One situation resolves `inapplicable`: #31, Conflict with a God. The reason is
a real design commitment. Hornvale's religion domain derives every deity from a
salient phenomenon and commits the provenance; a god is a community's account
of the sky or the weather, and the causal chain terminates in astronomy and
climate. A mortal contending with an immortal antagonist presupposes a
supernatural agent with will, which the world deliberately does not contain.

The finding is that even this one is contestable, and in a way the artifact
cannot show. A mortal contending with a god they *believe* acts is
representable — belief is exactly what the religion domain models, and
`named-pantheon` and `celestial-portent`, the two bundles that carry it,
both resolve **satisfied**. Read that way, #31 is not inapplicable at all; it
is a belief-mediated situation the decomposition declined to write.

There is a sharper edge under it. The resolver short-circuits on exclusion:
if a situation carries a reason, its tokens are never checked. #31 is the only
situation in the corpus requiring `divine-agency`, and `divine-agency` expands
to `predicate:deity-acts` and `predicate:divine-will`, **neither of which the
registry holds**. So the corpus has thirty-two missing bundles and the leverage
table has thirty-one rows: the thirty-second is hidden behind the exclusion.
Structurally, #31 is *also* blocked, and nothing in the artifact distinguishes
"the world will not do this" from "the world does not do this yet" — the very
distinction the third verdict exists to draw. The exclusion's prose asserts
that the world contains no divine will; the registry independently reports that
it holds no `divine-will`. Those are the same sentence at two altitudes, and
the report shows only one of them.

The corpus was **not** changed to settle this. Rewriting #31 after unblinding
would move the score the prediction was made about. It is left as a finding,
and as the first question the next corpus revision has to answer: on the
present evidence the third verdict may be earning its place zero times rather
than once, which is a stronger falsification of P4 than one-versus-four looks.

## What the ratchet is, and is not

The artifact is guarded by a whole-file byte comparison of the live report
against the committed one, accepted deliberately under `REBASELINE=1`. The spec
asked for a per-situation ratchet — no situation that was stageable becomes
unstageable — and the whole-file compare is **strictly stronger** given
identical accept semantics, since any verdict change changes bytes. It is also
vacuous today at zero stageable, and it is not a *direction* check: it reddens
on improvement exactly as it reddens on regression. A reader coming from the
spec should expect a directional guard and will not find one.

## The half of Supply that was not built

The supply section lists 277 registered tokens no situation in the corpus
requires. The spec asked for tokens no situation requires **and no readout
consumes**; only the first conjunct exists. So the list includes tokens that
are heavily consumed — `predicate:is-a` carries the Book, the `moon-*` family
carries the almanac — and must be read as *unrequired by this catalogue*, not
*unused*. The Goodhart guard the spec wanted, a rising demand score beside a
rising count of genuinely unconsumed tokens, needs the missing half before it
can serve. The section says so, above the list rather than below it.

## What was settled

The campaign ratified one thing, as [decision
0095](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0095-a-corpus-is-an-instrument-never-a-standard.md):
a trope corpus is a provenance-stamped **instrument**, never a **standard**.
Provenance is emitted before any number rather than documented elsewhere; a
situation the world deliberately forecloses resolves `inapplicable(reason)`
rather than counting against it; and the output is a matrix over corpora, of
which one corpus can only ever supply a column.

That last commitment is the one this campaign left owed. Polti asks which
situations exist, Propp asks what order they arrive in, a fan taxonomy asks what
an audience notices — and the disagreement between them is the measurement this
instrument is eventually for. With one corpus the report could only say *reach
against this catalogue*, and it was careful never to say *coverage*.

A later campaign paid it, and the debt is recorded here as paid rather than
edited out. A second corpus — a fan taxonomy compiled from a 2012 wiki rip,
carrying its own declared bias — now has a column of its own,
[`docs/audits/trope-coverage-tvtropes-2012.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/trope-coverage-tvtropes-2012.md),
and
[`docs/audits/trope-matrix.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/trope-matrix.md)
renders both columns beside each other, generated by `hornvale tropes matrix`
and ratcheted the same way. It does not revise anything on this page: both
columns still read zero stageable, so the matrix is not a scoreboard and the
readings above stand as taken. What it adds is the thing a single column cannot
carry — the two catalogues, asked the same question of the same registry in the
same run, **do not ask the world for the same things**, and the matrix is where
that disagreement is legible and re-derivable. The wording holds too: it is
still reach against *these* catalogues, and still never *coverage*.
