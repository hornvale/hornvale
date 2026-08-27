# The Interlinear

*An interlinear is the artifact where a foreign sentence, its analysis, and its
translation sit on stacked lines. It is what this campaign produces: one
structure, realized twice.*

Here is the campaign, in six lines. One `ClauseSpec`, built from a real
occupation in seed 42's ledger, realized once through Common and once through
the hobgoblin tongue:

```text
COMMON: Nwamvam is the home of the hobgoblins, in the clearing at vertex 8835,
        founded in year 25; it ended in year 375.

TONGUE: Nwamvam Qoqe Bae 8835 25 375.
```

**A note added by [The Scarf](the-scarf.md), 2026-08-26.** The *words* above
are what this campaign built and are still exactly right. The *numerals* are
ledger data and have since moved: the live line reads `Nwamvam Qoqe Bae 18822
25 1600`, and the Common line's vertex and years move with it. The site and the
two years are `Argument::Count` adjuncts read straight off `occ-site`,
`occ-founded` and `occ-ended`, so they track whatever seed 42's deep history
currently commits — and the demonstration above was authored on 2026-08-25,
before 202 commits of `main` were absorbed into this branch the next day, and
was never re-run. Nothing objected because nothing asserts it: `git grep 8835
-- '*.rs'` finds only synthetic test literals. The line is kept as the record
of what this campaign produced rather than silently rewritten, because the
staleness is the more useful fact. **A demonstration no test asserts is prose,
and absorbing `main` is exactly when prose goes stale.**

`Qoqe` is *home*; `Bae` is *hobgoblin-kind*; the hobgoblin tongue drew a zero
copula, so its sentence is juxtaposition rather than "is". Nothing about that
sentence was authored. The people, the site, the years and the tongue's own
words all came out of a world.

## Where the project was

`domains/language` could say exactly one kind of sentence. `Frame` had one
variant, `Classify`, and `ClauseSpec` — which called itself "a language-neutral
clause" — carried `modifiers: Vec<String>` of pre-rendered English like
`"with two moons"`, which the realizer "only joins".

Two consequences, and the second is the sharper one. Anything richer than the
bare frame was **untranslatable by construction**, because a tongue realization
cannot carry an English phrase. And English grammar had already leaked *out* of
the language domain and *into* a window: `windows/book` duplicated
`indefinite_article`, with a doc comment explaining that "the aggregation seam
keeps `domains/language` untouched". A window knowing how English articles work
is not a lapse — it is the correct local response to an interlingua that is not
expressive enough, which is exactly why the interlingua had to change and the
window did not.

Meanwhile the tongues were far ahead of the syntax. They draw constituent order
six ways on WALS-like weights, draw a copula or none, draw noun-class and
evidential marking at three depths and join it at the *segment* level, and
inflect for number with irregularity **emerging** from sound change rather than
drawn. They could inflect beautifully and had almost nothing to say.

## The shape the project already had

The design question was whether to invent a thematic-role inventory — agent,
patient, experiencer — or to reuse the fact. It turned out the project had
already answered it twice.

`domains/history` reifies an occupation as an **entity**, `is-occupation`, and
hangs its roles off it as ordinary predicates: `occ-people`, `occ-site`,
`occ-founded`, `occ-ended`, `occ-cause`, `occ-ended-by`. The last two *are*
thematic roles, living in the one registry, covered by the drift-checked concept
page.

And `domains/language/src/account.rs` — the epistemic account, the four-filter
stack a culture's knowledge passes through — already operates on facts, already
treats `"is-a"` as one predicate among many, and already yields
`Substituted { truth: "planet", theirs: "earth" }` when a culture carves the
world differently. `Frame::Classify` was a second, weaker encoding of a relation
the same crate handled as data.

So `Frame` was deleted rather than extended. Constructions key on a predicate id;
the complement stopped being a special case and became the object. That is
*less* machinery, not more. Decision
[0266](../../../docs/decisions/0266-an-utterance-is-a-fact.md) records it.

## What the campaign refused to do

**It did not sweep the world.** Byte-identity was the assertion: `make
rebaseline` and `make rebaseline-goldens` moved nothing, through a refactor that
deleted a field from the clause structure and rewrote every construction site
across two crates. The gallery held still because the port was faithful, and the
empty diff has a positive control — `the-book.md` is rewritten on every
rebaseline and carries three distinct tail shapes, including one with no trailing
clause at all.

**It did not grow recognition.** `parse_common` recovers the clause skeleton and
returns **no adjuncts at all**. The loss is stated on the function and pinned by
a 400-case round trip rather than one example. Parsing is where controlled
languages rot, and the campaign froze its coverage rather than growing it before
there was anything to measure against.

## The honest limits

Five, recorded here because a chronicle that only reports the good sentence is
a worse record than one that says where the sentence stops.

**The tongue has no role marking.** Look again at `8835 25 375`. Common
distinguishes "in the clearing at", "founded in", "ended in". The tongue
distinguishes nothing — `realize_adjuncts` matches on the *argument* and never
reads the *role*. A hobgoblin speaker could not tell the founding year from the
ending. A tongue's role surfaces are its adpositions and case morphology, and
building them is the next campaign's work, not a defect in this one.

**Corrected by [The Scarf](the-scarf.md), 2026-08-26.** This paragraph
originally ended "…its adpositions and case morphology, which `paradigm.rs`
already draws and nothing realizes". That is false in both halves.
`domains/language/src/` contains no adposition or case machinery at all — one
grep for `adposition|postposition|preposition` returns a single doc comment,
and `typology.rs` has no case dimension. What `paradigm.rs` actually draws is
**Number and Tense**: depths, attachment sides, family affix protos, paradigm
cells. Those *are* real unrealized machinery, with **zero consumers** anywhere
outside their own module — just not the machinery this sentence named. The
correction matters because the original reading makes role-marking sound
nearly free: pick up morphology that is already drawn and surface it. It is
not. Adpositions and case must be **drawn**, which means new permanent stream
labels — a save-format contract — and the original framing hid that cost
behind a claim about code.

**The subject is a liberty.** `Nwamvam` is the *people's* autonym used as the
holding's headword, because the ledger names neither the occupation nor its site.
If a campaign ever commits a name for either, that is the honest subject.

**This particular tongue exercises none of the deep machinery.** Hobgoblin at
seed 42 draws `None` for both noun-class and evidential depth, and no copula —
so the flagship line is the bare floor assembly. Seven of the fifteen placed
peoples do mark, five of them by affixation; this one does not.

**And Common is not yet a peer in full.** `TongueClause` still takes a different
shape from `ClauseSpec` — it carries an already-surfaced subject and a bare
complement concept. Collapsing them is attractive and was deliberately not done
here, because it is a second restructuring riding on a refactor that already took
a fix round. **[The Scarf](the-scarf.md) did it**: `TongueClause` is deleted,
`ClauseSpec` absorbed its one unique field and is now called `Clause`, and both
realizers take the same `&Clause`.

**The interlingua's speaker features are two of five.** The spec promised
`ClauseSpec` would carry number, definiteness, polarity, mood and evidential;
it carries number and definiteness. Polarity and mood exist nowhere in the
domain, and evidential lives only on `TongueClause`, so a projection from
`ClauseSpec` has to supply it out of band rather than read it off the clause.
The right call was to add nothing speculative — `Argument`'s own doc already
states the discipline, a variant is added "when a role needs it, never
speculatively" — so this is a deviation recorded rather than a defect fixed.
**Now three of five**: [The Scarf](the-scarf.md) moved `evidential` onto the
clause, which is what let the out-of-band projection go. Polarity and mood
still exist nowhere in the domain, and are still not added speculatively.

## The instrument that outlives it

The campaign founded `sentences/` — a frozen, provenance-stamped corpus of
things this world should be able to say, the third sibling to `tropes/` and
`systems/`, with the same discipline: the corpus is data, the resolver is code,
and the corpus is frozen before any measurement is taken.

Its founding entry is a merchant conversation Nathan wrote in the brainstorm
that opened the campaign, recorded verbatim: a man reporting a death he
witnessed, admitting what he does not know about it, and naming who else was
there.

Its first score is **0 of 12**, which is the correct answer. Every line demands
something this campaign did not build. What the tally gives is a map:

```text
past-tense           7      embedded-clause      2
pronoun-reference    4      epistemic-hedge      2
temporal-adverbial   3      negation             2
polar-question       2      wh-question          2
witness-set          2      coordination         1
existential          1      named-entity-list    1
transitive-frame     1
```

A measurement whose only possible answer is zero cannot distinguish a working
resolver from a broken one, so the test asserts a second thing: a synthetic
`classify`-only entry must resolve as **covered**. Emptying the implemented-demand
list reddens that control while leaving the zero untouched — which is how we know
the zero means something.
