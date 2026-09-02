# The Avowal

*To avow is to declare openly what is already the case. This campaign taught
Hornvale nothing new about drama. It gave the world words for capabilities it
already had, and it repaired the instrument that could not see them.*

## Two numbers that disagreed

`wolverson-2021`, the systems corpus, reads 27 present of 74 — the program is
built. `polti-1895` and `tvtropes-2012`, the trope corpora, read 0 of 36 and 0
of 409 — the world represents no dramatic situation at all. Both numbers were
true, and the second one was lying by omission.

`cli/src/tropes.rs::resolve` scored a situation stageable when every one of
its required tokens was a name the concept registry held, and consulted
nothing else. Registry membership is append-only, so the number could only
ever climb, and it could not tell a real capability from a predicate somebody
registered on optimism. It was worse than that, in a way the campaign's own
audit found by grepping rather than reasoning: `windows/sentiment` computes
exactly what `bundle:felt-affect` demands — a warmth/competence/emotion
appraisal between two peoples — and commits not one fact. The instrument
reads the `ConceptRegistry` alone, and a capability can live in three places
this architecture recognizes: the ledger (entity-keyed, saved), the component
layer (kind-keyed, build-state, never saved), and session state (entity-keyed,
persisted only when asked). **A capability the world genuinely has scored as
absent whenever it lived anywhere but the first**, which under decisions 0001,
0346 and 0366 is most of what this project derives on purpose rather than
stores.

## The provision table, and the witness

Two instruments closed that gap, and the second one is the campaign's real
subject.

`Provision` (decision 0576) is a declared table mapping every corpus token to
the resolver that actually serves it, spanning all three homes. It answers
*declared ⊆ served*: a row that resolves `false` still counts as missing,
exactly as membership-only reading did, but a row can now name a component or
a session resolver instead of only a ledger fact. It reuses
`hornvale_kernel::Correspondent<T, V>` — `Present(payload) | Absent(reason)` —
rather than reinventing the shape, and pairs it with a project-local reason
type rather than stretching `manifest.rs`'s `Void`, which names lexicon and
cognition reasons that do not fit a missing storage home.

A wider table would have moved the headline number for nothing, which is
exactly what decision 0330 already ruled out for the sibling sentence corpus:
*"a token added on optimism moves the score without moving the grammar, which
would make the instrument worse than no instrument."* The trope corpus had no
witness. This campaign built one (decisions 0577→0581→0582→0583): a situation
earns `Stageable` only when a committed `Tableau` places its actants and the
tableau's staged relations equal — by *set*, not by subset in either
direction — the situation's own required `predicate:` tokens. It shipped red
first, against a one-situation corpus with no witness registered, and it does
**not** prove any world produces the situation; it proves the machinery can
carry it, which `windows/vessel/src/tableau.rs` already states as its own
purpose.

The witness earned its wariness. Three consecutive review rounds each found
that its own prior round had claimed *completeness* and been wrong: round 1
said a relation-less witness "binds vacuously" only when a situation requires
no predicate token, and a probe found it bound to *any* situation regardless.
Round 2's fix closed that and declared actant-role assignment "the ONLY
disclosed limit," and a third probe produced a situation requiring
`phenomenon:eclipse` that resolved `Stageable` while nothing staged an
eclipse — the witness cannot check a `concept:`/`phenomenon:` requirement at
all, because nothing a `Tableau` stages can represent one. The record that
finally held stopped asserting completeness: *"the limits include X and Y"*
survives discovering a third limit; *"X is the only limit"* does not survive
any, found or not. Migration cost was zero — nothing had ever claimed
`Stageable` under the old reading, so nothing needed retrofitting.

## What was already there, given words

Three capabilities the world was already computing got vocabulary, one per
home:

**Kinship (the ledger, decision 0578→0584).** `domains/history::descent`
already exposed `Kinship`, `kinship()` and `ancestor()`, committing zero
facts. The bake now commits `parent-of` (an `Ancestor(1)` forebear) and
`kin-of` (`Sibling`, or any farther remove) alongside `is-person` and
`person-founded` — 32 and 61 facts respectively on seed 42, +93 total,
exactly spec §5's preregistered figure. A promoted-forebear yield probe
(§4.3's required kill criterion) swept 25 seeds before this shipped: median
yield 44.9%, every seed clearing 38.7%, nearly 4× the 10% floor. Review
caught two real defects in what shipped first — every remove from 1 to 37
generations was collapsing onto `parent-of`, contradicting the registered
concept's own "father or mother" definition, and the fact's direction read
backwards (the descendant as the parent of their own ancestor). Both are
fixed in the shipped predicate; `bundle:consanguineal-kin` is the first
bundle this project has ever fully satisfied.

**Affect (the component layer, decision 0579).** `windows/sentiment::
snap_judgment` computes a people-to-people warmth/competence/emotion
appraisal and was already correct; committing it would have written roughly
840 facts identical in every world ever generated, precisely the duplication
decision 0366 forbids. `affect-kind` and `affect-intensity` resolve through
`Provision`'s component home and commit nothing — proven with a positive
control, not merely asserted: a real world's serialized ledger is
byte-identical before and after a full `resolve` pass requiring both tokens.
Polti's `feels-toward` is person-to-person, not people-to-people, and Nathan
ruled at brainstorm that the two grains are different predicates: the corpus
resolves only against the finer one, so **`bundle:felt-affect` reads 2 of 3
and stays deliberately blocked** until a person-scale producer exists. The
trope number does not move on a grain the corpus never asked for.

**Acts (session state, decision 0580→0585).** No act had identity anywhere
in the tree — `occ-cause` and `occ-ended-by` are `Text` labels, not
references — and the controller first costed reifying one as if
addressability implied storage, recommending against it. Nathan corrected
that at brainstorm, citing decisions already in force: passage state, an
affordance, and a live-play fact are all derived and persisted only when a
snapshot asks. An act gets a derived identity instead — `ActHandle`, folding
`actor`/`deed`/`patient`/`day` through four separate mix steps, the same
shape as `RoleHandle` and `barrier_of`. The property-test sweep caught a real
collision on its first run, not on inspection: an early draft folded a
presence tag against a raw `EntityId`, and `mix`'s first step is a bare XOR,
so `mix(a, a) == 0` for any `a` — `EntityId::new(1)` collided with the
literal tag `1`. Fixed by folding each tag against the already-avalanched
accumulator instead. `witnessed`, `present-at`, `deed-of`, `act-precedes` and
`act-occurred-on` resolve through the session home, and **two bundles
complete**: `bundle:witnessing` (2 of 2) and `bundle:act-chronology` (4 of
4 — its fourth token, `predicate:history-now`, was already a ledger-home
fact and this task supplied the other three). `windows/vessel/src/
session.rs` was not touched; the derived view was built entirely against
`Session`'s existing public surface.

## The number that did not move, on purpose

`polti-1895` and `tvtropes-2012` both read exactly what spec §5
preregistered: 0 of 36, 0 of 409, unchanged from before this campaign. That
is the headline, not a caveat. Three bundles completing shortens a `Blocked`
reason without ever reaching a witness — every situation either corpus names
still requires at least one bundle this campaign left untouched (combat does
not exist; person-scale affect is deferred; the concept/phenomenon layer the
witness cannot check at all). Under Nathan's grain ruling, the number is not
supposed to move until a person-scale affect producer exists, and the witness
exists precisely so that registering vocabulary can never substitute for one.

## What stays unwired, and one thing that turned out already wired

Three mechanisms this campaign touched were already built and already silent
before it started, and all three got wired this time in one home or another.
`descent.rs::ancestor()`'s own doc still calls itself "reserved and
currently unconsumed" — its sibling `kinship()` is now spoken through
`parent-of`/`kin-of`, but `ancestor()` itself has no caller yet. `snap_
judgment` still commits nothing — by design, at the people grain, with
person-scale affect recorded as an idea-registry row rather than built. And
the tone tier — every authored row's `tonality` reads 0.0 — was found in
passing and is not this campaign's to fix.

The fourth candidate on this list was wrong, and finding out why is its own
small instance of the campaign's recurring lesson. `kernel/src/manifest.rs`'s
module doc still reads "Stage 1 lands the types only — nothing constructs a
`Manifest` yet," and the spec quoted that sentence (§4.1) as a reason
`Provision` reuses `Correspondent`'s *shape* without reusing `Manifest`
itself. The sentence is false: `register_manifest` is "the only public path
to add a concept to the registry" by its own doc, every domain's `register_
concepts` constructs one `Manifest` per concept, the composition root calls
it for every world, and `hornvale concepts --manifest` renders the result —
none of that is speculative, all of it runs today. A stale doc comment
outlived the stage it described, nobody had reason to open that file and
notice, and this campaign cited it as evidence without checking. Corrected
at the source rather than repeated here a second time. It changes nothing
about the decision to build `Provision` separately — that reasoning never
depended on whether `Manifest` had callers — but it is one more instance of
this chronicle's own theme: a committed sentence is not evidence just
because it is durable.

A project this large accretes capability faster than it accretes the words
for it; this campaign is one pass at closing that gap, not the last
one it will need.
