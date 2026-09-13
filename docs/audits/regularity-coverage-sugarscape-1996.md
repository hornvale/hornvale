<!-- GENERATED FILE — do not edit. Regenerate with `hornvale regularities --corpus regularities/sugarscape-1996.regularity.json report`. -->

# Regularity coverage

## Provenance

- **Corpus:** `sugarscape-1996`
- **Population:** `the-census`, 1000 world(s)
- **Source:** Joshua M. Epstein and Robert Axtell, *Growing Artificial Societies: Social
Science From the Bottom Up* (Brookings Institution Press / MIT Press, 1996).
Items are drawn from Appendix B's complete rule roster (growback, movement,
replacement, seasonal growback, pollution formation and diffusion, mating,
inheritance, cultural transmission, group membership, combat, trade, credit,
immune response, disease transmission) and from the emergence claims the
chapters attach to those rules. The taxonomy in `emergence_type` is the
book's own, from Chapter II footnote 24: type 1 is a property meaningful for
an individual but exhibited only by the collective (the diagonal migration
wave — "the group adopts a heading unavailable to any individual"); type 2
is a property meaningful only for a collective (a wealth distribution). AN
INSTRUMENT WITH KNOWN BIAS, NEVER A STANDARD (decision 0095). Sugarscape is
one 1996 lattice model of agents harvesting a renewable resource, and
roughly half of its roster is economic — trade, prices, credit, inheritance
of holdings — because its authors were economists building toward generative
social science. Hornvale has no economy, no per-individual wealth, and no
disease model, so a large block of this corpus can only ever score `absent`;
that is a property of the source's coverage, not a defect Hornvale is being
charged with. Conversely Sugarscape has no terrain, no astronomy, no
language and no deep time, so nothing here scores Hornvale's strongest
ground. Coverage measures reach against this catalogue only. THE MAPPING
FROM A SUGARSCAPE CLAIM TO A CENSUS COLUMN IS AN ANALOGY, AND EACH ITEM'S
`note` STATES WHERE THE ANALOGY IS LOAD-BEARING. A Sugarscape agent is an
individual; a Hornvale settlement is a community. Where an item reads a
per-community statistic against a per-agent claim, the item says so, and a
`flat` verdict on such an item may be about the analogy rather than about
the world.
- **Frozen:** before first measurement, The Seedbed (2026-09-07). Every item below was
authored before any statistic in it was computed against the census, and no
evaluation code existed in the repository when this file was written:
`meets()`, the census reader and the audit all arrive in later tasks of the
same campaign. The freeze is therefore structural rather than a promise
(decision 0016). ONE DISCLOSED EXCEPTION, `sug-wealth-skew`, carries its
disclosure in its own `note`. RE-FREEZING THIS CORPUS IS NOT A THING ANY
SESSION MAY DO, AND THAT IS A PERMANENT PROPERTY OF THE INSTRUMENT RATHER
THAN A RULE OF THIS CAMPAIGN. Tightening a band, or promoting an `absent`
item back to a measurable one, requires a session that has NOT read the
census — and reading the census is not only opening
`book/src/laboratory/generated/the-census/`: several metric rustdocs in
`windows/lab/src/metrics.rs` state their own column's distribution in prose,
so a session that reads a column's definition to learn its SEMANTICS may be
disqualified from banding THAT column while remaining free to band others.
`sug-externality-displaces` is the worked example: its discriminating
criterion is known and one line long, and the session that identified it
could not author it. EVERY SESSION THAT SCORES THIS CORPUS IS THEREAFTER
DISQUALIFIED FROM RE-FREEZING IT, for every column the scoring touched. The
practical consequence is a scheduling constraint, not a style note: band
quality can only be improved BEFORE the first measurement, so a campaign
that wants tighter bands must author them ahead of the run, not in response
to it. Each measurable item's `note` states its FALSIFYING WORLD so that
discriminating power can be audited from this file alone, with no data —
which is the one repair a later reader can always make without disqualifying
themselves.

## Reading this report

This measures reach against ONE catalogue. It is a reading taken through
that catalogue's declared bias (decision 0095), never a grade and never a
verdict on the world. Sugarscape is a 1996 lattice model whose roster is
roughly half economic, so a large block of this corpus can only ever score
`absent` — that is a property of the source's coverage, not a charge against
Hornvale. Conversely the source has no terrain, no astronomy, no language
and no deep time, so nothing here scores Hornvale's strongest ground.

Every mapping from a source claim to a census column is an ANALOGY: a
Sugarscape agent is an individual and a Hornvale settlement is a community.
Each item's note states where that analogy is load-bearing, and a `flat`
verdict on such an item may be about the analogy rather than about the
world.

## Power

The corpus proposes 4 measurable item(s), and they make 3 independent
claim(s) — see the pairs below. Of those items, 2 carry a two-sided
criterion and 2 carry a one-sided one.

2 OF THE 4 SURVIVING CRITERIA ARE CONSERVATIVE ONE-SIDED BOUNDS, and this
report must not be read as though any of them were a calibrated target. A
one-sided criterion says nothing on its unconstrained side: a floor is met
by every world above it and a ceiling by every world below it, however far
past anything the source describes. It separates a world that does the thing
at all from one that does not, and it says nothing about magnitude. So a
near-uniform `grown` sweep across these items is NOT evidence of reach. It
is evidence that the world clears a small number of low bars that a
plausibly-flat world would fail — which is the most this instrument was
built to claim, and less than a reader scanning a tally will assume.

1 of the 4 measurable item(s) DECLARE THEMSELVES NOT BLIND —
`sug-wealth-skew` — and each states its own disclosure below. The rest were
authored ahead of measurement. A disclosed item is still measured and still
scored; what it is not is a preregistered prediction, so it must not be
counted as one.

A two-sided band is the only shape here that can fail at BOTH poles, and it
is correspondingly the shape with real discriminating power; the count above
says how many of these items carry one. Read the individual notes for which
bound is at risk on which item: several say so about themselves, in both
directions.

The claim count is the item count with near-collinear statistics merged: two
items reading statistics correlated at |r| >= 0.95 over the same population
are one claim measured twice, not two corroborations. The coefficients below
are measured on THIS census, not quoted from any earlier probe.

| item | item | correlation | merged |
|---|---|---|---|
| sug-wealth-skew | sug-predation-is-bounded | -0.115 (n = 1000) | no |
| sug-wealth-skew | sug-retaliation-deters | -0.104 (n = 1000) | no |
| sug-wealth-skew | sug-credit-makes-hierarchy | -0.113 (n = 1000) | no |
| sug-predation-is-bounded | sug-retaliation-deters | 0.999 (n = 1000) | yes |
| sug-predation-is-bounded | sug-credit-makes-hierarchy | 0.685 (n = 1000) | no |
| sug-retaliation-deters | sug-credit-makes-hierarchy | 0.689 (n = 1000) | no |

## Tally

Six coverage verdicts, then `unmeasured` — which is a lifecycle state, not a
coverage verdict: an item frozen but not yet run. It is listed here so the
totals add up, and again by name below.

- grown: 3 (7%)
- flat: 1 (2%)
- refused: 2 (4%)
- deferred: 9 (20%)
- absent: 24 (53%)
- inapplicable: 6 (13%)
- unmeasured: 0 (0%)
- **total:** 45

By CLAIM rather than by item, merging the near-collinear pairs listed above:
2 of 3 measured claim(s) grew. A claim counts as grown only when every item
merged into it does. Cite this number, not the item tally, when stating what
the world grew — the item tally counts a merged pair twice.

## Unmeasured

None — every item carries a coverage verdict.

## `absent` splits two ways

An `absent` verdict says only that the statistic cannot be computed and
nobody has registered it. That covers two very different situations. An item
that DECLARES the instrument which would settle it — in the corpus's own
`roadmap_instrument` field, not in its prose — is ROADMAP: the work is
identified. An item declaring none is a GAP: the mechanism itself is
missing, and there is nothing to point a column at.

IDENTIFICATION IS NOT SIZING, and this split must not be read as an
estimate. Naming the right instrument says only that somebody knows what to
build; several of the items below need a new census metric or a new
criterion kind, which is a code change, a review and a test. Each roadmap
item's declared instrument is printed with it, so the size can be judged
rather than assumed.

- roadmap (the item declares the instrument): 6
- gap (the mechanism is missing): 18

The roadmap items, each with the instrument it declares:

- `sug-heterogeneous-landscape` — The skew is characteristic of heterogeneous agents extracting resources from a landscape of fixed, unevenly distributed capacity
  - instrument: a Gini or coefficient of variation over habitable capacity per site -- the quantity settlement actually competes for, at the site grain the claim is about
- `sug-spatial-segregation` — Purposeful local movement toward the best available site produces spatially segregated population pools rather than one homogeneous spread
  - instrument: a clustering or dispersion statistic over settlement positions: a nearest-neighbour distance, a join-count, or a Moran's I over an explicit weights matrix
- `sug-seasonal-phase-lock` — A seasonal environment does not merely move agents; it locks collective behaviour to the phase of the year
  - instrument: a Rayleigh test statistic (n*R^2, or its p-value), or a mean resultant length corrected for n -- a new criterion KIND, not a new band on the existing column
- `sug-culture-is-generative` — Local cultural transmission is sufficient to generate culture; the rule K produces cultural formations rather than inert copies
  - instrument: a measure of cultural divergence driven by contact: a distance between two peoples' lexica that moves with their history, rather than a per-species liveness count
- `sug-social-speciation` — Separated subpopulations differentiate: each converges on its own culture, and cultural distance is what marks the boundary between them
  - instrument: a cross-species lexical or phonological distance between separated peoples -- a distance, which no per-species liveness reading is at any bound
- `sug-externality-displaces` — Environmental change is a live driver of relocation, not a decorative backdrop: degraded or depleted ground moves the people standing on it
  - instrument: median-at-least: 1.0 on the climate-displacement count this item's note names -- a criterion kind this schema already offers, one line long, authorable only by a session that has not read that column's distribution

## Emergence type

The source's taxonomy (Epstein & Axtell, Ch. II footnote 24): type 1 is a
property meaningful for an individual but exhibited only by the collective;
type 2 is a property meaningful only for a collective. An item where the
taxonomy does not apply carries neither — a model abstraction and a bare
micro-rule assert no regularity — and is EXCLUDED from the split rather than
defaulted into a type.

- type 1: 7 held, 1 grown
- type 2: 31 held, 2 grown
- taxonomy does not apply: 7 (excluded from the split above)

## Items

`measured` is the reading the audit itself computes — the same string, from
the same function, so it cannot disagree with the verdict beside it.
`observed` is the scored column's range over this population, printed so a
one-sided bound can be judged: a bound no world in the census approaches
separates nothing, and the verdict alone cannot say so.

| id | title | type | verdict | statistic | measured | observed | anchor | note |
|---|---|---|---|---|---|---|---|---|
| sug-wealth-skew | Holdings are distributed far more unequally than the endowments that produce them | 2 | flat | rank-size-slope | median(rank-size-slope) = -0.552118; band [-1.2, -0.8] | min -0.960769, max -0.321143 | doc:book/src/domesday/settlement.md | NOT A BLIND TEST, disclosed under decision 0016: this statistic's distribution was measured during the brainstorm that motivated The Seedbed, before any corpus existed. Every other item in this corpus was authored before its statistic was looked at. FALSIFYING WORLD: one whose settlements all carry the same population, giving an OLS slope near 0, or one dominated by a single primate settlement, giving a slope near -2 — both fall outside the band and redden. The band is two-sided and both poles are real shapes, and it is the only band in this corpus taken from an OUTSIDE LAW WITH A NUMBER IN IT (Zipf/Auerbach's slope of about -1) rather than from a floor chosen to be safe. It is the model the other items are measured against, and the metric's own rustdoc records that Hornvale's condensation is deliberately NOT tuned to a rank-size target, so the band is genuinely at risk. |
| sug-heterogeneous-landscape | The skew is characteristic of heterogeneous agents extracting resources from a landscape of fixed, unevenly distributed capacity | 2 | absent |  | — | — |  | DEMOTED BY THE FALSIFICATION TEST, and the first draft's own note contained the argument without drawing the conclusion: it conceded that `plate-size-gini` is 'over plate vertex counts, which is upstream of habitability, so a flat here would say the tectonic layer is uniform, not that settlement is.' Construct the falsifying world — a Hornvale world whose habitable capacity is uniform across every site, which is what the source's regularity denies — and the criterion DOES NOT REDDEN: plate sizes are a Voronoi partition over randomly placed seeds and stay heavy-tailed whatever habitability does above them. The two quantities have no path between them. Worse, the criterion is close to unfalsifiable by construction: a Voronoi tessellation from random seeds is never uniform, so the bound restates a property of the plate layer's construction rather than a shape worlds differ on. The discriminating instrument is a Gini or coefficient of variation over HABITABLE CAPACITY per site — the quantity settlement actually competes for — which the census does not carry and no registry row asks for. |
| sug-spatial-segregation | Purposeful local movement toward the best available site produces spatially segregated population pools rather than one homogeneous spread | 2 | absent |  | — | — |  | DEMOTED FOR THE SAME DEFECT AS `sug-social-speciation`, and recorded rather than quietly deleted. The first draft read `settlement-count >= 2`, with the bound taken honestly from the source (one pool per sugar peak) rather than from Hornvale's scale. It still cannot fail on this item's negation: the alternative the source contrasts with — ONE homogeneous spread over the whole landscape — is not a world with fewer settlements in Hornvale, it is a world with the same settlements differently arranged. A count is blind to arrangement. Segregation is a spatial-clustering claim and needs a clustering or dispersion statistic (nearest-neighbour distance, a join-count, a Moran's I over an explicit weights matrix); the census carries none, and no registry row asks for one. |
| sug-predation-is-bounded | Combat between groups claims a real but minority share of the population; predation does not consume the society that practises it | 2 | grown | raid-victim-rate | 990 of 1000 world(s) have raid-victim-rate in [0.02, 0.5] = 0.990000; required >= 0.5 | min 0.005128, max 0.454924 | doc:book/src/domesday/society.md | The combat rule is self-limiting by construction: an attacker discards every site vulnerable to retaliation, so violence is common without being the dominant mode of interaction. Band edges are the source's two poles — nonzero (combat happens at all) and under half (it does not become the society's principal way of ending). Read on Hornvale's community grain: the share of occupation records that ended at another community's hand. FALSIFYING WORLDS, ONE AT EACH POLE: a world whose bake never resolves a contest by conquest, so no occupation ends at another's hand and the rate sits at 0, below the 0.02 floor; and a world where raiding is the majority cause of ending, putting the rate above 0.5. Both reflect real settings of the bake's own `strength <= holder_strength * RAID_MARGIN` gate, so neither pole is hypothetical, and the criterion reddens at both. |
| sug-retaliation-deters | The prospect of retaliation deters attack, so most agents in a mixed population never initiate combat | 1 | grown | raid-initiator-rate | median(raid-initiator-rate) = 0.274692; required <= 0.5 | min 0.005128, max 0.426136 | doc:book/src/domesday/society.md | The offence side, separated from the defence side above because the source treats deterrence as its own finding: precisely the high vision that produced the migration waves suppresses them once combat is on. `initiate at all` is meaningful for an individual agent and for a community alike, hence type 1. FALSIFYING WORLD: one in which a majority of communities initiate at least one raid, putting the rate above 0.5 and reddening the criterion. TWO WEAKNESSES THAT SURVIVE THAT TEST AND ARE RECORDED RATHER THAN PATCHED. (1) The bound is ONE-SIDED, so a world with no raiding at all also scores `grown` — the deterrence claim is untested there, and it is only the LOWER edge of `sug-predation-is-bounded` that rules that world out. (2) The two items are NEAR-COLLINEAR: the metric's own rustdoc records the initiator/victim ratio at 1.00-1.03 on a non-census probe, so `initiator <= 0.5` is nearly implied by `victim <= 0.5`. A report must not count these two as independent corroborations of anything. Adding a lower edge here was considered and declined: it would restate `sug-predation-is-bounded`'s existing claim rather than add one, and a redundant criterion is worse than an acknowledged one-sided ceiling. |
| sug-seasonal-phase-lock | A seasonal environment does not merely move agents; it locks collective behaviour to the phase of the year | 2 | absent |  | — | — |  | DEMOTED FOR AN ESTIMATOR BIAS, WHICH IS THE ONE OF THESE THAT WAS NOT VISIBLE FROM THE ANALOGY. The first draft read `granary-raid-phase-concentration >= 0.1`, reasoning that R is exactly 0 under a uniform phase distribution, so any positive value is evidence of seasonality. THAT IS TRUE OF THE POPULATION VALUE AND FALSE OF THE ESTIMATE. R is a mean resultant length over n observed phases, and for n phases drawn UNIFORMLY at random its expectation is 0.886/sqrt(n) — 0.40 at n=5, 0.28 at n=10, 0.18 at n=25, and it does not fall below 0.1 until n is about 78. The metric's own `Absent` floor is 5 raid-caused endings, so worlds carrying between 5 and 77 of them are scored, and on every one of them a COMPLETELY UNSEASONAL raid process is expected to clear this bound by chance alone. Construct the falsifying world — raids uniformly distributed around the year, exactly what the source's seasonal claim denies — and on a small-n world the criterion does not redden; it reports the sample size. The discriminating instrument is a Rayleigh test statistic (n*R^2, or its p-value), or an R corrected for n, neither of which is a criterion kind this schema offers — and adding one is deliberately a code change, a review and a test, not a data edit. |
| sug-credit-makes-hierarchy | Persistent asymmetric obligation is what makes a flat society hierarchical; without it no agent is subordinate to another | 2 | grown | tribute-relations-standing | median(tribute-relations-standing) = 104.000000; required >= 1 | min 1.000000, max 282.000000 | doc:book/src/domesday/society.md | The source's own framing: 'So far the agent societies studied in this book have been flat -- there is no sense in which some agents are subordinate to others. This stems from the fact that agent interactions are usually short lived ... or are symmetrical.' Sugarscape reaches hierarchy through credit; Hornvale reaches it through standing tribute. The MECHANISM differs and the item does not claim otherwise -- what is imported is the claim that a durable, asymmetric, one-directional relation is the thing that makes a society stop being flat. A standing relation is a property of a pair and a hierarchy of the whole, hence type 2. FALSIFYING WORLD: one whose bake resolves every contest by destruction or displacement and never by subjugation — communities raid, collapse and resettle, and no community ever ends up owing another anything that outlasts the encounter. Verified constructible from the emitter rather than assumed: `history_emit.rs` commits one `pays-tribute-to` fact per entry of the bake's own `h.tribute` and none otherwise, so an empty `h.tribute` yields a standing stock of 0 and the criterion reddens. That is a real outcome of the contest resolution, not a constant of the code — which is what separates this floor from the three demoted in fix round 2. It IS still a floor, and the honest limit of a floor is stated once here for the whole corpus: it can tell a hierarchy-forming world from a flat one, and it cannot say anything about how deep the hierarchy runs. |
| sug-culture-is-generative | Local cultural transmission is sufficient to generate culture; the rule K produces cultural formations rather than inert copies | 2 | absent |  | — | — |  | DEMOTED: THE STATISTIC HAS NO CAUSAL PATH TO THE REGULARITY. The source's claim is that LOCAL TRANSMISSION BETWEEN NEIGHBOURS is sufficient to generate cultural formations — a claim about a social process. `cascade-rules-fired-goblin` counts distinct sound rules that changed at least one lexicon Root, and the cascade it counts is drawn from `seed.derive(ROOT).derive(species).derive(LEXICON).derive(CASCADE).derive(CASCADE_V2)`: a pure function of the seed and the species NAME, with no input from history, contact, society or transmission anywhere in its derivation. Construct the falsifying world — one in which peoples never differentiate culturally, never meet, never transmit — and this column is bit-for-bit unchanged, because nothing it reads depends on any of that. It is also near-unfalsifiable on its own terms: `CascadeRegime::SETTLED` draws 2 to 4 rules for every species unconditionally, and each drawn rule is pre-filtered so the species' own phonology can host it, so `>= 1 fires across a whole lexicon` restates the language crate's construction. The discriminating instrument is a measure of cultural DIVERGENCE driven by contact — a distance between two peoples' lexica that moves with their history — which the census does not carry. |
| sug-social-speciation | Separated subpopulations differentiate: each converges on its own culture, and cultural distance is what marks the boundary between them | 2 | absent |  | — | — |  | AUTHORED MEASURABLE AND DEMOTED IN THE SAME CAMPAIGN, BEFORE ANY MEASUREMENT, AND THE REASON IS WORTH KEEPING. The first draft read `cascade-rules-fired-bugbear >= 1` — the same statistic `sug-culture-is-generative` reads of goblin, applied to a second people. That criterion CANNOT FAIL ON THIS ITEM'S NEGATION: two pools that converged on one identical culture each have a live cascade and would score `grown` on an item about speciation. The source's claim is about the DISTANCE between pools, and a per-species liveness reading is not a distance at any bound. The right instrument is a cross-species lexical or phonological distance, which the census does not carry and no registry row asks for — hence `absent` rather than `deferred`: the trajectory row covers series-blindness, and this is not that. |
| sug-externality-displaces | Environmental change is a live driver of relocation, not a decorative backdrop: degraded or depleted ground moves the people standing on it | 1 | absent |  | — | — |  | DEMOTED, AND THIS ONE IS DECIDABLE FROM FOUR LINES OF THE EXTRACTOR. The first draft used `present-on-fraction: 0.9` to dodge a disclosure hazard: the column's rustdoc states its distribution, so a numeric band authored after reading it would be contaminated, and a presence test looked scale-free and safe. It is not merely weak, it is INVERTED. The extractor returns `Absent` only when a world has no occupation records at all, and returns `Number(0.0)` for a world with a settled history in which climate displaced nobody — and `Number(0.0)` IS PRESENT. So the falsifying world, the one where environmental change never moves anyone, scores this item `grown`. A criterion that a regularity's own negation satisfies is not a criterion. WHAT MAKES THIS ITEM THE CAMPAIGN'S WORKED EXAMPLE OF THE RE-FREEZE CONSTRAINT: the discriminating criterion is known, nameable and one line long — `median-at-least: 1.0` — and this session may not author it, because it has read the paragraph that says where the median sits. The item is left `absent` for a session that has not. |
| sug-carrying-capacity | A given environment will not support an indefinite population; the population approaches an asymptotic carrying capacity from above | 2 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | The claim is not that population is bounded -- any finite world bounds it -- but that it APPROACHES a level and stays there. That is a statement about a time series, and a census row is one number per finished world. |
| sug-carrying-capacity-tracks-traits | Carrying capacity rises with mean agent vision and falls with mean metabolism: the environment's ceiling is a function of who is standing on it | 2 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | A comparative static over the asymptote of the item above, so it inherits that item's blocker: with no way to read the asymptote there is nothing to compare across pin sets. Hornvale's species vectors would supply the trait axis. |
| sug-inequality-accumulates | Inequality is not a starting condition but an accumulation: the Gini coefficient rises as the run proceeds and society becomes less egalitarian | 2 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | The concentrating shape, distinct from `sug-wealth-skew`, which asks only whether the finished distribution is skewed. A final skew is compatible with a skew that was there all along; only the trajectory separates them, and that separation is the whole force of the source's 'emergent structure' claim. |
| sug-migration-wave | A dense block of agents propagates as a succession of coherent waves whose collective heading is diagonal -- a direction no individual, restricted to the four lattice moves, can take | 1 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | The book's canonical type-1 emergence and the example footnote 24 is written around: 'the group adopts a heading unavailable to any individual'. A wave is a position field over time; a census row cannot hold one. Occupation records carry founded/ended spans, so the underlying data is partly in the ledger already -- nothing reads it as a series. |
| sug-population-oscillates | Local mating rules alone produce regular, bounded population oscillations with a long period, produced entirely from the bottom up rather than imposed by an aggregate equation | 2 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | The source is explicit that the point is provenance as much as shape: oscillations, intermittencies and punctuated equilibria are usually modelled top-down with differential equations, and here they are grown. Amplitude, period and boundedness are all series properties. |
| sug-endogenous-crash | A population can crash to extinction endogenously, through local interaction alone, with no external shock | 2 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | Sugarscape's mechanism is density-dependent: mating is local, so a thinning population stops reproducing and the thinning becomes self-reinforcing. Hornvale collapses and abandons settlements, but 'crashed' is a shape a trajectory has, and a finished world that reads low is indistinguishable from one that was always small. |
| sug-cultural-convergence | Under local tag transmission an isolated subpopulation converges to a single pure cultural group; mixing does not persist indefinitely | 2 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | Convergence is a limit, and a limit needs the approach to it. Note the source's own hedge in footnote 32: which group wins is stochastic and the twin-peak outcome obtains in somewhat less than half of runs, so this is a claim about the SHAPE of the trajectory, not about its endpoint. |
| sug-epochs-alternate | Inter-group history alternates between expansionist phases, in which one group looks set to achieve hegemony, and epochs of stalemate in which scattered border contact and assimilation are the rule | 2 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | The proto-history's headline result and the closest thing in the source to Hornvale's own subject matter. It is a claim about phase structure in time -- that the series has epochs at all -- which is the accumulating/oscillating blindness the registry row names. |
| sug-cycle-outlives-the-individual | A society can sustain a cycle whose period exceeds any individual's maximum lifetime, so no member ever experiences the full pattern | 1 | deferred |  | — | — | registry:TOOL-a-regularity-corpus-can-measure-a-trajectory | Type 1 by footnote 24's test: experiencing a cycle is meaningful for an individual, and here only the collective does it. Deferred rather than absent because the claim does not depend on Sugarscape's route to it -- trade -- and Hornvale's millennia of founding and abandonment are exactly the substrate on which an intergenerational period would show up, if anything read the records as a series. |
| sug-tribe-is-an-input-to-violence | Group membership is a primitive input to conduct: the combat rule discards every site held by the agent's own tribe before considering any material fact about the target | — | refused |  | — | — | decision:0021 | 0021 forecloses exactly this shape: 'a people's conduct is a situation derived from material conditions, and any ideology ranking species is a generated output, never an input'. Hornvale's raid gate reads strength against a margin, not identity, and an in-group/out-group ranking is required to be derived rather than declared. Note what is NOT refused: Sugarscape's tags are themselves endogenous, so tribe FORMATION is fine and is scored at `sug-culture-is-generative`. What 0021 refuses is letting the resulting label enter the conduct rule as a term. |
| sug-conquest-monoculture | Combat drives the landscape toward a single surviving group; the terminal state of inter-group contest is one culture on both mountains | 2 | refused |  | — | — | decision:0096 | 0096 makes peoples-diversity a TERMINAL VALUE of the project -- 'a world that loses its goblins has gotten worse, however faithfully it did so' -- and names single-axis contest as the mechanism that manufactures monoculture, citing `domains/demography/src/coexist.rs` replacing per-cell fitness-argmax with a softmax, a viability floor, founder floors and refugia. Sugarscape resolves contest on one scalar (accumulated wealth) and gets the predicted result. THE REFUSAL IS NARROW AND SHOULD NOT BE READ WIDER: 0096 clause 2 explicitly accepts annihilation as an absorbing state a people can fall into, so the refusal is of monoculture as the DESIGNED terminal state and of the single-axis contest that guarantees it, not of any particular people dying. |
| sug-income-less-skewed-than-wealth | Income -- what is harvested per period net of metabolism -- is much less skewed than accumulated holdings; the skew is a property of the stock, not the flow | 2 | absent |  | — | — |  | One of the source's sharpest findings and it needs both halves: a per-individual flow and a per-individual stock. Hornvale has neither. Its `stores` is community-scale non-edible wealth and no per-resident holding exists. |
| sug-selection-without-sex | Selection operates with no fitness function specified anywhere: mean vision rises and mean metabolism falls across generations under replacement alone | 2 | absent |  | — | — |  | Requires heritable per-individual traits that vary and are transmitted. Hornvale's species vectors are fixed per people; decision 0548 draws a resident's deviation from the kind's dispersion per resident, which is variation without inheritance, so there is nothing for selection to act on across generations. |
| sug-inheritance-blunts-selection | A social institution -- inheritance of holdings -- interferes with a biological process, weakening the selection pressure that would otherwise raise mean vision | 2 | absent |  | — | — |  | The source's most interdisciplinary claim and it depends on the two mechanisms above simultaneously: transmissible holdings and heritable traits. Hornvale has neither, so the interaction has no possible referent. |
| sug-genealogical-network | Local mating yields family trees that branch across the landscape, and the genealogical graph is a distinct social network from the neighbourhood graph | 2 | absent |  | — | — |  | Hornvale's history bake carries founder handles and community ancestry (decision 0127 distinguishes identity keys from discrimination keys over exactly those records), so a lineage graph is not unimaginable -- but it is a graph between COMMUNITIES, not between individuals, and no census column reads either. Absent rather than deferred: the blocker is that nothing computes the graph, not that the census cannot hold a series. |
| sug-friendship-network | Cultural proximity plus repeated neighbourhood contact yields a friendship network distinct from both kinship and neighbourhood | 2 | absent |  | — | — |  | Rests on a per-agent memory of the five culturally nearest agents encountered. Hornvale holds no per-individual acquaintance record. |
| sug-neighbour-network | Agents are connected in a neighbourhood graph that expands, contracts and deforms as they move; social connection is a shifting structure rather than a fixed one | 2 | absent |  | — | — |  | The book's unifying theme -- every chapter defines one more network over the same population. Hornvale commits inter-community relations (tribute, raiding) but never a per-individual adjacency graph, and no census column reads network structure of any kind. |
| sug-pollution-degrades-the-commons | Production and consumption generate a pollutant that diffuses through the neighbourhood, degrading welfare for agents who did not produce it | 1 | absent |  | — | — |  | Hornvale has no pollutant field and no mechanism by which one community's subsistence degrades another's site. Type 1 because suffering the externality is meaningful for an individual, while the externality itself only exists between them. |
| sug-bilateral-price-equilibrium | Decentralized bilateral bargaining between neighbours, with no auctioneer and no global information, approaches a market-clearing price -- but a statistical equilibrium, not the point equilibrium of neoclassical theory | 2 | absent |  | — | — |  | Hornvale has no money, no commodities, no prices and no exchange. Decision 0886 gives a dynamics probe (D2) two typed subsistence commodities, which is exploration outside the shipped world and outside the census, and no ratified decision refuses an economy -- so this is a gap, not a refusal. |
| sug-trade-raises-carrying-capacity | Turning trade on raises the environment's carrying capacity, and the same change raises inequality: the gain is real and it is not free | 2 | absent |  | — | — |  | The book's clearest policy-shaped result and the one Chapter VI's indecomposability demonstration turns on -- two societies identical but for trade, one extinct and one prosperous. Needs both an economy and a readable asymptote; Hornvale lacks the first outright. |
| sug-horizontal-inequality | Agents identical in preferences and endowments end in very different welfare states purely through the path of decentralized trade: they meet different partners and bargain to different prices | 2 | absent |  | — | — |  | Inequality with no underlying difference to explain it -- the source's strongest challenge to the welfare properties of markets, and amplified further when agents have finite lives. No economy, and no per-individual welfare state to compare. |
| sug-local-optimality-global-inefficiency | Every trade improves both parties and the allocation is nonetheless globally inefficient: exchange and production compete, leaving the economy perpetually out of equilibrium | 2 | absent |  | — | — |  | The negative result that undermines the First Welfare Theorem's case for laissez-faire in this model. Requires a welfare function over holdings, which Hornvale does not define. |
| sug-credit-roles-overlap | Some agents are simultaneously borrowers and lenders, so the credit graph is a hierarchy of several layers rather than two flat classes | 2 | absent |  | — | — |  | The source flags this as unexpected. Distinct from `sug-credit-makes-hierarchy`, which imports only the claim that durable asymmetric obligation makes a society non-flat: the LAYERING requires an agent to hold both roles at once, and Hornvale's tribute relation is one-directional between communities with no equivalent of simultaneous roles read anywhere. |
| sug-trade-network-computes | The trade partner graph is a massively parallel computer whose interconnections evolve: agents optimizing only for themselves collectively perform a computation none of them represents | 2 | absent |  | — | — |  | Type 2 unambiguously: the computation is a property of the network and of nothing in it. No economy and no network readout. |
| sug-immune-memory-locks-in | An immune system that has once matched a pathogen keeps the match: immunological memory arises as lock-in, with no memory mechanism written into the rule | 1 | absent |  | — | — |  | Hornvale has no disease model and no immune model; no ratified decision refuses one, so this is a gap. Type 1: immunity is an individual property, and the memory effect is observed in the individual too -- included because the roster is Appendix B's and this rule is on it. |
| sug-childhood-diseases-persist | Some diseases persist as childhood diseases because acquired immunity is phenotypic and the transmitted genome is not: each generation must learn them again | 2 | absent |  | — | — |  | The chapter's most striking result and a genuinely non-obvious explanation of a real epidemiological regularity -- one a Lamarckian could not give. Needs disease, immunity and inheritance together; Hornvale has none of the three. |
| sug-epidemic-couples-to-economy | Infection raises metabolism, which changes movement and trading behaviour: epidemic dynamics and economic dynamics are not separable subsystems | 1 | absent |  | — | — |  | The coupling is the point of putting epidemiology inside a social model at all. Both coupled systems are absent from Hornvale. |
| sug-society-can-clear-its-diseases | Whether a society rids itself of its diseases or carries them permanently depends on the size of the pathogen pool relative to the immune repertoire, not on any agent's behaviour | 2 | absent |  | — | — |  | A threshold result: the same rules give opposite societal outcomes at two parameterizations. No disease model. |
| sug-mass-migration-is-driven-by-resource | The fundamental drive for the resource, and nothing else, is what produces mass movement across the landscape | 2 | absent |  | — | — |  | The ATTRIBUTION half, separated from the volume half at `sug-externality-displaces`: the source's claim is that resource gradient alone accounts for the movement. Testing an attribution needs the counterfactual world with the gradient removed, and a census is a population of worlds under one physics, not an ablation. |
| sug-multiple-microrules-one-macrostructure | The mapping from micro-rules to macrostructure may be many-to-one: several different rule sets can generate the same aggregate pattern | — | inapplicable |  | — | — | reason:a claim about the space of models -- that several rule sets may generate one macrostructure -- and not a regularity any world can exhibit | BUCKETED WITH ITS SIBLING `sug-generative-explanation`, from the same section of Ch. I, after a first draft split the two: this was `absent` and that was `inapplicable`, on reasons that were in fact the same reason. `absent` means Hornvale lacks a mechanism; nothing here is missing from Hornvale, because a many-to-one mapping is a property of the space of possible models and no single world can be measured for it. Testing it would mean varying the RULES and comparing outcomes, which is a second generator, not a second world. |
| sug-growback-parameterization | Sugarscape growback rule G(alpha): at each lattice position the resource grows back at alpha units per time interval, up to that position's capacity | — | inapplicable |  | — | — | reason:a per-site regrowth constant on the source model's own resource lattice -- the knob its experiments turn, not a claim about how any world behaves | Included because it heads Appendix B's roster and because the corpus should show where the source stops describing worlds and starts describing itself. Hornvale's harvest curve is a different abstraction serving a different purpose and the two are not commensurable. |
| sug-lattice-geometry | Space is a fifty-by-fifty toroidal lattice on which vision extends in the four von Neumann directions and movement is one of those four | — | inapplicable |  | — | — | reason:the source's own spatial representation; the four-direction restriction is the premise that makes its diagonal-wave result surprising, not a property of physical space | Worth keeping visible rather than dropping, because `sug-migration-wave` is only interesting relative to it: the wave is emergent BECAUSE the lattice forbids the heading. On a sphere with continuous bearings the same finding has no content. |
| sug-agent-replacement-device | Replacement rule R[a,b]: when an agent dies it is replaced by a new agent of age zero with random attributes, random position, random endowment and a maximum age drawn from [a,b] | — | inapplicable |  | — | — | reason:an experimental control that holds the population stationary so a distribution can be studied at all -- the source introduces it to study wealth rather than demography, and turns it off once sex arrives | Not scored as refused despite resembling an authored floor under decision 0096: R is applied uniformly and takes no input from any agent's rank or weakness, so 0096's forbidden shape is not what this is. It is a scaffold, and calling it a world claim would be a category error. |
| sug-tag-string-encoding | Culture is an eleven-bit binary string, transmission is a single bit flip toward a neighbour, and group membership is the majority bit | — | inapplicable |  | — | — | reason:the source's chosen representation of culture, and it says so -- footnote 20 lists several alternative membership rules and notes that longer strings with tag ordering give far more refined schemes | The SUFFICIENCY claim this encoding supports is scored at `sug-culture-is-generative`; only the encoding itself is inapplicable. Keeping the two apart is what stops a Hornvale verdict from turning on whether its cultures happen to be binary. |
| sug-generative-explanation | To explain a macro-structure is to grow it: exhibiting micro-specifications sufficient to generate the pattern is what constitutes an explanation | — | inapplicable |  | — | — | reason:the book's methodological thesis -- a claim about what explanation is, not a regularity a world can exhibit | It is nonetheless the reason this corpus exists, and the sense in which the whole file is one instrument: every measurable item above asks the book's own question of Hornvale rather than of Sugarscape. Recorded as an item so the thesis travels with the catalogue instead of only with the spec. |
