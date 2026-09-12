<!-- GENERATED FILE — do not edit. Regenerate with `hornvale technologies report asimov-1989`. -->

# Technology coverage

## Provenance

- **Corpus:** `asimov-1989`
- **Source:** Isaac Asimov, *Asimov's Chronology of Science and Discovery* (Harper & Row,
1989), as presented by invention.cards, whose own subtitle states the
attribution verbatim: "A visual chronology of Asimov's ~1500 scientific
inventions and discoveries." Asimov is the catalogue; invention.cards is
where it was encountered, and the date and counts of both fetches are
recorded below. TWO INDEPENDENT FETCHES, AND WHAT EACH ONE HAD TO FIND OUT.
The site root is a d3 canvas application: `curl -sL
https://invention.cards/` returns 200 and 10,866 bytes of shell with no item
list in it, and `https://invention.cards/universe.js` is a 404, so a naive
fetch of the catalogue enumerates nothing and reports success. The
enumeration is `https://invention.cards/browse/`, which is server-rendered
and links every invention as `href="/<slug>/"`. FETCH 1 (the campaign
controller, 2026-09-11, before this task was dispatched): 150,326 bytes,
1,484 distinct invention slugs -- the "~1,500" the spec cites -- and exactly
three named story arcs linked from the shell, `/story/knights/`,
`/story/republic-of-letters/` and `/story/steam-diffusion/`, with 16, 10 and
15 linked inventions and a union of 41. FETCH 2 (this task,
2026-09-11T17:33:34Z, independently issued and independently parsed):
150,326 bytes, 1,484 distinct slugs after discarding the page's self-link,
the same three arcs at 16, 10 and 15, the same union of 41 -- `diff` against
fetch 1's saved slug list and arc union is empty in both cases. NO
DISCREPANCY. There is no `/stories/` index; that URL 404s, and the three
arcs were enumerated from the shell's own `href` set both times. Each of the
41 item pages was then fetched once (2026-09-11T17:34:02Z-17:34:09Z) for its
title, attested date, attributed person and place, and -- the part that
matters structurally -- its own "Built on" and "Led to" links, which are the
catalogue's dependency chains in machine-readable form. THE SELECTION RULE,
STATED BEFORE ANYTHING WAS SELECTED, AND APPLICABLE WITHOUT READING A LINE
OF HORNVALE. (1) Enumerate the catalogue's own named story arcs -- the
curated sequences the site itself publishes under `/story/<name>/`, not a
grouping anyone here invented. (2) Take every invention linked from every
arc, whole, and take their union. (3) If the union is fewer than 40 items,
widen by taking whole additional arcs; if more than 80, drop whole arcs.
Never add or remove an individual item. The arcs taken are ALL THREE THAT
EXIST: `knights` (16), `republic-of-letters` (10), `steam-diffusion` (15),
union 41 distinct items. No arc was dropped, because none had to be. A rule
keyed to Hornvale's own code would make the selection the measurement;
hand-picking a single item would do the same thing one item at a time, which
is why step 3 operates on arcs only. THE RULE IS AT ITS FLOOR, NOT
COMFORTABLY INSIDE IT, AND THE WIDEN BRANCH HAS NO ESCAPE HERE. 41 is one
item above the spec's 40-item minimum. The widen branch did not fire, and it
COULD NOT HAVE: three arcs is all the catalogue publishes, so "take whole
additional arcs" has nothing to take. Recorded rather than repaired, because
the repair would have been to invent a fourth grouping or to pick items by
hand, and both are the thing the rule exists to forbid. A later reader
should know that a single arc going missing upstream, or a single item being
unlinked from one, would put this corpus below its own floor with no
mechanical remedy. ONE ALTERNATIVE READING OF THE RULE WAS MEASURED AND
REFUSED. The spec says items come from the catalogue's own "named story arcs
AND dependency chains", which can be read as: take the arcs, then close over
the "Built on" links. Measured: the 41 arc items name 15 distinct
prerequisites outside the arcs (`alchemy`, `artillery`, `biped`,
`coke-iron`, `copper`, `crossbow`, `falling-motion`, `fire`, `geometry`,
`heat-capacity`, `nation`, `plow`, `steel`, `stone-tool`, `university`), so
one step of closure would give 56 items -- also inside the 40-80 band. That
is exactly why it was refused: TWO readings both land in the band, and
choosing between them after seeing which number is nicer is hand-picking at
the level of the rule instead of the item. The arcs-whole reading was taken
because it is the one the spec's own decision rule is written in terms of --
widen or narrow by whole ARCS -- and a one-step dependency ring is neither
an arc nor an item, so the decision rule has no operation that produces it.
The full transitive closure was not measured and is not proposed; it would
plainly exceed 80. DEMANDS ARE DERIVED, NEVER WRITTEN (decision 0386). Each
item names the ONE demand it `introduces` and the items it `presupposes`;
the demand set is the transitive closure over `presupposes`, computed on
read. No demand list appears anywhere in this file, and adding one would
reopen 0386 rather than extend it. `presupposes` NAMES ITEMS IN THIS CORPUS
AND NOTHING ELSE, WHICH DROPS REAL PREREQUISITES ON PURPOSE. Every
`presupposes` edge is transcribed from the catalogue's own "Built on" list
for that item, filtered to the 41. The 15 prerequisites listed above sit
outside the corpus and are therefore ABSENT FROM `presupposes`, not
approximated and not invented: `inv-cart` really is built on copper,
`inv-writing` on the stone tool, `inv-printing-press` on steel,
`inv-scientific-societies` on the university, and the lattice in this file
says none of it. Each affected item's `note` names what was dropped. The
consequence is that the derived demand set UNDER-DESCRIBES every such item's
real prerequisites -- a known, bounded distortion of taking 41 items out of
1,484, and the price of a corpus whose every edge is checkable against the
source. ORDERING. `ordered` is true: items are in the catalogue's own
chronological order, by attested date ascending, with the single tie that
matters (`inv-arquebus` and `inv-standing-army` are both dated 1450) broken
so that a prerequisite precedes its dependent. Every `presupposes` edge
therefore points backwards in the file. DECLARED BIAS -- AN INSTRUMENT WITH
KNOWN BIAS, NEVER A STANDARD (decision 0095). This is a mid-20th-century
Western progress narrative, ordered by European and Mediterranean
attestation, treating discovery as cumulative and one-directional, and
under-recording non-material technique. The three arcs concentrate that bias
rather than diluting it: `knights` is the European mounted-warfare sequence,
`republic-of-letters` the alphabetic-literacy-to-printing sequence,
`steam-diffusion` the 17th-18th century pneumatics-to-Watt sequence. Counted
rather than asserted: the catalogue attributes a place to 37 of the 41
items, and 26 of those are in Europe, 5 more in Egypt and the Near East
(Sumer twice, Assyria, Phoenicia, Egypt), and 6 elsewhere (China three
times, India, Central Asia, and one attributed to "Slavs" rather than to a
place). NOT ONE of the 41 is attested in the Americas, sub-Saharan Africa or
Oceania. For a world of goblins, kobolds and drow this is load-bearing: the
catalogue demands a horse and a printing press and never once asks whether a
people can find its way in the dark, keep a lineage's obligations straight
across ten generations, or name a place for the crop that grows there -- all
of which Hornvale models. ASIMOV IS NOT OWED A WORLD. Coverage here measures
reach against THIS catalogue only, and a 38-item `absent` column is a
statement about the intersection of two rosters, not a deficiency score. THE
REACH QUESTION, STATED EXACTLY, BECAUSE EVERY VERDICT BELOW TURNS ON IT. Can
a Hornvale world TODAY carry the capability this item introduces as a
committed fact that CAN DIFFER BETWEEN TWO PEOPLES? The second half is the
load-bearing half. A label every community carries by construction is not a
capability a people acquires, and a label that conflates the demand with a
neighbouring one -- so that no world can hold the one without the other --
does not meet the demand either. Such near-misses are scored `absent` WITH
THE NEAR-MISS NAMED IN THE `note`, which is what makes this column auditable
by someone who disagrees with the ruling: the evidence for the other verdict
is printed beside the verdict. VERDICT TALLY AND THE TWO VALUES THAT DO NOT
APPEAR. 38 `absent`, 3 `deferred`, 0 `present`, 0 `refused`, 0
`inapplicable`, 0 measured. `present` is the verdict this instrument is
least entitled to and it earns none here. `refused` is unavailable as a
matter of fact, not of judgement: a case-insensitive grep of
`docs/digest/decisions-in-force.md` for technology, tech tree, economy,
craft, metallurgy, gunpowder and invention returns NOTHING, so no in-force
decision declines any of these 41 capabilities and none could be cited.
`inapplicable` earns none either, and that is worth stating because spec
section 4.1 provisions it explicitly for "things a subterranean people would
never want": the verdict requires the world to DELIBERATELY lack a
precondition, and the three arcs taken are surface-world European sequences
whose preconditions Hornvale lacks by omission rather than by design. "A
people would not want it" is in any case a claim about a people's
preferences, and Hornvale models no preference over technologies at all --
which is itself a finding, and a cleaner one than six soft `inapplicable`
verdicts would have been. THE `absent` COLUMN WAS SWEPT AGAINST TWO NAMED
IDEA-REGISTRY ROWS, AND THE SWEEP MOVED ONE ITEM. The first draft cited
`BIO-animal-domestication` and `TECH-3` and never checked `BIO-8` or
`TECH-2`, both of which touch this corpus's subject matter -- a miss made
easier by `BIO-animal-domestication` opening with the words "Sharpens
[[BIO-8]]", so the unchecked row was one link from a row that WAS checked.
`BIO-8` ("Domestication & agriculture -- the culture layer harnessing
biosphere fields; staple crop per biome, the Boserup plough (SOC-2's input),
nightsoil (BIO-5), famine as paleoclimate (MAP-6) x carrying capacity
(MAP-7)", `raw`) names `inv-turnplow`'s demand outright, so THAT ITEM IS NOW
`deferred` ON IT. A refusal was available in principle, on the same argument
this file uses to refuse `NARR-monument-writes-itself` for `inv-writing` --
that a row naming a capability as an INPUT it assumes does not plan that
capability -- and it was tested and does not hold here. The plough appears
in `BIO-8` as one of the row's OWN enumerated elements, and the
parenthetical "(SOC-2's input)" names where the plough's output ROUTES, i.e.
a downstream consumer, not a precondition the row presumes. Decisively:
another element of the same list, "staple crop per biome", HAS SHIPPED
(`domains/climate/src/crops.rs`), which proves `BIO-8`'s elements are
deliverables rather than assumptions. A row one of whose enumerated elements
shipped as a deliverable is a row that plans deliverables. `TECH-2` ("The
pyrotechnology ladder -- pottery/ceramics gate storage->surplus->the culture
rung ladder; kiln temperature gates smelting; metallurgy yields tools,
weapons (MAP-9's war-cost damper), and coinage (SOC-1's exchange axis)",
`raw`) MOVED NOTHING, and the reason is a rule this file now states once so
that it can be applied consistently and argued against: A ROW THAT PLANS A
PREREQUISITE OF AN ITEM'S DEMAND DOES NOT MAKE THAT ITEM `deferred`; the row
must name the capability the item's own `introduces` token names. Demands
are derived by closure, so an item whose prerequisite becomes planned still
has its own demand unplanned, and its verdict is its weakest demand.
`TECH-2` names four things -- pottery, food storage and surplus, smelting,
and metallurgy's outputs -- and this corpus contains no pottery, storage or
coinage item at all (the catalogue's pottery entry is in none of the three
arcs, which is worth noting in a campaign named The Kiln). What it touches
is the METAL in `inv-metal-stirrup`, `inv-iron-horseshoes`, `inv-arquebus`,
`inv-pike`, `inv-couched-lance` and `inv-printing-press`, and in every one
of those the metal is a DROPPED PREREQUISITE (`steel`, `artillery`) rather
than the item's own demand: the stirrup's demand is a load-bearing fitting,
the horseshoe's a durability term on a working animal, the arquebus's a
chemical propellant in a tube, the pike's and the lance's a formation and a
tactic, the press's mass reproduction. Each of those six items records the
refusal in its own `note`, so the argument travels with the verdict rather
than living only here. BOTH ROWS ARE QUOTED ABOVE IN FULL, AND AN EARLIER
DRAFT SILENTLY DROPPED THEIR CROSS-REFERENCES. THE ONE NORMALISATION CLAIM
THIS FILE MAKES, because it is the checkable one: every quotation of a
registry row matches that row after four character transliterations -- em
dash, en dash, arrow and multiplication sign to ASCII -- with markup
ignored. Measured across both corpora in fix round 4: 38 quotation segments,
0 mismatches. MARKUP IS OUTSIDE THAT GUARANTEE: the registry's `**bold**`
and `[[row]]` markup is NOT normalised by this file, practice varies --
measured in fix round 4, four quotations across the two corpora keep it and
one drops it -- and the comparison ignores it on both sides, so a reader
diffing bytes should expect markup differences and nothing else. An earlier
version of this clause asserted the markup 'is dropped', which is false in
four places and was introduced by the same round whose mechanical sweep
caught two misquotes: the sweep checked quotations against rows and nothing
tested the sentence describing the sweep (campaign ledger #24). The elisions
were not self-serving -- the one parenthetical kept was the one the argument
has to overcome, and the dropped "nightsoil (BIO-5)" would have STRENGTHENED
the reading that `BIO-8` enumerates deliverables with consumers attached --
but a frozen artifact should quote its anchor exactly, so they are restored.
THE RULE IS CHECKED AGAINST THE TWO `deferred` VERDICTS IT COULD HAVE
INVALIDATED, not only against the refusals. `BIO-animal-domestication` names
"a working animal" as "the missing half of an existing axis", which is
`inv-animal-dom`'s own demand, not a prerequisite of it. `TECH-3` names
"mining/refinement" as still deferred, and extraction is `inv-coal-mining`'s
own demand rather than a prerequisite of it. Both hold. And breadth alone is
not a refusal this file applies: `TECH-3` is broader than
`inv-coal-mining`'s demand and is accepted, so `BIO-8` being broader than
`inv-turnplow`'s could not be grounds for refusing it. THE CEILING RULING,
WHICH IS THE SINGLE MOST VERDICT-SHAPING CALL IN THIS FILE. `TechHorizon`
(`domains/history/src/record.rs:60`) has four variants and its own doc calls
Classical "the ceiling this engine models". It would be easy to score
everything past that ceiling `inapplicable` or `refused`. Both are refused
here, because spec finding F1 established that NO decision record covers the
four-rung horizon -- its entire rationale is one paragraph, fact #7 of 12,
in section 5.3 of a spec about ruins. An unratified code fact is not a
deliberate world choice, and scoring it as one would launder the exact thing
this campaign exists to put under measurement. Every item past the ceiling
is therefore `absent`, with the ceiling named in its `note` as the cause.
AND A TRAP THE CEILING SETS FOR THE NEXT READER, NAMED SO NOBODY WALKS INTO
IT. `tech_for(year)` (`windows/worldgen/src/history_bake.rs:2900`) puts its
rungs at 400, 900 and 1400 -- and those are WORLD years since genesis, read
off a history bake whose `end_year` is 2000.0. The catalogue's dates are
real-world attestation dates in BCE/CE. 1454 CE for the printing press and
1400 for the Classical rung are not the same kind of number and MUST NOT BE
COMPARED; the near-coincidence of their magnitudes is an artifact of both
being four-digit integers. No verdict or band in this file is keyed to a
catalogue date. THE CRITERION: ONE STATISTIC, ONE BAND, THE SAME BAND
EVERYWHERE. Every item carries `statistic: fraction-of-peoples-holding` and
`criterion: {fraction-in-band, lo 0.05, hi 0.95}`, inclusive at both bounds.
Spec section 5.2a is the reason a criterion exists at all: the pathology
this campaign was opened to detect is not that Hornvale fails to acquire
technologies but that EVERY surviving community acquires all of them, and a
boolean verdict scores "every people has bronze" and "half the peoples have
bronze" identically, so it is blind to precisely the defect. The band is
UNIFORM across all 41 items, deliberately and for two reasons. First, the
corpus has exactly one hypothesis -- that a technology's holding varies
across peoples -- and 0.05 to 0.95 is the minimal statement of it. Second, a
per-item band would have been authored by a session that has read the model,
which is the one thing section 4.1's selection rule is built to prevent; a
uniform band cannot be tuned item by item to pass, because there is nothing
item-specific in it to tune. THE STATISTIC IS DEFINED HERE, NOT LEFT TO THE
SESSION THAT SCORES IT, because a frozen band over an undefined quantity
freezes nothing. `fraction-of-peoples-holding` is: (NUMERATOR) the number of
peoples holding the capability, over (DENOMINATOR) the number of peoples
with at least one community alive at the evaluation instant. AGGREGATION --
ANY LIVE COMMUNITY. `tech` and `tech_offset` are fields on a COMMUNITY, not
on a people (`windows/worldgen/src/history_bake.rs:2388-2390`, beside that
struct's own `alive: bool`), and the rung is advanced per community by
`tech_for(year + c.tech_offset)` at `:4806`. A people therefore has several
communities which may sit on different rungs, and the census reports the
quantity per OCCUPATION (`first-day-occ-tech-neolithic` / `-bronze` /
`-iron` / `-classical`), never per people. **A people holds the capability
if AT LEAST ONE of its communities alive at the evaluation instant holds
it.** Three alternatives were available and are refused for stated reasons:
ALL is wrong because one newly founded community on a lower rung would make
the whole people stop holding something it demonstrably still has, which
would report loss where there is only growth; LATEST needs an ordering over
a people's communities that the bake does not define; AT-CLOSURE exists only
for peoples that have died, so it cannot produce a cross-sectional fraction
over the living. ANY is also the rule the `lost` verdict needs: under it a
people loses a capability exactly when NO live community holds it any more,
which is this family's ratified scope for `lost` -- a people that held the
capability no longer holds it. A WRINKLE IN THAT CODE WORTH RECORDING, SINCE
IT IS THE REASON AGGREGATION HAD TO BE RULED ON AT ALL. The `tech_offset`
field's own doc comment calls it "Per-people tech-advance offset (years),
drawn at genesis", and the draw at `:6577` is `pstream.range_u32(0, 300)`
INSIDE the loop that opens a people's genesis communities -- so each genesis
community of one people draws its OWN offset, and the field is per-community
in fact while its doc says per-people. Daughters inherit their parent's
(spec finding F3). So a people's communities genuinely can differ, and "the
people's rung" is not a quantity the engine holds anywhere. EVALUATION
INSTANT -- BAKE END. The fraction is taken once, at `BakeConfig::end_year`
(default 2000.0, `history_bake.rs:986`), over peoples with at least one
community alive then -- the population the census column
`peoples-alive-at-bake-end` counts, NOT `peoples-placed`. The two give
different denominators and the choice is not cosmetic: `peoples-placed`
includes peoples that have already closed, which hold nothing, so using it
would dilute the denominator with extinction and read as an absence of
capability where the finding is an absence of a people. FALSIFYING WORLDS,
stated once because they are the same for every item: a world where no
people holds the capability gives a fraction of 0.0, and a world where every
surviving people holds it gives 1.0. BOTH REDDEN. The band is two-sided and
both poles are real shapes this engine produces. THE ARGUMENT FOR THE 1.0
POLE NEEDS ONE STEP THIS FILE USED TO SKIP, and it is the step that converts
a community observation into a people-level fraction: spec finding F4's
committed gallery tally, 10 `classical` and 1 `bronze-working`, is per
COMMUNITY while this statistic's unit is PEOPLES. The step is arithmetic
rather than empirical. Offsets are uniform integers on [0, 300] and
`end_year` is 2000.0, so every community still alive at bake end is
evaluated at `tech_for(2000 + offset)` with the argument in [2000, 2300],
every value of which exceeds the Classical threshold of 1400 -- so EVERY
live community is Classical, and therefore under ANY of the four aggregation
rules above every surviving people is Classical. The people-level fraction
is 1.0 for whatever the Classical rung names and 0.0 for everything else.
AND THAT IS WHY F4'S TALLY READS 10 AND 1 RATHER THAN 11 AND 0: the
`bronze-working` entry is a DEAD community, whose rung stopped advancing in
the year it closed, which is F4's own "the horizon dates the dead". The two
numbers are not in tension -- they are the living and the dead measured at
different instants, and only the living enter this statistic. Under this
criterion today's world fails every item in this corpus, in one direction or
the other, and a bare boolean would have reported some of it as success. THE
BAND'S STATED MEANING IS N-DEPENDENT, AND N IS NAMED. "At least one people
lacks it and at least one holds it" is what the band means only while the
number of peoples in the denominator is at most 20. The arithmetic: the
smallest non-zero fraction is 1/N, which is inside the band only while 1/N
>= 0.05, i.e. N <= 20; symmetrically the largest non-one fraction, (N-1)/N,
is inside only while N <= 20. MEASURED: `peoples-placed` and
`peoples-alive-at-bake-end` both run 9 to 15 across all 1,000 censused
worlds, so the claim is true today with headroom. ABOVE N = 20 IT BECOMES
FALSE SILENTLY: a divergent world with exactly one holder, or exactly one
non-holder, would fall outside the band and redden, so the band would be
STRICTER than minimal divergence rather than equal to it. Expressing the
criterion as a count (at least 1 and at most N-1) would be N-independent and
was considered; the fraction is kept because it stays comparable across
worlds of different N, which a count does not, and because a count is
satisfied by a single holder in a world of a thousand peoples, which is
noise rather than divergence. A successor meeting a red at N > 20 should
read this paragraph before concluding the world diverged less than it did.
THE SINGLE `verdict` FIELD IS A PIPELINE, AND THE PIPELINE IS WHY THIS FILE
CARRIES NO TRAJECTORY VALUE AT ALL. Per spec section 5.2 a measured value
(`grown`/`flat`/`lost`) is reachable only if reach already succeeded. No
item's reach succeeds, so no item is eligible for a measured verdict, and
consequently this corpus carries ZERO `grown`, ZERO `flat`, ZERO `lost` AND
ZERO `unmeasured`. `unmeasured` is for an item whose reach PASSES and whose
trajectory is not yet scored; inventing one here to make the column look
complete would assert a reach success that does not exist. The criterion
above is therefore preregistered for a successor campaign to evaluate and is
evaluated by nothing today -- which is the freeze working, not an omission.
Nothing in Hornvale can lose a technology (`tech_for` is documented
"monotone in `year`, so tech only ever rises"), so `lost` is unreachable on
this column by construction as well as by pipeline. NON-BLINDNESS, AT TWO
LEVELS, WITH A DIFFERENT INSTRUMENT FOR EACH. CORPUS LEVEL: these verdicts
were authored by a session that had read `tech_for`, `tech_weight`,
`TechHorizon` and the spec's seven findings. That is uniform across all 41
items and to the same degree, so it is stated here ONCE at full strength;
copying it into 41 identical fields would state one fact 41 times, which is
the duplication decision 0261 forbids and the same argument that keeps the
derived demand set out of this file. ITEM LEVEL: `disclosure` marks PER-ITEM
non-blindness, matching `regularities/sugarscape-1996`'s semantics exactly,
and **an `absent` whose verdict turned on having read the model owes one**
(campaign ledger #12). THAT RULE IS AN INVERSION OF THE ONE THIS FILE FIRST
SHIPPED WITH, AND THE REASON IS WORTH CARRYING. The first draft gave
`disclosure` only to items whose verdict was NOT `absent`, reasoning that
`absent` is the default that cites nothing, so model knowledge could not
have manufactured it. That rationale assumed `absent` is the conservative
direction. FOR THIS CAMPAIGN IT IS THE FLATTERING ONE: the thesis is that
Hornvale's technology model is impoverished, so a high `absent` count is the
result that confirms the author's expectation, and an item that is really
`deferred` but scored `absent` is exactly the self-serving error. A rule
exempting that direction from disclosure exempted the only direction that
needed it -- and the exemption produced an instance immediately:
`inv-turnplow` was a ROOT scored `absent` while `BIO-8` names the Boserup
plough, and because the item carried no disclosure nothing invited anyone to
audit the search. It is `deferred` now. Under the rejected rule it would
have gone unexamined and read as blind. WHICH ITEMS CARRY ONE: THE RULE IS
CHOSEN-VS-INHERITED, AND IT IS STATED HERE RATHER THAN ENFORCED BY ANYTHING.
A verdict is CHOSEN when NO PREREQUISITE IN THE ITEM'S DERIVED CLOSURE IS
`absent` -- nothing upstream forces it, so it rests on a search of the
repository -- and INHERITED otherwise, when the weakest-demand rule reads
the verdict off an `absent` prerequisite and no Hornvale fact decided it.
CHOSEN ITEMS CARRY A `disclosure`: 12 of 41 -- the 10 items with no
`presupposes` edge at all, plus `inv-horse` and `inv-parchment`, whose
single prerequisite `inv-animal-dom` is `deferred` rather than `absent`. THE
RULE WAS FIRST CHECKED AS "every ROOT", WHICH UNDER-COVERS, AND
`inv-parchment` IS THE ITEM THAT SHOWED IT (campaign ledger #13). A root --
no `presupposes` at all -- is a strict SUBSET of chosen: an item whose
prerequisites are all non-`absent` is equally unforced, and `inv-parchment`
is such an item, so a check keyed to the proxy rather than to the rule left
it undisclosed and unswept. THE PROXY DIVERGED FROM THE RULE AT THE MOMENT
THE RULE WAS WRITTEN, AND AN EARLIER DRAFT OF THIS PARAGRAPH BLAMED A
RE-SCORE INSTEAD. It said the round that re-scored `inv-turnplow` to
`deferred` "created two such items in the same commit that ratified the
rule". MEASURED, AND FALSE: the chosen set was recomputed from this corpus's
own freeze commit `14147856a` and it is THE IDENTICAL TWELVE ITEMS,
symmetric difference empty, so the re-score created ZERO chosen items.
`inv-animal-dom` was already `deferred` at the freeze -- it carried one of
that commit's only two disclosures -- so `inv-horse` and `inv-parchment`
were chosen non-roots from the first authoring; `inv-turnplow` is a ROOT,
hence chosen whether `absent` or `deferred`; and its only dependent
`inv-horse-collar` was inherited before and after, through
`inv-iron-horseshoes`. THE TRUE STATEMENT IS DULLER AND WORSE: a root-keyed
check under-covers by every item whose prerequisites are all non-`absent`,
which can be true from the first authoring, so it needs no trigger and there
is no moment at which it becomes safe. Recorded at length because the false
version narrowed a PERMANENT STRUCTURAL hazard into an EVENT, which would
have told a later author to start distrusting the proxy after something
happens rather than always -- and because the error entered through the word
"now" in a ruling that was otherwise correct, read as causal by a reader who
did not check it. The conclusion was right and the evidence offered for it
was not, which is the one combination nobody audits. SIX ITEMS WHOSE NOTES
RECORD A CONSIDERED-AND-REFUSED ANCHOR DELIBERATELY CARRY NO `disclosure`,
stated so the omission is not read as concealment. `inv-metal-stirrup`,
`inv-iron-horseshoes`, `inv-couched-lance`, `inv-pike`, `inv-printing-press`
and `inv-steam-engine` each refuse a candidate row in their notes, and each
has an `absent` prerequisite -- so ACCEPTING the refused row would not have
moved the verdict, which the weakest-demand rule fixes at `absent` either
way. A refusal that could not have changed the outcome is evidence in a
note, not a choice in a verdict, and marking it would restore the noise
decision 0261 and ledger #12 both refuse. An earlier draft of this file
carried those six and not `inv-parchment`, which is the proxy's error in
both directions at once. NOTHING IN THIS REPOSITORY ENFORCES ANY OF THE
ABOVE, AND AN EARLIER DRAFT OF THIS PARAGRAPH CLAIMED OTHERWISE. It said
"the build refuses to write this file unless every root carries a
disclosure", which was false as a statement about the repository and
contradicted this file's own `frozen` field in the same breath. The
validation was a throwaway script in a session scratchpad, never committed,
and committing one would have made `frozen`'s claim untrue and destroyed the
property that makes decision 0016's freeze structural here (campaign ledger
#13). WHAT WAS ACTUALLY RUN, in the past tense and carrying no standing
guarantee: an uncommitted authoring script asserted, before writing this
file, that the authored slug set equalled the verified 41-item arc union,
that ids were unique, that `presupposes` resolved inside the corpus, that
every edge pointed backwards in file order, that no item outranked a
prerequisite, that `absent` cited nothing while every other verdict cited a
known anchor prefix, that every item with a dropped prerequisite named it,
that the disclosure set equalled the chosen set, and that the verdict tally
and disclosure count written into this prose matched the data. It is gone
with its worktree. A CITED MITIGATION THAT DOES NOT EXIST IS WORSE THAN AN
ABSENT ONE, because a later author reading "enforced" has no reason to build
the check, so the invariant would be believed by everyone and held by
nothing. `cli/tests/suite/technology_corpus.rs` is where these become
enforcement, and `technologies/CLAUDE.md` states them as instructions to
that resolver. What the selection rule protects, and the only thing it could
protect, is WHICH 41 items are scored: those came from the catalogue's own
arcs before any Hornvale fact was consulted. EVERY `note` IN THIS FILE IS
MODEL-DERIVED, AND THE ABSENCE OF A `disclosure` NEVER CLAIMS OTHERWISE. The
near-miss analysis in an inherited item's note was written by the same
non-blind session; what its missing `disclosure` says is that no such fact
DECIDED the verdict, not that the note was authored blind. The corpus-level
statement above is what covers the notes. WHY THE `absent` MASS IS
STRUCTURAL RATHER THAN A SCORE. Hornvale's concept registry grows on demand,
and the idea registry says so of itself:
`LANG-registry-is-exotic-heavy-and-core-light` records that "demand-driven
growth built an almost entirely sim-specific vocabulary while the
conversational core is missing". The same asymmetry governs this column.
Everyday material technique -- a cart, a plough, a harness, a written record --
has had no consumer in this engine, so no predicate was ever minted for it,
while the registry carries 441 distinct keys across the 449 rows of
`book/src/reference/concept-registry-generated.md`, including `anvil` ("a
heavy iron block a smith hammers metal against") and `loom` ("a frame for
weaving thread into cloth") as scene OBJECTS a possessed body can stand next
to. A smith's anvil is a noun in a room while no people in any world holds
smithing. That gap between the furniture and the capability is the most
compact statement of what this corpus measures. AMENDED AFTER FREEZE,
2026-09-11, BY THE CAMPAIGN THAT FROZE IT (campaign ledger #17). THREE
VERDICTS MOVED FROM `absent` TO `deferred` -- `inv-writing` on `MAP-8`,
`inv-literature` on `DOM-aesthetics`, `inv-library` on `MEM-4` -- so the
tally above reads 38/3 and the data now reads **35 `absent`, 6 `deferred`**,
and the disclosure count reads 12 above and **16** in the data. The
superseded sentences are left standing rather than rewritten, because a
frozen artifact's own history is evidence and this paragraph is the
correction a reader needs in order to distrust them. WHY AMENDING A FROZEN
CORPUS DOES NOT BREACH DECISION 0016, stated here because the next reader
will ask. No measurement has occurred, no evaluation code exists, and the
correction is against an EXTERNAL fact -- the idea registry's own contents --
rather than against a result. The precedent is loud: `wolverson-2021`'s
provenance records four corrections its own campaign got wrong, and decision
0136 records a re-audit of all 74 items that moved TWELVE verdicts after
that corpus froze. A freeze makes a change deliberate; it does not make an
error permanent. The item SET is untouched -- still the same 41 items from
the same three arcs, selected before any Hornvale fact was consulted -- and
only `verdict`, `anchor`, `note` and `disclosure` have moved. WHAT WAS
ACTUALLY WRONG, AND IT WAS THE SWEEP'S SCOPE RATHER THAN ITS RULES. THE
SWEEP'S POPULATION IS NOW EVERY ROW OF THE IDEA REGISTRY, AND IT IS STATED
AS A POPULATION RATHER THAN AS A PURPOSE (campaign ledger #17). METHOD,
exactly: all 1,779 rows of `book/src/frontier/idea-registry.md` were
extracted programmatically (the count is the file's own); the OPENING 92
CHARACTERS OF EVERY ONE OF THE 1,779 was then read in full, in category
order -- the registry's own authoring rule puts the subject in the opening
clause ('A row is a shelf-mark: what the idea is, plus a pointer') -- and
every row whose opening named a material capability, a craft, a skill, an
institution, a transmission mechanism or a decay mechanism was then read in
FULL TEXT and ruled on in writing. A KEYWORD SIEVE WAS BUILT FIRST AND
ABANDONED AS USELESS, which is worth recording because it is why the reading
was done: a 313-term sieve derived from both corpora's own `introduces`
tokens and titles matched 1,704 of 1,779 rows, and a 141-term sieve of
specific capability nouns matched 1,169 -- a filter that admits 66-96% of
its population narrows nothing, so there was no substitute for reading. WHAT
THE POPULATIONS ARE, SEPARATELY: the population READ is every row's opening
plus the full text of every candidate; the population the CLAIM ranges over
is every row. THE RESIDUAL GAP, NAMED: a row whose subject is not in its
opening 92 characters and which no candidate rule caught is invisible to
this sweep. That is a far smaller gap than the one ledger #17 measured, and
it is not zero. Under that method three items' demands are named by rows
this file never looked at. THE DIRECTION MATTERS AND IS REPORTED: every one
of the three corrections moves an item OFF `absent`, which is the
UNFLATTERING direction for this campaign -- a lower `absent` count makes
Hornvale look better and weakens the thesis the corpus was built to test.
Not one correction ran the other way. A campaign whose errors all ran toward
the flattering result should accept these without argument, and this
sentence is here so that a later reader can check the claim by counting. THE
DISCLOSURE SET MOVED WITH THE VERDICTS, IN ONE DIRECTION ONLY: 12 -> 16.
Four items gained one (`inv-papyrus`, `inv-literature`, `inv-alphabet`,
`inv-library`) because `inv-writing` leaving `absent` unforced them; none
lost one. That is the chosen-vs-inherited rule of ledger #13 behaving
exactly as it predicted -- an item's disclosure obligation depends on its
prerequisites' verdicts, so re-scoring one item rewrites the obligation of
everything downstream of it -- and it is the first time this corpus has
demonstrated rather than asserted it. RULING ON THE ONE ROW THIS CORPUS'S
SIBLING CITES AND THIS CORPUS DID NOT MENTION (fix round 2, under the
cross-corpus obligation `technologies/CLAUDE.md` now states as family law).
`MAP-18` ('Calendrics as computed knowledge -- the sky's true periods [...]
are exact almanac facts, but a culture *knows* them only at the precision it
has attained ... the knowledge ladder is qualitative -> counted -> computed ->
predictive', `raw` -- the first elision drops '(synodic month, year length)'
and the second the row's gating clause, both marked in fix round 3 because
this file holds its `BIO-8` and `TECH-2` quotations to verbatim and owes its
own new ones the same standard) anchors `col-long-count` in
`henrich-2004-extended` and discharges NO demand here: none of these 41
items introduces a calendrical or astronomical capability, because none of
the catalogue's three arcs contains one. The nearest neighbours are the two
`inv-scientific-societies` and `inv-hydrostatics` refusals already recorded,
which turn on the same cut -- a row planning what a culture comes to KNOW
does not discharge an item whose demand is a particular thing known. THE
POINT OF WRITING THIS DOWN is that silence is the detectable signal: a row
cited in one column of decision 0095's matrix and unexamined in the other
makes the comparison across them meaningless, and an agreement test between
two citations is structurally blind to it, because the defect is one
citation.
- **Frozen:** before first measurement, The Kiln (2026-09-11). THE FREEZE IS STRUCTURAL
RATHER THAN PROMISED, which is the discipline decision 0936 established for
`regularities/` and the reason decision 0016 means anything here. This file
is Task 1 of 10 of its campaign, and NO EVALUATION CODE EXISTS IN THE
REPOSITORY AS IT IS COMMITTED: there is no
`cli/tests/suite/technology_corpus.rs`, no closure computation, no anchor
resolver, no report generator and no
`docs/audits/technology-coverage-asimov-1989.md`. Every one of them arrives
in a later task of the same campaign, and nothing in `domains/*` or
`windows/*` reads a corpus file at all (decision 0011: the corpus is data,
the resolver is code). A reader can confirm the freeze from the git history
rather than taking this sentence's word for it. THE ITEM COUNT IS 41, AND
TASK 4'S RESOLVER MUST ASSERT IT so that changing this corpus becomes a
deliberate act (decision 0016), the way the already-shipped
`regularity_corpus.rs` asserts 45 for its own corpus. Stated as an
obligation rather than a fact, deliberately: nothing holds that invariant
while this file is the only thing that exists, and the paragraph above says
so two sentences earlier. An earlier draft wrote it in the present tense and
contradicted itself inside one field. RE-FREEZING IS NOT SOMETHING A LATER
SESSION MAY DO, and for this corpus the disqualification has a specific
shape. The criterion being frozen is a distribution across peoples of who
holds a capability. The quantity that distribution is computed from is
`occ-tech`, and `occ-tech`'s distribution is ALREADY PUBLISHED in several
places a session reads for ordinary reasons -- spec finding F4 states it
outright (10 `classical`, 1 `bronze-working` in the committed gallery),
`book/src/laboratory/generated/the-census/` carries four census metrics over
it, and the almanac and `windows/lot` render it as prose. A session that has
read any of those may not tighten this band for any item, which is all 41 of
them, because the band is uniform. The practical consequence is a scheduling
constraint rather than a style note: band quality on this column can only be
improved by a session that has not read `occ-tech`'s distribution, and after
the successor campaign scores this corpus there will be no such session.
Tighten it now or accept it. WHAT A LATER READER CAN ALWAYS DO WITHOUT
DISQUALIFYING THEMSELVES is audit the discriminating power of the band from
this file alone, with no data: the falsifying worlds are stated in
`provenance` (a fraction of 0.0 and a fraction of 1.0, both reddening), and
the argument for uniformity is stated there too so that it can be attacked
directly.

## Reading this report

This measures whether a PEOPLE acquires, holds, and loses one imported
technology-capability catalogue, resolved against the per-people trajectory
a census would report — an instrument with known bias (decision 0095), never
a standard and never a verdict on the world. `present` and `unmeasured` are
both only WEAKLY checked: a mechanism anchor that resolves (a real
`test:`/`path:`) is not proof the capability is met, only that something at
that location exists. `unmeasured` additionally means the trajectory itself
has not been scored at all — reach passed, nothing about growth, flatness or
loss has been measured yet — so an `unmeasured` count is not a weaker
`present`, it is a different kind of claim. Both are printed here, above the
tally they most affect, per 0136's consequence clause: a reader must pass
this sentence before reaching a score.

## Tally

The eight coverage verdicts below are percentages of 41 — every item MINUS
the ones still `unmeasured` (see the next section). An `unmeasured` item has
not been judged, so counting it here would move a coverage percentage for a
reason unrelated to what that percentage claims to measure.

- present: 0 (0%)
- refused: 0 (0%)
- deferred: 6 (15%)
- absent: 35 (85%)
- inapplicable: 0 (0%)
- grown: 0 (0%)
- flat: 0 (0%)
- lost: 0 (0%)
- **coverage total:** 41

## Unmeasured

None — every item carries a coverage verdict.

THE FINDING THIS CORPUS MAKES SAYABLE: of the 41 item(s) here, Hornvale's
mechanism reaches 0 of them at all (0 `present`, 0 `unmeasured`) — and of
those 0, it can currently represent the LOSS of exactly 0. A single tally
has no way to say this; it takes both counts together.

## Demand set

Not a backlog: this is what each item's own `presupposes` closure (decision
0386) names, derived on read and never authored by hand. An item's demand
set always includes its own `introduces` token, so a root's set has one
entry and a deep item's may have several. `refused` and `inapplicable` items
are excluded — those are decided non-goals, not open demands.

| id | title | verdict | demand set |
|---|---|---|---|
| inv-animal-dom | Animals domesticated — domestic animals kept | deferred | domestic-animals-kept |
| inv-writing | Writing — written record kept | deferred | written-record-kept |
| inv-cart | Wheeled carts — wheeled land haulage | absent | wheeled-land-haulage |
| inv-papyrus | Papyrus — plant fibre writing surface | absent | plant-fibre-writing-surface, written-record-kept |
| inv-literature | Literature — composed work transmitted as text | deferred | composed-work-transmitted-as-text, written-record-kept |
| inv-horse | Horses — riding animal bred | absent | domestic-animals-kept, riding-animal-bred |
| inv-alphabet | Phonetic alphabet — phonemic script | absent | phonemic-script, written-record-kept |
| inv-bridle | Bridle — animal guided by rein | absent | animal-guided-by-rein, domestic-animals-kept, riding-animal-bred |
| inv-saddle | Saddle — seated riding rig | absent | animal-guided-by-rein, domestic-animals-kept, riding-animal-bred, seated-riding-rig |
| inv-library | Libraries — collected holdings outliving their keepers | deferred | collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, written-record-kept |
| inv-parchment | Parchment — hide writing surface | absent | domestic-animals-kept, hide-writing-surface |
| inv-wooden-stirrup | Wooden stirrups — foot support in the saddle | absent | animal-guided-by-rein, domestic-animals-kept, foot-support-in-the-saddle, riding-animal-bred, seated-riding-rig |
| inv-basic-steam-engine | Rudimentary steam motion — steam moves a mechanism | absent | steam-moves-a-mechanism |
| inv-paper | Paper — cheap pulped writing surface | absent | cheap-pulped-writing-surface, domestic-animals-kept, hide-writing-surface |
| inv-metal-stirrup | Metal stirrups — load bearing metal stirrup | absent | domestic-animals-kept, load-bearing-metal-stirrup, riding-animal-bred |
| inv-turnplow | Turnplows — traction tillage of heavy soil | deferred | traction-tillage-of-heavy-soil |
| inv-block-printing | Block printing — text reproduced from a carved form | absent | cheap-pulped-writing-surface, domestic-animals-kept, hide-writing-surface, text-reproduced-from-a-carved-form |
| inv-high-backed-saddle | High-backed saddle — braced saddle transmits shock | absent | animal-guided-by-rein, braced-saddle-transmits-shock, domestic-animals-kept, load-bearing-metal-stirrup, riding-animal-bred, seated-riding-rig |
| inv-iron-horseshoes | Iron horseshoes — shod draught animal | absent | domestic-animals-kept, riding-animal-bred, shod-draught-animal |
| inv-couched-lance | Couched lance — mounted shock charge | absent | animal-guided-by-rein, braced-saddle-transmits-shock, domestic-animals-kept, load-bearing-metal-stirrup, mounted-shock-charge, riding-animal-bred, seated-riding-rig |
| inv-horse-collar | Horse collars — harness transmits animal draught | absent | domestic-animals-kept, harness-transmits-animal-draught, riding-animal-bred, shod-draught-animal, traction-tillage-of-heavy-soil |
| inv-coal-mining | Coal mining — mineral fuel extracted | deferred | mineral-fuel-extracted |
| inv-longbow | Longbows — massed missile volley | absent | massed-missile-volley |
| inv-pike | Pike — massed polearm formation | absent | animal-guided-by-rein, braced-saddle-transmits-shock, domestic-animals-kept, load-bearing-metal-stirrup, massed-polearm-formation, mounted-shock-charge, riding-animal-bred, seated-riding-rig |
| inv-arquebus | Arquebus — handheld firearm | absent | handheld-firearm |
| inv-standing-army | Standing army — permanent force apart from population | absent | animal-guided-by-rein, braced-saddle-transmits-shock, domestic-animals-kept, handheld-firearm, load-bearing-metal-stirrup, massed-polearm-formation, mounted-shock-charge, permanent-force-apart-from-population, riding-animal-bred, seated-riding-rig |
| inv-printing-press | Printing press — movable type mass reproduction | absent | cheap-pulped-writing-surface, domestic-animals-kept, hide-writing-surface, movable-type-mass-reproduction, phonemic-script, written-record-kept |
| inv-scientific-societies | Scientific societies — corresponding body of inquirers | absent | cheap-pulped-writing-surface, corresponding-body-of-inquirers, domestic-animals-kept, hide-writing-surface, movable-type-mass-reproduction, phonemic-script, written-record-kept |
| inv-hydrostatics | Hydrostatics — quantitative law of fluids known | absent | quantitative-law-of-fluids-known |
| inv-gas | Gas state — matter recognised in a third state | absent | matter-recognised-in-a-third-state, quantitative-law-of-fluids-known |
| inv-barometer | Barometers — atmospheric pressure measured | absent | atmospheric-pressure-measured, matter-recognised-in-a-third-state, quantitative-law-of-fluids-known |
| inv-air-pump | Air pumps — air evacuated from a vessel | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, matter-recognised-in-a-third-state, quantitative-law-of-fluids-known |
| inv-air-pressure | Early pneumatics — pressure as a motive force | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, matter-recognised-in-a-third-state, pressure-as-a-motive-force, quantitative-law-of-fluids-known |
| inv-boyles-law | Boyle's law — pressure volume relation known | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, matter-recognised-in-a-third-state, pressure-as-a-motive-force, pressure-volume-relation-known, quantitative-law-of-fluids-known |
| inv-pressure-cooker | Pressure cookers — pressure vessel held above ambient | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, matter-recognised-in-a-third-state, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, steam-moves-a-mechanism |
| inv-miners-friend | Miner's friend — engine raises water from a working | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, engine-raises-water-from-a-working, matter-recognised-in-a-third-state, mineral-fuel-extracted, quantitative-law-of-fluids-known |
| inv-gas-volume-temperature | Gas volume temperature — thermal expansion of gas known | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, matter-recognised-in-a-third-state, pressure-as-a-motive-force, quantitative-law-of-fluids-known, thermal-expansion-of-gas-known |
| inv-newcomen-steam-engine | Newcomen steam engine — engine does sustained useful work | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, engine-does-sustained-useful-work, engine-raises-water-from-a-working, matter-recognised-in-a-third-state, mineral-fuel-extracted, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, steam-moves-a-mechanism |
| inv-latent-heat | Latent heat — latent heat known | absent | latent-heat-known |
| inv-steam-engine | Steam engine — engine efficient enough to spread | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, engine-does-sustained-useful-work, engine-efficient-enough-to-spread, engine-raises-water-from-a-working, latent-heat-known, matter-recognised-in-a-third-state, mineral-fuel-extracted, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, steam-moves-a-mechanism |
| inv-improved-steam-engine | Improved steam engine — rotary power drives general machinery | absent | air-evacuated-from-a-vessel, atmospheric-pressure-measured, engine-does-sustained-useful-work, engine-efficient-enough-to-spread, engine-raises-water-from-a-working, latent-heat-known, matter-recognised-in-a-third-state, mineral-fuel-extracted, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, rotary-power-drives-general-machinery, steam-moves-a-mechanism |

## Items

| id | title | verdict | anchor | contested | disclosure | note |
|---|---|---|---|---|---|---|
| inv-animal-dom | Animals domesticated — domestic animals kept | deferred | registry:BIO-animal-domestication |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. `BIO-animal-domestication` was found by searching the idea registry, so the `deferred` is a positive claim that search produced. Its SELECTION was blind: it is in the `knights` arc's own link list. | THE NEAREST MISS IN THE CORPUS, and it misses on the animal rather than on the institution. `subsistence` is a committed predicate ("a settlement's subsistence mode") whose values are Farming, Herding, Fishing and Foraging (`domains/culture/src/subsistence.rs`), it differs between settlements in one world, and Herding is documented "Pastoral herding" -- so a people that lives off animals is already a fact Hornvale commits and varies. What is missing is the animal: `BIO-animal-domestication` records that the registry "already admits domestication for the plant kingdom (barley, wheat, rice, millet, tuber, with a crops model behind them) and refuses it for the animal kingdom -- every one of the ~30 registered `*-kind` fauna is wild", and the registry bears that out: 39 `*-kind` rows in `book/src/reference/concept-registry-generated.md`, peoples and beasts together, and not one of the beasts domestic. A herding people herds nothing in particular. The row is `raw`, so this is `deferred` rather than `absent`: what it would take is the row's own content, a working animal as "the missing half of an existing axis". PREREQUISITE DROPPED: built on `biped` in the catalogue, outside the corpus. Swept against `BIO-8`, which `BIO-animal-domestication` opens by sharpening: `BIO-8` is the broader row and names domestication generally, so the sharper row is the better anchor and is the one cited. |
| inv-writing | Writing — written record kept | deferred | registry:MAP-8 |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. `NARR-monument-writes-itself` was found by searching the idea registry, considered as a `deferred` anchor and refused; a reader who reads that row as planning written records in the world rather than their RENDERING should move this item to `deferred` and the five items downstream of it with it. Its SELECTION was blind: it is in the `republic-of-letters` arc's own link list. FIX ROUND 1: THIS DISCLOSURE'S OWN PREDICTION CAME HALF TRUE, AND THE HALF THAT FAILED IS THE INTERESTING ONE. It said a reader who read `NARR-monument-writes-itself` as planning written records 'should move this item to `deferred` and the five items downstream of it with it'. The item did move -- on `MAP-8`, a row this file never cited, not on the row the prediction named -- and the five downstream items did NOT follow: `inv-literature` moved on its own row (`DOM-aesthetics`) and `inv-library` behind it, while `inv-papyrus`, `inv-alphabet`, `inv-paper`, `inv-block-printing` and `inv-printing-press` stayed `absent` on their own demands. A prerequisite leaving `absent` unforces its dependents' verdicts; it does not raise them. | TWO NEAR-MISSES, AND BOTH ARE INSTRUCTIVE FAILURES OF THE SAME KIND: the vocabulary for writing exists and nothing in any world writes. (1) `write` and `read` are registered concepts -- `write`, "to set words down in writing"; `read`, "to take meaning from written words" -- and they are INERT. `LANG-in-character-acts-are-unspeakable` records that seven minted concepts including `read` and `write` "render `Gap \| Gap` for every species in every world, because nothing grants them `Steeped` or `KnowsOf`", verified in the committed `book/src/reference/concept-manifest-generated.md`. No culture can even SAY the act, let alone perform it. (2) `Orthography` (`domains/language/src/typology.rs:90`) looks like a writing system and is not: its own doc says "How this family's segments are spelled in the romanization. A view over `Segment`, so this field alone moves no stream draw". It is a reader-facing spelling convention for committed name strings, chosen per language family, with no in-world existence -- a fact about how the artifact prints, not about what a people can do. WHAT WOULD CHANGE THE VERDICT: a committed fact that some peoples keep written records and others do not. `NARR-monument-writes-itself` is the nearest plan (it makes literacy a gate on reading an inscription) and it is `elaborated`, not shipped; it was considered as a `deferred` anchor and refused here, because that row plans the RENDERING of a monument's text and treats literacy as a gate it assumes rather than as a capability a people acquires and loses, which is this corpus's demand. PREREQUISITE DROPPED: built on `stone-tool` in the catalogue, outside the corpus. The row's own closing sentence is the evidence for that refusal rather than a paraphrase of it: "Rendering, not authoring, is the whole cost." A row whose self-assessment is that the content already exists and only needs displaying does not plan a people that keeps written records. Swept against `BIO-8` and `TECH-2`: neither names writing, literacy or a record. AMENDED AFTER FREEZE (campaign ledger #17, fix round 1): THIS ITEM IS NOW `deferred` ON `MAP-8`, and the verdict above is not the one this corpus froze with. `MAP-8` ('Writing as a culture acquiring its own ledger -- oral = phenomena, literate = freezing phenomena into facts; borrowed scripts as contact fossils', `elaborated`) names `written-record-kept` as its entire content: a culture acquiring a ledger IS a people keeping a written record, so the row plans THE DEMAND ITSELF rather than a prerequisite of it, which is the rule this file established. It is not `shipped`, so `deferred` is admissible. The sentence two paragraphs up -- 'WHAT WOULD CHANGE THE VERDICT: a committed fact that some peoples keep written records and others do not' -- is the condition `MAP-8` meets, and this item's `disclosure` predicted the consequence before the row was found. WHY THE ORIGINAL VERDICT WAS WRONG, AND IT IS A SCOPE FAILURE RATHER THAN A MISREPORTED ONE -- THE FILE STATED ITS OWN POPULATION HONESTLY AT BOTH LEVELS, which is worth saying plainly because the opposite reading is the easy one. `provenance` is headed 'THE `absent` COLUMN WAS SWEPT AGAINST TWO NAMED IDEA-REGISTRY ROWS', and this note already ended 'Swept against `BIO-8` and `TECH-2`: neither names writing, literacy or a record', which is true. Nothing here concealed anything. What was wrong is that a TWO-ROW population cannot support a verdict for 38 items, and the two rows were named by the dispatching controller rather than chosen by a search -- so the sweep's population was set before the question was asked. That is the fourth instance of campaign ledger #14's pattern (the check's population narrower than the claim's) and the second of them the controller's own. Measured at the amendment: this file cited 18 distinct registry rows and its sibling `henrich-2004-extended` cited 49, under identical rules. |
| inv-cart | Wheeled carts — wheeled land haulage | absent |  |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`.  Its SELECTION was blind: it is in the `knights` arc's own link list. | Nothing in the engine has a wheel. The honest near-miss is elsewhere in the same economic space: `occ-function` commits Trade, "a waypoint or market on a trade route" (`domains/history/src/record.rs`), so a world has routes and market seats -- and no conveyance on them. Haulage capacity never enters any computation: a community's strength is population times `tech_weight` and its delving is metres per head per epoch, both of which read population where a cart would read carrying capacity. PREREQUISITE DROPPED: the catalogue builds this on `copper`, which is outside the corpus, so the derived demand set under-describes it. Swept against `BIO-8` and `TECH-2`: neither names haulage, draught or the wheel. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `MAP-61`'s connection graph plans roads as graph EDGES and `UNI-5` ('Authored transmission media (the "stones") -- comms/energy/transport as independent latent graph-construction systems', `elaborated`) plans a world-potential a culture realizes at a craft fraction -- a route and an exotic medium respectively, neither a vehicle. Nothing in 1,779 rows names a wheeled conveyance. |
| inv-papyrus | Papyrus — plant fibre writing surface | absent |  |  | NOT BLIND, AND CHOSEN ONLY SINCE THE FIX-ROUND-1 AMENDMENT: its sole prerequisite `inv-writing` moved from `absent` to `deferred`, so nothing upstream forces this verdict any more and it rests on a search -- which is the disclosure rule working as ledger #13 intended, and a worked example of why a root-keyed check under-covers. The search: all 1,779 registry rows' openings read, no row names a writing surface, `MEM-8` read in full and refused in the note. The session that ran it had already read `TechHorizon`, `tech_for` and this campaign's findings. | Downstream of `inv-writing` (which was `absent` when this was written and is `deferred` since fix round 1, so the clause that once read 'this cannot score higher' no longer holds -- the item stays `absent` on its OWN demand, not on its prerequisite's); recorded with its own near-miss because the near-miss is a genuinely different one. Hornvale models plants as a CLIMATE fact rather than a material a people processes: `Crop` (`domains/climate/src/crops.rs`) carries barley, wheat, rice, millet, tuber and vine, and the module's own doc says "A crop is a climate fact -- a band of temperature and moisture on arable ground". There is no fibre, no sedge, and no step between a standing plant and a made thing. PREREQUISITE DROPPED: built on `nation` in the catalogue, outside the corpus. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1) AND NOTHING MOVES IT: no row names a writing SURFACE. `MEM-8` ('The artifact channel ... Inscribed rots (the unpaid scribe)', `raw`) is the nearest, and it plans a transmission CHANNEL whose medium is assumed rather than a material anyone learns to make. |
| inv-literature | Literature — composed work transmitted as text | deferred | registry:DOM-aesthetics |  | NOT BLIND, AND CHOSEN SINCE THE FIX-ROUND-1 AMENDMENT (its prerequisite `inv-writing` is now `deferred`). The `deferred` on `DOM-aesthetics` is a positive claim produced by reading all 1,779 registry row openings and then that row in full; it is also the amendment this corpus is least sure of, and the note states the objection and the consequence of taking it. A reader who reverses it returns this item to `absent` and `inv-library` to inherited. | Downstream of `inv-writing`. The near-miss is the most substantial in the corpus and it is worth being exact about why it does not count: Hornvale has a large, shipped knowledge-and-transmission layer -- claims propagate between people, degrade with each boundary they cross, and `KNOW-boundary-not-accumulation` records damage accumulating "as a continuous width, resolved to a rung only at emit". All of it is ORAL. A composed work whose wording survives its teller is exactly the thing that layer's physics is built to deny, so this item's demand is not a gap in the transmission model but a different mechanism beside it. AMENDED AFTER FREEZE (campaign ledger #17, fix round 1): `deferred` ON `DOM-aesthetics`, AND THIS IS THE MOST CONTESTABLE OF THE THREE AMENDMENTS -- a reviewer should check it first. The row ('**aesthetics** crate -- the generative output of expressive artifacts (text / image / music / built space) from a small per-culture vector behind a content->render seam ... Realizes EXP-1/EXP-2/EXP-3a', `raw`) names TEXT as the first of its own enumerated elements, produced from a PER-CULTURE vector, which is `composed-work-transmitted-as-text` at the grain this family requires: a work a people makes, varying between peoples. THE ARGUMENT AGAINST, stated so it can be taken: the row plans GENERATION behind a content->render seam and says nothing about a work PERSISTING, while the note above locates this item's demand precisely in persistence ('a composed work whose wording survives its teller'). A reader who takes that objection should return this item to `absent`, and `inv-library` becomes inherited again with it. `EXP-1` ('One seeded grammar engine, four media (text / image / music / space) from a small per-culture vector', `elaborated`) is the row `DOM-aesthetics` says it realizes; one anchor is cited and the second recorded, per the no-double-anchoring rule. |
| inv-horse | Horses — riding animal bred | absent |  |  | NOT BLIND: this item is CHOSEN rather than inherited -- its sole prerequisite `inv-animal-dom` is `deferred`, so nothing upstream forces the `absent` -- and `inapplicable` was additionally considered and refused for it, and the refusal was authored by a session that had read the model. A known limit of the argument, recorded rather than repaired: it reasons about the catalogue's INSTANCE -- no equine among the 39 `*-kind` rows -- where this corpus elsewhere scores the CAPABILITY. | ITS PREREQUISITE IS `deferred` AND THIS ITEM IS STILL `absent`, which is the weakest-demand discipline doing its job rather than a contradiction. `inv-animal-dom` has a registry row behind it; a riding animal has none, and the gap is two-layered: there is NO EQUINE at all among the 39 `*-kind` rows -- the roster runs to `giant-elk-kind`, `giant-goat-kind`, `rhinoceros-kind` and `woolly-mammoth-kind` alongside `owlbear-kind`, `otyugh-kind` and `xorn-kind` -- so even the wild ancestor the catalogue's chain needs is missing, and `BIO-animal-domestication` names the domestication half without naming that. `inapplicable` was considered and refused: the absence of an equine reads as a roster that grew on demand, not as a world deliberately lacking a precondition. Swept against `BIO-8`: it names domestication and agriculture without naming a mount, and `BIO-animal-domestication`, which sharpens it, is already cited on this item's prerequisite. Swept against `TECH-2`: its ladder is pottery, storage, smelting and metallurgy and names no animal at all. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): no row names a riding animal, and `BIO-breed-as-kind` is `rejected` ('do not mint a -kind per working-animal breed'), so the nearest neighbouring idea is a closed question rather than a plan. |
| inv-alphabet | Phonetic alphabet — phonemic script | absent |  |  | NOT BLIND, AND CHOSEN SINCE THE FIX-ROUND-1 AMENDMENT (`inv-writing` is now `deferred`). The search that keeps it `absent`: `alphabet` returns one row in 1,779 (`SKY-25`, about presiding-belief selection); `MAP-8`'s borrowed-scripts clause was read in full and refused in the note as the input-assumption case. The near-miss the note already records -- that `domains/language` computes the phonemic analysis a script would encode while no people has noticed it -- was read from the source by a non-blind session. | Downstream of `inv-writing`. The near-miss is sharper here than anywhere else in the arc and it cuts the other way: `domains/language` DOES hold a per-family phoneme inventory, a syllable law, and a typology bundle, so the analysis a phonemic script encodes is already computed -- see `book/src/reference/phonology.md`. What no world has is a people that has NOTICED it. The catalogue's claim is that someone discovered one sign per sound; Hornvale's phonology is the author's model of the language, held outside the world, and `Orthography` spells it for the reader rather than for the speakers. A capability the engine exercises on a people's behalf is not a capability the people holds. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1) AND NOTHING MOVES IT. `MAP-8`'s 'borrowed scripts as contact fossils' clause names a script being BORROWED, which presupposes one exists and is therefore the input-assumption case rather than a plan for `phonemic-script`; the row is cited on `inv-writing` for the demand it does plan. |
| inv-bridle | Bridle — animal guided by rein | absent |  |  |  | Downstream of `inv-horse`. No tack, no harness and no control relationship between a person and an animal exists; the only committed person-to-creature relations in the registry are perceptual and social (`care`, `custody`, `dependency`, `association`), all of which hold between people. |
| inv-saddle | Saddle — seated riding rig | absent |  |  |  | Downstream of `inv-bridle`. Nothing a people makes is modelled as a made thing with a use: the registry's manufactured objects (`anvil`, `loom`, `bench`, `brazier`, `bed`, `door`, `altar`) are scene furniture a possessed body stands beside, committed as `thing \| object`, never as a capability a community holds or loses. |
| inv-library | Libraries — collected holdings outliving their keepers | deferred | registry:MEM-4 |  | NOT BLIND, AND CHOSEN SINCE THE FIX-ROUND-1 AMENDMENT, which is the whole point of this disclosure: before it, `inv-literature` was `absent` and the weakest-demand rule fixed this item's verdict whatever the registry said, so no search could have moved it. Once the prerequisite moved, `MEM-4` and `MEM-5` -- both found by reading all 1,779 openings and then both rows in full -- became load-bearing. The dependency between the two amendments is stated in the note rather than left for a reader to reconstruct. | Downstream of `inv-literature`. The near-miss is an institutional one and it is real: `occ-function` commits Cult, "a shrine or temple seat", so a world already has a seat whose purpose outlives the individuals in it, and the history bake carries tenure across generations. What it does not carry is HOLDINGS -- a durable store of content, as against a durable store of food (community stores exist and are lost on closure). A library is the second kind of store and the engine models only the first. PREREQUISITE DROPPED: built on `nation` in the catalogue, outside the corpus. AMENDED AFTER FREEZE (campaign ledger #17, fix round 1): `deferred` ON `MEM-4`. The note above names this item's demand exactly -- HOLDINGS, 'a durable store of content' -- and `MEM-4` ('Preservation as craft -- a conservator institution spends surplus specifically to *lower* the decay rate of chosen items; the active-voice complement to MEM-1's rot', `raw`) plans precisely that: an institution paying to keep chosen content from melting. THE ROW THAT SUPPLIES THE OTHER HALF IS `SOC-11`, NOT `MEM-5`, AND THIS SENTENCE SAID `MEM-5` FOR TWO FIX ROUNDS (corrected in fix round 3). 'A queen's court OUTLIVES its courtiers' is `SOC-11`'s clause (`book/src/frontier/idea-registry.md:451`); `MEM-5` contains no form of the word 'outlive' at all, and its lifecycle clause is the opposite emphasis -- verbatim, 'each with a MAP-7-style founding->neglect->death lifecycle', the institution's own MORTALITY rather than its persistence past its members. The original sentence quoted `MEM-5` with an ellipsis that bridged silently into a different row. HOW IT SURVIVED: the quotation reads plausibly and nobody diffed it against the row until the third review -- the same non-verification that produced this file's ordering defect, in a place where a single `grep outliv` would have settled it. Repo-wide, 'courtier' occurs in exactly two places: `SOC-11`'s row and this note. SO THE UNION IS `MEM-4` UNION `SOC-11` ('The institutional layer -- a mid-scale social structure between the community and the persona: a court, a bureaucracy, a guild, a temple hierarchy, a slave system, an army ... an institution is a persistent entity with *roles*, a *hierarchy*, and a *function*, cross-cutting communities (a trade guild spans ports; a queen's court outlives its courtiers)', `raw`). Both rows are `raw`, so the verdict is unaffected and the cited anchor `MEM-4` -- which genuinely plans the holdings half on its own -- does not move. `MEM-5` is left named here rather than deleted, because a corrected misquote should remain findable from the thing it got wrong. THE VERDICT WAS ONLY REACHABLE ONCE `inv-literature` MOVED: while that prerequisite was `absent` the weakest-demand rule fixed this item at `absent` however many rows named its demand, which is why these two amendments must be read together. FIX ROUND 2: NO SINGLE ROW NAMES THE WHOLE DEMAND, AND THE ANCHOR SHOULD NOT BE READ AS CLAIMING OTHERWISE. `collected-holdings-outliving-their-keepers` has two halves and they are discharged by `MEM-4` UNION `SOC-11`: `MEM-4` supplies the holdings (a conservator institution paying to lower the decay rate of chosen content) and `SOC-11` supplies the outliving ('a queen's court outlives its courtiers'). THIS SENTENCE SAID `MEM-5` WHEN IT WAS WRITTEN, INHERITING A MISQUOTE FROM THE ROUND BEFORE IT, which is why the correction above is stated at length rather than applied silently: a fix round that restates an unverified quotation propagates it. Family law's rule 2 asks whether THE row names the capability, and strictly neither does alone. The verdict is kept because both rows are `raw`, both plan deliverables, and between them they plan this item's own demand rather than a prerequisite of it -- but a reader weighing the rule literally is entitled to call this the weakest of the three post-freeze amendments on that ground, and the no-double-anchoring convention is what forces one row into the field and the other into this note. If rule 2 is later tightened to forbid a union, this item returns to `absent`. AND THE CORRECTION OPENS A CROSS-CORPUS TENSION THIS NOTE RECORDS RATHER THAN RESOLVES. `SOC-11` is the row the sibling column `henrich-2004-extended` REFUSES for `col-palace-accounting`, on the cut that an institution with roles and a hierarchy is not the record it keeps -- and here `SOC-11` is credited with supplying half of a demand about holdings that outlive their keepers. THE TWO COLUMNS THEREFORE APPLY THAT CUT IN OPPOSITE DIRECTIONS ON ONE ROW. The honest reading of why is that the halves differ: there the demand is the ACT of accounting, which an institution can exist without performing, while here the demand's second half is institutional PERSISTENCE, which is what `SOC-11` plans. Whether that distinction survives scrutiny is not settled, and settling it by moving a verdict is forbidden here -- it is carried as a campaign follow-up. It is recorded because a matrix read ACROSS its columns is exactly where a cut applied two ways does its damage, and neither column can show it alone. |
| inv-parchment | Parchment — hide writing surface | absent |  |  | NOT BLIND, and this item is the reason the disclosure rule is stated as CHOSEN rather than as ROOT (campaign ledger #13): its sole prerequisite is `deferred`, so the lattice does not force its verdict, yet it is not a root and an earlier round's check missed it on exactly that difference. The `absent` rests on a search of the registry and the concept vocabulary run by a session that had read the model. Audit it at the substance vocabulary: if `blood` and `bone` being registered is read as a people working animal material, this item moves. | Downstream of `inv-animal-dom`, which is `deferred`, so NOTHING UPSTREAM FORCES THIS `absent` and the verdict is the item's own -- which is why it carries a disclosure and a sweep where its neighbours in the arc do not. The checkable argument, and the whole of it: no material is committed as coming off a creature at all. `blood` and `bone` are registered as substances in the scene vocabulary, a possessed body's perceptual nouns, not as yields a people takes and works; there is no hide, no tanning and no made surface anywhere in the engine. A SECOND HALF OF THE ARGUMENT WAS WITHDRAWN RATHER THAN REPAIRED: an earlier draft also argued that parchment needs a written record, and `inv-writing` IS NOT IN THIS ITEM'S `presupposes` CLOSURE -- the catalogue builds parchment on `animal-dom` alone -- so that half rested on a lattice edge nothing in this file draws and nothing could check (decision 0386 derives the demand set from these edges only). The edge was NOT added: every `presupposes` edge in this corpus is transcribed from the catalogue's own "Built on" list, and authoring one to support an argument would make the lattice partly mine and break the property that every edge is checkable against the source. Adding it would also have put an `absent` item in this closure and so removed this item's disclosure obligation altogether -- a repair that erases the finding, and not the reason the edge was refused. SWEPT against `BIO-8`, which names nightsoil -- an animal by-product, but as an INPUT TO AGRICULTURE rather than a material a people works, and its domestication half reaches this item only through its prerequisite, where it is already cited; and against `TECH-2`, whose ladder is pottery, storage, smelting and metallurgy and names no hide, surface or written thing. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): no row names a writing surface, and the one row naming a tanner (`CUL-12`) plans a purity taboo attached to that rung rather than the preparation of a hide -- the same refusal the sibling corpus records for `col-tanning`. |
| inv-wooden-stirrup | Wooden stirrups — foot support in the saddle | absent |  |  |  | Downstream of `inv-saddle`. Included whole because the arc links it and the selection rule forbids dropping an item; nothing in the corpus builds on it, so it is a leaf of the catalogue's own chain and contributes one demand that no other item's closure inherits. |
| inv-basic-steam-engine | Rudimentary steam motion — steam moves a mechanism | absent |  |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. The verdict rests on knowing `tech_weight` to be the engine's only conversion of a physical quantity into an effect on a people. Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list. | A root of the steam arc in this corpus, so its verdict is chosen rather than inherited. Hornvale has fire (`fire` is a registered substance and `brazier` a scene object) and it has no mechanism, no work, and no energy accounting anywhere: the only place a physical quantity is converted into an effect on a people is `tech_weight`, which multiplies population and is a scalar on a four-rung clock. PREREQUISITE DROPPED: built on `fire` in the catalogue, outside the corpus -- which is the one dropped prerequisite Hornvale would actually have satisfied. Swept against `TECH-2`: its ladder ends at metallurgy and names no mechanism, no work and no engine. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `TECH-4` is the only row in which the word steam appears, and it appears as an ANALOGY for electricity ('an instance of the ladder like steam'), which is a comparison rather than a plan. |
| inv-paper | Paper — cheap pulped writing surface | absent |  |  |  | Downstream of `inv-parchment`. The demand that distinguishes it from its prerequisite is CHEAPNESS -- a surface abundant enough to change who writes -- and cost is the axis Hornvale most completely lacks: `ECON-livelihood` is `raw` and its row records that "no economy domain exists yet". Nothing in any world has a price. |
| inv-metal-stirrup | Metal stirrups — load bearing metal stirrup | absent |  |  |  | Downstream of `inv-horse`. The metalworking half is the more interesting absence and it is the `anvil` case exactly: the registry carries `anvil`, "a heavy iron block a smith hammers metal against", as a scene object, so a possessed body can stand next to a smith's anvil in a world where no people holds smithing. `occ-tech`'s rungs NAME bronze-working and iron-working, which is the closest the engine comes -- and a rung is a date-derived label on a community, not a capability it acquired. PREREQUISITE DROPPED: built on `steel` in the catalogue, outside the corpus. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through `steel`, which is a DROPPED PREREQUISITE rather than this item's own demand (a load-bearing fitting), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-turnplow | Turnplows — traction tillage of heavy soil | deferred | registry:BIO-8 |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. THIS IS THE ITEM THE INVERTED DISCLOSURE RULE EXISTS FOR: the first draft scored it `absent`, in the direction that flattered the campaign's thesis, and carried no disclosure inviting anyone to audit the search (campaign ledger #12). Its SELECTION was blind: it is in the `knights` arc's own link list. | RE-SCORED FROM `absent` BY THE REGISTRY SWEEP, and the original verdict is the instance that motivated the sweep. `BIO-8` ("Domestication & agriculture -- the culture layer harnessing biosphere fields; staple crop per biome, the Boserup plough (SOC-2's input), nightsoil (BIO-5), famine as paleoclimate (MAP-6) x carrying capacity (MAP-7)" (`raw`) names THE BOSERUP PLOUGH, which is this item's demand under its own name, and the first draft of this corpus cited the row NOWHERE -- while citing `BIO-animal-domestication`, whose first two words are "Sharpens [[BIO-8]]". A root scored `absent` with nothing upstream forcing it is the verdict most in need of a search, and it got the least. WHAT `deferred` MEANS IS UNBUILT, and today's state is a near-miss: `subsistence: Farming` ("Settled agriculture") is committed per settlement and varies, `occ-function` commits Agrarian, and the `Crop` model says which staple a place supports -- so a world knows that a people farms and what grows there, and nothing anywhere says HOW the ground is worked. `occ-function`'s Agrarian also CONFLATES farming with herding in one label, so no world holds the one without the other. THE REFUSAL THAT WAS TESTED AND FAILED: `BIO-8` frames the plough as "SOC-2's input", which looks like the ground on which this corpus refuses `NARR-monument-writes-itself` for `inv-writing` -- a capability a row ASSUMES rather than delivers. It does not hold here. The plough is one of `BIO-8`'s own enumerated elements and the parenthetical names where its output ROUTES, not a precondition it presumes; and another element of the same list, "staple crop per biome", has already SHIPPED as `domains/climate/src/crops.rs`, which settles that `BIO-8` enumerates deliverables. PREREQUISITES DROPPED: built on `plow` and `steel`, both outside the corpus. |
| inv-block-printing | Block printing — text reproduced from a carved form | absent |  |  |  | Downstream of `inv-paper`. Reproduction-without-re-authoring has no analogue in the engine, and the transmission layer is built on the opposite assumption: every retelling degrades, by design (`KNOW-boundary-not-accumulation`). A carved form that makes the hundredth copy identical to the first is a mechanism that would CONTRADICT shipped physics rather than extend it, which is a more interesting `absent` than a missing predicate. |
| inv-high-backed-saddle | High-backed saddle — braced saddle transmits shock | absent |  |  |  | Downstream of two `absent` items. The demand is mechanical -- a rig that transmits an impact into a frame rather than into the rider -- and the engine has no force, no impact and no body mechanics at any scale; combat between peoples resolves as a comparison of population times `tech_weight`. |
| inv-iron-horseshoes | Iron horseshoes — shod draught animal | absent |  |  |  | Downstream of `inv-horse`. What makes a horseshoe a capability rather than an object is that it extends an animal's WORKING LIFE on hard ground -- a durability term on a productive asset. Hornvale has no productive assets and no wear on anything a people owns; the only durability modelled is a community's stores, which decay at a flat per-epoch rate and are destroyed on closure. PREREQUISITE DROPPED: built on `steel`, outside the corpus. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through `steel`, which is a DROPPED PREREQUISITE rather than this item's own demand (a durability term on a working animal), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-couched-lance | Couched lance — mounted shock charge | absent |  |  |  | Downstream of two `absent` items. Worth a note anyway because it names the thing `tech_weight` is standing in for: a tactic, held by some peoples and not others, that changes the outcome of a meeting between them. Hornvale's answer is a single scalar multiplier on population, 1.0/1.5/2.25/3.0 by rung -- which spec finding F5 identifies as "the progress scalar ... a civilisation with a *level*" that `book/src/frontier/frontier.md` rejects by name. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through the lance-head's metal, which is a DROPPED PREREQUISITE rather than this item's own demand (a mounted shock charge, which is a tactic), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-horse-collar | Horse collars — harness transmits animal draught | absent |  |  |  | Downstream of two `absent` items. The catalogue's own chain is the interesting part: it builds the collar on the PLOUGH and the HORSESHOE rather than on the horse, i.e. on a use and a durability, which is a capability-shaped dependency rather than an artifact-shaped one. Nothing in Hornvale composes capabilities that way; `tech_for` is monotone in a year and has no inputs. |
| inv-coal-mining | Coal mining — mineral fuel extracted | deferred | registry:TECH-3 |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. `TECH-3` was found by searching the idea registry, by a session that had already read `occ-delve-depth` and knew the shipped half existed. Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list. | A root, and the only item in the steam arc that is not `absent`. Hornvale genuinely mines: `occ-function` commits Mine, "Extraction -- ore, stone, salt", `occ-delve-depth` commits "how far below its seat the occupation drove a working, in metres", both vary between occupations in one world, and ore deposits themselves shipped with The Lode (2026-07-22). `TECH-3` records precisely what is left: "Ore deposits shipped (The Lode, 2026-07-22); mining/refinement/tech-gating still deferred (the event-features + economy rungs)" -- the row is `raw`. What this item's demand adds beyond the shipped half is that the mineral is a FUEL: something extracted to be burnt, which is what makes coal a prerequisite of an engine rather than a commodity. Hornvale's mines yield no named mineral at all, only a depth. PREREQUISITE DROPPED: built on `steel`, outside the corpus. Swept against `TECH-2`, whose "kiln temperature gates smelting" is downstream of extraction rather than naming it; `TECH-3` names mining itself and is the anchor. |
| inv-longbow | Longbows — massed missile volley | absent |  |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`.  Its SELECTION was blind: it is in the `knights` arc's own link list. | A root, so the verdict is chosen. The demand is a capability whose value depends on being held COLLECTIVELY -- a volley, not an archer -- and the engine has no formation, no tactic and no per-individual contribution to a collective outcome. PREREQUISITE DROPPED: built on `crossbow`, outside the corpus. TECH-2 REFUSED: its "metallurgy yields ... weapons" names neither a bow -- which is wood and sinew, not metal -- nor this item's actual demand, which is explicitly COLLECTIVE (a volley, not an archer). Swept against `BIO-8`: nothing. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `EXP-8` ('Martial traditions as culture, not choreography -- embodied combat practice fused with dance, ritual, and belief ... arising downstream of disarmament under coercion', `elaborated`) is the only row that treats fighting as a transmitted practice, and it is refused twice over: its own text says 'the payload is the tradition and its significance, not the moves', and its generative premise is a people DENIED weapons, which is the opposite of a massed volley. |
| inv-pike | Pike — massed polearm formation | absent |  |  |  | Downstream of `inv-couched-lance`. The catalogue puts the pike after the lance because it is an ANSWER to it -- a counter a people adopts because a neighbour has something. That is the shape spec finding F3 found missing: section 5.3 of the Living Community spec justified committing the tech horizon on the grounds that "a people's trajectory is globally dependent (contact, displacement)", and `tech_offset` is drawn once at genesis, inherited down the lineage, and never mutated. No people in any world has ever changed what it can do because of a neighbour. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through the pike-head's metal, which is a DROPPED PREREQUISITE rather than this item's own demand (a massed formation), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-arquebus | Arquebus — handheld firearm | absent |  |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. Both the ceiling ruling and the `TECH-2` refusal were authored here rather than inherited. Its SELECTION was blind: it is in the `knights` arc's own link list. | A root, so the verdict is chosen, and this is the clearest case for the ceiling ruling stated in `provenance`. `TechHorizon`'s own doc calls Classical "the ceiling this engine models", so a firearm is past the end of the enum. It is scored `absent` rather than `inapplicable` or `refused` because spec finding F1 established that NO decision record covers the four-rung horizon -- its whole rationale is one paragraph, fact #7 of 12, in a spec about ruins -- and an unratified code fact is not a deliberate world choice. PREREQUISITES DROPPED: built on `artillery` and `crossbow`, both outside the corpus. TECH-2 REFUSED, and for a root this is the refusal most worth auditing: the row's "metallurgy yields tools, weapons" supplies a barrel's MATERIAL, and this item's own demand is a chemical propellant in a tube. `TECH-2`'s ladder runs pottery -> kiln -> smelting -> metallurgy and contains no combustion-as-propellant at any rung; no row in the idea registry mentions gunpowder at all. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): `firearm` and `gunpowder` return ZERO rows across all 1,779, and `EXP-8` is refused for the reasons recorded on `inv-longbow`. The sibling corpus's `col-firearms` reached the same null independently. |
| inv-standing-army | Standing army — permanent force apart from population | absent |  |  |  | Downstream of two `absent` items, and the item whose demand Hornvale contradicts most exactly. The near-miss is `occ-function: Fort`, "a garrisoned defensive point", committed and varying. But the demand is a force MAINTAINED APART FROM the population that feeds it, and `Bake::strength` and `roller_strength` both compute strength AS population times `tech_weight` -- so a people's fighting power is its headcount by construction, and an army cannot be distinguished from the people it is drawn from even in principle. This is the one item where `absent` understates the finding: the demand is not unmet, it is unrepresentable without changing a formula that raiding, delving depth and residue structures all read. SWEPT AGAINST THE WHOLE REGISTRY (fix round 1) AND ONE ROW NAMES THIS ITEM'S DEMAND, found by the reviewer's own probes rather than by that sweep (fix round 2): `SOC-11` ('The institutional layer -- a mid-scale social structure between the community and the persona: a court, a bureaucracy, a guild, a temple hierarchy, a slave system, AN ARMY', `raw`) names an army among its own enumerated elements, which is `permanent-force-apart-from-population`. IT CHANGES NO VERDICT TODAY, and the reason is the weakest-demand rule: this item's prerequisites `inv-arquebus` and `inv-pike` are both `absent`, so its verdict is inherited and a row naming its own demand cannot raise it -- which is also why it carries no `disclosure`. It is recorded because it becomes LOAD-BEARING the moment either prerequisite moves, and an item whose note is silent about a row that would then decide it is the shape this campaign keeps producing. The sibling corpus read and refused `SOC-11` for `col-tanning` and `col-palace-accounting` in the same round, on the distinction that an institution with roles is not the record it keeps; an ARMY is different, because here the institution IS the demand. |
| inv-printing-press | Printing press — movable type mass reproduction | absent |  |  |  | Downstream of two `absent` items, and past the modelled ceiling. PREREQUISITE DROPPED: built on `steel`, outside the corpus. Recorded without further near-miss analysis because it inherits `inv-block-printing`'s contradiction with the transmission layer and `inv-paper`'s missing cost axis, and restating them here would duplicate two notes rather than add one. TECH-2 REFUSED: the row's metallurgy would supply cast type's MATERIAL -- the dropped `steel` -- and this item's own demand is mass reproduction. |
| inv-scientific-societies | Scientific societies — corresponding body of inquirers | absent |  |  |  | Downstream of `inv-printing-press`. Its demand is the most explicitly social-epistemic in the corpus -- a durable body whose members correspond -- and Hornvale's knowledge layer is the part of the engine with the most shipped machinery, which makes the miss specific rather than broad: transmission is modelled between INDIVIDUALS who meet, and `SOC-information-economy` (`raw`) proposes the strategic layer above it. An institution that outlives its members and transmits deliberately at a distance is neither. PREREQUISITE DROPPED: built on `university`, outside the corpus. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1); two rows were read in full and both refused. `MEM-5` names an institution's cargoes as memory, legitimacy, exchange and skill -- not inquiry. `LANG-49` ('The epistemic arc -- do cultures ever discover proto-mathematics, proto-philosophy, or proto-medicine as EMERGENT reasoning capabilities') plans the CAPACITY to discover and says of itself that it is 'named as a direction, not designed'; a row planning the capacity to inquire is a prerequisite of a corresponding body of inquirers, not the body. The verdict is inherited through `inv-printing-press` in any case. |
| inv-hydrostatics | Hydrostatics — quantitative law of fluids known | absent |  |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. This is the verdict the corpus leans on hardest -- five other items cite it for the general argument -- and it rests on a reading of the whole `KNOW-*` program, so a reader who finds a row proposing general-claim knowledge moves six items at once. Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list. | A root, so the verdict is chosen, and it is the representative case for all six discovery items in the steam arc. Hornvale's knowledge model holds PARTICULAR FACTS -- where a thing is, who did what, what was seen -- and carries them with provenance, degradation and belief. A law of nature is a different object: a general claim, true of cases never observed, whose value is that it predicts. `KNOW-study` (`raw`) is the nearest row and it proposes measuring how much of a world's knowable TRUTH a walker covers, which is the particular kind again. No world can hold a general claim, correct or mistaken, so no people can hold or lose one. PREREQUISITE DROPPED: built on `geometry`, outside the corpus. Swept against `BIO-8` and `TECH-2`: neither names natural philosophy, and the `KNOW-*` program is about particular facts and their transmission. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `LANG-49` is the one row that plans a culture discovering anything, and it plans the CAPACITY ('does a culture ever notice a repeatable cause-and-effect regularity'), never a law; its status is partly `shipped` and its own text calls it a direction rather than a design. A row planning the capacity to discover does not discharge an item whose demand is a specific discovery -- the same cut this file applies to `TECH-2` and the metal items. |
| inv-gas | Gas state — matter recognised in a third state | absent |  |  |  | Downstream of `inv-hydrostatics`; a discovery item, see that item's note for the general argument. The engine's substances (`fire`, `earth`, `blood`, `bone`, `water`) are scene vocabulary for what a body can perceive and name, with no state, phase or quantity behind them. PREREQUISITE DROPPED: built on `alchemy`, outside the corpus. |
| inv-barometer | Barometers — atmospheric pressure measured | absent |  |  |  | Downstream of `inv-gas`. Distinct from the discovery items in one respect worth recording: its demand is an INSTRUMENT, a made thing that yields a number a people then reasons with. Hornvale has climate fields with real values at every point and no way for anyone inside the world to read one. PREREQUISITE DROPPED: built on `falling-motion`, outside the corpus. |
| inv-air-pump | Air pumps — air evacuated from a vessel | absent |  |  |  | Downstream of `inv-barometer`. An apparatus item: its demand is the ability to produce a condition that does not occur naturally, which is the step from observing the world to experimenting on it. Nothing in the engine lets a people construct a situation. |
| inv-air-pressure | Early pneumatics — pressure as a motive force | absent |  |  |  | Downstream of `inv-air-pump`; a discovery item, see `inv-hydrostatics`. The catalogue's own chain makes this the hinge of the whole arc -- everything from Boyle to Watt is built on it -- which is why the corpus keeps it as its own item rather than folding it into its neighbours. |
| inv-boyles-law | Boyle's law — pressure volume relation known | absent |  |  |  | Downstream of `inv-air-pressure`; a discovery item, see `inv-hydrostatics`. Its specific demand -- a RELATION between two measured quantities, held as knowledge -- is the sharpest form of that item's argument: Hornvale's belief layer can carry a false claim about a place, and has no representation for a claim about how two quantities covary. |
| inv-pressure-cooker | Pressure cookers — pressure vessel held above ambient | absent |  |  |  | Downstream of two `absent` items. Its demand is containment -- a vessel that holds a condition against the world -- and the nearest thing the engine has is a community's stores, which are a scalar that decays. |
| inv-miners-friend | Miner's friend — engine raises water from a working | absent |  |  |  | Downstream of `inv-air-pump` (`absent`) and `inv-coal-mining` (`deferred`), so `absent` by weakest demand. The catalogue's chain is doing something Hornvale's model cannot: the engine exists BECAUSE the mine flooded, so a constraint on one capability calls a second into being. `occ-delve-depth` accrues metres per head per epoch with no obstacle term at all, so a Hornvale working has no depth at which it needs anything. |
| inv-gas-volume-temperature | Gas volume temperature — thermal expansion of gas known | absent |  |  |  | Downstream of `inv-air-pressure`; a discovery item, see `inv-hydrostatics`. Recorded separately because the catalogue does, and because its demand names temperature, which Hornvale models richly as a field (`hornvale_kernel::Temperature` crosses domain boundaries as a typed quantity) and not at all as something anyone in the world knows a fact about. |
| inv-newcomen-steam-engine | Newcomen steam engine — engine does sustained useful work | absent |  |  |  | Downstream of two `absent` items. PREREQUISITE DROPPED: built on `coke-iron`, outside the corpus. Its demand -- sustained useful work -- is the first item in the arc whose value is a RATE rather than a possibility, and the engine has exactly one rate of this shape, `DELVE_M_PER_PERSON_EPOCH`, whose own doc records that it was "CHOSEN FOR DYNAMIC RANGE, AND THAT IS AN INSTRUMENT DECISION RATHER THAN A RESULT ONE". A rate chosen to make a distribution legible is not a rate a people improves. |
| inv-latent-heat | Latent heat — latent heat known | absent |  |  | NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`.  Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list. | A root, so the verdict is chosen; a discovery item, see `inv-hydrostatics` for the general argument. PREREQUISITE DROPPED: built on `heat-capacity`, outside the corpus. Scored `absent` independently of its arc rather than inherited, which matters for the ratchet: it is one of ten ROOTS in this corpus -- the items whose verdict rests on its own evidence rather than on a prerequisite's. Swept against `BIO-8` and `TECH-2`: nothing. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): `LANG-49` refused for the reason given on `inv-hydrostatics`, and nothing else in 1,779 rows names a thermal quantity a people knows. |
| inv-steam-engine | Steam engine — engine efficient enough to spread | absent |  |  |  | Downstream of two `absent` items, and the item whose demand is closest to what this corpus is FOR. Its content is not that an engine works but that it works well enough to SPREAD -- a capability crossing a threshold and then diffusing across peoples, which is the shape `TECH-1` proposes ("a capability is crossed when biome resources x subsistence x surplus clear a bar") and `tech_for` does not implement. It is `absent` rather than `deferred` on `TECH-1` deliberately: `TECH-1` names the MECHANISM this corpus wants, not this item's capability, and anchoring 38 absent items to one generic row would turn this column into the backlog that decision 0095 and spec section 2 both forbid. |
| inv-improved-steam-engine | Improved steam engine — rotary power drives general machinery | absent |  |  |  | Downstream of `inv-steam-engine`, and the last item in the catalogue's chronological order here. Its demand is generality -- one capability that powers arbitrary others -- which is the furthest any item in this corpus stands from a four-rung clock, and the corpus ends on it because the catalogue does. |
