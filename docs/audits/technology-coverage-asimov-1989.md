<!-- GENERATED FILE — do not edit. Regenerate with `hornvale technologies report asimov-1989`. -->

# Technology coverage

## Provenance

- **Corpus:** `asimov-1989`
- **Source:** Isaac Asimov, *Asimov's Chronology of Science and Discovery* (Harper & Row,
1989), as presented by invention.cards, whose own subtitle states the
attribution verbatim: "A visual chronology of Asimov's ~1500 scientific
inventions and discoveries." Asimov is the catalogue; invention.cards is
where it was encountered. THE SELECTION RULE, STATED BEFORE THE COUNTS SO IT
IS APPLICABLE BY SOMEONE WHO HAS NEVER READ THIS CORPUS'S OWN CLOSURE CODE.
Take the union of the catalogue's three named story arcs (`knights`,
`republic-of-letters`, `steam-diffusion`) -- the predecessor corpus's whole
population -- UNION every item the catalogue attests before the year 1700.
Close that set under the catalogue's own "Built on" edges: whenever a
member's prerequisite is missing from the set, add it, and repeat until
nothing more is added. The result is the closed population this file scores.
1700 IS A JUDGEMENT CALL, NOT A DERIVATION. Nothing in the catalogue, the
spec or Hornvale's code names 1700 as a boundary; it is an authored decision
about how far into the world's plausible intellectual reach this corpus
should look, per spec section 4.4's licence for a corpus to set its own
scope deliberately rather than discover one. `< 1700` was chosen, and chosen
to be generous rather than conservative: it deliberately ADMITS Newton --
`laws-motion`, `universal-gravitation` and `shape-of-earth` are all attested
at 1687, inside the boundary -- so that this corpus can report a real
ceiling on what it finds rather than assume one by stopping short of the
Principia. The three arcs themselves are UNION'D IN REGARDLESS of the year
clause: seven of their members (`newcomen-steam-engine` 1712,
`mercury-thermometer` 1714, `coke-iron` 1709, `heat-capacity` 1760,
`steam-engine` 1764, `latent-heat` 1762, `improved-steam-engine` 1781)
postdate 1700 and are in this population only because they are arc members
or the closure of one, never because of the year clause. EVERY COUNT TASK 1
MEASURED, IN THE ORDER IT MEASURED THEM. Two independent enumeration fetches
of `https://invention.cards/browse/`, 2026-09-13 ~01:20 UTC, agreed
byte-for-byte at 150,326 bytes each (empty diff). Parsing that page with the
brief's own prescribed slug pattern (`[a-z0-9-]+`) yields 1,484 distinct
items, matching The Kiln's prior count exactly -- BUT THIS IS A FALSE
CONFIRMATION: the page actually holds 1,486 `<li>` entries, and the
ASCII-only slug class silently drops two real items, `2,4-d` (a comma in its
slug) and `mössbauer-effect` (a non-ASCII `ö`). Broadening the capture to
`[^"/]+` recovers both. NEITHER ITEM CHANGES ANYTHING DOWNSTREAM: both
postdate 1700, neither is in any of the three arcs, and neither slug is ever
a value in any closed-population item's `Built on` list -- verified directly
against the built-on graph. So this file states the catalogue at its correct
size, 1,486, while recording that a naive re-derivation using the brief's
own regex reproduces the old, wrong 1,484 and reads as confirmation rather
than as the shared blind spot it is. The three arcs: `knights` 16,
`republic-of-letters` 10, `steam-diffusion` 15, union 41 -- unchanged from
The Kiln's count, and this file's 41 arc items are the identical 41 items,
individually unchanged in `id`, `title`, `source`, `verdict`, `anchor`,
`statistic` and `criterion` (see the completion note below). Era clause
(attested before 1700, over the corrected 1,486-item catalogue): 294 items.
Seed (arc union UNION era clause): 298, matching The Kiln's prior 298
exactly -- the enumeration correction changes nothing here because both
recovered items postdate 1700. Closing the 298-item seed under `Built on`
adds exactly 3 items (`coke-iron`, `heat-capacity`, `mercury-thermometer`)
for a CLOSED POPULATION OF 301 ITEMS AND 401 EDGES. Zero cycles. One root:
`biped`, attested 4,000,000 BCE, the sole item in the whole 301-item
population with an empty `Built on` list. The catalogue's own "Led to" edges
agree with "Built on" completely: 401 built-on edges, 401 inverted led-to
edges, zero asymmetries in either direction. THIS CORPUS SUPERSEDES THE
KILN'S 41-ITEM ARC-ONLY PREDECESSOR, AND THE DEFECT IT REPAIRS IS SPECIFIC.
Decision 0386 requires `presupposes` to name only items inside the corpus,
so the predecessor -- population 41, no closure step -- dropped every
prerequisite pointing outside its own 41 items: 15 distinct items
(`alchemy`, `artillery`, `biped`, `coke-iron`, `copper`, `crossbow`,
`falling-motion`, `fire`, `geometry`, `heat-capacity`, `nation`, `plow`,
`steel`, `stone-tool`, `university`) named as a "Built on" target by some
arc item but never added, filtered out at the edge rather than the item. The
predecessor's own `provenance` said so honestly ("`presupposes` NAMES ITEMS
IN THIS CORPUS AND NOTHING ELSE, WHICH DROPS REAL PREREQUISITES ON PURPOSE")
and drew the correct conclusion from it ("the derived demand set
UNDER-DESCRIBES every such item's real prerequisites") -- but a corpus whose
own founding document states that its central computed quantity (the
transitive closure decision 0386 defines) is known to under-describe most of
its items cannot support the claims that quantity is meant to carry. This
file closes the population instead of filtering the edges, so every `Built
on` target the catalogue draws for any of these 301 items is a real item
scored in this file, and the derived demand set is exact rather than a
known-partial approximation. THE 41 ARC ITEMS REMAIN INDIVIDUALLY
IDENTIFIABLE: every one keeps its original `id` and its `source` field's
`arc '<name>'` suffix unchanged, so a reader can always recover exactly
which items are the predecessor's whole population versus which 260 The
Cadastre added to close it. DECLARED BIAS -- AN INSTRUMENT WITH KNOWN BIAS,
NEVER A STANDARD (decision 0095), RE-COUNTED AT THE GRAIN THE AVAILABLE DATA
SUPPORTS. The predecessor's geographic count (37 of 41 items attributed a
place; 26 Europe, 5 Egypt/the Near East, 6 elsewhere, 0 in the Americas,
sub-Saharan Africa or Oceania) is UNCHANGED and still exactly true, because
the 41 arc items and their `source` fields are byte-identical to the
predecessor's. IT IS NOT RE-COUNTED OVER ALL 301, AND THAT LIMIT IS STATED
RATHER THAN PAPERED OVER: Task 1's interface
(`/tmp/cadastre/population.json`) carries `{slug, title, year, field,
built_on, led_to}` for every item and never scraped attributed person or
place for the 260 new items -- only the original 41 (authored individually,
by hand, by The Kiln) carry that detail in their `source` strings. The
cached per-item pages Task 1 fetched (`/tmp/cadastre/pages/*.html`) do not
repair this: the page's own attribution fields render client-side from a
data payload absent from the static HTML, so no geographic recount over the
full 301 is possible from what this campaign fetched, and re-scraping 260
more pages for it is out of Task 2's scope. What CAN be re-counted from
`population.json` is the catalogue's own topical tag, `field`, across all
301: Science 95, General 47, Geography 36, Space 36, Culture 31, Math 29,
War 18, Design 9. This says nothing about geography, but it is worth stating
plainly what it does say: the closure pulls in a wide swath of `General`,
`Space` and `Math` items (numbers, geometry, logic, astronomy) that the
three arcs' narrow military/literacy/pneumatics focus never touched, so the
closed population's topical spread is broader than its predecessor's even
though its geographic bias (measurable only on the original 41) is
unchanged. ASIMOV IS NOT OWED A WORLD. Coverage here measures reach against
THIS catalogue only. DEMANDS ARE DERIVED, NEVER WRITTEN (decision 0386).
Each item names the ONE demand it `introduces` and the items it
`presupposes`; the demand set is the transitive closure over `presupposes`,
computed on read. No demand list appears anywhere in this file. Unlike the
predecessor, `presupposes` here is never filtered to a subset of the
corpus's own items -- the closure guarantees every "Built on" target is
itself a scored item, verified directly (Task 2, Step 6): zero items whose
authored `presupposes` disagrees with the catalogue's own "Built on" list.
ORDERING. `ordered` is true: the 301 items are arranged so every
`presupposes` edge points backwards in the file, computed by a topological
sort (Kahn's algorithm) that breaks ties toward the catalogue's own
attested-date order wherever the lattice allows it. Four pairs in the
underlying population share an attested year with a direct dependency
between them (`florida`/`pacific-ocean`,
`shape-of-earth`/`universal-gravitation`, `standing-army`/`arquebus`,
`universal-gravitation`/`laws-motion`, all 1513 or 1687); in every one the
prerequisite is placed first, which the topological sort guarantees
mechanically rather than by manual inspection. THE REACH QUESTION, STATED
EXACTLY, BECAUSE EVERY AUTHORED VERDICT BELOW TURNS ON IT. Can a Hornvale
world TODAY carry the capability this item introduces as a committed fact
that CAN DIFFER BETWEEN TWO PEOPLES? The second half is the load-bearing
half. A label every community carries by construction is not a capability a
people acquires, and a label that conflates the demand with a neighbouring
one -- so that no world can hold the one without the other -- does not meet
the demand either. VERDICT TALLY, AND WHAT IT DOES NOT YET MEAN. 295
`absent`, 6 `deferred`, 0 `present`, 0 `refused`, 0 `inapplicable`, 0
measured. THE 295 IS NOT ONE POPULATION: 35 of those `absent` verdicts are
the predecessor's own, individually authored and searched (unchanged by this
task); the remaining 260 are TASK 2 PLACEHOLDERS, authored with no
repository search at all, exactly as this campaign's plan requires ("An
`absent` here is a placeholder that Task 3 must confirm or replace") -- so a
report reader must not read the 295 figure as 295 searched findings. The 6
`deferred` are the predecessor's, unchanged. `present` remains the verdict
this instrument is least entitled to and it earns none here. THE NOVELTY
RATCHET'S BASELINE IS RE-MEASURED, NOT CARRIED FORWARD, AND THE
RE-MEASUREMENT IS MOSTLY PLACEHOLDER MASS. `cli/src/technologies.rs`'s
`novelty_baseline("asimov-1989")` moves from the predecessor's 35 to 295 --
the true `absent` count of the file as committed here -- because the ratchet
compares against whatever this corpus actually says, and a stale 35 would
fire as a false "improvement" the moment Task 3 confirms even one of the 260
placeholders. A successor reading a future drop in the `absent` count
against this 295 baseline should expect most of that drop to be Task 3 doing
its job, not a discovery. NON-BLINDNESS, AT TWO LEVELS. CORPUS LEVEL: Task
2's session read `technologies/CLAUDE.md`, `cli/src/technologies.rs`
(`is_chosen`, `disclosure_gaps`, `derived_demands`), and Task 1's report,
and used that reading only to (a) construct the closure mechanically from
`population.json` and (b) satisfy the disclosure obligation the resolver's
own chosen/inherited rule imposes -- it performed NO repository search for
any of the 260 new items' verdicts, which is exactly why every one of them
is scored `absent` as a placeholder rather than searched. ITEM LEVEL:
`disclosure` marks PER-ITEM non-blindness. Under this closure, exactly ONE
item in the entire 301-item population is CHOSEN -- `inv-biped`, the sole
root -- because closing the population restores a real, `absent` ancestor
(ultimately `inv-biped` itself, since it is the population's only root and
every other item's closure must terminate there) into every other item's
`presupposes` closure. **This is the whole population, not a proxy**: the
family's own rule computes "chosen" from whether any closure prerequisite is
`absent`, never from whether an item is a root, and family law
(`technologies/CLAUDE.md`) explicitly warns against relying on the two
coinciding. At this population's current verdict assignment they DO coincide
exactly -- the chosen set `{inv-biped}` equals the root set `{inv-biped}` --
and that is recorded as a fact about this snapshot, not as license to check
roots instead of running `is_chosen`: the moment Task 3 moves even one item
off `absent` inside some other item's closure, the two sets can diverge
again exactly as `technologies/CLAUDE.md`'s own worked example
(`inv-parchment`) describes. `inv-biped` therefore carries a `disclosure`
stating plainly that its `absent` verdict is an unauthored Task 2
placeholder, not a search result -- the honest content of a disclosure that
the resolver requires regardless of how thin the underlying reasoning is.
SIXTEEN ITEMS LOST THEIR DISCLOSURE IN THIS TASK, AND NONE GAINED ONE
BESIDES `inv-biped`. Under the predecessor's 41-item population, sixteen
items were CHOSEN, recomputed directly from `is_chosen` over the
predecessor's own commit (`e3755b45c`) rather than taken from either this
file's own prior claim or from any reviewer's count: TEN ROOTS
(`inv-animal-dom`, `inv-cart`, `inv-writing` in the knights and
republic-of-letters arcs, plus `inv-basic-steam-engine`, `inv-turnplow`,
`inv-coal-mining`, `inv-longbow`, `inv-arquebus`, `inv-hydrostatics` and
`inv-latent-heat` in the steam and knights arcs) AND SIX NON-ROOT ITEMS
whose own prerequisite chain held nothing `absent` (`inv-papyrus`,
`inv-literature`, `inv-horse`, `inv-alphabet`, `inv-library` and
`inv-parchment` — each one's sole prerequisite, `inv-writing`,
`inv-animal-dom` or `inv-literature`, was itself `deferred` rather than
`absent`, the exact shape family law's own `inv-parchment` example warns a
root-keyed check would miss). Closure restores each of their real,
previously-dropped prerequisites -- named individually in each item's own
`note`, which is where the deleted `disclosure` text was moved verbatim for
the historical record -- and every one of those restored prerequisites is
itself `absent` (whether directly, like `inv-biped` under `inv-animal-dom`,
or through a longer inherited chain, like the six-rung descent under
`inv-latent-heat`). So all sixteen are now INHERITED: nothing upstream
forces their verdicts and the weakest-demand rule reads each one off an
ancestor no Hornvale fact decided. Their `verdict`, `anchor`, `statistic`
and `criterion` are untouched; only `presupposes`, `note` and `disclosure`
moved, exactly as this task's plan requires. REGISTRY RULINGS ON THE ARC
ITEMS ARE UNCHANGED IN SUBSTANCE. The predecessor's registry sweep (`BIO-8`
discharging `inv-turnplow`'s Boserup-plough demand; `TECH-2` refused for
every metal item whose metal is a dropped-then-restored prerequisite rather
than its own demand; `MAP-8` discharging `inv-writing`; `DOM-aesthetics`
discharging `inv-literature`; `MEM-4` union `SOC-11` discharging
`inv-library`; the ceiling ruling that scores everything past
`TechHorizon::Classical` `absent` rather than `inapplicable` or `refused`,
because no decision record ratifies the four-rung horizon) is preserved
verbatim in each item's own `anchor` and `note`, since Step 2 of this task's
plan requires the 41 arc items' `verdict`, `anchor`, `statistic` and
`criterion` to be preserved exactly. The cross-corpus ruling on `MAP-18`
(discharges `henrich-2004-extended`'s `col-long-count`, discharges nothing
here because none of the 41 arc items names a calendrical or astronomical
capability) likewise stands unchanged; whether any of the 260 new items
reopens that ruling is Task 3's question, not this one's. THE CRITERION AND
STATISTIC APPLY ONLY TO THE 41 ARC ITEMS TODAY. `statistic:
fraction-of-peoples-holding` and `criterion: {fraction-in-band, lo 0.05, hi
0.95}` are unchanged on every arc item that already carried them; none of
the 260 new items carries a `statistic` or `criterion` at all, per this
task's own plan ("No disclosure, no note, no statistic, no criterion at this
step"). The statistic's own definition -- numerator peoples holding the
capability, denominator peoples with at least one community alive at the
evaluation instant; aggregation by ANY live community (never ALL, LATEST or
AT-CLOSURE, for the reasons the predecessor's provenance worked out in
full); evaluation at `BakeConfig::end_year`; both poles (0.0 and 1.0)
falsifying; the band's stated meaning holding only while the relevant
peoples count stays at or below 20 -- is untouched, because Task 2 authors
no new criterion and re-freezing an already-frozen band is not something a
later session may do (`technologies/CLAUDE.md`). THE SINGLE `verdict` FIELD
IS A PIPELINE. A measured value (`grown`/`flat`/`lost`) is reachable only if
reach already succeeded, so this corpus carries ZERO `grown`, ZERO `flat`,
ZERO `lost` and ZERO `unmeasured` -- no item's reach succeeds yet. WHY THE
`absent` MASS IS STRUCTURAL RATHER THAN A SCORE. Hornvale's concept registry
grows on demand, and everyday material technique -- a cart, a plough, a
written record, and now (via closure) a domesticated draft animal, a smelted
metal, a unit of standing force -- has had no consumer in this engine, so no
predicate was ever minted for most of it. THE CORPUS IS AN INSTRUMENT, NEVER
A ROADMAP (decision 0095): a 295-item `absent` column is a statement about
the intersection of two rosters, not a deficiency score, and 260 of those
295 are additionally placeholders this task did not search.
- **Frozen:** Before first measurement, The Cadastre (Task 2, 2026-09-12), rebuilding The
Kiln's `asimov-1989` at its closed 301-item population. UNLIKE THE
PREDECESSOR, THIS FILE'S `frozen` CANNOT CLAIM NO EVALUATION CODE EXISTS,
BECAUSE IT DOES: `cli/src/technologies.rs` (the loader, `derived_demands`,
`is_chosen`, `disclosure_gaps`, `meets`, `audit`, `audit_family`,
`novelty_baseline`, `render`), `cli/tests/suite/technology_corpus.rs` (the
item-count freeze and lattice tests) and
`docs/audits/technology-coverage-asimov-1989.md` (the committed,
drift-checked report) all exist in this repository today and were exercised
against this very file before it was committed (`hornvale technologies check
asimov-1989` exits 0). What is frozen here is narrower than "no code exists"
and is stated exactly rather than borrowing the predecessor's sentence:
SELECTION is structural — the two-clause rule (three arcs union
era-before-1700, closed under Built on) admits everything it reaches and
nothing else, so it cannot be tuned item-by-item to produce a nicer
population, and Step 6's verification (zero items whose `presupposes`
disagrees with the catalogue's own "Built on" list) is a check against that
structural rule rather than against a human's transcription. VERDICTS are
non-blind and it matters which of two very different kinds: the 41 arc
items' verdicts are the predecessor's own individually-searched,
disclosed-where-chosen judgements, untouched by this task; the 260 new
items' verdicts are UNSEARCHED PLACEHOLDERS, uniformly `absent`, that Task 3
owes a real search before anyone reads them as findings. `inv-biped` carries
the one `disclosure` this task authors, and it says exactly that: a
placeholder, not a search result. THE ITEM COUNT IS 301, AND
`cli/tests/suite/technology_corpus.rs`'s
`the_asimov_corpus_is_frozen_at_its_declared_size` ASSERTS IT, so that
changing this corpus again is a deliberate act (decision 0016). The NOVELTY
ratchet's baseline (`cli/src/technologies.rs::novelty_baseline`) is
re-measured at 295 for the same reason. RE-FREEZING IS NOT SOMETHING A LATER
SESSION MAY DO, and the disqualification carried forward from the
predecessor is UNCHANGED for the 41 arc items' criterion: any session that
has read `occ-tech`'s published distribution (spec finding F4, the committed
census metrics, the almanac, `windows/lot`'s prose) may not tighten their
`[0.05, 0.95]` band. IT DOES NOT YET APPLY TO THE 260 NEW ITEMS, because
none of them has a criterion to disqualify tightening on — that
disqualification begins the moment Task 3 authors one, not before. THE
DISQUALIFICATION FOR THIS TASK'S OWN WORK IS NARROWER AND WORTH STATING
SEPARATELY: this session read `population.json`, the cached catalogue pages
under `/tmp/cadastre/pages/`, and the resolver's disclosure and closure
logic, in order to build the closure and satisfy the chosen/inherited rule
mechanically — a structural operation, not a judgement about which
technologies Hornvale should have. It did not read `occ-tech`'s distribution
while doing so, and did not author or move any item's `verdict`, `statistic`
or `criterion` beyond the placeholder `absent` this task's own plan
prescribes.

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

The eight coverage verdicts below are percentages of 301 — every item MINUS
the ones still `unmeasured` (see the next section). An `unmeasured` item has
not been judged, so counting it here would move a coverage percentage for a
reason unrelated to what that percentage claims to measure.

- present: 0 (0%)
- refused: 0 (0%)
- deferred: 6 (2%)
- absent: 295 (98%)
- inapplicable: 0 (0%)
- grown: 0 (0%)
- flat: 0 (0%)
- lost: 0 (0%)
- **coverage total:** 301

## Unmeasured

None — every item carries a coverage verdict.

THE FINDING THIS CORPUS MAKES SAYABLE: of the 301 item(s) here, Hornvale's
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
| inv-biped | Bipedal species | absent | biped |
| inv-stone-tool | Stone tools | absent | biped, stone-tool |
| inv-fire | Fire tamed | absent | biped, fire |
| inv-ceremonial-burial | Ceremonial burial | absent | biped, ceremonial-burial |
| inv-art | Art | absent | art, biped, ceremonial-burial |
| inv-bow | Bows and arrows | absent | biped, bow, stone-tool |
| inv-oil-lamp | Oil lamps | absent | biped, fire, oil-lamp |
| inv-animal-dom | Animals domesticated — domestic animals kept | deferred | biped, domestic-animals-kept |
| inv-agriculture | Plants domesticated | absent | agriculture, biped, domestic-animals-kept |
| inv-pottery | Pottery | absent | biped, pottery |
| inv-fish-nets | Linen | absent | biped, fish-nets, stone-tool |
| inv-raft | Rafts | absent | biped, raft, stone-tool |
| inv-sickle | Sickles | absent | agriculture, biped, domestic-animals-kept, sickle, stone-tool |
| inv-irrigation | Irrigation | absent | agriculture, biped, domestic-animals-kept, irrigation |
| inv-scale | Weight scales | absent | biped, scale, stone-tool |
| inv-copper | Copper | absent | biped, copper, fire, stone-tool |
| inv-simple-sundial | Simple sundials | absent | biped, simple-sundial, stone-tool |
| inv-bronze | Bronze | absent | biped, bronze, copper, fire, stone-tool |
| inv-cart | Wheeled carts — wheeled land haulage | absent | biped, copper, fire, stone-tool, wheeled-land-haulage |
| inv-plow | Plows | absent | agriculture, biped, domestic-animals-kept, plow |
| inv-river-boat | River boats | absent | biped, raft, river-boat, stone-tool |
| inv-writing | Writing — written record kept | deferred | biped, stone-tool, written-record-kept |
| inv-nation | Nations | absent | agriculture, biped, domestic-animals-kept, nation |
| inv-candle | Candles | absent | biped, candle, domestic-animals-kept, fire, oil-lamp |
| inv-papyrus | Papyrus — plant fibre writing surface | absent | agriculture, biped, domestic-animals-kept, nation, plant-fibre-writing-surface, stone-tool, written-record-kept |
| inv-calendar | Calendar | absent | biped, calendar, simple-sundial, stone-tool, written-record-kept |
| inv-stone-monument | Stone monuments | absent | agriculture, biped, ceremonial-burial, domestic-animals-kept, nation, stone-monument |
| inv-glass | Glass | absent | biped, fire, glass |
| inv-literature | Literature — composed work transmitted as text | deferred | biped, composed-work-transmitted-as-text, stone-tool, written-record-kept |
| inv-empire | Empires | absent | agriculture, biped, domestic-animals-kept, empire, nation |
| inv-horse | Horses — riding animal bred | absent | biped, domestic-animals-kept, riding-animal-bred |
| inv-fermentation | Fermentation | absent | agriculture, biped, domestic-animals-kept, fermentation |
| inv-numbers | Number system | absent | biped, numbers, stone-tool, written-record-kept |
| inv-law | Code of Laws | absent | agriculture, biped, domestic-animals-kept, empire, law, nation, stone-tool, written-record-kept |
| inv-medicine | Recorded medicine | absent | biped, ceremonial-burial, medicine, stone-tool, written-record-kept |
| inv-alphabet | Phonetic alphabet — phonemic script | absent | biped, phonemic-script, stone-tool, written-record-kept |
| inv-bridle | Bridle — animal guided by rein | absent | animal-guided-by-rein, biped, domestic-animals-kept, riding-animal-bred |
| inv-monotheism | Monotheism | absent | biped, composed-work-transmitted-as-text, monotheism, stone-tool, written-record-kept |
| inv-dye | Resistant dyes | absent | art, biped, ceremonial-burial, dye |
| inv-sea-navigation | Sea navigation | absent | biped, raft, river-boat, sea-navigation, stone-tool |
| inv-steel | Steel | absent | biped, bronze, copper, fire, steel, stone-tool |
| inv-arch | Architectural arches | absent | agriculture, arch, biped, ceremonial-burial, domestic-animals-kept, nation, stone-monument |
| inv-aqueduct | Aqueducts | absent | agriculture, aqueduct, biped, ceremonial-burial, domestic-animals-kept, irrigation, nation, stone-monument |
| inv-saddle | Saddle — seated riding rig | absent | animal-guided-by-rein, biped, domestic-animals-kept, riding-animal-bred, seated-riding-rig |
| inv-sundial | Improved sundials | absent | biped, simple-sundial, stone-tool, sundial |
| inv-zoo | Zoos | absent | biped, domestic-animals-kept, zoo |
| inv-coin | Currency (Coins) | absent | agriculture, biped, coin, domestic-animals-kept, nation, scale, stone-tool, written-record-kept |
| inv-library | Libraries — collected holdings outliving their keepers | deferred | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, nation, stone-tool, written-record-kept |
| inv-eclipse | Solar eclipse predicted | absent | biped, calendar, eclipse, numbers, simple-sundial, stone-tool, written-record-kept |
| inv-water-element | Water as element | absent | biped, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-irrational-numbers | Irrational numbers | absent | biped, irrational-numbers, numbers, stone-tool, written-record-kept |
| inv-realistic-maps | Realistic maps | absent | agriculture, biped, domestic-animals-kept, empire, nation, realistic-maps, stone-tool, written-record-kept |
| inv-abacus | Abacus | absent | abacus, biped, numbers, stone-tool, written-record-kept |
| inv-cadaver | Human dissection | absent | biped, cadaver, ceremonial-burial, medicine, stone-tool, written-record-kept |
| inv-ocean-navigation | Ocean navigation | absent | biped, bronze, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-venus-named | Venus named | absent | biped, stone-tool, venus-named, written-record-kept |
| inv-dream-interpretation | Dream interpretation | absent | biped, composed-work-transmitted-as-text, dream-interpretation, stone-tool, written-record-kept |
| inv-atom | Atoms | absent | atom, biped, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-epilepsy | Epilepsy | absent | biped, ceremonial-burial, epilepsy, medicine, stone-tool, written-record-kept |
| inv-catapult | Catapult | absent | biped, bow, catapult, numbers, stone-tool, written-record-kept |
| inv-university | Advanced schools | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, nation, stone-tool, university, written-record-kept |
| inv-animal-classification | Animal classification | absent | animal-classification, biped, domestic-animals-kept |
| inv-five-elements | Five elements theorized | absent | atom, biped, five-elements, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-heliocentric-theory | Non-geocentric theory | absent | biped, heliocentric-theory, stone-tool, written-record-kept |
| inv-logic | Logic | absent | biped, logic, numbers, stone-tool, written-record-kept |
| inv-spherical-earth | Spherical earth theory | absent | biped, bronze, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, spherical-earth, stone-tool |
| inv-star-maps | Star maps | absent | agriculture, biped, domestic-animals-kept, empire, nation, realistic-maps, star-maps, stone-tool, written-record-kept |
| inv-botany | Botany book | absent | agriculture, biped, botany, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, nation, stone-tool, university, written-record-kept |
| inv-paved-road | Paved roads | absent | agriculture, biped, ceremonial-burial, copper, domestic-animals-kept, fire, nation, paved-road, stone-monument, stone-tool, wheeled-land-haulage |
| inv-arteries-veins | Arteries vs veins | absent | arteries-veins, biped, cadaver, ceremonial-burial, medicine, stone-tool, written-record-kept |
| inv-geometry | Geometry | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-tides | Tides | absent | biped, bronze, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, stone-tool, tides |
| inv-brain-areas | Parts of brain | absent | biped, brain-areas, cadaver, ceremonial-burial, medicine, stone-tool, written-record-kept |
| inv-lighthouse | Lighthouses | absent | agriculture, biped, bronze, ceremonial-burial, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, domestic-animals-kept, fire, geometry, lighthouse, logic, nation, numbers, ocean-navigation, raft, river-boat, sea-navigation, stone-monument, stone-tool, university, written-record-kept |
| inv-moon-sun-size | Moon and sun size estimate | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, heliocentric-theory, logic, moon-sun-size, nation, numbers, stone-tool, university, written-record-kept |
| inv-water-clock | Water clocks | absent | agriculture, biped, domestic-animals-kept, nation, simple-sundial, stone-tool, sundial, water-clock |
| inv-lever-math | Lever mathematics | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, lever-math, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-earth-size | Earth size estimate | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, earth-size, geometry, logic, nation, numbers, raft, river-boat, stone-tool, university, written-record-kept |
| inv-year-number | Standardized years | absent | biped, calendar, simple-sundial, stone-tool, written-record-kept, year-number |
| inv-great-wall | Great Wall | absent | agriculture, biped, ceremonial-burial, domestic-animals-kept, great-wall, nation, stone-monument |
| inv-parchment | Parchment — hide writing surface | absent | biped, domestic-animals-kept, hide-writing-surface |
| inv-moon-distance | Distance to moon estimate | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, moon-distance, nation, numbers, raft, river-boat, stone-tool, university, written-record-kept |
| inv-star-maps-better | Better star maps | absent | agriculture, biped, domestic-animals-kept, empire, nation, realistic-maps, star-maps, star-maps-better, stone-tool, written-record-kept |
| inv-glass-blowing | Glass blowing | absent | biped, fire, glass, glass-blowing |
| inv-wooden-stirrup | Wooden stirrups — foot support in the saddle | absent | animal-guided-by-rein, biped, domestic-animals-kept, foot-support-in-the-saddle, riding-animal-bred, seated-riding-rig |
| inv-water-wheel | Waterwheels | absent | biped, copper, fire, stone-tool, water-wheel, wheeled-land-haulage |
| inv-julian-calendar | Julian calendar | absent | biped, calendar, julian-calendar, simple-sundial, stone-tool, written-record-kept, year-number |
| inv-climactic-zone | Climactic zones | absent | agriculture, biped, bronze, climactic-zone, copper, domestic-animals-kept, empire, fire, nation, ocean-navigation, raft, realistic-maps, river-boat, sea-navigation, spherical-earth, stone-tool, written-record-kept |
| inv-basic-steam-engine | Rudimentary steam motion — steam moves a mechanism | absent | biped, fire, steam-moves-a-mechanism |
| inv-medicinal-plants | Recorded medicinal plants | absent | biped, ceremonial-burial, medicinal-plants, medicine, stone-tool, written-record-kept |
| inv-paper | Paper — cheap pulped writing surface | absent | biped, cheap-pulped-writing-surface, domestic-animals-kept, hide-writing-surface |
| inv-geocentric-universe | Geocentric universe | absent | agriculture, biped, domestic-animals-kept, empire, geocentric-universe, nation, realistic-maps, star-maps, star-maps-better, stone-tool, written-record-kept |
| inv-spinal-cord | Spinal cord | absent | biped, ceremonial-burial, medicine, spinal-cord, stone-tool, written-record-kept |
| inv-algebra | Algebra | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-tea | Tea | absent | agriculture, biped, domestic-animals-kept, tea |
| inv-alchemy | Recorded alchemy | absent | alchemy, atom, biped, five-elements, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-metal-stirrup | Metal stirrups — load bearing metal stirrup | absent | biped, bronze, copper, domestic-animals-kept, fire, load-bearing-metal-stirrup, riding-animal-bred, steel, stone-tool |
| inv-wheelbarrow | Wheelbarrows | absent | biped, bronze, copper, fire, steel, stone-tool, wheelbarrow |
| inv-dome | Architectural domes | absent | agriculture, arch, biped, ceremonial-burial, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, dome, domestic-animals-kept, geometry, logic, nation, numbers, stone-monument, stone-tool, university, written-record-kept |
| inv-silk-europe | Silk | absent | agriculture, biped, ceremonial-burial, copper, domestic-animals-kept, fire, nation, paved-road, silk-europe, stone-monument, stone-tool, wheeled-land-haulage |
| inv-turnplow | Turnplows — traction tillage of heavy soil | deferred | agriculture, biped, bronze, copper, domestic-animals-kept, fire, plow, steel, stone-tool, traction-tillage-of-heavy-soil |
| inv-greek-fire | Greek fire | absent | alchemy, atom, biped, bow, catapult, five-elements, greek-fire, numbers, raft, river-boat, sea-navigation, stone-tool, water-element, written-record-kept |
| inv-block-printing | Block printing — text reproduced from a carved form | absent | biped, cheap-pulped-writing-surface, domestic-animals-kept, hide-writing-surface, text-reproduced-from-a-carved-form |
| inv-porcelain | Porcelain | absent | biped, porcelain, pottery |
| inv-acetic-acid | Acetic acid | absent | acetic-acid, agriculture, biped, domestic-animals-kept, fermentation |
| inv-high-backed-saddle | High-backed saddle — braced saddle transmits shock | absent | animal-guided-by-rein, biped, braced-saddle-transmits-shock, bronze, copper, domestic-animals-kept, fire, load-bearing-metal-stirrup, riding-animal-bred, seated-riding-rig, steel, stone-tool |
| inv-iron-horseshoes | Iron horseshoes — shod draught animal | absent | biped, bronze, copper, domestic-animals-kept, fire, riding-animal-bred, shod-draught-animal, steel, stone-tool |
| inv-couched-lance | Couched lance — mounted shock charge | absent | animal-guided-by-rein, biped, braced-saddle-transmits-shock, bronze, copper, domestic-animals-kept, fire, load-bearing-metal-stirrup, mounted-shock-charge, riding-animal-bred, seated-riding-rig, steel, stone-tool |
| inv-zero | Zero | absent | abacus, agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, stone-tool, university, written-record-kept, zero |
| inv-iceland-settled | Iceland settled | absent | biped, bronze, copper, fire, iceland-settled, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-coffee | Coffee | absent | agriculture, biped, coffee, domestic-animals-kept |
| inv-arctic-circle | Arctic circle | absent | arctic-circle, biped, bronze, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-horse-collar | Horse collars — harness transmits animal draught | absent | agriculture, biped, bronze, copper, domestic-animals-kept, fire, harness-transmits-animal-draught, plow, riding-animal-bred, shod-draught-animal, steel, stone-tool, traction-tillage-of-heavy-soil |
| inv-greenland-viking | Greenland | absent | biped, bronze, copper, fire, greenland-viking, iceland-settled, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-newfoundland | Newfoundland discovered | absent | biped, bronze, copper, fire, greenland-viking, iceland-settled, newfoundland, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-optics | Optics | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-crossbow | Crossbows | absent | agriculture, biped, bow, bronze, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, crossbow, domestic-animals-kept, fire, lever-math, logic, nation, numbers, steel, stone-tool, university, written-record-kept |
| inv-new-star | New star | absent | biped, new-star, stone-tool, written-record-kept |
| inv-bright-comet | Bright comets | absent | biped, bright-comet, stone-tool, written-record-kept |
| inv-fork | Forks | absent | agriculture, biped, ceremonial-burial, copper, domestic-animals-kept, fire, fork, nation, paved-road, stone-monument, stone-tool, wheeled-land-haulage |
| inv-flying-buttress | Flying buttresses | absent | agriculture, arch, biped, ceremonial-burial, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, dome, domestic-animals-kept, flying-buttress, geometry, logic, nation, numbers, stone-monument, stone-tool, university, written-record-kept |
| inv-compass | Magnetic navigation | absent | biped, bronze, compass, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-windmill | Windmills | absent | biped, copper, fire, stone-tool, water-wheel, wheeled-land-haulage, windmill |
| inv-spitsbergen | Spitsbergen | absent | biped, bronze, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, spitsbergen, stone-tool |
| inv-arab-numbers | Arabic numerals | absent | abacus, agriculture, algebra, arab-numbers, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, raft, river-boat, sea-navigation, stone-tool, university, written-record-kept, zero |
| inv-coal-mining | Coal mining — mineral fuel extracted | deferred | biped, bronze, copper, fire, mineral-fuel-extracted, steel, stone-tool |
| inv-rudder | Rudders | absent | biped, bronze, copper, fire, ocean-navigation, raft, river-boat, rudder, sea-navigation, stone-tool |
| inv-eyeglass | Eyeglasses invented | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-gunpowder | Gunpowder | absent | biped, cheap-pulped-writing-surface, domestic-animals-kept, fire, gunpowder, hide-writing-surface |
| inv-planetary-tables | Planetary tables | absent | agriculture, biped, domestic-animals-kept, empire, geocentric-universe, nation, planetary-tables, realistic-maps, star-maps, star-maps-better, stone-tool, written-record-kept |
| inv-magnetic-pole | Magnetic poles | absent | biped, bronze, compass, copper, fire, magnetic-pole, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-mirror | Mirrors | absent | biped, bronze, copper, fire, glass, glass-blowing, mirror, steel, stone-tool |
| inv-far-east | Far east | absent | agriculture, biped, ceremonial-burial, copper, domestic-animals-kept, far-east, fire, nation, paved-road, stone-monument, stone-tool, wheeled-land-haulage |
| inv-longbow | Longbows — massed missile volley | absent | agriculture, biped, bow, bronze, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, crossbow, domestic-animals-kept, fire, lever-math, logic, massed-missile-volley, nation, numbers, steel, stone-tool, university, written-record-kept |
| inv-spinning-wheel | Spinning wheels | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, lever-math, logic, nation, numbers, spinning-wheel, stone-tool, university, written-record-kept |
| inv-distillation | Liquor distillation | absent | agriculture, alchemy, atom, biped, distillation, domestic-animals-kept, fermentation, five-elements, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-pike | Pike — massed polearm formation | absent | animal-guided-by-rein, biped, braced-saddle-transmits-shock, bronze, copper, domestic-animals-kept, fire, load-bearing-metal-stirrup, massed-polearm-formation, mounted-shock-charge, riding-animal-bred, seated-riding-rig, steel, stone-tool |
| inv-sulfuric-acid | Sulfuric acid | absent | acetic-acid, agriculture, biped, domestic-animals-kept, fermentation, sulfuric-acid |
| inv-comet-painting | Realistic comets | absent | art, biped, bright-comet, ceremonial-burial, comet-painting, stone-tool, written-record-kept |
| inv-canary-islands | Canary islands | absent | biped, bronze, canary-islands, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-anatomy | Anatomy book | absent | anatomy, biped, cadaver, ceremonial-burial, medicine, stone-tool, written-record-kept |
| inv-mechanical-clock | Mechanical clocks | absent | agriculture, biped, domestic-animals-kept, mechanical-clock, nation, simple-sundial, stone-tool, sundial, water-clock |
| inv-cannon | Cannons | absent | biped, bow, cannon, catapult, cheap-pulped-writing-surface, domestic-animals-kept, fire, gunpowder, hide-writing-surface, numbers, stone-tool, written-record-kept |
| inv-quarantine | Quarantine | absent | agriculture, biped, ceremonial-burial, domestic-animals-kept, empire, medicine, nation, quarantine, stone-tool, written-record-kept |
| inv-indian-ocean | Indian ocean | absent | biped, bronze, copper, fire, indian-ocean, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-madeira | Madeira settled | absent | biped, bronze, compass, copper, fire, madeira, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-azores | Azores | absent | azores, biped, bronze, compass, copper, fire, madeira, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-perspective | Perspective drawing | absent | agriculture, biped, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, fire, geometry, glass, glass-blowing, hide-writing-surface, logic, nation, numbers, optics, perspective, stone-tool, university, written-record-kept |
| inv-artillery | Artillery | absent | artillery, biped, bow, cannon, catapult, cheap-pulped-writing-surface, domestic-animals-kept, fire, gunpowder, hide-writing-surface, numbers, stone-tool, written-record-kept |
| inv-arquebus | Arquebus — handheld firearm | absent | agriculture, artillery, biped, bow, bronze, cannon, catapult, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, crossbow, domestic-animals-kept, fire, gunpowder, handheld-firearm, hide-writing-surface, lever-math, logic, nation, numbers, steel, stone-tool, university, written-record-kept |
| inv-standing-army | Standing army — permanent force apart from population | absent | agriculture, animal-guided-by-rein, artillery, biped, bow, braced-saddle-transmits-shock, bronze, cannon, catapult, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, crossbow, domestic-animals-kept, fire, gunpowder, handheld-firearm, hide-writing-surface, lever-math, load-bearing-metal-stirrup, logic, massed-polearm-formation, mounted-shock-charge, nation, numbers, permanent-force-apart-from-population, riding-animal-bred, seated-riding-rig, steel, stone-tool, university, written-record-kept |
| inv-concave-lenses | Concave lenses | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, concave-lenses, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-printing-press | Printing press — movable type mass reproduction | absent | biped, bronze, cheap-pulped-writing-surface, copper, domestic-animals-kept, fire, hide-writing-surface, movable-type-mass-reproduction, phonemic-script, steel, stone-tool, written-record-kept |
| inv-comet-tracking | Comet trajectory tracking | absent | agriculture, art, biped, bright-comet, ceremonial-burial, collected-holdings-outliving-their-keepers, comet-painting, comet-tracking, composed-work-transmitted-as-text, domestic-animals-kept, empire, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, realistic-maps, star-maps, star-maps-better, stone-tool, university, written-record-kept |
| inv-cape-good-hope | Cape of Good Hope | absent | biped, bronze, cape-good-hope, compass, copper, fire, ocean-navigation, raft, river-boat, rudder, sea-navigation, stone-tool |
| inv-magnetic-declination | Magnetic declination | absent | biped, bronze, compass, copper, fire, magnetic-declination, magnetic-pole, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-new-world | New world | absent | biped, bronze, compass, copper, fire, new-world, ocean-navigation, raft, river-boat, rudder, sea-navigation, stone-tool |
| inv-syphilis | Syphilis | absent | agriculture, biped, ceremonial-burial, domestic-animals-kept, empire, medicine, nation, quarantine, stone-tool, syphilis, written-record-kept |
| inv-india-water-route | India water route | absent | biped, bronze, cape-good-hope, compass, copper, fire, india-water-route, ocean-navigation, raft, river-boat, rudder, sea-navigation, stone-tool |
| inv-america | America | absent | agriculture, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, fire, nation, new-world, ocean-navigation, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-hand-watch | Spring-powered watches | absent | agriculture, biped, bronze, copper, domestic-animals-kept, fire, hand-watch, mechanical-clock, nation, simple-sundial, steel, stone-tool, sundial, water-clock |
| inv-pacific-ocean | Pacific ocean | absent | agriculture, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, fire, nation, new-world, ocean-navigation, pacific-ocean, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-florida | Florida | absent | agriculture, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, fire, florida, nation, new-world, ocean-navigation, pacific-ocean, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-mexico | Mexico conquered | absent | agriculture, america, biped, bow, bronze, cannon, catapult, ceremonial-burial, cheap-pulped-writing-surface, compass, copper, domestic-animals-kept, fire, gunpowder, hide-writing-surface, mexico, nation, new-world, numbers, ocean-navigation, pacific-ocean, paved-road, raft, riding-animal-bred, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage, written-record-kept |
| inv-around-earth | Earth circumnavigated | absent | agriculture, america, around-earth, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, fire, nation, new-world, ocean-navigation, pacific-ocean, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-peru | Peru conquered | absent | agriculture, america, biped, bow, bronze, cannon, catapult, ceremonial-burial, cheap-pulped-writing-surface, compass, copper, domestic-animals-kept, fire, gunpowder, hide-writing-surface, nation, new-world, numbers, ocean-navigation, pacific-ocean, paved-road, peru, raft, riding-animal-bred, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage, written-record-kept |
| inv-cubic-equations | Cubic equations | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, cubic-equations, domestic-animals-kept, geometry, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-comet-tails | Comet tails | absent | agriculture, art, biped, bright-comet, ceremonial-burial, collected-holdings-outliving-their-keepers, comet-painting, comet-tails, comet-tracking, composed-work-transmitted-as-text, domestic-animals-kept, empire, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, realistic-maps, star-maps, star-maps-better, stone-tool, university, written-record-kept |
| inv-mississipi-river | Mississipi river | absent | agriculture, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, fire, mississipi-river, nation, new-world, ocean-navigation, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-amazon-river | Amazon river | absent | agriculture, amazon-river, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, fire, nation, new-world, ocean-navigation, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-heliocentric-practice | Math of heliocentricity | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, heliocentric-practice, heliocentric-theory, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-illustrated-anatomy | Illustrated anatomy book | absent | anatomy, art, biped, cadaver, ceremonial-burial, illustrated-anatomy, medicine, stone-tool, written-record-kept |
| inv-negative-numbers | Negative numbers | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, negative-numbers, numbers, stone-tool, university, written-record-kept |
| inv-surgery | Rational surgery | absent | anatomy, biped, cadaver, ceremonial-burial, medicine, stone-tool, surgery, written-record-kept |
| inv-planetary-tables-better | Better planetary tables | absent | agriculture, biped, domestic-animals-kept, empire, geocentric-universe, nation, planetary-tables, planetary-tables-better, realistic-maps, star-maps, star-maps-better, stone-tool, written-record-kept |
| inv-trig-tables | Trigonometric tables | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, irrational-numbers, logic, nation, numbers, stone-tool, trig-tables, university, written-record-kept |
| inv-eustacian-tubes | Eustacian tubes | absent | anatomy, art, biped, cadaver, ceremonial-burial, eustacian-tubes, illustrated-anatomy, medicine, stone-tool, written-record-kept |
| inv-northeast-passage | Northeastern passage | absent | arctic-circle, biped, bronze, copper, fire, northeast-passage, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-homologies | Vertebrate skeletons | absent | anatomy, animal-classification, biped, cadaver, ceremonial-burial, domestic-animals-kept, homologies, medicine, stone-tool, written-record-kept |
| inv-mineralogy | Mining book | absent | biped, bronze, copper, fire, mineral-fuel-extracted, mineralogy, steel, stone-tool |
| inv-tobacco | Tobacco | absent | agriculture, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, fire, nation, new-world, ocean-navigation, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, tobacco, wheeled-land-haulage |
| inv-scientific-societies | Scientific societies — corresponding body of inquirers | absent | agriculture, biped, bronze, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, corresponding-body-of-inquirers, domestic-animals-kept, fire, hide-writing-surface, movable-type-mass-reproduction, nation, phonemic-script, steel, stone-tool, university, written-record-kept |
| inv-musket | Musket | absent | agriculture, artillery, biped, bow, bronze, cannon, catapult, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, crossbow, domestic-animals-kept, fire, gunpowder, handheld-firearm, hide-writing-surface, lever-math, logic, musket, nation, numbers, steel, stone-tool, university, written-record-kept |
| inv-world-maps | Mercator projection | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, empire, geometry, logic, nation, numbers, realistic-maps, stone-tool, university, world-maps, written-record-kept |
| inv-supernova | Supernova | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, moon-distance, nation, new-star, numbers, raft, river-boat, stone-tool, supernova, university, written-record-kept |
| inv-greenland | Greenland revisited | absent | arctic-circle, biped, bronze, copper, fire, greenland, northeast-passage, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-comet-distance | Comet distances | absent | agriculture, art, biped, bright-comet, ceremonial-burial, collected-holdings-outliving-their-keepers, comet-distance, comet-painting, comet-tracking, composed-work-transmitted-as-text, domestic-animals-kept, empire, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, realistic-maps, star-maps, star-maps-better, stone-tool, university, written-record-kept |
| inv-drake-strait | Drake Strait | absent | agriculture, america, around-earth, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, drake-strait, fire, nation, new-world, ocean-navigation, pacific-ocean, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-pendulum | Pendulums for time | absent | agriculture, biped, domestic-animals-kept, mechanical-clock, nation, pendulum, simple-sundial, stone-tool, sundial, water-clock |
| inv-siberia | Siberia settled | absent | biped, bronze, compass, copper, domestic-animals-kept, fire, ocean-navigation, raft, riding-animal-bred, river-boat, sea-navigation, siberia, stone-tool |
| inv-gregorian-calendar | Gregorian calendar | absent | biped, calendar, gregorian-calendar, julian-calendar, simple-sundial, stone-tool, written-record-kept, year-number |
| inv-hydrostatics | Hydrostatics — quantitative law of fluids known | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, quantitative-law-of-fluids-known, stone-tool, university, written-record-kept |
| inv-decimal-notation | Positional notation | absent | abacus, agriculture, algebra, arab-numbers, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, decimal-notation, domestic-animals-kept, geometry, logic, nation, numbers, raft, river-boat, sea-navigation, stone-tool, university, written-record-kept, zero |
| inv-cryptanalysis | Code breaking | absent | abacus, agriculture, algebra, arab-numbers, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, cryptanalysis, domestic-animals-kept, empire, geometry, logic, nation, numbers, raft, river-boat, sea-navigation, stone-tool, university, written-record-kept, zero |
| inv-falling-motion | Falling motion | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, geometry, logic, nation, numbers, scale, stone-tool, university, written-record-kept |
| inv-stocking-frame | Stocking frame | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, lever-math, logic, nation, numbers, stocking-frame, stone-tool, university, written-record-kept |
| inv-microscope | Microscope | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-algebraic-symbol | Algebraic symbols | absent | agriculture, algebra, algebraic-symbol, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-thermometer | Thermometer | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, fire, geometry, glass, glass-blowing, logic, nation, numbers, quantitative-law-of-fluids-known, stone-tool, thermometer, university, written-record-kept |
| inv-east-indies | East indies settled | absent | biped, bronze, copper, east-indies, fire, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-pi-accuracy | PI accuracy | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, lever-math, logic, nation, numbers, pi-accuracy, stone-tool, university, written-record-kept |
| inv-chemistry-textbook | Chemistry book | absent | acetic-acid, agriculture, biped, chemistry-textbook, domestic-animals-kept, fermentation, sulfuric-acid |
| inv-earth-magnet | Earth as giant magnet | absent | biped, bronze, compass, copper, earth-magnet, fire, magnetic-declination, magnetic-pole, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-vein-valves | Vein valves | absent | biped, ceremonial-burial, medicine, spinal-cord, stone-tool, vein-valves, written-record-kept |
| inv-english-settlement-america | English America | absent | agriculture, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, english-settlement-america, fire, nation, new-world, ocean-navigation, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-french-settlement-america | French America | absent | agriculture, america, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, english-settlement-america, fire, french-settlement-america, nation, new-world, ocean-navigation, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, wheeled-land-haulage |
| inv-telescope | Telescope | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, stone-tool, telescope, university, written-record-kept |
| inv-elliptical-orbits | Elliptical orbits | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, elliptical-orbits, empire, geocentric-universe, geometry, irrational-numbers, logic, nation, numbers, planetary-tables, planetary-tables-better, realistic-maps, star-maps, star-maps-better, stone-tool, trig-tables, university, written-record-kept |
| inv-milky-way | Milky Way | absent | agriculture, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, milky-way, nation, numbers, optics, raft, river-boat, sea-navigation, stone-tool, telescope, university, water-element, written-record-kept |
| inv-moon | Moon mountains | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, moon, nation, numbers, optics, stone-tool, telescope, university, written-record-kept |
| inv-jupiter-moons | Jupiter&#39;s four moons | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, elliptical-orbits, empire, eyeglass, fire, geocentric-universe, geometry, glass, glass-blowing, irrational-numbers, jupiter-moons, logic, microscope, moon, nation, numbers, optics, planetary-tables, planetary-tables-better, realistic-maps, star-maps, star-maps-better, stone-tool, telescope, trig-tables, university, written-record-kept |
| inv-sunspots | Sunspots | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, stone-tool, sunspots, telescope, university, written-record-kept |
| inv-venus-phases | Venus phases | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, heliocentric-practice, heliocentric-theory, logic, microscope, nation, numbers, optics, stone-tool, telescope, university, venus-phases, written-record-kept |
| inv-andromeda-nebula | Andromeda Nebula | absent | agriculture, andromeda-nebula, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, stone-tool, telescope, university, written-record-kept |
| inv-logarithms | Logarithms | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logarithms, logic, nation, negative-numbers, numbers, stone-tool, university, written-record-kept |
| inv-metabolism | Biological metabolism | absent | anatomy, art, biped, cadaver, ceremonial-burial, illustrated-anatomy, medicine, metabolism, scale, stone-tool, written-record-kept |
| inv-baffin-bay | Baffin Bay | absent | arctic-circle, baffin-bay, biped, bronze, copper, fire, greenland, northeast-passage, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-tierra-del-fuego | Tierra Del Fuego | absent | agriculture, america, around-earth, biped, bronze, ceremonial-burial, compass, copper, domestic-animals-kept, drake-strait, fire, nation, new-world, ocean-navigation, pacific-ocean, paved-road, raft, river-boat, rudder, sea-navigation, stone-monument, stone-tool, tierra-del-fuego, wheeled-land-haulage |
| inv-scientific-method | Scientific method | absent | agriculture, biped, bronze, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, corresponding-body-of-inquirers, domestic-animals-kept, fire, hide-writing-surface, movable-type-mass-reproduction, nation, phonemic-script, scientific-method, steel, stone-tool, university, written-record-kept |
| inv-stagecoach | Stagecoaches | absent | agriculture, biped, ceremonial-burial, copper, domestic-animals-kept, fire, nation, paved-road, stagecoach, stone-monument, stone-tool, wheeled-land-haulage |
| inv-refraction | Light refraction | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, quantitative-law-of-fluids-known, refraction, stone-tool, university, written-record-kept |
| inv-slide-rules | Slide rules | absent | abacus, agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logarithms, logic, nation, negative-numbers, numbers, slide-rules, stone-tool, university, written-record-kept |
| inv-gas | Gas state — matter recognised in a third state | absent | agriculture, alchemy, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, quantitative-law-of-fluids-known, raft, river-boat, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-aurochs-extinct | Aurochs extinction | absent | agriculture, aurochs-extinct, biped, domestic-animals-kept |
| inv-planetary-tables-even-better | Even better planetary tables | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, elliptical-orbits, empire, geocentric-universe, geometry, irrational-numbers, logarithms, logic, nation, negative-numbers, numbers, planetary-tables, planetary-tables-better, planetary-tables-even-better, realistic-maps, star-maps, star-maps-better, stone-tool, trig-tables, university, written-record-kept |
| inv-blood-circulation | Blood circulation | absent | anatomy, biped, blood-circulation, cadaver, ceremonial-burial, medicine, spinal-cord, stone-tool, vein-valves, written-record-kept |
| inv-science-vs-religion | Science religion debates | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, heliocentric-practice, heliocentric-theory, logic, nation, numbers, science-vs-religion, stone-tool, university, written-record-kept |
| inv-magnetic-declination-variation | Shifting magnetic declination | absent | biped, bronze, compass, copper, fire, magnetic-declination, magnetic-declination-variation, magnetic-pole, ocean-navigation, raft, river-boat, sea-navigation, stone-tool |
| inv-analytic-geometry | Cartesian geometry | absent | agriculture, algebra, analytic-geometry, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-fermat-last-theorem | Fermat&#39;s last theorem | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, fermat-last-theorem, geometry, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-cross-hairs | Cross hairs | absent | agriculture, artillery, biped, bow, bronze, cannon, catapult, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, cross-hairs, crossbow, domestic-animals-kept, fire, geometry, glass, glass-blowing, gunpowder, handheld-firearm, hide-writing-surface, lever-math, logic, musket, nation, numbers, optics, steel, stone-tool, university, written-record-kept |
| inv-adding-machine | Mechanical calculators | absent | abacus, adding-machine, biped, bronze, copper, fire, numbers, steel, stone-tool, written-record-kept |
| inv-quinine | Quinine | absent | biped, bronze, ceremonial-burial, copper, fire, medicinal-plants, medicine, ocean-navigation, quinine, raft, river-boat, sea-navigation, stone-tool, written-record-kept |
| inv-south-pacific | New Zealand and Tasmania | absent | biped, bronze, compass, copper, fire, ocean-navigation, raft, river-boat, sea-navigation, south-pacific, stone-tool |
| inv-barometer | Barometers — atmospheric pressure measured | absent | agriculture, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-air-pump | Air pumps — air evacuated from a vessel | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-air-pressure-altitude | Air pressure altitude | absent | agriculture, air-pressure-altitude, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-biblical-age-of-earth | Biblical earth age | absent | biblical-age-of-earth, biped, composed-work-transmitted-as-text, monotheism, stone-tool, written-record-kept |
| inv-double-star | Double star | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, double-star, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, stone-tool, telescope, university, written-record-kept |
| inv-names-on-moon | Moon features named | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, names-on-moon, nation, numbers, optics, stone-tool, telescope, university, written-record-kept |
| inv-lymphatic-vessels | Lymphatic vessels | absent | biped, ceremonial-burial, lymphatic-vessels, medicine, spinal-cord, stone-tool, vein-valves, written-record-kept |
| inv-air-pressure | Early pneumatics — pressure as a motive force | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, pressure-as-a-motive-force, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-probability | Probability | absent | abacus, agriculture, algebra, arab-numbers, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, cryptanalysis, domestic-animals-kept, empire, geometry, logic, nation, numbers, probability, raft, river-boat, sea-navigation, stone-tool, university, written-record-kept, zero |
| inv-pendulum-clock | Grandfather clocks | absent | agriculture, biped, domestic-animals-kept, mechanical-clock, nation, pendulum, pendulum-clock, simple-sundial, stone-tool, sundial, water-clock |
| inv-saturn-ring | Saturn&#39;s ring | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, saturn-ring, stone-tool, telescope, university, written-record-kept |
| inv-falling-motion-experiment | Falling motion experiment | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, falling-motion-experiment, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-red-blood-cells | Red blood cells | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, red-blood-cells, stone-tool, university, written-record-kept |
| inv-syrtis-major | Syrtis Major | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, stone-tool, syrtis-major, telescope, university, written-record-kept |
| inv-capillaries | Capillaries | absent | anatomy, biped, blood-circulation, cadaver, capillaries, ceremonial-burial, medicine, spinal-cord, stone-tool, vein-valves, written-record-kept |
| inv-static-electricity | Static electricity | absent | biped, fire, static-electricity |
| inv-acid-base | Acid-base balance | absent | acid-base, biped, ceremonial-burial, epilepsy, medicine, stone-tool, written-record-kept |
| inv-chemical-elements | Scientific chemistry | absent | alchemy, atom, biped, chemical-elements, five-elements, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-boyles-law | Boyle's law — pressure volume relation known | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, pressure-as-a-motive-force, pressure-volume-relation-known, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-royal-society | Royal Society | absent | agriculture, biped, bronze, cheap-pulped-writing-surface, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, corresponding-body-of-inquirers, domestic-animals-kept, fire, hide-writing-surface, movable-type-mass-reproduction, nation, phonemic-script, royal-society, steel, stone-tool, university, written-record-kept |
| inv-jupiter-red-spot | Jupiter&#39;s Red Spot | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, jupiter-red-spot, logic, microscope, nation, numbers, optics, stone-tool, telescope, university, written-record-kept |
| inv-cell | Cells | absent | agriculture, biped, cell, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-light-diffraction | Diffraction | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, fire, geometry, glass, glass-blowing, light-diffraction, logic, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-planet-rotations | Planetary rotation | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, planet-rotations, stone-tool, telescope, university, written-record-kept |
| inv-light-spectrum | Prisms | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, light-spectrum, logic, microscope, nation, numbers, optics, stone-tool, telescope, university, written-record-kept |
| inv-conservation-momentum | Conservation of momentum | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, conservation-momentum, domestic-animals-kept, falling-motion, falling-motion-experiment, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, university, water-element, written-record-kept |
| inv-no-spontaneous-generation | No spontaneous generation | absent | alchemy, atom, biped, chemical-elements, five-elements, no-spontaneous-generation, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-reflecting-telescope | Reflecting telescopes | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, light-spectrum, logic, microscope, nation, numbers, optics, quantitative-law-of-fluids-known, reflecting-telescope, refraction, stone-tool, telescope, university, written-record-kept |
| inv-blood-color | Blood colors | absent | arteries-veins, biped, blood-color, cadaver, ceremonial-burial, medicine, stone-tool, written-record-kept |
| inv-calculus | Calculus | absent | agriculture, algebra, analytic-geometry, biped, calculus, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, logic, nation, numbers, stone-tool, university, written-record-kept |
| inv-double-refraction | Double refraction | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, double-refraction, fire, geometry, glass, glass-blowing, logic, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-fossils | Fossils | absent | biped, bronze, copper, fire, fossils, mineral-fuel-extracted, mineralogy, steel, stone-tool |
| inv-phosphorus | Phosphorus | absent | alchemy, atom, biped, chemical-elements, five-elements, phosphorus, raft, river-boat, sea-navigation, stone-tool, water-element |
| inv-diabetes | Diabetes diagnosed | absent | anatomy, art, biped, cadaver, ceremonial-burial, diabetes, illustrated-anatomy, medicine, metabolism, scale, stone-tool, written-record-kept |
| inv-saturn-satellites | Saturn&#39;s four moons | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microscope, nation, numbers, optics, saturn-satellites, stone-tool, telescope, university, written-record-kept |
| inv-mars-distance | Mars distance | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, elliptical-orbits, empire, geocentric-universe, geometry, irrational-numbers, logarithms, logic, mars-distance, nation, negative-numbers, numbers, planetary-tables, planetary-tables-better, planetary-tables-even-better, realistic-maps, star-maps, star-maps-better, stone-tool, trig-tables, university, written-record-kept |
| inv-saturn-rings | Saturn&#39;s multiple rings | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, light-spectrum, logic, microscope, nation, numbers, optics, quantitative-law-of-fluids-known, reflecting-telescope, refraction, saturn-rings, stone-tool, telescope, university, written-record-kept |
| inv-speed-of-light | Speed of light | absent | agriculture, biped, calendar, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eclipse, eyeglass, fire, geometry, glass, glass-blowing, light-spectrum, logic, microscope, nation, numbers, optics, quantitative-law-of-fluids-known, reflecting-telescope, refraction, simple-sundial, speed-of-light, stone-tool, telescope, university, written-record-kept |
| inv-microorganisms | Microorganisms | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microorganisms, microscope, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-light-as-wave | Light as wave | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, fire, geometry, glass, glass-blowing, light-as-wave, logic, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-southern-stars | Southern stars | absent | agriculture, biped, bronze, cape-good-hope, collected-holdings-outliving-their-keepers, compass, composed-work-transmitted-as-text, copper, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, india-water-route, light-spectrum, logic, microscope, nation, numbers, ocean-navigation, optics, quantitative-law-of-fluids-known, raft, reflecting-telescope, refraction, river-boat, rudder, sea-navigation, southern-stars, stone-tool, telescope, university, written-record-kept |
| inv-pressure-cooker | Pressure cookers — pressure vessel held above ambient | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, fire, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, steam-moves-a-mechanism, stone-tool, university, water-element, written-record-kept |
| inv-muscles-bones | Human kinetics | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, lever-math, logic, muscles-bones, nation, numbers, stone-tool, university, written-record-kept |
| inv-dodo-extinction | Dodo extinct | absent | biped, bronze, cape-good-hope, compass, copper, dodo-extinction, fire, india-water-route, ocean-navigation, raft, river-boat, rudder, sea-navigation, stone-tool |
| inv-plant-sexuality | Plant sexuality | absent | agriculture, biped, botany, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, nation, plant-sexuality, stone-tool, university, written-record-kept |
| inv-bacteria | Bacteria | absent | agriculture, bacteria, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, eyeglass, fire, geometry, glass, glass-blowing, logic, microorganisms, microscope, nation, numbers, optics, stone-tool, university, written-record-kept |
| inv-earth-size-accurate | Earth size accurate | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, earth-size-accurate, eyeglass, fire, geometry, glass, glass-blowing, light-spectrum, logic, microscope, nation, numbers, optics, quantitative-law-of-fluids-known, reflecting-telescope, refraction, stone-tool, telescope, university, written-record-kept |
| inv-imaginary-numbers | Complex numbers | absent | agriculture, algebra, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, geometry, imaginary-numbers, logic, nation, negative-numbers, numbers, stone-tool, university, written-record-kept |
| inv-meteorological-map | Trade winds mapped | absent | agriculture, biped, bronze, climactic-zone, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, domestic-animals-kept, empire, fire, geometry, logic, meteorological-map, nation, numbers, ocean-navigation, raft, realistic-maps, river-boat, sea-navigation, spherical-earth, stone-tool, university, world-maps, written-record-kept |
| inv-plant-species-classified | Plant species classified | absent | animal-classification, biped, domestic-animals-kept, plant-species-classified |
| inv-laws-motion | Laws of motion | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, elliptical-orbits, empire, geocentric-universe, geometry, irrational-numbers, laws-motion, logic, nation, numbers, planetary-tables, planetary-tables-better, realistic-maps, star-maps, star-maps-better, stone-tool, trig-tables, university, written-record-kept |
| inv-universal-gravitation | Universal graviation | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, elliptical-orbits, empire, geocentric-universe, geometry, irrational-numbers, laws-motion, logic, nation, numbers, planetary-tables, planetary-tables-better, realistic-maps, star-maps, star-maps-better, stone-tool, trig-tables, universal-gravitation, university, written-record-kept |
| inv-shape-of-earth | Non-spherical earth | absent | agriculture, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, elliptical-orbits, empire, geocentric-universe, geometry, irrational-numbers, laws-motion, logic, nation, numbers, planetary-tables, planetary-tables-better, realistic-maps, shape-of-earth, star-maps, star-maps-better, stone-tool, trig-tables, universal-gravitation, university, written-record-kept |
| inv-plate-glass | Plate glass | absent | biped, fire, glass, glass-blowing, plate-glass |
| inv-animal-classification-improved | Improved animal classifications | absent | animal-classification, animal-classification-improved, biped, domestic-animals-kept |
| inv-calculating-machines | Calculating machines | absent | abacus, adding-machine, biped, bronze, calculating-machines, copper, fire, numbers, steel, stone-tool, written-record-kept |
| inv-mortality-tables | Mortality tables | absent | abacus, agriculture, algebra, arab-numbers, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, cryptanalysis, domestic-animals-kept, empire, geometry, logic, mortality-tables, nation, numbers, probability, raft, river-boat, sea-navigation, stone-tool, university, written-record-kept, zero |
| inv-miners-friend | Miner's friend — engine raises water from a working | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, bronze, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, domestic-animals-kept, engine-raises-water-from-a-working, falling-motion, fire, five-elements, geometry, logic, matter-recognised-in-a-third-state, mineral-fuel-extracted, nation, numbers, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, steel, stone-tool, university, water-element, written-record-kept |
| inv-scientific-voyages | Scientific ocean voyages | absent | biped, bronze, cape-good-hope, compass, copper, fire, india-water-route, ocean-navigation, raft, river-boat, rudder, scientific-voyages, sea-navigation, stone-tool |
| inv-gas-volume-temperature | Gas volume temperature — thermal expansion of gas known | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, five-elements, geometry, logic, matter-recognised-in-a-third-state, nation, numbers, pressure-as-a-motive-force, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, thermal-expansion-of-gas-known, university, water-element, written-record-kept |
| inv-coke-iron | Coke and iron | absent | biped, bronze, coke-iron, copper, fire, mineral-fuel-extracted, mineralogy, steel, stone-tool |
| inv-newcomen-steam-engine | Newcomen steam engine — engine does sustained useful work | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, bronze, coke-iron, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, domestic-animals-kept, engine-does-sustained-useful-work, engine-raises-water-from-a-working, falling-motion, fire, five-elements, geometry, logic, matter-recognised-in-a-third-state, mineral-fuel-extracted, mineralogy, nation, numbers, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, steam-moves-a-mechanism, steel, stone-tool, university, water-element, written-record-kept |
| inv-mercury-thermometer | Mercury thermometer | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, fire, five-elements, geometry, glass, glass-blowing, logic, matter-recognised-in-a-third-state, mercury-thermometer, nation, numbers, pressure-as-a-motive-force, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, thermal-expansion-of-gas-known, thermometer, university, water-element, written-record-kept |
| inv-heat-capacity | Heat capacity | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, fire, five-elements, geometry, glass, glass-blowing, heat-capacity, logic, matter-recognised-in-a-third-state, mercury-thermometer, nation, numbers, pressure-as-a-motive-force, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, thermal-expansion-of-gas-known, thermometer, university, water-element, written-record-kept |
| inv-latent-heat | Latent heat — latent heat known | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, domestic-animals-kept, falling-motion, fire, five-elements, geometry, glass, glass-blowing, heat-capacity, latent-heat-known, logic, matter-recognised-in-a-third-state, mercury-thermometer, nation, numbers, pressure-as-a-motive-force, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, stone-tool, thermal-expansion-of-gas-known, thermometer, university, water-element, written-record-kept |
| inv-steam-engine | Steam engine — engine efficient enough to spread | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, bronze, coke-iron, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, domestic-animals-kept, engine-does-sustained-useful-work, engine-efficient-enough-to-spread, engine-raises-water-from-a-working, falling-motion, fire, five-elements, geometry, glass, glass-blowing, heat-capacity, latent-heat-known, logic, matter-recognised-in-a-third-state, mercury-thermometer, mineral-fuel-extracted, mineralogy, nation, numbers, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, raft, river-boat, scale, sea-navigation, steam-moves-a-mechanism, steel, stone-tool, thermal-expansion-of-gas-known, thermometer, university, water-element, written-record-kept |
| inv-improved-steam-engine | Improved steam engine — rotary power drives general machinery | absent | agriculture, air-evacuated-from-a-vessel, alchemy, atmospheric-pressure-measured, atom, biped, bronze, coke-iron, collected-holdings-outliving-their-keepers, composed-work-transmitted-as-text, copper, domestic-animals-kept, engine-does-sustained-useful-work, engine-efficient-enough-to-spread, engine-raises-water-from-a-working, falling-motion, fire, five-elements, geometry, glass, glass-blowing, heat-capacity, latent-heat-known, logic, matter-recognised-in-a-third-state, mercury-thermometer, mineral-fuel-extracted, mineralogy, nation, numbers, pressure-as-a-motive-force, pressure-vessel-held-above-ambient, pressure-volume-relation-known, quantitative-law-of-fluids-known, raft, river-boat, rotary-power-drives-general-machinery, scale, sea-navigation, steam-moves-a-mechanism, steel, stone-tool, thermal-expansion-of-gas-known, thermometer, university, water-element, written-record-kept |

## Items

| id | title | verdict | anchor | contested | disclosure | note |
|---|---|---|---|---|---|---|
| inv-biped | Bipedal species | absent |  |  | NOT BLIND, IN THE NARROW SENSE THE FAMILY'S RULE STILL DEMANDS: `inv-biped` is a ROOT — no `presupposes` — the sole root of the closed 301-item population (Task 1 verified exactly one). Nothing upstream forces its verdict, so the family's chosen/inherited rule (`technologies/CLAUDE.md`) requires a disclosure regardless of how the verdict was reached. THIS VERDICT IS A TASK 2 PLACEHOLDER, NOT AN AUTHORED ONE, and that is the honest thing to disclose: Task 2 (The Cadastre) adds every one of the 260 new items at `absent` per its own brief's Step 4 ("An absent here is a placeholder that Task 3 must confirm or replace") and performs no repository search for any of them, this one included. Task 3 owes the real search this disclosure cannot honestly claim was already run, and if Task 3 changes this item's verdict it inherits this disclosure and must revisit it rather than leave a placeholder's reasoning attached to an authored verdict. |  |
| inv-stone-tool | Stone tools | absent |  |  |  |  |
| inv-fire | Fire tamed | absent |  |  |  |  |
| inv-ceremonial-burial | Ceremonial burial | absent |  |  |  |  |
| inv-art | Art | absent |  |  |  |  |
| inv-bow | Bows and arrows | absent |  |  |  |  |
| inv-oil-lamp | Oil lamps | absent |  |  |  |  |
| inv-animal-dom | Animals domesticated — domestic animals kept | deferred | registry:BIO-animal-domestication |  |  | THE NEAREST MISS IN THE CORPUS, and it misses on the animal rather than on the institution. `subsistence` is a committed predicate ("a settlement's subsistence mode") whose values are Farming, Herding, Fishing and Foraging (`domains/culture/src/subsistence.rs`), it differs between settlements in one world, and Herding is documented "Pastoral herding" -- so a people that lives off animals is already a fact Hornvale commits and varies. What is missing is the animal: `BIO-animal-domestication` records that the registry "already admits domestication for the plant kingdom (barley, wheat, rice, millet, tuber, with a crops model behind them) and refuses it for the animal kingdom -- every one of the ~30 registered `*-kind` fauna is wild", and the registry bears that out: 39 `*-kind` rows in `book/src/reference/concept-registry-generated.md`, peoples and beasts together, and not one of the beasts domestic. A herding people herds nothing in particular. The row is `raw`, so this is `deferred` rather than `absent`: what it would take is the row's own content, a working animal as "the missing half of an existing axis". PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-biped`, previously described here as outside the corpus. The closed 301-item population makes `biped` a real item in this corpus — the population's sole root (Task 1), scored `absent` as a Task 2 placeholder pending Task 3's own search — so this item is now INHERITED rather than chosen under the family's chosen/inherited rule (`technologies/CLAUDE.md`): the weakest-demand rule reads its `deferred` verdict off `inv-biped`'s `absent`, and its former `disclosure` (which called this item a root) is deleted. Swept against `BIO-8`, which `BIO-animal-domestication` opens by sharpening: `BIO-8` is the broader row and names domestication generally, so the sharper row is the better anchor and is the one cited. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. `BIO-animal-domestication` was found by searching the idea registry, so the `deferred` is a positive claim that search produced. Its SELECTION was blind: it is in the `knights` arc's own link list." |
| inv-agriculture | Plants domesticated | absent |  |  |  |  |
| inv-pottery | Pottery | absent |  |  |  |  |
| inv-fish-nets | Linen | absent |  |  |  |  |
| inv-raft | Rafts | absent |  |  |  |  |
| inv-sickle | Sickles | absent |  |  |  |  |
| inv-irrigation | Irrigation | absent |  |  |  |  |
| inv-scale | Weight scales | absent |  |  |  |  |
| inv-copper | Copper | absent |  |  |  |  |
| inv-simple-sundial | Simple sundials | absent |  |  |  |  |
| inv-bronze | Bronze | absent |  |  |  |  |
| inv-cart | Wheeled carts — wheeled land haulage | absent |  |  |  | Nothing in the engine has a wheel. The honest near-miss is elsewhere in the same economic space: `occ-function` commits Trade, "a waypoint or market on a trade route" (`domains/history/src/record.rs`), so a world has routes and market seats -- and no conveyance on them. Haulage capacity never enters any computation: a community's strength is population times `tech_weight` and its delving is metres per head per epoch, both of which read population where a cart would read carrying capacity. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): the catalogue builds this on `inv-copper`, previously described here as outside the corpus and as under-describing the derived demand set. The closed 301-item population makes `copper` a real item in this corpus (itself inherited, `absent`, from `fire` and `stone-tool` further back), so the demand set no longer under-describes this item — and this item is now INHERITED rather than chosen: its former `disclosure` (which called it a root) is deleted. Swept against `BIO-8` and `TECH-2`: neither names haulage, draught or the wheel. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `MAP-61`'s connection graph plans roads as graph EDGES and `UNI-5` ('Authored transmission media (the "stones") -- comms/energy/transport as independent latent graph-construction systems', `elaborated`) plans a world-potential a culture realizes at a craft fraction -- a route and an exotic medium respectively, neither a vehicle. Nothing in 1,779 rows names a wheeled conveyance. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`.  Its SELECTION was blind: it is in the `knights` arc's own link list." |
| inv-plow | Plows | absent |  |  |  |  |
| inv-river-boat | River boats | absent |  |  |  |  |
| inv-writing | Writing — written record kept | deferred | registry:MAP-8 |  |  | TWO NEAR-MISSES, AND BOTH ARE INSTRUCTIVE FAILURES OF THE SAME KIND: the vocabulary for writing exists and nothing in any world writes. (1) `write` and `read` are registered concepts -- `write`, "to set words down in writing"; `read`, "to take meaning from written words" -- and they are INERT. `LANG-in-character-acts-are-unspeakable` records that seven minted concepts including `read` and `write` "render `Gap \| Gap` for every species in every world, because nothing grants them `Steeped` or `KnowsOf`", verified in the committed `book/src/reference/concept-manifest-generated.md`. No culture can even SAY the act, let alone perform it. (2) `Orthography` (`domains/language/src/typology.rs:90`) looks like a writing system and is not: its own doc says "How this family's segments are spelled in the romanization. A view over `Segment`, so this field alone moves no stream draw". It is a reader-facing spelling convention for committed name strings, chosen per language family, with no in-world existence -- a fact about how the artifact prints, not about what a people can do. WHAT WOULD CHANGE THE VERDICT: a committed fact that some peoples keep written records and others do not. `NARR-monument-writes-itself` is the nearest plan (it makes literacy a gate on reading an inscription) and it is `elaborated`, not shipped; it was considered as a `deferred` anchor and refused here, because that row plans the RENDERING of a monument's text and treats literacy as a gate it assumes rather than as a capability a people acquires and loses, which is this corpus's demand. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-stone-tool`, previously described here as outside the corpus. The closed 301-item population makes `stone-tool` a real item in this corpus (`absent`, a Task 2 placeholder pending Task 3), so this item is now INHERITED rather than chosen under the family's chosen/inherited rule: the weakest-demand rule reads its `deferred` verdict off `inv-stone-tool`'s `absent`, and its former `disclosure` (which called it a root) is deleted. The row's own closing sentence is the evidence for that refusal rather than a paraphrase of it: "Rendering, not authoring, is the whole cost." A row whose self-assessment is that the content already exists and only needs displaying does not plan a people that keeps written records. Swept against `BIO-8` and `TECH-2`: neither names writing, literacy or a record. AMENDED AFTER FREEZE (campaign ledger #17, fix round 1): THIS ITEM IS NOW `deferred` ON `MAP-8`, and the verdict above is not the one this corpus froze with. `MAP-8` ('Writing as a culture acquiring its own ledger -- oral = phenomena, literate = freezing phenomena into facts; borrowed scripts as contact fossils', `elaborated`) names `written-record-kept` as its entire content: a culture acquiring a ledger IS a people keeping a written record, so the row plans THE DEMAND ITSELF rather than a prerequisite of it, which is the rule this file established. It is not `shipped`, so `deferred` is admissible. The sentence two paragraphs up -- 'WHAT WOULD CHANGE THE VERDICT: a committed fact that some peoples keep written records and others do not' -- is the condition `MAP-8` meets, and this item's `disclosure` predicted the consequence before the row was found. WHY THE ORIGINAL VERDICT WAS WRONG, AND IT IS A SCOPE FAILURE RATHER THAN A MISREPORTED ONE -- THE FILE STATED ITS OWN POPULATION HONESTLY AT BOTH LEVELS, which is worth saying plainly because the opposite reading is the easy one. `provenance` is headed 'THE `absent` COLUMN WAS SWEPT AGAINST TWO NAMED IDEA-REGISTRY ROWS', and this note already ended 'Swept against `BIO-8` and `TECH-2`: neither names writing, literacy or a record', which is true. Nothing here concealed anything. What was wrong is that a TWO-ROW population cannot support a verdict for 38 items, and the two rows were named by the dispatching controller rather than chosen by a search -- so the sweep's population was set before the question was asked. That is the fourth instance of campaign ledger #14's pattern (the check's population narrower than the claim's) and the second of them the controller's own. Measured at the amendment: this file cited 18 distinct registry rows and its sibling `henrich-2004-extended` cited 49, under identical rules. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. `NARR-monument-writes-itself` was found by searching the idea registry, considered as a `deferred` anchor and refused; a reader who reads that row as planning written records in the world rather than their RENDERING should move this item to `deferred` and the five items downstream of it with it. Its SELECTION was blind: it is in the `republic-of-letters` arc's own link list. FIX ROUND 1: THIS DISCLOSURE'S OWN PREDICTION CAME HALF TRUE, AND THE HALF THAT FAILED IS THE INTERESTING ONE. It said a reader who read `NARR-monument-writes-itself` as planning written records 'should move this item to `deferred` and the five items downstream of it with it'. The item did move -- on `MAP-8`, a row this file never cited, not on the row the prediction named -- and the five downstream items did NOT follow: `inv-literature` moved on its own row (`DOM-aesthetics`) and `inv-library` behind it, while `inv-papyrus`, `inv-alphabet`, `inv-paper`, `inv-block-printing` and `inv-printing-press` stayed `absent` on their own demands. A prerequisite leaving `absent` unforces its dependents' verdicts; it does not raise them." |
| inv-nation | Nations | absent |  |  |  |  |
| inv-candle | Candles | absent |  |  |  |  |
| inv-papyrus | Papyrus — plant fibre writing surface | absent |  |  |  | Downstream of `inv-writing` (which was `absent` when this was written and is `deferred` since fix round 1, so the clause that once read 'this cannot score higher' no longer holds -- the item stays `absent` on its OWN demand, not on its prerequisite's); recorded with its own near-miss because the near-miss is a genuinely different one. Hornvale models plants as a CLIMATE fact rather than a material a people processes: `Crop` (`domains/climate/src/crops.rs`) carries barley, wheat, rice, millet, tuber and vine, and the module's own doc says "A crop is a climate fact -- a band of temperature and moisture on arable ground". There is no fibre, no sedge, and no step between a standing plant and a made thing. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-nation`, previously described here as outside the corpus. The closed 301-item population makes `nation` a real item in this corpus (itself inherited, `absent`, from `agriculture` further back), and this item's other prerequisite `inv-writing` is ALSO now inherited (from `inv-stone-tool`, restored the same way) — so this item is now INHERITED rather than chosen under the family's chosen/inherited rule, on either edge alone: its former `disclosure` (which explained why it was chosen since fix round 1) is deleted. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1) AND NOTHING MOVES IT: no row names a writing SURFACE. `MEM-8` ('The artifact channel ... Inscribed rots (the unpaid scribe)', `raw`) is the nearest, and it plans a transmission CHANNEL whose medium is assumed rather than a material anyone learns to make. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND, AND CHOSEN ONLY SINCE THE FIX-ROUND-1 AMENDMENT: its sole prerequisite `inv-writing` moved from `absent` to `deferred`, so nothing upstream forces this verdict any more and it rests on a search -- which is the disclosure rule working as ledger #13 intended, and a worked example of why a root-keyed check under-covers. The search: all 1,779 registry rows' openings read, no row names a writing surface, `MEM-8` read in full and refused in the note. The session that ran it had already read `TechHorizon`, `tech_for` and this campaign's findings." |
| inv-calendar | Calendar | absent |  |  |  |  |
| inv-stone-monument | Stone monuments | absent |  |  |  |  |
| inv-glass | Glass | absent |  |  |  |  |
| inv-literature | Literature — composed work transmitted as text | deferred | registry:DOM-aesthetics |  |  | INHERITED BY THE CADASTRE (Task 2, 2026-09-12): this item's sole prerequisite `inv-writing` is itself now inherited — the closed 301-item population restores `inv-writing`'s real prerequisite `inv-stone-tool` (`absent`), so `inv-writing` no longer forces nothing upstream. This item is therefore INHERITED rather than chosen under the family's chosen/inherited rule (`technologies/CLAUDE.md`), and its former `disclosure` (which explained why it was chosen since fix round 1) is deleted. Downstream of `inv-writing`. The near-miss is the most substantial in the corpus and it is worth being exact about why it does not count: Hornvale has a large, shipped knowledge-and-transmission layer -- claims propagate between people, degrade with each boundary they cross, and `KNOW-boundary-not-accumulation` records damage accumulating "as a continuous width, resolved to a rung only at emit". All of it is ORAL. A composed work whose wording survives its teller is exactly the thing that layer's physics is built to deny, so this item's demand is not a gap in the transmission model but a different mechanism beside it. AMENDED AFTER FREEZE (campaign ledger #17, fix round 1): `deferred` ON `DOM-aesthetics`, AND THIS IS THE MOST CONTESTABLE OF THE THREE AMENDMENTS -- a reviewer should check it first. The row ('**aesthetics** crate -- the generative output of expressive artifacts (text / image / music / built space) from a small per-culture vector behind a content->render seam ... Realizes EXP-1/EXP-2/EXP-3a', `raw`) names TEXT as the first of its own enumerated elements, produced from a PER-CULTURE vector, which is `composed-work-transmitted-as-text` at the grain this family requires: a work a people makes, varying between peoples. THE ARGUMENT AGAINST, stated so it can be taken: the row plans GENERATION behind a content->render seam and says nothing about a work PERSISTING, while the note above locates this item's demand precisely in persistence ('a composed work whose wording survives its teller'). A reader who takes that objection should return this item to `absent`, and `inv-library` becomes inherited again with it. `EXP-1` ('One seeded grammar engine, four media (text / image / music / space) from a small per-culture vector', `elaborated`) is the row `DOM-aesthetics` says it realizes; one anchor is cited and the second recorded, per the no-double-anchoring rule. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND, AND CHOSEN SINCE THE FIX-ROUND-1 AMENDMENT (its prerequisite `inv-writing` is now `deferred`). The `deferred` on `DOM-aesthetics` is a positive claim produced by reading all 1,779 registry row openings and then that row in full; it is also the amendment this corpus is least sure of, and the note states the objection and the consequence of taking it. A reader who reverses it returns this item to `absent` and `inv-library` to inherited." |
| inv-empire | Empires | absent |  |  |  |  |
| inv-horse | Horses — riding animal bred | absent |  |  |  | INHERITED BY THE CADASTRE (Task 2, 2026-09-12): this item's sole prerequisite `inv-animal-dom` is itself now inherited — the closed 301-item population restores `inv-animal-dom`'s real prerequisite `inv-biped` (`absent`, the population's sole root) — so this item is INHERITED rather than chosen under the family's chosen/inherited rule, and its former `disclosure` (which explained why it was chosen rather than a root) is deleted. ITS PREREQUISITE IS `deferred` AND THIS ITEM IS STILL `absent`, which is the weakest-demand discipline doing its job rather than a contradiction. `inv-animal-dom` has a registry row behind it; a riding animal has none, and the gap is two-layered: there is NO EQUINE at all among the 39 `*-kind` rows -- the roster runs to `giant-elk-kind`, `giant-goat-kind`, `rhinoceros-kind` and `woolly-mammoth-kind` alongside `owlbear-kind`, `otyugh-kind` and `xorn-kind` -- so even the wild ancestor the catalogue's chain needs is missing, and `BIO-animal-domestication` names the domestication half without naming that. `inapplicable` was considered and refused: the absence of an equine reads as a roster that grew on demand, not as a world deliberately lacking a precondition. Swept against `BIO-8`: it names domestication and agriculture without naming a mount, and `BIO-animal-domestication`, which sharpens it, is already cited on this item's prerequisite. Swept against `TECH-2`: its ladder is pottery, storage, smelting and metallurgy and names no animal at all. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): no row names a riding animal, and `BIO-breed-as-kind` is `rejected` ('do not mint a -kind per working-animal breed'), so the nearest neighbouring idea is a closed question rather than a plan. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: this item is CHOSEN rather than inherited -- its sole prerequisite `inv-animal-dom` is `deferred`, so nothing upstream forces the `absent` -- and `inapplicable` was additionally considered and refused for it, and the refusal was authored by a session that had read the model. A known limit of the argument, recorded rather than repaired: it reasons about the catalogue's INSTANCE -- no equine among the 39 `*-kind` rows -- where this corpus elsewhere scores the CAPABILITY." |
| inv-fermentation | Fermentation | absent |  |  |  |  |
| inv-numbers | Number system | absent |  |  |  |  |
| inv-law | Code of Laws | absent |  |  |  |  |
| inv-medicine | Recorded medicine | absent |  |  |  |  |
| inv-alphabet | Phonetic alphabet — phonemic script | absent |  |  |  | INHERITED BY THE CADASTRE (Task 2, 2026-09-12): this item's sole prerequisite `inv-writing` is itself now inherited (its real prerequisite `inv-stone-tool` is restored by the closed 301-item population and scores `absent`), so this item is INHERITED rather than chosen under the family's chosen/inherited rule, and its former `disclosure` (which explained why it was chosen since fix round 1) is deleted. Downstream of `inv-writing`. The near-miss is sharper here than anywhere else in the arc and it cuts the other way: `domains/language` DOES hold a per-family phoneme inventory, a syllable law, and a typology bundle, so the analysis a phonemic script encodes is already computed -- see `book/src/reference/phonology.md`. What no world has is a people that has NOTICED it. The catalogue's claim is that someone discovered one sign per sound; Hornvale's phonology is the author's model of the language, held outside the world, and `Orthography` spells it for the reader rather than for the speakers. A capability the engine exercises on a people's behalf is not a capability the people holds. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1) AND NOTHING MOVES IT. `MAP-8`'s 'borrowed scripts as contact fossils' clause names a script being BORROWED, which presupposes one exists and is therefore the input-assumption case rather than a plan for `phonemic-script`; the row is cited on `inv-writing` for the demand it does plan. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND, AND CHOSEN SINCE THE FIX-ROUND-1 AMENDMENT (`inv-writing` is now `deferred`). The search that keeps it `absent`: `alphabet` returns one row in 1,779 (`SKY-25`, about presiding-belief selection); `MAP-8`'s borrowed-scripts clause was read in full and refused in the note as the input-assumption case. The near-miss the note already records -- that `domains/language` computes the phonemic analysis a script would encode while no people has noticed it -- was read from the source by a non-blind session." |
| inv-bridle | Bridle — animal guided by rein | absent |  |  |  | Downstream of `inv-horse`. No tack, no harness and no control relationship between a person and an animal exists; the only committed person-to-creature relations in the registry are perceptual and social (`care`, `custody`, `dependency`, `association`), all of which hold between people. |
| inv-monotheism | Monotheism | absent |  |  |  |  |
| inv-dye | Resistant dyes | absent |  |  |  |  |
| inv-sea-navigation | Sea navigation | absent |  |  |  |  |
| inv-steel | Steel | absent |  |  |  |  |
| inv-arch | Architectural arches | absent |  |  |  |  |
| inv-aqueduct | Aqueducts | absent |  |  |  |  |
| inv-saddle | Saddle — seated riding rig | absent |  |  |  | Downstream of `inv-bridle`. Nothing a people makes is modelled as a made thing with a use: the registry's manufactured objects (`anvil`, `loom`, `bench`, `brazier`, `bed`, `door`, `altar`) are scene furniture a possessed body stands beside, committed as `thing \| object`, never as a capability a community holds or loses. |
| inv-sundial | Improved sundials | absent |  |  |  |  |
| inv-zoo | Zoos | absent |  |  |  |  |
| inv-coin | Currency (Coins) | absent |  |  |  |  |
| inv-library | Libraries — collected holdings outliving their keepers | deferred | registry:MEM-4 |  |  | Downstream of `inv-literature`. The near-miss is an institutional one and it is real: `occ-function` commits Cult, "a shrine or temple seat", so a world already has a seat whose purpose outlives the individuals in it, and the history bake carries tenure across generations. What it does not carry is HOLDINGS -- a durable store of content, as against a durable store of food (community stores exist and are lost on closure). A library is the second kind of store and the engine models only the first. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-nation`, previously described here as outside the corpus. The closed 301-item population makes `nation` a real item in this corpus (inherited, `absent`), and this item's other prerequisite `inv-literature` is ALSO now inherited (from `inv-writing` / `inv-stone-tool`) — so this item is now INHERITED rather than chosen under the family's chosen/inherited rule, on either edge alone: its former `disclosure` (which explained why it was chosen since fix round 1) is deleted. AMENDED AFTER FREEZE (campaign ledger #17, fix round 1): `deferred` ON `MEM-4`. The note above names this item's demand exactly -- HOLDINGS, 'a durable store of content' -- and `MEM-4` ('Preservation as craft -- a conservator institution spends surplus specifically to *lower* the decay rate of chosen items; the active-voice complement to MEM-1's rot', `raw`) plans precisely that: an institution paying to keep chosen content from melting. THE ROW THAT SUPPLIES THE OTHER HALF IS `SOC-11`, NOT `MEM-5`, AND THIS SENTENCE SAID `MEM-5` FOR TWO FIX ROUNDS (corrected in fix round 3). 'A queen's court OUTLIVES its courtiers' is `SOC-11`'s clause (`book/src/frontier/idea-registry.md:451`); `MEM-5` contains no form of the word 'outlive' at all, and its lifecycle clause is the opposite emphasis -- verbatim, 'each with a MAP-7-style founding->neglect->death lifecycle', the institution's own MORTALITY rather than its persistence past its members. The original sentence quoted `MEM-5` with an ellipsis that bridged silently into a different row. HOW IT SURVIVED: the quotation reads plausibly and nobody diffed it against the row until the third review -- the same non-verification that produced this file's ordering defect, in a place where a single `grep outliv` would have settled it. Repo-wide, 'courtier' occurs in exactly two places: `SOC-11`'s row and this note. SO THE UNION IS `MEM-4` UNION `SOC-11` ('The institutional layer -- a mid-scale social structure between the community and the persona: a court, a bureaucracy, a guild, a temple hierarchy, a slave system, an army ... an institution is a persistent entity with *roles*, a *hierarchy*, and a *function*, cross-cutting communities (a trade guild spans ports; a queen's court outlives its courtiers)', `raw`). Both rows are `raw`, so the verdict is unaffected and the cited anchor `MEM-4` -- which genuinely plans the holdings half on its own -- does not move. `MEM-5` is left named here rather than deleted, because a corrected misquote should remain findable from the thing it got wrong. THE VERDICT WAS ONLY REACHABLE ONCE `inv-literature` MOVED: while that prerequisite was `absent` the weakest-demand rule fixed this item at `absent` however many rows named its demand, which is why these two amendments must be read together. FIX ROUND 2: NO SINGLE ROW NAMES THE WHOLE DEMAND, AND THE ANCHOR SHOULD NOT BE READ AS CLAIMING OTHERWISE. `collected-holdings-outliving-their-keepers` has two halves and they are discharged by `MEM-4` UNION `SOC-11`: `MEM-4` supplies the holdings (a conservator institution paying to lower the decay rate of chosen content) and `SOC-11` supplies the outliving ('a queen's court outlives its courtiers'). THIS SENTENCE SAID `MEM-5` WHEN IT WAS WRITTEN, INHERITING A MISQUOTE FROM THE ROUND BEFORE IT, which is why the correction above is stated at length rather than applied silently: a fix round that restates an unverified quotation propagates it. Family law's rule 2 asks whether THE row names the capability, and strictly neither does alone. The verdict is kept because both rows are `raw`, both plan deliverables, and between them they plan this item's own demand rather than a prerequisite of it -- but a reader weighing the rule literally is entitled to call this the weakest of the three post-freeze amendments on that ground, and the no-double-anchoring convention is what forces one row into the field and the other into this note. If rule 2 is later tightened to forbid a union, this item returns to `absent`. AND THE CORRECTION OPENS A CROSS-CORPUS TENSION THIS NOTE RECORDS RATHER THAN RESOLVES. `SOC-11` is the row the sibling column `henrich-2004-extended` REFUSES for `col-palace-accounting`, on the cut that an institution with roles and a hierarchy is not the record it keeps -- and here `SOC-11` is credited with supplying half of a demand about holdings that outlive their keepers. THE TWO COLUMNS THEREFORE APPLY THAT CUT IN OPPOSITE DIRECTIONS ON ONE ROW. The honest reading of why is that the halves differ: there the demand is the ACT of accounting, which an institution can exist without performing, while here the demand's second half is institutional PERSISTENCE, which is what `SOC-11` plans. Whether that distinction survives scrutiny is not settled, and settling it by moving a verdict is forbidden here -- it is carried as a campaign follow-up. It is recorded because a matrix read ACROSS its columns is exactly where a cut applied two ways does its damage, and neither column can show it alone. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND, AND CHOSEN SINCE THE FIX-ROUND-1 AMENDMENT, which is the whole point of this disclosure: before it, `inv-literature` was `absent` and the weakest-demand rule fixed this item's verdict whatever the registry said, so no search could have moved it. Once the prerequisite moved, `MEM-4` and `MEM-5` -- both found by reading all 1,779 openings and then both rows in full -- became load-bearing. The dependency between the two amendments is stated in the note rather than left for a reader to reconstruct." |
| inv-eclipse | Solar eclipse predicted | absent |  |  |  |  |
| inv-water-element | Water as element | absent |  |  |  |  |
| inv-irrational-numbers | Irrational numbers | absent |  |  |  |  |
| inv-realistic-maps | Realistic maps | absent |  |  |  |  |
| inv-abacus | Abacus | absent |  |  |  |  |
| inv-cadaver | Human dissection | absent |  |  |  |  |
| inv-ocean-navigation | Ocean navigation | absent |  |  |  |  |
| inv-venus-named | Venus named | absent |  |  |  |  |
| inv-dream-interpretation | Dream interpretation | absent |  |  |  |  |
| inv-atom | Atoms | absent |  |  |  |  |
| inv-epilepsy | Epilepsy | absent |  |  |  |  |
| inv-catapult | Catapult | absent |  |  |  |  |
| inv-university | Advanced schools | absent |  |  |  |  |
| inv-animal-classification | Animal classification | absent |  |  |  |  |
| inv-five-elements | Five elements theorized | absent |  |  |  |  |
| inv-heliocentric-theory | Non-geocentric theory | absent |  |  |  |  |
| inv-logic | Logic | absent |  |  |  |  |
| inv-spherical-earth | Spherical earth theory | absent |  |  |  |  |
| inv-star-maps | Star maps | absent |  |  |  |  |
| inv-botany | Botany book | absent |  |  |  |  |
| inv-paved-road | Paved roads | absent |  |  |  |  |
| inv-arteries-veins | Arteries vs veins | absent |  |  |  |  |
| inv-geometry | Geometry | absent |  |  |  |  |
| inv-tides | Tides | absent |  |  |  |  |
| inv-brain-areas | Parts of brain | absent |  |  |  |  |
| inv-lighthouse | Lighthouses | absent |  |  |  |  |
| inv-moon-sun-size | Moon and sun size estimate | absent |  |  |  |  |
| inv-water-clock | Water clocks | absent |  |  |  |  |
| inv-lever-math | Lever mathematics | absent |  |  |  |  |
| inv-earth-size | Earth size estimate | absent |  |  |  |  |
| inv-year-number | Standardized years | absent |  |  |  |  |
| inv-great-wall | Great Wall | absent |  |  |  |  |
| inv-parchment | Parchment — hide writing surface | absent |  |  |  | INHERITED BY THE CADASTRE (Task 2, 2026-09-12): this item's sole prerequisite `inv-animal-dom` is itself now inherited (its real prerequisite `inv-biped` is restored by the closed 301-item population and scores `absent`), so this item is INHERITED rather than chosen under the family's chosen/inherited rule, and its former `disclosure` (which explained why it was chosen rather than a root — campaign ledger #13's own worked example) is deleted. The `presupposes` edge this note discusses and refused to add is unaffected: still just `inv-animal-dom`, unchanged by The Cadastre. Downstream of `inv-animal-dom`, which is `deferred`, so NOTHING UPSTREAM FORCES THIS `absent` and the verdict is the item's own -- which is why it carries a disclosure and a sweep where its neighbours in the arc do not. The checkable argument, and the whole of it: no material is committed as coming off a creature at all. `blood` and `bone` are registered as substances in the scene vocabulary, a possessed body's perceptual nouns, not as yields a people takes and works; there is no hide, no tanning and no made surface anywhere in the engine. A SECOND HALF OF THE ARGUMENT WAS WITHDRAWN RATHER THAN REPAIRED: an earlier draft also argued that parchment needs a written record, and `inv-writing` IS NOT IN THIS ITEM'S `presupposes` CLOSURE -- the catalogue builds parchment on `animal-dom` alone -- so that half rested on a lattice edge nothing in this file draws and nothing could check (decision 0386 derives the demand set from these edges only). The edge was NOT added: every `presupposes` edge in this corpus is transcribed from the catalogue's own "Built on" list, and authoring one to support an argument would make the lattice partly mine and break the property that every edge is checkable against the source. Adding it would also have put an `absent` item in this closure and so removed this item's disclosure obligation altogether -- a repair that erases the finding, and not the reason the edge was refused. SWEPT against `BIO-8`, which names nightsoil -- an animal by-product, but as an INPUT TO AGRICULTURE rather than a material a people works, and its domestication half reaches this item only through its prerequisite, where it is already cited; and against `TECH-2`, whose ladder is pottery, storage, smelting and metallurgy and names no hide, surface or written thing. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): no row names a writing surface, and the one row naming a tanner (`CUL-12`) plans a purity taboo attached to that rung rather than the preparation of a hide -- the same refusal the sibling corpus records for `col-tanning`. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND, and this item is the reason the disclosure rule is stated as CHOSEN rather than as ROOT (campaign ledger #13): its sole prerequisite is `deferred`, so the lattice does not force its verdict, yet it is not a root and an earlier round's check missed it on exactly that difference. The `absent` rests on a search of the registry and the concept vocabulary run by a session that had read the model. Audit it at the substance vocabulary: if `blood` and `bone` being registered is read as a people working animal material, this item moves." |
| inv-moon-distance | Distance to moon estimate | absent |  |  |  |  |
| inv-star-maps-better | Better star maps | absent |  |  |  |  |
| inv-glass-blowing | Glass blowing | absent |  |  |  |  |
| inv-wooden-stirrup | Wooden stirrups — foot support in the saddle | absent |  |  |  | Downstream of `inv-saddle`. Included whole because the arc links it and the selection rule forbids dropping an item; nothing in the corpus builds on it, so it is a leaf of the catalogue's own chain and contributes one demand that no other item's closure inherits. |
| inv-water-wheel | Waterwheels | absent |  |  |  |  |
| inv-julian-calendar | Julian calendar | absent |  |  |  |  |
| inv-climactic-zone | Climactic zones | absent |  |  |  |  |
| inv-basic-steam-engine | Rudimentary steam motion — steam moves a mechanism | absent |  |  |  | AT THE PREDECESSOR'S 41-ITEM POPULATION THIS OPENING SENTENCE WAS TRUE; THE CADASTRE'S CLOSURE HAS MADE IT FALSE, AND IT MUST BE READ HISTORICALLY (see the amendment below): "A root of the steam arc in this corpus, so its verdict is chosen rather than inherited." Hornvale has fire (`fire` is a registered substance and `brazier` a scene object) and it has no mechanism, no work, and no energy accounting anywhere: the only place a physical quantity is converted into an effect on a people is `tech_weight`, which multiplies population and is a scalar on a four-rung clock. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-fire`, previously described here as outside the corpus and as the one dropped prerequisite Hornvale would actually have satisfied. The closed 301-item population makes `fire` a real item in this corpus, scored `absent` as a Task 2 placeholder pending Task 3's own search (Hornvale's registered `fire` substance and `brazier` scene object may well move it to `present`) — but until Task 3 runs that search, `inv-fire` scores `absent` and this item is INHERITED rather than chosen under the family's chosen/inherited rule: its former `disclosure` (which called it a root) is deleted. Swept against `TECH-2`: its ladder ends at metallurgy and names no mechanism, no work and no engine. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `TECH-4` is the only row in which the word steam appears, and it appears as an ANALOGY for electricity ('an instance of the ladder like steam'), which is a comparison rather than a plan. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. The verdict rests on knowing `tech_weight` to be the engine's only conversion of a physical quantity into an effect on a people. Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list." |
| inv-medicinal-plants | Recorded medicinal plants | absent |  |  |  |  |
| inv-paper | Paper — cheap pulped writing surface | absent |  |  |  | Downstream of `inv-parchment`. The demand that distinguishes it from its prerequisite is CHEAPNESS -- a surface abundant enough to change who writes -- and cost is the axis Hornvale most completely lacks: `ECON-livelihood` is `raw` and its row records that "no economy domain exists yet". Nothing in any world has a price. |
| inv-geocentric-universe | Geocentric universe | absent |  |  |  |  |
| inv-spinal-cord | Spinal cord | absent |  |  |  |  |
| inv-algebra | Algebra | absent |  |  |  |  |
| inv-tea | Tea | absent |  |  |  |  |
| inv-alchemy | Recorded alchemy | absent |  |  |  |  |
| inv-metal-stirrup | Metal stirrups — load bearing metal stirrup | absent |  |  |  | Downstream of `inv-horse`. The metalworking half is the more interesting absence and it is the `anvil` case exactly: the registry carries `anvil`, "a heavy iron block a smith hammers metal against", as a scene object, so a possessed body can stand next to a smith's anvil in a world where no people holds smithing. `occ-tech`'s rungs NAME bronze-working and iron-working, which is the closest the engine comes -- and a rung is a date-derived label on a community, not a capability it acquired. PREREQUISITE DROPPED: built on `steel` in the catalogue, outside the corpus. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through `steel`, which is a DROPPED PREREQUISITE rather than this item's own demand (a load-bearing fitting), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-wheelbarrow | Wheelbarrows | absent |  |  |  |  |
| inv-dome | Architectural domes | absent |  |  |  |  |
| inv-silk-europe | Silk | absent |  |  |  |  |
| inv-turnplow | Turnplows — traction tillage of heavy soil | deferred | registry:BIO-8 |  |  | RE-SCORED FROM `absent` BY THE REGISTRY SWEEP, and the original verdict is the instance that motivated the sweep. `BIO-8` ("Domestication & agriculture -- the culture layer harnessing biosphere fields; staple crop per biome, the Boserup plough (SOC-2's input), nightsoil (BIO-5), famine as paleoclimate (MAP-6) x carrying capacity (MAP-7)" (`raw`) names THE BOSERUP PLOUGH, which is this item's demand under its own name, and the first draft of this corpus cited the row NOWHERE -- while citing `BIO-animal-domestication`, whose first two words are "Sharpens [[BIO-8]]". READ HISTORICALLY -- TRUE OF THE PREDECESSOR'S 41-ITEM POPULATION, FALSE OF THIS ONE SINCE THE CADASTRE'S CLOSURE (see the amendment below): "A root scored `absent` with nothing upstream forcing it is the verdict most in need of a search, and it got the least." WHAT `deferred` MEANS IS UNBUILT, and today's state is a near-miss: `subsistence: Farming` ("Settled agriculture") is committed per settlement and varies, `occ-function` commits Agrarian, and the `Crop` model says which staple a place supports -- so a world knows that a people farms and what grows there, and nothing anywhere says HOW the ground is worked. `occ-function`'s Agrarian also CONFLATES farming with herding in one label, so no world holds the one without the other. THE REFUSAL THAT WAS TESTED AND FAILED: `BIO-8` frames the plough as "SOC-2's input", which looks like the ground on which this corpus refuses `NARR-monument-writes-itself` for `inv-writing` -- a capability a row ASSUMES rather than delivers. It does not hold here. The plough is one of `BIO-8`'s own enumerated elements and the parenthetical names where its output ROUTES, not a precondition it presumes; and another element of the same list, "staple crop per biome", has already SHIPPED as `domains/climate/src/crops.rs`, which settles that `BIO-8` enumerates deliverables. PREREQUISITES RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-plow` and `inv-steel`, previously described here as outside the corpus. The closed 301-item population makes both real items in this corpus (`inv-plow` `absent`; `inv-steel` inherited, `absent`, from `bronze`/`copper`/`fire`/`stone-tool` further back), so this item is now INHERITED rather than chosen under the family's chosen/inherited rule: its former `disclosure` (which called it a root and explained the inverted-disclosure-rule episode) is deleted. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. THIS IS THE ITEM THE INVERTED DISCLOSURE RULE EXISTS FOR: the first draft scored it `absent`, in the direction that flattered the campaign's thesis, and carried no disclosure inviting anyone to audit the search (campaign ledger #12). Its SELECTION was blind: it is in the `knights` arc's own link list." |
| inv-greek-fire | Greek fire | absent |  |  |  |  |
| inv-block-printing | Block printing — text reproduced from a carved form | absent |  |  |  | Downstream of `inv-paper`. Reproduction-without-re-authoring has no analogue in the engine, and the transmission layer is built on the opposite assumption: every retelling degrades, by design (`KNOW-boundary-not-accumulation`). A carved form that makes the hundredth copy identical to the first is a mechanism that would CONTRADICT shipped physics rather than extend it, which is a more interesting `absent` than a missing predicate. |
| inv-porcelain | Porcelain | absent |  |  |  |  |
| inv-acetic-acid | Acetic acid | absent |  |  |  |  |
| inv-high-backed-saddle | High-backed saddle — braced saddle transmits shock | absent |  |  |  | Downstream of two `absent` items. The demand is mechanical -- a rig that transmits an impact into a frame rather than into the rider -- and the engine has no force, no impact and no body mechanics at any scale; combat between peoples resolves as a comparison of population times `tech_weight`. |
| inv-iron-horseshoes | Iron horseshoes — shod draught animal | absent |  |  |  | Downstream of `inv-horse`. What makes a horseshoe a capability rather than an object is that it extends an animal's WORKING LIFE on hard ground -- a durability term on a productive asset. Hornvale has no productive assets and no wear on anything a people owns; the only durability modelled is a community's stores, which decay at a flat per-epoch rate and are destroyed on closure. PREREQUISITE DROPPED: built on `steel`, outside the corpus. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through `steel`, which is a DROPPED PREREQUISITE rather than this item's own demand (a durability term on a working animal), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-couched-lance | Couched lance — mounted shock charge | absent |  |  |  | Downstream of two `absent` items. Worth a note anyway because it names the thing `tech_weight` is standing in for: a tactic, held by some peoples and not others, that changes the outcome of a meeting between them. Hornvale's answer is a single scalar multiplier on population, 1.0/1.5/2.25/3.0 by rung -- which spec finding F5 identifies as "the progress scalar ... a civilisation with a *level*" that `book/src/frontier/frontier.md` rejects by name. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through the lance-head's metal, which is a DROPPED PREREQUISITE rather than this item's own demand (a mounted shock charge, which is a tactic), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-zero | Zero | absent |  |  |  |  |
| inv-iceland-settled | Iceland settled | absent |  |  |  |  |
| inv-coffee | Coffee | absent |  |  |  |  |
| inv-arctic-circle | Arctic circle | absent |  |  |  |  |
| inv-horse-collar | Horse collars — harness transmits animal draught | absent |  |  |  | Downstream of two `absent` items. The catalogue's own chain is the interesting part: it builds the collar on the PLOUGH and the HORSESHOE rather than on the horse, i.e. on a use and a durability, which is a capability-shaped dependency rather than an artifact-shaped one. Nothing in Hornvale composes capabilities that way; `tech_for` is monotone in a year and has no inputs. |
| inv-greenland-viking | Greenland | absent |  |  |  |  |
| inv-newfoundland | Newfoundland discovered | absent |  |  |  |  |
| inv-optics | Optics | absent |  |  |  |  |
| inv-crossbow | Crossbows | absent |  |  |  |  |
| inv-new-star | New star | absent |  |  |  |  |
| inv-bright-comet | Bright comets | absent |  |  |  |  |
| inv-fork | Forks | absent |  |  |  |  |
| inv-flying-buttress | Flying buttresses | absent |  |  |  |  |
| inv-compass | Magnetic navigation | absent |  |  |  |  |
| inv-windmill | Windmills | absent |  |  |  |  |
| inv-spitsbergen | Spitsbergen | absent |  |  |  |  |
| inv-arab-numbers | Arabic numerals | absent |  |  |  |  |
| inv-coal-mining | Coal mining — mineral fuel extracted | deferred | registry:TECH-3 |  |  | AT THE PREDECESSOR'S 41-ITEM POPULATION THIS OPENING SENTENCE WAS TRUE; THE CADASTRE'S CLOSURE HAS MADE IT FALSE, AND IT MUST BE READ HISTORICALLY (see the amendment below): "A root, and the only item in the steam arc that is not `absent`." Hornvale genuinely mines: `occ-function` commits Mine, "Extraction -- ore, stone, salt", `occ-delve-depth` commits "how far below its seat the occupation drove a working, in metres", both vary between occupations in one world, and ore deposits themselves shipped with The Lode (2026-07-22). `TECH-3` records precisely what is left: "Ore deposits shipped (The Lode, 2026-07-22); mining/refinement/tech-gating still deferred (the event-features + economy rungs)" -- the row is `raw`. What this item's demand adds beyond the shipped half is that the mineral is a FUEL: something extracted to be burnt, which is what makes coal a prerequisite of an engine rather than a commodity. Hornvale's mines yield no named mineral at all, only a depth. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-steel`, previously described here as outside the corpus. The closed 301-item population makes `steel` a real item in this corpus (inherited, `absent`, from `bronze`/`copper`/`fire`/`stone-tool` further back), so this item is now INHERITED rather than chosen under the family's chosen/inherited rule: its former `disclosure` (which called it a root) is deleted. Swept against `TECH-2`, whose "kiln temperature gates smelting" is downstream of extraction rather than naming it; `TECH-3` names mining itself and is the anchor. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. `TECH-3` was found by searching the idea registry, by a session that had already read `occ-delve-depth` and knew the shipped half existed. Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list." |
| inv-rudder | Rudders | absent |  |  |  |  |
| inv-eyeglass | Eyeglasses invented | absent |  |  |  |  |
| inv-gunpowder | Gunpowder | absent |  |  |  |  |
| inv-planetary-tables | Planetary tables | absent |  |  |  |  |
| inv-magnetic-pole | Magnetic poles | absent |  |  |  |  |
| inv-mirror | Mirrors | absent |  |  |  |  |
| inv-far-east | Far east | absent |  |  |  |  |
| inv-longbow | Longbows — massed missile volley | absent |  |  |  | AT THE PREDECESSOR'S 41-ITEM POPULATION THIS OPENING SENTENCE WAS TRUE; THE CADASTRE'S CLOSURE HAS MADE IT FALSE, AND IT MUST BE READ HISTORICALLY (see the amendment below): "A root, so the verdict is chosen." The demand is a capability whose value depends on being held COLLECTIVELY -- a volley, not an archer -- and the engine has no formation, no tactic and no per-individual contribution to a collective outcome. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-crossbow`, previously described here as outside the corpus. The closed 301-item population makes `crossbow` a real item in this corpus (inherited, `absent`, from a deep chain running back through `bow`/`lever-math`/`logic`/`geometry`/`university`/`numbers`/`nation`/`agriculture`/`bronze`/`copper`/`fire`/`stone-tool`/`biped`), so this item is now INHERITED rather than chosen under the family's chosen/inherited rule: its former `disclosure` (which called it a root) is deleted. TECH-2 REFUSED: its "metallurgy yields ... weapons" names neither a bow -- which is wood and sinew, not metal -- nor this item's actual demand, which is explicitly COLLECTIVE (a volley, not an archer). Swept against `BIO-8`: nothing. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `EXP-8` ('Martial traditions as culture, not choreography -- embodied combat practice fused with dance, ritual, and belief ... arising downstream of disarmament under coercion', `elaborated`) is the only row that treats fighting as a transmitted practice, and it is refused twice over: its own text says 'the payload is the tradition and its significance, not the moves', and its generative premise is a people DENIED weapons, which is the opposite of a massed volley. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`.  Its SELECTION was blind: it is in the `knights` arc's own link list." |
| inv-spinning-wheel | Spinning wheels | absent |  |  |  |  |
| inv-distillation | Liquor distillation | absent |  |  |  |  |
| inv-pike | Pike — massed polearm formation | absent |  |  |  | Downstream of `inv-couched-lance`. The catalogue puts the pike after the lance because it is an ANSWER to it -- a counter a people adopts because a neighbour has something. That is the shape spec finding F3 found missing: section 5.3 of the Living Community spec justified committing the tech horizon on the grounds that "a people's trajectory is globally dependent (contact, displacement)", and `tech_offset` is drawn once at genesis, inherited down the lineage, and never mutated. No people in any world has ever changed what it can do because of a neighbour. TECH-2 REFUSED: the pyrotechnology row's "metallurgy yields tools, weapons" reaches this item only through the pike-head's metal, which is a DROPPED PREREQUISITE rather than this item's own demand (a massed formation), and a row that plans a prerequisite does not discharge the demand -- see `provenance`. |
| inv-sulfuric-acid | Sulfuric acid | absent |  |  |  |  |
| inv-comet-painting | Realistic comets | absent |  |  |  |  |
| inv-canary-islands | Canary islands | absent |  |  |  |  |
| inv-anatomy | Anatomy book | absent |  |  |  |  |
| inv-mechanical-clock | Mechanical clocks | absent |  |  |  |  |
| inv-cannon | Cannons | absent |  |  |  |  |
| inv-quarantine | Quarantine | absent |  |  |  |  |
| inv-indian-ocean | Indian ocean | absent |  |  |  |  |
| inv-madeira | Madeira settled | absent |  |  |  |  |
| inv-azores | Azores | absent |  |  |  |  |
| inv-perspective | Perspective drawing | absent |  |  |  |  |
| inv-artillery | Artillery | absent |  |  |  |  |
| inv-arquebus | Arquebus — handheld firearm | absent |  |  |  | AT THE PREDECESSOR'S 41-ITEM POPULATION THIS OPENING SENTENCE WAS TRUE; THE CADASTRE'S CLOSURE HAS MADE IT FALSE, AND IT MUST BE READ HISTORICALLY (see the amendment below): "A root, so the verdict is chosen, and this is the clearest case for the ceiling ruling stated in `provenance`." `TechHorizon`'s own doc calls Classical "the ceiling this engine models", so a firearm is past the end of the enum. It is scored `absent` rather than `inapplicable` or `refused` because spec finding F1 established that NO decision record covers the four-rung horizon -- its whole rationale is one paragraph, fact #7 of 12, in a spec about ruins -- and an unratified code fact is not a deliberate world choice. PREREQUISITES RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-artillery` and `inv-crossbow`, previously described here as outside the corpus. The closed 301-item population makes both real items in this corpus (both inherited, `absent`, `inv-artillery` via `cannon`/`gunpowder`/`paper`/`parchment`/... and `inv-crossbow` via the chain noted on `inv-longbow`), so this item is now INHERITED rather than chosen under the family's chosen/inherited rule: its former `disclosure` (which called it a root and authored the ceiling ruling) is deleted; the ceiling ruling itself is unaffected and stands in this note. TECH-2 REFUSED, and for a root this is the refusal most worth auditing: the row's "metallurgy yields tools, weapons" supplies a barrel's MATERIAL, and this item's own demand is a chemical propellant in a tube. `TECH-2`'s ladder runs pottery -> kiln -> smelting -> metallurgy and contains no combustion-as-propellant at any rung; no row in the idea registry mentions gunpowder at all. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): `firearm` and `gunpowder` return ZERO rows across all 1,779, and `EXP-8` is refused for the reasons recorded on `inv-longbow`. The sibling corpus's `col-firearms` reached the same null independently. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. Both the ceiling ruling and the `TECH-2` refusal were authored here rather than inherited. Its SELECTION was blind: it is in the `knights` arc's own link list." |
| inv-standing-army | Standing army — permanent force apart from population | absent |  |  |  | Downstream of two `absent` items, and the item whose demand Hornvale contradicts most exactly. The near-miss is `occ-function: Fort`, "a garrisoned defensive point", committed and varying. But the demand is a force MAINTAINED APART FROM the population that feeds it, and `Bake::strength` and `roller_strength` both compute strength AS population times `tech_weight` -- so a people's fighting power is its headcount by construction, and an army cannot be distinguished from the people it is drawn from even in principle. This is the one item where `absent` understates the finding: the demand is not unmet, it is unrepresentable without changing a formula that raiding, delving depth and residue structures all read. SWEPT AGAINST THE WHOLE REGISTRY (fix round 1) AND ONE ROW NAMES THIS ITEM'S DEMAND, found by the reviewer's own probes rather than by that sweep (fix round 2): `SOC-11` ('The institutional layer -- a mid-scale social structure between the community and the persona: a court, a bureaucracy, a guild, a temple hierarchy, a slave system, AN ARMY', `raw`) names an army among its own enumerated elements, which is `permanent-force-apart-from-population`. IT CHANGES NO VERDICT TODAY, and the reason is the weakest-demand rule: this item's prerequisites `inv-arquebus` and `inv-pike` are both `absent`, so its verdict is inherited and a row naming its own demand cannot raise it -- which is also why it carries no `disclosure`. It is recorded because it becomes LOAD-BEARING the moment either prerequisite moves, and an item whose note is silent about a row that would then decide it is the shape this campaign keeps producing. The sibling corpus read and refused `SOC-11` for `col-tanning` and `col-palace-accounting` in the same round, on the distinction that an institution with roles is not the record it keeps; an ARMY is different, because here the institution IS the demand. |
| inv-concave-lenses | Concave lenses | absent |  |  |  |  |
| inv-printing-press | Printing press — movable type mass reproduction | absent |  |  |  | Downstream of two `absent` items, and past the modelled ceiling. PREREQUISITE DROPPED: built on `steel`, outside the corpus. Recorded without further near-miss analysis because it inherits `inv-block-printing`'s contradiction with the transmission layer and `inv-paper`'s missing cost axis, and restating them here would duplicate two notes rather than add one. TECH-2 REFUSED: the row's metallurgy would supply cast type's MATERIAL -- the dropped `steel` -- and this item's own demand is mass reproduction. |
| inv-comet-tracking | Comet trajectory tracking | absent |  |  |  |  |
| inv-cape-good-hope | Cape of Good Hope | absent |  |  |  |  |
| inv-magnetic-declination | Magnetic declination | absent |  |  |  |  |
| inv-new-world | New world | absent |  |  |  |  |
| inv-syphilis | Syphilis | absent |  |  |  |  |
| inv-india-water-route | India water route | absent |  |  |  |  |
| inv-america | America | absent |  |  |  |  |
| inv-hand-watch | Spring-powered watches | absent |  |  |  |  |
| inv-pacific-ocean | Pacific ocean | absent |  |  |  |  |
| inv-florida | Florida | absent |  |  |  |  |
| inv-mexico | Mexico conquered | absent |  |  |  |  |
| inv-around-earth | Earth circumnavigated | absent |  |  |  |  |
| inv-peru | Peru conquered | absent |  |  |  |  |
| inv-cubic-equations | Cubic equations | absent |  |  |  |  |
| inv-comet-tails | Comet tails | absent |  |  |  |  |
| inv-mississipi-river | Mississipi river | absent |  |  |  |  |
| inv-amazon-river | Amazon river | absent |  |  |  |  |
| inv-heliocentric-practice | Math of heliocentricity | absent |  |  |  |  |
| inv-illustrated-anatomy | Illustrated anatomy book | absent |  |  |  |  |
| inv-negative-numbers | Negative numbers | absent |  |  |  |  |
| inv-surgery | Rational surgery | absent |  |  |  |  |
| inv-planetary-tables-better | Better planetary tables | absent |  |  |  |  |
| inv-trig-tables | Trigonometric tables | absent |  |  |  |  |
| inv-eustacian-tubes | Eustacian tubes | absent |  |  |  |  |
| inv-northeast-passage | Northeastern passage | absent |  |  |  |  |
| inv-homologies | Vertebrate skeletons | absent |  |  |  |  |
| inv-mineralogy | Mining book | absent |  |  |  |  |
| inv-tobacco | Tobacco | absent |  |  |  |  |
| inv-scientific-societies | Scientific societies — corresponding body of inquirers | absent |  |  |  | Downstream of `inv-printing-press`. Its demand is the most explicitly social-epistemic in the corpus -- a durable body whose members correspond -- and Hornvale's knowledge layer is the part of the engine with the most shipped machinery, which makes the miss specific rather than broad: transmission is modelled between INDIVIDUALS who meet, and `SOC-information-economy` (`raw`) proposes the strategic layer above it. An institution that outlives its members and transmits deliberately at a distance is neither. PREREQUISITE DROPPED: built on `university`, outside the corpus. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1); two rows were read in full and both refused. `MEM-5` names an institution's cargoes as memory, legitimacy, exchange and skill -- not inquiry. `LANG-49` ('The epistemic arc -- do cultures ever discover proto-mathematics, proto-philosophy, or proto-medicine as EMERGENT reasoning capabilities') plans the CAPACITY to discover and says of itself that it is 'named as a direction, not designed'; a row planning the capacity to inquire is a prerequisite of a corresponding body of inquirers, not the body. The verdict is inherited through `inv-printing-press` in any case. |
| inv-musket | Musket | absent |  |  |  |  |
| inv-world-maps | Mercator projection | absent |  |  |  |  |
| inv-supernova | Supernova | absent |  |  |  |  |
| inv-greenland | Greenland revisited | absent |  |  |  |  |
| inv-comet-distance | Comet distances | absent |  |  |  |  |
| inv-drake-strait | Drake Strait | absent |  |  |  |  |
| inv-pendulum | Pendulums for time | absent |  |  |  |  |
| inv-siberia | Siberia settled | absent |  |  |  |  |
| inv-gregorian-calendar | Gregorian calendar | absent |  |  |  |  |
| inv-hydrostatics | Hydrostatics — quantitative law of fluids known | absent |  |  |  | AT THE PREDECESSOR'S 41-ITEM POPULATION THIS OPENING SENTENCE WAS TRUE; THE CADASTRE'S CLOSURE HAS MADE IT FALSE, AND IT MUST BE READ HISTORICALLY (see the amendment below): "A root, so the verdict is chosen, and it is the representative case for all six discovery items in the steam arc." Hornvale's knowledge model holds PARTICULAR FACTS -- where a thing is, who did what, what was seen -- and carries them with provenance, degradation and belief. A law of nature is a different object: a general claim, true of cases never observed, whose value is that it predicts. `KNOW-study` (`raw`) is the nearest row and it proposes measuring how much of a world's knowable TRUTH a walker covers, which is the particular kind again. No world can hold a general claim, correct or mistaken, so no people can hold or lose one. PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-geometry`, previously described here as outside the corpus. The closed 301-item population makes `geometry` a real item in this corpus (inherited, `absent`, from `logic`/`nation`/`numbers`/`university`/`agriculture`/`stone-tool`/`biped`), so this item is now INHERITED rather than chosen under the family's chosen/inherited rule: its former `disclosure` (which called it a root and said five other items lean on its argument) is deleted — those five items' own text is unaffected by this correction. Swept against `BIO-8` and `TECH-2`: neither names natural philosophy, and the `KNOW-*` program is about particular facts and their transmission. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1). `LANG-49` is the one row that plans a culture discovering anything, and it plans the CAPACITY ('does a culture ever notice a repeatable cause-and-effect regularity'), never a law; its status is partly `shipped` and its own text calls it a direction rather than a design. A row planning the capacity to discover does not discharge an item whose demand is a specific discovery -- the same cut this file applies to `TECH-2` and the metal items. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`. This is the verdict the corpus leans on hardest -- five other items cite it for the general argument -- and it rests on a reading of the whole `KNOW-*` program, so a reader who finds a row proposing general-claim knowledge moves six items at once. Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list." |
| inv-decimal-notation | Positional notation | absent |  |  |  |  |
| inv-cryptanalysis | Code breaking | absent |  |  |  |  |
| inv-falling-motion | Falling motion | absent |  |  |  |  |
| inv-stocking-frame | Stocking frame | absent |  |  |  |  |
| inv-microscope | Microscope | absent |  |  |  |  |
| inv-algebraic-symbol | Algebraic symbols | absent |  |  |  |  |
| inv-thermometer | Thermometer | absent |  |  |  |  |
| inv-east-indies | East indies settled | absent |  |  |  |  |
| inv-pi-accuracy | PI accuracy | absent |  |  |  |  |
| inv-chemistry-textbook | Chemistry book | absent |  |  |  |  |
| inv-earth-magnet | Earth as giant magnet | absent |  |  |  |  |
| inv-vein-valves | Vein valves | absent |  |  |  |  |
| inv-english-settlement-america | English America | absent |  |  |  |  |
| inv-french-settlement-america | French America | absent |  |  |  |  |
| inv-telescope | Telescope | absent |  |  |  |  |
| inv-elliptical-orbits | Elliptical orbits | absent |  |  |  |  |
| inv-milky-way | Milky Way | absent |  |  |  |  |
| inv-moon | Moon mountains | absent |  |  |  |  |
| inv-jupiter-moons | Jupiter&#39;s four moons | absent |  |  |  |  |
| inv-sunspots | Sunspots | absent |  |  |  |  |
| inv-venus-phases | Venus phases | absent |  |  |  |  |
| inv-andromeda-nebula | Andromeda Nebula | absent |  |  |  |  |
| inv-logarithms | Logarithms | absent |  |  |  |  |
| inv-metabolism | Biological metabolism | absent |  |  |  |  |
| inv-baffin-bay | Baffin Bay | absent |  |  |  |  |
| inv-tierra-del-fuego | Tierra Del Fuego | absent |  |  |  |  |
| inv-scientific-method | Scientific method | absent |  |  |  |  |
| inv-stagecoach | Stagecoaches | absent |  |  |  |  |
| inv-refraction | Light refraction | absent |  |  |  |  |
| inv-slide-rules | Slide rules | absent |  |  |  |  |
| inv-gas | Gas state — matter recognised in a third state | absent |  |  |  | Downstream of `inv-hydrostatics`; a discovery item, see that item's note for the general argument. The engine's substances (`fire`, `earth`, `blood`, `bone`, `water`) are scene vocabulary for what a body can perceive and name, with no state, phase or quantity behind them. PREREQUISITE DROPPED: built on `alchemy`, outside the corpus. |
| inv-aurochs-extinct | Aurochs extinction | absent |  |  |  |  |
| inv-planetary-tables-even-better | Even better planetary tables | absent |  |  |  |  |
| inv-blood-circulation | Blood circulation | absent |  |  |  |  |
| inv-science-vs-religion | Science religion debates | absent |  |  |  |  |
| inv-magnetic-declination-variation | Shifting magnetic declination | absent |  |  |  |  |
| inv-analytic-geometry | Cartesian geometry | absent |  |  |  |  |
| inv-fermat-last-theorem | Fermat&#39;s last theorem | absent |  |  |  |  |
| inv-cross-hairs | Cross hairs | absent |  |  |  |  |
| inv-adding-machine | Mechanical calculators | absent |  |  |  |  |
| inv-quinine | Quinine | absent |  |  |  |  |
| inv-south-pacific | New Zealand and Tasmania | absent |  |  |  |  |
| inv-barometer | Barometers — atmospheric pressure measured | absent |  |  |  | Downstream of `inv-gas`. Distinct from the discovery items in one respect worth recording: its demand is an INSTRUMENT, a made thing that yields a number a people then reasons with. Hornvale has climate fields with real values at every point and no way for anyone inside the world to read one. PREREQUISITE DROPPED: built on `falling-motion`, outside the corpus. |
| inv-air-pump | Air pumps — air evacuated from a vessel | absent |  |  |  | Downstream of `inv-barometer`. An apparatus item: its demand is the ability to produce a condition that does not occur naturally, which is the step from observing the world to experimenting on it. Nothing in the engine lets a people construct a situation. |
| inv-air-pressure-altitude | Air pressure altitude | absent |  |  |  |  |
| inv-biblical-age-of-earth | Biblical earth age | absent |  |  |  |  |
| inv-double-star | Double star | absent |  |  |  |  |
| inv-names-on-moon | Moon features named | absent |  |  |  |  |
| inv-lymphatic-vessels | Lymphatic vessels | absent |  |  |  |  |
| inv-air-pressure | Early pneumatics — pressure as a motive force | absent |  |  |  | Downstream of `inv-air-pump`; a discovery item, see `inv-hydrostatics`. The catalogue's own chain makes this the hinge of the whole arc -- everything from Boyle to Watt is built on it -- which is why the corpus keeps it as its own item rather than folding it into its neighbours. |
| inv-probability | Probability | absent |  |  |  |  |
| inv-pendulum-clock | Grandfather clocks | absent |  |  |  |  |
| inv-saturn-ring | Saturn&#39;s ring | absent |  |  |  |  |
| inv-falling-motion-experiment | Falling motion experiment | absent |  |  |  |  |
| inv-red-blood-cells | Red blood cells | absent |  |  |  |  |
| inv-syrtis-major | Syrtis Major | absent |  |  |  |  |
| inv-capillaries | Capillaries | absent |  |  |  |  |
| inv-static-electricity | Static electricity | absent |  |  |  |  |
| inv-acid-base | Acid-base balance | absent |  |  |  |  |
| inv-chemical-elements | Scientific chemistry | absent |  |  |  |  |
| inv-boyles-law | Boyle's law — pressure volume relation known | absent |  |  |  | Downstream of `inv-air-pressure`; a discovery item, see `inv-hydrostatics`. Its specific demand -- a RELATION between two measured quantities, held as knowledge -- is the sharpest form of that item's argument: Hornvale's belief layer can carry a false claim about a place, and has no representation for a claim about how two quantities covary. |
| inv-royal-society | Royal Society | absent |  |  |  |  |
| inv-jupiter-red-spot | Jupiter&#39;s Red Spot | absent |  |  |  |  |
| inv-cell | Cells | absent |  |  |  |  |
| inv-light-diffraction | Diffraction | absent |  |  |  |  |
| inv-planet-rotations | Planetary rotation | absent |  |  |  |  |
| inv-light-spectrum | Prisms | absent |  |  |  |  |
| inv-conservation-momentum | Conservation of momentum | absent |  |  |  |  |
| inv-no-spontaneous-generation | No spontaneous generation | absent |  |  |  |  |
| inv-reflecting-telescope | Reflecting telescopes | absent |  |  |  |  |
| inv-blood-color | Blood colors | absent |  |  |  |  |
| inv-calculus | Calculus | absent |  |  |  |  |
| inv-double-refraction | Double refraction | absent |  |  |  |  |
| inv-fossils | Fossils | absent |  |  |  |  |
| inv-phosphorus | Phosphorus | absent |  |  |  |  |
| inv-diabetes | Diabetes diagnosed | absent |  |  |  |  |
| inv-saturn-satellites | Saturn&#39;s four moons | absent |  |  |  |  |
| inv-mars-distance | Mars distance | absent |  |  |  |  |
| inv-saturn-rings | Saturn&#39;s multiple rings | absent |  |  |  |  |
| inv-speed-of-light | Speed of light | absent |  |  |  |  |
| inv-microorganisms | Microorganisms | absent |  |  |  |  |
| inv-light-as-wave | Light as wave | absent |  |  |  |  |
| inv-southern-stars | Southern stars | absent |  |  |  |  |
| inv-pressure-cooker | Pressure cookers — pressure vessel held above ambient | absent |  |  |  | Downstream of two `absent` items. Its demand is containment -- a vessel that holds a condition against the world -- and the nearest thing the engine has is a community's stores, which are a scalar that decays. |
| inv-muscles-bones | Human kinetics | absent |  |  |  |  |
| inv-dodo-extinction | Dodo extinct | absent |  |  |  |  |
| inv-plant-sexuality | Plant sexuality | absent |  |  |  |  |
| inv-bacteria | Bacteria | absent |  |  |  |  |
| inv-earth-size-accurate | Earth size accurate | absent |  |  |  |  |
| inv-imaginary-numbers | Complex numbers | absent |  |  |  |  |
| inv-meteorological-map | Trade winds mapped | absent |  |  |  |  |
| inv-plant-species-classified | Plant species classified | absent |  |  |  |  |
| inv-laws-motion | Laws of motion | absent |  |  |  |  |
| inv-universal-gravitation | Universal graviation | absent |  |  |  |  |
| inv-shape-of-earth | Non-spherical earth | absent |  |  |  |  |
| inv-plate-glass | Plate glass | absent |  |  |  |  |
| inv-animal-classification-improved | Improved animal classifications | absent |  |  |  |  |
| inv-calculating-machines | Calculating machines | absent |  |  |  |  |
| inv-mortality-tables | Mortality tables | absent |  |  |  |  |
| inv-miners-friend | Miner's friend — engine raises water from a working | absent |  |  |  | Downstream of `inv-air-pump` (`absent`) and `inv-coal-mining` (`deferred`), so `absent` by weakest demand. The catalogue's chain is doing something Hornvale's model cannot: the engine exists BECAUSE the mine flooded, so a constraint on one capability calls a second into being. `occ-delve-depth` accrues metres per head per epoch with no obstacle term at all, so a Hornvale working has no depth at which it needs anything. |
| inv-scientific-voyages | Scientific ocean voyages | absent |  |  |  |  |
| inv-gas-volume-temperature | Gas volume temperature — thermal expansion of gas known | absent |  |  |  | Downstream of `inv-air-pressure`; a discovery item, see `inv-hydrostatics`. Recorded separately because the catalogue does, and because its demand names temperature, which Hornvale models richly as a field (`hornvale_kernel::Temperature` crosses domain boundaries as a typed quantity) and not at all as something anyone in the world knows a fact about. |
| inv-coke-iron | Coke and iron | absent |  |  |  |  |
| inv-newcomen-steam-engine | Newcomen steam engine — engine does sustained useful work | absent |  |  |  | Downstream of two `absent` items. PREREQUISITE DROPPED: built on `coke-iron`, outside the corpus. Its demand -- sustained useful work -- is the first item in the arc whose value is a RATE rather than a possibility, and the engine has exactly one rate of this shape, `DELVE_M_PER_PERSON_EPOCH`, whose own doc records that it was "CHOSEN FOR DYNAMIC RANGE, AND THAT IS AN INSTRUMENT DECISION RATHER THAN A RESULT ONE". A rate chosen to make a distribution legible is not a rate a people improves. |
| inv-mercury-thermometer | Mercury thermometer | absent |  |  |  |  |
| inv-heat-capacity | Heat capacity | absent |  |  |  |  |
| inv-latent-heat | Latent heat — latent heat known | absent |  |  |  | AT THE PREDECESSOR'S 41-ITEM POPULATION THIS OPENING SENTENCE WAS TRUE; THE CADASTRE'S CLOSURE HAS MADE IT FALSE, AND IT MUST BE READ HISTORICALLY (see the amendment immediately after it): "A root, so the verdict is chosen; a discovery item, see `inv-hydrostatics` for the general argument." PREREQUISITE RESTORED BY THE CADASTRE (Task 2, 2026-09-12): built on `inv-heat-capacity`, previously described here as outside the corpus. The closed 301-item population makes `heat-capacity` a real item in this corpus (itself a closure-added item, `absent`, from a deep chain through `thermometer`/`hydrostatics`/`geometry`/... down to `biped`), so this item is now INHERITED rather than chosen under the family's chosen/inherited rule. It is therefore NO LONGER one of the corpus's roots, which the note's own 'one of ten ROOTS' sentence below must be read historically rather than currently: its former `disclosure` (which called it a root) is deleted. Scored `absent` independently of its arc rather than inherited, which matters for the ratchet: it is one of ten ROOTS in this corpus -- the items whose verdict rests on its own evidence rather than on a prerequisite's. Swept against `BIO-8` and `TECH-2`: nothing. SWEPT AGAINST THE WHOLE REGISTRY (ledger #17, fix round 1): `LANG-49` refused for the reason given on `inv-hydrostatics`, and nothing else in 1,779 rows names a thermal quantity a people knows. FORMER DISCLOSURE (kept for its search record; superseded by The Cadastre, Task 2, 2026-09-12 — this item is now INHERITED, not chosen, so the disclosure itself no longer applies): "NOT BLIND: a ROOT -- no `presupposes` -- so nothing upstream forced this verdict and it rests entirely on a search of the repository run by a session that had already read `tech_for`, `tech_weight` and `TechHorizon`.  Its SELECTION was blind: it is in the `steam-diffusion` arc's own link list." |
| inv-steam-engine | Steam engine — engine efficient enough to spread | absent |  |  |  | Downstream of two `absent` items, and the item whose demand is closest to what this corpus is FOR. Its content is not that an engine works but that it works well enough to SPREAD -- a capability crossing a threshold and then diffusing across peoples, which is the shape `TECH-1` proposes ("a capability is crossed when biome resources x subsistence x surplus clear a bar") and `tech_for` does not implement. It is `absent` rather than `deferred` on `TECH-1` deliberately: `TECH-1` names the MECHANISM this corpus wants, not this item's capability, and anchoring 38 absent items to one generic row would turn this column into the backlog that decision 0095 and spec section 2 both forbid. |
| inv-improved-steam-engine | Improved steam engine — rotary power drives general machinery | absent |  |  |  | Downstream of `inv-steam-engine`, and the last item in the catalogue's chronological order here. Its demand is generality -- one capability that powers arbitrary others -- which is the furthest any item in this corpus stands from a four-rung clock, and the corpus ends on it because the catalogue does. |
