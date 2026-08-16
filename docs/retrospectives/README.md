# Retrospectives

One page per merged campaign, written at merge time alongside the
chronicle entry (decision 0020). This is the **process** story — the
chronicle tells the product story, the decision log records settled
choices; this directory captures what the work taught about the working.

## Conventions

- One file per campaign, named after its slug: `campaign-<slug>.md`
  (matching the chronicle entry's naming).
- **One page maximum.** If it wants more, the surplus is probably a
  decision record or a spec correction, not retrospective prose.
- Process, not product: what the campaign shipped belongs in the
  chronicle. What belongs here:
  - **Recurring review findings** — anything a reviewer flagged that has
    been flagged before (the fmt-gate skip is the canonical example).
  - **Estimate deltas** — which stages ran long or short, and why.
  - **Spec versus reality** — assumptions the spec made that the plan or
    the code had to correct, so the next spec doesn't repeat them.
- Written once, at merge; not edited after (append a dated postscript if
  something is learned later).

## Template

```markdown
# Campaign <name> — retrospective

**Merged:** YYYY-MM-DD

**Recurring findings.** …
**Estimate deltas.** …
**Spec vs. reality.** …
**Do differently next time.** …
```

## Index

One line per merged campaign — the distilled lesson, not the story; the linked
page is the full account. **Grep here before reopening an area**: a campaign
that already refuted a premise, or already paid for a measurement, is the
cheapest thing you can find before specifying the next one.

Grouped the way the work actually clusters, not chronologically. Campaigns
whose retrospective predates this index are still listed — the pointer is the
point. Entries are terse and occasionally shouty because they began life as
recall hooks in a per-machine agent memory store, which is exactly the problem
this index fixes: that store did not reach `lefford` or `ambrose`, and was not
under review. This file is.

### The peoples program (C2)

- [The Range](the-range.md) — realm gate couldn't reach the WORLD
- [The Generalist](the-generalist.md) — MINERAL was the wrong axis
- [The Warren](the-warren.md) — confinement made places **less alike**
- [The Long Age](the-long-age.md) — lifespan gained an authored pace factor; the null held exactly; long life is a COST under 0066
- [The Tolerance](the-tolerance.md) — C2t — a species gained an interior (dispersion + per-settlement draw); H1 was mis-specified, H2 partly refuted, H3 confirmed; merged 2026-08-05

### Sky / astronomy

- [Shadow Track](the-shadow-track.md) — scene/eclipses/v1 producer + Orrery eclipse marks + globe shadow band; shipped world-wasm-v7; two keystone lessons
- [Wandering Sun](the-wandering-sun.md) — obliquity libration on locked worlds + spinning year_phase fix so ice tracks the sun; SHIPPED+PUSHED both repos, world-wasm-v6
- [Real Sky](the-real-sky.md) — scene/neighbors/v1 real starfield + retrograde moon orbits; SHIPPED+PUSHED both repos, world-wasm-v5…
- [Presiding](the-presiding.md) — (SKY-25) merged to local main 2026-07-17 — retired the world-level belief-kind metric for per-species readings; SKY-25 dissolved not repaired
- [Reckoning](the-reckoning.md) — SHIPPED+PUSHED (main @e04d0d3 landed on origin 2026-07-17); epoch went PARTIAL→TOTAL…
- [Terminator](the-terminator.md) — SKY-24 regime-aware insolation SHIPPED to local main @de7ddbe 2026-07-16 (habitability fix; religion payoff FALSIFIED, banked as SKY-25); NOT pushed
- [Eclipse Seasons](eclipse-seasons.md) — SHIPPED+PUSHED 2026-07-14 (@8dfd8d3) — dated solar/lunar eclipses, moon ephemeris, ground tracks, recurrence ladder…
- [Long Count](the-long-count.md) — SHIPPED — merged+pushed to origin/main 2026-07-14 (@da79556); eclipse core ceded to Eclipse Seasons; census regen DONE 2026-07-14 (ebf196c)
- [Self-Desc Sky](the-self-describing-sky.md) — SKY-15 — ledger facts + explain verb + deity-name /v2 epoch; MERGED+PUSHED to origin/main 2026-07-14
- [Faces](the-faces.md) — (hornvale#4) SHIPPED+PUSHED both repos 2026-07-16 — scene/moons/v1 moon surface descriptors, world-wasm-v4 released
- [Night Sky](night-sky-instrument.md) — Instrument campaign — SHIPPED, merged+pushed to origin/main 8c12791 on 2026-07-14; AWS census regen deferred

### Terrain / climate / weather / life

- [The Ford](the-ford.md) — 110 km rivers were a **TYPE** error, not resolution
- [The Millrace](the-millrace.md) — indexed `nearest_line` BYTE-IDENTICALLY; census 19,208 s → 950 s
- [The Rill](the-rill.md) — 0130; keystone REWRITTEN mid-flight
- [The Confluence](the-confluence.md) — SEQ-2, settlements near fresh water
- [The Mantle](the-mantle.md) — globe cloud-texture shell (completes The Firmament's deferred cloud visual); SHIPPED+PUSHED 2026-07-19 world-wasm-v11…
- [The Firmament](the-firmament.md) — drawn weather (Weather Program C4), felt-weather half SHIPPED hornvale-only 2026-07-19; globe cloud RENDERING deferred (billboarded sprites → haze)…
- [The Rains](the-rains.md) — precipitation derived-field EPOCH (Weather Program C3); moisture-budget trace; SHIPPED+PUSHED both repos 2026-07-19 world-wasm-v10; census DEFERRED…
- [The Turning](the-turning.md) — diurnal temperature (Weather Program campaign 1); byte-identical zero-mean day/night term + orrery \"watch a day\"…
- [The Fare](the-fare.md) — (Weather Consequence C2) — weather's effect on travel is a TAIL phenomenon medians erase, and it cannot be routed around
- [Sculpting](sculpting.md) — (terrain epoch v3) MERGED+PUSHED 2026-07-16 (@c537008) — carve engine A, full decoration mandate, 5/6 bands, shoreline handed to MAP-21…
- [rift-and-fit](rift-and-fit.md) — MAP-21 rift-and-fit (terrain epoch v4) SHIPPED to local main @de7ddbe 2026-07-16, NOT pushed (regen deferred)…
- [Single-craton hypsometry](single-craton-hypsometry.md) — SHIPPED to origin/main 2026-07-14 (4398cf0) — shelf-break fallback, decision 0053, land-normalized shelf ruling
- [The Ground](the-ground.md) — (lithology/pedology substrate) — MERGED to local main @0900411 2026-07-14 (NOT pushed); census reds since cleared (2026-07-14 regen ebf196c)…
- [The Fathom](the-fathom.md) — the column ALREADY EXISTED one crate over; H-1 falsified on my own unmeasured threshold
- [The Axes](the-axes.md) — Chorography c1; the gate's own criterion COULD NOT FAIL (no-collisions is satisfied by one axis with 21 values, which is the enum); a rule applied reactively caught 8 more violations six tasks later; 4 plan defects found by executing, 0 by reviewing
- [The Isotherm](the-isotherm.md) — cross-repo goldens producer-sourced
- [The Freshet](the-freshet.md) — Salt/fresh water classification (DOM-5 first slice) — SHIPPED+PUSHED @207fc65
- [BIO-2](bio2-life-history.md) — life-history allometry — first campaign-autopilot validation run SHIPPED to local main @35be669 (NOT pushed); zero G3 vetoes
- [The Demesne](the-demesne.md) — (BIO-35 Stage 1): per-axis spatial supply restored the rank collapse. SHIPPED to main @33f15a7f (census regenerated on lefford per 0063).…
- [Living Community](the-living-community.md) — C1 (history-first placement) SHIPPED @4f74961f…
- [Connection Graph](the-connection-graph.md) — C2 slice 1 (transport topology, derived + legible) SHIPPED @cfaf57e3. Natural-routes-not-built-roads reframe; ocean-is-the-real-separator finding…
- [The Sundering](the-sundering.md) — (The Moving Sea), living-community C2 slice 2…
- [The Hollow](the-hollow.md) — my repair committed both defects
- [The Deep Realm](the-deep-realm.md) — 2 of 3 falsification clauses failed
- [The Tithe](the-tithe.md) — (living-community C3 slice 2, SHIPPED @346fd2ee): tribute as accumulation…
- [The Tumult](the-tumult.md) — SOC power law FALSIFIED, shipped as headline
- [The Watershed](the-watershed.md) — FALSIFIED before it was built
- [The Sounding](the-sounding.md) — (feasibility benchmark for the living-community engine) SHIPPED @5bf3a452. The census-with-floor tool; the overclaim-caught-by-review lesson…
- [The Witness](the-witness.md) — Discharged The Wearing's F5/F7/F13 — a check whose input is authored cannot witness reachability; F5 was four defects stacked
- [The Waterline](the-waterline.md) — 0079, headline feature DELETED
- [Assay](the-assay.md) — (decision 0064) — potency=CR/30; campaign 1 of the Dragons program (personhood-as-region reframe)
- [Eremite](the-eremite.md) — (decision 0065, UNI-31 shipped) — SocialForm axis + nested-capacity lattice + dragons' solitary mind; the Dragons program KEYSTONE
- [Cloister](the-cloister.md) — (decision 0067) — split PsychVector into MindVector + SocietyVector; Dragons program C4 (the enabler, not the payoff); SHIPPED byte-identical
- [Vigil](the-vigil.md) — Dragons program C5 SHIPPED (0074): dragon perception, the enforced capacity CHAIN, by-class fact gating…
- [Solitary Tongue](the-solitary-tongue.md) — (decision 0066, BIO-37 language half) — drift = f(sociality × lifespan); dragons speak a frozen Draconic isolate; Dragons program C3
- [the-gathering](the-gathering.md) — MAP-7 the-gathering campaign — MERGED to main 2026-07-13; census regen still pending ON MAIN (main CI red until then)

### Engine / ECS / infrastructure

- [The Hearsay](the-hearsay.md) — myth gets its channel; four things that could not go red, all in controller text and all caught by mutation; violence is intramural twice over
- [The Retelling](the-retelling.md) — content learns to vary; stance replaces two incommensurable keys; the antichain predicts semantic divergence at rho 0.666; distortion caps at one rung, which is the third measurement of the same scarcity; six repo mechanisms found green while blind
- [The Begat](the-begat.md) — a column 44% dearer with no code change, superlinear over a world that grew; the projected depth-map fix was unsafe on a cycle-guarded walk and a downward walk needed no cycle assumption at all; a BFS claim falsified against itself; byte-identity re-established on the joined result rather than inherited; a cost claim wrong 10.7x against a threshold that never existed

- [Dissolution](the-dissolution.md) — ECS Campaign 3 \"The Dissolution\" — SpeciesDef god-struct DELETED, dissolved into per-domain component registries…
- [Individuation](the-individuation.md) — ECS Campaign 5 (instance ⋈ ledger) — the instance half of the entity-component substrate; SHIPPED+PUSHED @1acbe98
- [Ordination](the-ordination.md) — ECS Campaign 6 (systems & schedule) — the derived execution schedule + BSP tick mechanism; SHIPPED+PUSHED @c9cb8b6
- [The Named](the-named.md) — (hornvale#1) shipped to local main 2026-07-16 — almanac attribution by property not position; the ticket's own premise was wrong…
- [The Pyx](the-pyx.md) — 0090, binary hashing is the host oracle
- [The Digest](the-digest.md) — time-free fact ledger behind `make doctor`
- [The Armature](the-armature.md) — 0119; 30 links frozen BLIND
- [The Domesday](the-domesday.md) — generated survey, 8 detectors
- [The Gnomon](the-gnomon.md) — the Domesday's transpose; H1 FALSIFIED at recall@10 0.5667 vs a 0.60 bar — a tail rank measures unusualness, not change; `refuted` opened and spent in one campaign
- [census-as-data](census-as-data.md) — campaign SHIPPED to main+origin 2026-07-13…
- [The Assize](the-assize.md) — "move it to the census" is NOT the universal cure
- [Single Saying](the-single-saying.md) — PROC-18 The Single Saying…
- [Compound Word](the-compound-word.md) — PROC-19 The Compound Word…
- [Standing Offer](the-standing-offer.md) — PROC-12 The Standing Offer…
- [The Staff](the-staff.md) — 0132/0133; 15 plan/spec defects, 0 in implementer code; found 5 checks reporting green on nothing then wrote a 6th verifying the fix for one; a scope boundary stated as a prohibition is not enforcement
- [The Ballast](the-ballast.md) — 0134; all five first diagnoses overturned by the cheapest measurement; an allow-list gate cannot see its own list go short (one crate had zero commit-gate coverage since it merged); existence is not evidence of rewrite
- [The Compendium](the-compendium.md) — 0135/0136; a capability corpus scoring the PROGRAM, sibling to the trope family; FOUR false-cleans in one resolver, each found by a different mechanism and none by a passing suite; a rule derived from one real row is still an anecdote; two controller figures corrected by implementers who re-derived them
- [The Glasshouse](the-glasshouse.md) — 0137/0138; the census warmed −11.99 → −3.65 °C and ice-dominant worlds 651 → 187/1000, meeting 4 of 6 frozen criteria; an inherited "8.6σ" was 1.09σ; "fix this constant from Earth" was provably impossible (the term vanishes at the anchor); a column that moved by ZERO was a scope error, not a null; how to freeze a decision rule you already know the answer to

### The Walk (game-layer liveness arc — M2)

- [The Quire](the-quire.md) — native character-grid game client; 13 defects
- [The Seam](the-seam.md) — walkable Chunk 0 of The Walk, first game campaign; spec'd 2026-07-13 on branch the-seam (possess/verb loop/five interfaces in windows/vessel)
- [The Panes](the-panes.md) — (2026-08-06) shipped the two-pane roguelike+text view; EVERY review finding traced to my plan text, and 4 of 5 were invisible to a green suite
- [The Sighting](the-sighting.md) — occlusion DEAD in production
- [The Beholding](the-beholding.md) — the possessed species' eyes, 2/255
- [The Reassay](the-reassay.md) — 0108-0112; BRANCH NAME ≠ campaign name
- [The Lantern](the-lantern.md) — the hearth had **no cell**
- [The Quickening](the-quickening.md) — The Walk Milestone 2, first liveness — the world's first autonomous motion (NPC daily movement); SHIPPED+PUSHED @67bc1a2
- [The Wanting](the-wanting.md) — The Walk M2, the drive rung (PSY-6 motivation) — NPCs act from a homeostatic drive; the reserved GOAP seam; SHIPPED+PUSHED @36cd84c
- [The Foresight](the-foresight.md) — The Walk M2 GOAP goal rung — kernel A* planner (UNI-19/PSY-6); SHIPPED+PUSHED @7f928e1
- [The Surmise](the-surmise.md) — The Walk M2 perception→belief tier (UNI-1 first instantiation) — SHIPPED+PUSHED @9c3736d
- [The Casement](the-casement.md) — (wasm possession exhibit in the book) SHIPPED to main + pushed 2026-07-14; decision 0052; sequels MAP-35..38; vessel-check is its gate

### Client / book / world-model

- [The Idioms](the-idioms.md) — Orrery render-style layer (MAP-60) — 4 data-aware skins; the black-render bug class + shader-gotcha checklist
- [The Cartographer](the-cartographer.md) — the visual pass overturned the MEDIUM
- [The Freshwater](the-freshwater.md) — a pure read is the cheapest save-format change
- [Diorama](the-diorama.md) — MAP-67 — Voxel-2.5D style for the Orrery MAP rung (fixed-isometric heightfield diorama); 3rd of the view-remake program…
- [The Massing](the-massing.md) — voxel = the exposed CLIFF
- [The Overworld](the-overworld.md) — the look is Nathan's call
- [The Excursion](the-excursion.md) — whole-branch review caught what per-task missed
- [The Selvage](the-selvage.md) — MAP-71 — closed the Orrery voxel map's cross-tile seam…
- [The Threshold](the-threshold.md) — the right thing went unasked
- [The Frame Budget](the-frame-budget.md) — "12× win" was a settle bug
- [The Particular](the-particular.md) — persons SHIPPED; `person-died` UNREACHABLE — **RESOLVED by [The Ell](the-ell-campaign.md)** (0126/0127)
- [The Ell](the-ell.md) — (merged 2026-08-11, 0126/0127) — typed Fact.day and made the ledger speak days…
- [The Radiation](the-radiation.md) — 0120; `0.25` came from test-fixture code
- [The Muster](the-muster.md) — 0128; **refuted its own premise**
- [The Signet](the-signet.md) — id = lineage, not mint order
- [The Salt](the-salt.md) — an EntityId is never **read for its value**
- [The Namesake](the-namesake.md) — the GENERATOR won't collide
- [The Helm](the-helm.md) — Orrery controls campaign: SHIPPED+CLOSED 2026-07-15 (orrery main @051e34e deployed; hornvale close @0ddea57 pushed)
- [The Vantage](the-vantage.md) — MAP-65 — explicit 3-way view dropdown (System/Globe/Map), retired the zoom-handoff…
- [The Lens](the-lens.md) — jsdom has no layout/paint
- [The Region](the-region.md) — scene/tiles-region/v1 regional/LOD tile contract; SHIPPED+PUSHED, world-wasm-v3 released (the old G6 blocker is RESOLVED)
- [Goldengrove](goldengrove.md) — (3D orrery successor) — SHIPPED, merged+pushed 2026-07-14 @bd4e59c; client LIVE at hornvale.github.io/goldengrove
- [Locale Window](the-locale-window.md) — (P2 campaign 2) merged to main 2026-07-12 (5ea774e)…
- [Uncommon Ground](the-uncommon-ground.md) — MAP-29 first campaign (The Uncommon Ground) — SHIPPED to main 2026-07-13 (950d17e); natural-tier strangeness overlay…
- [The Speakable](2026-07-14-the-speakable.md) — (LANG-32 repair-collapse fix) MERGED to local main @9198c65 2026-07-14 — attested tier makes repair identity for native words; NOT pushed…
- [Book Polish](the-book-polish.md) — Small readability-only campaign fixing the folk-line stutter and dangling initiated line in the Reckoning-of-Years/esoteric layers…
