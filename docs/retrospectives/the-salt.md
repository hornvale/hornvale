# The Salt — retrospective

**Completed:** 2026-08-03 (slug-named per decision 0026; spec
`docs/superpowers/specs/2026-08-03-the-salt-design.md`, plan
`docs/superpowers/plans/2026-08-03-the-salt.md`, eight tasks). Ran under
campaign-autopilot. Process lessons only; the product is in the chronicle.

**Scope was set by a rehearsal, not a grep, and the rehearsal was still
blind.** The population of id-coupled channels was found by burning a thousand
extra mints at the top of `build_to` and regenerating every artifact against a
verified-clean baseline — chosen precisely because The Scaffold had just
recorded three of four grep-and-trace artifact predictions coming back wrong.
It worked, and it *still missed the largest channel in the campaign*, because
person names surface only in the census fixture and `regenerate-artifacts.sh`
excludes censuses by design. **A sweep over "the committed artifacts" is not a
sweep over "everything this change can move."** The census fixtures are
committed and drift-checked and invisible to the mechanism that exists to catch
drift. Found by asking what the rehearsal could not see, then measuring that
channel directly (18 of 20 world-rows moved).

**Three of the plan's own tests would have passed vacuously, and one author
wrote all three.** Caught only because every dispatch demanded a demonstrated
RED before the test was trusted:

- The flesh fixture built `OccupationRecord`s by hand on a world with an empty
  ledger, but `flesh_seed` re-reads the core *through the ledger*, so every
  fixture collapsed onto the same `unwrap_or(0)` fallback — both tests green,
  proving nothing.
- The material-difference test perturbed `peak_population`, which
  `structures_of` reads *directly* as well as through the seed, so it stayed
  green under a constant seed.
- `founder_handles_are_free_of_the_entity_id` asserted "same key ⇒ same
  handle", which a constant `RoleHandle(0)` satisfies trivially.

The lesson is not "write better tests." It is that **a test written by the
person who wrote the design inherits the design's blind spot**, and no amount
of care at authoring time substitutes for making the test fail on purpose once.
This is `[[PROC-20]]`'s verify-before-assert turned on tests themselves, and it
belongs in every dispatch that ships a test, not just the ones that feel risky.

**A tidy-looking fix would have corrupted a committed census, silently.** Task
3's first implementation resolved the ledger's people label against
`WorldComponents::assemble()` to obtain a `'static KindId`. Lab's synthetic
rosters carry species the canonical roster has never heard of — `goblin-twin`
*is* `census-of-the-meeting`, the solo-roster null control — so every founder in
that study would have resolved to `None` and collapsed onto one handle, giving
every figure in a committed fixture the same name, with nothing red anywhere.
Two transferable pieces: the resolution introduced a dependency **the
derivation never needed** (the key folds the label by content, so a `KindId`
was never required), and **the blast radius of a lab-visible change includes
the synthetic rosters**, which no default-world test exercises. Found by asking
what the `None` branch does on a non-canonical world.

**A `stream_labels!` bump touches two artifacts, and only one is documented.**
The known one is the stream manifest, which the plan called out and the
implementer regenerated. The second is `cli/src/streams.rs`'s
`the_stamp_is_exactly_this_roster` golden — the deliberate review gate that
makes a new versioned label a decision rather than a silent change to what
every saved world claims. Nothing in the spec, the plan, or `CLAUDE.md`'s regen
list mentions it. It was caught by `make gate` after six tasks had gone green,
and no crate-scoped run would have found it. **Candidate fix:** wherever the
"a `stream_labels!` change needs a manifest regen" note lives, make it name
both artifacts, so the pair travels together.

**A wrapper's exit code is not the command's.** `make gate > log 2>&1; echo
"EXIT=$?"` exits 0 because the `echo` succeeds, so the harness reported a
**failing gate as a completed command**. The failure was found only by reading
the log. Any long command whose result matters must have its own exit code
written *into* the artifact it produces and read back from there — which is
what the second gate run did (`GATE2_EXIT` appended to the log).

**The campaign before this one drew its successor's scope one item too small,
and said so in a doc comment.** The Scaffold documented that `layer_key`'s
fourth key still ordered by the predecessor's `EntityId`, called it "a
compromise, not a material fact," and deferred it to The Signet. That deferral
defeated the sequence's whole purpose: The Signet renumbers everything, so
leaving a comparator reading a number guarantees the unreadable diff the split
exists to avoid. **A deferral written into a doc comment is not a decision that
has been reviewed** — it is one campaign's judgment inherited unexamined by the
next. Re-reading the predecessor's *apologetic* comments, not just its
chronicle, is what surfaced it, and the fix cost one site's ordering across
three seeds.

**Two controller-side fixes worth the pattern, not just the instance.** After
Task 2, `render_flesh` was still routing an id through `record_of` to
reconstruct a record `layers_at` had built moments earlier — a readout
re-deriving what it already held, which is exactly the shape decision 0092
exists to stop; fixing it made `Layer::entity` dead and its removal is the
campaign's point made concrete. And two doc claims went stale in the same
commit, one of them *false* (the module header still said the flesh seed
derives from the entity id). **A re-key changes prose about the code as surely
as it changes the code**, and the compiler checks neither.

**A census refresh at the close conflates every campaign it absorbed.** The
run picked up seventeen moved metric distributions, and only three are The
Salt's. The other fourteen (name-length, syllables, transparency,
collision-rate, homophony, monophyly) belong to `ce13bae0`, the language
compass absorbed hours earlier — it added east/west and four intercardinals as
concepts, which moves the lexicon every name is drawn from. Attribution was
possible only because an **isolated 20-seed before/after had been taken on this
branch alone**, before the merge, and it moved the three name-prefix columns and
nothing else. Without that earlier run there would have been no way to divide
the diff short of a second twelve-minute census at main's tip. **Take the
isolated measurement before absorbing main**, or accept that the close's census
cannot tell you whose change did what.

**The canonical-host guard fired, correctly, and I had told Nathan the wrong
thing.** `CLAUDE.md` says the census is a "~7-min LOCAL run", which means local
to `lefford` rather than remote on AWS — not local to the Mac. I read it as the
latter and said so out loud before `scripts/census-run.sh` refused the run on
`MacBookPro` (decision 0063). The guard cost nothing and caught a real error;
the phrasing that misled is worth a word in `CLAUDE.md`, since "LOCAL" there is
doing double duty against an abandoned remote path nobody uses any more.

**Book freshness sweep: no Confidence Gradient bet moved.** Grepping
`book/src/open-questions.md` for identity / entity / stratigraphy / mint-order /
palimpsest returned only incidental prose, never a tracked bet — the same result
The Scaffold recorded, and checked rather than assumed per decision 0030.

## Handoff — what The Signet must not rediscover

**The Signet's artifact diff should now contain exactly two prose files** —
`book/src/gallery/possession-seed-42.md` and
`possession-over-time-seed-42.md`, where entity numbers are printed *as*
numbers (`settlement/7/name`, `[819]`). Anything else that moves is a channel
The Salt missed, and that is the cheapest possible signal.

**The keystone test is `cli/tests/id_shift_invariance.rs`**, and it does not
work by shifting ids. It groups seed 42's real occupations by material key and
by founding key and asserts that members of a colliding group — which have
*different* ids by construction — produce identical flesh seeds, structures,
residue and founder handles. Seed 42 supplies 3 colliding material-core groups
(7 records) and 29 colliding founding-key groups (61 records). If The Signet's
id derivation is a function of the same material facts, **those groups may
merge or vanish**, and the test's own anti-vacuity assert (it requires at least
one colliding group) will catch it rather than passing silently.

**`layer_key` is no longer total, on purpose.** Ties fall through to
`sort_by_key`'s stability over ledger iteration order — which is *commit* order,
and no change of id derivation moves it. That is the property that makes a
tie-ing material key safe under The Signet, and it is stated in `layer_key`'s
doc and enforced by nothing. Registry row:
`SIG-layer-key-ties-rest-on-a-stable-sort`.

**The epoch question this campaign left open.** Decision 0084 declined an epoch
for a derivation whose renderer "commits nothing" — which describes
`history/flesh` exactly — yet The Salt bumped it to `/v2`. The distinguishing
fact is that 0084's measurement came back byte-identical (the *empty epoch*
0089 warns against) and this one did not. The reasoning is written beside the
row in `cli/src/streams.rs`. **There is no decision that settles the general
case**, and the next campaign to move a never-committed derivation will face it
again; a decision record would be cheap and is not yet written.

**Two registry rows were minted rather than left in scratch:**
`SIG-layer-key-ties-rest-on-a-stable-sort` and `LANG-entity-id-as-concept-text`
(`account.rs` renders an entity id as *concept text*, so entity-valued facts are
silently unspeakable for every culture on every world — real, inert today, and
out of The Salt's scope).

**Also cleared, and unrelated to this campaign:** `main` was already red on
`registry_idea_cells_are_within_budget` — four `MAP-*` rows from `93e30931` at
1230–1539 characters against a 600 budget, never waived. CI is manual-only
(0042), so nothing surfaced it. Fixed by the remedy the check's own message
names: the four arguments moved into a `frontier.md` section ("The place as
medium") and the rows became the index entries they were meant to be, `raw` →
`elaborated`. Compacting to 600 would have destroyed most of four dense
arguments.
