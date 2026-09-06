# The Spillway

A spillway is the channel that lets a full reservoir deliver past the dam
instead of backing up behind it. The Warp's census filled the reservoir and
the dam held: a hundred and thirty-six regenerated goldens sat staged on the
canonical box for an evening while a person broke the deadlock by hand.

The census is the project's reference dataset — every registered metric over
a thousand unselected worlds — refreshed once per campaign on one enforced
host and delivered as a branch the merge queue gates like any other
candidate. The delivery commits through the ordinary commit gate. That is the
whole design, and it is what makes the refusal interesting: the gate was
working correctly, and what it refused was its own evidence being brought up
to date.

## Three legs, and the registry row named two

The refusal is in the queue's own log:

```
sluice-census:    docs/audits/type-audit-report.md                   |   16 +-
...
pre-commit: HV_CENSUS_DELIVERY=1 — skipping the golden-pins guard for this census delivery.
...
        FAIL [   0.549s] hornvale-lab domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_249_column_census
...
pre-commit: 'make gate-commit' failed — fix fmt/clippy/type-audit/tests before committing.
sluice-census: COMMIT REFUSED — the census ran and its output is NOT delivered.
sluice-census: 144 staged path(s) (136 golden) remain in ... ; nothing was pushed.
```

Three separate facts hold that cycle shut.

**A gate test compares the fixtures' columns to the census's.** The Gnomon
injection battery is eight frozen arms, each a perturbation of one generative
constant scored against the census as its prior. A commit-gate test asserts
that every arm's schema carries exactly the committed census's column set, so
a census that *registers a metric* is red by construction — and the test's
message names the remedy: re-author the arms with their script, on the
canonical box, in the same commit as the refresh.

**The authoring script refuses the delivery's own dirt.** That script mutates
tracked source in place and restores it with a checkout, so it demands a
clean tree. A delivery's staged goldens are exactly a dirty tree. The arms
cannot be re-authored until the refresh is committed, and the refresh cannot
be committed until the arms are re-authored.

**A second gate test carries the column count in its own name.** It asserts
how many of the census's columns are evaluable surface for the anomaly
report; its doc-comment is a per-campaign narrative of every epoch's counts;
and the sub-floor roster selects it by *exact name*. A growing census reds it
on every delivery commit, and no machine may fix it, because the point of the
assertion is that a person re-measured and wrote down what moved.

The registry row that filed this named the first two. Only the log named the
third, and it is the one that failed first.

## The rule, and where each check lands under it

The delivery already stood two checks down, each justified on its own terms.
This campaign states the rule those two were instances of:

> A delivery satisfies every check whose remedy is a regeneration, and defers
> only a check whose remedy is a human re-statement.

The fixture-columns test demands regenerated evidence, and a machine holding
the canonical box at the freshest moment the world has ever been can produce
it in the same sitting — so the delivery **satisfies** it. The count witness
demands a re-measurement with a narrative; a machine that renamed the test
and rewrote the integers would satisfy the assertion while destroying what
the assertion is for — so it is **deferred**. Deferral is safe for exactly
one reason: a delivery never pushes the trunk. Its branch is gated as an
ordinary merge, and the merge re-demands every deferred check of the campaign
that submits it, the same way it already re-demands the calibration pins.
Nothing stood down here can reach the trunk ungated. That is decision 0836.

## What the delivery does now

After the census returns and its own diff is staged, the delivery asks two
questions: did a golden move, or does any arm's column set differ from the
census's? Either is enough. The second exists because the arms are evidence
about a *world*, not about a schema: a census whose values moved with no new
column would leave the arms describing an older world.

On a trigger it runs the *ref's own* copy of the authoring script — not the
queue's — because the script's table of constants must match the source it is
about to mutate. It asks that copy a `check` question first, which runs the
guards and stops without building, so a refusal costs nothing. Then it takes
the same lock every expensive job on that box takes, authors the arms under a
timer whose row lands beside the census's in the cost ledger, releases the
lock, stages the arms with the goldens, and names the outcome in the commit
message.

The guard it must pass was narrowed rather than removed. Its stated reasons
are both claims about *source* — mutated files restored by a checkout, and a
manifest that stamps the commit it was built from — so the predicate now
excludes the book and the documentation tree, where a census's output and the
project's prose live, and (added in the final review, once the census's own
artifact sweep was traced end to end) the browser clients' committed session
fixtures under `clients/`, which sit outside the cargo workspace entirely and
which no `lab run` reads. It still refuses an uncommitted edit anywhere a
build or the mutation can see. Controls in its test file assert that other
half for each excluded tree, because without them every "this dirt is
allowed" assertion would pass equally well against a guard someone had
deleted.

Any failure in that sequence — a missing script, a refused check, a lock that
times out, an authoring run that returns non-zero — is a refusal with the
same exit code the old `COMMIT REFUSED` used. The goldens stay staged for
recovery by hand and nothing is pushed. A void arm is a real finding about
the world, and delivering the goldens without the arms would bury it.

## The honest limits

**No witness is re-pinned by machine**, by design; the deferral is recorded
rather than discharged.

**A ref that predates this merge cannot deliver itself.** It carries the old
whole-tree guard, which refuses the delivery's own goldens as dirt. The
delivery detects that at its pre-flight and refuses loudly; recovery is the
by-hand path The Warp walked. This expires as branches absorb the trunk.

**The lock is held with no claim file** for the minutes the re-authoring
takes. The eight-field claim format has exactly two writers and a third would
be the kind of duplication a test exists to refuse, so during those minutes
the status readers report no run while every waiter still waits.

**Nothing checks the arms between censuses.** They are re-authored at
delivery and compared at delivery; in the interval the only thing holding
them to the census is that both were written at the same ref.

**And the largest: this campaign could not exercise its own delivery script
through the queue at all, before merging — not the growing-census arm, and
not the null arm either.** The queue's drain dispatches the census script
relative to *its own* repo root, the operator's main checkout on lefford,
never the censused ref; only the arms-authoring script,
`gnomon-injection.sh`, is read from the ref itself. A campaign that changes
the delivery, as this one does, cannot therefore prove its own change in
production before that change is on `main` — the queue always runs main's
copy of the delivery script, never the branch tip's. The spec asserted the
opposite (§6.2), and nothing caught it until the delivery log for the
campaign's own pre-merge census was read and found to carry no Gnomon-arms
line at all.

That census (queue row `req-81968faee96c-20260906T144636Z`, 1272 s, NO
GOLDENS MOVED, delivering a branch that carried only the timings row) is
real evidence, just not the evidence the spec claimed: it proved main's own
null path, dead until this campaign and live now that the delivery ships on
main, and it proved the timings row lands. It proved nothing about the arms
step, because main's checkout of the delivery script at that moment named
no Gnomon arm at all. Five arms of a shell test drive the delivery against a
fake census and a fake authoring script and assert what it did, including
that the lock was genuinely held while the stub ran — real evidence about
the wiring and none at all about an eight-minute authoring run over twenty
worlds. The proof is now two-step and both steps sit after this merge: the
first census the queue runs once lefford's checkout has advanced past it
(expected to report `Gnomon arms unchanged` on a null), and the first
census by a campaign that registers a metric (expected to report
`re-authoring the Gnomon injection arms` and a new timings row). Until both
have run, this chronicle claims a mechanism, corroborated by a harness and
by one production run of half of it, and not a result.

## A defect the close found in its own trigger, and fixed before merge

Reading the arms at the campaign's own tip, to check a figure before writing
it down, the staleness comparison reported all eight arms stale — by exactly
one column on each side. Those two columns are the *study names*: the census
schema calls itself `the-census`, an arm calls itself `gnomon-injection`. The
extractor skipped that line by its indentation, on a stated belief that a
study's own name sits one level shallower than a column's — and the belief
itself was wrong. In the real files a column's `"name"` sits at exactly six
spaces, one per `"kind"`, and the study's own `"name"` sits at four spaces,
nested under a `"study"` object the drafting-time diagnosis never checked
against a real file. The original extractor's rule, `^ {4,}"name": "`, is
*greater-or-equal* four, so it caught both depths — not because they
coincide, but because the rule was wide enough to swallow the shallower one
too. This chronicle's own earlier draft repeated the "same depth" claim
without measuring it, which is the same defect shape one level up: an
assertion about the data, asserted rather than counted.

So the second trigger fired on every run and the branch that says *arms
unchanged* was unreachable. The failure was in the safe direction — a
mis-parse here can only cause a needless re-authoring, never a missed one,
and the arms were in fact current at every tip this campaign measured. But
the null path was dead, every null census paid minutes it did not owe, and
the test arm meant to catch exactly this passed, because its synthetic
fixture put the study name at the shallower indent the belief predicted
rather than where the pretty-printer actually puts it — the same shape as a
defect this campaign had already found one file over, and the one recorded
in the project's own census notes: a fixture that does not reproduce the
case that actually occurs reads exactly like coverage.

**This was found and fixed before merge, in the campaign's own fix wave.**
`census_schema_columns` now anchors on exactly six spaces
(`^ {6}"name": "`), verified against every committed schema.json at this tip:
`grep -c '"kind":'` and `grep -c '^      "name":'` both read 290 on the
census, and each of the eight injection arms; the study's own `"name"` is
the sole indent-four hit, one, on every one of them.
`scripts/test-sluice-census.sh`'s `write_schema` helper now emits the real
shape (a `"study"` object at indent two, its `"name"` at indent four), and
three controls were added: a fixture-shape control that pins the fixture to
the real committed files, a real-tree control asserting
`injection_arms_stale` over the actual checkout prints nothing (the same
fact `anomaly_injection::the_fixture_columns_match_the_census` asserts in
Rust), and a positive control that deletes one column from a copied real
census schema and confirms the drift is caught. A census run of this tip's
own script — by hand, or once main's checkout carries it — is therefore
expected to report `Gnomon arms unchanged` on a null, not to pay for a
re-authoring it does not need. **What the campaign's own pre-merge census
through the queue actually ran was not this tip's script at all** — see the
honest-limits section above for why, and for what that run proved instead.
