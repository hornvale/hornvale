# The Mortise — retrospective

**Merged:** 2026-08-27

## The headline: a scope decision reversed because the original price was wrong, not because the taste changed

Coordination (`m07`) was cut whole from the first draft of the spec, priced at
"three mechanisms for one entry" — right-node raising, a gerund subject, and
the coordination node itself. Nathan asked whether that price was actually
true. It was not, in two places, and both were checkable in under a minute
against the tree that already existed:

1. **`Subject::Clause` reads as expensive and is cheap.** Only two places in
   the whole crate consume a `Subject` *structurally* —
   `tongue_subject` (`grammar.rs:316`) and `Part::Subject` (`clause.rs`) —
   against roughly seventy-five construction sites a new variant does not
   touch. Both consumers were already gaining nested-clause logic for the
   object slot, so the subject slot's version was additive on top of work the
   spec was doing anyway.
2. **"Coordination" was quoted as one price and is three.** A substitution
   pass laddered it into tiers: tier 1 (every clause states its own subject)
   is a list node plus a drawn conjunction; tier 2 adds subject elision; tier
   3 is right-node raising. The original estimate priced tier 3 and quoted it
   for the whole feature, which is a category error a substitution pass
   catches directly rather than by argument.

A third finding rode along rather than changing the price: the gerund subject
in `m07`'s literal wording (*"Seeing it confused and upset me"*) was never
load-bearing for its three demand tokens — substituting a complementizer
subject clause satisfies the identical tokens through machinery the spec
already built for `m06`. Nominalization stayed cut for its own reasons, not
because `m07` needed it.

**This is a process finding about plan-text estimates, not a product
finding.** The structural worry that motivated the original cut — that
embedding and coordination would get conflated under one name — survived the
reversal and is answered by §4.10 naming them as two operators sharing only a
boundary marker (ratified as
[decision 0327](../decisions/0327-embedding-and-coordination-are-two-operators-a-slot-and-a-list.md)).
What did not survive is the *price*, and the price was wrong because it was
quoted from the most expensive tier rather than derived from the cheapest one
that would actually satisfy the demand.

## Five plan-text defects, zero in implementer code

The pattern The Scarf, The Reticence and The Inquest each named held again:
every substantive defect this campaign found originated in the controlling
session's own plan or spec text, and every one died to somebody running a
command — never to a re-read.

1. **A brief that named an incoherent placement.** Task 2's dispatch said to
   add `think` "beside `know`'s pack" in the universal stratum. `know` is not
   a `PackEntry` at all — it lives in `action_suite_pack()`, a different shape
   entirely (`&[(&str, &str)]`, each concept a gap in every lexicon). Caught
   pre-dispatch, before any implementer saw it, by reading the target file
   rather than trusting the plan's noun phrase.
2. **A consumer grep scoped to one crate that missed a third match site in
   another.** Task 4's pre-dispatch verification claimed `Subject` had
   "exactly two" structural consumers, having grepped only
   `domains/language/src/`. A third exhaustive match lived in
   `windows/book/src/lib.rs` and broke the full-workspace build. It surfaced
   only because the implementer was told to re-derive the claim rather than
   trust it — the one instruction in the dispatch that turned a
   would-be-red-workspace into a caught, fixed defect before landing.
3. **Three different answers to one counting question, and the one with a
   derivation attached was ALSO wrong.** How many `TongueGrammar {`
   construction sites exist: the controller said 25, the implementer said
   21, the reviewer said 22 and showed its derivation — and the controller
   recorded 22 as authoritative precisely because it came with working
   shown. Re-derived directly against the tree at `0c59434df`: `git grep -c
   'TongueGrammar {' -- domains/language/src/grammar.rs` returns 25 raw
   hits; subtracting the type's own `pub struct TongueGrammar {` definition
   (1) and its three `-> TongueGrammar {` return-signature false positives
   (`tongue_grammar`, `overt_copula_grammar`, `svo_with_copula`) leaves
   **21** — the implementer's number, not the reviewer's. The sharper
   lesson isn't "prefer the count with a derivation attached" — it's that **a
   reviewer's derivation is unaudited text too**: the reviewer's own
   subtraction undercounted the return-signature false positives by one,
   and nothing re-ran it before the controller filed 22 as settled. This is
   `grep -c 'Type {'` counting the type's own definition and its
   `-> Type {` return-signature false positives — the same trap that produced
   three wrong figures in The Scarf — firing three separate times on one
   question in one task, and a fourth time on the fix that corrected it.
4. **A `make rebaseline` step that could not see byte-goldens.** Task 2's plan
   said to run `make rebaseline` and the drift check after registering a
   concept. Neither can see a byte-golden — that path is `make
   rebaseline-goldens` / `REBASELINE=1`, structurally separate — and three
   goldens sat red on the branch for two tasks before Task 3's own residue
   check found it. The fix generalizes past this campaign: `make rebaseline`
   and a byte-golden accept are two different commands with two different
   review postures, and a regeneration step that only names the first is an
   incomplete instruction, silently.
5. **A plan that put subject elision where it structurally could not work.**
   Task 7's plan said to modify "clause.rs (the coordination realizer)" —
   singular, and the wrong file set. `join_coordinated` operates on
   already-realized strings, so it cannot elide a subject: a tongue orders
   constituents by a drawn `ConstituentOrder`, and the subject can land first,
   middle, or last across the six orders. Elision has to happen *inside*
   per-clause realization, in all three realizers, not as a post-process on
   joined text. Caught pre-dispatch by reading `join_coordinated`'s actual
   signature rather than trusting the plan's one-line description of it.

**What caught them, again:** not review of the plan against itself — a
command, run against the tree, every time. The two mechanisms worth repeating
are verifying each brief against the code *one task ahead* rather than in a
batch at plan time (this is what caught 1, 4 partially, and 5), and telling
implementers plainly that they outrank the plan (this is what caught 2, and
is the only reason 2 was fixed before it shipped rather than after).

## A witness whose RED had to be demonstrated, and a review that found it wasn't

Success criterion 9 asked for something unusually strict: not that the
realization witness exists and passes, but that its implementer *find* a
mutation the witness is sensitive to, from inside the code, and record it
failing. This paid off in a way a prescribed mutation would not have.

`m07`'s first-draft witness gave both coordinated clauses a bare pronoun
subject. It passed every assertion — including the count, the id list, and
the surface-equality check — while never constructing an `Argument::Clause`
or `Subject::Clause` at all, despite `m07`'s demand tokens crediting it for
exactly that capability. The controller's own concern (m07's witness surface
contains no embedded clause) was routed to the reviewer rather than resolved
unilaterally, and the reviewer did more than adjudicate it: it found that the
spec's own §9.1 had already stated the fix in writing — *"substitute the
subject form and hold everything else constant"* — and that the machinery
`m07` needed had been built and tested in Task 4 and sat unused one task
later. The implementer's own commit message stated the general principle
almost verbatim but applied it one level too shallow — to the
Clause-vs-Coordination axis, not to the embedded-clause token one level
inside the coordination. Right principle, applied one layer short of where it
needed to reach.

Two perturbations were then required and both were demonstrated, not
asserted: `elide_coordinated_subjects` neutralized (coordination half) and
`Subject::Clause`'s realization stubbed to `"GERUND"` (embedding half), both
failing the witness, both restored with an md5 check that the file was
byte-identical afterward. That is the shape criterion 9 was written to force,
and it worked exactly once it was pushed on.

## Two inherited claims, one confirmed and one that did not survive a recount

The plan handed this task two "small" corrections from the previous
campaign's own retrospective and README entry, both flagged with the standing
warning that a handoff claim is exactly the class this campaign learned not to
trust. Both were checked against git history rather than applied on faith.

**The Inquest's merge date was wrong, and is now fixed.** The merge commit
(`e5cae97aa`) is timestamped `2026-08-27T07:38:42-04:00` — the retrospective's
own "Merged: 2026-08-26" line was off by a day, corrected here.

**The Scarf's "twelve defects" claim did not survive a recount and was left
alone.** The handoff text (and this campaign's own plan, which had copied it
forward) asserted the true count was twelve, "the twelfth surfaced during its
own DoD task." A full recount of the retrospective's own enumerated groups —
the destructive-checkout step, the three count errors, the two false-compiles
claims, the omitted artifact regeneration, the stale flagship line, the
silently-no-op rename *and* its own wrong site count, and the controller's own
rustdoc error-count correction — sums to exactly eleven, matching the
document as written, and a search of the campaign's board posts and commit
history around its DoD commit (`c4cffa9e3`) found no distinct twelfth item
that commit omitted. The claim is recorded here as **unverified and most
likely a transcription error carried forward through a handoff**, not acted
on. The README's Scarf entry stays at eleven.

This is worth naming as its own small process finding: a claim about a
*headline number* can enter a handoff document, survive one campaign's plan
text unquestioned, and only get checked when the next campaign's own
process — "verify before changing" — is applied to it literally rather than
performed.

## What was deliberately not fixed, and why that is different from not noticing

`m07`'s witness surface — *"they killed them killed me and knowed me"* — is a
garden path, and the diagnosis (Common has no complementizer, and the
asymmetry between object- and subject-embedding is partly true of English too)
was sharpened by a second reviewer rather than accepted from the first
draft. It stays unfixed on purpose: a complementizer means a new `Part` in the
shared `common_constructions()` table that both `realize_common_with_subject`
and `parse_common_with_tail` read from one source, so it would move every
embedded surface this campaign shipped, including `m06`'s, and the parser
work Task 8 had just finished. Recording the reason alongside the omission —
not just the omission — is what keeps this a tripwire for the next campaign
rather than a silent gap it has to rediscover.
