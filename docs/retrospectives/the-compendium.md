# The Compendium — retrospective

**Merged:** 2026-08-15

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-compendium.md): a second corpus
family scoring the program rather than the world, five verdicts, an anchor
per verdict that the resolver re-checks, and one reading whose headline is
that the first unmet item is the tutorial's own **first page** — 2.1, *Entities
and Components*, `absent`, because no per-entity colour channel exists anywhere.
That headline said 2.6 and *refused* until two review rounds moved it; sections
6 and 7 are why.

## 1. This campaign built one resolver and produced four separate false-cleans in it

A false-clean is a guard reporting safety it does not have. Four of them, in
the anchor parser and audit — `cli/src/systems.rs` minus its rendering
functions and inline unit tests — whose *entire purpose* is to notice when a
citation stops being true. At this campaign's closing commit, `a386b784`
(2026-08-15), that resolution code was 984 of the file's 1,406 lines, the
rest split between rendering and the resolver's own inline tests — a
snapshot, stated with its commit and date rather than as a fact about "the
file" that could go stale, because an earlier draft of exactly this sentence
already did: it cited a count taken before the same commit's own additions
landed. In the order they were found:

| the defect | why it read as safe | found by |
|---|---|---|
| a mechanism anchor matched a symbol as a **bare substring**, so a citation of `fn foo` resolved clean against `fn foobar` | there were no `present` verdicts yet, so no fixture exercised the path at all | task review |
| the novelty ratchet used `?` **inside a `for` loop**, so the always-present banner line propagated a `None` out of the whole function — the guard could never fire | the function returned a plausible type and its unit tests passed | the implementer, dogfooding its own command |
| the stale-deferral check compared a status cell by **exact equality** to `shipped`, missing `shipped (C1)`, `**shipped**` and arrow transitions — **36 of the 206 rows then reading `shipped`, 17.5%** | the one fixture row used to derive the rule has the bare word as its status | the controller, preparing the next task |
| a mechanism anchor could cite an **`#[ignore]`d test** — one that never runs — and the raw-text resolver called it resolved | nothing in the design had ever named `#[ignore]` as a state | by accident, while hunting stronger evidence for an unrelated item |

**None was found by a passing test suite, and no two were found by the same
mechanism.** Review caught one. The implementer caught one by using the tool
rather than testing it. The controller caught one while reading ahead. The
fourth surfaced only because somebody went looking for *better* evidence for a
verdict that was already defensible, and tripped over it.

The uncomfortable reading is the fourth column, not the first: four different
detection mechanisms, each of which found exactly one. If any one of the four
had been skipped as redundant, a false-clean would have shipped inside an
instrument whose whole claim is *the anchor is the evidence*. Redundant
detection is not redundant when each detector has a different blind spot.

A fifth, of the same family, was found in **inherited** code and deliberately
not fixed: the heavy-tier gate's ignore-reason scan matches an attribute by
raw text and false-positived on a doc comment in this campaign's new code.
Changing a gate to suit a new caller is backwards, so it is captured as a row
instead.

## 2. Three of the four are the same shape: a fragile text match wearing a different face

Substring versus token. Exact equality versus a normalized token. Present in
the text versus actually compiled and run. Every one is *the same category
error* — treating a syntactic coincidence as a semantic fact — and this
campaign's own code committed it three times before anyone said the sentence
out loud. Writing it down after the first would not obviously have prevented
the second, because each looked like a different problem at the moment it was
made. What might have: a standing question asked of every match this resolver
performs — *what is the cheapest string that satisfies this check while being
the wrong thing?* — applied once per predicate rather than once per campaign.

**The same shape then escaped the code and infected the campaign's own
close**, which is why it is recorded here rather than filed under the
resolver. A wrong figure — "roughly a third of the tutorial is renderer work",
a pre-measurement estimate never re-derived after scoring — had been copied
from the spec into a decision record, the chronicle and `CLAUDE.md`. Removing
it took **three passes, each of which searched a smaller set than the claim it
then made**:

```
  pass 1 (review)      named 3 sites          -> missed 2
  pass 2 (my fix)      swept the files I had  -> missed 2 more, both inside
                       edited; claimed it        the diff's own file set
                       had grepped "the claim"
  pass 3 (re-review)   found the last 2
```

Neither pass was careless. Each returned a *true answer to a narrower question
than the one it asserted* — which is precisely the resolver's four
false-cleans, one level up and in prose instead of code. My second pass is the
worst of the three, because it was the one that explicitly claimed to have
grepped the claim rather than the files, and had not.

> A correction has a blast radius, and **the sweep for that blast radius has a
> blast radius of its own.**

The durable fix is mechanical and cheap: **grep the claim across the whole
repository before declaring a correction complete, not across the files you
happen to have edited** — and paste the full hit list with a disposition for
each, including the hits that are correct and must stay, so that "I searched"
is a readable artifact rather than an assertion. A quotation of the erroneous
original inside an amendment note is a legitimate hit; it is indistinguishable
from a survivor unless the sweep is written down.

## 3. The status defect was mine, and its shape is specific

The dispatch that produced the exact-equality bug told the implementer to
reuse the existing registry-ID parser and *"do not invent a second rule"* —
then handed them the **status** rule as my own finding: "status is cell index
2, trim whitespace," derived by running `awk` against exactly one row. That
row's status happens to be the bare word `shipped`. One sample cannot show a
parenthetical qualifier, a bold wrapper, or a transition arrow, and it did
not.

The repository already contained the right answer. `normalize_status`, sitting
in the very drift check the ID rule was borrowed from, is documented as
reducing a status cell to its bare token by stripping emphasis, a trailing
arrow transition, and any trailing parenthetical. The fix was to reuse it
byte-for-byte, which is what the dispatch had demanded for the *other* field.

Two lessons, and the second is the transferable one:

- A rule derived from one instance is an anecdote **even when the instance is
  real**. The awk was correct. The row was correct. The generalization was
  false.
- **Telling someone else to reuse a precedent while not reusing it yourself is
  a distinct failure**, not a mild version of the first one. It arrives
  wearing the authority of the instruction it sits beside, so the implementer
  had every reason to treat the handed-over rule as equally checked. The
  dispatch would have been safer with *no* status rule in it at all.

## 4. Two controller figures were wrong; implementers derived both and corrected them

The plan asserted 75 corpus items. The real figure is **74** — my count
quietly included an unnumbered front-matter page alongside the numbered ones.
The dispatch for the surplus read asserted 19 uncited subsystems. The real
figure is **20** — one cited crate maps to a directory that was never in the
enumerated set, so five directories are cited, not six.

In both cases the implementer derived the number independently, verified it by
hand, reported the correction, and did **not** code to my figure. That is the
behaviour the process wants and it should be recorded as having happened
rather than assumed. It is also the second half of section 3's lesson: a
controller's number carries the same unearned authority a controller's rule
does, and the only thing that reliably strips it is somebody re-deriving it.

Worth noting what made re-derivation cheap: both figures were **countable from
a committed artifact**, so checking cost minutes. A controller figure that can
only be checked by re-running an expensive measurement gets believed by
default.

## 5. The novelty ratchet fired for real, on the campaign's own correction

Re-verdicting one item after review moved the `absent` count from 9 to 10, and
the ratchet went red with the message it was designed to produce. This is the
one guard in the campaign that was observed failing against a *genuine* change
rather than a synthetic fixture, and it happened because the campaign corrected
itself rather than because anyone arranged a demonstration.

It has since fired a second time, on a much larger correction: the
weakest-half re-verdict of section 6 took `absent` from 10 to 11 and the guard
went red again, before the rebaseline, exactly as designed.

Two things follow. The guard's first real firing being self-inflicted is the
best available evidence that it is pointed the right way — the deliberate
rebaseline that followed is exactly the human act it exists to force. And the
correction it caught was a verdict that had been over-generous: one `present`
in twenty-six, found by **internal inconsistency** (the verdict sat directly
above its own note conceding the chapter's real subject was absent, and its
sibling chapter read `absent`) rather than by anyone's opinion about the code.
An instrument that authors verdicts about itself will produce self-flattery;
the useful question is not whether it happens but whether the artifact carries
enough internal structure for a reader to catch it.

## 6. The instrument silently changed subject, and every anchor resolved

The largest defect of the campaign was not in the resolver. It was in the
corpus, it survived five reviews, and Nathan found it at G6 by doing the one
thing none of the reviews did: reading the client.

**Decision 0022 was applied in two opposite directions, and which direction a
chapter got tracked which produced the more favourable verdict.** A chapter
that was mostly simulation had its sim half scored `present` and its render
half waved away as 0022's business — chapter 2.1's own note said exactly that,
in the artifact, published: "the chapter's other half — drawing an `@` — is
decision 0022's, not the sim's". A chapter that was mostly rendering had the
*whole* chapter scored `refused` under that same decision. Eight of them.

Three things make this worth a section rather than a line.

**The anchor discipline is structurally blind to it.** Every one of those
anchors resolved, on every run, because every one was a real test over a real
sim mechanism. The resolver checks that a citation still points at something.
It cannot check that the citation is evidence for *the question the row is
answering*, and a verdict that quietly narrows its subject to the half it can
evidence will be green forever. This family's entire safeguard — the thing
decision 0135 exists to install — does not see the failure mode that actually
bit. `an-empty-diff-needs-a-positive-control` has a sibling: **an anchor that
resolves needs a check that it answers the right question**, and nothing
mechanical can supply one.

**The missing thing was a rule nobody had written.** There was no policy for
scoring a chapter that spans the sim and the client, so each chapter got
whichever half the author found easiest to evidence, and the bias was
invisible per-chapter and obvious in aggregate. The fix is one sentence
(0135's clause 2: the corpus scores the whole program, and a spanning item
takes its **weakest half**), and it could have been written before any verdict
existed. `imperative-mood-hides-assertions` again: "score each chapter" is an
instruction; "a spanning chapter takes its weakest half" is a decision rule,
and only the second one can be violated visibly.

**And the correction ran in both directions, which is the test of whether a
correction is honest.** Re-auditing all 74 items moved twelve verdicts: four
demotions where the render half is genuinely missing (you can `delve` into the
caves and no renderer can draw them — the underground band folds into the walk
band, so the pane and the verb agree on a chart of the country *overhead*), and
six promotions out of `refused` where the clients do the work (a journal-spread
UI, a pan-and-zoom viewport in the atlas, text composited independently of the
map in the Casement). A pass that had only demoted would have been a different
kind of wrong. The count of items refused under 0022 went from eight to zero,
and every surviving refusal in the corpus is decision 0070.

The cheap generalisation, worth carrying to the next corpus in this family:
**name the subject boundary before authoring any verdict, and write it as a
rule.** "Does Hornvale do this?" is not a question until you have said what
Hornvale is.

## 7. Colour: three wrong answers in one session, and the code was never wrong

Section 6's defect has a sharper instance inside it, and it is the one worth
carrying forward, because nothing in the process caught it and the thing that
did was a human running the program.

The question was whether Hornvale draws its world in colour. It was answered
three times in one session, by the controller, and every answer was wrong:

1. **A client-side deferral the sim was past** — colour framed as something the
   renderers had not caught up to.
2. **Absent, agreed without verifying** — the opposite conclusion, adopted
   because the first was challenged rather than because anything was read.
3. **Present** — after finding `windows/scene/src/surrounds_ascii.rs`'s
   `\x1b[38;2;` truecolor path and asserting from its existence that Hornvale
   puts colour onscreen.

**Each was a true answer to a narrower question than the one asserted.** The
truecolor path is real and reachable. What none of the three checked is the
*default*: `PossessOpts::default()` selects `Lens::Off`, and `session.rs`
documents that as returning the pre-lens output byte for byte — no tint, no
escape sequence — which is why all four committed `scripts/possession-*.txt`
transcripts contain **zero escape bytes**. A player sees no colour in the
terminal until they type `eyes <species>`. Nathan found it by playing the game.

The corrected picture is not "colour is off" either, and flattening it that way
would be the same error facing the other direction: `eyes` defaults to `Own`,
so the emitted scene document *does* carry per-cell colour and the browser
client renders it unasked, while the terminal game client is monochrome by
construction. Two independent switches, opposite defaults, three renderers
disagreeing. Any single sentence about "colour" is wrong about at least one of
them.

Three things to carry:

- **A code path is not a default.** Reading `\x1b[38;2;` and concluding the
  screen is coloured is the same move as reading a feature flag's
  implementation and concluding the feature is on. Grep found the capability;
  nothing grepped the configuration. `measure-dont-narrate-the-mechanism` in
  its sharpest form.
- **Five agent reviews read the code and missed it; one human ran the
  program and did not.** The reviews were not lazy — they verified anchors,
  opened mechanisms, re-derived counts. They were reading the same artifact the
  claim was made from. Running the thing is a *different instrument*, not a
  more diligent application of the same one, and this campaign has no cheap
  substitute for it.
- **Say which surface and whether the player opted in.** That sentence is now
  in `windows/CLAUDE.md` beside the rendering rules, because the next reader
  will find `surrounds_ascii.rs:79` exactly as three readers already have. It
  is also in the corpus's own provenance, where it is met before any tally.

The verdict consequence was not cosmetic: chapter 2.1, *Entities and
Components*, went to `absent` — on the stricter and unconditional half of the
finding, that **no per-entity colour channel exists on any path** (`Mark`
carries `noun`/`kind`/`datum`/`salience`) — and with it the corpus's headline
became "the first page Hornvale cannot replicate is the tutorial's *first*
page." Which is where Nathan's reading of the project had been before the
campaign began.

## 8. Two Minor findings were ruled into fix rounds, deliberately

Minor findings do not normally reopen a round. Twice they did here, and both
overrides used the same argument: this instrument's entire claim is that *the
anchor is the evidence*, so a probably-right verdict resting on a
not-quite-right citation costs more than the fix does. A file-exists-but-empty
case was promoted for the same reason — an empty in-force set would have made
every decision anchor dangle at once, reading as catastrophic instrument
failure rather than as a missing file.

The general form: **severity should be judged against what the artifact
claims, not against what the code does.** A cosmetic defect in an instrument
that publishes a number is not cosmetic.

## 9. A spec claim about enforcement had to be amended mid-campaign, not inherited

The spec said the ratchet "runs in the gate," written against a single gate
that no longer exists. The first task absorbed sixty-six commits carrying the
gate-ladder split, and under it the local commit gate runs only tests with a
recorded baseline duration — so a *newly written* test is excluded by design
and enters only after a green stage gate measures it.

Inherited unamended, a green local commit gate immediately after writing these
tests would have read as evidence the ratchet worked, when it had not run at
all. The spec gained a section stating which gate actually enforces and from
when. Nothing in the design was damaged; the claim simply had to be corrected
rather than left standing, and the amendment belongs in the spec because that
is where the next reader will look for it.

## 10. What held up

**Freezing the catalogue before authoring any verdict.** The corpus shipped
with every verdict null and its item count asserted, one task before anything
was measured. When the count turned out to be 74 rather than 75, the freeze
was the thing that made changing it a deliberate, visible act instead of a
silent adjustment.

**Refusing to mint a registry row to escape an `absent`.** Twice, an item
could have been upgraded from `absent` to `deferred` by writing a row that
said the project planned the thing. Both were declined with a written reason.
The instrument's `absent` count is its only unfalsifiable verdict and the only
one the ratchet watches; inflating the backlog to improve it would have
corrupted both.

**A fix landed in the task that owned the file, not the task that found it.**
The status defect was found while preparing the verdict-authoring task, and
was fixed by reopening the resolver task with its original implementer, whose
context was intact. Folding a resolver bugfix into the authoring task would
have muddied both diffs and put the fix outside the review that owns that
file — at the cost of one extra cycle, sequenced so two implementers never
held the same file at once.

## Follow-ups

Captured as registry rows rather than folded in:

- **`TOOL-system-probes-are-executable`** — read a verdict off a transcript
  rather than a citation, scoped to the items where a transcript would
  discriminate.
- **`TOOL-surplus-granularity-is-coarse`** — the surplus read counts crates, so
  one citation reads as full coverage.
- **`TOOL-refusal-needs-a-decision-anchor`** — four items read `absent` where
  the project's real position is a refusal recorded in a registry row rather
  than a ratified decision. Both reviewers judged `absent` correct under the
  rules and the gap real.
- **`TOOL-ignore-scanner-matches-prose`** — the inherited fifth false-match of
  section 1, and the mirror of an already-banked false-negative in the same
  scanner.
- **`TOOL-normalize-status-is-tail-blind`** — a status transition's tail is
  invisible to the shared normalizer, so `raw → shipped` would not fire the
  stale-deferral check. No such row exists today; mirroring the precedent
  exactly was the requirement, and diverging would make two consumers disagree
  about one row.
- **`TOOL-mechanism-anchor-matches-prose`** — the raw-text mechanism scan
  accepts a signature that appears in a comment or a string literal; a live
  instance already sits in the tree.
- **`TOOL-worktree-take-branches-from-stale-origin`** — a recycled worktree
  starts from `origin/main` with no warning when the local `main` is ahead. Bit
  at this campaign's start. Banked as `raw` on a single observation, which is
  what `raw` is for: the alternative on offer was leaving it uncaptured because
  it could not be verified without running the command mid-campaign, and an
  unverified row beats a lost one.
