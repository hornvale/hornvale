# The Cartulary — retrospective

**Merged:** 2026-08-30

## The campaign's method was its own subject, and it caught itself failing at it

This campaign existed because five prior campaigns lost decision-ledger
material at close. Its remedy — a committed, per-campaign ledger, written to
contemporaneously rather than promoted at the end — was tested against the
exact failure it was built to prevent, and it worked (H1, confirmed, on
stronger ground than the task claimed: the reviewer read
`scripts/worktree-take.sh`'s real mechanism and found the clone simulation is
a *harder* test of recycling than the thing it was standing in for, not a
weaker one).

But the campaign's second hypothesis — that committing does not change
candour — found a live instance of exactly the failure H1 disproves the
*mechanical* version of. A self-critical line survived in this campaign's own
ephemeral scratch report and did not survive into the committed ledger, same
task, same author, minutes apart. Not proof (an unblinded single pair cannot
separate "committed, therefore softened" from "second pass, therefore
tightened"), but a real, confounded observation, found because a cheap
instrument existed and someone finally applied it to the campaign's own
material instead of only to what it was built to protect.

## The sibling sweep earned itself

Task 4 was asked to re-point three skills' stale ledger references and, while
doing it, to sweep each edited skill for *siblings* of the same defect — any
step naming a mechanism (a path, a grep, a command) rather than a thing to
find. It found one: `closing-a-campaign`'s Quick Reference table had a
"Scratch promotion" row restating the exact stale instruction the targeted
fix in step 2 had just corrected. A fix scoped to the reported instance would
have shipped a document that contradicted itself in two places instead of
one. The instruction to sweep for siblings is cheap to write into a task
brief; this is a case where it produced something the targeted fix,
executed perfectly, would not have — which is the argument for keeping the
instruction rather than trusting a careful implementer to think of it
unprompted.

The re-review repeated the discipline one level up: it re-derived the sibling
sweep independently rather than trusting the first pass's method, found that
an anchored `grep '^```'` missed indented code fences, widened the search,
and re-ran it. The same habit that found the first sibling caught a gap in
its own search a task later.

## A fix can carry its own defect, and this one was checked rather than assumed clean

Task 5's review found a candid line dropped between an ephemeral scratch
report and its matching committed ledger entry — the exact shape of loss this
campaign exists to prevent, now demonstrated inside the campaign itself. The
obvious remedy is to write the missing line into the ledger. The obvious risk
is that a second pass at writing a self-critical sentence produces a
*softer* version of it — grammatically a fix, substantively the same defect,
now harder to see because something now occupies the space where the honest
line should be.

That trap was checked, not assumed absent: the reviewer put the scratch
report's original wording and the ledger's added wording side by side,
verbatim, and confirmed they matched character-for-character, with only the
incorrect framing (the earlier ledger entry's claim that spec §9 already
covered this) removed rather than softened. The general lesson is not
"remedies can be defective," which every retrospective in this project
already assumes — it is that *this specific class* of remedy (restoring
dropped candour) has an unusually easy way to look complete while quietly
reintroducing its own defect, because the reader checking it is comparing
prose against prose rather than a number against a number, and a softened
paraphrase reads as a successful restoration unless someone puts the two
wordings next to each other.

## H2's unfalsifiability was authored, not discovered, and that distinction matters

The spec preregistered H2 ("committing does not change what gets written")
together with its own author's prediction that it does, and an admission
that this "probably" cannot be measured from inside one campaign. Task 5's
first pass reported "unmeasurable, as preregistered" and treated that as the
honest, structurally-forced answer. Review pushed one level further: nothing
about H2 itself is unmeasurable — a blinded twin-campaign study could settle
it. What guaranteed the unmeasurable report was a spec that preregistered
failure-to-measure instead of an instrument, which produces "unmeasurable"
regardless of whether the underlying claim is true. **Preregister the
instrument, not the expectation of failing to find one** — a preregistration
step that only records what you expect not to learn is not doing the job
preregistration exists for.

## Recurring findings

- **A record that outlives its subject produces wrong answers**, restated
  once more, this time inside the campaign's own edited skill:
  `closing-a-campaign`'s step 2 named a discovery mechanism
  (`.superpowers/sdd/`, a grep) rather than a thing to find, and the fix that
  moved the ledger left that instruction pointing at the ledger's old
  location. Same shape as the CLAUDE.md corrections several prior campaigns
  have logged; this project keeps finding it because *every* migration
  leaves exactly this kind of stale pointer behind unless something is asked
  to look for it specifically.
- **A harness caching issue was caught by verifying `git diff` after every
  edit, not by trusting a tool's reported success** (Task 4) — the Edit tool
  reported success twice for edits that never reached disk, and then failed
  to match text it had itself reported as applied. Nothing wrong shipped
  because the implementer checked; an agent that trusted the return value
  would have committed a change silently missing edits it believed it had
  made.

## Estimate deltas

None material — five of six tasks landed with zero or one fix round, in line
with the campaign's own low-risk, docs-and-tooling shape. Task 5's fix round
was the deepest of the six and was also the one task whose subject was the
campaign measuring its own honesty, which is not a coincidence worth reading
past.

## Do differently next time

When a spec preregisters "I expect this will be unmeasurable," treat that
line as a request to either build the cheap instrument that exists or state
explicitly why none is being built — not as license to report the predicted
non-result at face value. And when a fix restores dropped or softened text,
diff the restored wording against the original byte for byte before calling
it complete; a prose comparison this project can otherwise only eyeball.
