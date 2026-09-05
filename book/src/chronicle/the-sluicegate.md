# The Sluicegate

The Sluicegate moved the merge queue's state machine out of shell and into a
unit-tested Rust tool, without changing a byte of the format it keeps or the
behaviour any caller sees.

The queue serialises the project's one canonical build box, and its state lived
in a tab-separated file managed by a bash script. Selecting a row and marking it
`running` were two separate commands, so the advisory lock — which dies with the
process that took it — was released between them. On 2026-09-04 two dispatchers
both saw the same unclaimed row and one merge executed twice: two logs of some
eight hundred kilobytes each for the same commit, both reporting success. The
defect was repaired in shell the same day, by making a single `claim` verb do
the selecting and the marking under one lock. This campaign is about why that
repair was not enough on its own.

The argument for moving is narrower than "shell is unsuitable", and the campaign
had to be corrected into making it. Lift the domain away and the shape is a
shared mutable resource coordinated by short-lived processes that talk through
inherited environment, with an invariant spanning more than one process
lifetime; the standard answer is to make the transaction boundary and the
process boundary coincide, and that answer is language-agnostic. Neither defect
this campaign reacted to *required* Rust. What the port buys is that the bad
shapes become hard to express and cheap to test: a claim that holds its lock
across the read-modify-write cannot be written as two statements, and a
parameter cannot leak into a grandchild.

That second half is not hypothetical. The shell repair coordinated through an
exported variable, which every phase and everything a phase ran inherited — so
a nested chamber run inside a test read its parent's claim, concluded its own
row was already taken, and skipped the interlock entirely. The hole the repair
closed reopened one process level down, in the same day, by the same hand.
The tool replaces that variable with a request identifier: the runner is told
which row authorised it rather than what to run, and there is nothing left to
export, inherit, or unset.

What landed is `tools/sluice`, a dependency-free crate whose every verb is a
function over an injected state directory — no library function reads the
environment, which is the discipline the campaign exists to buy. `File::lock`
on the pinned toolchain needs no crate and takes the same lock `flock(1)` does,
measured in both directions, which is what lets the ported verbs and the
unported `add` share one file safely. `scripts/sluice-queue.sh` survives as a
shim: three verbs forward to the binary, `add` stays in shell with its
three-valued ancestry and its headline refusal, and every caller is unchanged.
The seven-column format is untouched, and `scripts/queue-watch.sh` was not
modified by the campaign — which is the evidence, rather than the claim, that
the contract held.

`add` is deliberately unported. It sources the headline parser, resolves
ancestry three ways where exit 128 means *this box cannot answer*, coalesces
per branch and per kind, and must never supersede a running row; each of those
is a defended incident. A plan step that said "port the coalescing rules" was
rejected by its own review as a description of work rather than the work, and
the hybrid it leaves — ported verbs to the binary, `add` in shell — is a
stable end state rather than a half-migration.

The campaign reintroduced its own bug class three times before it was done.
A test overrode the isolation its harness provided and reset the very tree a
merge was running in; that run reported success and landed the pre-merge state.
A build invoked on the queue's hot path made a toolchain failure
indistinguishable from a verb failure, so the drain reported an occupied queue
as drained and the chamber proceeded outside the interlock. And the request
identifier that replaced the exported variable inherited the assumption the
variable had encoded — that somebody already held the row — without ever
asserting it, so a run resolved from an identifier walked past the interlock
with its row still queued. Each was found by an experiment rather than by
reading, and the third was pinned as correct behaviour by a passing test of
the campaign's own writing.
