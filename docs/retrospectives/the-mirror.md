# The Mirror — retrospective

Merged 2026-08-23 (`173a55ddb`). Process lessons only. The product story is
in [the chronicle](../../book/src/chronicle/the-mirror.md); the finding is
registry row `TOOL-almanac-mirror-day-crossing`, shipped.

## One commit, one idea: put the guard where both halves are visible

The whole campaign was choosing the vantage for the missing comparison. The
obvious home — a worldgen test reaching into the almanac — is backwards: the
composition root cannot see downward, and seam-guard could not be pointed at
either half anyway (name-keyed seams, `TOOL-seam-guard-path-keyed` still
open). The working shape inverts it: the tests live in the *mirror's* crate,
where the private fns are visible, and reach **up** through a dev-only
dependency on the composition root. Hearsay had already paid for that
precedent; reusing it cost one Cargo.toml stanza and one enforcer rebaseline.

## Mutation-check a guard before you trust it

All three parity tests passed on first run — which proves nothing about
whether they would fail when they should (a test suite that asserts nothing
also passes). Two quick sed mutations — drop the crossing in `present_year`,
drop it in `record_of` — turned exactly the right test red and no other.
Five minutes of deliberate damage bought the difference between "green" and
"guards". This is the same lesson the heavy-tier red-allowlist row keeps
re-deriving from the other side.

## Parity alone is not enough

The first test written compared the mirror to its original — which stays
green if both halves drift identically, the exact failure mode duplicated
logic has. The unit-pinning test (bake year 200, not ledger day 73,050)
exists only because that hole was noticed while writing the second test.
When guarding a duplicate, pin the property directly AND compare the halves;
each covers the other's blind spot.

## Process friction worth recording

- The merge request was refused once for a missing `Sluice-Headline` trailer
  (fair — the refusal names the exact fix), and the amend-then-force-push of
  an unmerged branch then tripped `pre-push`'s non-fast-forward check,
  needing `HV_PUSH_OK=1` on the campaign branch itself. The escape works,
  but its name says "hotfix to main" while its real meaning is "deliberate
  history rewrite anywhere".
- The stage gate absorbed two main movements (`49bf2427c` → `b7583e489`)
  without local action — the first campaign this session has run where
  absorption required nothing at all. Working as designed; noting it because
  quiet is what success looks like here.
