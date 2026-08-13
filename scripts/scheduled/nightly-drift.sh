#!/usr/bin/env bash
# scripts/scheduled/nightly-drift.sh — the drift check, unattended.
#
# THE CONSTITUTIONAL CONSTRAINT OF THIS DIRECTORY: a scheduled job NEVER
# commits and NEVER touches main's working tree. It reports. The precedent is
# decision 0129's lane rule — "the lane must never be wired to auto-implement a
# suggestion... that would make the board self-modifying with no human in the
# loop" — and the concrete hazard is a nightly job committing while a session
# is mid-landing, which `make preflight` warns about and cannot prevent.
#
# Wall-clock time is used freely here. The determinism ban governs the SIM;
# scheduling is outside that boundary, exactly as clients/ is.
set -uo pipefail

REPO="${HV_SCHED_REPO:?HV_SCHED_REPO must name a checkout this job owns}"
cd "$REPO" || exit 1

git fetch origin --quiet || { echo "nightly-drift: fetch failed" >&2; exit 1; }
git reset --hard origin/main --quiet || exit 1

make rebaseline >/tmp/hv-nightly-drift.log 2>&1
rc=$?

paths="$(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | tr '\n' ' ')"
# shellcheck disable=SC2086
drift="$(git diff --stat -- $paths)"

# Leave the checkout clean regardless — this job owns no changes. `--quiet`
# must come BEFORE the `--` pathspec separator: `git checkout -- . --quiet`
# treats `--quiet` as a pathspec, fails, and (because the failure is
# swallowed below) silently leaves the working tree dirty — caught by the
# dry run in Step 4, not by reading.
git checkout --quiet -- . 2>/dev/null || true

if [ "$rc" -ne 0 ]; then
    make board-post KIND=technique BY=scheduler PATHS='scripts/' \
      NOTE="nightly-drift: make rebaseline FAILED on main (rc=$rc). Tail: $(tail -5 /tmp/hv-nightly-drift.log | tr '\n' ' ' | tr -d '"')" || true
elif [ -n "$drift" ]; then
    make board-post KIND=notice BY=scheduler FIELDS='polarity=fyi' PATHS='book/ docs/audits/ docs/digest/' \
      NOTE="nightly-drift: main has UNCOMMITTED generated-artifact drift. Someone merged without running make rebaseline. $(echo "$drift" | tr '\n' ' ')" || true
fi

make board-sync >/dev/null 2>&1 || true
exit 0
