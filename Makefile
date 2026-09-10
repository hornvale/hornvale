# Hornvale developer task runner (TOOL-14).
#
# Encodes the cost-ordered gate CLAUDE.md describes as prose, so the ordering
# stops being tribal knowledge re-derived each session. `just` is not a repo
# dependency; this uses `make`, already present everywhere.
#
#   make quick        # cheap half: fmt --check + clippy + type-audit
#   make gate-commit  # THE PRE-COMMIT GATE: lints, tripwires, and the sub-floor test tier
#                     # (local; ~10-16 s on a clean tree, up to ~470 s after a
#                     # kernel/-layer edit — cost is the edit's blast radius in
#                     # the kernel -> domains/* -> windows/* -> cli layering;
#                     # see spec 2026-08-14-the-staff-design.md §4.2b)
#   make sluice       BRANCH=<branch> REF=<full-sha> # THE MERGE QUEUE: gates the merge product and pushes it (decision 0139)
#   make sluice-stage BRANCH=<branch> REF=<full-sha> # THE STAGE GATE: the same chamber, same phases, no push
#   make sluice-status # what is queued, running, held, landed, reported
#   make sluice-log    # read a finished chamber job back (JOB=<id>, or omit for the most recent)
#   # `make gate`, `ci`, `gate-fast`, `gate-full`, `gate-campaign`, `gate-stage`,
#   # `preflight` and the whole `lane*` family are RETIRED (decisions 0132,
#   # 0139) and now refuse with a nonzero exit, naming their replacements:
#   # gate-commit and the two queue entry points above.
#   make prewarm      # warm a fresh worktree's target/ (start right after worktree add)
#   make worktree-take NAME=<campaign> [BASE=main] # claim a recycled pool worktree
#   make rebaseline   # regenerate committed artifacts EXCEPT censuses (refresh those with scripts/census-run.sh)
#   make rebaseline-goldens # accept drifted byte-golden test fixtures
#   make lab-diff STUDY=<name> # report which census metrics moved vs HEAD
#   make doctor       # print the repo self-map (orientation for a fresh session)
#   make install-hooks# point git at scripts/hooks (opt-in; edits local config)
#   make gate-remote  # ABANDONED (decision 0063): the AWS path is unused; kept only as history
#   make vessel-check  # the Casement's local gate: deno + wasm fmt/clippy + byte-identity smoke
#   make world-check  # the world catalog's local gate: fmt/clippy + byte-identity smoke + size gate
#   make game-check  # the game client's local gate: fmt/clippy/test on both crates + the containment guard
#
# Cost-ordered by design: fmt and clippy are cheapest and the most common
# review finding, so they run first; `--workspace` tests are the final step.

.PHONY: context context-prepare absorb decision-block decision-blocks help quick quick-run gate-commit gate-commit-run style-run subfloor-run gate-stage gate-campaign gate-suite-run gate gate-run gate-fast gate-full ci seam-guard seam-guard-list heavy-remote heavy-status heavy-log lane lane-status lane-log lane-roster lane-wait sluice sluice-stage sluice-census sluice-status sluice-log nextest-check docs-tests prewarm prewarm-run worktree-take sweep sweep-dry sweep-exact sweep-check fmt fmt-check clippy type-audit type-audit-report placement-audit placement-audit-report plumb plumb-report test rebaseline artifacts rebaseline-goldens regen-remote lab-diff timings preflight doctor shapecheck install-hooks gate-remote gate-remote-verify gate-panic gate-remote-setup gate-remote-teardown shellcheck observation-check census census-query census-history census-check wasm-vessel vessel-check vessel-check-run wasm-world world-check world-check-run wasm-lot game-check game-check-run visual-check visual-check-run atlas-check lot-check lot-check-run clients-check-run board board-digest board-post board-redact board-sync

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "} {printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}'

# Export the raw Make value as environment data, without shell interpolation.
export HV_CONTEXT_SCOPE = $(value SCOPE)
context: ## Collect checked context from this checkout (SCOPE=domains/thing or .)
	@test -n "$$HV_CONTEXT_SCOPE" || { echo 'usage: make context SCOPE=<scope>' >&2; exit 2; }
	@env -u CARGO_TARGET_DIR -u CARGO_BUILD_TARGET -u CARGO_BUILD_TARGET_DIR cargo run --quiet --manifest-path tools/digest/Cargo.toml --package digest --bin digest --target-dir "$(CURDIR)/tools/digest/target" --locked --offline -- context "$$HV_CONTEXT_SCOPE"

context-prepare: ## Prepare Digest workspace dependencies/builds (may update Cargo.lock)
	@env -u CARGO_TARGET_DIR -u CARGO_BUILD_TARGET -u CARGO_BUILD_TARGET_DIR cargo build --manifest-path tools/digest/Cargo.toml --workspace --target-dir "$(CURDIR)/tools/digest/target"

quick: ## Cheap half of the gate (fmt-check + clippy + type-audit + type-audit-report + placement-audit + placement-audit-report + plumb + plumb-report)
	@bash scripts/timed.sh quick -- make --no-print-directory quick-run

quick-run: fmt-check clippy type-audit type-audit-report placement-audit placement-audit-report plumb plumb-report

gate-commit: ## THE COMMIT GATE: lints, tripwires and the sub-floor test tier (local; ~10-16 s clean, up to ~470 s after a kernel/-layer edit — see spec §4.2b)
	@bash scripts/timed.sh gate-commit -- make --no-print-directory gate-commit-run

gate-commit-run: style-run subfloor-run

# NO FRESHNESS CHECK HERE. An earlier draft called
# scripts/test-worktree-freshness.sh from this recipe. Ruled out on
# measurement (R5): target/debug/deps holds 24,544 files and the scan costs
# ~1 s per sibling worktree, so with six worktrees it is ~5 s on EVERY commit
# gate — a third of the whole budget, paid continuously, for a condition that
# arises exactly once, at worktree-take's `mv`. Task 1 placed the call in
# scripts/worktree-take.sh instead, which is where the condition is created.
style-run: fmt-check clippy type-audit type-audit-report placement-audit placement-audit-report plumb plumb-report

# THE SUB-FLOOR TIER. Selection is EXCLUDE-UNKNOWN: a test absent from the
# roster is not run here, and enters on the next green chamber `gate` phase
# (a merge or a `make sluice-stage`), which measures it and rewrites the file.
# That inverts this repo's default-deny instinct deliberately — the commit gate
# is a SPEED tier and coverage is the stage gate's job. Defaulting the other
# way was measured at 178.6 s against a ~14 s estimate.
#
# Exit 3 from the roster script means NO ROSTER for this host, which is a
# different thing from an empty roster and must not read as a green gate.
#
# WHY THE ROSTER'S LINE COUNT DOES NOT MATCH THIS TARGET'S TEST COUNT, AND WHY
# THAT IS EXPECTED. docs/timings/subfloor-roster.tsv carries 2748 non-comment
# lines but only 2730 DISTINCT trailing test names: 12 names are each
# duplicated across crates, which is what makes those two counts differ
# (2748 - 2730 = 18 excess LINE-occurrences of an already-seen name — a
# property of the roster FILE). A Mac's
# `cargo nextest run --workspace -E "$$filter"` then selects 2746 tests, not
# 2730 — a different number counting a different thing (excess SELECTED
# TESTS at run time, not excess lines; see the +19 bullet below). Two
# effects, opposite in direction, explain the run-time number:
#   -3  three roster names match ZERO tests in this host's compiled binary:
#       `census_claim::tests::a_claim_naming_a_dead_pid_is_stale`,
#       `a_live_ancestor_holding_the_lock_makes_a_claim_a_no_op` and
#       `a_live_claim_is_reported_with_its_context`
#       (windows/lab/src/census_claim.rs) are `#[cfg(target_os = "linux")]`-
#       gated, so they never exist in the test binary on Darwin at all. The
#       roster is authored on the canonical (Linux) gating host and read on
#       every host, by design (`subfloor-roster.sh`'s own header) — the cost
#       this buys is not host SPEED shifting membership at the margin, it is
#       PLATFORM GATING, which is categorical: a whole class of test can be
#       structurally absent from every Mac's commit gate while still reading
#       as present in the roster. Benign here — those three still run in the
#       stage gate on the canonical box — but worth knowing before treating a
#       roster/run count mismatch as a bug.
#  +19  twelve names collide across 2-7 crates each (nextest's `test(=NAME)`
#       matches by test NAME, not by binary — see subfloor-roster.sh's own
#       comment on why), so each such roster line over-selects every crate
#       sharing that name. Over-selection is safe: it costs a little time and
#       never hides a failure.
#   2730 - 3 + 19 = 2746, which is exactly what this target runs.
# EVERY NUMBER IN THAT ARITHMETIC IS A MEASUREMENT OF ONE ROSTER FILE, AND
# THAT FILE IS ABOUT TO MOVE FOR THE FIRST TIME. The roster had a single
# hand-authored commit in its whole history until The Sluice fixed the
# `ci-record` refusal that prevented it from ever being rewritten (root
# CLAUDE.md); the chamber now commits a fresh one with every green merge. So
# read the block above as an explanation of WHY the two counts differ — the
# platform-gating and name-collision effects are structural and permanent —
# and re-derive the four figures rather than trusting them. They will be
# stale by the next merge.
subfloor-run: nextest-check
	@tmp_filter="$$(mktemp)"; \
	trap 'rm -f "$$tmp_filter"' EXIT; \
	bash scripts/subfloor-roster.sh > "$$tmp_filter"; \
	status=$$?; \
	if [ $$status -eq 3 ]; then \
	    echo "gate-commit: no sub-floor roster for this host — the test tier is UNAVAILABLE, not empty." >&2; \
	    echo "gate-commit: run a green 'make sluice-stage BRANCH=<b> REF=<sha>' to author one." >&2; \
	    exit 1; \
	elif [ $$status -ne 0 ]; then \
	    echo "gate-commit: scripts/subfloor-roster.sh failed with an unrecognised exit status ($$status), not the empty-filter case." >&2; \
	    echo "gate-commit: read its stderr above; this is a script fault, not a policy verdict." >&2; \
	    exit 1; \
	fi; \
	if [ ! -s "$$tmp_filter" ]; then \
	    echo "gate-commit: the roster is EMPTY. That is never correct — it would make this gate vacuous." >&2; \
	    exit 1; \
	fi; \
	bash scripts/subfloor-run-chunked.sh "$$tmp_filter"

# RETIRED AS A DISPATCH PATH, NOT AS A GATE (The Sluice, Task 12). The stage
# gate still exists and still runs the same phases; it is now a queue entry
# with `kind=stage` instead of four independent lane dispatches. A SIGNPOST
# rather than an alias for the reason 0132 gives and one more: the argument
# SHAPE changed (a stage request needs a BRANCH as well as a REF, because the
# chamber merges main+branch and tests the product, where `gate-stage` tested
# a bare tip), so aliasing would fail obscurely at the far end instead of
# clearly here.
gate-stage: ## RETIRED (Task 12) -- the stage gate is a queue entry now: make sluice-stage
	@echo "make gate-stage no longer dispatches anything." >&2; \
	echo >&2; \
	echo "It ran four independent lane dispatches against a BRANCH TIP. The" >&2; \
	echo "stage gate is now a queue entry like a merge -- same mouth, same" >&2; \
	echo "chamber, same claim -- run against the real main+branch merge" >&2; \
	echo "product, and it never pushes:" >&2; \
	echo >&2; \
	echo "    make sluice-stage BRANCH=<branch> REF=<full-sha>" >&2; \
	echo "    make sluice-status        # the entry ends 'reported'" >&2; \
	echo >&2; \
	echo "Note the BRANCH: a stage gate tests the merge, not the tip." >&2; \
	exit 1

# RETIRED, NOT FOLDED INTO THE `gate ci gate-fast gate-full` SIGNPOST BELOW.
# That rule's own message points callers AT `gate-campaign` as the still-live
# replacement for the three names it refuses; this target's refusal has to
# say something different — point at the merge queue instead — so it cannot
# share that rule's text. Verified before writing this: nothing in this
# Makefile declares `gate-campaign` as a prerequisite of another target (the
# way `gate-full: gate` did for `gate`), so retiring it orphans no downstream
# target the way a naive `gate` signpost would have orphaned `gate-full`.
gate-campaign: ## RETIRED (decision 0139) -- the merge queue gates the merge product
	@echo "make gate-campaign no longer runs anything." >&2; \
	echo >&2; \
	echo "It gated a BRANCH TIP. What lands is that branch merged into whatever" >&2; \
	echo "main is at merge time, and nothing ever built that object -- which is" >&2; \
	echo "how two campaigns both minted decision 0134 through a green gate." >&2; \
	echo >&2; \
	echo "Use the merge queue, which gates the merge product and pushes the" >&2; \
	echo "exact SHA it tested:" >&2; \
	echo "    make sluice BRANCH=<branch> REF=<full-sha>" >&2; \
	echo "    make sluice-status" >&2; \
	echo >&2; \
	echo "The stage gate moved with it: make sluice-stage BRANCH=<branch> REF=<full-sha>." >&2; \
	exit 1

# The former `make gate` body, now a set that runs ON the lane
# (scripts/lane-sets.tsv's `gate` row: `make --no-print-directory
# gate-suite-run`). `sluice-run.sh` supplies the `timed.sh` wrapping
# that the old top-level `gate` target used to do itself; this target's own
# job is unchanged from before The Staff — run the cheap checks, then the
# nextest+doctest body below (gate-run), unchanged.
gate-suite-run: fmt-check clippy type-audit type-audit-report plumb plumb-report nextest-check
	@$(MAKE) --no-print-directory gate-run

# The gate's body, split out so `timed.sh` can wrap it. Until this split,
# docs/timings.md carried ZERO rows labelled `gate` (0086's amendment): the
# ledger built to catch a suite creeping "65s -> 43.5 min" was never wired to
# the most-run expensive command in the repo, so a 4-minute budget drifting to
# 15+ was never observable. Read them filtered — `scripts/timed.sh report
# gate` — because gates are frequent and will dominate the ledger by row count.
#
# THREE CORRECTIONS TO AN EARLIER DRAFT OF THIS RECIPE, all found by the
# one-task-ahead brief check and all load-bearing:
#
# (a) `nextest-check` STAYS a prerequisite. The earlier draft dropped the
#     `test` target as a prereq and called nextest directly, which silently
#     discarded `test`'s own `nextest-check` prereq — the target whose entire
#     job is to fail with an install hint when cargo-nextest is missing. A
#     machine without it would have got `command not found` instead.
#
# (b) THE DEFAULT PROFILE, NOT `ci`. `.config/nextest.toml` states that
#     `[profile.default]` is "deliberately left at nextest's own defaults:
#     `make gate` must behave exactly as it did before this campaign", and the
#     `ci` profile sets `fail-fast = false`. Running the gate under `ci` would
#     silently turn every red gate into a full-suite run — 368 times a month,
#     on the axis this campaign exists to protect. The durations the alarm
#     needs are complete on a GREEN run regardless of profile, and a green run
#     is the only run whose durations are ever recorded, so the `ci` profile
#     buys nothing here and costs fast red feedback.
#
# (c) THE ALARM RUNS ONLY ON GREEN. Consequence of (b), and correct
#     independently: under fail-fast a red run's `run.json` is TRUNCATED, so
#     alarming against it compares a partial suite to a whole-suite baseline
#     and can report a regression that does not exist. The spec already states
#     this principle for S7 — "a duration measured under a partial run is not
#     comparable to a baseline" — and it binds here first.
#
# THE GATE IS NOW ALSO THE MEASUREMENT (The Sexton, Task 3). `make ci` ran 9
# times against this target's 368: an instrument watching a gate that crept
# 234 s -> 934 s, running at 2.4% of that gate's frequency, while every gate
# already computed the durations it needed and threw them away.
#
# ORDER IS LOAD-BEARING, unchanged from ci-run: the alarm must compare this run
# against the baseline still on disk from the LAST recorded run, so it runs
# BEFORE ci-record overwrites that file. Recording first would make every run
# compare against itself and the alarm could never fire.
#
# A RED RUN NEVER BECOMES THE BASELINE — guarded on BOTH statuses, because the
# unguarded version was a one-way ratchet: the alarm fired at 2x and ci-record
# immediately wrote the inflated durations back as the new reference, erasing
# its own evidence.
#
# THE HUMAN OUTPUT IS TEE'D, NOT REDIRECTED. `--message-format libtest-json-plus`
# puts the JSON on STDOUT and the ordinary progress stream (Compiling…, PASS/FAIL
# lines, the Summary) on STDERR. Sending stderr straight to run.log left the
# most-run command in the repo printing NOTHING for ~7 minutes and pointing at a
# file on failure. That was tolerable at `make ci`'s 9 runs a month; at `make
# gate`'s 368 it is a daily regression, and staring at a silent terminal is how
# a gate starts looking hung. So stderr goes through `tee`: the file the alarm
# and the archaeology need still receives the complete stream, and the terminal
# gets it live.
#
# WHY THE `.rc` FILE AND NOT `PIPESTATUS`/`pipefail`. This Makefile sets no
# SHELL, so recipes run under `/bin/sh` — `dash` on lefford. `${PIPESTATUS[0]}`,
# `set -o pipefail` and `2> >(tee …)` are all bashisms that would work on this
# Mac and fail there, silently reporting `tee`'s exit status (always 0) as the
# gate's verdict — a gate that can never go red. Writing the status inside the
# brace group is POSIX and reads the same everywhere.
gate-run:
	@mkdir -p target/nextest/ci docs/timings
# THE STATUS FILE IS DELETED FIRST AND VALIDATED AFTER, and both halves are
# load-bearing. nextest's status reaches us through a file because the brace
# group runs in a subshell feeding `tee` (whose own status is always 0), and a
# POSIX-portable capture is required — `$${PIPESTATUS[0]}` is a bashism and
# this Makefile sets no SHELL, so under lefford's dash it would silently
# evaluate to nothing.
#
# But a status read from a file has two failure modes a `$$?` does not, and
# BOTH report GREEN:
#   - an EMPTY or unwritable file: `[ '' -eq 0 ]` is a syntax error, and `if`
#     reads an errored test as false, so the red branch never fires. Verified
#     directly: an empty rc file made the gate report green.
#   - a STALE file: if the brace group dies before reaching its `echo` (OOM
#     kill, SIGKILL — a shape this repo already documents for parallel heavy
#     jobs), `cat` returns the PREVIOUS run's value, which on a previously
#     green box is 0.
# So: remove it before the run, and treat missing / empty / non-numeric as a
# failure rather than as success. A gate that reports green when it does not
# know is worse than one that reports red when it is unsure.
	@rm -f target/nextest/ci/nextest.rc
	@{ NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1 cargo nextest run --workspace \
	    --message-format libtest-json-plus \
	    2>&1 1>target/nextest/ci/run.json; \
	   echo $$? > target/nextest/ci/nextest.rc; \
	 } | tee target/nextest/ci/run.log >&2; \
	nextest_status=$$(cat target/nextest/ci/nextest.rc 2>/dev/null); \
	case "$$nextest_status" in \
	    ''|*[!0-9]*) \
	        echo "make gate: FAILED — nextest's status file is missing, empty or non-numeric ('$$nextest_status'). The run did not complete; treating as RED." >&2; \
	        nextest_status=1 ;; \
	esac; \
	cargo test -q --workspace --doc; \
	doctest_status=$$?; \
	bash scripts/defect-ledger.sh target/nextest/ci/run.json || true; \
	if [ $$nextest_status -eq 0 ] && [ $$doctest_status -eq 0 ]; then \
	    cargo test -q -p hornvale --test suite -- timings_alarm --ignored --nocapture; \
	    alarm_status=$$?; \
	else \
	    alarm_status=0; \
	    echo "make gate: skipping the duration alarm — the run was red, so its durations are truncated and not comparable to a baseline" >&2; \
	fi; \
	if [ $$nextest_status -eq 0 ] && [ $$doctest_status -eq 0 ] && [ $$alarm_status -eq 0 ]; then \
	    cargo run --quiet -p hornvale -- ci-record; \
	else \
	    echo "make gate: NOT recording a baseline — the run was red, so these durations are not a reference" >&2; \
	fi; \
	echo ""; \
	echo "== detail written to =="; \
	echo "  target/nextest/ci/run.json   structured per-test durations"; \
	echo "  target/nextest/ci/run.log    human output, including failures"; \
	echo "  docs/timings/test-baseline-$$(hostname -s).tsv   recorded baseline"; \
	if [ $$nextest_status -ne 0 ]; then \
	    echo "make gate: FAILED — nextest was red (exit $$nextest_status); see target/nextest/ci/run.log" >&2; \
	    exit $$nextest_status; \
	fi; \
	if [ $$doctest_status -ne 0 ]; then \
	    echo "make gate: FAILED — doctests were red (exit $$doctest_status)" >&2; \
	    exit $$doctest_status; \
	fi; \
	bash scripts/census-advisory.sh || true; \
	exit $$alarm_status

# SIGNPOSTS, NOT ALIASES. Aliasing `gate` to the commit gate would silently
# change what 417 runs a month mean: a caller expecting the full suite would
# get lints plus the sub-floor tier and no warning. Refusing is the same shape
# scripts/hv-guard-bash.sh uses when it intercepts a raw whole-workspace
# `cargo test` and names the project's own targets instead.
# `gate-full` IS IN THIS LIST AND THE REASON IS NOT COSMETIC. It was declared
# `gate-full: gate` — a prerequisite — so the moment `gate` becomes a refusing
# signpost, `gate-full` would inherit the refusal and stop doing its job. It is
# also genuinely superseded: `gate-full` was `gate` + the heavy tier, and
# `gate-campaign` was the stage sets + heavy + census, which strictly contains it.
# Leaving it as a live target pointing at a dead prerequisite would be the
# worst of both. Verified before this task: `gate-full` and `ci` were the only
# two targets that took `gate` as a prerequisite.
# `gate-fast` IS ALSO RETIRED (n=4 measurement: it only bought ~10% over the
# full gate) rather than pointed at a set — see docs/decisions/0132.
# `gate-campaign` ITSELF IS NOW RETIRED TOO (decision 0139, The Sluice), so
# this message no longer names it as a live replacement — it points at the
# merge queue instead, which is its own signpost below with its own message.
gate ci gate-fast gate-full:
	@echo "make $@ no longer exists. Since The Sluice, merging goes through" >&2
	@echo "the merge queue, and only two gates remain before it:" >&2
	@echo "" >&2
	@echo "  make gate-commit                    local, seconds, every commit" >&2
	@echo "  make sluice-stage BRANCH=<branch> REF=<full-sha>   the queue, each plan-stage boundary" >&2
	@echo "  make sluice BRANCH=<branch> REF=<full-sha>   the merge queue, before merging" >&2
	@echo "" >&2
	@echo "gate-full is superseded by the merge queue, which gates the merge" >&2
	@echo "product rather than a branch tip." >&2
	@echo "" >&2
	@echo "The heavy tier alone: make heavy-remote REF=<full-sha>" >&2
	@echo "The roster:         scripts/lane-sets.tsv" >&2
	@exit 2

# Deliberately NOT in the commit gate: each registered call site costs a full
# scoped test run, so cost scales with the roster. It is its own `campaign`
# rung set in scripts/lane-sets.tsv (The Staff, Task 8's fix round) --
# briefly folded into the `outboard` stage set, then split back out once
# measurement showed its 7 sites were 97% of that set's 855.222 s wall time.
# What a seam guards (which functions no test pins) moves only when seams or
# tests change: slow-moving and campaign-shaped, not a per-plan-stage-boundary
# cadence, and the same place it lived pre-Staff (the old gate-full evidence
# tier) for the same cost reason.
#
# REFUSES ON AN UNCLEAN WORKING TREE, AND THAT IS CORRECT, NOT A BUG. The run
# rewrites real source files in place and restores them; on the lane this
# always operates on a fresh checkout of a committed ref, so it never sees a
# dirty tree. If a local dev loop hits the refusal, that is the guard
# working -- commit or stash first, do not "fix" the check.
seam-guard: ## Neutralise each registered seam and report the ones no test notices
	cargo run --quiet --manifest-path tools/seam-guard/Cargo.toml -- run

seam-guard-list: ## Print the registered seams and their call sites (cheap, no build)
	@cargo run --quiet --manifest-path tools/seam-guard/Cargo.toml -- list

# The claim lives in the canonical box's OWN /tmp, so a local `heavy-run.sh
# status` answers "is a heavy run holding THIS machine?" — from the Mac that is
# always no, and is not the question you meant. This target asks the box that
# actually holds the claim.
heavy-status: ## Ask the canonical box whether a heavy run is holding it (The Siding)
	@ssh lefford 'cd ~/Projects/hornvale && scripts/heavy-run.sh status'

# Read back what the canonical box's heavy runs actually did. Exists because
# observing an expensive run through the CALLER's plumbing means any surprise
# costs the whole run again; heavy-run.sh emits this instead.
heavy-log: ## Show the canonical box's recent heavy-run outcomes and the latest log tail
	@ssh lefford 'd=$${HV_HEAVY_LOG_DIR:-/tmp/hornvale-heavy}; \
		echo "== outcomes (utc, why, rc, wall_s, sha, log) =="; \
		tail -10 "$$d/runs.tsv" 2>/dev/null || echo "  (no runs recorded yet)"; \
		echo; echo "== tail of the most recent log =="; \
		latest=$$(ls -t "$$d"/heavy-*.log 2>/dev/null | head -1); \
		if [ -n "$$latest" ]; then echo "-- $$latest"; tail -30 "$$latest"; \
		else echo "  (no logs yet)"; fi'

# Pass a SHA, not a branch name: HV_HEAVY_REF feeds `reset --hard`, which can
# otherwise land on a stale LOCAL branch of that name on the canonical box.
# heavy-run.sh echoes the resolved HEAD so you can check what actually ran.
heavy-remote: ## Run the heavy tier on the canonical box (The Siding); REF=<full-sha> required
	@test -n "$(REF)" || { \
		echo "usage: make heavy-remote REF=<full-sha>"; \
		echo "  push the branch first; the heavy tier authors committed artifacts"; \
		echo "  and may only run on the canonical box (decisions 0063/0079)."; \
		exit 1; }
	ssh lefford 'cd ~/Projects/hornvale && HV_HEAVY_REF=$(REF) scripts/heavy-run.sh'

# THE LANE'S FIVE TARGETS ARE GONE, AND EACH ONE'S WORK WENT SOMEWHERE
# NAMEABLE (The Sluice, Task 12). This is one refusing rule rather than five,
# because they share an answer: the asynchronous dispatch layer they were the
# front end of no longer exists, and the queue's own two readers replace all
# of the reading half.
#
# `lane-roster` deserves its own sentence, because deleting it looks like the
# loss of the only path to updating `docs/timings/subfloor-roster.tsv` and is
# the opposite. That copy-out could never fire: `ci-record` refused whenever
# ANY claim was held, and the lane always held one, so the file it copied was
# always the unchanged committed one and `git diff --quiet` skipped the copy.
# The roster has exactly one commit in its whole history, authored by hand.
# Task 12 fixed the actual blocker (`contending_holder`, cli/src/main.rs), so
# the chamber's `gate` phase now genuinely rewrites the file — and the phase
# loop commits it and pushes it with the merge product, like every other
# artifact the chamber authors. There is nothing left to fetch by hand.
lane lane-status lane-log lane-roster lane-wait:
	@echo "make $@ no longer exists. The lane's dispatch layer is deleted;" >&2
	@echo "everything above the commit gate is a queue entry now:" >&2
	@echo "" >&2
	@echo "  make sluice       BRANCH=<branch> REF=<full-sha>   gate + merge + push" >&2
	@echo "  make sluice-stage BRANCH=<branch> REF=<full-sha>   gate only, never pushes" >&2
	@echo "  make sluice-status / make sluice-log [JOB=<id>]    read either back" >&2
	@echo "" >&2
	@echo "The two sets the chamber does NOT run keep their own entry points:" >&2
	@echo "  make heavy-remote REF=<full-sha>                   the heavy tier alone" >&2
	@echo "  ssh <canonical> 'cd ~/Projects/hornvale && HV_CENSUS_REF=<sha> scripts/census-run.sh'" >&2
	@echo "" >&2
	@echo "lane-roster is retired because the roster now updates itself: the" >&2
	@echo "chamber's gate phase rewrites it and commits it with the merge" >&2
	@echo "product. The copy-out it fetched never once produced a byte." >&2
	@exit 2

sluice: ## Request a merge through the queue (BRANCH=<branch> REF=<full-sha>)
	@bash scripts/sluice-request.sh "$(BRANCH)" "$(REF)"

# THE STAGE GATE, ABSORBED. Same script, same queue, same chamber, same
# claim — `kind=stage` is one column in the queue TSV and one branch at the
# push step. It runs the `stage`-rung phases against the real main+branch
# merge product and reports; the entry ends `reported`, and main never moves.
sluice-stage: ## Request a STAGE GATE through the queue — phases run, nothing is pushed (BRANCH=<branch> REF=<full-sha>)
	@bash scripts/sluice-request.sh "$(BRANCH)" "$(REF)" stage

# BRANCH is the REQUESTER here, not the thing gated: a census regenerates at a
# REF and delivers its goldens on a fresh `census/...` branch, which the
# requester then submits as an ordinary merge. Nothing about a census pushes
# main — see the header of scripts/sluice-census.sh for why that restraint is
# deliberate rather than a limitation.
sluice-census: ## Request a CENSUS through the queue — regenerates at REF, delivers goldens on a branch, never pushes main (BRANCH=<requester> REF=<full-sha>)
	@bash scripts/sluice-request.sh "$(BRANCH)" "$(REF)" census

# FIX ROUND 1: the queue and its jobs live on the CANONICAL BOX
# ($HV_SLUICE_DIR under ITS $HOME — sluice-request.sh enqueues over ssh, and
# sluice-run.sh runs there too), never on whatever machine typed `make
# sluice-status`. The first cut of these two targets read a purely local
# path instead — from anywhere but the canonical box that is silently an
# EMPTY queue or "no such job", never an error, which is the worst kind of
# wrong answer. lane-status/lane-log (above) already get this right by
# ssh-ing first; these follow that exact shape.
decision-block: ## Reserve a disjoint range of decision numbers (NAME=<campaign>)
	@test -n "$(NAME)" || { echo "usage: make decision-block NAME=<campaign>"; exit 1; }
	@bash scripts/decision-block-request.sh "$(NAME)"

decision-blocks: ## Show every reserved decision-number block (reads the canonical box)
	@ssh $$(cat scripts/census-canonical-host.txt) 'cd ~/Projects/hornvale && scripts/decision-block.sh list'

sluice-ack: ## Adjudicate an out-of-band landing on main (REASON='what you checked'); canonical box only
	@test -n "$(REASON)" || { echo "usage: make sluice-ack REASON='what you checked'" >&2; exit 2; }
	@bash scripts/sluice-ack.sh "$(REASON)"

sluice-status: ## The queue: what is queued, running, held, landed, reported (reads the canonical box over ssh)
	@ssh $$(cat scripts/census-canonical-host.txt) 'd=$${HV_SLUICE_DIR:-$$HOME/.local/state/hornvale/sluice}; \
	    cat "$$d/queue.tsv" 2>/dev/null || true' \
	    | column -t -s "$$(printf '\t')" || true

sluice-log: ## Read a finished merge-queue job back (JOB=<id>, or omit for the most recent; reads the canonical box over ssh)
	@ssh $$(cat scripts/census-canonical-host.txt) 'd=$${HV_SLUICE_DIR:-$$HOME/.local/state/hornvale/sluice}; \
	    if [ -n "$(JOB)" ]; then f="$$d/$(JOB).log"; else f=$$(ls -1t "$$d"/*.log 2>/dev/null | head -1); fi; \
	    if [ -z "$$f" ] || [ ! -f "$$f" ]; then echo "sluice-log: no such job" >&2; exit 1; fi; \
	    echo "== $$f"; cat "$$f"'

fmt: ## Format the workspace in place
	cargo fmt

fmt-check: ## Verify formatting without writing
	cargo fmt --check

clippy: ## Lint with warnings denied
	cargo clippy --workspace --all-targets -- -D warnings

# In the gate because it is a lint, not an artifact: default-deny over every
# pub-boundary primitive (decisions 0027/0028), 1.2s warm. It lives here
# because CI is manual-only (decision 0042), so CI's own type-audit step
# caught nothing before merge — The Named shipped a malformed tag past fmt,
# clippy and the full suite.
type-audit: ## Verify pub-boundary type-audit tags (default-deny; decisions 0027/0028)
	cargo run --quiet --manifest-path tools/type-audit/Cargo.toml -- check

# Freshness of the committed REPORT is a SEPARATE thing from the lint above:
# it is a generated artifact (scripts/regenerate-artifacts.sh), and used to be
# drift-checked only by `make rebaseline`'s `git diff --exit-code docs/audits/`
# sweep — which nobody runs per commit. A commit that adds or changes a
# pub-boundary primitive left the report stale with nothing in the everyday
# loop noticing (The Mire, task 1). This target closes that hole in the gate
# itself rather than leaving it to the rebaseline sweep. It runs right after
# type-audit, so the tool binary is already built and the marginal cost is
# ~1s; it regenerates to a temp file rather than docs/audits/ so a failing
# run never mutates the tree.
type-audit-report: ## Fail if the committed type-audit report is stale (regen cmd in the message)
	@tmp="$$(mktemp /tmp/hv-type-audit-report.XXXXXX)"; \
	trap 'rm -f "$$tmp"' EXIT; \
	cargo run --quiet --manifest-path tools/type-audit/Cargo.toml -- report > "$$tmp"; \
	if ! diff -q "$$tmp" docs/audits/type-audit-report.md >/dev/null 2>&1; then \
		echo "type-audit-report: docs/audits/type-audit-report.md is stale. Regenerate it with:" >&2; \
		echo "  cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md" >&2; \
		exit 1; \
	fi

# In the gate for the same reason type-audit is: a lint, not an artifact —
# default-deny on undeclared shape twins across kernel/domains (decision
# 0517; The Hallmark spec §3). Warm ~4.5-5.8s on this Mac (The Hallmark,
# 2026-09-01) — well under type-audit's ~6.3-6.4s, so it joins the same
# commit-gate rung rather than being pushed to the stage gate.
placement-audit: ## Verify shape-twin placement tags (default-deny; decision 0517)
	cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- check

placement-audit-report: ## Fail if the committed placement roster is stale (regen cmd in the message)
	@tmp="$$(mktemp /tmp/hv-placement-audit-report.XXXXXX)"; \
	trap 'rm -f "$$tmp"' EXIT; \
	cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- report > "$$tmp"; \
	if ! diff -q "$$tmp" docs/audits/placement-audit-roster.md >/dev/null 2>&1; then \
		echo "placement-audit-report: docs/audits/placement-audit-roster.md is stale. Regenerate it with:" >&2; \
		echo "  cargo run --manifest-path tools/placement-audit/Cargo.toml -- report > docs/audits/placement-audit-roster.md" >&2; \
		exit 1; \
	fi

# The Cairn (tools/board): a git-native message board for parallel agent
# sessions, outside the cargo workspace like type-audit and the digest above
# (so `make gate` never builds it — its own tests run under
# `cargo test --manifest-path tools/board/Cargo.toml`).
# In the gate for the same reason type-audit is (The Plumb, Task 4, decision
# ledger #31): default-deny over every authored numeric constant in
# domains/*/src and windows/*/src. THE PAIR (this target plus plumb-report
# below) IS WHAT style-run ACTUALLY PAYS, and a fix-round review measurement
# (confirmed by re-measurement 2026-09-02, `/usr/bin/time -p make <target>`,
# warm tree, 681 constants over 290 files) corrected an earlier draft of this
# comment that quoted a single-target 3.5s number against type-audit's
# single-target ~1.2s — both wrong for this crate and not the comparison that
# matters. The real pair costs: `make plumb` + `make plumb-report` ~7.1s
# (7.09/7.06/7.11s across three runs) against `make type-audit` +
# `make type-audit-report` ~10.9s (10.88/10.90/11.0s) — plumb is roughly
# TWO-THIRDS the incumbent's cost, not costlier than it. Still a source scan
# with no workspace build, and the whole point of this campaign is that
# nothing else runs it: unlike seam-guard (whose cost is a scoped TEST RUN
# per call site, not a scan, and whose declared-survivor grammar makes an
# occasional manual run adequate), an untagged constant here is a silent
# regression the campaign's own motivating bug (FATIGUE_RISE) shipped as. A
# scanner nothing schedules guards nothing.
plumb: ## Verify every authored numeric constant carries a plumb: rung (default-deny)
	cargo run --quiet --manifest-path tools/plumb/Cargo.toml -- check

# Freshness of the committed roster, same shape as type-audit-report above.
plumb-report: ## Fail if the committed plumb roster is stale (regen cmd in the message)
	@tmp="$$(mktemp /tmp/hv-plumb-report.XXXXXX)"; \
	trap 'rm -f "$$tmp"' EXIT; \
	cargo run --quiet --manifest-path tools/plumb/Cargo.toml -- report > "$$tmp"; \
	if ! diff -q "$$tmp" docs/audits/plumb-roster.md >/dev/null 2>&1; then \
		echo "plumb-report: docs/audits/plumb-roster.md is stale. Regenerate it with:" >&2; \
		echo "  cargo run --manifest-path tools/plumb/Cargo.toml -- report > docs/audits/plumb-roster.md" >&2; \
		exit 1; \
	fi

# The Cairn (tools/board): a git-native message board for parallel agent
# sessions, outside the cargo workspace like type-audit and the digest above
# (so `make gate` never builds it — its own tests run under
# `cargo test --manifest-path tools/board/Cargo.toml`).

board: ## The Cairn: read the board (full, unfiltered)
	@cargo run --quiet --manifest-path tools/board/Cargo.toml -- read

board-digest: ## The Cairn: the human digest over the board's history (default 14 days)
	@cargo run --quiet --manifest-path tools/board/Cargo.toml -- digest $(DAYS)

board-sync: ## The Beacon: publish this host's board and fetch the peers' (B2)
	@cargo run --quiet --manifest-path tools/board/Cargo.toml -- sync

# BY defaults to the current branch: attribution is mandatory (decision 0118) and
# a default that is always right beats one a session has to remember.
#
# NOTE and PATHS have their own variables rather than living in FIELDS, and both
# reasons were found by using this target rather than by reading it:
#   NOTE  contains spaces, and make would split it out of an unquoted FIELDS.
#   PATHS must reach the tool as a JSON *array*. Passing FIELDS='paths=["a"]'
#         loses the inner quotes to the shell, so the tool stores the STRING
#         "[a]", `paths()` finds no array, and the post silently degrades from
#         path-routed to broadcast — wrong in kind, invisible in effect. Building
#         the JSON inside the recipe, where the quoting is ours, removes the trap.
# FIELDS remains for single-token pairs (polarity=hold-off, ttl_s=900, host=…).
board-post: ## The Cairn: post to the board (KIND=technique NOTE='...' [PATHS='a/ b/'] [FIELDS='polarity=hold-off'] [BY=])
	@test -n "$(KIND)" || { echo "usage: make board-post KIND=<kind> NOTE='<text>' [PATHS='dir/ dir/'] [FIELDS='k=v k=v'] [BY=<branch>]" >&2; exit 2; }
	@cargo run --quiet --manifest-path tools/board/Cargo.toml -- post \
		"$(KIND)" "$(if $(BY),$(BY),$(shell git branch --show-current))" \
		$(if $(NOTE),note="$(NOTE)",) \
		$(if $(PATHS),'paths=[$(shell printf '%s' '$(PATHS)' | tr -s ' ' '\n' | sed 's/.*/"&"/' | paste -sd, -)]',) \
		$(FIELDS)

# Undiscoverable before The Beacon (task 11): every other board write had a
# target (board-post) or a read had one (board, board-digest, board-sync),
# but this one -- the command to reach for when something sensitive lands --
# had neither a target nor a mention in CLAUDE.md. It does not delete
# anything (D13: history keeps the post); it appends a `redact` control post
# and evicts the named post from the TIP tree, so every future read (digest,
# render, `board read`) suppresses its body while still reporting that the
# act happened. BY defaults to the current branch, same as board-post.
board-redact: ## The Cairn: suppress a post's body at read time, keeping the act visible (ID=<post-id> [BY=])
	@test -n "$(ID)" || { echo "usage: make board-redact ID=<post-id> [BY=<branch>]" >&2; exit 2; }
	@cargo run --quiet --manifest-path tools/board/Cargo.toml -- redact \
		"$(if $(BY),$(BY),$(shell git branch --show-current))" "$(ID)"

test: nextest-check ## Run the workspace tests: nextest (parallel binaries) + doctests
	cargo nextest run --workspace
	cargo test --workspace --doc

nextest-check: ## Fail with an install hint if cargo-nextest is missing
	@command -v cargo-nextest >/dev/null 2>&1 || { \
		echo "cargo-nextest not found — install it (decision 0040):"; \
		echo "  cargo install cargo-nextest   # or: brew install cargo-nextest"; \
		exit 1; }

# HV_DOCS_TESTS_EXCLUDE narrows this roster for ONE caller only: a census
# delivery commit, which cannot satisfy the yellow-census alarm by construction
# (see scripts/hooks/pre-commit's HV_CENSUS_DELIVERY block). Empty for every
# other caller, so the default roster is unchanged.
docs-tests: nextest-check ## The prose-subject tests -- run by pre-commit when only docs are staged (The Nettle)
	@cargo nextest run -p hornvale --test suite -E '(test(docs_consistency) or test(generated_paths) or test(census_duration) or test(repose_byte_identity) or test(audio_artifacts) or test(lexicon_guard) or test(subfloor_roster_coverage) or test(architecture) or test(temp_path_ratchet))$(if $(HV_DOCS_TESTS_EXCLUDE), and not test($(HV_DOCS_TESTS_EXCLUDE)),)'

absorb: ## Absorb main into this campaign branch, regenerating artifacts it cannot merge
	@bash scripts/absorb.sh

prewarm: ## Warm a fresh worktree's caches (start in the background right after `git worktree add`)
	@bash scripts/timed.sh prewarm -- make --no-print-directory prewarm-run

worktree-take: ## Claim a recycled campaign worktree (NAME=<campaign> [BASE=main])
	@NAME="$(NAME)" BASE="$(BASE)" bash scripts/worktree-take.sh

# THE TARGET DIRS ACCUMULATE DEAD BUILD GENERATIONS, AND CARGO NEVER RECLAIMS
# THEM (decision 0848). `target/debug/deps` is keyed by a metadata hash, so
# every STRUCTURAL configuration that has ever been built keeps its own
# permanent copy of every artifact. Measured on this repo: 1,152 executables
# for 307 distinct test targets -- 845 (73%) dead copies, `hornvale` alone
# holding 40 generations at ~20 MB each.
#
# WHAT MULTIPLIES THEM IS STRUCTURAL CHURN, NOT EDITING. Measured, one knob at
# a time on a scratch crate (2 -> 10 executables): a version bump, a RUSTFLAGS
# change, a feature added, a different toolchain. Editing a source file
# OVERWRITES in place and adds nothing -- so the accrual tracks toolchain
# upgrades, Cargo.lock churn and `.cargo/config.toml` changes, at campaign
# cadence rather than per-commit.
#
# NOTHING RUNS THIS FOR YOU, and that is the seam-guard arrangement on purpose
# (decisions 0148, 0426): a reclamation pass is maintenance whose guarantee
# moves at campaign cadence, so it is not a phase of any gate and not a step of
# `worktree-take`. Both automatic homes were measured and REFUSED:
#   * age-based inside `worktree-take` -- on a pool member idle longer than the
#     threshold, EVERY artifact is old, so the sweep destroys the warm target/
#     the pool exists to preserve.
#   * stamp-based inside `worktree-take` -- `--file` deletes whatever the
#     intervening build did not touch, and `worktree-take` deliberately does
#     not build. Verified: stamp -> partial build -> `--file` proposes deleting
#     the live test binaries.
sweep-check: ## Fail with an install hint if cargo-sweep is missing
	@command -v cargo-sweep >/dev/null 2>&1 || { \
		echo "cargo-sweep not found — install it (decision 0848):"; \
		echo "  cargo install cargo-sweep   # or: brew install cargo-sweep"; \
		exit 1; }

# TIME defaults to 30 days, not 7. A campaign worktree is rebuilt continuously
# while it is live, so its working set is days old at most and 7 would be safe
# for it -- but a PARKED worktree's live artifacts are as old as its last
# build, and the threshold cannot tell "old but reachable" from "old and dead".
# 30 keeps a month-idle worktree warm; lower it deliberately (`SWEEP_DAYS=7`) when
# reclaiming a tree you accept rebuilding.
SWEEP_DAYS ?= 30

sweep-dry: sweep-check ## Report what a sweep would reclaim, deleting nothing (SWEEP_DAYS=<days>)
	@cargo sweep --dry-run --time $(SWEEP_DAYS) -r .

sweep: sweep-check ## Reclaim dead build generations older than SWEEP_DAYS days (default 30, recursive)
	@cargo sweep --time $(SWEEP_DAYS) -r .

# THE EXACT MODE, AND WHY IT IS NOT THE DEFAULT. `--stamp` then a build then
# `--file` is mark-and-sweep: it reclaims precisely the generations the build
# did not touch, at any age, so it preserves a warm working set that `--time`
# would eat. The mark must be COMPLETE or it collects live objects, which is
# why the build here is `--workspace --all-targets` and why this is a target of
# its own rather than a flag on `sweep`: it costs a full workspace build.
#
# WORKSPACE-SCOPED, DELIBERATELY NOT RECURSIVE. The build below does not cover
# `clients/*/target` or the `tools/*` crates, so a recursive `--file` here
# would read their untouched artifacts as garbage and delete them. Use plain
# `make sweep` for those.
sweep-exact: sweep-check ## Mark-and-sweep the workspace target: exact, age-independent, costs a full build
	@cargo sweep --stamp .
	@cargo build --workspace --all-targets
	@cargo sweep --file .

# THE COLD-BUILD COST WAS INVISIBLE UNTIL THIS LANDED (The Sexton, Task 2).
# docs/timings.md carried five labels and 73 branches went through this target
# in one month with zero rows — roughly eight unrecorded hours, comparable to
# the census line. The wrapper above is the whole fix.
prewarm-run:
	cargo build --workspace --all-targets
	cargo build --release -p hornvale
	cargo build --manifest-path tools/type-audit/Cargo.toml
	cargo build --manifest-path tools/placement-audit/Cargo.toml
	# The Cairn's binary, without which THREE of its four read seams are
	# silently inert in a fresh worktree: `scripts/board-render.sh` (the
	# SessionStart hook), `doctor`, and `sluice-request.sh`'s hold-off
	# advisory all require a prebuilt
	# binary and all deliberately refuse to compile one. `tools/board/target/`
	# is gitignored and per-worktree, so nothing else in the repo ever
	# produces it — a new campaign therefore started with the board dead and
	# no signal anywhere, which is this tool's own failure mode aimed at
	# itself. Release, so the hook prefers it and the per-post cost is lower.
	# `-` prefixed: prewarm is a convenience, and a board that will not build
	# must not fail the target that warms the workspace.
	-cargo build --release --manifest-path tools/board/Cargo.toml
	# The queue's own binary, same reasoning (fix round 2, Critical F1):
	# scripts/sluice-queue.sh deliberately NEVER compiles it (a build failure
	# under that script's own `set -euo pipefail` aborted with cargo's rc
	# before dispatch ever ran, and callers read that as "queue drained" or
	# "unbookkept" — see the comment above its forwarding block), so a fresh
	# worktree with no prewarm has no way to claim, set-state or list a row
	# until something builds it. `-` prefixed for the same reason as board.
	-cargo build --release --manifest-path tools/sluice/Cargo.toml

rebaseline artifacts: ## Regenerate committed artifacts EXCEPT censuses (refresh those with scripts/census-run.sh)
	@bash scripts/timed.sh rebaseline -- bash scripts/regenerate-artifacts.sh

timings: ## Show the timing ledger (usage: make timings [LABEL=rebaseline])
	@bash scripts/timed.sh report $(LABEL)

# EVERY BYTE-GOLDEN IN THE TREE MUST HAVE A LINE HERE. This recipe is a hand-
# maintained list of scoped invocations, not a sweep, so a golden added later
# is accepted by nothing: `REBASELINE=1` is read by the golden helper, and no
# test binary this list does not name is ever run. The failure mode is quiet
# and expensive - the fixture's own message names this target as if it works,
# so an omitted golden sends you round the loop believing it is broken rather
# than unlisted.
#
# `hornvale-terrain --test channel_golden` was missing for exactly that reason
# and cost The Glasshouse a debugging cycle. When you add a golden, add its
# line here in the same commit.
#
# Every target below is `--test suite -- <name>`, not `--test <name>`: test-
# binary consolidation (perf(*): consolidate N test binaries into 1) put
# every crate's integration tests behind one `tests/suite.rs` binary named
# `suite`, so the old per-file binary name is now a libtest name FILTER
# passed after `--`, not a `--test` target of its own.
rebaseline-goldens: ## Accept drifted byte-golden test fixtures (REBASELINE=1), then review the diff
	REBASELINE=1 cargo test -q -p hornvale --test suite -- lens_purity
	REBASELINE=1 cargo test -q -p hornvale-scene --test suite -- golden
	REBASELINE=1 cargo test -q -p hornvale-worldgen --test suite -- proto_goblinoid_golden
	REBASELINE=1 cargo test -q -p hornvale --test suite -- architecture
	REBASELINE=1 cargo test -q -p hornvale-vessel --test suite -- session_snapshot
	REBASELINE=1 cargo test -q -p hornvale-worldgen --test suite -- solitary_tongue
	REBASELINE=1 cargo test -q -p hornvale-lab --test suite -- affect_trace_golden
	REBASELINE=1 cargo test -q -p hornvale-terrain --test suite -- channel_golden

lab-diff: ## Report which census metrics moved vs HEAD (usage: make lab-diff STUDY=the-census)
	@test -n "$(STUDY)" || { echo "usage: make lab-diff STUDY=<study-name>"; exit 2; }
	@old="$$(mktemp)"; \
	if ! git show HEAD:book/src/laboratory/generated/$(STUDY)/rows.csv > "$$old" 2>/dev/null; then \
	    rm -f "$$old"; \
	    echo "lab-diff: no committed rows.csv for study '$(STUDY)' at HEAD (check the name under book/src/laboratory/generated/)"; \
	    exit 2; \
	fi; \
	cargo run -q -p hornvale -- lab diff studies/$(STUDY).study.json "$$old" \
	    book/src/laboratory/generated/$(STUDY)/rows.csv; \
	status=$$?; rm -f "$$old"; exit $$status

shapecheck: ## Compare the key-path SHAPE of two JSON docs (usage: make shapecheck OLD=a.json NEW=b.json)
	@test -n "$(OLD)" && test -n "$(NEW)" || { echo "usage: make shapecheck OLD=<path> NEW=<path>"; exit 2; }
	@python3 scripts/shapecheck.py "$(OLD)" "$(NEW)"

census: ## Build the analysis DB from committed censuses and open DuckDB on it
	@bash tools/census/build.sh
	@duckdb tools/census/.build/census.duckdb

census-query: ## One-shot census query (usage: make census-query Q="SELECT ...")
	@test -n "$(Q)" || { echo "usage: make census-query Q=\"SELECT ...\""; exit 2; }
	@bash tools/census/build.sh
	@duckdb tools/census/.build/census.duckdb -c "$(Q)"

census-history: ## Load a study's git history into census_history (usage: make census-history STUDY=the-census)
	@test -n "$(STUDY)" || { echo "usage: make census-history STUDY=<study-name>"; exit 2; }
	@bash tools/census/history.sh "$(STUDY)"

census-check: ## Harness gate: mount-validate + smoke + golden-pins (local; needs duckdb+python3)
	@bash tools/census/check.sh

regen-remote: ## ABANDONED (decision 0063) — censuses regenerate LOCALLY via scripts/census-run.sh; this AWS path is unused
	@scripts/aws-gate/regen-git.sh .

# RETIRED (The Sluice, Task 12), and each of its four halves went somewhere
# BETTER, which is the only reason deleting a GO/NO-GO gate is defensible:
#
#   ancestry ("has main moved under this branch?") -> scripts/sluice-mouth.sh
#     checks the ACTUAL merge with `git merge-tree`, where preflight compared
#     ancestry as a proxy for it and could say GO on a branch that conflicts.
#   both-sides-added slugs (a decision/chronicle/retro filename minted twice)
#     -> an add/add conflict, which the mouth reports as a conflict (exit 1).
#   registry row IDs minted on both sides -> `cli/tests/suite/docs_consistency.rs`
#     asserts ID uniqueness, and the chamber runs it against the real merge
#     product in the `gate` phase. Again: the object that lands, not a proxy.
#   the board's hold-off advisory -> `scripts/sluice-request.sh`, which is now
#     the moment work asks to integrate. This is the one half with no other
#     home, so it was moved rather than assumed covered.
#
# Its unmechanizable half ("read the other branches' chronicles, not just
# their diffs") was always human and lives in the submitting-a-campaign skill.
preflight:
	@echo "make preflight no longer exists. Its checkable halves are now run" >&2
	@echo "against the real merge, not against ancestry as a proxy for it:" >&2
	@echo "" >&2
	@echo "  make sluice-stage BRANCH=<branch> REF=<full-sha>" >&2
	@echo "      merges main+branch in the chamber and runs the stage phases." >&2
	@echo "      A conflict is refused at the mouth in milliseconds; a" >&2
	@echo "      duplicate registry ID or slug reddens the gate phase." >&2
	@echo "" >&2
	@echo "The judgment half preflight printed and could not score is still" >&2
	@echo "yours: read the other live branches' chronicles, not just their" >&2
	@echo "diffs. Two campaigns have collided semantically on a clean GO." >&2
	@exit 2

doctor: ## Print the repo self-map (orientation for a fresh session)
	@bash scripts/doctor.sh

# The `merge.hv-regenerate` driver registration was removed here by decision
# 0166, which retired PROC-12's Tier B. NOTE FOR ANYONE WONDERING WHY THEIR
# CHECKOUT STILL HAS ONE: that line wrote to `.git/config`, which is NOT
# tracked, so every checkout that ever ran this target keeps a dangling
# registration pointing at a script that no longer exists. It is inert — an
# ATTRIBUTE is what triggers a driver, and the attributes are gone from
# `.gitattributes` — so removing the stale config line is optional cleanup,
# never required for correctness:
#     git config --unset merge.hv-regenerate.driver
install-hooks: ## Point git at scripts/hooks (opt-in; edits local config)
	git config core.hooksPath scripts/hooks
	@echo "git hooks path set to scripts/hooks; 'make quick' now runs pre-commit."

gate-remote: ## ABANDONED (decision 0063) — the AWS spot box is unused; kept only as history
	@scripts/aws-gate/gate-remote.sh

gate-remote-verify: ## Local-vs-remote byte-identity acceptance test (libm go-live gate)
	@scripts/aws-gate/gate-remote-verify.sh

gate-panic: ## EMERGENCY: disable the runner and kill all gate resources
	@scripts/aws-gate/panic.sh

gate-remote-setup: ## Provision remote-gate infra (BILLABLE; confirmation-gated)
	@scripts/aws-gate/setup.sh

gate-remote-teardown: ## Remove all remote-gate infra
	@scripts/aws-gate/teardown.sh

shellcheck: ## Lint all shell scripts
	@shellcheck scripts/*.sh scripts/aws-gate/*.sh scripts/aws-gate/test/*.sh scripts/hooks/* scripts/scheduled/*.sh tools/census/*.sh

observation-check: ## Validate/export observation fixtures and test local film assembly (never publishes)
	@tmp="$$(mktemp -d)"; \
	trap 'rm -rf "$$tmp"' EXIT; \
	cargo run --quiet -p hornvale -- observations validate --manifest observations/episodes/HV-001.json; \
	cargo run --quiet -p hornvale -- observations export --manifest observations/episodes/HV-001.json --out "$$tmp/frames"; \
	cmp "$$tmp/frames/frame-000.json" observations/fixtures/HV-001/expected-frame-000.json; \
	cmp observations/fixtures/HV-001/expected-frame-000.json observations/fixtures/HV-001/render-input.json; \
	cargo run --quiet -p hornvale -- observations validate --manifest observations/episodes/HV-009.json; \
	cargo run --quiet -p hornvale -- observations export --manifest observations/episodes/HV-009.json --out "$$tmp/neighbors-first"; \
	cargo run --quiet -p hornvale -- observations export --manifest observations/episodes/HV-009.json --out "$$tmp/neighbors-second"; \
	diff -qr "$$tmp/neighbors-first" "$$tmp/neighbors-second"; \
	cmp "$$tmp/neighbors-first/frame-000.json" observations/fixtures/HV-009/expected-frame-000.json; \
	cmp observations/fixtures/HV-009/expected-frame-000.json observations/fixtures/HV-009/render-input.json; \
	HV_OBSERVATION_FFMPEG=hornvale-no-ffmpeg bash scripts/observation-film.sh --manifest observations/episodes/HV-009.json --frames "$$tmp/neighbors-first" --out "$$tmp/neighbors-film"; \
	test -s "$$tmp/neighbors-film/HV-009.sha256"; \
	shellcheck scripts/observation-film.sh scripts/test-observation-film.sh scripts/observation-render.sh scripts/test-observation-render.sh; \
	bash scripts/test-observation-film.sh; \
	bash scripts/test-observation-render.sh

wasm-vessel: ## Build the Casement wasm into book/src/gallery (deploy runs this too; never committed)
	rustup target add wasm32-unknown-unknown 2>/dev/null || true
	cargo build --manifest-path clients/vessel/wasm/Cargo.toml --release --target wasm32-unknown-unknown
	cp clients/vessel/wasm/target/wasm32-unknown-unknown/release/hornvale_vessel_wasm.wasm book/src/gallery/vessel.wasm

vessel-check: ## The Casement's local gate: deno checks + wasm fmt/clippy + byte-identity smoke
	@bash scripts/timed.sh vessel-check -- make --no-print-directory vessel-check-run

vessel-check-run: wasm-vessel
	cd clients/vessel && deno fmt --check && deno lint && deno task check && deno task test
	cargo fmt --check --manifest-path clients/vessel/wasm/Cargo.toml
	cargo clippy --manifest-path clients/vessel/wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
	node clients/vessel/wasm/drive.mjs book/src/gallery/vessel.wasm

# The wasm features this binary actually uses. wasm-opt VALIDATES before it
# optimizes, so a feature rustc emitted but binaryen was not told to accept
# makes it refuse the input outright rather than produce a worse result. All
# four are required: dropping any one fails validation.
#
# Deliberately long-established flags only. An earlier attempt also passed
# --enable-bulk-memory-opt, which Homebrew's binaryen 131 accepts and the
# binaryen in Ubuntu's apt repo does not — the release job failed with
# "Unknown option" while the local gate was green. It bought nothing (byte-
# identical output at 883473 either way), so it is gone. Prefer flags old
# enough that a distro package has them over squeezing the last byte.
WASM_OPT_FEATURES := --enable-bulk-memory --enable-sign-ext --enable-nontrapping-float-to-int --enable-mutable-globals

wasm-world: ## Build the world catalog wasm (external clients consume this; never committed)
	rustup target add wasm32-unknown-unknown 2>/dev/null || true
	cargo build --manifest-path clients/world-wasm/Cargo.toml --release --target wasm32-unknown-unknown
	@# wasm-opt in place, so everything downstream — the byte-identity smoke,
	@# the size gate, and the released asset — sees the binary we actually
	@# ship rather than the raw cargo output. Measured -11.0% raw / -3.5%
	@# gzip at world-wasm-v14; -O3 came out LARGER than -Oz on this input, so
	@# do not "upgrade" the flag without measuring. Optional on a dev box:
	@# skipped with a warning when binaryen is absent, since the size gate
	@# below still holds and CI installs it.
	@if command -v wasm-opt >/dev/null 2>&1; then \
	  wasm-opt -Oz $(WASM_OPT_FEATURES) \
	    clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm \
	    -o clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm.opt \
	  && mv clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm.opt \
	        clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm \
	  && echo "wasm-opt -Oz applied"; \
	else \
	  echo "WARNING: wasm-opt not found (brew install binaryen) — shipping unoptimized; CI will optimize"; \
	fi

world-check: ## The catalog's local gate: lint + golden byte-identity smoke + size gate
	@bash scripts/timed.sh world-check -- make --no-print-directory world-check-run

world-check-run: wasm-world
	cargo fmt --check --manifest-path clients/world-wasm/Cargo.toml
	cargo clippy --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
	cargo run -p hornvale -- new --seed 42 --out /tmp/hv-wc.json
	cargo run -p hornvale -- scene system --world /tmp/hv-wc.json > /tmp/hv-wc-system.json
	cargo run -p hornvale -- scene tiles --world /tmp/hv-wc.json --width 256 > /tmp/hv-wc-tiles.json
	cargo run -p hornvale -- scene tiles-region --world /tmp/hv-wc.json --face 0 --level 3 --ix 4 --iy 4 --samples 16 > /tmp/hv-wc-region.json
	cargo run -p hornvale -- new --seed 42 --plates 12 --out /tmp/hv-wc-pinned.json
	cargo run -p hornvale -- scene tiles --world /tmp/hv-wc-pinned.json --width 256 > /tmp/hv-wc-pinned-tiles.json
	cargo run -p hornvale -- new --seed 42 --stellar-topology close-binary --wanderers 3 --out /tmp/hv-wc-binary.json
	cargo run -p hornvale -- scene system --world /tmp/hv-wc-binary.json > /tmp/hv-wc-binary-system.json
	node clients/world-wasm/drive.mjs \
	  clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm \
	  /tmp/hv-wc-system.json /tmp/hv-wc-tiles.json 256 /tmp/hv-wc-pinned-tiles.json /tmp/hv-wc-region.json /tmp/hv-wc-binary-system.json
	@# The gate is denominated in COMPRESSED bytes, because that is what a
	@# visitor actually downloads: GitHub Pages serves the catalog gzipped
	@# (brotli where the client offers it), so the raw figure overstates the
	@# real cost by ~2.8x. The old 1 MiB raw ceiling was measuring the wrong
	@# quantity, and it was doing real harm: it is what pins the release
	@# profile to opt-level = "z" on a binary whose dominant cost is compute
	@# (hw_new is ~55% of the orrery's cold start). Re-denominating keeps the
	@# job the gate exists for — catching unbounded growth — while trading in
	@# the units that bind.
	@#
	@# 512 KiB compressed against 337 KiB today leaves ~34% headroom. Do not raise
	@# it to buy room for one more field; that discards the growth signal. The
	@# levers when it binds, cheapest first: serde_json is the only external
	@# dependency and JSON is already 72% of the export's own cost, so a
	@# binary payload attacks size and speed together.
	@raw=$$(wc -c < clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm); \
	  gz=$$(gzip -9 -c clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm | wc -c); \
	  echo "world wasm size: $$gz bytes gzipped ($$raw raw)"; \
	  [ $$gz -le 524288 ] || { echo "SIZE GATE FAILED: > 512 KiB gzipped"; exit 1; }

wasm-lot: ## Build the Lot's own exhibit wasm into book/src/gallery (deploy runs this too; never committed)
	rustup target add wasm32-unknown-unknown 2>/dev/null || true
	cargo build --manifest-path clients/lot/wasm/Cargo.toml --release --target wasm32-unknown-unknown
	@# Same wasm-opt in-place step as wasm-world (Task 10b, ledger #17): the
	@# byte-identity smoke and the shipped asset see the binary we actually
	@# ship. No size gate follows it — an exhibit in an unpublished book is
	@# not a released download (see lot-check-run below).
	@if command -v wasm-opt >/dev/null 2>&1; then \
	  wasm-opt -Oz $(WASM_OPT_FEATURES) \
	    clients/lot/wasm/target/wasm32-unknown-unknown/release/hornvale_lot_wasm.wasm \
	    -o clients/lot/wasm/target/wasm32-unknown-unknown/release/hornvale_lot_wasm.wasm.opt \
	  && mv clients/lot/wasm/target/wasm32-unknown-unknown/release/hornvale_lot_wasm.wasm.opt \
	        clients/lot/wasm/target/wasm32-unknown-unknown/release/hornvale_lot_wasm.wasm \
	  && echo "wasm-opt -Oz applied"; \
	else \
	  echo "WARNING: wasm-opt not found (brew install binaryen) — shipping unoptimized; CI will optimize"; \
	fi
	cp clients/lot/wasm/target/wasm32-unknown-unknown/release/hornvale_lot_wasm.wasm book/src/gallery/lot.wasm

visual-check: ## The visual client's CPU gate (GPU qualification is separate)
	@bash scripts/timed.sh visual-check -- make --no-print-directory visual-check-run

visual-check-run:
	cd clients/visual && cargo +1.96.1 fmt --check
	cargo +1.96.1 clippy --locked --manifest-path clients/visual/Cargo.toml --workspace --all-targets -- -D warnings
	cargo +1.96.1 test --locked --manifest-path clients/visual/Cargo.toml --workspace
	python3 scripts/visual-dependencies.py
	python3 scripts/test-visual-dependencies.py

game-check: ## The game client's local gate: fmt/clippy/test on both crates
	@bash scripts/timed.sh game-check -- make --no-print-directory game-check-run

game-check-run:
	cargo fmt --check --manifest-path clients/game/core/Cargo.toml
	cargo fmt --check --manifest-path clients/game/bin/Cargo.toml
	cargo clippy --manifest-path clients/game/core/Cargo.toml --all-targets -- -D warnings
	cargo clippy --manifest-path clients/game/bin/Cargo.toml --all-targets -- -D warnings
	cargo test --manifest-path clients/game/core/Cargo.toml
	cargo test --manifest-path clients/game/bin/Cargo.toml
	@bash scripts/game-no-vessel-dep.sh

# ATLAS: `deno task build` is what makes book/src/gallery/atlas.js a real
# generated artifact. Before The Staff it was committed, declared in
# docs/generated-paths.txt, and regenerated by nothing — so `make rebaseline`
# never wrote it and the drift check that follows always reported clean,
# whatever clients/atlas/src/ had done.
#
# THIS CHECK IS BLIND TO COMMENT-ONLY EDITS: `deno task build` runs
# `deno bundle --minify`, which strips comments, so a source change confined
# to comments produces a byte-identical bundle and no diff. If you are
# probing whether this check can still go RED, appending a `//` comment is
# NOT a valid mutation — it will pass silently and look like the check is
# vacuous when it is only the probe that was. Use a change with real runtime
# effect (a new statement, an altered literal) instead.
atlas-check:
	cd clients/atlas && deno fmt --check && deno lint && deno task check && deno task test
	cd clients/atlas && deno task build
	@git diff --exit-code -- book/src/gallery/atlas.js || { \
	    echo "atlas: book/src/gallery/atlas.js is stale — commit the rebuilt bundle." >&2; exit 1; }

# THE LOT EXHIBIT: the same shape as atlas-check, plus a wasm smoke, because
# this client is the only deno-driven one whose bundle typechecks against an
# INTERFACE it declares for its wasm exports rather than against the binary
# it will load. `deno check` cannot tell you the export is gone; the smoke
# can, and costs a wasm load.
#
# THIS CHECK IS BLIND TO COMMENT-ONLY EDITS, exactly as atlas-check is and for
# the same reason: `deno task build` runs `deno bundle --minify`, which strips
# comments, so a source change confined to comments produces a byte-identical
# bundle and no diff. Probing whether this check can go RED needs a change with
# real runtime effect (a new statement, an altered literal), never an appended
# `//` line — that passes silently and reads as a vacuous check when it is only
# a vacuous probe.
#
# THE LOT HAS ITS OWN WASM CRATE (Task 10b, ledger #17), not the catalog's.
# Task 10 put the four `hw_lot*` exports into `clients/world-wasm` and grew the
# catalog from ~337 KiB to ~490 KiB gzipped on this Mac; the canonical box's
# older binaryen emits ~11-13% larger output, which projected the catalog past
# its 512 KiB release-asset size gate. An exhibit in an unpublished book is not
# a released download, so `clients/lot/wasm` (exports `hl_*`) carries no size
# gate of its own — the same shape as the Casement's `clients/vessel/wasm`
# (decision 0052) — and the catalog is back at its pre-campaign weight. This
# depends on `wasm-lot` the way `vessel-check-run` depends on `wasm-vessel`:
# the smoke needs the exhibit wasm the page loads, and that wasm is
# deploy-built and never committed, so a fresh checkout has none.
lot-check: ## The Lot exhibit's local gate: deno checks + bundle drift + a wasm smoke
	@bash scripts/timed.sh lot-check -- make --no-print-directory lot-check-run

lot-check-run: wasm-lot
	cargo fmt --check --manifest-path clients/lot/wasm/Cargo.toml
	cargo clippy --manifest-path clients/lot/wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
	cargo run -p hornvale -- new --seed 42 --out /tmp/hv-lc.json
	cargo run -p hornvale -- lot --world /tmp/hv-lc.json --index 0 --json > /tmp/hv-lc-lot0.json
	cargo run -p hornvale -- lot --world /tmp/hv-lc.json --index 3 --year 1500 --json > /tmp/hv-lc-lot3y1500.json
	@# The smoke runs BEFORE the deno suite (Task 10b fix round 1, F3): it is
	@# what writes /tmp/hv-lot-{life-0,curve,places}.json, which
	@# clients/lot/src/payload_test.ts reads as its belt-and-braces witness
	@# over the hand-reduced fixtures' fidelity — a witness `deno task test`
	@# could otherwise run without ever seeing the real payload shapes.
	node clients/lot/wasm/drive.mjs book/src/gallery/lot.wasm /tmp/hv-lc-lot0.json /tmp/hv-lc-lot3y1500.json
	cd clients/lot && deno fmt --check && deno lint && deno task check && deno task test && deno task build
	@git diff --exit-code -- book/src/gallery/lot.js book/src/gallery/lot-worker.js || { \
	    echo "lot: book/src/gallery/lot.js or lot-worker.js is stale — commit the rebuilt bundles." >&2; exit 1; }
	@# No size gate (see the note above): an exhibit wasm in an unpublished
	@# book is not a released download. Printed so growth is still visible.
	@raw=$$(wc -c < clients/lot/wasm/target/wasm32-unknown-unknown/release/hornvale_lot_wasm.wasm); \
	  gz=$$(gzip -9 -c clients/lot/wasm/target/wasm32-unknown-unknown/release/hornvale_lot_wasm.wasm | wc -c); \
	  echo "lot wasm size: $$gz bytes gzipped ($$raw raw)"

# THE CLIENT ARMS RUN IN PARALLEL, and they used to be plain prerequisites
# (i.e. serial). Measured on lefford 2026-08-23, alternating arms on an idle
# box to cancel cache-warming drift, when there were four arms (vessel, world,
# game, atlas — the Lot exhibit did not exist yet):
#
#     serial    337, 337, 324 s   mean 330   <- matches the chamber's own
#     parallel  239, 236, 250 s   mean 243      clients phase, 330.8 s
#                                 gap 87 s, ~26%
#
# Round 1 ran serial-then-parallel and was NOT trusted: its warmup->serial delta
# showed caches still warming, so the later arm was flattered. Round 2 ran
# PARALLEL FIRST and the gap survived, with each arm's own spread at 13-14 s
# against an 87 s difference.
#
# WHY IT WINS is not "more cores": the box has 40 and cargo already uses them.
# It is that the arms are differently shaped — world-check-run spends most of
# its time in six SERIAL single-process `cargo run` scene generations, which
# occupy roughly one core while game-check-run's builds want all of them.
# Overlapping a latency-bound job with a throughput-bound one is the whole
# saving.
#
# LOT-CHECK-RUN IS NOW A FIFTH, INDEPENDENT ARM, not a second phase sharing
# world-check-run's arm the way Task 11's `world-then-lot-run` had it. That
# arrangement existed only because the two checks both built `wasm-world` —
# a single `.PHONY` target whose recipe (a `cargo build`, an in-place
# `wasm-opt` `mv`, and a `cp`) would otherwise race onto the same
# `book/src/gallery/world.wasm` if run concurrently. Task 10b gives the Lot
# its own crate and its own `wasm-lot` target writing a different path
# (`book/src/gallery/lot.wasm`), so the two no longer share a writer and the
# correctness constraint that paired them is gone with it.
#
# WHY SHELL BACKGROUNDING AND NOT `$(MAKE) -j5 -O`. The -j form works and
# measured the same, but every cargo it spawns prints:
#
#     warning: failed to connect to jobserver from environment variable
#     `MAKEFLAGS=" -j4 -Otarget --jobserver-auth=3,4 ..."`: Bad file descriptor
#
# make -j creates a jobserver and exports MAKEFLAGS naming its file
# descriptors, but only passes those fds to recipe lines it recognises as
# sub-makes -- so every cargo sees the advertisement, cannot open the fds, and
# warns. Harmless (cargo falls back to its own -j nproc, which is exactly what
# was measured) and NOT harmless in a gate log, where recurring benign warnings
# are how people learn to stop reading gate logs. `--jobserver-style=fifo`
# fixes it upstream and needs make 4.4; lefford has 4.3.
#
# Backgrounding the SERIAL sub-makes creates no jobserver at all, so the
# warning cannot arise. Each target's output is captured to its own file and
# printed whole after the `wait`, which gives strictly better grouping than
# -Otarget did, and every target's pass/fail is named before the logs.
clients-check-run:
	@set -u; pids=""; names=""; \
	for t in vessel-check-run world-check-run lot-check-run game-check-run atlas-check visual-check-run; do \
	  $(MAKE) --no-print-directory $$t > /tmp/hv-clients-$$t.log 2>&1 & \
	  pids="$$pids $$!"; names="$$names $$t"; \
	done; \
	rc=0; i=1; \
	for p in $$pids; do \
	  n=$$(echo $$names | cut -d' ' -f$$i); i=$$((i+1)); \
	  if wait $$p; then echo "clients: $$n OK"; else rc=1; echo "clients: $$n FAILED"; fi; \
	done; \
	for t in vessel-check-run world-check-run lot-check-run game-check-run atlas-check visual-check-run; do \
	  echo "----- $$t -----"; cat /tmp/hv-clients-$$t.log; \
	done; \
	exit $$rc
