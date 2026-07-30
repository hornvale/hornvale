# Hornvale developer task runner (TOOL-14).
#
# Encodes the cost-ordered gate CLAUDE.md describes as prose, so the ordering
# stops being tribal knowledge re-derived each session. `just` is not a repo
# dependency; this uses `make`, already present everywhere.
#
#   make quick        # cheap half: fmt --check + clippy (the pre-commit gate)
#   make gate         # the commit gate: fmt + clippy + nextest + doctests (heavy tier skipped)
#   make gate-fast    # ITERATION ONLY: scope fmt/clippy/test to changed crates (make gate still gates commits)
#   make gate-full    # full evidence: the commit gate + the cost-tagged heavy tier
#   make prewarm      # warm a fresh worktree's target/ (start right after worktree add)
#   make rebaseline   # regenerate committed artifacts EXCEPT censuses (refresh those with scripts/census-run.sh)
#   make rebaseline-goldens # accept drifted byte-golden test fixtures
#   make lab-diff STUDY=<name> # report which census metrics moved vs HEAD
#   make preflight    # GO/NO-GO before integrating a campaign branch with main
#   make doctor       # print the repo self-map (orientation for a fresh session)
#   make install-hooks# point git at scripts/hooks (opt-in; edits local config)
#   make gate-remote  # ABANDONED (decision 0063): the AWS path is unused; kept only as history
#   make vessel-check  # the Casement's local gate: deno + wasm fmt/clippy + byte-identity smoke
#   make world-check  # the world catalog's local gate: fmt/clippy + byte-identity smoke + size gate
#
# Cost-ordered by design: fmt and clippy are cheapest and the most common
# review finding, so they run first; `--workspace` tests are the final step.

.PHONY: help quick gate gate-fast gate-full ci ci-run heavy-remote heavy-status heavy-log nextest-check prewarm fmt fmt-check clippy type-audit test rebaseline artifacts rebaseline-goldens regen-remote lab-diff timings preflight doctor install-hooks gate-remote gate-remote-verify gate-panic gate-remote-setup gate-remote-teardown shellcheck census census-query census-history census-check wasm-vessel vessel-check wasm-world world-check

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "} {printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}'

quick: fmt-check clippy type-audit ## Cheap half of the gate (fmt-check + clippy + type-audit)

gate: fmt-check clippy type-audit test ## The commit gate (fmt + clippy + type-audit + nextest + doctests; heavy tier #[ignore]d, ~15 min — 0040 budgeted 4)
	@bash scripts/census-advisory.sh || true

gate-fast: ## ITERATION TOOL ONLY: fmt/clippy/test scoped to changed crates (`make gate` still gates commits)
	@bash scripts/gate-fast.sh

gate-full: gate ## Full evidence: the commit gate + the heavy tier (cost-tagged #[ignore]d tests only)
	@bash scripts/gate-full-heavy.sh
	@echo "reminder: 'make census-check' verifies the analysis harness (local-only, brew tools)"

# The CI entry point. A WRAPPER: every decision it makes lives in Rust
# (windows/lab/src/timings.rs, cli/tests/timings_alarm.rs). Raw output is
# persisted before anything summarises it, so a surprise never costs a re-run.
# ORDER IS LOAD-BEARING: the alarm must compare this run against the baseline
# still sitting on disk from the LAST recorded run, so it runs BEFORE
# ci-record overwrites that file — recording first would make every run
# compare against itself and the alarm could never fire.
#
# The libtest-json-plus stream must survive a failing nextest run — the
# alarm and ci-record still need to read it, and a red run's durations
# belong on disk for archaeology — so this recipe cannot simply abort the
# moment nextest exits nonzero. The whole recipe is one shell script (note
# the backslash continuations) with no `set -e`, so a mid-script nonzero
# exit does not by itself stop anything; the FIX is that nextest's status is
# now CAPTURED immediately (`nextest_status=$$?`) instead of being discarded
# by an `|| true` on that line — discarding it entirely was the original
# bug: `make ci` reported success on a fully failing suite because nothing
# downstream ever re-checked pass/fail. The alarm and ci-record still run in
# the same order as before, and the captured status is re-raised at the very
# end, after the summary prints, so a red suite now fails `make ci` while
# still leaving every artifact on disk for inspection.
#
# A RED RUN NEVER BECOMES THE BASELINE. `ci-record` is guarded on both
# statuses, because the un-guarded version was a one-way ratchet: the alarm
# fired on two tests at 2x, and `ci-record` — running unconditionally on the
# next line — immediately wrote those inflated durations back as the new
# reference. The alarm erased its own evidence, and the following run would
# have compared against the regression and seen nothing. Caught by running
# `make ci` for real (2026-07-30) and noticing the alarmed values sitting in
# the baseline afterwards. This is the same ratchet the final review found in
# the CONTENTION path; the fix there guarded `cmd_ci_record` against a live
# claim and did not guard this path. Re-recording a regression stays a
# deliberate act: fix it, or re-record in the commit that caused it.
#
# `ci` is a thin timing wrapper around `ci-run`; the body lives there so
# scripts/timed.sh can measure the WALL TIME a human actually waits through —
# suite, alarm and record together — and append it to docs/timings.md beside
# the `rebaseline` and `census` rows. This campaign exists because that ledger
# carried ZERO rows for `make gate`, so the gate's creep from 234s to 934s was
# unobservable; shipping a per-test recorder that did not record its own wall
# time would have repeated the same omission one level up. `make timings
# LABEL=ci` reads it back. timed.sh passes the wrapped command's exit status
# through, so a red suite still fails `make ci`.
ci: ## Run the suite under the ci profile, alarm on a shift, then record this run's baseline
	@bash scripts/timed.sh ci -- make --no-print-directory ci-run

ci-run:
	@mkdir -p target/nextest/ci docs/timings
	@NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1 cargo nextest run --workspace \
	    --profile ci --message-format libtest-json-plus \
	    > target/nextest/ci/run.json 2> target/nextest/ci/run.log; \
	nextest_status=$$?; \
	cargo test -q -p hornvale --test timings_alarm -- --ignored --nocapture; \
	alarm_status=$$?; \
	if [ $$nextest_status -eq 0 ] && [ $$alarm_status -eq 0 ]; then \
	    cargo run --quiet -p hornvale -- ci-record; \
	else \
	    echo "make ci: NOT recording a baseline — the run was red, so these durations are not a reference" >&2; \
	fi; \
	echo ""; \
	echo "== make ci: detail written to =="; \
	echo "  target/nextest/ci/run.json   structured per-test durations"; \
	echo "  target/nextest/ci/run.log    human output, including failures"; \
	echo "  docs/timings/test-baseline-$$(hostname -s).tsv   recorded baseline"; \
	if [ $$nextest_status -ne 0 ]; then \
	    echo "make ci: FAILED — the nextest run itself was red (exit $$nextest_status); see target/nextest/ci/run.log" >&2; \
	    exit $$nextest_status; \
	fi; \
	exit $$alarm_status

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
# clippy and the full suite. Freshness of the committed REPORT is a different
# thing: that is a generated artifact, regenerated by
# scripts/regenerate-artifacts.sh and drift-checked like every other one.
type-audit: ## Verify pub-boundary type-audit tags (default-deny; decisions 0027/0028)
	cargo run --quiet --manifest-path tools/type-audit/Cargo.toml -- check

test: nextest-check ## Run the workspace tests: nextest (parallel binaries) + doctests
	cargo nextest run --workspace
	cargo test --workspace --doc

nextest-check: ## Fail with an install hint if cargo-nextest is missing
	@command -v cargo-nextest >/dev/null 2>&1 || { \
		echo "cargo-nextest not found — install it (decision 0040):"; \
		echo "  cargo install cargo-nextest   # or: brew install cargo-nextest"; \
		exit 1; }

prewarm: ## Warm a fresh worktree's caches (start in the background right after `git worktree add`)
	cargo build --workspace --all-targets
	cargo build --release -p hornvale
	cargo build --manifest-path tools/type-audit/Cargo.toml

rebaseline artifacts: ## Regenerate committed artifacts EXCEPT censuses (refresh those with scripts/census-run.sh)
	@bash scripts/timed.sh rebaseline -- bash scripts/regenerate-artifacts.sh

timings: ## Show the timing ledger (usage: make timings [LABEL=rebaseline])
	@bash scripts/timed.sh report $(LABEL)

rebaseline-goldens: ## Accept drifted byte-golden test fixtures (REBASELINE=1), then review the diff
	REBASELINE=1 cargo test -q -p hornvale --test lens_purity
	REBASELINE=1 cargo test -q -p hornvale-scene --test golden
	REBASELINE=1 cargo test -q -p hornvale-worldgen --test proto_goblinoid_golden
	REBASELINE=1 cargo test -q -p hornvale --test architecture
	REBASELINE=1 cargo test -q -p hornvale-vessel --test session_snapshot
	REBASELINE=1 cargo test -q -p hornvale-worldgen --test solitary_tongue

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

preflight: ## GO/NO-GO before integrating a campaign branch with main (run from the branch)
	@bash scripts/preflight-merge.sh

doctor: ## Print the repo self-map (orientation for a fresh session)
	@bash scripts/doctor.sh

install-hooks: ## Point git at scripts/hooks + register the regenerate-on-conflict merge driver (PROC-12)
	git config core.hooksPath scripts/hooks
	git config merge.hv-regenerate.driver 'scripts/merge-regenerate.sh %O %A %B %P'
	@echo "git hooks path set to scripts/hooks; 'make quick' now runs pre-commit."
	@echo "merge.hv-regenerate driver registered for generated-artifact conflicts."

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
	@shellcheck scripts/*.sh scripts/aws-gate/*.sh scripts/aws-gate/test/*.sh scripts/hooks/* tools/census/*.sh

wasm-vessel: ## Build the Casement wasm into book/src/gallery (deploy runs this too; never committed)
	rustup target add wasm32-unknown-unknown 2>/dev/null || true
	cargo build --manifest-path clients/vessel/wasm/Cargo.toml --release --target wasm32-unknown-unknown
	cp clients/vessel/wasm/target/wasm32-unknown-unknown/release/hornvale_vessel_wasm.wasm book/src/gallery/vessel.wasm

vessel-check: wasm-vessel ## The Casement's local gate: deno checks + wasm fmt/clippy + byte-identity smoke
	cd clients/vessel && deno fmt --check && deno lint && deno task check && deno task test
	cargo fmt --check --manifest-path clients/vessel/wasm/Cargo.toml
	cargo clippy --manifest-path clients/vessel/wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
	node clients/vessel/wasm/drive.mjs book/src/gallery/vessel.wasm

wasm-world: ## Build the world catalog wasm (external clients consume this; never committed)
	rustup target add wasm32-unknown-unknown 2>/dev/null || true
	cargo build --manifest-path clients/world-wasm/Cargo.toml --release --target wasm32-unknown-unknown

world-check: wasm-world ## The catalog's local gate: lint + golden byte-identity smoke + size gate
	cargo fmt --check --manifest-path clients/world-wasm/Cargo.toml
	cargo clippy --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown -- -D warnings
	cargo run -p hornvale -- new --seed 42 --out /tmp/hv-wc.json
	cargo run -p hornvale -- scene system --world /tmp/hv-wc.json > /tmp/hv-wc-system.json
	cargo run -p hornvale -- scene tiles --world /tmp/hv-wc.json --width 256 > /tmp/hv-wc-tiles.json
	cargo run -p hornvale -- scene tiles-region --world /tmp/hv-wc.json --face 0 --level 3 --ix 4 --iy 4 --samples 16 > /tmp/hv-wc-region.json
	cargo run -p hornvale -- new --seed 42 --plates 12 --out /tmp/hv-wc-pinned.json
	cargo run -p hornvale -- scene tiles --world /tmp/hv-wc-pinned.json --width 256 > /tmp/hv-wc-pinned-tiles.json
	node clients/world-wasm/drive.mjs \
	  clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm \
	  /tmp/hv-wc-system.json /tmp/hv-wc-tiles.json 256 /tmp/hv-wc-pinned-tiles.json /tmp/hv-wc-region.json
	@size=$$(wc -c < clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm); \
	  echo "world wasm size: $$size bytes"; \
	  [ $$size -le 1048576 ] || { echo "SIZE GATE FAILED: > 1 MiB"; exit 1; }
