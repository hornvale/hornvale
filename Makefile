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
#   make game-check  # the game client's local gate: fmt/clippy/test on both crates + the containment guard
#
# Cost-ordered by design: fmt and clippy are cheapest and the most common
# review finding, so they run first; `--workspace` tests are the final step.

.PHONY: help quick quick-run gate gate-run gate-fast gate-fast-run gate-full seam-guard seam-guard-list ci ci-run heavy-remote heavy-status heavy-log nextest-check prewarm prewarm-run fmt fmt-check clippy type-audit type-audit-report test rebaseline artifacts rebaseline-goldens regen-remote lab-diff timings preflight preflight-run doctor install-hooks gate-remote gate-remote-verify gate-panic gate-remote-setup gate-remote-teardown shellcheck census census-query census-history census-check wasm-vessel vessel-check vessel-check-run wasm-world world-check world-check-run game-check game-check-run board board-digest board-post board-redact board-sync

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "} {printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}'

quick: ## Cheap half of the gate (fmt-check + clippy + type-audit + type-audit-report)
	@bash scripts/timed.sh quick -- make --no-print-directory quick-run

quick-run: fmt-check clippy type-audit type-audit-report

gate: ## The commit gate (fmt + clippy + type-audit + nextest + doctests; heavy tier #[ignore]d, ~8 min since 0113 — 0040 budgeted 4)
	@bash scripts/timed.sh gate -- make --no-print-directory gate-run

# The gate's body, split out so `timed.sh` can wrap it — the same shape `ci`
# and `ci-run` use. Until this split, docs/timings.md carried ZERO rows
# labelled `gate` (0086's amendment): the ledger built to catch a suite
# creeping "65s -> 43.5 min" was never wired to the most-run expensive command
# in the repo, so a 4-minute budget drifting to 15+ was never observable.
# Read them filtered — `scripts/timed.sh report gate` — because gates are
# frequent and will dominate the ledger by row count.
gate-run: fmt-check clippy type-audit type-audit-report test
	@bash scripts/census-advisory.sh || true

gate-fast: ## ITERATION TOOL ONLY: fmt/clippy/test scoped to changed crates (`make gate` still gates commits)
	@bash scripts/timed.sh gate-fast -- make --no-print-directory gate-fast-run

gate-fast-run:
	@bash scripts/gate-fast.sh

gate-full: gate ## Full evidence: the commit gate + the heavy tier (cost-tagged #[ignore]d tests only)
	@bash scripts/gate-full-heavy.sh
	@$(MAKE) --no-print-directory seam-guard
	@echo "reminder: 'make census-check' verifies the analysis harness (local-only, brew tools)"

# Deliberately NOT in the commit gate: each registered call site costs a full
# scoped test run, so cost scales with the roster. gate-full is the evidence
# tier, which is where a check this expensive belongs (the same argument that
# put the heavy batteries there).
seam-guard: ## Neutralise each registered seam and report the ones no test notices
	cargo run --quiet --manifest-path tools/seam-guard/Cargo.toml -- run

seam-guard-list: ## Print the registered seams and their call sites (cheap, no build)
	@cargo run --quiet --manifest-path tools/seam-guard/Cargo.toml -- list

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

prewarm: ## Warm a fresh worktree's caches (start in the background right after `git worktree add`)
	@bash scripts/timed.sh prewarm -- make --no-print-directory prewarm-run

# THE COLD-BUILD COST WAS INVISIBLE UNTIL THIS LANDED (The Sexton, Task 2).
# docs/timings.md carried five labels and 73 branches went through this target
# in one month with zero rows — roughly eight unrecorded hours, comparable to
# the census line. The wrapper above is the whole fix.
prewarm-run:
	cargo build --workspace --all-targets
	cargo build --release -p hornvale
	cargo build --manifest-path tools/type-audit/Cargo.toml
	# The Cairn's binary, without which THREE of its four read seams are
	# silently inert in a fresh worktree: `scripts/board-render.sh` (the
	# SessionStart hook), `doctor`, and `preflight` all require a prebuilt
	# binary and all deliberately refuse to compile one. `tools/board/target/`
	# is gitignored and per-worktree, so nothing else in the repo ever
	# produces it — a new campaign therefore started with the board dead and
	# no signal anywhere, which is this tool's own failure mode aimed at
	# itself. Release, so the hook prefers it and the per-post cost is lower.
	# `-` prefixed: prewarm is a convenience, and a board that will not build
	# must not fail the target that warms the workspace.
	-cargo build --release --manifest-path tools/board/Cargo.toml

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
	REBASELINE=1 cargo test -q -p hornvale-lab --test affect_trace_golden

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
	@bash scripts/timed.sh preflight -- make --no-print-directory preflight-run

preflight-run:
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
	node clients/world-wasm/drive.mjs \
	  clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm \
	  /tmp/hv-wc-system.json /tmp/hv-wc-tiles.json 256 /tmp/hv-wc-pinned-tiles.json /tmp/hv-wc-region.json
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
