# Hornvale developer task runner (TOOL-14).
#
# Encodes the cost-ordered gate CLAUDE.md describes as prose, so the ordering
# stops being tribal knowledge re-derived each session. `just` is not a repo
# dependency; this uses `make`, already present everywhere.
#
#   make quick        # cheap half: fmt --check + clippy (the pre-commit gate)
#   make gate         # the full commit gate: fmt + clippy + workspace tests
#   make gate-fast    # ITERATION ONLY: scope fmt/clippy/test to changed crates (make gate still gates commits)
#   make prewarm      # warm a fresh worktree's target/ (start right after worktree add)
#   make rebaseline   # regenerate every committed generated artifact
#   make rebaseline-goldens # accept drifted byte-golden test fixtures
#   make lab-diff STUDY=<name> # report which census metrics moved vs HEAD
#   make preflight    # GO/NO-GO before integrating a campaign branch with main
#   make doctor       # print the repo self-map (orientation for a fresh session)
#   make install-hooks# point git at scripts/hooks (opt-in; edits local config)
#
# Cost-ordered by design: fmt and clippy are cheapest and the most common
# review finding, so they run first; `--workspace` tests are the final step.

.PHONY: help quick gate gate-fast gate-full nextest-check prewarm fmt fmt-check clippy test rebaseline artifacts rebaseline-goldens lab-diff preflight doctor install-hooks

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "} {printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}'

quick: fmt-check clippy ## Cheap half of the gate (fmt-check + clippy)

gate: fmt-check clippy test ## The commit gate (fmt + clippy + nextest + doctests; heavy tier #[ignore]d, ~4 min)

gate-fast: ## ITERATION TOOL ONLY: fmt/clippy/test scoped to changed crates (`make gate` still gates commits)
	@bash scripts/gate-fast.sh

gate-full: gate ## Full evidence: the commit gate + the heavy tier (cost-tagged #[ignore]d tests only)
	@bash scripts/gate-full-heavy.sh

fmt: ## Format the workspace in place
	cargo fmt

fmt-check: ## Verify formatting without writing
	cargo fmt --check

clippy: ## Lint with warnings denied
	cargo clippy --workspace --all-targets -- -D warnings

test: nextest-check ## Run the workspace tests: nextest (parallel binaries) + doctests
	cargo nextest run --workspace
	cargo test --workspace --doc

nextest-check: ## Fail with an install hint if cargo-nextest is missing
	@command -v cargo-nextest >/dev/null 2>&1 || { \
		echo "cargo-nextest not found — install it (decision 0027):"; \
		echo "  cargo install cargo-nextest   # or: brew install cargo-nextest"; \
		exit 1; }

prewarm: ## Warm a fresh worktree's caches (start in the background right after `git worktree add`)
	cargo build --workspace --all-targets
	cargo build --release -p hornvale
	cargo build --manifest-path tools/type-audit/Cargo.toml

rebaseline artifacts: ## Regenerate every committed generated artifact (review the diff, then commit)
	bash scripts/regenerate-artifacts.sh

rebaseline-goldens: ## Accept drifted byte-golden test fixtures (REBASELINE=1), then review the diff
	REBASELINE=1 cargo test -q -p hornvale --test lens_purity
	REBASELINE=1 cargo test -q -p hornvale-scene --test golden
	REBASELINE=1 cargo test -q -p hornvale-worldgen --test proto_goblinoid_golden
	REBASELINE=1 cargo test -q -p hornvale --test architecture

lab-diff: ## Report which census metrics moved vs HEAD (usage: make lab-diff STUDY=census-lands-drift)
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

preflight: ## GO/NO-GO before integrating a campaign branch with main (run from the branch)
	@bash scripts/preflight-merge.sh

doctor: ## Print the repo self-map (orientation for a fresh session)
	@bash scripts/doctor.sh

install-hooks: ## Point git at scripts/hooks (runs `make quick` pre-commit)
	git config core.hooksPath scripts/hooks
	@echo "git hooks path set to scripts/hooks; 'make quick' now runs pre-commit."
