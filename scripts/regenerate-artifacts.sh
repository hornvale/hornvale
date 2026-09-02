#!/usr/bin/env bash
# scripts/regenerate-artifacts.sh — regenerate every committed generated
# artifact (TOOL-15).
#
# This is the SINGLE source of truth for "how the drift-checked artifacts are
# produced." CI's "Artifacts are current" step and the local `make rebaseline`
# target both call it, so the two can never silently diverge (they used to be a
# hand-copied command list in two places).
#
# It performs GENERATION ONLY — it never asserts freshness. CI wraps this call
# with its verification tail (`release_determinism`, `type-audit check`, and
# the `git diff --exit-code` drift assertion). Locally, run it to regenerate,
# then review and commit the diff yourself:
#
#   make rebaseline        # or: bash scripts/regenerate-artifacts.sh
#   git diff               # review what moved
#
# Canonical numeric artifacts are byte-identical across platforms (floats are
# quantized at every serialization boundary — decision
# 0033). The PNG maps
# and scene/tiles are rendered per-cell views whose pixels/indices come from
# host-libm-divergent transcendentals; CI excludes those from its byte drift
# check (see ci.yml), but this script still regenerates them so a local
# rebaseline produces the full set.
set -euo pipefail

# THE SCRIPT WAS A LIST AND IS NOW A DAG (The Sexton, Task 5).
#
# 62 sequential `cargo run` invocations measured cpu_ratio 0.72-2.11 on 10-12
# core boxes — effectively serial, on 15.9% of all measured human waiting.
# Nobody decided that; it fell out of the file being a shell list.
#
# Outputs are DISTINCT FILES, so ordering cannot affect bytes. The success
# criterion is `make rebaseline` leaving every generated artifact
# byte-unchanged — the same falsifier The Whetstone used for its profile
# change. Group D (studies) stays serial: `lab run` already saturates every
# core internally, so co-scheduling it would oversubscribe, exactly as
# .config/nextest.toml documents for the scattered batteries.
#
# THE CLASSIFICATION (traced by reading what each invocation actually reads,
# not guessed from its name — a wrong assignment here is a write race that
# surfaces as intermittent byte drift and gets blamed on determinism):
#
#   GROUP A — world builders. The three `hornvale new` calls writing the
#   throwaway temp files $w42/$wsky/$wlocked. Mutually independent; every
#   other world-touching invocation below depends on one of these three.
#
#   GROUP B — readers of $w42/$wsky/$wlocked (traced: each takes `--world` or
#   `--seed`, and reads it before writing exactly one output). Almanacs,
#   `explain`, `dictionary`, `locale`, `possess`, `history`, `connections`,
#   the gallery maps, the scene exports, the surrounds ascii charts. Each
#   writes to its own file, so independent of every other Group B member.
#   Two `possess --seed 42 --snapshot …` calls (the committed game-core
#   fixtures) do NOT read $w42/$wsky/$wlocked at all — traced their args:
#   `--seed` builds a fresh internal genesis, not the temp files — so they
#   have no dependency on Group A's reap either, but scheduling them with the
#   rest of B is harmless (no shared write target) and keeps this script
#   simple.
#
#   GROUP C — world-free dumps, traced to have NO `--world`/`--seed` build
#   dependency on Group A's temp files: `concepts`, `concepts --manifest`,
#   `streams`, `phonology`, `proto goblinoid|dwarf|elf`, the type-audit
#   report, and the digest renders, as the plan names. Tracing turned up four
#   more of the same shape, not named in the plan: the `first_light` example
#   (hardcodes `Seed(42)` and builds its own mini-genesis internally — never
#   touches $w42/$wsky/$wlocked), `book` (loops over `Seed(1..=3)`, its own
#   internal builds), `tropes report`/`report --corpus …`/`matrix` (each
#   builds its own `Seed(0)` world via `world_builder::build_world`,
#   independent of Group A), and the seam-guard roster (a source-tree scan,
#   no world at all). `systems report`/`matrix` (The Compendium) joined this
#   group later still, and builds no world at all — not even its own —
#   because its anchors resolve against the digest, the idea registry, and
#   the filesystem, never a genesis. `lab confidant` (The Confidant, Task 7
#   reshape) joined it the same way `first_light` did: it builds its own
#   internal `Seed(42)` `FullView`, never touches $w42/$wsky/$wlocked, and
#   its answers are world-invariant by measurement (a 1000-seed census run
#   found all three metric families it reads constant across every seed), so
#   any seed would do and Group A's already-built worlds are simply not
#   needed. All of these are safe to co-schedule with B:
#   distinct write targets, and no read dependency on B's or A's outputs.
#
#   GROUP D — the lab studies (`lab run`, traced: internally parallel across
#   seeds via `std::thread::available_parallelism`, per
#   `windows/lab/src/runner.rs`). Runs serially with respect to each other and
#   is never co-scheduled with anything else, so it does not oversubscribe a
#   box A/B/C are already using every core of. This is the-chorus (always)
#   plus the-census/census-of-the-meeting (only under HV_CENSUS=1) — moved
#   next to each other below; their relative order is unchanged from before
#   this DAG (chorus, then the census pair), only their position in the
#   overall script moved.
#
#   NOT IN A/B/C/D — the census-schema backfill loop, the domesday survey,
#   and the anomaly report.
#   Traced: `backfill-schema` reads the CSVs Group D's census studies write
#   (when HV_CENSUS=1), and both `domesday` and `anomalies` read what
#   `backfill-schema` just wrote, so all three stay serial, right after
#   Group D reaps — exactly where they sat in the original list, since
#   Group D itself did not move relative to them.
#
# Schedule: A, reap; B+C together, reap; D serially; then the three dependent
# trailers (schema backfill, domesday, anomalies) serially.
#
# FAN-OUT IS BOUNDED BY HV_JOBS, not by wishful thinking. Group B+C alone has
# over 50 `spawn` call sites; left uncapped on a quiet box that is a >50-way
# `cargo run` fan-out, and one world build peaks around 300 MB, so that is
# north of the ~16 GB `.config/nextest.toml` already measured as the failure
# shape for uncapped concurrency on the test runner (it projected ~82 GB
# co-resident there). This repo's normal state is two to three parallel
# campaign sessions on one Mac (root CLAUDE.md's working-ceiling note), so
# "quiet box" is the exception, not the default to design for.
HV_JOBS="${HV_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"
_pids=()
# Parallel to `_pids`, index for index: the command line each PID was given.
# A bare "a parallel job failed" names nothing — with 53 spawn sites and up to
# HV_JOBS running at once, it says only that one of them died, and the output
# of the survivors is interleaved on top of it. That already cost this campaign
# a diagnosis: a `make rebaseline` rc=1 that could not be reproduced because
# nothing recorded which job it was. `make rebaseline` is on every close path,
# so this is the message a stuck session reads first.
_labels=()
spawn() {
    # Bound the fan-out. `wait -n` would be the clean way to block until any
    # one background job frees a slot, but `/bin/bash` on the box this runs
    # on is 3.2.57 (macOS ships it; `#!/usr/bin/env bash` resolves there) and
    # 3.2 has no `wait -n` — so poll instead. 0.2s granularity is noise next
    # to invocations costing seconds, and confirmed (by hand, off-script)
    # that `jobs -rp` reports backgrounded PIDs correctly in this script's
    # non-interactive context (job control being off affects terminal
    # signalling, not bash's own jobs table).
    while [ "$(jobs -rp | wc -l | tr -d ' ')" -ge "$HV_JOBS" ]; do sleep 0.2; done
    "$@" & _pids+=("$!")
    _labels+=("$*")
}
reap() {
    # `${_pids+"${_pids[@]}"}` rather than `"${!_pids[@]}"`: same reason the
    # original loop used it — bash 3.2 (what `#!/usr/bin/env bash` resolves to
    # on macOS) errors on an empty array under `set -u`. So walk the values and
    # carry the index by hand.
    local rc=0 p i=0
    for p in ${_pids+"${_pids[@]}"}; do
        if ! wait "$p"; then
            rc=1
            echo "regenerate-artifacts: JOB FAILED (pid $p): ${_labels[$i]}" >&2
        fi
        i=$((i + 1))
    done
    _pids=()
    _labels=()
    [ "$rc" -eq 0 ] || { echo "regenerate-artifacts: a parallel job failed (named above)" >&2; exit 1; }
}

# CENSUS HOST GUARD, hoisted to the top: with HV_CENSUS=1 this script writes
# the committed census goldens, which only the canonical box may author
# (decision 0063). Checked BEFORE the ~4 minutes of other regeneration, so a
# wrong-machine run is refused in a second rather than after the work.
if [ "${HV_CENSUS:-0}" = 1 ] && [ "${SKIP_CENSUS:-0}" != 1 ]; then
    # shellcheck source=scripts/census-canonical-host.sh
    . "$(dirname "$0")/census-canonical-host.sh"
    require_canonical_census_host census || exit 1

    # Serialize with any other heavy run on this box (decision 0081). This
    # script is one of three entry points that write census goldens and was
    # the only unguarded one a doc told you to use.
    #
    # RE-ENTRANCY IS NOT OPTIONAL: `flock` is per open-file-description, so if
    # census-run.sh already holds this lock and we re-flock the same path on a
    # fresh fd, we DEADLOCK against our own parent — and under a bounded wait
    # that means hanging the box for the full timeout. An ancestor that says
    # it holds the lock, and is still alive, means "already serialized".
    if [ -z "${HV_CENSUS_LOCK_HELD:-}" ] || ! kill -0 "${HV_CENSUS_LOCK_HELD}" 2>/dev/null; then
        exec 9>"${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
        census_timeout_s="${HV_CENSUS_WAIT_TIMEOUT:-2700}"
        echo "regenerate-artifacts: waiting for the census lock (up to ${census_timeout_s}s) …" >&2
        if ! flock -w "$census_timeout_s" 9; then
            echo "regenerate-artifacts: TIMED OUT after ${census_timeout_s}s waiting for the census lock." >&2
            exit 75
        fi
        export HV_CENSUS_LOCK_HELD=$$
    fi
fi


# Root from the script's own location, not `git rev-parse` — the remote gate
# runs this in an rsync'd tree that is not a git repository.
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# Intermediate world files are throwaway; their path never enters artifact
# bytes. A dedicated temp dir keeps them out of the tree.
work="$(mktemp -d "${TMPDIR:-/tmp}/hv-regen.XXXXXX")"
trap 'rm -rf "$work"' EXIT
w42="$work/hv-42.json"       # seed 42, tier-0 constant sun
wsky="$work/hv-sky.json"     # seed 42, generated sky (default)
wlocked="$work/hv-locked.json" # seed 42, tidally locked

# THE WRITE-SET CAPTURE (Task 4, The Attestation). Task 1's Step 1 measured
# how much of each declared generated path an ordinary run actually touches
# by hand, once, off a marker file and `find -newer`; this makes that
# measurement a byproduct of every run instead of a by-hand recipe someone
# has to re-derive. Stamp the marker now, before any generation happens; the
# footer near the end of this script counts, per declared path, how many of
# its tracked files carry an mtime newer than it. `sleep 1` guards against a
# filesystem whose mtime resolution is coarser than the time this script
# itself takes to reach its first write.
write_capture_marker="$work/hv-write-capture-marker"
touch "$write_capture_marker"
sleep 1

run() { cargo run -q "$@"; }
run_release() { cargo run -q --release "$@"; }

# `docs/audits/sentence-coverage.md` (The Stile). Unlike every artifact
# above, the generator is a `cargo test`, not a `cargo run` binary printing
# to stdout — `sentence_coverage_report` (`cli/tests/suite/sentence_corpus.rs`)
# writes the file itself via `std::fs::write` under `HV_SENTENCE_REBASELINE=1`,
# the same env-var-gated-test shape `make rebaseline-goldens` already uses
# for byte-golden fixtures, so this copies THAT working pattern rather than
# inventing a `> file` redirect this generator has no need of. Until this
# campaign nothing ever set the env var here, so the drift check on this
# file (covered by `docs/audits/` in `docs/generated-paths.txt`) could only
# ever compare the committed file against itself — a remedy (`make
# rebaseline` regenerates every artifact) that named no actual writer. See
# `REPORT_PATH`'s own doc comment in `sentence_corpus.rs` for the fuller
# account.
gen_sentence_coverage() { HV_SENTENCE_REBASELINE=1 cargo test -q -p hornvale --test suite -- sentence_coverage_report; }

echo "regenerate-artifacts: GROUP A — world builders (parallel)" >&2
spawn run -p hornvale -- new --seed 42 --sky constant --out "$w42"
spawn run -p hornvale -- new --seed 42 --out "$wsky"
spawn run -p hornvale -- new --seed 42 --rotation locked --out "$wlocked"
reap

# ---- Group B/C job bodies that are more than one `run` call ----------------
# (compound blocks: a hand-authored header/frame around one or more `run`
# invocations, all destined for one committed file). Wrapped in functions so
# `spawn` can background the whole block and the caller's `> file` redirect
# captures every printf and `run` inside it, exactly as the original `{ …; }
# > file` groups did.

# The live-pane preamble is hand-authored framing (The Casement, decision
# 0052): the possess dump replaces the whole file, so re-emit the preamble
# here rather than losing it on every regen — it was clobbered twice by
# earlier regen runs before this step carried it.
gen_possession_day0() {
    local possess_tmp
    possess_tmp="$(mktemp)"
    run -p hornvale -- possess --world "$wsky" --script scripts/possession-walk.txt > "$possess_tmp"
    head -n 1 "$possess_tmp"
    printf '\n*(This transcript is frozen. [The live pane](./possession-live.md) derives\nthe same world in your browser — same crates, same bytes.)*\n'
    tail -n +2 "$possess_tmp"
    rm -f "$possess_tmp"
}

# The over-time transcript (the-quickening, T4; the-wanting, T4): a NEW,
# separate recording — the day-0 transcript above never advances time, so it
# cannot show the world moving. This one `wait`s across a full drive cycle,
# so a derived NPC's homeostatic thirst rises and is satisfied (narrated by
# `wait`, felt directly through `needs`, and recounted with its own reason
# by `why`). Wiring it here (rather than editing the day-0 script) is what
# keeps the day-0 transcript byte-identical.
#
# THE CONFLUENCE (settlement condensation re-pointed at the real river
# network): this world's flagship settlement now sits directly on fresh
# water, so the NPC drinks in place rather than walking to it — `why`
# recounts a drink, not a journey. Not every settlement's fate (condensation
# lands most, not all, towns on the river network — a real, measured
# fraction, not every seed/settlement), but this world's own flagship
# settlement's real, measured outcome.
gen_possession_overtime() {
    local possess_ot_tmp
    possess_ot_tmp="$(mktemp)"
    run -p hornvale -- possess --world "$wsky" --script scripts/possession-over-time-walk.txt > "$possess_ot_tmp"
    # Both transcripts start at day 0, so `possess`'s own H1 is identical for
    # the two pages (The Running Head). Override it here rather than teaching
    # `possess` about the book's page layout: the day-0 page above keeps the
    # command's real heading, and only this page — which is defined by the
    # time it covers, not the day it opens on — is retitled.
    printf '# A Possession of Seed 42 — over time\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf '\n*(This transcript is frozen too — a recording, not a live session — but\nunlike the [day-0 transcript](./possession-seed-42.md), it `wait`s across a\nfull homeostatic drive cycle: watch a derived NPC grow thirsty and\nsatisfy it — narrated by `wait`, felt directly through `needs`, and\nrecounted with its own reason by `!why`. This settlement condenses\ndirectly onto fresh water (settlements-near-rivers): the NPC drinks in\nplace rather than walking to it, so `!why` recounts a drink, not a\njourney — not every settlement'"'"'s fate (condensation lands most, not\nall, towns on the river network), but this world'"'"'s own flagship\nsettlement'"'"'s real, measured outcome. The world still moves only\ninside a possess session; a freshly built world commits none of this.)*\n'
    tail -n +2 "$possess_ot_tmp"
    rm -f "$possess_ot_tmp"
}

# The custody transcript (The Chattel, Task 13). The campaign shipped six
# verbs — `open`, `close`, `take`, `drop`, `put`, `carrying` — and NO gallery
# page typed one of them: `possession-walk.txt` and
# `possession-over-time-walk.txt` are the only inputs the two seed-42
# transcripts are generated from, and neither uses any of the six. That
# absence was already load-bearing before anyone noticed it — decision 0399
# records `Session::take`'s own doc deferring a defect on the grounds that
# "their transcripts are in the galleries", which they were not — and it is
# why "regenerate the galleries" produced an empty diff at the end of two
# consecutive tasks.
#
# SEED 1, NOT 42, and the reason is measured rather than stylistic: seed 42's
# flagship structure does not draw enough chambers for its possession to reach
# a `Store` role, so no strongbox stands anywhere it can walk (decision 0398;
# `windows/vessel/tests/suite/strongbox_reachability.rs` holds the same fact
# as a test). 10 of 48 swept seeds reach one; 1 is the lowest. (Decision 0398
# recorded 8, measured in-process at `PossessOpts::default()`'s noon; The
# Custodian re-swept through this same CLI at its own `--day 0` default and
# got 10, unchanged across the key's move off the threshold. Two instruments,
# two numbers, and the CLI's is the one this comment is about.) This is
# therefore a NEW script rather than an edit to an existing one — the seed-42
# transcripts stay byte-identical.
#
# THE WALK CHANGED IN TASK 13'S FIX ROUND, AND SO DID WHAT THE PAGE IS
# EVIDENCE OF. The first version opened by taking the key straight out of a
# shut, LOCKED strongbox and then opening that strongbox with it — a lock
# defeated in one move, published as an ordinary retrieval. That was a real
# defect in `Session::take` (the lid gate ran on the ledger path only), and
# fixing it needed a second key pattern as well, since the only key in the
# world was inside the box it opened. The walk now does what the campaign
# always claimed: picks up the key `the-key-by-the-loom` composes beside the
# loom, carries it one chamber further, is REFUSED by the shut lid, opens the
# chest with the key in hand, and only then moves what is inside.
#
# IT CHANGED AGAIN IN THE CUSTODIAN, AND THE CLOSING BEAT GOT STRONGER FOR
# IT. That second key pattern was `roles: &[Role::Threshold]` — the role
# `interior::pattern::role_for` gives chamber index 0 unconditionally — so a
# key stood in the entrance of every built structure in every world, and the
# walk began by taking one on the first move. The pattern is
# `roles: &[Role::Loomroom]` now (`requires: Some(Loom)`, so the grammar
# confines it the way an alcove confines the fire), which is why the walk
# below types two `enter further in` before it finds anything to lift.
#
# IT CARRIES ITS OWN NEGATIVE CONTROLS, which is the other thing the first
# version lacked. `take a key` is typed twice in rooms that answer
# differently for reasons the page can state: once in the THRESHOLD chamber,
# whose grammar composes no key at all now ("You see no a key here."), and
# once at the shut chest ("The key is shut away in something closed.").
# Without them the closing beat — a key set down and picked up again — reads
# as a reply rather than as evidence, because nothing shows what a room
# WITHOUT one says.
#
# THE NEGATIVE CONTROL AND THE CLOSING BEAT ARE NOW THE SAME ROOM, which they
# were not before. The key is set down in the threshold chamber, whose
# grammar composes no key — so the retake reaches `take_from_the_ledger`'s
# second source rather than the SHADOWING path a room with its own key anchor
# takes. The page shows the same room answering "You see no a key here." and
# then "You take the key.", eighteen moves apart, with nothing between them
# but the ledger.
#
# SEED 14, NOT 1, SINCE THE PAVEMENT — AND THE PAIRING WITH
# `windows/vessel/tests/suite/strongbox_reachability.rs` IS THE POINT. A
# structure's chambers are drawn from its room's seed, the epoch moved every
# room address, and so which seeds draw a `Store` room is NOT preserved across
# one. Seed 1's flagship dwelling now draws three chambers — screen, alcove,
# loomroom — and no strongbox at all, so this page regenerated onto a
# transcript in which `open a strongbox` answers "You see no a strongbox here."
# five times while the caption below still promised a lid refusal and two keys.
# `strongbox_reachability.rs` had ALREADY moved to seed 14 for exactly this
# reason (its `CHAMBERED_SEED` doc records the sweep); this generator had not,
# and that split — one instrument about the property re-pointed, its published
# evidence left behind — is what let a gutted page through a green suite.
#
# Independently re-measured here before the move: over seeds 1..=60, the seeds
# whose flagship dwelling draws a strongbox AND two key-bearing chambers AND no
# key in the entrance chamber are 8, 14, 15, 23, 28, 31, 38, 49, 56, 58 — and
# within 1..=20 that is {7, 8, 11, 14, 15}, reproducing `CHAMBERED_SEED`'s own
# sweep exactly. 14 is the lowest that also draws the separate loomroom, which
# is the one this script's third `enter further in` lands in.
#
# THE TRIPWIRE IS `strongbox_reachability.rs`'s OWN PREMISE GUARD, not this
# comment: it asserts the chambered seed still composes a key and a strongbox
# and fails loudly when it stops. Because the two now name the same seed, the
# next epoch reddens a test instead of quietly hollowing out this page.
#
# ONE RUN, TWO ARTIFACTS. The same invocation writes the page and, through
# `--snapshot`, the committed `session-seed-14-carrying.json` fixture — the one
# artifact of `vessel/session/v2` in which `self.carrying` is NOT empty. Every
# seed-42 fixture records a possession that never typed `take`, so all of them
# carry `"carrying":[]`, which is exactly what a broken fold would emit too;
# a golden can only hold a field it has a non-empty value for. The script
# deliberately ends WITHOUT `release`, so the snapshot is taken with the key
# still in hand.
gen_possession_carry() {
    local possess_tmp
    possess_tmp="$(mktemp)"
    run -p hornvale -- possess --seed 14 --script scripts/possession-carry.txt \
        --snapshot clients/game/core/tests/fixtures/session-seed-14-carrying.json \
        > "$possess_tmp"
    # Retitled at this seam rather than in the command, the same move
    # `gen_possession_overtime` makes: `possess`'s own H1 is "A Possession of
    # Seed 14 — day 0", and this page is defined by what it does, not the day
    # it opens on.
    printf '# A Possession of Seed 14 — a thing carried\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf '\n*(This transcript is frozen. It is the only gallery page that types the\ncustody verbs — `take`, `drop`, `put`, `open`, `close`, `carrying` — and it\nis the campaign'"'"'s thesis end to end: a key is picked up beside a loom two\nchambers into a village dwelling, carried one room further, refused by a shut\nlid, and then used to open that chest. Two refusals are the evidence, not\nthe noise. `take a key` in the very first room answers \"You see no a key\nhere.\" — that room composes none, since The Custodian moved the key pattern\noff `Role::Threshold`, the one role every built structure has — which is\nwhat makes the closing beat, where a key set down in that same room is\npicked up again, a measurement rather than a reply. And `take a key` at the\nshut chest answers \"The key is shut away in something closed.\": the lid\nmeans something, and until The Chattel'"'"'s fix round it did not.*\n\n*A key on a floor is a stand-in for a PERSON, and reading it as a difficulty\nsetting is the mistake this page invites. The household that lives here\nwould hold its own key or stash it somewhere only a resident knows; the\ncustody mechanism for exactly that already exists and is body-agnostic. What\nis missing is the resident, so which rooms furnish a key is a\nprop-management knob for as long as nobody is home to carry one\n(`PLAY-key-placement-stands-in-for-a-resident`).*\n\n*Read the room descriptions as the GRAMMAR'"'"'s catalogue and not as an\ninventory, because that is what they are — and this dwelling has TWO keys,\nwhich is what makes the difference visible. Chamber prose renders the\npattern the room was composed from, never the ledger, so it moves for\nnobody: the storeroom lists \"a key\" while the chest is shut on it\n(`PLAY-closed-container-conceals-nothing`), and the front room says \"a\ndoorway and a screen\" on the last entry, silent about the key a player has\njust set down on its floor (`PLAY-room-prose-omits-what-the-ledger-holds`).\nBoth are the same absent read, and both are deferred with a priced bill\nrather than unnoticed.*\n\n*One more thing not to mistake for a bug: `close` does not re-lock. A lid\nand a lock are separate states, so the second `open` needs no key\n(decision 0399).*\n\n*Seed 14, and it was seed 1 until The Pavement. A dwelling'"'"'s chambers are\ndrawn from its room'"'"'s seed, and that campaign moved every room address, so\nwhich seeds draw a strongbox is not preserved across an epoch — seed 1'"'"'s\nflagship dwelling now draws three chambers and no chest, and this page\nregenerated onto it with every beat below the loomroom answering \"You see no\na strongbox here.\" The seed moved to the one\n`windows/vessel/tests/suite/strongbox_reachability.rs` had already moved to\nfor the same reason, so the test that guards this shape and the page that\npublishes it now name the same world.)*\n'
    tail -n +2 "$possess_tmp"
    rm -f "$possess_tmp"
}

# The chart reference fixture (Task 11, the-illumination; spec §5.3): the
# sim's own ASCII renderer's SHAPE for the seed-42 walk band, generated so
# `clients/game/core/tests/chart.rs`'s `the_shape_matches_the_sims_own_ascii_render`
# golden reads a committed artifact instead of the hand-pasted raw-string
# literal it replaces — a replica of the sim's answer that only a doc comment
# kept anyone re-deriving correctly. This reproduces exactly what that doc
# comment already told a human to do by hand: type `map` at the flagship
# possession's opening room and keep the five grid lines between the `sight:`
# caption and the `ways on:` footer — the same "terrain"/"colour" render this
# module's own comment already established as identical for this fixture.
#
# Captured through a command substitution, which strips the trailing newline
# `sed` leaves after its last printed line, so the fixture is byte-identical
# to the raw-string literal it replaces: `include_str!` would otherwise hand
# the test a string one byte longer than every `assert_eq!` in that file has
# ever compared against, silently changing what the golden means while
# looking like a pure relocation.
#
# `--seed 42` here is self-contained, same note as the turn-0 fixture below:
# it builds its own internal genesis and never reads $w42/$wsky/$wlocked.
gen_chart_reference() {
    local script_tmp shape
    script_tmp="$(mktemp)"
    printf 'map\n' >"$script_tmp"
    shape="$(run -p hornvale -- possess --seed 42 --script "$script_tmp" |
        sed -n '/^  sight:/,/^  ways on:/p' | sed '1d;$d')"
    printf '%s' "$shape"
    rm -f "$script_tmp"
}

# The legibility surface (living-community, T7): a real seed-42 site read back
# off the ledger as prose — its stratigraphy of occupation layers plus the
# derived flesh in the present-day grass. The framing paragraph below is
# hand-authored; the fenced block is the `history` verb's exact, drift-checked
# output. THE TWO MUST AGREE. `HISTORY_SITE` is the single source of the cell
# id for both, and `cli/tests/suite/docs_consistency.rs` asserts that the id named in
# the prose is the one the block reports and that the block is not empty —
# because they silently disagreed once. The moving-sea epoch (The Sundering)
# emptied the previously-pinned cell 36918 while its hand-authored paragraph
# went on describing a lineage that no longer existed there; the drift check
# passed throughout, since the *generated* half was current.
#
# The Contour (position-aware conflict, decision 0096): the same class of
# drift recurred a second time. The paragraph below IS the hand-authored
# half, embedded here rather than typed directly into the committed .md —
# editing the committed file alone (as a merge-reconciliation pass once did)
# does not survive the next `make rebaseline`, which re-emits this exact
# text. Fix drift HERE, not in the .md file, or the fix is silently undone by
# the next regen.
#
# The Contour epoch v2 (history/bake/v2, the BAKE label bump): a THIRD
# occurrence of the same drift class, from the label bump alone rather than
# from the mechanism itself — cell 28414 emptied again. Repointed at cell
# 1400, chosen because it is the richest single-people stratigraphy in the
# new world and because its shape is a small showcase of the mechanism this
# whole campaign adds: several of its completed layers ended not in cold but
# in eviction by a RIVAL gnoll band wanting the same defensible ground — a
# people fighting only itself over position, which a single strength scalar
# could not do.
#
# The Generalist (human joins the roster): a FOURTH occurrence of the same
# drift class, from a new competing people entering the settlement packer's
# roster rather than from any label or mechanism change — cell 1400 is still
# the richest single-people (all-gnoll) stratigraphy, but the packer's
# resolution against a sixth competitor shifted its stratigraphy from 20
# layers (year 500-1950) to 16 (year 550, still standing at the time of this
# regen). The counts below are read off the live block each time this
# comment is touched, not carried forward from memory of the last count.
#
# The Tolerance (warlikeness became a per-settlement draw instead of a
# per-species constant): a FIFTH occurrence, and the first that had to move
# the SITE rather than only re-count it. Cell 1400 collapsed to two layers,
# one of them zero-tenure — no longer a showcase for stratigraphy at all.
# Re-found by enumerating occ-site over the live seed-42 ledger: cell 21953
# is now the richest single-people (all-gnoll) column at 8 layers, and it is
# the better fit for this page's own thesis besides — it is genuinely
# CONTESTED, three of its seven completed layers ending in flight from
# another gnoll band and three more ending because the occupants won
# somewhere else and carried the settlement onto the ground they took.
# (Cell 3518 has 9 layers but every one of them ends in ice or famine, which
# would leave the page titled "The Contested Clearing" describing a quiet
# one.) All counts below re-read off the live block, per the rule above.
# Repointed from cell 21953 by The Tense: under era-varying capacity that cell
# holds no column at all, while 3293 carries sixteen layers. Repointing is
# legitimate HERE and was not when this page last went empty -- the earlier
# emptiness meant the world had no deep history to show, and moving the camera
# would have hidden a null result.
# Repointed AGAIN from 3293 by The Delvers (C2c): three new settling peoples
# re-decide settlement placement on every seed, and 3293 went empty. Same test
# as before -- is the WORLD empty, or only this cell? -- and the world is not:
# scanning occ-site over the seed-42 sky world, cell 5508 carries ELEVEN
# layers. So the camera moved and the null result is intact.
# Two honest notes, since a repointing is exactly where a quiet claim can slip
# in. (1) The deepest column on this seed is now 11 where it was 16; that is a
# reported number and this script asserts NO mechanism for the drop, because a
# roster change moves several things at once. (2) 5508 is all-kobold and stays
# a genuine fit for the page's title -- six of its layers end in flight from
# other kobolds and four more end because the occupants won ground elsewhere
# and carried the settlement onto it. All counts below are re-read off the live
# block, per the rule above.
# The Radiation (C2d) RE-COUNTED this block once, mid-campaign, rather than
# repointing it: 5508 went from eleven layers to TEN and stayed a fit. That
# re-count is now SUPERSEDED. The campaign moved placement twice more after it
# (the task-3 affinity relevel, and the founder-collision cut), and 5508 has
# collapsed to TWO layers -- both founded in the year 1025, one of them
# zero-tenure -- which is the same "no longer a showcase for stratigraphy at
# all" state that made The Tolerance repoint away from cell 1400. So the camera
# moves, under this block's standing rule.
#
# The rule, applied again: is the WORLD empty, or only this cell? The world is
# not. Scanning occ-site over the live seed-42 sky world, cell 4604 carries
# TWELVE layers, every one of them gnoll -- the deepest column on this seed and
# the deepest any repointing of this page has ever pointed at. Repointing is
# legitimate here for the same reason it was at The Tolerance and The Delvers,
# and would NOT be if the world itself had gone shallow; the null result stays
# reportable.
#
# Re-read off the live block for 4604: twelve layers from the year 525, eleven
# completed (seven put to flight by other gnolls, four leaving because they had
# taken better ground elsewhere and carried the settlement onto it), five of the
# twelve lasting under a year, founding parties arriving from five distinct
# neighbouring clearings, and the standing twelfth founded in the year 1775. No
# layer ends in ice -- every founding party instead ARRIVED fleeing it, the
# shape this page has carried at every cell it has ever pointed at.
#
# THE CAMERA MOVED AGAIN, 4604 -> 35120, for decision 0134's terrain epoch (The
# Glasshouse). New coastlines put cell 4604 under a different sea and emptied
# it; `the_history_page_prose_names_the_cell_it_renders` caught it by name
# rather than letting the showcase for stratigraphy quietly render "Nothing
# ever settled here". The standing rule applied once more: is the WORLD empty,
# or only this cell? Scanning occ-site over the live seed-42 sky world, cell
# 35120 carries TEN layers, every one hobgoblin, tied with 7754 for the deepest
# column on this seed. The world is not shallow, so repointing is legitimate,
# exactly as at The Tolerance and The Delvers.
#
# Re-read off the live block for 35120: ten layers from the year 300, nine
# completed -- five put to flight by other hobgoblins, four leaving because
# they had taken a neighbour's ground and carried the settlement onto it -- one
# layer lasting under a year, and the standing tenth founded in the year 1200.
# Nine of the ten founding parties arrived fleeing ice, from three distinct
# neighbouring clearings; the tenth was sent out from cell 35127. No layer ends
# in ice, the shape this page has carried at every cell it has ever pointed at.
# The column also carries its own technological arc, bronze -> iron ->
# classical, which none of the previous cells showed this cleanly. The deepest
# column on this seed has now gone 16 -> 11 -> 10 -> 12 -> 10 across five
# roster changes; that is a reported number and this script asserts NO
# mechanism for it, because a roster change moves several things at once.
#
# THE CAMERA MOVED AGAIN, 35120 -> 2738, for The Glasshouse Stage B (Tasks
# 4/5: the carbonate-silicate thermostat plus the area-mean-zero latitude
# profile). The warmer, redistributed population re-placed settlements again
# and emptied cell 35120; `the_history_page_prose_names_the_cell_it_renders`
# caught it by name, exactly as designed. The standing rule applied once
# more: is the WORLD empty, or only this cell? Scanning occ-site over the
# live seed-42 sky world, cell 2738 carries EIGHTEEN layers, every one
# hobgoblin — the deepest column this page has ever pointed at, and the
# deepest this seed has shown across every roster and terrain change
# recorded above. The world is not shallow, so repointing is legitimate.
#
# Re-read off the live block for 2738: eighteen layers from the year 0,
# seventeen completed -- twelve put to flight by other hobgoblins, five
# leaving because they had taken a neighbour's ground and carried the
# settlement onto it -- eight of the eighteen lasting under a year, and the
# standing eighteenth founded in the year 1800 (54 souls, "two huts and a
# granary" in the grass today, matching the standing layer's own peak). This
# cell differs from every prior one in ONE respect: the DEEPEST layer was not
# founded fleeing ice -- it was raised from nothing, "the first to break this
# soil" -- and every layer above it (seventeen of eighteen) arrived fleeing
# ice instead, driven off one of five distinct neighbouring clearings (10885,
# 10886, 10890, 10891, 10968). No layer ENDS in ice, the shape this page has
# carried at every cell it has ever pointed at; only the FIRST founding breaks
# the "arrived fleeing it" pattern, because there was no earlier hobgoblin
# ground to flee from. The column carries its own bronze -> iron -> classical
# arc, same as at 35120. The deepest column on this seed has now gone
# 16 -> 11 -> 10 -> 12 -> 10 -> 18 across six roster/terrain changes; that is
# a reported number and this script asserts NO mechanism for it.
#
# THE GLASSHOUSE, Stage B, resumed session: 2738 -> 7738. Task 5's latitude
# profile landed and main was absorbed (50 commits, The Repose), and between
# them cell 2738 went to ZERO occupations —
# `the_history_page_prose_names_the_cell_it_renders` caught it by name for the
# second time in one campaign, which is twice this guard has paid for itself
# here. The standing rule applied again, and the answer is the same: scanning
# `occ-site` over the live seed-42 sky world gives 620 occupations across 217
# distinct sites, so the WORLD is not empty and only this cell is. Deepest
# column is now cell 7738 at FIFTEEN layers (next are 8041 at 12 and 30730 at
# 11), so repointing is legitimate.
#
# Re-read off the live block for 7738: fifteen layers from the year 25,
# fourteen completed -- twelve put to flight by other hobgoblins, two leaving
# because they had taken a neighbour's ground and carried the settlement onto
# it -- five of the fifteen founded and ended in the SAME year, and the
# standing fifteenth founded in the year 1525 (75 souls, "two huts and a
# granary" in the grass today, matching the standing layer's own peak).
# This cell restores the pattern 2738 broke: ALL FIFTEEN layers, the deepest
# included, arrived fleeing ice, driven off one of five distinct neighbouring
# clearings (30689 x5, 30693 x3, 30690 x3, 30748 x2, 30745 x2). There is no
# first-breaker here at all — the ground has been a refuge from its first
# layer, which is a cleaner statement of the same shape than 2738 gave. No
# layer ENDS in ice, as at every cell this page has ever pointed at. The
# column carries the fullest technological arc yet: neolithic at the base,
# then bronze-working (ten layers), iron, and classical at the top. The
# deepest column on this seed has now gone
# 16 -> 11 -> 10 -> 12 -> 10 -> 18 -> 15 across seven roster/terrain/climate
# changes; that is a reported number and this script asserts NO mechanism
# for it.
#
# THE GLASSHOUSE, k re-decided (0.4 -> 0.3): 7738 -> 757, the THIRD repoint of
# this one page inside a single campaign. 7738 fell from fifteen hobgoblin
# layers to TWO (a high-elf steading and the human one that took it), and this
# time the guard caught it on its OTHER arm — not the empty-column check but
# the people cross-check, because the prose still said "hobgoblin" while the
# rendered column no longer contained one.
#
# THE PATTERN IS NOW THE FINDING, and it should be read before the fourth
# repoint rather than after. A showcase keyed on a HAND-PICKED CELL with
# hand-written narrative about that cell's specific contents is invalidated by
# every physics change, and this campaign has invalidated it three times
# (Task 4 emptied 35120, Task 5 plus the absorption emptied 2738, k emptied
# 7738). Each repoint costs a scan, a re-read and a rewritten paragraph. The
# guard is not the problem — it is the only reason the page has never shipped
# a lie — but the page's SHAPE is: prose asserting counts the tool could
# derive. Options a later campaign should weigh: derive the framing sentences
# from the rendered column, pin the showcase to a committed world rather than
# regenerating from the live seed, or keep the prose but strip it of specific
# counts. Not decided here; recorded so the fourth repoint is a choice.
#
# Read off the live block for 757: fifteen layers from the year 375, fourteen
# completed, and this column tells a DIFFERENT story from its predecessors.
# At 2738 and 7738 the endings were mostly flight; here EIGHT of the fourteen
# ended because the occupants took a neighbour's ground and carried the
# settlement onto it, against six put to flight — an expansionist column
# rather than a harried one, and the first time this page has shown that. Only
# one layer lasted under a year. All fifteen still arrived fleeing ice, from
# five distinct clearings (11930 x5, 11939 x4, 13032 x3, 13022 x2, 11941 x1),
# and none ends in ice, which is the one shape every cell this page has
# pointed at has shared. Tech runs bronze -> iron -> classical with no
# neolithic base. The standing fifteenth was founded in 1925 and holds 55
# souls. Depth across eight changes:
# 16 -> 11 -> 10 -> 12 -> 10 -> 18 -> 15 -> 15.
#
# THE UNDERWORLD, Task 8 (spec §4.6): 757 -> 5585, the FIFTH repoint, and the
# pattern above is unchanged and unaddressed. Re-keying the deep-history node
# index on (cell, rung) takes drow — the roster's one settled subterranean
# people — out of the competition for surface cells, so every people seeded
# after it draws from a different pool and the whole surface world re-rolls.
# 757 emptied; the guard caught it on the empty-column arm again.
#
# Deepest columns on the re-keyed seed 42 (sky world): 5585 and 22173 at NINE
# layers each, then 22170 / 22167 / 543 / 10626 / 32833 / 22193 / 5584 at
# eight. 5585 is taken: it and 22173 are neighbours that spent two thousand
# years taking the ground off one another, and 5585 is the one whose standing
# layer has a NAMED founder to read.
#
# RE-READ 2026-08-17 (The Underworld, Task 9), and the column moved. The
# genus join between `CaveKind` and the underworld corpus was repaired
# (`windows/worldgen/src/delve_seating.rs`), which moved drow's seating, which
# moved the bake — so this cell's stratigraphy is a different one and the
# framing prose below was re-derived from the new block rather than patched.
#
#
# RE-READ 2026-08-24 (The Granary, T7): the campaign's sub-year phase
# placement moved seed-42's raid/founding outcomes, and 5585 now renders an
# EMPTY column — the showcase had nothing to show. Repointed to 10626, found
# by dumping candidate cells through `history --site`: TWELVE kobold
# occupations from the year 200 to the present, seven put to flight by other
# kobolds, four departures that carried the settlement onto land taken from
# neighbours (2666 once, 10628 three times), one still standing.
#
# Read off the live block for 5585: SIX layers from the year 100, five
# completed, and the split is no longer even — THREE were put to flight, TWO
# left because they had taken a neighbour's ground and carried the settlement
# onto it (22170, 22193). No layer is a same-year founding-and-ending any
# more; the shortest holds 25 years. All six arrived fleeing ice, from TWO
# distinct clearings (22170 x4, 22169 x2), and none ends in ice — the one
# shape every cell this page has ever pointed at has shared. Tech runs a
# shorter arc than before: bronze at the base, then iron, then classical, with
# no neolithic layer left. The standing sixth was founded in 800 by
# Venggomnjen and holds 84 souls.
history_site=10626
gen_history() {
    printf '# The Contested Clearing of Seed 42\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'A site read back out of the ledger by the `history` verb: the stratigraphy\n'
    printf 'of every people that ever settled one vertex, oldest layer deepest, and the\n'
    printf 'derived flesh — the structures they raised, the residue in the grass\n'
    printf 'today. Nothing here replays the deep-history bake; it is all a\n'
    printf '*present-as-query* over committed occupation facts, with the flesh\n'
    printf '(structures, residue) derived on demand and never committed.\n\n'
    printf 'This is a real clearing on the world of seed 42 — vertex %s — and\n' "$history_site"
    printf 'twelve kobold steadings have risen on it, one settling atop the ruins\n'
    printf 'of the last, from the year 200 down to the present. No other people\n'
    printf 'ever touched this ground: it is a people with only itself to fight,\n'
    printf 'and it has fought itself here for eighteen centuries. Seven of the\n'
    printf 'eleven completed layers ended at kobold hands; four were not\n'
    printf 'evictions at all — the occupants drove rival kobolds off better\n'
    printf 'ground nearby (once off vertex 2666, three times off vertex 10628) and\n'
    printf 'carried the settlement onto the land they had taken, so the layer\n'
    printf 'closes on a departure rather than a defeat.\n\n'
    printf 'The cold is in this column, but never as an ending. Not one layer\n'
    printf 'fell to ice. Every layer instead *arrived* fleeing it — all twelve,\n'
    printf 'the deepest included, driven off one of four neighbouring clearings\n'
    printf '(2666 and 10628 most often, 10627 and 10638 once each). No one ever\n'
    printf 'broke this soil by choice: it has been a refuge from its first layer\n'
    printf 'to its last. Read bottom to top, the column carries a complete\n'
    printf 'technological arc — two neolithic layers at the base, then bronze,\n'
    printf 'iron, and six classical layers on top, the whole craft history of a\n'
    printf 'people in one stack of earth. The twelfth was founded in the year\n'
    printf '1875 and stands yet: some 24 souls, two huts and a granary, and no\n'
    printf 'ruin yet to read.\n\n'
    printf '```text\n'
    run -p hornvale -- history --world "$wsky" --site "$history_site"
    printf '```\n'
}

# The transport topology's legibility surface (The Connection Graph, T6): two
# real seed-42 sites read off the derived ConnectionGraph as prose, plus the
# world-level reachability overview. Cell 13980 is this world's flagship
# settlement -- inside the largest connected region, reached only by land
# routes here. Cell 28435 sits on a *different* landmass (a real, separate
# region under natural travel) and shows both a sea-lane and land routes at
# once, so the page demonstrates every edge kind the graph derives. Framing
# lines are hand-authored (the render replaces the file body, so re-emit
# them here); the fenced blocks are the `connections` verb's exact,
# drift-checked output.
gen_connections() {
    printf '# The Transport Topology of Seed 42\n\n'
    printf 'The connection graph'\''s legibility surface: a site'\''s natural sea-lanes and\n'
    printf 'overland routes, and which of the world'\''s naturally-connected regions it\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'belongs to, read off the `connections` verb. Nothing here is authored\n'
    printf 'infrastructure -- a "route" is always a natural corridor the terrain and\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'currents make easy, never a built road (see `EdgeKind`). The graph itself\n'
    printf 'is purely derived (no epoch, no seed draw): the same world always yields\n'
    printf 'the same topology.\n\n'
    printf '## A well-linked capital\n\n'
    printf 'The flagship settlement, on the world'\''s largest connected landmass. Its\n'
    printf 'own overland routes reach two neighboring settlements directly.\n\n'
    printf '```text\n'
    run -p hornvale -- connections --world "$wsky" --site 13980
    printf '```\n\n'
    printf '## A hub on a different shore\n\n'
    printf 'Vertex 28435 sits on a *separate* landmass under natural travel -- close\n'
    printf 'enough to its neighbors to reach several by both sea-lane and land route,\n'
    printf 'but with no natural corridor at all bridging it back to the flagship'\''s\n'
    printf 'larger region.\n\n'
    printf '```text\n'
    run -p hornvale -- connections --world "$wsky" --site 28435
    printf '```\n\n'
    printf '## The world, in sum\n\n'
    printf 'The world-level reachability summary: how many real regions natural\n'
    printf 'travel divides this world into, the largest, and the rest.\n\n'
    printf '```text\n'
    run -p hornvale -- connections --world "$wsky" --overview
    printf '```\n'
}

# The variety surface (the-shoal, T4): a global sample of rooms, so the book
# shows what the world's places actually read like. Roughly two thirds of any
# sample is sea — which is exactly why this page exists. Before The Shoal every
# one of those rows said "broken terrain", and no committed artifact sampled a
# marine room, so the gap was invisible in the book for as long as it existed.
gen_room_sample() {
    printf '# The Look of the World — Seed 42\n\n'
    printf 'A Fibonacci-lattice sample of rooms spread evenly over the globe, each\n'
    printf 'rendered by the locale window: its biome, its strangeness, and the\n'
    printf 'descriptor drawn for it. Most of any honest sample of a world is ocean,\n'
    printf 'so most of this page is ocean — the sea read at its own depths, with the\n'
    printf 'sunlit water described by its light and the lightless water not.\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'Generated by `hornvale locale --world world.json --sample 48`.\n\n'
    printf '```text\n'
    run -p hornvale -- locale --world "$wsky" --sample 48
    printf '```\n'
}

# The findability surface (the-occlusion, T7): the placed exotic sites. The
# strangeness budget keeps them a rare minority of land by design, so a random
# `locale --sample` essentially never lands on one — the tier was generated but
# unreachable. This listing is where it becomes visible.
gen_strange_sites() {
    printf '# The Strange Sites of Seed 42\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'The world'"'"'s placed exotic regimes: where each is, and what makes it\nstrange. Generated by `hornvale locale --world world.json --strange`.\n\n'
    run -p hornvale -- locale --world "$wsky" --strange
}

# THE UNDERWORLD WITNESS (The Stope, Task 2b). Until this line existed, no
# committed artifact carried one byte of chamber-lattice content: you could
# have deleted every chamber from every world and this whole script would have
# produced a byte-identical tree, so every later "regenerate and see what
# moved" would have said "nothing moved" and meant nothing.
#
# World-free (Group C): each `underworld` call builds its own world internally
# to BuildDepth::Terrain -- the shallowest rung a chamber needs -- and reads
# none of $w42/$wsky/$wlocked. Measured 0.37 s per seed against a warm binary,
# so the three-seed panel is ~1 s inside a ~50-60 s rebaseline.
#
# THREE SEEDS, not one: the campaign's own preregistered panel (spec S5). One
# seed's chamber counts are an anecdote, and a witness that moves on one world
# and not the other two is telling you something a single-seed readout cannot.
#
# Framing lines are hand-authored (the redirect replaces the whole file body,
# so re-emit them here); the fenced blocks are the `underworld` verb's exact,
# drift-checked output.
gen_underworld_lattice() {
    printf '# The Underworld of Seeds 42, 7 and 1234\n\n'
    printf 'The chamber lattice as three worlds actually realize it: how many cave\n'
    printf 'systems each has, how many chambers exist beneath them, how those chambers\n'
    printf 'distribute over the delve ladder and over the rock they sit in, and then --\n'
    printf 'run by run -- the first three cave systems of each world.\n\n'
    printf 'A chamber is never stored. Existence and content are pure functions of an\n'
    printf 'address, so this page is a *witness*, not a record: every line is re-derived\n'
    printf 'from the seed on each regeneration, and a change to the derivation key, to\n'
    printf 'the existence gate, to a run'"'"'s drawn length, to a chamber'"'"'s content or\n'
    printf 'to the depth the rock grants a cave moves bytes here.\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf '`reachable` is the count a player would actually experience: chambers\n'
    printf 'reachable from an entrance by the passage graph, whose vertical axis is\n'
    printf 'the descent sequence (a run'"'"'s drawn length is its sojourn; past it, the\n'
    printf 'next band'"'"'s floor 0). That sequence is the number, not a rounding error.\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'The `key` column is a DISPLAY FORMATTER of that run'"'"'s floor 0'"'"'s address,\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'not a derivation key (The Drift, spec amendment A.6): Task 1 deleted the\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'chamber existence draw, chamber_key'"'"'s only production caller, so nothing in a\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'shipped world derives from it any more. The real derivation keys are\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf '`levels_in_branch`'"'"'s RUN_FLOORS leg and the three per-branch legs in\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf '`crate::character` -- this column still witnesses the address'"'"'s SPELLING (the\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'rung name, the field order), which is what makes it worth printing.\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'Each run shows one glyph per floor the LATTICE admits, never per floor the\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'run drew: a `#` exists, a `.` sits deeper than the cave'"'"'s budget\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'reaches, and a `_` is past that run'"'"'s\n'
    printf 'own drawn length. Bounding the row by the drawn length instead is what made\n'
    printf 'an earlier version of this page unable to see either floor gate at all.\n\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'The `junctions` line counts the links between DIFFERENT cave systems that\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf '`junctions_at` derives at each shared delve band -- derived, not drawn: it\n'
    printf 'consumes no stream leg OF ITS OWN, so a shortcut is a fact about the geology\n'
    printf 'rather than a die roll on top of an epoch. (It does travel the legs the facts\n'
    printf 'it reads already have -- a branch-count draw here, a branch character there --\n'
    printf 'and an earlier version of this page dropped that qualifier and asserted the\n'
    printf 'falsehood that it consumes no stream at all.) A link is an EDGE, counted once\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'rather than once per endpoint; `largest network` is the largest component\n'
    printf 'within a SINGLE band, because a junction never crosses one -- so systems\n'
    printf 'joined only at the Undercroft and systems joined only at the Nadir are two\n'
    printf 'networks, not one, and unioning them would name a component nothing can\n'
    printf 'walk.\n\n'
    printf '```text\n'
    run -p hornvale -- underworld --seed 42
    printf '\n'
    run -p hornvale -- underworld --seed 7
    printf '\n'
    run -p hornvale -- underworld --seed 1234
    printf '```\n'
}

# The Crosscut's committed witness (spec §4): the four preregistered readouts
# — loop share, density ordering, cross-floor cycles, semilattice overlap —
# over every cave-bearing, non-ocean vertex of the same standing seed panel.
#
# Framing lines are hand-authored (the redirect replaces the whole file body,
# so re-emit them here); the fenced blocks are the `circuit` verb's exact,
# drift-checked output.
gen_underworld_circuit() {
    printf '# The Circuits of Seeds 42, 7 and 1234\n\n'
    printf 'What this page witnesses: for each seed'"'"'s every cave-bearing,\n'
    printf 'non-ocean vertex, a descent plan is grown (spec §3) and the four\n'
    printf 'preregistered readouts of spec §4 are measured against it, then\n'
    printf 'reduced to a panel median. A descent plan is never stored -- it is a\n'
    # shellcheck disable=SC2016  # markdown code spans: the backticks are literal
    printf 'pure function of `(seed, vertex, rungs, kind, character)`, so this page\n'
    printf 'is a witness, not a record: every number here is re-derived from the\n'
    printf 'seed on each regeneration.\n\n'
    printf 'The verdict words PASSED / FALSIFIED are frozen by spec §4, decided\n'
    printf 'before this code existed, and nothing here is tuned to reach one -- a\n'
    printf 'FALSIFIED verdict is a finding the campaign publishes, not a bug to fix\n'
    printf 'by moving the threshold. A comparison that cannot be made for a seed\n'
    printf '(no cave of some kind exists there) prints NOT MEASURABLE rather than a\n'
    printf 'vacuous PASSED.\n\n'
    printf '```text\n'
    run -p hornvale -- circuit --seed 42
    printf '\n'
    run -p hornvale -- circuit --seed 7
    printf '\n'
    run -p hornvale -- circuit --seed 1234
    printf '```\n'
}

# The atlas bundle. Without this line book/src/gallery/ is in the drift-check
# list but atlas.js is never rebuilt, so `git diff --exit-code` compares the
# committed file against itself and reports clean forever (The Staff, Task
# 9). World-free and outside cargo entirely (clients/atlas is a Deno
# workspace, decision 0055's determinism boundary), so it belongs beside the
# other Group C dumps below, not Group A/B.
build_atlas() {
    if command -v deno >/dev/null 2>&1; then
        (cd "$repo_root/clients/atlas" && deno task build)
    else
        echo "regenerate-artifacts: deno not found — SKIPPING the atlas bundle." >&2
        echo "  book/src/gallery/atlas.js will not be refreshed by this run." >&2
    fi
}

# The Purview's legibility surface (The Margin): the same scene/surrounds/v1
# chart the JSON export above carries, rendered through --render ascii at
# three genuinely different seed-42 observers -- the flagship settlement
# (uniform, kept for continuity with the possession transcript), a coastline
# half a degree east of Mjoexaenoenoa where the neighbourhood's own room mix
# reads land against ocean, and a room at latitude -10, longitude 0 that
# crosses a base-icosahedron face seam.
#
# `book/src/gallery/surrounds-seed-42.md` is hand-authored prose, NOT
# generated here -- edit it directly. Only the three CHARTS it `{{#include}}`s
# are regenerated, each to its own small file under generated/surrounds-
# seed-42/, following the `{{#include generated/<study>/...}}` convention the
# lab pages already use (book/src/laboratory/). This is the fix for the
# failure mode the previous shape had: the whole page used to be `printf`'d
# from here, so a direct edit to the committed .md was silently destroyed on
# the next regen. These chart files are `scene surrounds --render
# ascii`'s exact, drift-checked output -- excluded from CI's strict
# byte-drift check (ci.yml) for the same libm-threshold reason as
# scene-surrounds-seed-42.json, since they render the identical
# `biome`/`water`/`relief` classifications; the hand-authored .md that
# includes them carries no such exposure and is checked normally.
gen_surrounds_flagship() {
    printf '$ hornvale scene surrounds --world world.json --render ascii\n'
    run -p hornvale -- scene surrounds --world "$wsky" --render ascii
}
# THE TWO OBSERVER IDS BELOW WERE RE-MINTED BY THE PAVEMENT, AND THE OLD ONES
# ARE NOT MERELY STALE — THEY DO NOT DECODE. A room id packs its base face in
# its low five bits; the occupancy lattice is a cube-sphere now, so `FacetId::
# unpack` refuses any face >= 6, and the retired pair were faces 11 and 14 of
# the icosphere. That refusal is decision 0189 working (a pre-flip address must
# fail loudly, never decode into a valid-looking one) and it aborted this
# script partway through until the ids were re-minted.
#
#   coastline  897392747 (face 11, depth 12) -> 3015902083 (face 3, depth 13)
#   seam       724698318 (face 14, depth 12) -> 2290649216 (face 0, depth 13)
#
# The coastline observer keeps its GROUND: 3015902083 is the walk-depth room
# containing 17.1745 N, 103.6804 W, the same point the old id addressed. The
# seam observer could not — see `book/src/gallery/surrounds-seed-42.md`, which
# states why in full.
gen_surrounds_coastline() {
    printf '$ hornvale scene surrounds --world world.json --room 3015902083 --render ascii\n'
    run -p hornvale -- scene surrounds --world "$wsky" --room 3015902083 --render ascii
}
gen_surrounds_seam() {
    printf '$ hornvale scene surrounds --world world.json --room 2290649216 --render ascii\n'
    run -p hornvale -- scene surrounds --world "$wsky" --room 2290649216 --render ascii
}

echo "regenerate-artifacts: GROUP B+C — world readers and world-free dumps (parallel)" >&2

# `systems matrix` (below) is the first Group C job with a dependency ON
# ANOTHER GROUP C ARTIFACT: `RepoFacts::gather` reads
# `docs/digest/decisions-in-force.md`, which `digest render decisions`
# regenerates a few lines down — and until now nothing in this batch read a
# sibling's OUTPUT, only $w42/$wsky/$wlocked from the already-reaped Group A.
# Spawning both in the same untethered batch races: a `systems matrix` that
# starts before the digest job finishes writing can read a truncated or
# stale file and fail with "parsed to zero in-force decisions" (caught by
# running this script, not by any test — nothing exercises the two
# concurrently). So the digest decisions render runs first, alone, and is
# reaped before anything that might read its output is spawned; `render
# delta` has no such reader in this script and stays in the main batch
# below.
spawn run --manifest-path tools/digest/Cargo.toml -- render decisions \
  > docs/digest/decisions-in-force.md
reap

# Group C: world-free dumps (see classification comment above).
spawn run -p hornvale-kernel --example first_light
spawn run -p hornvale -- book > book/src/gallery/the-book.md
spawn run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
spawn run -p hornvale -- concepts --manifest > book/src/reference/concept-manifest-generated.md
spawn run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
spawn run -p hornvale -- phonology > book/src/reference/phonology.md
spawn run -p hornvale -- proto goblinoid > book/src/reference/proto-goblinoid-generated.md
spawn run -p hornvale -- proto dwarf > book/src/reference/proto-dwarf-generated.md
spawn run -p hornvale -- proto elf > book/src/reference/proto-elf-generated.md
spawn run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
# The plumb coverage report (The Plumb, Task 4): every authored numeric
# constant's rung, the same shape as the type-audit report above and drawn
# from the same tag grammar. Regenerates cheaply — a source scan, no build.
spawn run --manifest-path tools/plumb/Cargo.toml -- report > docs/audits/plumb-roster.md
# The seam-guard roster. STATIC by design — registrations, declarations and
# call sites, never verdicts (those cost a scoped test run per site, so an
# artifact carrying them could not be regenerated cheaply). Its job is to put
# every `expect(survives: …)` declaration under review pressure: a diff is
# harder to leave lying around than a doc comment.
spawn run --manifest-path tools/seam-guard/Cargo.toml -- report > docs/audits/seam-guard-roster.md
# The placement-audit roster: one section per shape-twin group across
# kernel/domains, each member's verdict and shape fingerprint (decision
# 0517; The Hallmark spec §3).
spawn run --manifest-path tools/placement-audit/Cargo.toml -- report > docs/audits/placement-audit-roster.md
spawn run -p hornvale -- tropes report > docs/audits/trope-coverage-polti-1895.md
spawn run -p hornvale -- tropes --corpus tropes/tvtropes-2012.trope.json report \
  > docs/audits/trope-coverage-tvtropes-2012.md
spawn run -p hornvale -- tropes matrix > docs/audits/trope-matrix.md
spawn run -p hornvale -- systems report > docs/audits/system-coverage-wolverson-2021.md
spawn run -p hornvale -- systems matrix > docs/audits/system-matrix.md
spawn gen_sentence_coverage
# The Confidant, Task 7 reshape: world-invariant (builds its own internal
# Seed(42), like `first_light` above), so it belongs in Group C alongside
# the other world-free/self-contained dumps rather than among $w42's readers.
spawn run -p hornvale -- lab confidant > docs/audits/the-confidant-report.md
# The Reticence, Task 6: builds its own internal Seed(42) too (see
# `render_reticence_report`'s own doc for why the full sculpt is paid for
# despite the felt-state half of its answer being world-invariant), so it
# belongs in Group C beside the Confidant's line rather than among $w42's
# readers.
spawn run -p hornvale -- lab reticence > docs/audits/the-reticence-report.md
spawn run --manifest-path tools/digest/Cargo.toml -- render delta \
  > docs/digest/intent-vs-reality.md
spawn gen_underworld_lattice > docs/audits/underworld-lattice-seed-panel.md
spawn gen_underworld_circuit > docs/audits/underworld-circuit-seed-panel.md
spawn build_atlas

# Group B: readers of $w42/$wsky/$wlocked.
spawn run -p hornvale -- almanac --world "$w42" > book/src/gallery/almanac-seed-42.md
spawn run -p hornvale -- almanac --world "$wsky" > book/src/gallery/almanac-seed-42-sky.md
spawn run -p hornvale -- almanac --world "$wlocked" > book/src/gallery/almanac-seed-42-locked.md
spawn run -p hornvale -- explain --world "$wsky" sky > book/src/gallery/explain-seed-42-sky.md
spawn run -p hornvale -- gazetteer --world "$wsky" > book/src/gallery/gazetteer-seed-42.md
spawn run -p hornvale -- dictionary --world "$wsky" > book/src/reference/dictionary-generated.md
# 1015166224 was face 16 of the icosphere and no longer decodes (see the
# observer note above gen_surrounds_coastline). 2853504131 is the walk-depth
# room containing the same point, 26.2560 N 132.4941 W — the same taiga
# hollow, re-addressed.
spawn run -p hornvale -- locale --world "$wsky" --room 2853504131 --json > book/src/reference/locale-seed-42.json
spawn gen_possession_day0 > book/src/gallery/possession-seed-42.md

# The committed session fixture (The Quire, Task 3): `hornvale-game-core`'s
# render tests read this instead of paying for genesis (measured 1.43 s).
# Regenerated here, beside the transcripts above, so it cannot silently lag
# `vessel/session/v2`'s schema.
#
# `--script` is REQUIRED here, even though the script is empty (the fixture
# is turn 0, the opening — no verb should run before the snapshot). Every
# OTHER `possess` call in this file passes `--script`, which routes input
# through a `Cursor`; without one, `possess` falls into its interactive arm
# and blocks reading `stdin.lock()`. That hangs a human running `make
# rebaseline` from an ordinary terminal even though it is invisible to a
# non-interactive agent or CI, whose stdin is already at EOF — an empty
# `--script` cannot block either way. `--lens`/`--echo` differ between the
# two arms, but neither reaches the snapshot (only the terminal draw does;
# see `PossessOpts::lens`'s doc), so this is byte-identical to the possess
# call this replaced.
#
# NOTE (traced, not guessed): `--seed 42` here builds its own internal
# genesis and never reads $w42/$wsky/$wlocked, so this has no real
# dependency on Group A's reap — it is scheduled here anyway because it
# shares no write target with anything else in this block.
# The glyph specimen sheet (The Legend, Task 5): candidate elevation ladders
# and the sim's own impedance ladder, rendered at the 80x24 monochrome floor
# so selection happens against the medium the glyphs ship in, not a
# document. `clients/game` is OUTSIDE the cargo workspace (root Cargo.toml's
# `exclude`), so `run`'s `cargo run -p ...` cannot reach it -- the same
# subshell-`cd` shape `build_atlas` uses for `clients/atlas`, the other
# out-of-workspace client.
gen_glyph_specimen_sheet() {
    (cd "$repo_root/clients/game/bin" && cargo run -q --example specimen_sheet)
}
spawn gen_glyph_specimen_sheet > docs/audits/glyph-specimen-sheet.txt

mkdir -p clients/game/core/tests/fixtures
spawn run -p hornvale -- possess --seed 42 --script scripts/possession-empty.txt \
    --snapshot clients/game/core/tests/fixtures/session-seed-42-turn-0.json > /dev/null

# The committed CHAMBER-band fixture (The Quire, Task 4 fix round): the
# turn-0 fixture above always lands on `spatial.band == "walk"`, so
# `hornvale-game-core`'s `Spatial::Chamber` mirror (`Plan`, `PlanExtent`,
# `PaletteEntry`, `PlanPoint`, `PlanMark`) had no committed coverage —
# nothing would catch a regression before Tasks 6/7 lean on those types.
# `scripts/possession-chamber.txt` is a single `enter`, verified to land
# seed 42's flagship possession inside a structure (`spatial.band ==
# "chamber"`) from its opening room — the same first move
# `possession-walk.txt` makes. `--script` is required for the same reason
# as the turn-0 call above: without it `possess` blocks on `stdin.lock()`.
# (Same note as above: `--seed 42` is self-contained, no Group A dependency.)
spawn run -p hornvale -- possess --seed 42 --script scripts/possession-chamber.txt \
    --snapshot clients/game/core/tests/fixtures/session-seed-42-chamber.json > /dev/null

spawn gen_chart_reference > clients/game/core/tests/fixtures/chart-reference-seed-42.txt

spawn gen_possession_carry > book/src/gallery/possession-carry-seed-14.md
spawn gen_possession_overtime > book/src/gallery/possession-over-time-seed-42.md
spawn gen_history > book/src/gallery/history-seed-42.md
spawn gen_connections > book/src/gallery/connections-seed-42.md

spawn run -p hornvale -- map --world "$wsky" --out book/src/gallery/elevation-seed-42.png \
    > book/src/gallery/elevation-seed-42.md
spawn run -p hornvale -- biome-map --world "$wsky" --out book/src/gallery/biome-seed-42.png \
    > book/src/gallery/biome-seed-42.md
spawn run -p hornvale -- biome-map --world "$wlocked" --out book/src/gallery/biome-seed-42-locked.png \
    > book/src/gallery/biome-seed-42-locked.md
spawn run -p hornvale -- settlement-map --world "$wsky" --out book/src/gallery/settlement-seed-42.png \
    > book/src/gallery/settlement-seed-42.md
spawn run -p hornvale -- settlement-map --world "$wlocked" --out book/src/gallery/settlement-seed-42-locked.png \
    > book/src/gallery/settlement-seed-42-locked.md
spawn run -p hornvale -- paleo-map --world "$wsky" --out book/src/gallery/paleo-seed-42.png \
    > book/src/gallery/paleo-seed-42.md
# The sediment/carve-delta lens (Sculpting): PNG only — no committed .md
# sibling yet, so the markdown goes to /dev/null.
spawn run -p hornvale -- map --world "$wsky" --out book/src/gallery/sediment-seed-42.png \
    --field sediment > /dev/null
spawn run -p hornvale -- map --world "$wsky" --out book/src/gallery/column-seed-42.png \
    --field column > book/src/gallery/column-seed-42.md
spawn run -p hornvale -- map --world "$wsky" --out book/src/gallery/features-seed-42.png \
    --field features > book/src/gallery/features-seed-42.md
spawn run -p hornvale -- vestige-map --world "$wsky" --out book/src/gallery/vestige-seed-42.png \
    > book/src/gallery/vestige-seed-42.md
spawn run -p hornvale -- star-chart --world "$wsky" --out book/src/gallery/star-chart-seed-42.png \
    > book/src/gallery/star-chart-seed-42.md

spawn run -p hornvale -- scene tiles --world "$wsky" > book/src/gallery/scene-tiles-seed-42.json
spawn run -p hornvale -- scene tiles-region --world "$wsky" --face 0 --level 3 --ix 4 --iy 4 --samples 16 > book/src/gallery/scene-tiles-region-seed-42.json
spawn run -p hornvale -- scene moons --world "$wsky" > book/src/gallery/scene-moons-seed-42.json
spawn run -p hornvale -- scene neighbors --world "$wsky" > book/src/gallery/scene-neighbors-seed-42.json
spawn run -p hornvale -- scene eclipses --world "$wsky" --from 0 --until 2000 > book/src/gallery/scene-eclipses-seed-42.json
spawn run -p hornvale -- scene surrounds --world "$wsky" > book/src/gallery/scene-surrounds-seed-42.json

spawn gen_room_sample > book/src/gallery/room-sample-seed-42.md
spawn gen_strange_sites > book/src/gallery/strange-sites-seed-42.md

mkdir -p book/src/gallery/generated/surrounds-seed-42
spawn gen_surrounds_flagship > book/src/gallery/generated/surrounds-seed-42/flagship.txt
spawn gen_surrounds_coastline > book/src/gallery/generated/surrounds-seed-42/coastline.txt
spawn gen_surrounds_seam > book/src/gallery/generated/surrounds-seed-42/seam.txt

reap

# GROUP D — the lab studies. Serial with respect to each other, and never
# co-scheduled with A/B/C: `lab run` already saturates every core internally
# (windows/lab/src/runner.rs reads std::thread::available_parallelism()), so
# running it alongside anything else would oversubscribe rather than help.
echo "regenerate-artifacts: GROUP D — lab studies (serial)" >&2

echo "regenerate-artifacts: the chorus study (C4/LANG-41, 50 seeds; live, not a census)" >&2
run_release -p hornvale -- lab run studies/the-chorus.study.json

# Censuses are still opt-in (HV_CENSUS=1) so the everyday gate stays fast:
# skipped BY DEFAULT, and SKIP_CENSUS=1 (CI's fast probe path) also skips.
# But since decision 0063 (The Local Census cut the per-world cost ~285 → ~8
# CPU-s) the sanctioned refresh is a local run ON THE CANONICAL BOX
# (`lefford`): `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh` once per
# campaign at the pre-merge close — the full ~2000-world census takes ~7 min —
# keeping the fixtures current with main instead of lagging. `make
# regen-remote` (the AWS box) is ABANDONED. Note "local" means "not AWS", NOT
# "whichever machine you are on": `lefford` is the single canonical platform,
# because boxes differ on ~0.1% of discrete-count metrics (0063). The guard
# below enforces that; see scripts/census-canonical-host.sh.
if [ "${HV_CENSUS:-0}" = 1 ] && [ "${SKIP_CENSUS:-0}" != 1 ]; then
    # (host already verified at the top of this script)
    echo "regenerate-artifacts: lab censuses (release; HV_CENSUS=1; ~7 min, canonical box)" >&2
    run_release -p hornvale -- lab run studies/the-census.study.json
    run_release -p hornvale -- lab run studies/census-of-the-meeting.study.json
else
    echo "regenerate-artifacts: censuses SKIPPED (HV_CENSUS=1 on the canonical box to refresh; ~7 min, decision 0063)" >&2
fi

# RE-DERIVE every census schema from the CURRENT metric registry.
#
# WHY THIS EXISTS. `schema.json` is written by whichever branch last ran the
# census (publish.rs), so it carries the schema FIELDS that branch knew about —
# not the ones the registry has now. A campaign that adds a per-metric field
# (The Domesday added `domain` and `role`) silently invalidates every committed
# schema until someone re-derives it by hand. That happened TWICE in one
# campaign: once from The Delvers' census, once from The Assize's, each time
# caught only by a test that went red naming the offending metric.
#
# `backfill-schema` re-renders the manifest from the committed rows.csv against
# the live registry. It builds NO world, so this is safe to run unconditionally
# and costs nothing. It must run AFTER the census block above (so a fresh run's
# rows are re-schema'd) and BEFORE the domesday survey below (which reads it).
# NOT parallelised with Group D above (or anything else): it reads whatever
# Group D's census pair just wrote, so it stays a serial trailer.
#
# The `"backfilled": true` marker the re-render adds is accurate: a committed
# schema IS derived after the fact relative to the census run that wrote its
# rows.
echo "regenerate-artifacts: re-deriving census schemas from the current registry" >&2
for study in the-census census-of-the-meeting; do
    rows="book/src/laboratory/generated/$study/rows.csv"
    schema="book/src/laboratory/generated/$study/schema.json"
    if [ -f "$rows" ]; then
        # Write via a temp so a failure cannot leave a truncated manifest.
        run -p hornvale -- lab backfill-schema "studies/$study.study.json" "$rows" > "$schema.tmp"
        mv "$schema.tmp" "$schema"
    fi
done

# The Domesday survey (2026-08-08 campaign): reads the COMMITTED census at
# book/src/laboratory/generated/the-census/ (whatever the last HV_CENSUS=1
# refresh left there, not a fresh run) and renders book/src/domesday/ — the
# index plus one page per domain. It never triggers a census itself (spec
# §4.5), so it runs unconditionally here, independent of the HV_CENSUS gate
# above. Serial (not parallelised): it reads the schema the backfill loop
# just wrote.
echo "regenerate-artifacts: the domesday survey" >&2
run -p hornvale -- lab domesday

# The anomaly report (The Gnomon, 2026-08-13): the Domesday's transpose, per
# world rather than per column. Also a pure read over the same COMMITTED
# census — it never triggers a census itself — so it runs unconditionally
# here too. It is a SERIAL TRAILER for the same reason `domesday` is: it
# reads the schema the backfill loop above just wrote, so it cannot be
# spawned into Group B+C.
echo "regenerate-artifacts: the anomaly report" >&2
run -p hornvale -- lab anomalies

echo "regenerate-artifacts: done." >&2

# Emit the write-set capture (Task 4, The Attestation): one row per declared
# path EXCEPT a `census`-authored one (see the scoping note below, added by
# the final review's I3 fix), `path<TAB>written<TAB>tracked`, where `written`
# is how many of that path's git-tracked files carry an mtime newer than the
# marker stamped at the top of this run (mtime ADVANCED, not content changed
# — a file this loop rewrites byte-for-byte identically still counts as
# written), and `tracked` is how many of its files git tracks at all. This
# is a READ over mtimes and `git ls-files`; it changes no artifact's bytes.
#
# `census`-AUTHORED ROWS ARE EXCLUDED, AND THIS WAS NOT ALWAYS TRUE (review
# finding I3, final review). This footer used to iterate every declared row
# regardless of author, including `census`-authored rows such as
# `book/src/laboratory/generated/the-census/`. Those rows have TWO authors
# that write different bytes through this one script, gated by `HV_CENSUS`:
# a PLAIN run (this branch, `HV_CENSUS` unset) never rewrites the census
# study's own output, so a census-authored row measured 1/229 (only
# `schema.json`, rewritten unconditionally by the backfill loop below); a
# CENSUS run (`HV_CENSUS=1`) rewrites the whole study, so the same row
# measures 229/229. `scripts/sluice-census.sh`'s `git add -u` commits
# whichever shape a census run left, and the next ordinary merge's
# `artifacts` phase then overwrote it with the plain-run shape — perpetual,
# silent, two-way churn on a drift-checked artifact, with nothing
# downstream able to tell the two shapes apart (`measured_writes()` in
# `cli/tests/suite/generated_paths.rs` reads only key presence, never the
# counts).
#
# ONLY `census` IS EXCLUDED, NOT EVERY NON-`artifacts` AUTHOR — `heavy`-
# authored and `none(...)` rows STAY IN, deliberately, and this needed a
# second look before shipping: a first attempt at this fix scoped emission
# to `artifacts`-authored rows only, which broke
# `an_overriding_declaration_must_be_measured` — that test requires EVERY
# overriding row, of ANY author including `none(...)`, to have a
# writes.tsv entry (key presence only, not the values), and most of the
# `none(...)` rows are exactly such overrides (e.g. `book/src/gallery/
# the-sky.md` overriding `book/src/gallery/`'s `artifacts`). `heavy`- and
# `none(...)`-authored rows have NO invocation ambiguity to exclude for:
# this script never writes either kind of path under any flag it accepts
# (`the-history/`/`the-sounding/` are written only by the separate heavy
# test binary; a `none(...)` row's whole claim is that nothing here writes
# it), so their captured counts are stable — always the same value — no
# matter which invocation ran. `census` is the one author whose bytes
# genuinely depend on which of two distinct invocations produced them, and
# excluding exactly that author loses no coverage
# `an_overriding_declaration_must_be_measured` needs, since every override
# case that test resolves today is a non-`census` row overriding a
# less-specific `census` row (e.g. `.../the-census/schema.json` over
# `.../the-census/`), never the reverse.
#
# THE OUTPUT FILE CANNOT MEASURE ITSELF, AND MUST NOT PRETEND TO (review
# finding, The Attestation Task 4). `docs/generated-path-writes.tsv` is
# itself declared in docs/generated-paths.txt with author `artifacts` — that
# declaration is correct, the file really is written every run — but a
# shell output redirect on a compound command (`{ ...; } > file`) truncates
# and stamps the target's mtime at REDIRECT-OPEN time, before the body
# inside it runs (confirmed empirically: a body emitting zero bytes still
# moved the file's mtime to within 0.0002s of a marker set immediately
# before). So by the time this loop would check its own row, this file's
# mtime has already moved past the marker — every single run, unconditional
# on whether anything below actually changed. A "written" value for this
# row would be guaranteed true by construction, not observed, and Step 3's
# falsifier ("delete the generator, watch the count drop to zero") cannot
# apply to it: deleting this very footer deletes the file's only writer, so
# there is no way to distinguish "not written" from "not generated at all".
# Presenting a number here would be exactly the failure this instrument
# exists to catch, so the self-referential path is named and explicitly
# excluded below rather than given a fabricated row.
write_capture_self_path="docs/generated-path-writes.tsv"
#
# Skipped outside a git checkout: `repo_root` above is deliberately resolved
# without `git rev-parse` because the (abandoned, decision 0063) AWS path
# once ran this script against an rsync'd, non-git tree, and `git ls-files`
# has no answer there. A missing write-set capture must never turn a
# working regeneration into a failed one.
if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "regenerate-artifacts: capturing the write set -> docs/generated-path-writes.tsv" >&2
    {
        echo "# path<TAB>written<TAB>tracked -- emitted by scripts/regenerate-artifacts.sh; do not hand-edit."
        echo "# written = how many of this row's git-tracked files carry an mtime newer than"
        echo "# the marker stamped at the top of this run. mtime ADVANCED, not content"
        echo "# CHANGED -- a file rewritten byte-for-byte identically still counts as written."
        echo "# census-AUTHORED ROWS GET NO LINE HERE (review finding I3): this script's own"
        echo "# HV_CENSUS conditional means a census-authored row's true written/tracked shape"
        echo "# differs by which invocation ran it, and this file has no way to say which one"
        echo "# did. See docs/generated-paths.txt's own comment beside this row for the account."
        echo "# ${write_capture_self_path} is declared but excluded from the rows below: this"
        echo "# file's own output redirect stamps its mtime before this loop ever runs, so a"
        echo "# self-observed count would be guaranteed true by construction, not measured."
        while IFS= read -r declared_path; do
            if [ "$declared_path" = "$write_capture_self_path" ]; then
                continue
            fi
            tracked=0
            written=0
            while IFS= read -r tracked_file; do
                [ -n "$tracked_file" ] || continue
                tracked=$((tracked + 1))
                if [ -n "$(find "$tracked_file" -newer "$write_capture_marker" -print 2>/dev/null)" ]; then
                    written=$((written + 1))
                fi
            done < <(git ls-files -- "$declared_path")
            printf '%s\t%s\t%s\n' "$declared_path" "$written" "$tracked"
        done < <(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | awk -F'\t' '$2 != "census" { print $1 }')
    } > docs/generated-path-writes.tsv
else
    echo "regenerate-artifacts: not a git checkout -- skipping the write-set capture" >&2
fi
