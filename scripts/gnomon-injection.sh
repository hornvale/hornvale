#!/usr/bin/env bash
# scripts/gnomon-injection.sh — the authoring path for The Gnomon's H1
# injection battery (spec §3.5).
#
# WHY A SCRIPT AND NOT A TEST. A compiled test binary cannot change the
# constants it was compiled from: source substitution only takes effect after
# a rebuild, and no `#[test]` can rebuild itself. The repo's only other
# source-mutating machinery, `tools/seam-guard`, is a standalone binary
# outside the cargo workspace for exactly this reason, and this script follows
# its discipline verbatim (`tools/seam-guard/src/lib.rs`: `tree_is_clean` at
# :194, `probe` at :224 — refuse on a dirty tree, assert the literal present,
# substitute, run, restore under a trap).
#
# So the seam is the one `scripts/census-run.sh` already uses: an expensive,
# host-pinned AUTHORING script produces evidence; the evidence is COMMITTED;
# a cheap TEST (`windows/lab/tests/suite/anomaly_injection.rs`) reads it.
#
# THIS USED TO SAY "nothing regenerates these fixtures automatically, and
# nothing should." Since The Spillway (decision 0836) that is false: the
# queued census delivery (`scripts/sluice-census.sh`) IS the fixtures'
# ordinary automatic author, running this script at the censused ref under
# the box lock. What remains true, and is why the fixtures stay absent from
# `docs/generated-paths.txt`, is narrower — the ARTIFACT SWEEP (`make
# rebaseline` / `regenerate-artifacts.sh`) must never author them, because
# that would mutate tracked source on its way past.
#
# USAGE
#   scripts/gnomon-injection.sh                  # the whole battery
#   scripts/gnomon-injection.sh baseline-a karst # only these arms (a pilot)
#   scripts/gnomon-injection.sh check            # run the guards, build nothing, exit 0/1
#   HV_GNOMON_PILOT=1 scripts/gnomon-injection.sh ...   # allow an off-host run
#
# The fixture directory is REBUILT from scratch on every invocation, so the
# committed manifest always describes exactly the arms on disk — a partial
# run leaves a small, self-consistent battery rather than a mixture of hosts
# and SHAs.
#
# WHERE IT RUNS. On the canonical box (decisions 0063/0079), because the
# fixtures are scored against census goldens authored there and the two
# machines disagree by one unit on ~0.1% of discrete-count metrics, decided in
# the compute path upstream of quantize-at-emit. `HV_GNOMON_PILOT=1` lifts the
# refusal for machinery validation and stamps the authoring host into the
# manifest, which is what `anomaly_injection.rs` reads to decide whether the
# battery is the preregistered one or a pilot.
#
# BY HAND, THE EXCEPTION NOW. Since The Spillway the ordinary caller is the
# queued census delivery (above); running this by hand is for a pilot or a
# recovery, not the everyday path:
#
#   ssh lefford 'cd ~/Projects/hornvale && git fetch --all && \
#     git checkout <full-sha> && scripts/gnomon-injection.sh'
#
# WHAT THE TREE GUARD PROTECTS, EXACTLY (The Spillway). This script mutates
# tracked SOURCE and restores it with `git checkout --`, and stamps the
# manifest with `sha=$(git rev-parse HEAD)` as a claim about what was BUILT.
# Both are claims about source. So the guard refuses dirt anywhere a build or
# the mutation can see, and allows it under `book/` (the census's own output
# and the project book) and `docs/` (prose and ledgers), which the `hornvale`
# binary neither compiles nor reads on a `lab run`. It used to refuse the
# whole tree, and that is what deadlocked a census delivery: the staged
# goldens ARE dirt under book/, and the arms this script authors are what the
# delivery's own gate compares against them (The Warp, ledger #12; spec §1).
# `check` runs both guards and stops, so the delivery can ask before it waits
# for the box lock, and so this guard has a test that needs no build.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

STUDY="studies/gnomon-injection.study.json"
FIXTURES="windows/lab/tests/fixtures/injection"
PUBLISHED="book/src/laboratory/generated/gnomon-injection"

# ---------------------------------------------------------------------------
# The battery. One record per arm: name|file|old|new|why
#
# COUNT THE SEPARATORS. Each record needs FOUR `|`, and a baseline's three
# empty fields therefore read as `name||||why` — four pipes with nothing
# between them. Two of these records were first written with THREE, so `why`
# landed in field 4 and each baseline's prose was recorded as the manifest's
# `new` value. It was found by reading the manifest the first full run
# produced, not by any test, and nothing would ever have caught it:
# `windows/lab/tests/suite/anomaly_injection.rs` reads `arms[].kind`, `[].name` and
# `[].host` and NEVER reads `file`, `old`, `new` or `why`. The general form is
# worth carrying past this script — **a manifest field no assertion reads is
# unguarded data**, and it will be wrong silently and stay wrong, because the
# only thing that ever looks at it is a human who happened to open the file.
# The provenance fields here are exactly that class: they exist to let a
# future reader reconstruct what was perturbed, which is precisely the moment
# nobody is left who could notice they are garbage.
#
# A baseline arm carries empty file/old/new: it is the unperturbed run, and
# TWO of them are authored (`baseline-a`, `baseline-b`) as SEPARATE
# invocations because H1's false-positive arm is about whether two independent
# world-BUILD runs agree, not whether two calls to `rank` on one in-memory
# census agree (they cannot disagree — that guard could never fail).
#
# The injections were chosen by reading the domains, not prescribed: each is a
# single-site change to one generative constant that still compiles, is
# expected to move a NAMED handful of census columns, and is not expected to
# move the whole census (an injection that moves everything tests nothing
# about ranking). They deliberately span four independent stretches of the
# pipeline — two terrain sub-layers, religion, and language — so a recall
# figure is not five readings of one mechanism.
# ---------------------------------------------------------------------------
ARMS=(
    "baseline-a||||the unperturbed run: the comparison arm every injection is diffed against"
    "baseline-b||||a SECOND independent unperturbed run: H1's false-positive arm compares its top-10s against baseline-a's"
    "geothermal|domains/terrain/src/strata.rs|const CRATONIC_GRADIENT_K_PER_KM: f64 = 15.0;|const CRATONIC_GRADIENT_K_PER_KM: f64 = 22.5;|the cratonic floor of the geothermal gradient clamp (strata.rs:28,54); expected to move mean-geothermal-gradient and little else"
    "unconformity|domains/terrain/src/strata.rs|const UNCONFORMITY_COVER_M: f64 = 200.0;|const UNCONFORMITY_COVER_M: f64 = 400.0;|the cover depth below which an old surface counts as an unconformity (strata.rs:146); expected to move unconformity-fraction"
    "aquifer|domains/terrain/src/lithology.rs|const CLASTIC_AQUIFER_MIN_POROSITY: f64 = 0.53;|const CLASTIC_AQUIFER_MIN_POROSITY: f64 = 0.30;|the porosity above which clastic rock is an aquifer (lithology.rs); expected to move aquifer-fraction. BASELINE RE-PLACED 0.46 -> 0.53 by The Trencher Task 13, which VOIDED this arm (the substitution matched 0 times and refused the census delivery at 8f506c1d9997). Only the OLD side is updated: 0.30 was chosen by reading the domain and keeps its meaning, so the perturbation is now -0.23 rather than the authored -0.16. If that proves too wide to stay localised, the fix is to re-choose NEW against the domain, not to shrink it to restore the old delta."
    "karst|domains/terrain/src/lithology.rs|const KARST_MIN_POROSITY: f64 = 0.4;|const KARST_MIN_POROSITY: f64 = 0.55;|the porosity above which carbonate rock karstifies (lithology.rs:360); expected to move karst-fraction and cave-fraction"
    "pantheon|domains/religion/src/lib.rs|const PANTHEON_FLOOR: f64 = 0.25;|const PANTHEON_FLOOR: f64 = 0.45;|the salience a phenomenon must reach to seat a deity (lib.rs:286); expected to move the pantheon-size family"
    "phonology|domains/language/src/phonology.rs|const LOUDNESS_PENALTY: f64 = 0.22;|const LOUDNESS_PENALTY: f64 = 0.60;|how strongly low voice-loudness down-weights sonorous consonants in the inventory draw (phonology.rs:450); expected to move the naming and homophony families"
)

arm_field() { printf '%s' "$1" | cut -d'|' -f"$2"; }

# ---------------------------------------------------------------------------
# Guards
# ---------------------------------------------------------------------------
mode="run"
if [ "${1:-}" = "check" ]; then
    mode="check"
    shift
    if [ "$#" -ne 0 ]; then
        echo "gnomon-injection: usage: gnomon-injection.sh check   (takes no arms)" >&2
        exit 2
    fi
fi

# shellcheck source=scripts/census-canonical-host.sh
. "$(dirname "$0")/census-canonical-host.sh"
here="$(hostname -s 2>/dev/null || hostname)"
if [ "$(printf '%s' "$here" | tr '[:upper:]' '[:lower:]')" \
     != "$(printf '%s' "$CANONICAL_CENSUS_HOST" | tr '[:upper:]' '[:lower:]')" ]; then
    if [ "${HV_GNOMON_PILOT:-}" != "1" ]; then
        cat >&2 <<EOF
gnomon-injection: REFUSING to author the battery on '$here'.

These fixtures are scored against census goldens authored on
'$CANONICAL_CENSUS_HOST' (decisions 0063/0079), and the boxes are not
byte-identical: ~0.1% of discrete-count metrics differ by one unit, decided
upstream of quantize-at-emit. Author them there:

  ssh $CANONICAL_CENSUS_HOST 'cd ~/Projects/hornvale && git fetch --all && \\
    git checkout <full-sha> && scripts/gnomon-injection.sh'

To validate the MACHINERY off-host, set HV_GNOMON_PILOT=1. The authoring host
is recorded per arm in the manifest, and anomaly_injection.rs reads it: a
battery with any off-host arm is a pilot and does not adjudicate H1.
EOF
        exit 1
    fi
    echo "gnomon-injection: PILOT run on '$here' (not '$CANONICAL_CENSUS_HOST') — these fixtures do not adjudicate H1." >&2
fi

# The tree must be clean everywhere a build or the mutation can see: this
# script edits TRACKED source in place, and a restore into a tree that
# already carried uncommitted edits to those files would silently discard
# them; and the manifest's `sha` is a claim about the source that was built.
#
# FOUR trees are excluded, and each exclusion is what makes this a guard
# rather than a one-shot or a deadlock:
#   - the fixture directory: this script's own output, wiped and rebuilt on
#     every invocation, so an unconditional check refuses forever after the
#     first run;
#   - book/: the census's own output and the project book. A census delivery
#     runs this script with its goldens STAGED there (The Spillway); the
#     `hornvale` binary neither compiles nor reads book/ on a `lab run`;
#   - docs/: prose, timings, audits. Same argument;
#   - clients/: outside the cargo workspace (root Cargo.toml excludes it) and
#     no `lab run` reads it — but the census's own artifact sweep regenerates
#     `clients/game/core/tests/fixtures/` (declared `artifacts` in
#     `docs/generated-paths.txt`), so a delivery's dirt can include it too.
# Anything else dirty — kernel/, domains/, windows/, cli/, studies/, scripts/,
# Cargo.* — refuses, and is named.
dirty="$(git status --porcelain -- . ":!$FIXTURES" ":!book" ":!docs" ":!clients")"
if [ -n "$dirty" ]; then
    echo "gnomon-injection: REFUSING to run with a dirty tree — this script rewrites" >&2
    echo "tracked source in place and restores it with 'git checkout --'; uncommitted" >&2
    echo "work in those files would be destroyed. Commit or stash first:" >&2
    printf '%s\n' "$dirty" >&2
    exit 1
fi

if [ "$mode" = "check" ]; then
    echo "gnomon-injection: check OK — host '$here', tree clean outside $FIXTURES, book/, docs/ and clients/" >&2
    exit 0
fi

# Restore whatever is currently substituted, on ANY exit including an
# interrupt. Set before the first substitution, cleared after each restore.
mutated=""
restore() {
    if [ -n "$mutated" ]; then
        echo "gnomon-injection: restoring $mutated" >&2
        git checkout -- "$mutated"
        mutated=""
    fi
}
trap restore EXIT

requested=("$@")
if [ ${#requested[@]} -eq 0 ]; then
    for record in "${ARMS[@]}"; do requested+=("$(arm_field "$record" 1)"); done
fi

sha="$(git rev-parse HEAD)"
# Clear the arms and the manifest, never the whole directory: the README is
# committed prose that lives here and explains why these fixtures are absent
# from docs/generated-paths.txt. A blanket `rm -rf "$FIXTURES"` deletes it,
# and the next run commits a fixture set with no explanation attached.
mkdir -p "$FIXTURES"
find "${FIXTURES:?}" -mindepth 1 -maxdepth 1 -type d -exec rm -rf {} +
rm -f "$FIXTURES/manifest.json"

manifest_arms=""
seed_from=""
seed_to=""

for name in "${requested[@]}"; do
    record=""
    for candidate in "${ARMS[@]}"; do
        if [ "$(arm_field "$candidate" 1)" = "$name" ]; then record="$candidate"; fi
    done
    if [ -z "$record" ]; then
        echo "gnomon-injection: unknown arm '$name'. Known arms:" >&2
        for candidate in "${ARMS[@]}"; do echo "  $(arm_field "$candidate" 1)" >&2; done
        exit 1
    fi

    file="$(arm_field "$record" 2)"
    old="$(arm_field "$record" 3)"
    new="$(arm_field "$record" 4)"
    why="$(arm_field "$record" 5)"

    if [ -n "$file" ]; then
        # Assert the target text is present BEFORE substituting, and that it
        # is present exactly once. A `cargo fmt` rewrap has previously made a
        # single-line replacement match nothing and produced a green that
        # looked like a robust implementation; a literal that matched TWICE
        # would silently make the injection multi-site.
        hits="$(grep -cF -- "$old" "$file" || true)"
        if [ "$hits" != "1" ]; then
            echo "gnomon-injection: TARGET NOT FOUND (or not unique) in $file: $old" >&2
            echo "gnomon-injection: matched $hits times; expected exactly 1." >&2
            exit 1
        fi
        echo "gnomon-injection: [$name] $file" >&2
        echo "gnomon-injection:   $old  ->  $new" >&2
        mutated="$file"
        # Literal substitution: \Q..\E quotes every metacharacter in the
        # pattern, and the replacement is a plain interpolation of the env
        # var's contents (never re-parsed), so neither side needs escaping.
        HV_OLD="$old" HV_NEW="$new" perl -0777 -pi -e 's/\Q$ENV{HV_OLD}\E/$ENV{HV_NEW}/g' "$file"
    else
        echo "gnomon-injection: [$name] unperturbed baseline" >&2
    fi

    rm -rf "${PUBLISHED:?}"
    cargo run -q --release -p hornvale -- lab run "$STUDY"
    restore

    mkdir -p "$FIXTURES/$name"
    cp "$PUBLISHED/rows.csv" "$FIXTURES/$name/rows.csv"
    cp "$PUBLISHED/schema.json" "$FIXTURES/$name/schema.json"
    rm -rf "${PUBLISHED:?}"

    # The seed range is read back OUT of the evidence rather than restated
    # from the study: the rows themselves are what the test scores.
    if [ -z "$seed_from" ]; then
        seed_from="$(awk -F, 'NR==2 && NF {print $1}' "$FIXTURES/$name/rows.csv")"
        seed_to="$(awk -F, 'NR>1 && NF {s=$1} END {print s}' "$FIXTURES/$name/rows.csv")"
    fi

    kind="injection"
    [ -z "$file" ] && kind="baseline"
    [ -n "$manifest_arms" ] && manifest_arms="$manifest_arms,"
    manifest_arms="$manifest_arms
    { \"name\": \"$name\", \"kind\": \"$kind\", \"file\": \"$file\", \"old\": \"$old\", \"new\": \"$new\", \"why\": \"$why\", \"host\": \"$here\", \"sha\": \"$sha\" }"
done

cat > "$FIXTURES/manifest.json" <<EOF
{ "schema": "gnomon-injection/v1",
  "study": "$STUDY",
  "census": "book/src/laboratory/generated/the-census",
  "seed_from": $seed_from,
  "seed_to": $seed_to,
  "arms": [$manifest_arms
  ]
}
EOF

echo "gnomon-injection: wrote $FIXTURES/manifest.json (${#requested[@]} arms, sha $sha, host $here)" >&2
echo "gnomon-injection: read the result with" >&2
echo "  cargo test -p hornvale-lab --test suite -- anomaly_injection --nocapture" >&2
