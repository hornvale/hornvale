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
# a cheap TEST (`windows/lab/tests/anomaly_injection.rs`) reads it. Nothing
# regenerates these fixtures automatically, and nothing should — see the
# fixture directory's README for why they are deliberately absent from
# `docs/generated-paths.txt`.
#
# USAGE
#   scripts/gnomon-injection.sh                  # the whole battery
#   scripts/gnomon-injection.sh baseline-a karst # only these arms (a pilot)
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
#   ssh lefford 'cd ~/Projects/hornvale && git fetch --all && \
#     git checkout <full-sha> && scripts/gnomon-injection.sh'
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

STUDY="studies/gnomon-injection.study.json"
FIXTURES="windows/lab/tests/fixtures/injection"
PUBLISHED="book/src/laboratory/generated/gnomon-injection"

# ---------------------------------------------------------------------------
# The battery. One record per arm: name|file|old|new|why
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
    "aquifer|domains/terrain/src/lithology.rs|const CLASTIC_AQUIFER_MIN_POROSITY: f64 = 0.46;|const CLASTIC_AQUIFER_MIN_POROSITY: f64 = 0.30;|the porosity above which clastic rock is an aquifer (lithology.rs:366); expected to move aquifer-fraction"
    "karst|domains/terrain/src/lithology.rs|const KARST_MIN_POROSITY: f64 = 0.4;|const KARST_MIN_POROSITY: f64 = 0.55;|the porosity above which carbonate rock karstifies (lithology.rs:360); expected to move karst-fraction and cave-fraction"
    "pantheon|domains/religion/src/lib.rs|const PANTHEON_FLOOR: f64 = 0.25;|const PANTHEON_FLOOR: f64 = 0.45;|the salience a phenomenon must reach to seat a deity (lib.rs:286); expected to move the pantheon-size family"
    "phonology|domains/language/src/phonology.rs|const LOUDNESS_PENALTY: f64 = 0.22;|const LOUDNESS_PENALTY: f64 = 0.60;|how strongly low voice-loudness down-weights sonorous consonants in the inventory draw (phonology.rs:450); expected to move the naming and homophony families"
)

arm_field() { printf '%s' "$1" | cut -d'|' -f"$2"; }

# ---------------------------------------------------------------------------
# Guards
# ---------------------------------------------------------------------------
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

# The tree must be clean before anything is rewritten: this script edits
# TRACKED source in place, and a restore into a tree that already carried
# uncommitted edits to those files would silently discard them.
#
# The fixture directory is EXCLUDED from that check, and the exclusion is
# what makes this a guard rather than a one-shot: it is this script's own
# output, wiped and rebuilt from scratch on every invocation, so after the
# very first run an unconditional check refuses forever — the second run
# reports the first run's evidence as the dirt it must not destroy.
if [ -n "$(git status --porcelain -- ":!$FIXTURES")" ]; then
    echo "gnomon-injection: REFUSING to run with a dirty tree — this script rewrites" >&2
    echo "tracked source in place and restores it with 'git checkout --'; uncommitted" >&2
    echo "work in those files would be destroyed. Commit or stash first:" >&2
    git status --short -- ":!$FIXTURES" >&2
    exit 1
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
echo "  cargo test -p hornvale-lab --test anomaly_injection -- --nocapture" >&2
