#!/usr/bin/env bash
# tools/census/history.sh <study> — load every committed snapshot of a
# study's rows.csv into census_history, tagged with commit/date/epoch label
# (the --first-parent subject; spec §3). Usage: bash tools/census/history.sh the-census
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
study="${1:?usage: history.sh <study>}"

read -r dir tier epoch < <(python3 -c "
import json,sys
m=json.load(open('tools/census/manifest.json'))
for t in ('live', 'frozen'):
    for e in m[t]:
        if e['study']==sys.argv[1]:
            epoch_val = e.get('epoch', t)
            print(f\"{e['dir']} {t} {epoch_val}\")
            sys.exit(0)
sys.exit('unknown study: '+sys.argv[1])" "$study")
path="$dir/rows.csv"
schema="$dir/schema.json"

# Build (and mount-validate) first — a failed mount ABORTS history extraction
# (fail-loudly doctrine, spec §3); pre-4b, verify with CENSUS_MANIFEST override.
bash tools/census/build.sh
hist="tools/census/.build/history-$study.csv"
: > "$hist"

# Single-pass walk: one git-log call emits a commit marker line followed by
# the pathname --follow attributes to that commit (the path may have lived
# at an older location — the-census was census-lands-drift before
# consolidation — --follow tracks the rename), instead of a second git-log
# invocation per commit. Each commit/pathname pair is then extracted with
# `git show <commit>:<pathname>`.
snapshots=0
commit=""
cdate=""
subject=""
file=""
flush() {
    [ -n "$commit" ] || return 0
    [ -n "$file" ] || return 0
    if git show "$commit:$file" 2>/dev/null |
        python3 tools/census/history_load.py "$study" "$tier" "$epoch" "$commit" "$cdate" "$subject" "$schema" >>"$hist"; then
        snapshots=$((snapshots + 1))
    fi
}
while IFS= read -r line; do
    case "$line" in
    C$'\t'*)
        flush
        IFS=$'\t' read -r _ commit cdate subject <<<"$line"
        file=""
        ;;
    "") ;;
    *)
        file="$line"
        ;;
    esac
done < <(git log --follow --first-parent --name-only --format='C%x09%H%x09%cI%x09%s' main -- "$path")
flush

if [ "$snapshots" -eq 0 ]; then
    echo "census: no committed history for $study on main (yet)" >&2
fi

insert=""
if [ -s "$hist" ]; then
    insert="INSERT INTO census_history SELECT * FROM read_csv('$hist', header=false, nullstr='', columns={
  'study':'VARCHAR','tier':'VARCHAR','epoch':'VARCHAR','seed':'BIGINT','pin_set':'VARCHAR','metric':'VARCHAR',
  'kind':'VARCHAR','value_num':'DOUBLE','value_text':'VARCHAR','value_flag':'BOOLEAN',
  'commit':'VARCHAR','commit_date':'TIMESTAMP','epoch_label':'VARCHAR'});"
fi

duckdb tools/census/.build/census.duckdb -c "
CREATE OR REPLACE TABLE census_history (
  study VARCHAR, tier VARCHAR, epoch VARCHAR, seed BIGINT, pin_set VARCHAR, metric VARCHAR,
  kind VARCHAR, value_num DOUBLE, value_text VARCHAR, value_flag BOOLEAN,
  commit VARCHAR, commit_date TIMESTAMP, epoch_label VARCHAR);
$insert
SELECT count(*) AS history_rows, count(DISTINCT commit) AS snapshots FROM census_history;"
echo "census: census_history loaded for $study ($snapshots snapshot(s))" >&2
