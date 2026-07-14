#!/usr/bin/env python3
"""Wide->long conversion for historical rows.csv snapshots (spec §3).

stdin: one wide rows.csv (any column set — history absorbs schema drift).
argv: study, tier, epoch, commit, commit_date, epoch_label, current-schema.json path.
stdout: long CSV rows (no header) matching census_history's column order.
Typing falls back: current schema for columns that still exist, VARCHAR
(value_text) for retired ones.
"""
import csv, json, sys

study, tier, epoch, commit, cdate, label, schema_path = sys.argv[1:8]
kinds = {}
try:
    for c in json.load(open(schema_path))["columns"]:
        kinds[c["name"]] = c["kind"]
except FileNotFoundError:
    pass

reader = csv.DictReader(sys.stdin)
w = csv.writer(sys.stdout)
for row in reader:
    for col, val in row.items():
        if col in ("seed", "pin_set", "refusal") or val is None:
            continue
        kind = kinds.get(col, "categorical")
        num = val if (kind == "numeric" and val != "") else ""
        flg = val if (kind == "flag" and val != "") else ""
        txt = val if (kind == "categorical" and val != "") else ""
        w.writerow([study, tier, epoch, row["seed"], row["pin_set"], col, kind,
                    num, txt, flg, commit, cdate, label])
