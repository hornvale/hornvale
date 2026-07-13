#!/usr/bin/env python3
"""Validate committed census artifacts and emit DuckDB DDL (census-as-data spec §3).

Reads the manifest given as argv[1]; for every study entry validates the
schema.json <-> rows.csv pair (row count, FNV-1a64 content hash, header
order), then prints CREATE VIEW DDL: one typed wide view per study plus the
unified long view `census_long` and the `timings` sidecar. Any validation
failure aborts with the file and the failed check — a dataset that cannot
mount must never silently vanish from query results.
"""
import csv, io, json, os, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
BUILD = os.path.join(ROOT, "tools", "census", ".build")
SQL_TYPES = {"integer": "BIGINT", "numeric": "DOUBLE", "flag": "BOOLEAN", "categorical": "VARCHAR"}

def die(msg):
    sys.stderr.write(f"census build: {msg}\n")
    sys.exit(1)

def fnv1a64(data: bytes) -> int:
    h = 0xCBF29CE484222325
    for b in data:
        h = ((h ^ b) * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return h

def validate(entry):
    d = os.path.join(ROOT, entry["dir"])
    schema_path, rows_path = os.path.join(d, "schema.json"), os.path.join(d, "rows.csv")
    for p in (schema_path, rows_path):
        if not os.path.exists(p):
            die(f"{entry['study']}: missing {p}")
    schema = json.load(open(schema_path))
    raw = open(rows_path, "rb").read()
    got = f"0x{fnv1a64(raw):016x}"
    if got != schema["rows"]["fnv1a64"]:
        die(f"{entry['study']}: rows.csv hash {got} != manifest {schema['rows']['fnv1a64']} (stale pair)")
    records = list(csv.reader(io.StringIO(raw.decode("utf-8"))))
    if len(records) - 1 != schema["rows"]["count"]:
        die(f"{entry['study']}: {len(records)-1} data rows != manifest count {schema['rows']['count']}")
    names = [c["name"] for c in schema["columns"]]
    if names != records[0]:
        die(f"{entry['study']}: schema columns disagree with the CSV header")
    return schema, rows_path

def wide_view(entry, schema, rows_path):
    cols = ", ".join(
        f"'{c['name']}': '{SQL_TYPES[c['kind']]}'" for c in schema["columns"]
    )
    return (
        f'CREATE OR REPLACE VIEW "{entry["study"]}" AS '
        f"SELECT * FROM read_csv('{rows_path}', header=true, nullstr='', columns={{{cols}}});"
    )

def long_selects(entry, schema, tier):
    epoch = entry.get("epoch", "live")
    for c in schema["columns"]:
        if c["name"] in ("seed", "pin_set", "refusal"):
            continue
        k = c["kind"]
        vals = {
            "numeric": (f'"{c["name"]}"', "NULL", "NULL"),
            "flag": ("NULL", "NULL", f'"{c["name"]}"'),
            "categorical": ("NULL", f'"{c["name"]}"', "NULL"),
        }[k]
        yield (
            f"SELECT '{entry['study']}' AS study, '{tier}' AS tier, '{epoch}' AS epoch, "
            f"seed, pin_set, '{c['name']}' AS metric, '{k}' AS kind, "
            f"CAST({vals[0]} AS DOUBLE) AS value_num, CAST({vals[1]} AS VARCHAR) AS value_text, "
            f'CAST({vals[2]} AS BOOLEAN) AS value_flag FROM "{entry["study"]}"'
        )

def timings_csv(path):
    """docs/timings.md markdown table -> .build/timings.csv."""
    out = os.path.join(BUILD, "timings.csv")
    with open(path) as f, open(out, "w", newline="") as o:
        w = csv.writer(o)
        rows = [l.strip() for l in f if l.strip().startswith("|")]
        for i, line in enumerate(rows):
            cells = [c.strip() for c in line.strip("|").split("|")]
            if i == 1 and set("".join(cells)) <= set("-: "):
                continue
            w.writerow(cells)
    return out

def main():
    if len(sys.argv) != 2:
        die("usage: build_ddl.py <manifest.json>")
    manifest_path = sys.argv[1]
    os.makedirs(BUILD, exist_ok=True)
    manifest = json.load(open(manifest_path))
    parts = []
    for tier in ("live", "frozen"):
        for entry in manifest.get(tier, []):
            schema, rows_path = validate(entry)
            print(wide_view(entry, schema, rows_path))
            parts.extend(long_selects(entry, schema, tier))
    print("CREATE OR REPLACE VIEW census_long AS\n" + "\nUNION ALL\n".join(parts) + ";")
    for side in manifest.get("sidecars", []):
        if side["format"] == "markdown-table":
            p = timings_csv(os.path.join(ROOT, side["path"]))
            print(
                f'CREATE OR REPLACE VIEW "{side["table"]}" AS '
                f"SELECT * FROM read_csv('{p}', header=true);"
            )

if __name__ == "__main__":
    main()
