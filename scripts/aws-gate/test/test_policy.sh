#!/usr/bin/env bash
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
p="$here/../policy/runner-policy.json"
python3 - "$p" <<'PY'
import json,sys
d=json.load(open(sys.argv[1]))
txt=json.dumps(d)
run=[s for s in d["Statement"] if "ec2:RunInstances" in (s.get("Action") if isinstance(s.get("Action"),list) else [s.get("Action")])]
assert run, "must grant RunInstances"
cond=json.dumps(run[0].get("Condition",{}))
assert "c7g.4xlarge" in cond, "RunInstances must be conditioned on the instance type"
assert "spot" in cond.lower(), "RunInstances must require spot"
assert "us-east-1" in txt, "must pin the region"
assert "project" in txt and "hornvale-gate" in txt, "must require the project tag"
assert '"iam:*"' not in txt and '"*"' not in [s.get("Action") for s in d["Statement"]], "no wildcard admin"
print("policy OK")
PY
