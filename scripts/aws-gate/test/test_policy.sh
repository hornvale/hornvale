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
# RunInstances is split across two statements (instance-scoped + supporting
# resources). The instance-scoped one carries the type/market/tag gate.
def _res(s):
    r = s.get("Resource"); return r if isinstance(r, list) else [r]
inst = [s for s in run if any("instance/" in r for r in _res(s))]
assert inst, "a RunInstances statement must be scoped to the instance resource"
run_cond = inst[0].get("Condition", {})
cond=json.dumps(run_cond)
assert "c7g.4xlarge" in cond, "RunInstances must be conditioned on the instance type"
assert run_cond.get("StringEquals",{}).get("ec2:InstanceMarketType") == "spot", \
    "RunInstances must require ec2:InstanceMarketType == spot (by key, not substring)"
assert "us-east-1" in txt, "must pin the region"
assert "project" in txt and "hornvale-gate" in txt, "must require the project tag"
assert '"iam:*"' not in txt and '"*"' not in [s.get("Action") for s in d["Statement"]], "no wildcard admin"

pass_role=[s for s in d["Statement"] if "iam:PassRole" in (s.get("Action") if isinstance(s.get("Action"),list) else [s.get("Action")])]
assert pass_role, "must grant iam:PassRole"
pr_resource = pass_role[0].get("Resource")
pr_resources = pr_resource if isinstance(pr_resource, list) else [pr_resource]
assert "*" not in pr_resources, "iam:PassRole must not be scoped to Resource: *"
assert any("role/hornvale-gate-runner-box" in r for r in pr_resources), \
    "iam:PassRole must be scoped to the box role"

# ec2:CreateTags is allowed ONLY at launch (ec2:CreateAction == RunInstances):
# the runner must be able to tag its box via RunInstances --tag-specifications,
# but must NOT be able to retag an existing box to escape the tag-based kill
# switches (breaker/panic/teardown/idle). So: any statement granting CreateTags
# MUST carry the ec2:CreateAction==RunInstances condition — no unconditional grant.
for s in d["Statement"]:
    acts = s.get("Action") if isinstance(s.get("Action"), list) else [s.get("Action")]
    if "ec2:CreateTags" in acts:
        ca = s.get("Condition", {}).get("StringEquals", {}).get("ec2:CreateAction")
        assert ca == "RunInstances", \
            "ec2:CreateTags must be scoped to ec2:CreateAction==RunInstances (launch-only, no retag evasion)"
print("policy OK")
PY
