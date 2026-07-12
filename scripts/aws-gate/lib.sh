#!/usr/bin/env bash
# scripts/aws-gate/lib.sh — shared constants and helpers for the remote gate.
# Sourced by setup/teardown/gate-remote/panic; never executed directly.

HVG_REGION="${HVG_REGION:-us-east-1}"
HVG_INSTANCE_TYPE="${HVG_INSTANCE_TYPE:-c7g.4xlarge}"
HVG_PROJECT_TAG="${HVG_PROJECT_TAG:-project=hornvale-gate}"
HVG_PROFILE="${HVG_PROFILE:-hornvale-gate}"
HVG_MAX_COUNT="${HVG_MAX_COUNT:-10}"
HVG_MAX_AGE_SECS="${HVG_MAX_AGE_SECS:-7200}"   # 2h
HVG_IDLE_MIN="${HVG_IDLE_MIN:-15}"
HVG_BUDGET_ALERT="${HVG_BUDGET_ALERT:-10}"
HVG_BUDGET_HARD="${HVG_BUDGET_HARD:-25}"
# AWS budget ACTIONS support only monthly granularity (not daily), so the
# auto-disable backstop is a MONTHLY ceiling well above normal use (~$30-75/mo).
# The fast asleep-safety is the 5-min circuit breaker, not this.
HVG_BUDGET_MONTHLY_HARD="${HVG_BUDGET_MONTHLY_HARD:-150}"
HVG_MANIFEST="${HVG_MANIFEST:-$HOME/.hornvale-gate/manifest.json}"

# Admin context: the developer's default credentials (setup/teardown/panic).
aws_admin() { aws --region "$HVG_REGION" "$@"; }
# Runner context: the boxed-in launcher identity (gate-remote launch/terminate).
aws_runner() { aws --profile "$HVG_PROFILE" --region "$HVG_REGION" "$@"; }

worktree_name() { basename "$(git rev-parse --show-toplevel)"; }

manifest_get() { # KEY -> value ("" if absent); pure text, no jq dependency assumption
    python3 -c 'import json,sys;print(json.load(open(sys.argv[1])).get(sys.argv[2],""))' "$HVG_MANIFEST" "$1"
}
manifest_set() { # KEY VALUE
    mkdir -p "$(dirname "$HVG_MANIFEST")"
    [ -f "$HVG_MANIFEST" ] || echo '{}' > "$HVG_MANIFEST"
    python3 -c 'import json,sys;p=sys.argv[1];d=json.load(open(p));d[sys.argv[2]]=sys.argv[3];json.dump(d,open(p,"w"),indent=2,sort_keys=True)' "$HVG_MANIFEST" "$1" "$2"
}

tagged_arns() { # -> ARNs carrying the project tag, one per line
    local key="${HVG_PROJECT_TAG%%=*}" val="${HVG_PROJECT_TAG#*=}"
    aws_admin resourcegroupstaggingapi get-resources \
        --tag-filters "Key=$key,Values=$val" \
        --query 'ResourceTagMappingList[].ResourceARN' --output text | tr '\t' '\n'
}
