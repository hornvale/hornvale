#!/usr/bin/env bash
# Idempotent: create the runner user, attach the boxed policy, ensure an access
# key exists and is written to the local `hornvale-gate` profile. Admin creds.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"

setup_iam() {
  local user="hornvale-gate-runner"
  aws_admin iam get-user --user-name "$user" >/dev/null 2>&1 || aws_admin iam create-user --user-name "$user" --tags "Key=project,Value=hornvale-gate"
  aws_admin iam put-user-policy --user-name "$user" --policy-name hornvale-gate-runner --policy-document "file://$here/policy/runner-policy.json"
  # Create a key only if the profile has none configured yet.
  if ! aws --profile "$HVG_PROFILE" sts get-caller-identity >/dev/null 2>&1; then
    local out; out="$(aws_admin iam create-access-key --user-name "$user" --output json)"
    local id secret; id="$(echo "$out" | python3 -c 'import json,sys;print(json.load(sys.stdin)["AccessKey"]["AccessKeyId"])')"
    secret="$(echo "$out" | python3 -c 'import json,sys;print(json.load(sys.stdin)["AccessKey"]["SecretAccessKey"])')"
    aws configure set aws_access_key_id "$id" --profile "$HVG_PROFILE"
    aws configure set aws_secret_access_key "$secret" --profile "$HVG_PROFILE"
    aws configure set region "$HVG_REGION" --profile "$HVG_PROFILE"
    chmod 600 "$HOME/.aws/credentials" 2>/dev/null || true
    manifest_set runner_key_id "$id"
  fi
  manifest_set runner_user "$user"
}
