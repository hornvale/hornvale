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
  # Re-enable path: reactivate any key a prior panic or budget action disabled,
  # and detach the hard-budget deny policy, so the existing profile works again
  # instead of minting a duplicate key.
  # shellcheck disable=SC2016  # backticks are JMESPath literal syntax, not shell
  for k in $(aws_admin iam list-access-keys --user-name "$user" --query 'AccessKeyMetadata[?Status==`Inactive`].AccessKeyId' --output text 2>/dev/null || true); do
    aws_admin iam update-access-key --user-name "$user" --access-key-id "$k" --status Active || true
  done
  aws_admin iam detach-user-policy --user-name "$user" \
    --policy-arn "arn:aws:iam::$(aws_admin sts get-caller-identity --query Account --output text):policy/hornvale-gate-deny" >/dev/null 2>&1 || true
  # Create a key ONLY if the user has none at all. Checking a live `sts` call
  # here would race the key-reactivation propagation above and mint a duplicate;
  # counting keys does not. (If a key exists but the local profile secret is
  # missing, delete the key and re-run — setup won't silently mint a second.)
  if [ "$(aws_admin iam list-access-keys --user-name "$user" --query 'length(AccessKeyMetadata)' --output text)" = "0" ]; then
    local out; out="$(aws_admin iam create-access-key --user-name "$user" --output json)"
    local id secret
    read -r id secret <<<"$(echo "$out" | python3 -c 'import json,sys;k=json.load(sys.stdin)["AccessKey"];print(k["AccessKeyId"], k["SecretAccessKey"])')"
    aws configure set aws_access_key_id "$id" --profile "$HVG_PROFILE"
    aws configure set aws_secret_access_key "$secret" --profile "$HVG_PROFILE"
    aws configure set region "$HVG_REGION" --profile "$HVG_PROFILE"
    chmod 600 "$HOME/.aws/credentials" 2>/dev/null || true
    manifest_set runner_key_id "$id"
  fi
  manifest_set runner_user "$user"
}
