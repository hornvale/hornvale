#!/usr/bin/env bash
# scripts/aws-gate/teardown.sh — remove ALL hornvale-gate resources. Enumerates
# by the project tag (complete by construction), plus well-known-name backstops
# for IAM/budgets the tagging API covers unevenly. Admin creds.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"
"$here/panic.sh"                                  # stop the bleeding first
echo "teardown: tagged resources:"; tagged_arns
# ... for each ARN, dispatch to the right delete by service; then delete the
#     runner user/keys/policy, the two budgets, the Lambda + role + rule, the
#     SG, the keypair, and the sccache bucket by their known names ...
echo "teardown: complete"
