#!/usr/bin/env bash
# scripts/aws-gate/setup.sh — provision the remote-gate infrastructure.
# Idempotent (create-if-absent, keyed by name/tag). BILLABLE on first real run.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=/dev/null
source "$here/lib.sh"
# shellcheck source=/dev/null
source "$here/setup-iam.sh"
DRY_RUN=0; [ "${1:-}" = "--dry-run" ] && DRY_RUN=1
run() { if [ "$DRY_RUN" = 1 ]; then echo "DRY: $*"; else "$@"; fi; }

section() { printf '\n== %s\n' "$1"; }

section "IAM runner identity"
if [ "$DRY_RUN" = 1 ]; then echo "DRY: setup_iam"; else setup_iam; fi

section "sccache S3 bucket"
bucket="hornvale-gate-sccache-$(aws_admin sts get-caller-identity --query Account --output text)"
aws_admin s3api head-bucket --bucket "$bucket" 2>/dev/null || run aws_admin s3api create-bucket --bucket "$bucket" --region "$HVG_REGION"
run aws_admin s3api put-bucket-tagging --bucket "$bucket" --tagging 'TagSet=[{Key=project,Value=hornvale-gate}]'
manifest_set sccache_bucket "$bucket"

section "AMI (Ubuntu 24.04 ARM64 via SSM public parameter)"
ami="$(aws_admin ssm get-parameter --name /aws/service/canonical/ubuntu/server/24.04/stable/current/arm64/hvm/ebs-gp3/ami-id --query Parameter.Value --output text)"
manifest_set ami "$ami"

section "Security group (SSH from this IP only)"
myip="$(curl -s https://checkip.amazonaws.com)"
echo "  admin IP: ${myip}/32"
# ... create SG if absent, authorize tcp/22 from ${myip}/32, tag project ...

section "Keypair"
# ... create keypair if absent, write private key to ~/.hornvale-gate/id (600), tag ...

section "Box instance profile (hornvale-gate-runner-box)"
# The BOX's own role — distinct from the runner user and the Lambda role. The
# runner's scoped iam:PassRole (Task 2) targets exactly this role name, so it
# MUST exist or every launch is denied. Create if absent:
#   - role `hornvale-gate-runner-box`, trust policy = ec2.amazonaws.com;
#   - inline policy: ec2:TerminateInstances on instances tagged project=hornvale-gate
#     (the 15-min idle self-terminate), + s3 Get/Put/List on the sccache bucket
#     (RUSTC_WRAPPER=sccache);
#   - instance profile `hornvale-gate-runner-box`, role added; tag project.

section "Launch template"
# ... register/refresh launch template: spot, $HVG_INSTANCE_TYPE, $ami, SG, keypair,
#     IamInstanceProfile=hornvale-gate-runner-box, base64(userdata.sh with bucket templated),
#     TagSpecifications project=hornvale-gate ...

echo "setup complete (dry-run=$DRY_RUN)"
