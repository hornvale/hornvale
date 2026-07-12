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
sg_id="$(aws_admin ec2 describe-security-groups --filters "Name=group-name,Values=hornvale-gate-ssh" --query 'SecurityGroups[0].GroupId' --output text 2>/dev/null || true)"
if [ -z "$sg_id" ] || [ "$sg_id" = "None" ]; then
    sg_id="$(run aws_admin ec2 create-security-group --group-name hornvale-gate-ssh \
        --description "hornvale remote gate SSH" \
        --tag-specifications 'ResourceType=security-group,Tags=[{Key=project,Value=hornvale-gate}]' \
        --query GroupId --output text)"
fi
echo "  security group: $sg_id"
# Authorize SSH from the current admin IP; ignore if the exact rule already exists.
aws_admin ec2 authorize-security-group-ingress --group-id "$sg_id" \
    --protocol tcp --port 22 --cidr "${myip}/32" >/dev/null 2>&1 || true
manifest_set security_group "$sg_id"

section "Keypair"
keyfile="$HOME/.hornvale-gate/id"
if ! aws_admin ec2 describe-key-pairs --key-names hornvale-gate >/dev/null 2>&1; then
    mkdir -p "$(dirname "$keyfile")"
    run aws_admin ec2 create-key-pair --key-name hornvale-gate \
        --tag-specifications 'ResourceType=key-pair,Tags=[{Key=project,Value=hornvale-gate}]' \
        --query KeyMaterial --output text > "$keyfile"
    chmod 600 "$keyfile"
    echo "  private key written to $keyfile (600)"
else
    echo "  keypair hornvale-gate already exists"
fi
manifest_set keypair hornvale-gate

section "Box instance profile (hornvale-gate-runner-box)"
# The BOX's own role (distinct from the runner user and the Lambda role); the
# runner's scoped iam:PassRole targets exactly this name, so it MUST exist.
box_role=hornvale-gate-runner-box
aws_admin iam get-role --role-name "$box_role" >/dev/null 2>&1 || \
    run aws_admin iam create-role --role-name "$box_role" \
        --assume-role-policy-document '{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":"ec2.amazonaws.com"},"Action":"sts:AssumeRole"}]}' \
        --tags Key=project,Value=hornvale-gate >/dev/null
# Inline policy: self-terminate tagged boxes (the idle timer) + sccache S3 RW.
box_policy="$(mktemp)"
cat > "$box_policy" <<JSON
{ "Version": "2012-10-17", "Statement": [
  { "Effect": "Allow", "Action": "ec2:TerminateInstances", "Resource": "*",
    "Condition": { "StringEquals": { "aws:ResourceTag/project": "hornvale-gate" } } },
  { "Effect": "Allow", "Action": ["s3:GetObject","s3:PutObject","s3:ListBucket"],
    "Resource": ["arn:aws:s3:::${bucket}","arn:aws:s3:::${bucket}/*"] } ]}
JSON
run aws_admin iam put-role-policy --role-name "$box_role" --policy-name box \
    --policy-document "file://$box_policy"
if ! aws_admin iam get-instance-profile --instance-profile-name "$box_role" >/dev/null 2>&1; then
    run aws_admin iam create-instance-profile --instance-profile-name "$box_role" \
        --tags Key=project,Value=hornvale-gate >/dev/null
    run aws_admin iam add-role-to-instance-profile --instance-profile-name "$box_role" --role-name "$box_role"
fi
echo "  box role + instance profile: $box_role"
manifest_set box_instance_profile "$box_role"

section "Launch template"
# Market (spot) and instance tags are supplied by the runner at run-instances
# time (gate-remote.sh), so the template carries image/type/key/SG/profile/
# userdata only. IMDSv2 tokens allowed-not-required so the idle check's curl works.
ud_b64="$(base64 < "$here/userdata.sh" | tr -d '\n')"
lt_data="$(mktemp)"
cat > "$lt_data" <<JSON
{ "ImageId": "${ami}", "InstanceType": "${HVG_INSTANCE_TYPE}", "KeyName": "hornvale-gate",
  "SecurityGroupIds": ["${sg_id}"], "IamInstanceProfile": { "Name": "${box_role}" },
  "UserData": "${ud_b64}", "MetadataOptions": { "HttpTokens": "optional" } }
JSON
lt_id="$(aws_admin ec2 describe-launch-templates --launch-template-names hornvale-gate --query 'LaunchTemplates[0].LaunchTemplateId' --output text 2>/dev/null || true)"
if [ -z "$lt_id" ] || [ "$lt_id" = "None" ]; then
    lt_id="$(run aws_admin ec2 create-launch-template --launch-template-name hornvale-gate \
        --launch-template-data "file://$lt_data" \
        --tag-specifications 'ResourceType=launch-template,Tags=[{Key=project,Value=hornvale-gate}]' \
        --query 'LaunchTemplate.LaunchTemplateId' --output text)"
else
    ver="$(run aws_admin ec2 create-launch-template-version --launch-template-id "$lt_id" \
        --launch-template-data "file://$lt_data" --query 'LaunchTemplateVersion.VersionNumber' --output text)"
    run aws_admin ec2 modify-launch-template --launch-template-id "$lt_id" --default-version "$ver" >/dev/null
fi
echo "  launch template: $lt_id"
manifest_set launch_template "$lt_id"

section "Circuit breaker (Lambda)"
# Local, free build step — packages the pure-decision-fn + handler for upload.
breaker_zip="$(mktemp -d)/circuit_breaker.zip"
(cd "$here" && zip -q -X "$breaker_zip" circuit_breaker.py)
echo "  packaging: $breaker_zip"
# ... create/update execution role `hornvale-gate-breaker` (trust = lambda.amazonaws.com):
#     inline policy: ec2:DescribeInstances + ec2:TerminateInstances on instances tagged
#     project=hornvale-gate, iam:ListAccessKeys + iam:UpdateAccessKey on user
#     hornvale-gate-runner, logs:CreateLogGroup/CreateLogStream/PutLogEvents; tag project ...
# ... create/update Lambda function `hornvale-gate-breaker`: runtime python3.12, handler
#     circuit_breaker.handler, code = $breaker_zip, role = above, env vars HVG_REGION /
#     HVG_MAX_AGE_SECS / HVG_MAX_COUNT (from lib.sh); tag project ...
# ... create/update EventBridge rule `hornvale-gate-breaker-tick`: schedule rate(5 minutes),
#     target = the Lambda's ARN; `aws lambda add-permission` granting events.amazonaws.com
#     invoke, SourceArn = the rule's ARN ...
# ... manifest_set breaker_lambda_arn / breaker_rule_arn once created ...

section "Budgets"
# NOTE: budgets are DAILY (TimeUnit: DAILY), matching the spec's "$10/day" alert
# and "$25/day" hard action. A MONTHLY budget would trip the deny-all during
# normal use (~$1-2.50/day crosses $25 cumulatively in ~2 weeks), defeating the
# ~$25 worst-case-per-runaway bound — the hard action must fire on a RUNAWAY DAY,
# not on ordinary accumulated spend.
# ... create/update a $HVG_BUDGET_ALERT ($10) DAILY ACTUAL-cost budget, NOTIFY only,
#     email subscriber = admin; tag project ...
# ... create/update a $HVG_BUDGET_HARD ($25) DAILY ACTUAL-cost budget with an
#     IAM-deny budget ACTION (auto-apply, no approval required) targeting the
#     hornvale-gate-runner user on ACTUAL >= 100% ...
# ... manifest_set budget_alert_arn / budget_action_arn once created ...

echo "setup complete (dry-run=$DRY_RUN)"
