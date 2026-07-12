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

section "EC2 Spot service-linked role (one-time account bootstrap)"
# The first spot launch needs AWSServiceRoleForEC2Spot; the least-privilege
# runner can't create it, so provision it here as admin. Propagation lags a
# few seconds after first creation.
aws_admin iam get-role --role-name AWSServiceRoleForEC2Spot >/dev/null 2>&1 || \
    run aws_admin iam create-service-linked-role --aws-service-name spot.amazonaws.com >/dev/null 2>&1 || true

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
account="$(aws_admin sts get-caller-identity --query Account --output text)"
breaker_role=hornvale-gate-breaker
aws_admin iam get-role --role-name "$breaker_role" >/dev/null 2>&1 || \
    run aws_admin iam create-role --role-name "$breaker_role" \
        --assume-role-policy-document '{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":"lambda.amazonaws.com"},"Action":"sts:AssumeRole"}]}' \
        --tags Key=project,Value=hornvale-gate >/dev/null
breaker_pol="$(mktemp)"
cat > "$breaker_pol" <<JSON
{ "Version":"2012-10-17","Statement":[
  {"Effect":"Allow","Action":"ec2:DescribeInstances","Resource":"*"},
  {"Effect":"Allow","Action":"ec2:TerminateInstances","Resource":"*","Condition":{"StringEquals":{"aws:ResourceTag/project":"hornvale-gate"}}},
  {"Effect":"Allow","Action":["iam:ListAccessKeys","iam:UpdateAccessKey"],"Resource":"arn:aws:iam::${account}:user/hornvale-gate-runner"},
  {"Effect":"Allow","Action":["logs:CreateLogGroup","logs:CreateLogStream","logs:PutLogEvents"],"Resource":"*"} ]}
JSON
run aws_admin iam put-role-policy --role-name "$breaker_role" --policy-name breaker --policy-document "file://$breaker_pol"
breaker_role_arn="arn:aws:iam::${account}:role/${breaker_role}"
if aws_admin lambda get-function --function-name hornvale-gate-breaker >/dev/null 2>&1; then
    run aws_admin lambda update-function-code --function-name hornvale-gate-breaker --zip-file "fileb://$breaker_zip" >/dev/null
else
    # New-role propagation to Lambda can lag; retry the create a few times.
    for _ in 1 2 3 4 5 6; do
        if aws_admin lambda create-function --function-name hornvale-gate-breaker \
            --runtime python3.12 --handler circuit_breaker.handler --role "$breaker_role_arn" \
            --zip-file "fileb://$breaker_zip" --timeout 30 \
            --environment "Variables={HVG_REGION=$HVG_REGION,HVG_MAX_AGE_SECS=$HVG_MAX_AGE_SECS,HVG_MAX_COUNT=$HVG_MAX_COUNT}" \
            --tags project=hornvale-gate >/dev/null 2>&1; then break; fi
        echo "  waiting for breaker-role propagation..."; sleep 5
    done
fi
lambda_arn="$(aws_admin lambda get-function --function-name hornvale-gate-breaker --query 'Configuration.FunctionArn' --output text)"
manifest_set breaker_lambda_arn "$lambda_arn"
rule_arn="$(aws_admin events put-rule --name hornvale-gate-breaker-tick --schedule-expression 'rate(5 minutes)' \
    --tags Key=project,Value=hornvale-gate --query RuleArn --output text)"
aws_admin lambda add-permission --function-name hornvale-gate-breaker --statement-id hvg-breaker-tick \
    --action lambda:InvokeFunction --principal events.amazonaws.com --source-arn "$rule_arn" >/dev/null 2>&1 || true
run aws_admin events put-targets --rule hornvale-gate-breaker-tick --targets "Id=1,Arn=$lambda_arn" >/dev/null
manifest_set breaker_rule_arn "$rule_arn"
echo "  breaker lambda: $lambda_arn"

section "Budgets"
# NOTE: budgets are DAILY (TimeUnit: DAILY), matching the spec's "$10/day" alert
# and "$25/day" hard action. A MONTHLY budget would trip the deny-all during
# normal use (~$1-2.50/day crosses $25 cumulatively in ~2 weeks), defeating the
# ~$25 worst-case-per-runaway bound — the hard action must fire on a RUNAWAY DAY,
# not on ordinary accumulated spend.
email="${HVG_BUDGET_EMAIL:-claude@tenesm.us}"   # alert recipient; override with HVG_BUDGET_EMAIL
echo "  alert email: $email"
# DAILY alert budget: two email thresholds — 100% (=$HVG_BUDGET_ALERT) and the
# $HVG_BUDGET_HARD level (AWS budget ACTIONS can't be daily, so daily is
# notify-only; the 5-min circuit breaker is the daily-scale enforcement).
warn_pct="$(python3 -c "print(round(100*$HVG_BUDGET_HARD/$HVG_BUDGET_ALERT))")"
ab="$(mktemp)"; an="$(mktemp)"
printf '{"BudgetName":"hornvale-gate-daily-alert","BudgetLimit":{"Amount":"%s","Unit":"USD"},"TimeUnit":"DAILY","BudgetType":"COST"}' "$HVG_BUDGET_ALERT" > "$ab"
printf '[{"Notification":{"NotificationType":"ACTUAL","ComparisonOperator":"GREATER_THAN","Threshold":100,"ThresholdType":"PERCENTAGE"},"Subscribers":[{"SubscriptionType":"EMAIL","Address":"%s"}]},{"Notification":{"NotificationType":"ACTUAL","ComparisonOperator":"GREATER_THAN","Threshold":%s,"ThresholdType":"PERCENTAGE"},"Subscribers":[{"SubscriptionType":"EMAIL","Address":"%s"}]}]' "$email" "$warn_pct" "$email" > "$an"
if aws_admin budgets describe-budget --account-id "$account" --budget-name hornvale-gate-daily-alert >/dev/null 2>&1; then
    run aws_admin budgets update-budget --account-id "$account" --new-budget "file://$ab" >/dev/null
    aws_admin budgets create-notification --account-id "$account" --budget-name hornvale-gate-daily-alert \
        --notification "{\"NotificationType\":\"ACTUAL\",\"ComparisonOperator\":\"GREATER_THAN\",\"Threshold\":$warn_pct,\"ThresholdType\":\"PERCENTAGE\"}" \
        --subscribers "{\"SubscriptionType\":\"EMAIL\",\"Address\":\"$email\"}" >/dev/null 2>&1 || true
else
    run aws_admin budgets create-budget --account-id "$account" --budget "file://$ab" --notifications-with-subscribers "file://$an"
fi
# Retire the earlier daily "hard" budget — AWS budget ACTIONS can't be daily, so
# the auto-disable backstop is monthly (below).
aws_admin budgets delete-budget --account-id "$account" --budget-name hornvale-gate-daily-hard >/dev/null 2>&1 || true
# Deny policy the hard action attaches (kills the runner's ability to launch).
deny_arn="arn:aws:iam::${account}:policy/hornvale-gate-deny"
aws_admin iam get-policy --policy-arn "$deny_arn" >/dev/null 2>&1 || \
    run aws_admin iam create-policy --policy-name hornvale-gate-deny \
        --policy-document '{"Version":"2012-10-17","Statement":[{"Effect":"Deny","Action":"ec2:RunInstances","Resource":"*"}]}' \
        --tags Key=project,Value=hornvale-gate >/dev/null
# Budgets-assumed role that attaches the deny policy to the runner.
ba_role=hornvale-gate-budget-action
aws_admin iam get-role --role-name "$ba_role" >/dev/null 2>&1 || \
    run aws_admin iam create-role --role-name "$ba_role" \
        --assume-role-policy-document '{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":"budgets.amazonaws.com"},"Action":"sts:AssumeRole"}]}' \
        --tags Key=project,Value=hornvale-gate >/dev/null
run aws_admin iam put-role-policy --role-name "$ba_role" --policy-name attach \
    --policy-document "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"iam:AttachUserPolicy\",\"Resource\":\"arn:aws:iam::${account}:user/hornvale-gate-runner\"}]}"
# MONTHLY hard budget ($HVG_BUDGET_MONTHLY_HARD) + auto-apply IAM-deny action at
# 100% ACTUAL. Monthly because AWS budget ACTIONS don't support daily; set well
# above normal use (~$30-75/mo) so it only fires on a sustained runaway.
hb="$(mktemp)"
printf '{"BudgetName":"hornvale-gate-monthly-hard","BudgetLimit":{"Amount":"%s","Unit":"USD"},"TimeUnit":"MONTHLY","BudgetType":"COST"}' "$HVG_BUDGET_MONTHLY_HARD" > "$hb"
aws_admin budgets describe-budget --account-id "$account" --budget-name hornvale-gate-monthly-hard >/dev/null 2>&1 || \
    run aws_admin budgets create-budget --account-id "$account" --budget "file://$hb"
existing_action="$(aws_admin budgets describe-budget-actions-for-budget --account-id "$account" --budget-name hornvale-gate-monthly-hard --query 'Actions[0].ActionId' --output text 2>/dev/null || true)"
if [ -z "$existing_action" ] || [ "$existing_action" = "None" ]; then
    run aws_admin budgets create-budget-action --account-id "$account" --budget-name hornvale-gate-monthly-hard \
        --notification-type ACTUAL --action-type APPLY_IAM_POLICY \
        --action-threshold '{"ActionThresholdValue":100,"ActionThresholdType":"PERCENTAGE"}' \
        --definition "{\"IamActionDefinition\":{\"PolicyArn\":\"$deny_arn\",\"Users\":[\"hornvale-gate-runner\"]}}" \
        --execution-role-arn "arn:aws:iam::${account}:role/${ba_role}" --approval-model AUTOMATIC \
        --subscribers "[{\"SubscriptionType\":\"EMAIL\",\"Address\":\"$email\"}]" >/dev/null
fi
echo "  budgets: \$${HVG_BUDGET_ALERT}/day + \$${HVG_BUDGET_HARD}/day email alerts; \$${HVG_BUDGET_MONTHLY_HARD}/mo hard IAM-deny action -> $email"

echo "setup complete (dry-run=$DRY_RUN)"
