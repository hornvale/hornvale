"""Circuit breaker for the remote gate. Runs on a 5-min schedule.

Terminates any hornvale-gate box older than MAX_AGE or, if more than MAX_COUNT
run at once, terminates all and disables the runner identity. Stdlib + boto3
(present in the Lambda runtime) only.
"""
import os, time

def instances_to_kill(instances, now_ts, max_age_secs, max_count):
    breach = len(instances) > max_count
    if breach:
        return [i["InstanceId"] for i in instances], True
    return [i["InstanceId"] for i in instances if i["age"] >= max_age_secs], False

def handler(event, context):
    import boto3
    region = os.environ.get("HVG_REGION", "us-east-1")
    max_age = int(os.environ.get("HVG_MAX_AGE_SECS", "7200"))
    max_count = int(os.environ.get("HVG_MAX_COUNT", "10"))
    ec2 = boto3.client("ec2", region_name=region)
    now = int(time.time())
    resv = ec2.describe_instances(
        Filters=[{"Name": "tag:project", "Values": ["hornvale-gate"]},
                 {"Name": "instance-state-name", "Values": ["pending", "running"]}])
    boxes = [{"InstanceId": i["InstanceId"], "age": now - int(i["LaunchTime"].timestamp())}
             for r in resv["Reservations"] for i in r["Instances"]]
    ids, breach = instances_to_kill(boxes, now, max_age, max_count)
    if ids:
        ec2.terminate_instances(InstanceIds=ids)
    if breach:
        iam = boto3.client("iam")
        for k in iam.list_access_keys(UserName="hornvale-gate-runner")["AccessKeyMetadata"]:
            iam.update_access_key(UserName="hornvale-gate-runner", AccessKeyId=k["AccessKeyId"], Status="Inactive")
    return {"terminated": ids, "breach": breach}
