#!/usr/bin/python3
#
# Delete IAM policies whose names match a pattern
#

import argparse
import boto3
import re
import click

def list_policies(iam, pattern):
    policies=[]
    paginator = iam.get_paginator('list_policies')
    for page in paginator.paginate(Scope='Local'):
        for policy in page['Policies']:
            if pattern.match(policy['PolicyName']):
                policies.append(policy)
    return policies


def delete_policy_versions(iam, policy):
    paginator = iam.get_paginator('list_policy_versions')
    for page in paginator.paginate(PolicyArn=policy['Arn']):
        for version in page['Versions']:
            if version['IsDefaultVersion']:
                continue
            print("Deleting version {v}".format(v=version['VersionId']))
            iam.delete_policy_version(PolicyArn=policy['Arn'], VersionId=version['VersionId'])


def delete_policy(iam, policy):
    print("Deleting policy {name}".format(name=policy['PolicyName']))
    delete_policy_versions(iam, policy)
    iam.delete_policy(PolicyArn=policy['Arn'])


def confirm_delete(policies):
    print("Delete policies:")
    for policy in policies:
        print(policy['PolicyName'])
    return click.confirm("Continue?")


def delete_matching_policies(pattern):
    iam = boto3.client('iam')
    policies = list_policies(iam, pattern)
    if len(policies) == 0:
        print("No matching policies")
        return
    if confirm_delete(policies):
        for policy in policies:
            delete_policy(iam, policy)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Delete IAM policies")
    parser.add_argument("--pattern", help="Regex to match policy name", default=".*")

    args = parser.parse_args()
    pattern = re.compile(args.pattern)
    delete_matching_policies(pattern)
