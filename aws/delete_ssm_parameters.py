#!/usr/bin/python3
#
# Delete all SSM parameters under the give prefix
#

import argparse
import boto3
import click
import sys

def list_parameters(ssm, prefix):
    parameters = []
    filter = {'Key': 'Name', 'Values': [prefix]}
    paginator = ssm.get_paginator('describe_parameters')
    for page in paginator.paginate(Filters=[filter]):
        for param in page['Parameters']:
            parameters.append(param['Name'])
    return parameters


def delete_parameters(ssm, parameter_names):
    n = len(parameter_names)
    for i in range(0, n, 10):
        batch = parameter_names[i:min(i+10, n)]
        ssm.delete_parameters(Names=batch)


parser = argparse.ArgumentParser(description="Delete SSM Parameters")
parser.add_argument("--region", help="AWS Region name", default="eu-west-1")
parser.add_argument("--prefix", help="Delete parameters with this prefix", required=True)

args = parser.parse_args()

ssm = boto3.client('ssm', region_name=args.region)
params = list_parameters(ssm, args.prefix)

if not params:
    print("No parameters with prefix {prefix}".format(prefix=args.prefix))
    sys.exit(0)

print("Delete parameters:")
for p in params:
    print(" {name}".format(name=p))

if click.confirm("Continue?"):
    delete_parameters(ssm, params)
