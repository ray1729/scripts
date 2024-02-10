#!/usr/bin/env python3

import boto3
import botocore
import click


def get_interfaces(ec2, groupId):
    ifs = ec2.describe_network_interfaces(Filters=[
        {"Name": "group-id", "Values": [groupId]}
    ])
    return ifs['NetworkInterfaces']


def list_unused_groups(ec2):
    unused = []
    paginator = ec2.get_paginator('describe_security_groups')
    for page in paginator.paginate():
        for sg in page['SecurityGroups']:
            interfaces = get_interfaces(ec2, sg['GroupId'])
            num_attachments = len(interfaces)
            if num_attachments == 0:
                unused.append(sg)
    return unused
    

def delete_security_groups(ec2, security_groups):
    for sg in security_groups:
        try:
            ec2.delete_security_group(GroupId=sg['GroupId'])
            print("Deleted security group {id}".format(id=sg['GroupId']))
        except botocore.exceptions.ClientError as err:
            print("Security group {id} could not be deleted".format(id=sg['GroupId']))
            print(err)
            

if __name__ == "__main__":
    ec2 = boto3.client('ec2')
    unused = list_unused_groups(ec2)
    for sg in unused:
        print(sg['GroupId'], sg['GroupName'], sg['Description'])
    if click.confirm("Delete {n} groups?".format(n=len(unused))):
        delete_security_groups(ec2, unused)





