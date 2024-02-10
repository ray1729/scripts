#!/usr/bin/python3
#
# Deleting a user programmatically requires deletion of several 
# entities for each user:
#
# Password ( DeleteLoginProfile)
# Access keys ( DeleteAccessKey)
# Signing certificate ( DeleteSigningCertificate)
# SSH public key ( DeleteSSHPublicKey)
# Git credentials ( DeleteServiceSpecificCredential)
# Multi-factor authentication (MFA) device ( DeactivateMFADevice, DeleteVirtualMFADevice)
# Inline policies ( DeleteUserPolicy)
# Attached managed policies ( DetachUserPolicy)
# Group memberships ( RemoveUserFromGroup)
#

import argparse
import boto3
import botocore

iam_client = boto3.client("iam")

def list_all(paginator_name, collection_name, key_name, init_args):
    result = []
    paginator = iam_client.get_paginator(paginator_name)
    for page in paginator.paginate(**init_args):
        for item in page.get(collection_name, []):
            if key_name:
                result.append(item[key_name])
            else:
                result.append(item)
    return result


def list_access_keys(user_name):
    return list_all("list_access_keys", "AccessKeyMetadata", "AccessKeyId", {"UserName": user_name})


def list_signing_certificates(user_name):
    return list_all("list_signing_certificates", "Certificates", "CertificateId", {"UserName": user_name})
 

def list_ssh_public_keys(user_name):
    return list_all("list_ssh_public_keys", "SSHPublicKeys", "SSHPublicKeyId", {"UserName": user_name})


def list_mfa_devices(user_name):
    return list_all("list_mfa_devices", "MFADevices", "SerialNumber", {"UserName": user_name})


def list_service_specific_credentials(user_name):
    credential_ids = []
    response = iam_client.list_service_specific_credentials(UserName=user_name)
    for item in response.get("ServiceSpecificCredentials", []):
        credential_ids.append(item["ServiceSpecificCredentialId"])
    return credential_ids


def list_inline_policies(user_name):
    return list_all("list_user_policies", "PolicyNames", None, {"UserName": user_name})


def list_attached_policies(user_name):
    return list_all("list_attached_user_policies", "AttachedPolicies", "PolicyArn", {"UserName": user_name})


def list_groups(user_name):
    return list_all("list_groups_for_user", "Groups", "GroupName", {"UserName": user_name})


def delete_user(user_name, dry_run=True):
    print(f"Delete user {user_name}:")
    user = iam_client.get_user(UserName=user_name)
    try:
        if not dry_run:
            iam_client.delete_login_profile(UserName=user_name)
        print("  Delete login profile")
    except botocore.exceptions.ClientError as err:
        if err.response["Error"]["Code"] != "NoSuchEntity":
            raise
    for key_id in list_access_keys(user_name):
        if not dry_run:
            iam_client.delete_access_key(UserName=user_name, AccessKeyId=key_id)
        print(f"  Delete access key {key_id}")
    for cert_id in list_signing_certificates(user_name):
        if not dry_run:
            iam_client.delete_signing_certificate(UserName=user_name, CertificateId=cert_id)
        print(f"  Delete signing certificate {cert_id}")
    for key_id in list_ssh_public_keys(user_name):
        if not dry_run:
            iam_client.delete_ssh_public_key(UserName=user_name, SSHPublicKeyId=key_id)
        print(f"  Delete SSH public key {key_id}")
    for cred_id in list_service_specific_credentials(user_name):
        if not dry_run:
            iam_client.delete_service_specific_credential(UserName=user_name, ServiceSpecificCredentialId=cred_id)
        print(f"  Delete service-specific credential {cred_id}")
    for serial_no in list_mfa_devices(user_name):
        if not dry_run:
            iam_client.deactivate_mfa_device(UserName=user_name, SerialNumber=serial_no)
            iam_client.delete_virtual_mfa_device(SerialNumber=serial_no)
        print(f"  Delete MFA device {serial_no}")
    for policy_name in list_inline_policies(user_name):
        if not dry_run:
            iam_client.delete_user_policy(UserName=user_name, PolicyName=policy_name)
        print(f"  Delete inline policy {policy_name}")
    for policy_arn in list_attached_policies(user_name):
        if not dry_run:
            iam_client.detach_user_policy(UserName=user_name, PolicyArn=policy_arn)
        print(f"  Detach policy {policy_arn}")
    for group_name in list_groups(user_name):
        if not dry_run:
            iam_client.remove_user_from_group(UserName=user_name, GroupName=group_name)
        print(f"  Remove user from group {group_name}")
    if not dry_run:
        iam_client.delete_user(UserName=user_name)
    print(f"  Deleted {user_name}")
    

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Delete IAM user(s)")
    parser.add_argument("--dry-run", help="Show the actions that would be performed", action=argparse.BooleanOptionalAction, default=True)
    parser.add_argument("user_names", nargs="+", )
    args = parser.parse_args()
    for user_name in args.user_names:
        delete_user(user_name, args.dry_run)