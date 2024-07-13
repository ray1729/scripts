#!/usr/bin/python3
#
# An S3 bucket can only be deleted if it is empty, so all
# objects must be deleted. For a versioned bucket, this includes
# object versions and object deletion markers.
#

import argparse
import boto3
import click


def delete_s3_bucket(bucket_name, dry_run=True):
    s3 = boto3.resource('s3')
    bucket = s3.Bucket(bucket_name)
    if not bucket.creation_date:
        print(f"Bucket {bucket_name} not found")
        return
    n = 0
    for o in bucket.objects.all():
        n = n+1
        print(f"Delete {o.key}")
    if click.confirm(f"Delete {n} objects from {bucket_name}?"):
        bucket.objects.all().delete()
        bucket.object_versions.all().delete()
        bucket.delete()
        print(f"Deleted bucket {bucket_name}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Delete S3 bucket and its contents")
    parser.add_argument("bucket", help="Name of the bucket to delete", nargs=1)
    args = parser.parse_args()
    for bucket in args.bucket:
        delete_s3_bucket(bucket)
