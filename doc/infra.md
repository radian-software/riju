# Riju infrastructure

This document has a brief description of how the deployed Riju
infrastructure is set up. It's *super* rough.

When you visit <https://riju.codes>, traffic is routed by CloudFlare
proxy (DNS hosted on Namecheap) to an AWS ALB (with TLS cert provided
by ACM) pointed at an EC2 ASG. Each EC2 node in the ASG (for now) has
its own EBS volume used for Docker data.

The nodes each run a supervisor binary written in Go, which
orchestrates Docker and systemd to run the Riju server container and
proxy traffic internally to the server to handle blue/green cutovers.
Deployment of the supervisor binary is via AMI, and other deployment
configuration is done by a JSON file in S3 that the supervisor binary
polls for.

Note: in the future, we will probably use EBS multi-attach and a
separate supervisor node that uses the EC2 API to manage the
attachments and farm out configuration updates to the server nodes.
This should help cut costs.

Docker images (for both the server and individual languages) are
hosted on AWS ECR, and other intermediate build artifacts are hosted
on S3.

Inside the Riju server container itself, which exposes HTTP traffic to
the internal supervisor proxy, we have an Express server that receives
websocket API messages and translates them into invocations of a C
setuid binary that is used to interface with the Docker daemon in a
safe way and spin up user containers with appropriate resource
restrictions.

Observability is mostly limited, but we have some CloudWatch
dashboards and alarms set up, with an SNS topic that goes to
PagerDuty.
