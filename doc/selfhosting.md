# How to self-host Riju

You can host your own instance of Riju! This requires a bit of manual
setup, but everything that *can* be automated, *has* been automated.

**Warning: AWS is expensive and you are responsible for your own
spending. If you would be perturbed by accidentally burning a few
hundred dollars on unexpected compute, you probably shouldn't follow
these instructions.**

## Sign up for accounts

* [GitHub](https://github.com/)
* [AWS](https://aws.amazon.com/)
* [Namecheap](https://www.namecheap.com/) (or any other DNS provider)
* [CloudFlare](https://www.cloudflare.com/) (optional, for content
  delivery optimization)
* [Fathom Analytics](https://usefathom.com/) (optional, for analytics)
* [Grafana Cloud](https://grafana.com/products/cloud/) (optional, for
  monitoring)
* [Sentry](https://sentry.io/) (optional, for error tracking)
* [PagerDuty](https://www.pagerduty.com/) (optional, for alerts)
* [UptimeRobot](https://uptimerobot.com/) (optional, for alerts)
* [Statuspage](https://www.atlassian.com/software/statuspage)
  (optional, for uptime reporting)

## Configure accounts
### GitHub

Fork the Riju repository under your account.

### AWS

You need to generate an access key with sufficient permission to apply
[Riju's Terraform](../tf/infra.tf). The easiest way to do that if you
don't already know your way around IAM is:

* Go to [IAM](https://console.aws.amazon.com/iam/home)
* Create a new user
* Select "Programmatic access"
* Select "Attach existing policies directly"
* Attach the "AdministratorAccess" policy
* Copy and save the access key ID and secret access key

You also need to create an S3 bucket to store Terraform state. [Go to
S3](https://s3.console.aws.amazon.com/s3/home?region=us-west-1),
select your favorite AWS region, and create a new bucket called
`riju-yourname-tf`.

(Note that there are two S3 buckets in play; one to store Terraform
state and one to store Riju's actual build artifacts. The Terraform
one is created manually in this step; the other one is provisioned via
Terraform, and you choose the name in your `.env` file, as described
in a later step.)

### Namecheap (or any other DNS provider)

Buy a domain name at which to host (or you can use one you already
own, or a subdomain of one you already own). All you need is DNS panel
access for creating a CNAME.

## Install dependencies

* [Docker](https://www.docker.com/)
* [Git](https://git-scm.com/)

## Set up Riju locally

Clone the repository:

    $ git clone https://github.com/yourname/riju.git
    $ cd riju

Build and start the admin shell; all future actions can be done from
within the shell:

    $ make image shell I=admin

To get multiple terminal sessions inside the shell, run `make tmux`
and refer to a [tmux
cheatsheet](https://danielmiessler.com/study/tmux/) if you are
unfamiliar with tmux usage.

## Authenticate with AWS

Run `aws configure` and enter your IAM access credentials and your
preferred AWS region.

## Create local configuration (part 1 of 3)

Create a file `.env` in the Riju directory with the following
contents, referring to the following sub-sections for how to fill in
the values properly:

```
# Packer
ADMIN_PASSWORD=50M9QDBtkQLV6zFAwhVg
SUPERVISOR_ACCESS_TOKEN=5ta2qzMFS417dG9gbfcMgjsbDnGMI4
```

### ADMIN\_PASSWORD

This will be the `sudo` password for Riju server nodes. Generate one
randomly with `pwgen -s 20 1`.

### SUPERVISOR\_ACCESS\_TOKEN

This is a static shared secret used for the Riju server's supervisor
API. Generate one randomly with `pwgen -s 30 1`.

## Build web AMI

You'll want to run `set -a; . .env` to load in the new variables from
`.env`. Now run `make packer-web`. This will take up to 10 minutes to
build a timestamped AMI with a name like `riju-web-20210711223158`.

## Create local configuration (part 2 of 3)

Add to `.env` the following contents:

```
# Terraform
AMI_NAME=riju-web-20210711223158
AWS_REGION=us-west-1
DOMAIN=your.domain
S3_BUCKET=yourname-riju
S3_CONFIG_PATH=config.json
```

### AMI\_NAME

This is the AMI name from `make packer-web`.

### AWS\_REGION

This is the region in which most Terraform infrastructure will be
created. It should be the same as the default region you configured
for the AWS CLI. It doesn't have to be the same as the region in which
your Terraform state bucket is configured, although it simplifies
matters to keep them in the same region.

The main utility of having this as an explicit environment variable is
that Terraform respects it and won't always ask you what region to
use.

### DOMAIN

This is the base hostname you will be hosting Riju at (e.g.
`riju.codes`). Even if you plan to host on a subdomain of your domain,
set this to the apex. It's just used for TLS certificate provisioning.

### S3\_BUCKET

This is the name of the S3 bucket that will be used to store Riju
build artifacts (aside from Docker images). It needs to be globally
unique, so `yourname-riju` is a good choice.

## Set up Terraform infrastructure

Run `set -a; . .env` again to load in the new variables from `.env`.

Now run `terraform init` and fill in the appropriate region and bucket
name for the Terraform state bucket you created in the AWS console.

At this point you can run `terraform apply` to create all the needed
infrastructure.

*Note: when updating `.env` configuration that affects the web AMI,
follow these steps:*

1. Update `.env` and make sure it is sourced (`set -a; . .env`).
2. Run `make packer-web` and get the name of the new AMI.
3. Update it in `.env` under `AMI_NAME` and make sure the update is
   sourced (`set -a; . .env`).
4. Run `terraform apply`.
5. In the AWS console, scale up the ASG to 2 replicas and wait for the
   new instance to become healthy.
6. Scale the ASG back down to 1 replica; the older instance should be
   terminated.

## Finish AWS configuration

Go back to the AWS console and take care of a few loose ends:

* If you want, register a [custom public registry alias for
  ECR](https://us-west-1.console.aws.amazon.com/ecr/registries?region=us-west-1).
  This will make your public registry URL easier to remember.
* In the "View push commands" modal dialogs, take note of the
  repository URLs for your public and private Riju ECR repositories.

## Create local configuration (part 3 of 3)

Add to `.env` the following contents:

```
# Build
DOCKER_REPO=800516322591.dkr.ecr.us-west-1.amazonaws.com/riju
PUBLIC_DOCKER_REPO=public.ecr.aws/yourname/riju
```

### DOCKER\_REPO

This is the URL for your private ECR repository.

### PUBLIC\_DOCKER\_REPO

This is the URL for your public ECR repository.

## Configure DNS

Obtain the DNS record for Riju's ALB from `terraform output` and
install it as a CNAME record in your DNS panel. After DNS propagates,
you should now be able to receive a 502 from the load balancer.

## Launch instance

Navigate to your EC2 dashboard instances. Select "Launch instance from
template" and select `riju-server` for the launch template.

## Attach to target group

Once your instance is running you can attach it to the target group.
Navigate to Load Balancing > Target Groups. Select `riju-server-http`
and register the instance that you just launched. Within a minute or
two, you should still be getting 502s, but now with an empty response
body (these are now coming from the Riju server itself rather than
from the load balancer).

## Build and deploy

*(Note: Although it's easy to build Riju locally, you have to be able
to upload the finished build artifacts to ECR, which amount to about
40 GB of data transfer. If you don't have a symmetric Internet plan at
home, you may need to do this on an EC2 instance instead. You can
provision one manually with at least 256 GB of disk space, install
Docker, clone down Riju, copy over your `.env` file, and proceed as if
you were running locally.)*

Authenticate to ECR (lasts for 12 hours):

```
$ make ecr
```

Invoke Depgraph:

```
$ dep deploy:live --publish
```

After innumerable hours of build time (and probably some debugging to
fix languages that have broken since the last full build), Riju
should(tm) be live on your domain. You can connect to the live server
using EC2 Instance Connect by retrieving its instance ID from the AWS
console and running `mssh admin@i-theinstanceid`. Then you can check
(using the previously configured admin password) `sudo journalctl -efu
riju` to see the supervisor logs.

## Set up content delivery caching (optional)

Enter your domain name on CloudFlare and go through the setup and DNS
verification. Update the nameserver settings on Namecheap's side, and
enable all the fun CloudFlare options you'd like.

## Set up analytics (optional)

Sign up for Fathom Analytics, enter your domain name, and get a tag
for embedding. Set this as `ANALYTICS_TAG` in your `.env` file (use
single quoting, as Makefile handling of quotes is a bit nonstandard),
and build and roll out a new web AMI.

## Set up monitoring (optional)

Register a free Grafana Cloud account and get your API key. Set it in
`.env` as `GRAFANA_API_KEY`, and build and roll out a new web AMI.

## Set up error tracking (optional)

Set up a Sentry project and get the DSN. Set it in `.env` as
`SENTRY_DSN_PACKER`, and build and roll out a new web AMI.

## Set up alerts (optional)

Set up notification rules as desired in PagerDuty. Configure the AWS
CloudWatch integration and obtain an integration URL. Then, in AWS,
[create an SNS
subscription](https://us-west-1.console.aws.amazon.com/sns/v3/home?region=us-west-1#/subscriptions)
from the Riju SNS topic to the PagerDuty integration URL.

On UptimeRobot, create a monitor for the domain on which you're
serving Riju. Then [follow the steps to integrate UptimeRobot with
PagerDuty](https://uptimerobot.com/integrations/pagerduty/).

## Set up uptime reporting (optional)

Register with Statuspage and create at least one component. Ideally
you want UptimeRobot to send email directly to Statuspage so that the
service status can be reported automatically, [but this doesn't work
out of the
box](https://community.atlassian.com/t5/Statuspage-questions/Uptime-Robot/qaq-p/1469835).
What you have to, as discussed in the linked forum thread, is set up a
filter in your personal email that will forward the uptime alerts to
the Statuspage email endpoint.

## Set up finance tracking (optional)

Go to the [AWS billing
console](https://us-east-1.console.aws.amazon.com/billing/home?region=us-west-1#/tags)
and activate `BillingCategory` and `BillingSubcategory` as cost
allocation tags. Then, under "Cost & Usage Reports", create a report
for delivery to an S3 bucket, with the following parameters:

* Additional report details: Include resource IDs
* Data refresh settings: Automatically refresh
* Time granularity: Hourly
* Report versioning: Overwrite existing report
* Compression type: GZIP
* File format: text/csv

In `.env`, set `BILLING_REPORTS_URL` to the S3 filepath prefix for
your report. It should look like
`s3://bucketname/reportpathprefix/reportname`. You can now use the
tools in the `financials` subdirectory to generate your own AWS
billing reports. It works best if all other AWS resources in your
account are also tagged with `BillingCategory`.
