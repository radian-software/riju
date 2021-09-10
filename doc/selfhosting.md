# How to self-host Riju

You can host your own instance of Riju! This requires a bit of manual
setup, but everything that *can* be automated, *has* been automated.

**Warning: AWS is expensive and you are responsible for your own
spending. If you would be perturbed by accidentally burning a few
hundred dollars on unexpected compute, you probably shouldn't follow
these instructions.**

## Sign up for accounts

* [AWS](https://aws.amazon.com/)
* [CloudFlare](https://www.cloudflare.com/)
* [Fathom Analytics](https://usefathom.com/) (if you want analytics)
* [GitHub](https://github.com/)
* [Namecheap](https://www.namecheap.com/)
* [PagerDuty](https://www.pagerduty.com/) (if you want alerts)

## Configure accounts
### GitHub

Fork the Riju repository under your account.

### PagerDuty

Set up notification rules as desired. Configure the AWS CloudWatch
integration and obtain an integration URL.

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

Finally, if you don't have a personal SSH key, generate one with
`ssh-keygen`, and upload the public key (e.g. `~/.ssh/id_rsa.pub`) as
an [EC2 Key
Pair](https://us-west-1.console.aws.amazon.com/ec2/v2/home?region=us-west-1#KeyPairs:).
Remember the name you use for the key pair.

### Namecheap

Buy a domain name at which to host.

### CloudFlare

Enter your domain name and go through the setup and DNS verification.
Update the nameserver settings on Namecheap's side, and enable all the
fun CloudFlare options you'd like.

### Fathom Analytics

Enter your domain name and get a site ID.

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
FATHOM_SITE_ID=
SUPERVISOR_ACCESS_TOKEN=5ta2qzMFS417dG9gbfcMgjsbDnGMI4
```

### ADMIN\_PASSWORD

This will be the `sudo` password for Riju server nodes. Generate one
randomly with `pwgen -s 20 1`.

### FATHOM\_SITE\_ID

This is the site ID from your Fathom Analytics account. If you don't
need analytics, just leave this unset.

### SUPERVISOR\_ACCESS\_TOKEN

This is a static shared secret used for the Riju server's supervisor
API. Generate one randomly with `pwgen -s 30 1`.

## Build AMI

You'll want to run `make env` to load in the new variables from
`.env`. Now run `make packer-ci`. This will take up to 5 minutes to
build a timestamped AMI with a name like `riju-ci-20210711223158`.
Then run `make packer-web`. This will take up to 10 minutes to build
another AMI with a name like `riju-web-20210711223158.`

## Create local configuration (part 2 of 3)

Add to `.env` the following contents:

```
# Terraform
AMI_NAME=riju-web-20210711223158
CI_AMI_NAME=riju-ci-20210711223158
AWS_REGION=us-west-1
S3_BUCKET=riju-yourname-tf
SSH_KEY_NAME=
```

### AMI\_NAME

This is the AMI name from `make packer-web`.

### CI\_AMI\_NAME

This is the AMI name from `make packer-ci`.

### AWS\_REGION

This is the region in which most Terraform infrastructure will be
created. It should be the same as the default region you configured
for the AWS CLI. It doesn't have to be the same as the region in which
your Terraform state bucket is configured, although it simplifies
matters to keep them in the same region.

The main utility of having this as an explicit environment variable is
that Terraform respects it and won't always ask you what region to
use.

### S3\_BUCKET

This is the name of the S3 bucket that you created in a previous step.
`riju-yourname-tf`

### SSH\_KEY\_NAME

This is the name of the EC2 Key Pair you created in the AWS console.
You'll use it to connect to the development server.

## Set up Terraform infrastructure

Run `make env` again to load in the new variables from `.env`.

Now run `terraform init` and fill in the appropriate region and bucket
name for the Terraform state bucket you created in the AWS console.

At this point you can run `terraform apply` to create all the needed
infrastructure. Caution! At this point you probably want to go to the
EC2 console and stop the dev server. It is very expensive and will
rack up a few hundred dollars a month of compute. You should only have
it running when you're actively working on Riju.

## Finish AWS configuration

Go back to the AWS console and take care of a few loose ends:

* If you want, register a [custom public registry alias for
  ECR](https://us-west-1.console.aws.amazon.com/ecr/registries?region=us-west-1).
  This will make your public registry URL easier to remember.
* In the "View push commands" modal dialogs, take note of the
  repository URLs for your public and private Riju ECR repositories.
* If you want alerts, [create an SNS
  subscription](https://us-west-1.console.aws.amazon.com/sns/v3/home?region=us-west-1#/subscriptions)
  from the Riju SNS topic to the PagerDuty integration URL.

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
install it as a proxied CNAME record in CloudFlare DNS for your apex
domain. After DNS propagates, you should now be able to receive a 502
from Riju with no body content.

## Set up dev server

The dev server is provisioned with a fresh Ubuntu AMI. You probably
want to clone your repository up there, enable SSH agent forwarding,
etc. Doing a full build on your laptop is feasible, but unless you
have symmetric gigabit ethernet you're not going to get all the build
artifacts uploaded in less than a week.

## Build and deploy

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

## Set up CI

In your GitHub repo settings, create the secrets `AWS_ACCESS_KEY_ID`
and `AWS_SECRET_ACCESS_KEY` with the values from `terraform output
-json`. GitHub Actions should be good to go! However, I would
recommend doing builds from the EC2 dev server when you need to
rebuild a lot of artifacts.

You'll also want to go to `.github/workflows/main.yml` and update the
environment variables `AWS_REGION`, `DOCKER_REPO`,
`PUBLIC_DOCKER_REPO`, and `S3_BUCKET` as appropriate for your own
deployment (see the `.env` file you created earlier).
