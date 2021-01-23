# Riju infrastructure

You can host your own instance of Riju! This requires a bit of manual
setup, but everything that *can* be automated, *has* been automated.

## Sign up for accounts

* [AWS](https://aws.amazon.com/)
* [Docker Hub](https://hub.docker.com/)
* [GitHub](https://github.com/)

## Configure accounts
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

You also need to create an S3 bucket to store Terraform's state. Go to
[S3 in
us-west-1](https://s3.console.aws.amazon.com/s3/home?region=us-west-1#)
and create a new bucket called `riju-yourname-tf`.

### Docker Hub

Create a new repository to use for Riju.

### GitHub

Fork the Riju repository under your account.

## Install dependencies

* [Docker](https://www.docker.com/)
* [Git](https://git-scm.com/)
* [SSH](https://www.openssh.com/)

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

## Configure your instance
### AWS

Sign in locally to the AWS CLI by running

    $ aws configure

and entering the access key that you generated on AWS. The default
region is unimportant, although you may want to set it to `us-west-1`
because this is where Riju is configured to be deployed and having
that be consistent with your default command-line environment may
reduce confusion.

### Password

You need an administrator password that will be used for `sudo` access
on the production instance of Riju. You can generate one using `pwgen
-s 20 1`.

### SSH keys

You need two keys. One is used for administrator login on the
production instance, and the other is used to trigger deployments. You
can use your personal SSH key, if you already have one, for the admin
key. However, you should definitely generate a new key for
deployments. To generate an SSH key, use the `ssh-keygen` utility.

Place both keys in `~/.ssh`. This directory is automatically mounted
into the admin shell at `/home/riju/.ssh`.

### Additional configuration

You also need to have:

* The name of the Docker Hub repository that you created earlier (e.g.
  `raxod502/riju`).
* The base name of the S3 bucket(s) for Riju that will be created in
  your AWS account. The official buckets use prefix `riju`, but since
  S3 buckets must be globally unique you should use `riju-yourname`.

With configuration in hand, create a file `.env` in the Riju directory
with the following contents, adjusting the values to match your
configuration:

```
ADMIN_PASSWORD=50M9QDBtkQLV6zFAwhVg
ADMIN_SSH_PUBLIC_KEY_FILE=/home/riju/.ssh/id_rsa.pub
DEPLOY_SSH_PUBLIC_KEY_FILE=/home/riju/.ssh/id_rsa_riju_deploy.pub
DOCKER_REPO=raxod502/riju
S3_BUCKET=riju-yourname
```

## Create infrastructure

Run `make env` in the admin shell to start a subshell with environment
variables set from `.env`. Your first step will be to create an
[AMI](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIs.html)
for Riju.

```
$ cd packer
$ packer build config.json
```

This will take several minutes. Note the name of the AMI that is
printed, and add a corresponding line to `.env`:

```
AMI_NAME=riju-1609531301
```

Next, we will create the rest of the infrastructure.

```
$ cd ../tf
$ terraform init -backend-config="bucket=riju-yourname-tf"
$ terraform apply
```

This will print out the IP address of your newly provisioned server,
as well as permission-limited AWS credentials for use in CI. Add a
line to `.env` with the IP address:

```
DOMAIN=54.183.183.91
```

## Bootstrap server

Your newly provisioned server also isn't running anything yet.
You'll want to bootstrap it with the official image to make sure
everything is in working order:

```
$ ./tools/deploy.bash raxod502/riju:app
```

This may take a while. After it's finished, however, you should be
able to navigate to the IP address of your server in a browser and see
Riju up and running.

## Bootstrap S3

You probably don't want to build all of Riju's languages from scratch
when deploying a modified version, since that would take a long time.
You can avoid it by copying the latest built languages from Riju's
production S3 bucket. This will be fastest if you SSH into your
production server, which lives in AWS:

```
$ ssh admin@your-ip-address
$  export AWS_ACCESS_KEY_ID=...
$  export AWS_SECRET_ACCESS_KEY=...
$ aws s3 cp --recursive --source-region us-west-1 s3://riju-debs s3://riju-yourname-debs
```

## Set up CI

Go to [CircleCI](https://app.circleci.com/dashboard) and enable builds
for your fork of Riju. In the project settings, configure the
following environment variables:

* `DOCKER_REPO`: same as in `.env`
* `DOMAIN`: same as in `.env`
* `S3_BUCKET`: same as in `.env`
* `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`: the AWS access
  credentials generated by Terraform
* `DOCKER_USERNAME` and `DOCKER_PASSWORD`: your credentials for Docker
  Hub

You should now be able to trigger a build and see the production
instance updated automatically with a newly built image.

## Enable TLS

By default Riju will serve HTTP traffic if a TLS certificate is not
available. You can fix this. First, you'll need a domain name (or
subdomain on an existing domain). If you don't have one, you can buy
one at e.g. [Namecheap](https://www.namecheap.com/).

In your domain registrar's configuration interface, go to the
manual/advanced DNS settings and create an A record for your domain
pointing at your server's IP address. (To point the top-level domain
at Riju, set the host to `@`; to point a subdomain
`foo.yourdomain.io`, set the host to `foo`.)

Now SSH into the production server and run:

```
$ sudo systemctl stop riju
$ sudo certbot certonly --standalone
$ riju-install-certbot-hooks
$ sudo systemctl start riju
```

Alternatively, if you have an existing Certbot certificate you'd like
to transfer to the new server, you can copy over the entire
`/etc/letsencrypt` direction exactly as it stands, and run
`riju-install-certbot-hooks`.
