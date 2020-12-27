terraform {
  backend "s3" {
    bucket = "riju-tf"
    key    = "state"
    region = "us-west-1"
  }
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 2.70"
    }
  }
}

locals {
  tags = {
    Terraform = "Managed by Terraform"
  }
}

data "external" "env" {
  program = ["jq", "-n", "env"]
}

provider "aws" {
  region = "us-west-1"
}

data "aws_region" "current" {}

resource "aws_iam_user" "deploy" {
  name = "riju-deploy"
  tags = local.tags
}

data "aws_iam_policy_document" "deploy" {
  statement {
    actions = [
      "s3:ListBucket",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju_debs.bucket}",
    ]
  }

  statement {
    actions = [
      "s3:*Object",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju_debs.bucket}/*",
    ]
  }
}

resource "aws_iam_policy" "deploy" {
  name        = "riju-deploy"
  description = "Role used by CI to deploy Riju"
  policy      = data.aws_iam_policy_document.deploy.json
}

resource "aws_iam_user_policy_attachment" "deploy" {
  user       = aws_iam_user.deploy.name
  policy_arn = aws_iam_policy.deploy.arn
}

resource "aws_s3_bucket" "riju_debs" {
  bucket = "riju-debs"
  acl    = "public-read"
  tags   = local.tags
}

data "aws_ami" "server" {
  owners = ["self"]

  filter {
    name   = "name"
    values = [data.external.env.result.AMI_NAME]
  }
}

resource "aws_security_group" "server" {
  name        = "riju-server"
  description = "Security group for Riju server"

  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "HTTP"
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "HTTPS"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = local.tags
}

resource "aws_instance" "server" {
  instance_type     = "t3.micro"
  ami               = data.aws_ami.server.id
  availability_zone = "${data.aws_region.current.name}b"
  security_groups   = [aws_security_group.server.name]
  tags              = local.tags
}

resource "aws_ebs_volume" "data" {
  availability_zone = "${data.aws_region.current.name}b"
  size              = 100
  tags              = local.tags
}

resource "aws_volume_attachment" "data" {
  device_name = "/dev/sdh"
  volume_id   = aws_ebs_volume.data.id
  instance_id = aws_instance.server.id
}
