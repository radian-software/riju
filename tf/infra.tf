terraform {
  backend "remote" {
    organization = "riju"
    workspaces {
      name = "riju"
    }
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
  profile = "default"
  region  = "us-west-1"
}

data "aws_region" "current" {}

resource "aws_s3_bucket" "riju_debs" {
  bucket = "riju-debs"
  acl    = "public-read"
  tags   = local.tags
}

resource "aws_instance" "server" {
  instance_type = "t3.micro"
  ami           = data.external.env.result.AMI_ID
  tags          = local.tags
}

resource "aws_ebs_volume" "data" {
  availability_zone = "${data.aws_region.current.name}a"
  size              = 100
  tags              = local.tags
}
