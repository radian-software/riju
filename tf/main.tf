terraform {
  backend "s3" {
    key = "state"
  }
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 3.45"
    }
    null = {
      source  = "hashicorp/null"
      version = "~> 3.1"
    }
  }
}

data "external" "env" {
  program = ["jq", "-n", "env"]
}

locals {
  tags = {
    Terraform       = "Managed by Terraform"
    BillingCategory = "Riju"
  }

  ami_available     = lookup(data.external.env.result, "AMI_NAME", "") != "" ? true : false
  ssh_key_available = lookup(data.external.env.result, "SSH_KEY_NAME", "") != "" ? true : false
}

provider "aws" {
  default_tags {
    tags = local.tags
  }
}

provider "aws" {
  alias  = "us_east_1"
  region = "us-east-1"
  default_tags {
    tags = local.tags
  }
}

data "aws_caller_identity" "current" {}

data "aws_region" "current" {}

data "aws_vpc" "default" {
  default = true
}

data "aws_subnet_ids" "default" {
  vpc_id = data.aws_vpc.default.id
}

data "aws_subnet" "default" {
  for_each = data.aws_subnet_ids.default.ids
  id       = each.value
}
