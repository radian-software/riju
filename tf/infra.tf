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

provider "aws" {
  profile = "default"
  region  = "us-west-1"
}

resource "aws_s3_bucket" "riju_debs" {
  bucket = "riju-debs"
  acl    = "private"
  tags = {
    Terraform = "Managed by Terraform"
  }
}

resource "aws_ecr_repository" "riju_app" {
  name                 = "riju-app"
  image_tag_mutability = "IMMUTABLE"
}
