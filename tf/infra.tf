terraform {
  backend "s3" {
    key    = "state"
    region = "us-west-1"
  }
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 3.45"
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
}

provider "aws" {
  region = "us-west-1"
  default_tags {
    tags = local.tags
  }
}

data "aws_region" "current" {}

data "aws_vpc" "default" {
  default = true
}

data "aws_subnet_ids" "default" {
  vpc_id = data.aws_vpc.default.id
}

data "aws_subnet" "default" {
  for_each = data.aws_subnet_ids.default.ids
  id = each.value
}

resource "aws_iam_user" "deploy" {
  name = "riju-deploy"
}

resource "aws_iam_access_key" "deploy" {
  user = aws_iam_user.deploy.name
}

data "aws_iam_policy_document" "deploy" {
  statement {
    actions = [
      "s3:ListBucket",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}",
    ]
  }

  statement {
    actions = [
      "s3:*Object",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}/*",
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

data "aws_iam_policy_document" "riju" {
  statement {
    principals {
      type        = "*"
      identifiers = ["*"]
    }

    actions = [
      "s3:ListBucket",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}",
    ]
  }

  statement {
    principals {
      type        = "*"
      identifiers = ["*"]
    }

    actions = [
      "s3:GetObject",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}/*",
    ]
  }
}

resource "aws_s3_bucket" "riju" {
  bucket = data.external.env.result.S3_BUCKET
}

resource "aws_s3_bucket_public_access_block" "riju" {
  bucket = aws_s3_bucket.riju.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_s3_bucket_policy" "riju" {
  bucket = aws_s3_bucket.riju.id
  policy = data.aws_iam_policy_document.riju.json
}

resource "aws_ecr_repository" "riju" {
  name                 = "riju"
  image_tag_mutability = "IMMUTABLE"
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
}

resource "aws_security_group" "alb" {
  name        = "riju-alb"
  description = "Security group for Riju application load balancer"

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
}

resource "aws_launch_template" "server" {
  name = "riju-server"
  image_id = data.aws_ami.server.id
  instance_type = "t3.small"
  security_group_names = [aws_security_group.server.name]

  block_device_mappings {
    device_name = "/dev/sdh"
    ebs {
      volume_type              = "gp3"
      volume_size              = 125
    }
  }

  tags = {
    Name = "Riju server"
  }

  tag_specifications {
    resource_type = "instance"
    tags = {
      Name = "Riju server"
    }
  }
}

resource "aws_autoscaling_group" "server" {
  availability_zones = [
    for subnet in data.aws_subnet.default : subnet.availability_zone
  ]
  desired_capacity = 1
  min_size = 1
  max_size = 3

  launch_template {
    id = aws_launch_template.server.id
  }

  target_group_arns = [
    aws_lb_target_group.server_http.arn,
    aws_lb_target_group.server_https.arn,
  ]

  tags = concat(
    [
      {
        key = "Name"
        value = "Riju server"
        propagate_at_launch = false
      }
    ],
    [
      for key, value in local.tags : {
        key = key,
        value = value,
        propagate_at_launch = true,
      }
    ],
  )
}

resource "aws_lb" "server" {
  name = "riju-server"
  security_groups = [aws_security_group.alb.id]
  subnets = data.aws_subnet_ids.default.ids
}

resource "aws_lb_target_group" "server_http" {
  name = "riju-server-http"
  port = 80
  protocol = "HTTP"
  vpc_id = data.aws_vpc.default.id
}

resource "aws_lb_target_group" "server_https" {
  name = "riju-server-https"
  port = 443
  protocol = "HTTPS"
  vpc_id = data.aws_vpc.default.id
}

output "alb_dns_name" {
  value = aws_lb.server.dns_name
}

output "deploy_aws_access_key_id" {
  value = aws_iam_access_key.deploy.id
}

output "deploy_aws_secret_access_key" {
  value     = aws_iam_access_key.deploy.secret
  sensitive = true
}
