variable "admin_password" {
  type    = string
  default = "${env("ADMIN_PASSWORD")}"
}

variable "aws_region" {
  type    = string
  default = "${env("AWS_REGION")}"
}

variable "fathom_site_id" {
  type    = string
  default = "${env("FATHOM_SITE_ID")}"
}

variable "grafana_api_key" {
  type    = string
  default = "${env("GRAFANA_API_KEY")}"
}

variable "s3_bucket" {
  type = string
  default = "${env("S3_BUCKET")}"
}

variable "sentry_dsn" {
  type = string
  default = "${env("SENTRY_DSN_PACKER")}"
}

variable "supervisor_access_token" {
  type = string
  default = "${env("SUPERVISOR_ACCESS_TOKEN")}"
}

data "amazon-ami" "ubuntu" {
  filters = {
    name                = "ubuntu/images/hvm-ssd/ubuntu-*-21.04-amd64-server-*"
    root-device-type    = "ebs"
    virtualization-type = "hvm"
  }
  most_recent = true
  owners      = ["099720109477"]
}

locals {
  timestamp = regex_replace(timestamp(), "[- TZ:]", "")
}

source "amazon-ebs" "ubuntu" {
  ami_name      = "riju-web-${local.timestamp}"
  instance_type = "t3.micro"
  source_ami    = "${data.amazon-ami.ubuntu.id}"
  ssh_username  = "ubuntu"

  tag {
    key = "BillingCategory"
    value = "Riju"
  }

  tag {
    key = "BillingSubcategory"
    value = "Riju:AMI"
  }

  tag {
    key = "Name"
    value = "riju-web-${local.timestamp}"
  }
}

build {
  sources = ["source.amazon-ebs.ubuntu"]

  provisioner "file" {
    destination = "/tmp/cloudwatch.json"
    source = "cloudwatch.json"
  }

  provisioner "file" {
    destination = "/tmp/docker.json"
    source = "docker.json"
  }

  provisioner "file" {
    destination = "/tmp/promtail.service"
    source = "promtail.service"
  }

  provisioner "file" {
    destination = "/tmp/promtail.yaml"
    source = "promtail.yaml"
  }

  provisioner "file" {
    destination = "/tmp/riju.service"
    source      = "riju.service"
  }

  provisioner "file" {
    destination = "/tmp/riju.slice"
    source = "riju.slice"
  }

  provisioner "file" {
    destination = "/tmp/riju-init-volume"
    source      = "riju-init-volume"
  }

  provisioner "file" {
    destination = "/tmp/riju-supervisor"
    source      = "../supervisor/out/riju-supervisor"
  }

  provisioner "shell" {
    environment_vars = [
      "ADMIN_PASSWORD=${var.admin_password}",
      "AWS_REGION=${var.aws_region}",
      "FATHOM_SITE_ID=${var.fathom_site_id}",
      "GRAFANA_API_KEY=${var.grafana_api_key}",
      "S3_BUCKET=${var.s3_bucket}",
      "SENTRY_DSN=${var.sentry_dsn}",
      "SUPERVISOR_ACCESS_TOKEN=${var.supervisor_access_token}",
    ]
    script           = "provision-web.bash"
  }
}
