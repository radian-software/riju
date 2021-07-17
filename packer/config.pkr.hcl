variable "admin_password" {
  type    = string
  default = "${env("ADMIN_PASSWORD")}"
}

variable "aws_region" {
  type    = string
  default = "${env("AWS_REGION")}"
}

variable "s3_bucket" {
  type = string
  default = "${env("S3_BUCKET")}"
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
  ami_name      = "riju-${local.timestamp}"
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
    value = "riju-${local.timestamp}"
  }
}

build {
  sources = ["source.amazon-ebs.ubuntu"]

  provisioner "file" {
    destination = "/tmp/cloudwatch.json"
    source = "cloudwatch.json"
  }

  provisioner "file" {
    destination = "/tmp/riju-init-volume"
    source      = "riju-init-volume"
  }

  provisioner "file" {
    destination = "/tmp/riju-supervisor"
    source      = "../supervisor/out/riju-supervisor"
  }

  provisioner "file" {
    destination = "/tmp/riju.service"
    source      = "riju.service"
  }

  provisioner "shell" {
    environment_vars = [
      "ADMIN_PASSWORD=${var.admin_password}",
      "AWS_REGION=${var.aws_region}",
      "S3_BUCKET=${var.s3_bucket}",
      "SUPERVISOR_ACCESS_TOKEN=${var.supervisor_access_token}",
    ]
    script           = "provision.bash"
  }
}
