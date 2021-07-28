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
  ami_name      = "riju-ci-${local.timestamp}"
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
    value = "riju-ci-${local.timestamp}"
  }
}

build {
  sources = ["source.amazon-ebs.ubuntu"]

  provisioner "file" {
    destination = "/tmp/riju-init-volume"
    source      = "riju-init-volume"
  }

  provisioner "shell" {
    script = "provision-ci.bash"
  }
}
