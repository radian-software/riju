data "aws_ami" "server" {
  count = local.ami_available ? 1 : 0

  owners = ["self"]

  filter {
    name   = "name"
    values = [data.external.env.result.AMI_NAME]
  }
}

data "aws_ami" "ubuntu" {
  count = local.ssh_key_available ? 1 : 0

  owners = ["099720109477"]

  filter {
    name = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-*-21.04-amd64-server-*"]
  }

  filter {
    name = "root-device-type"
    values = ["ebs"]
  }

  filter {
    name = "virtualization-type"
    values = ["hvm"]
  }

  most_recent = true
}
