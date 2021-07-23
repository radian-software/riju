data "aws_ami" "server" {
  count = local.ami_available ? 1 : 0

  owners = ["self"]

  filter {
    name   = "name"
    values = [data.external.env.result.AMI_NAME]
  }
}
