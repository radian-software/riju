data "aws_ami" "server" {
  owners = ["self"]

  filter {
    name   = "name"
    values = [data.external.env.result.AMI_NAME]
  }
}

# data "aws_ami" "ci" {
#   owners = ["self"]

#   filter {
#     name   = "name"
#     values = [data.external.env.result.CI_AMI_NAME]
#   }
# }
