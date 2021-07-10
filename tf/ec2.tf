resource "aws_security_group" "dev_server" {
  count = local.ssh_key_available ? 1 : 0

  name        = "riju-dev-server"
  description = "Security group for Riju dev server"

  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "HTTP"
    from_port   = 6119
    to_port     = 6119
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

resource "aws_instance" "dev_server" {
  count = local.ssh_key_available ? 1 : 0

  ami = data.aws_ami.ubuntu[0].id
  instance_type = "t3.2xlarge"
  ebs_optimized = true

  security_groups = [aws_security_group.dev_server[0].name]

  root_block_device {
    volume_size = 256

    tags = merge(local.tags, {
      Name = "Riju dev server"
    })
  }

  tags = {
    Name = "Riju dev server"
  }

  lifecycle {
    ignore_changes = [
      ami,
      security_groups,  # legacy
    ]
  }
}
