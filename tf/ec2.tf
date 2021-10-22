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

resource "aws_launch_template" "server" {
  name          = "riju-server"
  image_id      = data.aws_ami.server.id
  instance_type = "t3.large"

  security_group_names = [aws_security_group.server.name]
  iam_instance_profile {
    name = aws_iam_instance_profile.server.name
  }

  update_default_version = true

  block_device_mappings {
    device_name = "/dev/sdh"
    ebs {
      volume_type = "gp3"
      volume_size = 256
    }
  }

  tags = {
    Name = "Riju server"
  }

  tag_specifications {
    resource_type = "instance"
    tags = merge(local.tags, {
      Name               = "Riju server"
      BillingSubcategory = "Riju:EC2:Webserver"
    })
  }

  tag_specifications {
    resource_type = "volume"
    tags = merge(local.tags, {
      Name               = "Riju server"
      BillingSubcategory = "Riju:EBS:Webserver"
    })
  }
}

resource "aws_security_group" "deploy" {
  name        = "riju-deploy"
  description = "Security group for Riju CI"

  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
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
