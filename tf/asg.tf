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
  count = local.ami_available ? 1 : 0

  name          = "riju-server"
  image_id      = data.aws_ami.server[count.index].id
  instance_type = "t3.medium"

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
    tags = {
      Name = "Riju server"
    }
  }
}

resource "aws_autoscaling_group" "server" {
  count = local.ami_available ? 1 : 0

  name = "riju-server"

  availability_zones = [
    for subnet in data.aws_subnet.default : subnet.availability_zone
  ]
  desired_capacity = 1
  min_size         = 1
  max_size         = 3

  launch_template {
    id = aws_launch_template.server[count.index].id
  }

  tags = concat(
    [
      {
        key                 = "Name"
        value               = "Riju server"
        propagate_at_launch = false
      }
    ],
    [
      for key, value in local.tags : {
        key                 = key,
        value               = value,
        propagate_at_launch = true,
      }
    ],
  )

  lifecycle {
    ignore_changes = [target_group_arns]
  }
}
