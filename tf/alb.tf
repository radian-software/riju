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
