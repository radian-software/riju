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

resource "aws_lb_target_group" "server" {
  name = "riju-server-http"
  port = 80
  protocol = "HTTP"
  vpc_id = data.aws_vpc.default.id
}

resource "aws_lb_listener" "server_http" {
  load_balancer_arn = aws_lb.server.arn
  port = "80"
  protocol = "HTTP"

  default_action {
    type = "redirect"

    redirect {
      port = "443"
      protocol = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}

resource "aws_lb_listener" "server_https" {
  load_balancer_arn = aws_lb.server.arn
  port = "443"
  protocol = "HTTPS"
  ssl_policy = "ELBSecurityPolicy-2016-08"
  certificate_arn = aws_acm_certificate.riju.arn

  default_action {
    type = "forward"
    target_group_arn = aws_lb_target_group.server.arn
  }
}

resource "aws_autoscaling_attachment" "server" {
  count = local.ami_available ? 1 : 0

  autoscaling_group_name = aws_autoscaling_group.server[count.index].name
  alb_target_group_arn = aws_lb_target_group.server.arn
}
