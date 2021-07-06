resource "aws_acm_certificate" "riju" {
  domain_name = "riju.codes"
  subject_alternative_names = ["*.riju.codes"]
  validation_method = "DNS"

  tags = {
    Name = "Riju server"
  }
}

resource "aws_acm_certificate_validation" "riju" {
  certificate_arn = aws_acm_certificate.riju.arn
}
