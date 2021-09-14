resource "aws_acm_certificate" "riju" {
  domain_name               = data.external.env.result.DOMAIN
  subject_alternative_names = ["*.${data.external.env.result.DOMAIN}"]
  validation_method         = "DNS"

  tags = {
    Name               = "Riju server"
    BillingSubcategory = "Riju:ACM"
  }
}

resource "aws_acm_certificate_validation" "riju" {
  certificate_arn = aws_acm_certificate.riju.arn
}
