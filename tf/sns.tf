resource "aws_sns_topic" "riju" {
  name = "Riju"

  tags = {
    BillingSubcategory = "Riju:SNS"
  }
}
