resource "aws_ecr_repository" "riju" {
  name                 = "riju"
  image_tag_mutability = "MUTABLE"

  tags = {
    BillingSubcategory = "Riju:ECR:Private"
  }
}

resource "aws_ecrpublic_repository" "riju" {
  provider        = aws.us_east_1
  repository_name = "riju"
}
