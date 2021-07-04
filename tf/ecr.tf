resource "aws_ecr_repository" "riju" {
  name                 = "riju"
  image_tag_mutability = "IMMUTABLE"
}
