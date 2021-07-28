resource "aws_ssm_parameter" "ci_ami_id" {
  name      = "riju-ci-ami-id"
  type      = "String"
  value     = data.aws_ami.ci.id
  data_type = "aws:ec2:image"
}

resource "aws_ssm_parameter" "docker_repo" {
  name  = "riju-docker-repo-host"
  type  = "String"
  value = aws_ecr_repository.riju.repository_url
}

resource "aws_ssm_parameter" "public_docker_repo" {
  name  = "riju-public-docker-repo-host"
  type  = "String"
  value = aws_ecrpublic_repository.riju.repository_uri
}

resource "aws_ssm_parameter" "s3_bucket" {
  name  = "riju-s3-bucket-name"
  type  = "String"
  value = aws_s3_bucket.riju.bucket
}
