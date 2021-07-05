resource "aws_iam_user" "deploy" {
  name = "riju-deploy"
}

resource "aws_iam_access_key" "deploy" {
  user = aws_iam_user.deploy.name
}

data "aws_iam_policy_document" "deploy" {
  statement {
    actions = [
      "s3:ListBucket",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}",
    ]
  }

  statement {
    actions = [
      "s3:*Object",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}/*",
    ]
  }
}

resource "aws_iam_policy" "deploy" {
  name        = "riju-deploy"
  description = "Policy granting CI access to deploy Riju"
  policy      = data.aws_iam_policy_document.deploy.json
}

resource "aws_iam_user_policy_attachment" "deploy" {
  user       = aws_iam_user.deploy.name
  policy_arn = aws_iam_policy.deploy.arn
}

data "aws_iam_policy_document" "server" {
  statement {
    actions = [
      "s3:GetObject",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}/config.json",
    ]
  }

  statement {
    actions = [
      "ecr:GetAuthorizationToken",
    ]

    resources = [
      "*",
    ]
  }

  statement {
    actions = [
      "ecr:BatchGetImage",
      "ecr:GetDownloadUrlForLayer",
    ]

    resources = [
      aws_ecr_repository.riju.arn,
    ]
  }
}

resource "aws_iam_policy" "server" {
  name = "riju-server"
  description = "Policy granting supervisor process on Riju server ability to download from S3"
  policy = data.aws_iam_policy_document.server.json
}

data "aws_iam_policy_document" "server_assume_role" {
  statement {
    actions = [
      "sts:AssumeRole",
    ]

    principals {
      type = "Service"
      identifiers = [
        "ec2.amazonaws.com",
      ]
    }
  }
}

resource "aws_iam_role" "server" {
  name = "riju-server"
  description = "Role used by supervisor process on Riju server"
  assume_role_policy = data.aws_iam_policy_document.server_assume_role.json
}

resource "aws_iam_role_policy_attachment" "server" {
  role = aws_iam_role.server.name
  policy_arn = aws_iam_policy.server.arn
}

resource "aws_iam_instance_profile" "server" {
  name = "riju-server"
  role = aws_iam_role.server.name
}
