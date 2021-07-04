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
  description = "Role used by CI to deploy Riju"
  policy      = data.aws_iam_policy_document.deploy.json
}

resource "aws_iam_user_policy_attachment" "deploy" {
  user       = aws_iam_user.deploy.name
  policy_arn = aws_iam_policy.deploy.arn
}

data "aws_iam_policy_document" "riju" {
  statement {
    principals {
      type        = "*"
      identifiers = ["*"]
    }

    actions = [
      "s3:ListBucket",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}",
    ]
  }

  statement {
    principals {
      type        = "*"
      identifiers = ["*"]
    }

    actions = [
      "s3:GetObject",
    ]

    resources = [
      "arn:aws:s3:::${aws_s3_bucket.riju.bucket}/*",
    ]
  }
}
