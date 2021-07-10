resource "aws_iam_user" "deploy" {
  name = "riju-deploy"
}

resource "aws_iam_access_key" "deploy" {
  user = aws_iam_user.deploy.name
}

data "aws_iam_policy_document" "deploy" {
  statement {
    actions = [
      "ecr:GetAuthorizationToken",
      "ecr-public:GetAuthorizationToken",
      "sts:GetServiceBearerToken",
    ]

    resources = [
      "*",
    ]
  }

  statement {
    actions = [
      "ecr:DescribeRegistry",
    ]

    resources = [
      "*",
    ]
  }

  statement {
    actions = [
      "ecr:BatchGetImage",
      "ecr:BatchCheckLayerAvailability",
      "ecr:CompleteLayerUpload",
      "ecr:DescribeImages",
      "ecr:DescribeRepositories",
      "ecr:GetDownloadUrlForLayer",
      "ecr:InitiateLayerUpload",
      "ecr:ListImages",
      "ecr:PutImage",
      "ecr:UploadLayerPart",
    ]

    resources = [
      aws_ecr_repository.riju.arn,
    ]
  }

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

data "aws_iam_policy_document" "deploy_assume_role" {
  statement {
    actions = [
      "sts:AssumeRole",
    ]

    principals {
      type = "AWS"
      identifiers = [
        "${data.aws_caller_identity.current.account_id}",
      ]
    }
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

resource "aws_iam_role" "deploy" {
  name = "riju-deploy"
  description = "Role used by CI and deployment"
  assume_role_policy = data.aws_iam_policy_document.deploy_assume_role.json
}

resource "aws_iam_role_policy_attachment" "deploy" {
  role = aws_iam_role.deploy.name
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

data "aws_iam_policy_document" "dev_server" {
  statement {
    actions = [
      "*",
    ]

    resources = [
      "*",
    ]
  }
}

resource "aws_iam_policy" "dev_server" {
  name = "riju-dev-server"
  description = "Policy granting AWS administrative access from dev server"
  policy = data.aws_iam_policy_document.dev_server.json
}

data "aws_iam_policy_document" "dev_server_assume_role" {
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

resource "aws_iam_role" "dev_server" {
  name = "riju-dev-server"
  description = "Role used by Riju dev server"
  assume_role_policy = data.aws_iam_policy_document.dev_server_assume_role.json
}

resource "aws_iam_role_policy_attachment" "dev_server" {
  role = aws_iam_role.dev_server.name
  policy_arn = aws_iam_policy.dev_server.arn
}

resource "aws_iam_instance_profile" "dev_server" {
  name = "riju-dev-server"
  role = aws_iam_role.dev_server.name
}

data "aws_iam_policy_document" "backup_assume_role" {
  statement {
    actions = [
      "sts:AssumeRole",
    ]

    principals {
      type = "Service"
      identifiers = [
        "backup.amazonaws.com",
      ]
    }
  }
}

resource "aws_iam_role" "backup" {
  name = "riju-backup"
  description = "Role used by AWS Backup for Riju"
  assume_role_policy = data.aws_iam_policy_document.backup_assume_role.json
}

data "aws_iam_policy" "backup" {
  name = "AWSBackupServiceRolePolicyForBackup"
}

data "aws_iam_policy" "backup_restores" {
  name = "AWSBackupServiceRolePolicyForRestores"
}

resource "aws_iam_role_policy_attachment" "backup" {
  role = aws_iam_role.backup.name
  policy_arn = data.aws_iam_policy.backup.arn
}

resource "aws_iam_role_policy_attachment" "backup_restores" {
  role = aws_iam_role.backup.name
  policy_arn = data.aws_iam_policy.backup_restores.arn
}
