resource "aws_s3_bucket" "riju" {
  bucket = data.external.env.result.S3_BUCKET
}

resource "aws_s3_bucket_public_access_block" "riju" {
  bucket = aws_s3_bucket.riju.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_s3_bucket_policy" "riju" {
  bucket = aws_s3_bucket.riju.id
  policy = data.aws_iam_policy_document.riju.json
}
