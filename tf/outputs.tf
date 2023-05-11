output "alb_dns_name" {
  value = aws_lb.server.dns_name
}

output "deploy_aws_access_key_id" {
  value = aws_iam_access_key.deploy.id
}

output "deploy_aws_secret_access_key" {
  value     = aws_iam_access_key.deploy.secret
  sensitive = true
}
