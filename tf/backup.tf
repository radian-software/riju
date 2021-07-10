resource "aws_backup_vault" "riju" {
  name = "riju"
}

resource "aws_backup_plan" "riju" {
  name = "riju"

  rule {
    rule_name = "riju"
    target_vault_name = aws_backup_vault.riju.name
    schedule = "cron(0 5 ? * * *)"

    lifecycle {
      delete_after = 7
    }

    recovery_point_tags = {
      BillingCategory = "Riju"
    }
  }
}

resource "aws_backup_selection" "riju" {
  count = local.ssh_key_available ? 1 : 0

  iam_role_arn = aws_iam_role.backup.arn
  name = "riju"
  plan_id = aws_backup_plan.riju.id

  resources = [
    aws_instance.dev_server[0].arn,
  ]
}
