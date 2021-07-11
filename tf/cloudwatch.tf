resource "aws_cloudwatch_metric_alarm" "server_memory" {
  count = local.ami_available ? 1 : 0

  alarm_name = "riju-server-memory-high"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods = "5"
  metric_name = "mem_used_percent"
  namespace = "CWAgent"
  period = "60"
  statistic = "Maximum"
  threshold = "80"
  alarm_description = "Memory usage on Riju server is above 80%"
  ok_actions = [aws_sns_topic.riju.arn]
  alarm_actions = [aws_sns_topic.riju.arn]
  insufficient_data_actions = [aws_sns_topic.riju.arn]
  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.server[count.index].name
  }
}

resource "aws_cloudwatch_metric_alarm" "server_data_volume_disk_space" {
  count = local.ami_available ? 1 : 0

  alarm_name = "riju-server-data-volume-disk-usage-high"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods = "5"
  metric_name = "disk_used_percent"
  namespace = "CWAgent"
  period = "60"
  statistic = "Minimum"
  threshold = "80"
  alarm_description = "Disk space usage for data volume on Riju server is above 80%"
  ok_actions = [aws_sns_topic.riju.arn]
  alarm_actions = [aws_sns_topic.riju.arn]
  insufficient_data_actions = [aws_sns_topic.riju.arn]
  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.server[count.index].name
    path = "/mnt/riju/data"
  }
}

resource "aws_cloudwatch_metric_alarm" "server_root_volume_disk_space" {
  count = local.ami_available ? 1 : 0

  alarm_name = "riju-server-root-volume-disk-usage-high"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods = "5"
  metric_name = "disk_used_percent"
  namespace = "CWAgent"
  period = "60"
  statistic = "Minimum"
  threshold = "80"
  alarm_description = "Disk space usage for root volume on Riju server is above 80%"
  ok_actions = [aws_sns_topic.riju.arn]
  alarm_actions = [aws_sns_topic.riju.arn]
  insufficient_data_actions = [aws_sns_topic.riju.arn]
  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.server[count.index].name
    path = "/"
  }
}
