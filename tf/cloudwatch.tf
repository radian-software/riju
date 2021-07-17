resource "aws_cloudwatch_metric_alarm" "server_cpu" {
  count = local.ami_available ? 1 : 0

  alarm_name                = "riju-server-cpu-high"
  comparison_operator       = "GreaterThanOrEqualToThreshold"
  evaluation_periods        = "30"
  metric_name               = "cpu_usage_active"
  namespace                 = "CWAgent"
  period                    = "60"
  statistic                 = "Maximum"
  threshold                 = "90"
  alarm_description         = "CPU usage on Riju server is above 90% for 30 minutes"
  ok_actions                = [aws_sns_topic.riju.arn]
  alarm_actions             = [aws_sns_topic.riju.arn]
  insufficient_data_actions = [aws_sns_topic.riju.arn]
  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.server[count.index].name
  }

  tags = {
    BillingSubcategory = "Riju:CloudWatch:Alarm"
  }
}

resource "aws_cloudwatch_metric_alarm" "server_memory" {
  count = local.ami_available ? 1 : 0

  alarm_name                = "riju-server-memory-high"
  comparison_operator       = "GreaterThanOrEqualToThreshold"
  evaluation_periods        = "30"
  metric_name               = "mem_used_percent"
  namespace                 = "CWAgent"
  period                    = "60"
  statistic                 = "Maximum"
  threshold                 = "80"
  alarm_description         = "Memory usage on Riju server is above 80% for 30 minutes"
  ok_actions                = [aws_sns_topic.riju.arn]
  alarm_actions             = [aws_sns_topic.riju.arn]
  insufficient_data_actions = [aws_sns_topic.riju.arn]
  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.server[count.index].name
  }

  tags = {
    BillingSubcategory = "Riju:CloudWatch:Alarm"
  }
}

resource "aws_cloudwatch_metric_alarm" "server_data_volume_disk_space" {
  count = local.ami_available ? 1 : 0

  alarm_name                = "riju-server-data-volume-disk-usage-high"
  comparison_operator       = "GreaterThanOrEqualToThreshold"
  evaluation_periods        = "30"
  metric_name               = "disk_used_percent"
  namespace                 = "CWAgent"
  period                    = "60"
  statistic                 = "Maximum"
  threshold                 = "90"
  alarm_description         = "Disk space usage for data volume on Riju server is above 90% for 30 minutes"
  ok_actions                = [aws_sns_topic.riju.arn]
  alarm_actions             = [aws_sns_topic.riju.arn]
  insufficient_data_actions = [aws_sns_topic.riju.arn]
  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.server[count.index].name
    path                 = "/mnt/riju/data"
  }

  tags = {
    BillingSubcategory = "Riju:CloudWatch:Alarm"
  }
}

resource "aws_cloudwatch_metric_alarm" "server_root_volume_disk_space" {
  count = local.ami_available ? 1 : 0

  alarm_name                = "riju-server-root-volume-disk-usage-high"
  comparison_operator       = "GreaterThanOrEqualToThreshold"
  evaluation_periods        = "30"
  metric_name               = "disk_used_percent"
  namespace                 = "CWAgent"
  period                    = "60"
  statistic                 = "Maximum"
  threshold                 = "90"
  alarm_description         = "Disk space usage for root volume on Riju server is above 90% for 30 minutes"
  ok_actions                = [aws_sns_topic.riju.arn]
  alarm_actions             = [aws_sns_topic.riju.arn]
  insufficient_data_actions = [aws_sns_topic.riju.arn]
  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.server[count.index].name
    path                 = "/"
  }

  tags = {
    BillingSubcategory = "Riju:CloudWatch:Alarm"
  }
}

resource "aws_cloudwatch_dashboard" "riju" {
  count = local.ami_available ? 1 : 0

  dashboard_name = "Riju"
  dashboard_body = <<EOF
{
    "widgets": [
        {
            "type": "metric",
            "x": 0,
            "y": 0,
            "width": 6,
            "height": 6,
            "properties": {
                "title": "CPU",
                "annotations": {
                    "alarms": [
                        "${aws_cloudwatch_metric_alarm.server_cpu[count.index].arn}"
                    ]
                },
                "view": "timeSeries",
                "stacked": false
            }
        },
        {
            "type": "metric",
            "x": 12,
            "y": 0,
            "width": 6,
            "height": 6,
            "properties": {
                "title": "Root volume disk space",
                "annotations": {
                    "alarms": [
                        "${aws_cloudwatch_metric_alarm.server_root_volume_disk_space[count.index].arn}"
                    ]
                },
                "view": "timeSeries",
                "stacked": false,
                "type": "chart"
            }
        },
        {
            "type": "metric",
            "x": 18,
            "y": 0,
            "width": 6,
            "height": 6,
            "properties": {
                "title": "Data volume disk space",
                "annotations": {
                    "alarms": [
                        "${aws_cloudwatch_metric_alarm.server_data_volume_disk_space[count.index].arn}"
                    ]
                },
                "view": "timeSeries",
                "stacked": false,
                "type": "chart"
            }
        },
        {
            "type": "metric",
            "x": 6,
            "y": 0,
            "width": 6,
            "height": 6,
            "properties": {
                "title": "Memory",
                "annotations": {
                    "alarms": [
                        "${aws_cloudwatch_metric_alarm.server_memory[count.index].arn}"
                    ]
                },
                "view": "timeSeries",
                "stacked": false,
                "type": "chart"
            }
        }
    ]
}
EOF
}
