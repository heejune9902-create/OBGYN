source(file.path(if (file.exists("scripts/00_setup.R")) "." else "..", "scripts", "00_setup.R"), encoding = "UTF-8")

if (!exists("weekly_visits_df")) stop("01_prepare_cohort.R을 먼저 실행하세요.")

visit_times_df <- weekly_visits_df %>%
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 완료 시각
visits_with_previous_completion_df <- visit_times_df %>% arrange(complete_date.1) %>%
  group_by(application_date) %>%
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

visits_with_previous_completion_df %>% View()

visits_with_timestamps_df <- visits_with_previous_completion_df %>%
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

# 초진 소요시간의 기준 시각
visits_with_time_reference_df <- visits_with_timestamps_df %>%
  rowwise() %>%
  mutate(
    pre_complete.1 = ifelse(
    abs(difftime(complete_date.1, pre_pt_time, units = "secs")) <= abs(difftime(complete_date.1, complete_date.2, units = "secs")),
    pre_pt_time,
    complete_date.2
  )
) %>% ungroup()

visits_with_time_reference_df$pre_complete.1 <- as.POSIXct(visits_with_time_reference_df$pre_complete.1,tz="UTC")
visits_with_time_reference_df <- visits_with_time_reference_df %>% mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))
visits_with_time_reference_df %>% View()

# 보정 소요시간 계산
adjusted_visit_times_df <- visits_with_time_reference_df %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

adjusted_visit_times_df %>% View()
mytable(mobile_re~age+new_time_diff, data = adjusted_visit_times_df)

# 시간대별 진료량
visit_times_df <- adjusted_visit_times_df %>%
  mutate(year = year(application_time),
         month = month(application_time),
         day_of_week = wday(application_time, label = TRUE),
         day = day(application_time),
         hour = hour(application_time),
         week = isoweek(application_time),
         month_start = floor_date(application_date, unit = "month"))

hourly_counts_df <- visit_times_df %>% group_by(hour, application_date) %>% summarise(count = n(), .groups = "drop") %>% group_by(hour) %>% summarise(average_count = mean(count),.groups = "drop")

monthly_counts_df <- visit_times_df %>%
  group_by(year, month, day_of_week, day, hour, week, mobile_re) %>%
  summarise(count = n(), mean_time = mean(new_time_diff, na.rm = TRUE), .groups= "drop")

visit_times_df %>%
  group_by(day_of_week, hour) %>%
  summarise(count = n())

years <- unique(monthly_counts_df$year)
months <- unique(monthly_counts_df$month)

for (y in years) {
  for (m in months) {
    monthly_plot_df <- monthly_counts_df %>%
      filter(year == y, month == m) %>%
      arrange(week, day)

    if (nrow(monthly_plot_df) == 0) next

    visit_counts_plot <- ggplot(monthly_plot_df, aes(x = factor(hour), y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      facet_grid(week ~ day_of_week) +
      labs(title = paste("Hourly Data Count for", y, m),
           x = "Hour of the Day", y = "Count") +
      theme_minimal() +
      theme(strip.text.x = element_text(size = 8),
            strip.text.y = element_text(size = 8))

    print(visit_counts_plot)
  }
}

daily_counts_df <- visit_times_df %>%
  group_by(day_of_week, hour) %>%
  summarise(n_count = n(), .groups = 'drop')

mean_visit_counts_df <- monthly_counts_df %>%
  group_by(day_of_week, hour) %>%
  summarise(mean_n = round(mean(count, na.rm = TRUE)),
            sd_n = sd(count, na.rm = TRUE),
            n= n(),
            se_n = sd_n / sqrt(n),
            ci_lower = mean_n - qt(0.975, df = n-1) * se_n,
            ci_upper = mean_n + qt(0.975, df = n-1) * se_n,
            .groups = 'drop')

days_of_week <- unique(mean_visit_counts_df$day_of_week)

for (day in days_of_week) {
  weekday_plot_df <- mean_visit_counts_df %>% filter(day_of_week == day)

  visit_counts_plot <- ggplot(weekday_plot_df, aes(x = factor(hour), y = mean_n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_point(color = "blue", size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "blue") +
    geom_text(aes(label = mean_n, y = mean_n), hjust = -3, vjust = -0.5, size= 4, color = "black") +
    geom_text(aes(label = paste0("(", round(ci_lower, 1), ",", round(ci_upper, 1), ")"),
                  y = ci_upper), vjust = -0.5, color = "black") +
    labs(title = paste("Mean count", day),
         x = "Hour of Day", y = "Mean Counts") +
    theme_minimal() +
    theme(strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))

  print(visit_counts_plot)
}

weekly_visit_times_df <- visit_times_df %>% group_by(week_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE), .groups = "drop")

# 주별 평균 소요시간
ggplot(weekly_visit_times_df, aes(x = week_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
  geom_line()+
  geom_point() +
  labs(title = "초진 작성시간 (week)",
       x = "Date (week)",
       y = "Time (min)",
       color = "mobile") +
  scale_color_manual(values = c("1" = "orange", "0" = "blue"),
                     labels = c("0" = "모바일 문진 미시행", "1" = "모바일 문진 시행")) +
  theme_minimal()

monthly_visit_times_df <- visit_times_df %>% group_by(month_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE), .groups = "drop")

# 월별 평균 소요시간
ggplot(monthly_visit_times_df, aes(x = month_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
  geom_line()+
  geom_point() +
  labs(title = "초진 작성시간 (month)",
       x = "Date (month)",
       y = "Time (min)",
       color = "mobile") +
  scale_color_manual(values = c("1" = "orange", "0" = "blue"),
                     labels = c("0" = "모바일 문진 미시행", "1" = "모바일 문진 시행")) +
  theme_minimal()

weekly_visit_times_df <- visit_times_df %>% group_by(week_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")

weekly_outliers_df <- visit_times_df %>% left_join(weekly_visit_times_df, by = c("week_start", "mobile_re")) %>%  group_by(week_start, mobile_re) %>%
  summarize(count_out_ci = sum(new_time_diff < ci_lower | new_time_diff > ci_upper),
            total_count = n(),
            ratio_out = count_out_ci / total_count, .groups = "drop")

ggplot(weekly_outliers_df, aes(x = week_start, y = ratio_out, fill = factor(mobile_re)))+
  geom_bar(stat = "identity",position = position_dodge(width = 0.8), color = "black")

ggplot(weekly_visit_times_df, aes(x = week_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = mobile_re), alpha = 0.2) +
  geom_line(size = 1.2)+
  geom_point() +
  labs(title = "초진 작성시간 (week)",
       x = "week",
       y = "Time (min)",
       fill = 'mobile',
       color = "mobile") +
  scale_color_manual(values = c("1" = "orange", "0" = "blue"),
                     labels = c("0" = "모바일 문진 미시행", "1" = "모바일 문진 시행")) +

  scale_fill_manual(values = c('0' = "lightblue", '1' = "yellow"),
                    labels = c('0' = "모바일 문진 미시행", "1" = "모바일 문진 시행"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 평균 초진 소요시간

monthly_visit_times_df <- visit_times_df %>% group_by(month_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")

ggplot(monthly_visit_times_df, aes(x = month_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = mobile_re), alpha = 0.2) +
  geom_line(size = 1.2)+
  geom_point() +
  labs(title = "초진 작성시간 (Month)",
       x = "Month",
       y = "Time (min)",
       fill = 'mobile',
       color = "mobile") +
  scale_color_manual(values = c("1" = "orange", "0" = "blue"),
                     labels = c("0" = "모바일 문진 미시행", "1" = "모바일 문진 시행")) +
  scale_x_datetime(breaks = monthly_visit_times_df$month_start)+
  scale_fill_manual(values = c('0' = "lightblue", '1' = "yellow"),
                    labels = c('0' = "모바일 문진 미시행", "1" = "모바일 문진 시행"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
