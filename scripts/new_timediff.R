
mobile_time <- weekly_data %>% 
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
mobile_time.1 <- mobile_time %>% arrange(complete_date.1) %>% 
  group_by(application_date) %>% 
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

mobile_time.1 %>% View()

mobile_time.2 <- mobile_time.1 %>% 
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
mobile_time.3 <- mobile_time.2 %>% 
  rowwise() %>% 
  mutate(
    pre_complete.1 = ifelse(
    abs(difftime(complete_date.1, pre_pt_time, units = "secs")) <= abs(difftime(complete_date.1, complete_date.2, units = "secs")),
    pre_pt_time,
    complete_date.2
  )
) %>% ungroup() 

mobile_time.3$pre_complete.1 <- as.POSIXct(mobile_time.3$pre_complete.1,tz="UTC")
mobile_time.3 <- mobile_time.3 %>% mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))
mobile_time.3 %>% View()

# 새로운 초진 시간 계산
mobile_time.4 <- mobile_time.3 %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

mobile_time.4 %>% View()
mytable(mobile_re~age+new_time_diff, data = mobile_time)


# 시간대 별 환자 수
mobile_time <- mobile_time %>% 
  mutate(year = year(application_time),
         month = month(application_time),
         day_of_week = wday(application_time, label = TRUE),
         day = day(application_time),
         hour = hour(application_time),
         week = isoweek(application_time),
         month_start = floor_date(application_date, unit = "month"))

hourly_counts <- mobile_time %>% group_by(hour, application_date) %>% summarise(count = n(), .groups = "drop") %>% group_by(hour) %>% summarise(average_count = mean(count),.groups = "drop")

monthly_counts <- mobile_time %>% 
  group_by(year, month, day_of_week, day, hour, week, mobile_re) %>% 
  summarise(count = n(), mean_time = mean(new_time_diff, na.rm = TRUE), .groups= "drop")

mobile_time %>%
  group_by(day_of_week, hour) %>% 
  summarise(count = n())


years <- unique(monthly_counts$year)
months <- unique(monthly_counts$month)


for (y in years) {
  for (m in months) {
    monthly_data <- monthly_counts %>% 
      filter(year == y, month == m) %>% 
      arrange(week, day)
    
    if (nrow(monthly_data) == 0) next
    
    p <- ggplot(monthly_data, aes(x = factor(hour), y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      facet_grid(week ~ day_of_week) +
      labs(title = paste("Hourly Data Count for", y, m),
           x = "Hour of the Day", y = "Count") +
      theme_minimal() +
      theme(strip.text.x = element_text(size = 8),
            strip.text.y = element_text(size = 8))
    
    print(p)
  }
}

daily_counts <- mobile_time %>% 
  group_by(day_of_week, hour) %>% 
  summarise(n_count = n(), .groups = 'drop')

mean_time_data <- monthly_counts %>% 
  group_by(day_of_week, hour) %>% 
  summarise(mean_n = round(mean(count, na.rm = TRUE)),
            sd_n = sd(count, na.rm = TRUE),
            n= n(),
            se_n = sd_n / sqrt(n),
            ci_lower = mean_n - qt(0.975, df = n-1) * se_n,
            ci_upper = mean_n + qt(0.975, df = n-1) * se_n,
            .groups = 'drop')

days_of_week <- unique(mean_time_data$day_of_week)

for (day in days_of_week) {
  day_data <- mean_time_data %>% filter(day_of_week == day)
  
  p <- ggplot(day_data, aes(x = factor(hour), y = mean_n)) +
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
  
  print(p)
}

mobile_time_w <- mobile_time %>% group_by(week_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE), .groups = "drop")

# 주 단위 그래프
ggplot(mobile_time_w, aes(x = week_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
  geom_line()+
  geom_point() +
  labs(title = "초진 작성시간 (week)",
       x = "Date (week)",
       y = "Time (min)",
       color = "mobile") +
  scale_color_manual(values = c("1" = "orange", "0" = "blue"),
                     labels = c("0" = "모바일 문진 미시행", "1" = "모바일 문진 시행")) +
  theme_minimal()


mobile_time_m <- mobile_time %>% group_by(month_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE), .groups = "drop")

# 월 단위 그래프
ggplot(mobile_time_m, aes(x = month_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
  geom_line()+
  geom_point() +
  labs(title = "초진 작성시간 (month)",
       x = "Date (month)",
       y = "Time (min)",
       color = "mobile") +
  scale_color_manual(values = c("1" = "orange", "0" = "blue"),
                     labels = c("0" = "모바일 문진 미시행", "1" = "모바일 문진 시행")) +
  theme_minimal()


mobile_time_w <- mobile_time %>% group_by(week_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")

out_of_ci <- mobile_time %>% left_join(mobile_time_w, by = c("week_start", "mobile_re")) %>%  group_by(week_start, mobile_re) %>% 
  summarize(count_out_ci = sum(new_time_diff < ci_lower | new_time_diff > ci_upper),
            total_count = n(),
            ratio_out = count_out_ci / total_count, .groups = "drop")

ggplot(out_of_ci, aes(x = week_start, y = ratio_out, fill = factor(mobile_re)))+
  geom_bar(stat = "identity",position = position_dodge(width = 0.8), color = "black") 

# 월 단위 그래프
ggplot(mobile_time_w, aes(x = week_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
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
  #scale_x_datetime(breaks = mobile_time_w$week_start)+
  scale_fill_manual(values = c('0' = "lightblue", '1' = "yellow"),
                    labels = c('0' = "모바일 문진 미시행", "1" = "모바일 문진 시행"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 평균 초진 시간 그래프


mobile_time_m <- mobile_time %>% group_by(month_start, mobile_re) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")



# 월 단위 그래프
ggplot(mobile_time_m, aes(x = month_start, y= mean_time, color = as.factor(mobile_re), group = mobile_re)) +
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
  scale_x_datetime(breaks = mobile_time_m$month_start)+
  scale_fill_manual(values = c('0' = "lightblue", '1' = "yellow"),
                    labels = c('0' = "모바일 문진 미시행", "1" = "모바일 문진 시행"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

