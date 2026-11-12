source(file.path(if (file.exists("scripts/00_setup.R")) "." else "..", "scripts", "00_setup.R"), encoding = "UTF-8")

library(readr)
post_introduction_raw_df <- readxl::read_excel(raw_file("산부인과-초진(240902).xlsx"))
pre_introduction_raw_df <- readxl::read_excel(raw_file("산부인과-초진-2110-2112.xlsx"))

combined_visits_df <- pre_introduction_raw_df %>% rbind(post_introduction_raw_df)


standardized_visits_df <- combined_visits_df %>% rename(pt_id = `환자번호#1`,
                               application_date =`접수일자#2`,
                               dept_code = `외래존코드#3`,
                               place_code = `경로코드#4`,
                               place.1 = `경로#5`,
                               reservated_time = `예약시간#6`,
                               application_time = `외래접수#7`,
                               reservation_num = `예약번호#8`,
                               sex = `성별#9`,
                               age = `나이(접수일기준)#10`,
                               mobile = `모바일#11`,
                               complete = `완료#12`,
                               complete_date.1 = `완료일시#13`,
                               note = `특기사항#14`,
                               docter_name = "진료의명#17",
                               place.2 = '경로#20',
                               complete_date.2 = '완료일시#21',
                               nat_cd = '국적코드#22',
                               nat_nm = '국적명#23',
                               insurance_cd = '보험유형코드#24',
                               insurance_nm = '보험유형명#25',
                               treat_dt = '진료일자#26',
                               internat_treat = '국제진료여부#27',
                               doc_status_cd = '작성상태코드#28',
                               doc_status = '작성상태명#29',
                               fin_doc_dt = '최종작성일시#30')


standardized_visits_df %>% names()
standardized_visits_df <- standardized_visits_df %>% dplyr::select(-`진료과코드#31`, -'진료과코드#15', -'진료과명#32')

standardized_visits_df <- standardized_visits_df %>% filter(!is.na(complete_date.2))

before_standardized_visits_df <- standardized_visits_df %>% filter(application_date >= '2021-10-01' & application_date <= '2021-12-31')
after_standardized_visits_df <- standardized_visits_df %>% filter(application_date >= '2023-10-01' & application_date <= '2023-12-31')
before_standardized_visits_df %>% summary()
after_standardized_visits_df %>% summary()

# 산과 제외
excluded_physicians <- c("노정래", "오수영", "최석주", "성지희", "최윤선", "김예진","조혜정", "홍서연", "이유리", "함수지")
before_gynecology_visits_df <- before_standardized_visits_df %>% filter(!(docter_name %in% excluded_physicians))
after_gynecology_visits_df <- after_standardized_visits_df %>% filter(!(docter_name %in% excluded_physicians))
before_gynecology_visits_df$docter_name %>% unique()

# 18세 미만 제외
before_adult_visits_df <- before_gynecology_visits_df[!grepl("개월", before_gynecology_visits_df$age),]
before_adult_visits_df$age <- gsub('세', '', before_adult_visits_df$age)

before_adult_visits_df$age <- before_adult_visits_df$age %>% as.numeric()
before_adult_visits_df <- before_adult_visits_df[!is.na(before_adult_visits_df$age),]
before_adult_visits_df <- before_adult_visits_df %>% filter(age >= 18)

before_adult_visits_df %>% nrow() #5504

after_adult_visits_df <- after_gynecology_visits_df[!grepl("개월", after_gynecology_visits_df$age),]
after_adult_visits_df$age <- gsub('세', '', after_adult_visits_df$age)

after_adult_visits_df$age <- after_adult_visits_df$age %>% as.numeric()
after_adult_visits_df <- after_adult_visits_df[!is.na(after_adult_visits_df$age),]
after_adult_visits_df <- after_adult_visits_df %>% filter(age >= 18)

after_adult_visits_df %>% nrow() #3921


# 오후 외래 오전 접수 제외
before_valid_registration_visits_df <- before_adult_visits_df %>% filter(!(reservated_time > "12:00" & application_time < "12:00"))
after_valid_registration_visits_df <- after_adult_visits_df %>% filter(!(reservated_time > "12:00" & application_time < "12:00"))


#외국인 제거

before_eligible_visits_df <- before_valid_registration_visits_df %>% filter(nat_cd == "KR" )
before_eligible_visits_df <- before_eligible_visits_df %>%
  mutate(application_time = as.POSIXct(paste(application_date, application_time), tz = "UTC"))


before_eligible_visits_df %>% mutate(insurance_nm = ifelse(insurance_nm == "국제진료센터", insurance_nm, 0))
before_eligible_visits_df <- before_eligible_visits_df %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
before_eligible_visits_df$insurance_nm %>% unique()
before_eligible_visits_df$insurance_nm[is.na(before_eligible_visits_df$insurance_nm)] <- 0

before_eligible_visits_df$nat_cd %>% unique()

before_eligible_visits_df %>% nrow() #4107

#외국인 제거
after_eligible_visits_df <- after_valid_registration_visits_df %>% filter(nat_cd == "KR" )
after_eligible_visits_df <- after_eligible_visits_df %>%
  mutate(application_time = as.POSIXct(paste(application_date, application_time), tz = "UTC"))


after_eligible_visits_df <- after_eligible_visits_df %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
after_eligible_visits_df$insurance_nm %>% unique()
after_eligible_visits_df$insurance_nm[is.na(after_eligible_visits_df$insurance_nm)] <- 0

after_eligible_visits_df$nat_cd %>% unique()

after_eligible_visits_df %>% nrow() #3584

before_weekly_visits_df <- before_eligible_visits_df %>%
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

after_weekly_visits_df <- after_eligible_visits_df %>%
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

before_visit_times_df <- before_weekly_visits_df %>%
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

after_visit_times_df <- after_weekly_visits_df %>%
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
before_visits_with_previous_completion_df <- before_visit_times_df %>% arrange(complete_date.1) %>%
  group_by(application_date) %>%
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
after_visits_with_previous_completion_df <- after_visit_times_df %>% arrange(complete_date.1) %>%
  group_by(application_date) %>%
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

before_visits_with_timestamps_df <- before_visits_with_previous_completion_df %>%
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

after_visits_with_timestamps_df <- after_visits_with_previous_completion_df %>%
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
before_visits_with_time_reference_df <- before_visits_with_timestamps_df %>%
  rowwise() %>%
  mutate(
    pre_complete.1 = ifelse(
      abs(difftime(complete_date.1, pre_pt_time, units = "secs")) <= abs(difftime(complete_date.1, complete_date.2, units = "secs")),
      pre_pt_time,
      complete_date.2
    )
  ) %>% ungroup()

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
after_visits_with_time_reference_df <- after_visits_with_timestamps_df %>%
  rowwise() %>%
  mutate(
    pre_complete.1 = ifelse(
      abs(difftime(complete_date.1, pre_pt_time, units = "secs")) <= abs(difftime(complete_date.1, complete_date.2, units = "secs")),
      pre_pt_time,
      complete_date.2
    )
  ) %>% ungroup()

before_visits_with_time_reference_df$pre_complete.1 <- as.POSIXct(before_visits_with_time_reference_df$pre_complete.1,tz="UTC")
before_visits_with_time_reference_df <- before_visits_with_time_reference_df %>% mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))
before_visits_with_time_reference_df %>% View()

after_visits_with_time_reference_df$pre_complete.1 <- as.POSIXct(after_visits_with_time_reference_df$pre_complete.1,tz="UTC")
after_visits_with_time_reference_df <- after_visits_with_time_reference_df %>% mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))
mobile_time.3 %>% View()

# 새로운 초진 시간 계산
before_visits_with_adjusted_time_df <- before_visits_with_time_reference_df %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

# 새로운 초진 시간 계산
after_visits_with_adjusted_time_df <- after_visits_with_time_reference_df %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

# 초진 작성 60분 이상 제외

before_visits_with_adjusted_time_df <- before_visits_with_adjusted_time_df[abs(before_visits_with_adjusted_time_df$new_time_diff)<60,]
before_visits_with_adjusted_time_df %>% nrow() #4077

before_visits_with_adjusted_time_df %>% summary()
before_visits_with_adjusted_time_df %>% View()
# 초진 작성 60분 이상 제외

after_visits_with_adjusted_time_df <- after_visits_with_adjusted_time_df[abs(after_visits_with_adjusted_time_df$new_time_diff)<60,]
after_visits_with_adjusted_time_df %>% nrow() #3552

after_visits_with_adjusted_time_df %>% summary()


ggplot(before_visits_with_adjusted_time_df, aes(x = new_time_diff))+
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black")+
  labs(title = "모바일 도입 전 초진 작성시간 분포", x = "초진 작성 시간", "환자 수")+
  theme_minimal()

ggplot(after_visits_with_adjusted_time_df, aes(x = new_time_diff))+
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black")+
  labs(title = "모바일 도입 후 초진 작성시간 분포", x = "초진 작성 시간", "환자 수")+
  theme_minimal()

# 시간대 별 환자 수
before_visits_with_adjusted_time_df$application_time <- before_visits_with_adjusted_time_df$application_time %>% as.POSIXct(tz = "UTC")
before_visits_with_adjusted_time_df <- before_visits_with_adjusted_time_df %>%
  mutate(year = year(application_time),
         month = month(application_time),
         day_of_week = wday(application_time, label = TRUE),
         day = day(application_time),
         hour = hour(application_time),
         week = isoweek(application_time),
         month_start = floor_date(application_date, unit = "month"))

before_hourly_summary_df <- before_visits_with_adjusted_time_df %>%
  group_by(hour) %>%
  summarize(mean_new_time_diff = mean(new_time_diff,na.rm = TRUE),
            sd_new_time_diff = sd(new_time_diff, na.rm = TRUE),
            .groups = "drop"
            ) %>%
  mutate(
    lower_sd = pmax(mean_new_time_diff - sd_new_time_diff, 0),
    upper_sd = mean_new_time_diff + sd_new_time_diff
  )

before_visits_with_adjusted_time_df %>% filter(hour == 7) %>% View()

ggplot(before_hourly_summary_df, aes(x = hour, y = mean_new_time_diff)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_errorbar(aes(ymin = lower_sd, ymax = upper_sd), width = 0.3, color = "black") +
  geom_text(aes(label = paste0(round(mean_new_time_diff, 1), " (", round(sd_new_time_diff, 1), ")")),
            vjust= -0.5, color = "black", size = 3.5) +
  scale_x_continuous(breaks = unique(before_hourly_summary_df$hour))+
  labs(title = "모바일 도입 전 시간 별 평균 초진 작성 시간", x = "시간대", y = "평균 초진 작성 시간") +
  theme_minimal()

# 시간대 별 환자 수
after_visits_with_adjusted_time_df$application_date <- after_visits_with_adjusted_time_df$application_date %>% as.POSIXct()
after_visits_with_adjusted_time_df <- after_visits_with_adjusted_time_df %>%
  mutate(year = year(application_time),
          month = month(application_time),
          day_of_week = wday(application_time, label = TRUE),
          day = day(application_time),
          hour = hour(application_time),
          week = isoweek(application_time),
          month_start = floor_date(application_date, unit = "month"))

after_visits_with_adjusted_time_df %>% str()

after_hourly_summary_df <- after_visits_with_adjusted_time_df %>%
  group_by(hour) %>%
  summarize(mean_new_time_diff = mean(new_time_diff,na.rm = TRUE),
            sd_new_time_diff = sd(new_time_diff, na.rm = TRUE),
            .groups = "drop"
  ) %>%
  mutate(
    lower_sd = pmax(mean_new_time_diff - sd_new_time_diff, 0),
    upper_sd = mean_new_time_diff + sd_new_time_diff
  )

ggplot(after_hourly_summary_df, aes(x = hour, y = mean_new_time_diff)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_errorbar(aes(ymin = lower_sd, ymax = upper_sd), width = 0.3, color = "black") +
  geom_text(aes(label = paste0(round(mean_new_time_diff, 1), " (", round(sd_new_time_diff, 1), ")")),
            vjust= -0.5, color = "black", size = 3.5) +
  scale_x_continuous(breaks = unique(before_hourly_summary_df$hour))+
  labs(title = "모바일 도입 후 시간 별 평균 초진 작성 시간", x = "시간대", y = "평균 초진 작성 시간") +
  theme_minimal()

before_visits_with_adjusted_time_df$group = 1
after_visits_with_adjusted_time_df$group = 2

combined_adjusted_times_df <- before_visits_with_adjusted_time_df %>% rbind(after_visits_with_adjusted_time_df)
combined_adjusted_times_df$group <- combined_adjusted_times_df$group %>% as.factor()

mytable(group~age + new_time_diff, data= combined_adjusted_times_df)
?mytable()


weekly_visit_times_df <- combined_adjusted_times_df %>% group_by(week_start, group) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")

weekly_visit_times_df <- weekly_visit_times_df %>% group_by(group) %>% mutate(week_num = row_number()) %>% ungroup()

# 주 단위 그래프
ggplot(weekly_visit_times_df, aes(x = week_num, y= mean_time, color = as.factor(group), group = as.factor(group))) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = group), alpha = 0.2) +
  geom_line(size = 1.2)+
  geom_point() +
  labs(title = "초진 작성시간 (Week)",
       x = "week",
       y = "Time (min)",
       fill = 'mobile',
       color = "mobile") +
  scale_color_manual(values = c("2" = "orange", "1" = "blue"),
                     labels = c("1" = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후")) +
  #scale_x_datetime(breaks = weekly_visit_times_df$week_num)+
  scale_fill_manual(values = c('1' = "lightblue", '2' = "yellow"),
                    labels = c('1' = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 평균 초진 시간 그래프


monthly_visit_times_df <- combined_adjusted_times_df %>% group_by(month_start, group) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")

monthly_visit_times_df <- monthly_visit_times_df %>% group_by(group) %>% mutate(week_num = row_number()) %>% ungroup()


# 월 단위 그래프
ggplot(monthly_visit_times_df, aes(x = month_start, y= mean_time, color = as.factor(group), group = as.factor(group))) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = group), alpha = 0.2) +
  geom_line(size = 1.2)+
  geom_point() +
  labs(title = "초진 작성시간 (Month)",
       x = "Month",
       y = "Time (min)",
       fill = 'mobile',
       color = "mobile") +
  scale_color_manual(values = c("2" = "orange", "1" = "blue"),
                     labels = c("1" = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후")) +
  scale_x_datetime(breaks = monthly_visit_times_df$month_start)+
  scale_fill_manual(values = c('1' = "lightblue", '2' = "yellow"),
                    labels = c('1' = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


combined_adjusted_times_df %>% View()
daily_visit_times_df <- combined_adjusted_times_df %>% group_by(day_of_week, group) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")


# 일 단위 그래프
ggplot(daily_visit_times_df, aes(x = day_of_week, y= mean_time, color = as.factor(group), group = as.factor(group))) +
  geom_line(size= 1.2) +
  geom_point()+
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = group), alpha = 0.2) +
  labs(title = "초진 작성시간 (day)",
       x = "day",
       y = "Time (min)",
       fill = 'mobile',
       color = "mobile") +
  scale_color_manual(values = c("2" = "orange", "1" = "blue"),
                     labels = c("1" = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후")) +
  #scale_x_datetime(breaks = monthly_visit_times_df$month_start)+
  scale_fill_manual(values = c('1' = "lightblue", '2' = "yellow"),
                    labels = c('1' = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후"))+
  theme_minimal()
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
