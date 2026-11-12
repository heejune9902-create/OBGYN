source("C:/Users/박희준/Desktop/2024 MSBOS_multi_center/scr/libraries.R")
library(readr)
ob_df_raw1 <- read_csv("data/산부인과-모바일220301-240531.csv")
ob_df_raw1 <- readxl::read_excel("data/산부인과-초진(240902).xlsx")
ob_df_raw2 <- readxl::read_excel("data/산부인과-초진-2110-2112.xlsx")

ob_df_raw <- ob_df_raw2 %>% rbind(ob_df_raw1)


ob_df0 <- ob_df_raw %>% rename(pt_id = `환자번호#1`,
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


ob_df0 %>% names()
ob_df0 <- ob_df0 %>% dplyr::select(-`진료과코드#31`, -'진료과코드#15', -'진료과명#32')

ob_df0 <- ob_df0 %>% filter(!is.na(complete_date.2))

ob_df0.1 <- ob_df0 %>% filter(application_date >= '2021-10-01' & application_date <= '2021-12-31')
ob_df0.2 <- ob_df0 %>% filter(application_date >= '2023-10-01' & application_date <= '2023-12-31')
ob_df0.1 %>% summary()
ob_df0.2 %>% summary()

# 산과 제외
exclusion_docter_list <- c("노정래", "오수영", "최석주", "성지희", "최윤선", "김예진","조혜정", "홍서연", "이유리", "함수지")
ob_df1.1 <- ob_df0.1 %>% filter(!(docter_name %in% exclusion_docter_list))
ob_df1.2 <- ob_df0.2 %>% filter(!(docter_name %in% exclusion_docter_list))
ob_df1.1$docter_name %>% unique()

# 18세 미만 제외
ob_df2.1 <- ob_df1.1[!grepl("개월", ob_df1.1$age),]
ob_df2.1$age <- gsub('세', '', ob_df2.1$age)

ob_df2.1$age <- ob_df2.1$age %>% as.numeric()
ob_df2.1 <- ob_df2.1[!is.na(ob_df2.1$age),]
ob_df2.1 <- ob_df2.1 %>% filter(age >= 18)

ob_df2.1 %>% nrow() #5504

ob_df2.2 <- ob_df1.2[!grepl("개월", ob_df1.2$age),]
ob_df2.2$age <- gsub('세', '', ob_df2.2$age)

ob_df2.2$age <- ob_df2.2$age %>% as.numeric()
ob_df2.2 <- ob_df2.2[!is.na(ob_df2.2$age),]
ob_df2.2 <- ob_df2.2 %>% filter(age >= 18)

ob_df2.2 %>% nrow() #3921


ob_df2$mobile %>% table()
ob_df2[is.na(ob_df2$mobile),] %>% View()


# 오후 외래 오전 접수 제외
ob_df4.1 <- ob_df2.1 %>% filter(!(reservated_time > "12:00" & application_time < "12:00"))
ob_df4.2 <- ob_df2.2 %>% filter(!(reservated_time > "12:00" & application_time < "12:00"))

ob_df4 %>% nrow() #14229


#외국인 제거

ob_df5.1 <- ob_df4.1 %>% filter(nat_cd == "KR" )
ob_df5.1 <- ob_df5.1 %>% 
  mutate(application_time = as.POSIXct(paste(application_date, application_time), tz = "UTC"))


ob_df5.1 %>% mutate(insurance_nm = ifelse(insurance_nm == "국제진료센터", insurance_nm, 0))
ob_df5.1 <- ob_df5.1 %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
ob_df5.1$insurance_nm %>% unique()
ob_df5.1$insurance_nm[is.na(ob_df5.1$insurance_nm)] <- 0

ob_df5.1$nat_cd %>% unique()

ob_df5.1 %>% nrow() #4107

#외국인 제거
ob_df5.2 <- ob_df4.2 %>% filter(nat_cd == "KR" )
ob_df5.2 <- ob_df5.2 %>% 
  mutate(application_time = as.POSIXct(paste(application_date, application_time), tz = "UTC"))


ob_df5.2 <- ob_df5.2 %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
ob_df5.2$insurance_nm %>% unique()
ob_df5.2$insurance_nm[is.na(ob_df5.2$insurance_nm)] <- 0

ob_df5.2$nat_cd %>% unique()

ob_df5.2 %>% nrow() #3584

weekly_data.1 <- ob_df5.1 %>% 
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

weekly_data.2 <- ob_df5.2 %>% 
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

mobile_time.1 <- weekly_data.1 %>% 
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

mobile_time.2 <- weekly_data.2 %>% 
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
mobile_time.1.1 <- mobile_time.1 %>% arrange(complete_date.1) %>% 
  group_by(application_date) %>% 
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
mobile_time.1.2 <- mobile_time.2 %>% arrange(complete_date.1) %>% 
  group_by(application_date) %>% 
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

mobile_time.2.1 <- mobile_time.1.1 %>% 
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

mobile_time.2.2 <- mobile_time.1.2 %>% 
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
mobile_time.3.1 <- mobile_time.2.1 %>% 
  rowwise() %>% 
  mutate(
    pre_complete.1 = ifelse(
      abs(difftime(complete_date.1, pre_pt_time, units = "secs")) <= abs(difftime(complete_date.1, complete_date.2, units = "secs")),
      pre_pt_time,
      complete_date.2
    )
  ) %>% ungroup() 

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
mobile_time.3.2 <- mobile_time.2.2 %>% 
  rowwise() %>% 
  mutate(
    pre_complete.1 = ifelse(
      abs(difftime(complete_date.1, pre_pt_time, units = "secs")) <= abs(difftime(complete_date.1, complete_date.2, units = "secs")),
      pre_pt_time,
      complete_date.2
    )
  ) %>% ungroup() 

mobile_time.3.1$pre_complete.1 <- as.POSIXct(mobile_time.3.1$pre_complete.1,tz="UTC")
mobile_time.3.1 <- mobile_time.3.1 %>% mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))
mobile_time.3.1 %>% View()

mobile_time.3.2$pre_complete.1 <- as.POSIXct(mobile_time.3.2$pre_complete.1,tz="UTC")
mobile_time.3.2 <- mobile_time.3.2 %>% mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))
mobile_time.3 %>% View()

# 새로운 초진 시간 계산
mobile_time.4.1 <- mobile_time.3.1 %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

# 새로운 초진 시간 계산
mobile_time.4.2 <- mobile_time.3.2 %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

# 초진 작성 60분 이상 제외

mobile_time.4.1 <- mobile_time.4.1[abs(mobile_time.4.1$new_time_diff)<60,] 
mobile_time.4.1 %>% nrow() #4077

mobile_time.4.1 %>% summary()
mobile_time.4.1 %>% View()
# 초진 작성 60분 이상 제외

mobile_time.4.2 <- mobile_time.4.2[abs(mobile_time.4.2$new_time_diff)<60,] 
mobile_time.4.2 %>% nrow() #3552

mobile_time.4.2 %>% summary()


ggplot(mobile_time.4.1, aes(x = new_time_diff))+
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black")+
  labs(title = "모바일 도입 전 초진 작성시간 분포", x = "초진 작성 시간", "환자 수")+
  theme_minimal()

ggplot(mobile_time.4.2, aes(x = new_time_diff))+
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black")+
  labs(title = "모바일 도입 후 초진 작성시간 분포", x = "초진 작성 시간", "환자 수")+
  theme_minimal()

# 시간대 별 환자 수
mobile_time.4.1$application_time <- mobile_time.4.1$application_time %>% as.POSIXct(tz = "UTC")
mobile_time.4.1 <- mobile_time.4.1 %>% 
  mutate(year = year(application_time),
         month = month(application_time),
         day_of_week = wday(application_time, label = TRUE),
         day = day(application_time),
         hour = hour(application_time),
         week = isoweek(application_time),
         month_start = floor_date(application_date, unit = "month"))

hourly_avg.1 <- mobile_time.4.1 %>% 
  group_by(hour) %>% 
  summarize(mean_new_time_diff = mean(new_time_diff,na.rm = TRUE),
            sd_new_time_diff = sd(new_time_diff, na.rm = TRUE),
            .groups = "drop"
            ) %>% 
  mutate(
    lower_sd = pmax(mean_new_time_diff - sd_new_time_diff, 0),
    upper_sd = mean_new_time_diff + sd_new_time_diff
  )

mobile_time.4.1 %>% filter(hour == 7) %>% View()

ggplot(hourly_avg.1, aes(x = hour, y = mean_new_time_diff)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_errorbar(aes(ymin = lower_sd, ymax = upper_sd), width = 0.3, color = "black") +
  geom_text(aes(label = paste0(round(mean_new_time_diff, 1), " (", round(sd_new_time_diff, 1), ")")),
            vjust= -0.5, color = "black", size = 3.5) +
  scale_x_continuous(breaks = unique(hourly_avg.1$hour))+
  labs(title = "모바일 도입 전 시간 별 평균 초진 작성 시간", x = "시간대", y = "평균 초진 작성 시간") +
  theme_minimal()

# 시간대 별 환자 수
mobile_time.4.2$application_date <- mobile_time.4.2$application_date %>% as.POSIXct()
mobile_time.4.2 <- mobile_time.4.2 %>% 
  mutate(year = year(application_time),
          month = month(application_time),
          day_of_week = wday(application_time, label = TRUE),
          day = day(application_time),
          hour = hour(application_time),
          week = isoweek(application_time),
          month_start = floor_date(application_date, unit = "month"))
  
mobile_time.4.2 %>% str()  

hourly_avg.2 <- mobile_time.4.2 %>% 
  group_by(hour) %>% 
  summarize(mean_new_time_diff = mean(new_time_diff,na.rm = TRUE),
            sd_new_time_diff = sd(new_time_diff, na.rm = TRUE),
            .groups = "drop"
  ) %>% 
  mutate(
    lower_sd = pmax(mean_new_time_diff - sd_new_time_diff, 0),
    upper_sd = mean_new_time_diff + sd_new_time_diff
  )

ggplot(hourly_avg.2, aes(x = hour, y = mean_new_time_diff)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_errorbar(aes(ymin = lower_sd, ymax = upper_sd), width = 0.3, color = "black") +
  geom_text(aes(label = paste0(round(mean_new_time_diff, 1), " (", round(sd_new_time_diff, 1), ")")),
            vjust= -0.5, color = "black", size = 3.5) +
  scale_x_continuous(breaks = unique(hourly_avg.1$hour))+
  labs(title = "모바일 도입 후 시간 별 평균 초진 작성 시간", x = "시간대", y = "평균 초진 작성 시간") +
  theme_minimal()

mobile_time.4.1$group = 1
mobile_time.4.2$group = 2

mt4 <- mobile_time.4.1 %>% rbind(mobile_time.4.2)
mt4$group <- mt4$group %>% as.factor()

mytable(group~age + new_time_diff, data= mt4)
?mytable()


mobile_time_w <- mt4 %>% group_by(week_start, group) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")

mobile_time_w <- mobile_time_w %>% group_by(group) %>% mutate(week_num = row_number()) %>% ungroup()

# 주 단위 그래프
ggplot(mobile_time_w, aes(x = week_num, y= mean_time, color = as.factor(group), group = as.factor(group))) +
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
  #scale_x_datetime(breaks = mobile_time_w$week_num)+
  scale_fill_manual(values = c('1' = "lightblue", '2' = "yellow"),
                    labels = c('1' = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 평균 초진 시간 그래프


mobile_time_m <- mt4 %>% group_by(month_start, group) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")

mobile_time_m <- mobile_time_m %>% group_by(group) %>% mutate(week_num = row_number()) %>% ungroup()


# 월 단위 그래프
ggplot(mobile_time_m, aes(x = month_start, y= mean_time, color = as.factor(group), group = as.factor(group))) +
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
  scale_x_datetime(breaks = mobile_time_m$month_start)+
  scale_fill_manual(values = c('1' = "lightblue", '2' = "yellow"),
                    labels = c('1' = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



mt4 %>% View()
mobile_time_d <- mt4 %>% group_by(day_of_week, group) %>%
  summarise(mean_time = mean(new_time_diff, na.rm = TRUE),
            se = sd(new_time_diff) / sqrt(n()),
            ci_lower = mean_time - 1.96 *se,
            ci_upper = mean_time + 1.96* se, .groups = "drop")



# 일 단위 그래프
ggplot(mobile_time_d, aes(x = day_of_week, y= mean_time, color = as.factor(group), group = as.factor(group))) +
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
  #scale_x_datetime(breaks = mobile_time_m$month_start)+
  scale_fill_manual(values = c('1' = "lightblue", '2' = "yellow"),
                    labels = c('1' = "모바일 문진 도입 전", "2" = "모바일 문진 도입 후"))+
  theme_minimal() 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 주 단위 (월요일 기준)
weekly_data <- ob_df6 %>% 
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))
weekly_data$mobile <- weekly_data$mobile %>% as.factor()

weekly_data %>% summary()



# weekly_data <- mapping_1 %>% dplyr::select(-mapping)

# save(ob_df_raw,ob_df0,ob_df5,weekly_data, file = "C:/Users/박희준/Desktop/산부인과/data/ob_data.RData")
