source(file.path(if (file.exists("scripts/00_setup.R")) "." else "..", "scripts", "00_setup.R"), encoding = "UTF-8")

library(readr)
visits_raw_df <- readxl::read_excel(raw_file("산부인과-초진(240902).xlsx"))

standardized_visits_df <- visits_raw_df %>% rename(pt_id = `환자번호#1`,
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
standardized_visits_df %>% nrow() #16557
standardized_visits_df %>% summary()

standardized_visits_df <- standardized_visits_df %>% filter(!is.na(complete_date.2))
standardized_visits_df %>% nrow() #16549

# 18세 미만 제외
adult_visits_df <- standardized_visits_df[!grepl("개월", standardized_visits_df$age),]
adult_visits_df$age <- gsub('세', '', adult_visits_df$age)

adult_visits_df$age <- adult_visits_df$age %>% as.numeric()
adult_visits_df <- adult_visits_df[!is.na(adult_visits_df$age),]
adult_visits_df <- adult_visits_df %>% filter(age >= 18)

adult_visits_df %>% nrow() #16177


eligible_visits_df <- adult_visits_df
eligible_visits_df %>% nrow() #16177


eligible_visits_df$mobile %>% table()
eligible_visits_df[is.na(eligible_visits_df$mobile),] %>% View()

# 모바일 결측 제외

visits_with_mobile_status_df <- eligible_visits_df %>% filter(mobile != "")

visits_with_mobile_status_df$mobile[visits_with_mobile_status_df$application_date %in% c("2023-05-24", "2024-02-17") & visits_with_mobile_status_df$mobile == "2/1"] <- "2/2"
visits_with_mobile_status_df$mobile[visits_with_mobile_status_df$mobile == "2/1"] <- "2/0"

visits_with_mobile_status_df %>% nrow() # 15564

# 모바일 1/1, 2/2 -> 1, 1/0, 2/0 -> 0 ------------------------------------------

visits_with_mobile_status_df <- visits_with_mobile_status_df %>% mutate(mobile = ifelse(mobile %in% c("1/1", "2/2"), 1, 0))


# 오후 외래 오전 접수 제외
visits_with_valid_registration_df <- visits_with_mobile_status_df %>% filter(!(reservated_time > "12:00" & application_time < "12:00"))

visits_with_valid_registration_df %>% nrow() # 15273


#외국인 제거
domestic_visits_df <- visits_with_valid_registration_df %>% filter(nat_cd == "KR" )

domestic_visits_df %>% mutate(insurance_nm = ifelse(insurance_nm == "국제진료센터", insurance_nm, 0)) %>% mytable()
domestic_visits_df <- domestic_visits_df %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
domestic_visits_df$insurance_nm %>% unique()
domestic_visits_df$insurance_nm[is.na(domestic_visits_df$insurance_nm)] <- 0

domestic_visits_df$nat_cd %>% unique()

domestic_visits_df %>% nrow() # 11626

# 최종작성일시 결측 제거
## 모바일 문진 시행 조건 : 최종작성일시 <= 접수일시
domestic_visits_df <- domestic_visits_df %>%
  mutate(application_time = as.POSIXct(paste(application_date, application_time), tz = "UTC"))

domestic_visits_df %>% filter(!is.na(fin_doc_dt))
domestic_visits_df %>% filter(is.na(fin_doc_dt)) %>% dplyr::select(mobile) %>% unique()
domestic_visits_df %>% mutate(mobile_re = if_else(mobile == 1 & fin_doc_dt > application_time, 0 , mobile)) %>% dplyr::select(mobile_re) %>% table()
visits_with_exposure_df <- domestic_visits_df %>% mutate(mobile_re = if_else(mobile == 1 & fin_doc_dt >= application_time, 0 , mobile))

visits_with_exposure_df$mobile_re %>% table()


visits_with_previous_completion_df <- visits_with_exposure_df %>%
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
visits_with_previous_completion_df <- visits_with_previous_completion_df %>% arrange(complete_date.1) %>%
  group_by(application_date) %>%
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

visits_with_previous_completion_df <- visits_with_previous_completion_df %>%
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
visits_with_time_reference_df <- visits_with_previous_completion_df %>%
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

# 새로운 초진 시간 계산
visits_with_adjusted_time_df <- visits_with_time_reference_df %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

visits_with_adjusted_time_df %>% View()
mytable(mobile_re~age+new_time_diff, data = visits_with_adjusted_time_df)

# 초진 작성 60분 이상 제외

#visits_with_exposure_df <- visits_with_exposure_df[abs(difftime(visits_with_exposure_df$complete_date.2, visits_with_exposure_df$complete_date.1, units = "hours"))<1,]
analysis_visits_df <- visits_with_adjusted_time_df[abs(visits_with_adjusted_time_df$new_time_diff)<60,]
analysis_visits_df %>% nrow() # 11620

analysis_visits_df$mobile_re <- analysis_visits_df$mobile_re %>% as.factor()

analysis_visits_df %>% summary()

# 주 단위 (월요일 기준)
weekly_visits_df <- analysis_visits_df %>%
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

weekly_visits_df %>% summary()
weekly_visits_df %>% nrow() #11620

weekly_visit_counts_df <- weekly_visits_df %>% group_by(week_start) %>% summarise(count=n())%>% mutate(total = sum(count), percentage = count / total * 100)

ggplot(weekly_visit_counts_df, aes(x = week_start, y = count)) +
  geom_col() +
  #geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 10))

monthly_visit_counts_df <- weekly_visits_df %>% group_by(month) %>% summarise(count=n()) %>% mutate(total = sum(count), percentage = count / total * 100)

visit_counts_plot <- ggplot(monthly_visit_counts_df, aes(x = month, y = count)) +
  geom_line() +
  theme_minimal() +
  scale_x_datetime(breaks = monthly_visit_counts_df$month) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8))
visit_counts_plot

ggplot(monthly_visit_counts_df, aes(x = month, y = count)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5) +
  theme_minimal() +
  scale_x_datetime(breaks = monthly_visit_counts_df$month) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8))

# 세 그룹간 비교 ---------------------------------------------------------------
weekly_visits_df %>% colnames()

# 그룹 1: 완전 미시행 (mobile = 0)
no_questionnaire_df <- weekly_visits_df %>% filter(mobile == 0)
no_questionnaire_df %>% nrow() #448

# 그룹 2: 접수 후 모바일 완료 (mobile = 1 & fin_doc_dt >= application_time)
post_registration_questionnaire_df <- weekly_visits_df %>% filter(mobile == 1 & fin_doc_dt >= application_time)
post_registration_questionnaire_df %>% nrow() #3023

# 그룹 3: 시행 (mobile = 1 & fin_doc_dt < application_time)
pre_registration_questionnaire_df <- weekly_visits_df %>% filter(mobile == 1 & fin_doc_dt < application_time)
pre_registration_questionnaire_df %>% nrow() #8149


nrow(no_questionnaire_df) + nrow(post_registration_questionnaire_df) + nrow(pre_registration_questionnaire_df)
weekly_visits_df %>% nrow()

weekly_visits_df <- weekly_visits_df %>% mutate(group = case_when(
  mobile == 0 ~ 1,
  mobile == 1 & fin_doc_dt >= application_time ~ 2,
  mobile == 1 & fin_doc_dt < application_time ~ 3,
  TRUE ~ NA_integer_
))

weekly_visits_df %>%
  group_by(group) %>%
  summarise(
    average_age = mean(age, na.rm= TRUE),
    average_new_time_diff = mean(new_time_diff, na.rm = TRUE)
  )

no_vs_early_questionnaire_df <- subset(weekly_visits_df, group %in% c(1,3))
no_vs_late_questionnaire_df <- subset(weekly_visits_df, group %in% c(1,2))
late_vs_early_questionnaire_df <- subset(weekly_visits_df, group %in% c(2,3))
mytable(group~ age + new_time_diff, late_vs_early_questionnaire_df)
weekly_visits_df %>% summary()

library(car)
weekly_visits_df$group <- weekly_visits_df$group %>% as.factor()
leveneTest(weekly_visits_df$new_time_diff, weekly_visits_df$group)
aov(group~ age, data = weekly_visits_df)
aov(group~ new_time_diff, data = weekly_visits_df)

# 두 그룹 5분, 10분 이내 비율 --------------------------------------

weekly_visits_df %>%
  group_by(mobile_re) %>%
  summarise(
    n = n(),
    n_leq5 = sum(new_time_diff <= 5, na.rm = TRUE),
    n_leq10 = sum(new_time_diff <= 10, na.rm = TRUE),
    perc_leq5 = (n_leq5 / n) * 100,
    perc_leq10 = (n_leq10 / n) * 100
  )
