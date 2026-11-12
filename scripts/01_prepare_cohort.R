source(file.path(if (file.exists("scripts/00_setup.R")) "." else "..", "scripts", "00_setup.R"), encoding = "UTF-8")

library(readr)

visits_before_introduction_df <- read.csv(raw_file("산부인과-초진-202205-202209.csv"), check.names = FALSE)
visits_before_introduction_df <- visits_before_introduction_df %>%
  filter(visits_before_introduction_df$`접수일자#2` <'2022-09-01')
visits_before_introduction_df <- visits_before_introduction_df %>%
  mutate(across(c("진료일자#26","국제진료여부#27",
                  "작성상태코드#28","작성상태명#29","최종작성일시#30","진료과코드#31",
                  "진료과명#32"), ~ NA))

visits_before_introduction_df %>% colnames()
visits_before_introduction_df$`접수일자#2` %>% min() #2022-05-02
visits_before_introduction_df$`접수일자#2` %>% max() #2022-08-31
visits_before_introduction_df %>% nrow() #4251


visits_after_introduction_df <- readxl::read_excel(raw_file("산부인과-초진(240902).xlsx"))
visits_after_introduction_df <- visits_after_introduction_df %>%
  mutate(across(everything(), as.character))
visits_after_introduction_df <- visits_after_introduction_df %>%
  filter((visits_after_introduction_df$`접수일자#2` >='2022-10-01'))
visits_after_introduction_df$`접수일자#2` %>% max() #2024-05-31
visits_after_introduction_df$`접수일자#2` %>% min() #2022-10-01
visits_after_introduction_df %>% nrow() #15885

visits_followup_df <- read.csv(raw_file("산부인과-초진-202406-202503.csv"), check.names = FALSE)
visits_followup_df %>% nrow() #5335
visits_followup_df$`접수일자#2` %>% min() #2024-06-03

visits_after_introduction_df <- visits_after_introduction_df %>% rbind(visits_followup_df)
visits_after_introduction_df$`접수일자#2` %>% min() #2022-10-04
visits_after_introduction_df$`접수일자#2` %>% max() #2025-03-31
visits_after_introduction_df %>% nrow() #21220

visits_latest_period_df <- read.csv(raw_file("산부인과-초진-202505-202512.csv"), check.names = FALSE)
visits_latest_period_df$`접수일자#2` %>% min() #2025-05-02
visits_latest_period_df$`접수일자#2` %>% max() #2025-12-31
visits_latest_period_df %>% nrow() #5558


visits_raw_df <- visits_before_introduction_df %>%
  rbind(visits_after_introduction_df) %>%
  rbind(visits_latest_period_df)
visits_raw_df %>% nrow() #31033
visits_raw_df %>% colnames()


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
standardized_visits_df <- standardized_visits_df %>% dplyr::select(-`진료과코드#31`, -'진료과코드#15', -'진료과명#32',
                                   -sex,-note,-insurance_cd,-dept_code)
standardized_visits_df %>% nrow() #16557 -> 18842
standardized_visits_df$pt_id %>% unique() %>% length()
standardized_visits_df %>% summary()

standardized_visits_df <- standardized_visits_df %>% filter(!is.na(complete_date.2))
standardized_visits_df %>% nrow() #17876

standardized_visits_df$mobile %>% table()
# 모바일 1/1  , 2/2 -> 1, 1/0, 2/0 -> 0 ------------------------------------------

visits_with_mobile_status_df <- standardized_visits_df %>% copy() #16549
visits_with_mobile_status_df %>% filter(mobile == '')

visits_with_mobile_status_df$mobile %>% table()
visits_with_mobile_status_df$mobile[visits_with_mobile_status_df$mobile == ''] <- NA
visits_with_mobile_status_df %>% nrow()

visits_with_mobile_status_df$mobile <- ifelse(visits_with_mobile_status_df$mobile %in% c('1/1', '2/2'), 1, ifelse(is.na(visits_with_mobile_status_df$mobile), NA, 0))
visits_with_mobile_status_df$mobile %>% table(useNA='always')

visits_with_mobile_status_df$application_date %>% min()
visits_with_mobile_status_df$application_date %>% max()

# 환자 제거 전 초진 시간 계산
## 모바일 문진 시행 조건 : 최종작성일시 <= 접수일시
visits_with_timestamps_df <- visits_with_mobile_status_df %>%
  mutate(application_time = as.POSIXct(paste(application_date, application_time), format = '%Y-%m-%d %H:%M', tz = "UTC"),
         fin_doc_dt = as.POSIXct(fin_doc_dt, format = '%Y-%m-%d %H:%M:%S', tz = "UTC"),
         application_date = as.POSIXct(application_date, format = '%Y-%m-%d', tz = "UTC"),
         complete_date.1 = as.POSIXct(complete_date.1, format = '%Y-%m-%d %H:%M:%S', tz = "UTC"),
         complete_date.2 = as.POSIXct(complete_date.2, format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))


visits_with_timestamps_df$application_time
visits_with_timestamps_df$fin_doc_dt

visits_with_timestamps_df %>% filter(!is.na(fin_doc_dt))
visits_with_timestamps_df %>% filter(is.na(fin_doc_dt)) %>% dplyr::select(mobile) %>% unique()
visits_with_timestamps_df %>% mutate(mobile_re = if_else(mobile == 1 & fin_doc_dt > application_time, 0 , mobile)) %>% dplyr::select(mobile_re) %>% table()


# 전자 문진 완료 시간이 접수 후 초진 이전 전에 완성한 비율 약 13%
mean((visits_with_timestamps_df$fin_doc_dt >= visits_with_timestamps_df$application_time &
        visits_with_timestamps_df$fin_doc_dt < visits_with_timestamps_df$complete_date.2) &
       (visits_with_timestamps_df$mobile== 1), na.rm =TRUE)


visits_with_exposure_df <- visits_with_timestamps_df %>%
    mutate(mobile = if_else(mobile == '', 0, mobile))
visits_with_exposure_df$mobile %>% table(useNA='always')

visits_with_exposure_df <- visits_with_exposure_df %>%
  mutate(mobile_re = if_else((mobile == 1) & (fin_doc_dt > complete_date.2), 0 , mobile))
visits_with_exposure_df$mobile %>% table(useNA='always')


mean(visits_with_timestamps_df$fin_doc_dt >= visits_with_timestamps_df$application_time &
       visits_with_timestamps_df$fin_doc_dt < visits_with_timestamps_df$complete_date.2, na.rm =TRUE)

# 접수 이후 작성 완료 비율 약 48%
mean(visits_with_timestamps_df$fin_doc_dt >= visits_with_timestamps_df$application_date, na.rm =TRUE)


visits_with_exposure_df$application_time
visits_with_exposure_df$mobile_re %>% table(useNA='always')
visits_with_exposure_df$application_date %>% max()

visits_with_previous_completion_df <- visits_with_exposure_df %>%
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
visits_with_previous_completion_df <- visits_with_previous_completion_df %>%
  arrange(complete_date.1) %>%
  group_by(application_date) %>%
  mutate(pre_pt_time = lag(complete_date.1)) %>%
  ungroup()

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
      abs(difftime(complete_date.1, pre_pt_time, units = "mins")) <= abs(difftime(complete_date.1, complete_date.2, units = "mins")),
      pre_pt_time,
      complete_date.2
    )
  ) %>% ungroup()

visits_with_time_reference_df$pre_complete.1 <- as.POSIXct(visits_with_time_reference_df$pre_complete.1,tz="UTC")
visits_with_time_reference_df <- visits_with_time_reference_df %>%
  mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))

# 새로운 초진 시간 계산
visits_with_adjusted_time_df <- visits_with_time_reference_df %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
) #16549

# 18세 미만 제외
adult_visits_df <- visits_with_adjusted_time_df[!grepl("개월", visits_with_adjusted_time_df$age),]
adult_visits_df$age <- gsub('세', '', adult_visits_df$age)

adult_visits_df$age <- adult_visits_df$age %>% as.numeric()
adult_visits_df <- adult_visits_df[!is.na(adult_visits_df$age),]
adult_visits_df <- adult_visits_df %>% filter(age >= 18)

adult_visits_df %>% nrow() #17428

# 산과 제외
excluded_physicians <- c("노정래", "오수영", "최석주", "성지희", "최윤선", "김예진","조혜정", "홍서연", "이유리", "함수지")
gynecology_visits_df <- adult_visits_df %>% filter(!(docter_name %in% excluded_physicians))
gynecology_visits_df %>% nrow() #15990

# 모바일 결측 제외(589)

visits_with_valid_mobile_status_df <- gynecology_visits_df %>%
  filter(!(application_time >= as.POSIXct('2022-10-01', tz = 'UTC') & (is.na(mobile) | mobile == '')))

gynecology_visits_df %>% nrow()


# visits_with_valid_mobile_status_df$mobile[visits_with_valid_mobile_status_df$application_date %in% c("2023-05-24", "2024-02-17") & visits_with_valid_mobile_status_df$mobile == "2/1"] <- "2/2"
# visits_with_valid_mobile_status_df$mobile[visits_with_valid_mobile_status_df$mobile == "2/1"] <- "2/0"

visits_with_valid_mobile_status_df %>% nrow() #14510
visits_with_valid_mobile_status_df$mobile_re %>% table(useNA='always')
visits_with_valid_mobile_status_df <- visits_with_valid_mobile_status_df %>%
  mutate(mobile_re = ifelse(is.na(mobile_re), 0, mobile_re))


# 오후 외래 오전 접수 제외 (281)
visits_with_valid_registration_df <- visits_with_valid_mobile_status_df %>% filter(!(reservated_time > "12:00" & format(application_time, "%H:%M") < "12:00"))

visits_with_valid_registration_df %>% nrow() #14229


#외국인 제거 (3349)
analysis_visits_df <- visits_with_valid_registration_df %>% filter(nat_cd == "KR" )
analysis_visits_df %>% nrow() #10905

analysis_visits_df %>% mutate(insurance_nm = ifelse(insurance_nm == "국제진료센터", insurance_nm, 0)) %>% mytable()

analysis_visits_df <- analysis_visits_df %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
analysis_visits_df$insurance_nm %>% unique()
analysis_visits_df$insurance_nm[is.na(analysis_visits_df$insurance_nm)] <- 0

analysis_visits_df$nat_cd %>% unique()

analysis_visits_df %>% nrow() #12079

# 최종작성일시 결측 제거

mytable(mobile_re~age+new_time_diff, data = visits_with_valid_mobile_status_df)

# 초진 작성 60분 이상 제외 (2)

#visits_with_adjusted_time_df <- visits_with_adjusted_time_df[abs(difftime(visits_with_adjusted_time_df$complete_date.2, visits_with_adjusted_time_df$complete_date.1, units = "hours"))<1,]
analysis_visits_df <- analysis_visits_df[abs(analysis_visits_df$time_diff)<60,]
analysis_visits_df %>% nrow() #10878

analysis_visits_df$mobile_re <- analysis_visits_df$mobile_re %>% as.factor()

analysis_visits_df %>% summary()


analysis_visits_df <- analysis_visits_df %>%
  mutate(period_group = case_when(
    application_date <  as.Date("2022-10-01") ~ "1. 2022.09 이전",
    application_date >= as.Date("2022-10-01") & application_date <= as.Date("2025-03-31") ~ "2. 2022.10 ~ 2025.03",
    application_date >= as.Date("2025-05-01") & application_date <= as.Date("2025-12-31") ~ "3. 2025.05 ~ 2025.12",
    TRUE ~ 'else'
  ))
analysis_visits_df <- analysis_visits_df %>% filter(period_group != 'else')
analysis_visits_df$time_diff <- analysis_visits_df$time_diff %>% as.numeric()
analysis_visits_df$new_time_diff <- analysis_visits_df$new_time_diff %>% as.numeric()

mytable(period_group~age+new_time_diff, data = analysis_visits_df)

mytable(period_group~age+time_diff, data = analysis_visits_df)
mytable(mobile_re~age+time_diff+new_time_diff, data = analysis_visits_df)
lm(time_diff ~ mobile_re, data = analysis_visits_df) %>% summary()


analysis_visits_df %>%
  group_by(period_group) %>%
  summarise(
    total_count = n(),
    matching_count = sum(pre_complete.1 > complete_date.2, na.rm = TRUE),
    ratio = matching_count / total_count,
    percentage = paste0(round(ratio * 100, 2), '%')
  )

analysis_visits_df %>% filter(period_group == 'else')

# 주 단위 (월요일 기준)
weekly_visits_df <- analysis_visits_df %>%
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

weekly_visits_df %>% summary()
weekly_visits_df %>% nrow() #10878

# 1. 날짜 컬럼을 Date 객체로 변환 (date_col을 실제 날짜 컬럼명으로 수정하세요)
# visits_with_mobile_status_df$date_col <- as.Date(visits_with_mobile_status_df$date_col)

# 2. 날짜 기준에 따른 그룹 생성 및 통계 계산
period_summary_df <- analysis_visits_df %>%
  mutate(period_group = case_when(
    application_date <  as.Date("2022-10-01") ~ "1. 2022.09 이전",
    application_date >= as.Date("2022-10-01") & application_date <= as.Date("2025-03-31") ~ "2. 2022.10 ~ 2025.03",
    application_date >= as.Date("2025-05-01") & application_date <= as.Date("2025-12-31") ~ "3. 2025.05 ~ 2025.12",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period_group)) %>% # 정의되지 않은 기간(예: 2025.04) 제외
  group_by(period_group) %>%
  summarise(
    n = n(),
    # Age 통계
    age_mean = mean(age, na.rm = TRUE),
    age_sd   = sd(age, na.rm = TRUE),
    age_med  = median(age, na.rm = TRUE),

    # new_time_diff 통계
    time_mean = mean(new_time_diff, na.rm = TRUE),
    time_sd   = sd(new_time_diff, na.rm = TRUE),
    time_med  = median(new_time_diff, na.rm = TRUE)
  )

# 결과 확인
print(period_summary_df)
