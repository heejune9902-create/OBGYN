source("C:/Users/박희준/Desktop/2024 MSBOS_multi_center/scr/libraries.R")
library(readr)

ob_df_raw1 <- read.csv("C:/Users/박희준/Desktop/산부인과/data/산부인과-초진-202205-202209.csv", check.names = FALSE)
ob_df_raw1 <- ob_df_raw1 %>% 
  filter(ob_df_raw1$`접수일자#2` <'2022-09-01')
ob_df_raw1 <- ob_df_raw1 %>% 
  mutate(across(c("진료일자#26","국제진료여부#27",    
                  "작성상태코드#28","작성상태명#29","최종작성일시#30","진료과코드#31",      
                  "진료과명#32"), ~ NA))

ob_df_raw1 %>% colnames()
ob_df_raw1$`접수일자#2` %>% min() #2022-05-02
ob_df_raw1$`접수일자#2` %>% max() #2022-08-31
ob_df_raw1 %>% nrow() #4251



ob_df_raw2 <- readxl::read_excel("C:/Users/박희준/Desktop/산부인과/data/산부인과-초진(240902).xlsx")
ob_df_raw2 <- ob_df_raw2 %>% 
  mutate(across(everything(), as.character))
ob_df_raw2 <- ob_df_raw2 %>% 
  filter((ob_df_raw2$`접수일자#2` >='2022-10-01'))
ob_df_raw2$`접수일자#2` %>% max() #2024-05-31
ob_df_raw2$`접수일자#2` %>% min() #2022-10-01
ob_df_raw2 %>% nrow() #15885

ob_df_raw2.2 <- read.csv("C:/Users/박희준/Desktop/산부인과/data/산부인과-초진-202406-202503.csv", check.names = FALSE)
ob_df_raw2.2 %>% nrow() #5335
ob_df_raw2.2$`접수일자#2` %>% min() #2024-06-03

ob_df_raw2 <- ob_df_raw2 %>% rbind(ob_df_raw2.2)
ob_df_raw2$`접수일자#2` %>% min() #2022-10-04
ob_df_raw2$`접수일자#2` %>% max() #2025-03-31
ob_df_raw2 %>% nrow() #21220

ob_df_raw3 <- read.csv("C:/Users/박희준/Desktop/산부인과/data/산부인과-초진-202505-202512.csv", check.names = FALSE)
ob_df_raw3$`접수일자#2` %>% min() #2025-05-02
ob_df_raw3$`접수일자#2` %>% max() #2025-12-31
ob_df_raw3 %>% nrow() #5558


ob_df_raw <- ob_df_raw1 %>% 
  rbind(ob_df_raw2) %>% 
  rbind(ob_df_raw3)
ob_df_raw %>% nrow() #31033
ob_df_raw %>% colnames()
ob_df_raw <- ob_df_raw %>% dplyr::select(-'환자번호#1') 
save(ob_df_raw,file = 'C:/Users/박희준/Desktop/산부인과/data/ob_result.RData')

load('C:/Users/박희준/Desktop/산부인과/data/ob_result.RData')

ob_df0 <- ob_df_raw %>% rename(pt_id = `환자번호#1`,
                               application_date =`접수 일자#2`,
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
ob_df0 <- ob_df0 %>% dplyr::select(-`진료과코드#31`, -'진료과코드#15', -'진료과명#32',
                                   -sex,-note,-insurance_cd,-dept_code)
ob_df0 %>% nrow() #16557 -> 18842
ob_df0$pt_id %>% unique() %>% length()
ob_df0 %>% summary()
 
ob_df0 <- ob_df0 %>% filter(!is.na(complete_date.2)) 
ob_df0 %>% nrow() #17876

ob_df0$mobile %>% table()
# 모바일 1/1  , 2/2 -> 1, 1/0, 2/0 -> 0 ------------------------------------------

ob_df1 <- ob_df0 %>% copy() #16549
ob_df1 %>% filter(mobile == '')

ob_df1$mobile %>% table()
ob_df1$mobile[ob_df1$mobile == ''] <- NA
ob_df1 %>% nrow()

ob_df1$mobile <- ifelse(ob_df1$mobile %in% c('1/1', '2/2'), 1, ifelse(is.na(ob_df1$mobile), NA, 0))
ob_df1$mobile %>% table(useNA='always')

ob_df1$application_date %>% min()
ob_df1$application_date %>% max()

# 환자 제거 전 초진 시간 계산
## 모바일 문진 시행 조건 : 최종작성일시 <= 접수일시
ob_df2 <- ob_df1 %>% 
  mutate(application_time = as.POSIXct(paste(application_date, application_time), format = '%Y-%m-%d %H:%M', tz = "UTC"),
         fin_doc_dt = as.POSIXct(fin_doc_dt, format = '%Y-%m-%d %H:%M:%S', tz = "UTC"),
         application_date = as.POSIXct(application_date, format = '%Y-%m-%d', tz = "UTC"),
         complete_date.1 = as.POSIXct(complete_date.1, format = '%Y-%m-%d %H:%M:%S', tz = "UTC"),
         complete_date.2 = as.POSIXct(complete_date.2, format = '%Y-%m-%d %H:%M:%S', tz = "UTC"))


ob_df2$application_time
ob_df2$fin_doc_dt

ob_df2 %>% filter(!is.na(fin_doc_dt))
ob_df2 %>% filter(is.na(fin_doc_dt)) %>% dplyr::select(mobile) %>% unique()
ob_df2 %>% mutate(mobile_re = if_else(mobile == 1 & fin_doc_dt > application_time, 0 , mobile)) %>% dplyr::select(mobile_re) %>% table()


# 전자 문진 완료 시간이 접수 후 초진 이전 전에 완성한 비율 약 13%
mean((ob_df2$fin_doc_dt >= ob_df2$application_time &
        ob_df2$fin_doc_dt < ob_df2$complete_date.2) &
       (ob_df2$mobile== 1), na.rm =TRUE)


ob_df3 <- ob_df2 %>% 
    mutate(mobile = if_else(mobile == '', 0, mobile))
ob_df3$mobile %>% table(useNA='always')

ob_df3 <- ob_df3 %>% 
  mutate(mobile_re = if_else((mobile == 1) & (fin_doc_dt > complete_date.2), 0 , mobile))
ob_df3$mobile %>% table(useNA='always')


mean(ob_df2$fin_doc_dt >= ob_df2$application_time &
       ob_df2$fin_doc_dt < ob_df2$complete_date.2, na.rm =TRUE)

# 접수 이후 작성 완료 비율 약 48%
mean(ob_df2$fin_doc_dt >= ob_df2$application_date, na.rm =TRUE)


ob_df3$application_time
ob_df3$mobile_re %>% table(useNA='always')
ob_df3$application_date %>% max()

ob_df4 <- ob_df3 %>% 
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
ob_df4 <- ob_df4 %>% 
  arrange(complete_date.1) %>% 
  group_by(application_date) %>% 
  mutate(pre_pt_time = lag(complete_date.1)) %>% 
  ungroup()

ob_df4 <- ob_df4 %>% 
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
ob_df5 <- ob_df4 %>% 
  rowwise() %>% 
  mutate(
    pre_complete.1 = ifelse(
      abs(difftime(complete_date.1, pre_pt_time, units = "mins")) <= abs(difftime(complete_date.1, complete_date.2, units = "mins")),
      pre_pt_time,
      complete_date.2
    )
  ) %>% ungroup() 

ob_df5$pre_complete.1 <- as.POSIXct(ob_df5$pre_complete.1,tz="UTC")
ob_df5 <- ob_df5 %>% 
  mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))

# 새로운 초진 시간 계산
ob_df6 <- ob_df5 %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
) #16549

# 18세 미만 제외
ob_df7 <- ob_df6[!grepl("개월", ob_df6$age),]
ob_df7$age <- gsub('세', '', ob_df7$age)

ob_df7$age <- ob_df7$age %>% as.numeric()
ob_df7 <- ob_df7[!is.na(ob_df7$age),]
ob_df7 <- ob_df7 %>% filter(age >= 18)

ob_df7 %>% nrow() #17428

# 산과 제외
exclusion_docter_list <- c("노정래", "오수영", "최석주", "성지희", "최윤선", "김예진","조혜정", "홍서연", "이유리", "함수지")
ob_df8 <- ob_df7 %>% filter(!(docter_name %in% exclusion_docter_list))
ob_df8 %>% nrow() #15990

# 모바일 결측 제외(589)

ob_df9 <- ob_df8 %>% 
  filter(!(application_time >= as.POSIXct('2022-10-01', tz = 'UTC') & (is.na(mobile) | mobile == '')))

ob_df8 %>% nrow()


# ob_df9$mobile[ob_df9$application_date %in% c("2023-05-24", "2024-02-17") & ob_df9$mobile == "2/1"] <- "2/2"
# ob_df9$mobile[ob_df9$mobile == "2/1"] <- "2/0"

ob_df9 %>% nrow() #14510
ob_df9$mobile_re %>% table(useNA='always')
ob_df9 <- ob_df9 %>% 
  mutate(mobile_re = ifelse(is.na(mobile_re), 0, mobile_re))


# 오후 외래 오전 접수 제외 (281)
ob_df10 <- ob_df9 %>% filter(!(reservated_time > "12:00" & format(application_time, "%H:%M") < "12:00"))

ob_df10 %>% nrow() #14229




#외국인 제거 (3349)
ob_df11 <- ob_df10 %>% filter(nat_cd == "KR" )
ob_df11 %>% nrow() #10905

ob_df11 %>% mutate(insurance_nm = ifelse(insurance_nm == "국제진료센터", insurance_nm, 0)) %>% mytable()

ob_df11 <- ob_df11 %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
ob_df11$insurance_nm %>% unique()
ob_df11$insurance_nm[is.na(ob_df11$insurance_nm)] <- 0

ob_df11$nat_cd %>% unique()

ob_df11 %>% nrow() #12079

# 최종작성일시 결측 제거

mytable(mobile_re~age+new_time_diff, data = ob_df9)

# 초진 작성 60분 이상 제외 (2)

#ob_df6 <- ob_df6[abs(difftime(ob_df6$complete_date.2, ob_df6$complete_date.1, units = "hours"))<1,] 
ob_df11 <- ob_df11[abs(ob_df11$time_diff)<60,]
ob_df11 %>% nrow() #10878

ob_df11$mobile_re <- ob_df11$mobile_re %>% as.factor()

ob_df11 %>% summary()


ob_df11 <- ob_df11 %>%
  mutate(period_group = case_when(
    application_date <  as.Date("2022-10-01") ~ "1. 2022.09 이전",
    application_date >= as.Date("2022-10-01") & application_date <= as.Date("2025-03-31") ~ "2. 2022.10 ~ 2025.03",
    application_date >= as.Date("2025-05-01") & application_date <= as.Date("2025-12-31") ~ "3. 2025.05 ~ 2025.12",
    TRUE ~ 'else'
  ))
ob_df11 <- ob_df11 %>% filter(period_group != 'else')
ob_df11$time_diff <- ob_df11$time_diff %>% as.numeric()
ob_df11$new_time_diff <- ob_df11$new_time_diff %>% as.numeric()

mytable(period_group~age+new_time_diff, data = ob_df11)

mytable(period_group~age+time_diff, data = ob_df11)
mytable(mobile_re~age+time_diff+new_time_diff, data = ob_df11)
lm(time_diff ~ mobile_re, data = ob_df11) %>% summary()


ob_df11 %>% 
  group_by(period_group) %>% 
  summarise(
    total_count = n(),
    matching_count = sum(pre_complete.1 > complete_date.2, na.rm = TRUE),
    ratio = matching_count / total_count,
    percentage = paste0(round(ratio * 100, 2), '%')
  )

ob_df11 %>% filter(period_group == 'else')

# 주 단위 (월요일 기준)
weekly_data <- ob_df11 %>% 
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

weekly_data %>% summary()
weekly_data %>% nrow() #10878

# 1. 날짜 컬럼을 Date 객체로 변환 (date_col을 실제 날짜 컬럼명으로 수정하세요)
# ob_df1$date_col <- as.Date(ob_df1$date_col)

# 2. 날짜 기준에 따른 그룹 생성 및 통계 계산
summary_stats <- ob_df11 %>%
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
print(summary_stats)
