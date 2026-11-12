source("C:/Users/박희준/Desktop/2024 MSBOS_multi_center/scr/libraries.R")
library(readr)
ob_df_raw <- readxl::read_excel("data/산부인과-초진(240902).xlsx")

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
ob_df0 %>% nrow() #16557
ob_df0 %>% summary()

ob_df0 <- ob_df0 %>% filter(!is.na(complete_date.2))
ob_df0 %>% nrow() #16549

# 18세 미만 제외
ob_df1 <- ob_df0[!grepl("개월", ob_df0$age),]
ob_df1$age <- gsub('세', '', ob_df1$age)

ob_df1$age <- ob_df1$age %>% as.numeric()
ob_df1 <- ob_df1[!is.na(ob_df1$age),]
ob_df1 <- ob_df1 %>% filter(age >= 18)

ob_df1 %>% nrow() #16177


ob_df2 <- ob_df1
ob_df2 %>% nrow() #16177



ob_df2$mobile %>% table()
ob_df2[is.na(ob_df2$mobile),] %>% View()

# 모바일 결측 제외

ob_df3 <- ob_df2 %>% filter(mobile != "")

ob_df3$mobile[ob_df3$application_date %in% c("2023-05-24", "2024-02-17") & ob_df3$mobile == "2/1"] <- "2/2"
ob_df3$mobile[ob_df3$mobile == "2/1"] <- "2/0"

ob_df3 %>% nrow() # 15564

# 모바일 1/1, 2/2 -> 1, 1/0, 2/0 -> 0 ------------------------------------------

ob_df3 <- ob_df3 %>% mutate(mobile = ifelse(mobile %in% c("1/1", "2/2"), 1, 0))


# 오후 외래 오전 접수 제외
ob_df4 <- ob_df3 %>% filter(!(reservated_time > "12:00" & application_time < "12:00"))

ob_df4 %>% nrow() # 15273




#외국인 제거
ob_df5 <- ob_df4 %>% filter(nat_cd == "KR" )

ob_df5 %>% mutate(insurance_nm = ifelse(insurance_nm == "국제진료센터", insurance_nm, 0)) %>% mytable()
ob_df5 <- ob_df5 %>% filter(insurance_nm != "국제진료센터" | is.na(insurance_nm))
ob_df5$insurance_nm %>% unique()
ob_df5$insurance_nm[is.na(ob_df5$insurance_nm)] <- 0

ob_df5$nat_cd %>% unique()

ob_df5 %>% nrow() # 11626

# 최종작성일시 결측 제거
## 모바일 문진 시행 조건 : 최종작성일시 <= 접수일시
ob_df5 <- ob_df5 %>% 
  mutate(application_time = as.POSIXct(paste(application_date, application_time), tz = "UTC"))

ob_df5 %>% filter(!is.na(fin_doc_dt))
ob_df5 %>% filter(is.na(fin_doc_dt)) %>% dplyr::select(mobile) %>% unique()
ob_df5 %>% mutate(mobile_re = if_else(mobile == 1 & fin_doc_dt > application_time, 0 , mobile)) %>% dplyr::select(mobile_re) %>% table()
ob_df6 <- ob_df5 %>% mutate(mobile_re = if_else(mobile == 1 & fin_doc_dt >= application_time, 0 , mobile))

ob_df6$mobile_re %>% table()



ob_df7 <- ob_df6 %>% 
  mutate(time_diff = abs(difftime(complete_date.2, complete_date.1, units = "mins")),
         month = floor_date(application_date, unit = "month"))

# 이전 환자 초진 완료일시 가져오기 (pre_pt_time)
ob_df7 <- ob_df7 %>% arrange(complete_date.1) %>% 
  group_by(application_date) %>% 
  mutate(pre_pt_time = lag(complete_date.1)) %>% ungroup()

ob_df7 <- ob_df7 %>% 
  mutate(
    complete_date.1 = as.POSIXct(complete_date.1, origin = "1970-01-01", tz = "UTC"),
    pre_pt_time = as.POSIXct(pre_pt_time, origin = "1970-01-01", tz = "UTC"),
    complete_date.2 = as.POSIXct(complete_date.2, origin = "1970-01-01", tz = "UTC")
  )

# 이전 초진 완료시간 판단 (pre_complete.1 = 이전 환자 초진완료시간 vs 본인 이전 경로 완료 시간)
ob_df8 <- ob_df7 %>% 
  rowwise() %>% 
  mutate(
    pre_complete.1 = ifelse(
      abs(difftime(complete_date.1, pre_pt_time, units = "secs")) <= abs(difftime(complete_date.1, complete_date.2, units = "secs")),
      pre_pt_time,
      complete_date.2
    )
  ) %>% ungroup() 

ob_df8$pre_complete.1 <- as.POSIXct(ob_df8$pre_complete.1,tz="UTC")
ob_df8 <- ob_df8 %>% mutate(pre_complete.1 = if_else(is.na(pre_pt_time), complete_date.2, pre_complete.1))

# 새로운 초진 시간 계산
ob_df9 <- ob_df8 %>% mutate(
  new_time_diff = as.numeric(difftime(complete_date.1, pre_complete.1, units = "mins"))
)

ob_df9 %>% View()
mytable(mobile_re~age+new_time_diff, data = ob_df9)

# 초진 작성 60분 이상 제외

#ob_df6 <- ob_df6[abs(difftime(ob_df6$complete_date.2, ob_df6$complete_date.1, units = "hours"))<1,] 
ob_df10 <- ob_df9[abs(ob_df9$new_time_diff)<60,]
ob_df10 %>% nrow() # 11620

ob_df10$mobile_re <- ob_df10$mobile_re %>% as.factor()

ob_df10 %>% summary()

# 주 단위 (월요일 기준)
weekly_data <- ob_df10 %>% 
  mutate(week_start = floor_date(application_date, unit = "week", week_start = 1))

weekly_data %>% summary()
weekly_data %>% nrow() #11620

count <- weekly_data %>% group_by(week_start) %>% summarise(count=n())%>% mutate(total = sum(count), percentage = count / total * 100)

ggplot(count, aes(x = week_start, y = count)) +
  geom_col() +
  #geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 10))

count_m <- weekly_data %>% group_by(month) %>% summarise(count=n()) %>% mutate(total = sum(count), percentage = count / total * 100)

p <- ggplot(count_m, aes(x = month, y = count)) +
  geom_line() +
  theme_minimal() +
  scale_x_datetime(breaks = count_m$month) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8))
p

ggplot(count_m, aes(x = month, y = count)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5) +
  theme_minimal() +
  scale_x_datetime(breaks = count_m$month) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8))

# 세 그룹간 비교 ---------------------------------------------------------------
weekly_data %>% colnames()

# 그룹 1: 완전 미시행 (mobile = 0)
group1 <- weekly_data %>% filter(mobile == 0)
group1 %>% nrow() #448

# 그룹 2: 접수 후 모바일 완료 (mobile = 1 & fin_doc_dt >= application_time)
group2 <- weekly_data %>% filter(mobile == 1 & fin_doc_dt >= application_time)
group2 %>% nrow() #3023

# 그룹 3: 시행 (mobile = 1 & fin_doc_dt < application_time)
group3 <- weekly_data %>% filter(mobile == 1 & fin_doc_dt < application_time)
group3 %>% nrow() #8149


nrow(group1) + nrow(group2) + nrow(group3)
weekly_data %>% nrow()

weekly_data <- weekly_data %>% mutate(group = case_when(
  mobile == 0 ~ 1,
  mobile == 1 & fin_doc_dt >= application_time ~ 2,
  mobile == 1 & fin_doc_dt < application_time ~ 3,
  TRUE ~ NA_integer_
))

weekly_data %>% 
  group_by(group) %>% 
  summarise(
    average_age = mean(age, na.rm= TRUE),
    average_new_time_diff = mean(new_time_diff, na.rm = TRUE)
  )

con1_3 <- subset(weekly_data, group %in% c(1,3))
con1_2 <- subset(weekly_data, group %in% c(1,2))
con2_3 <- subset(weekly_data, group %in% c(2,3))
mytable(group~ age + new_time_diff, con2_3)
weekly_data %>% summary()

library(car)
weekly_data$group <- weekly_data$group %>% as.factor()
leveneTest(weekly_data$new_time_diff, weekly_data$group)
aov(group~ age, data = weekly_data)
aov(group~ new_time_diff, data = weekly_data)

# 두 그룹 5분, 10분 이내 비율 --------------------------------------

weekly_data %>% 
  group_by(mobile_re) %>% 
  summarise(
    n = n(),
    n_leq5 = sum(new_time_diff <= 5, na.rm = TRUE),
    n_leq10 = sum(new_time_diff <= 10, na.rm = TRUE),
    perc_leq5 = (n_leq5 / n) * 100,
    perc_leq10 = (n_leq10 / n) * 100
  )
