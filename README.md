# 산부인과

모바일 문진 시행 여부와 완료 시점에 따른 초진 소요시간을 비교하고 기간별 변화를 살펴본다.

## 실행 순서

1. 프로젝트 루트에서 실행한다. 경로와 공통 패키지는 `scripts/00_setup.R`에서 설정하며 `OBGYN_DATA_ROOT`, `OBGYN_OUTPUT_ROOT`로 경로를 변경할 수 있다.
2. 같은 R 세션에서 `source("scripts/01_prepare_cohort.R")`를 실행한 뒤 `source("scripts/02_plot_visit_times.R")`를 실행한다.
3. 기간 비교는 별도 R 세션에서 `source("scripts/03_compare_periods.R")`로 실행한다.
4. 산과 포함 분석은 별도 R 세션에서 `source("scripts/04_include_obstetrics.R")`로 실행한다.
