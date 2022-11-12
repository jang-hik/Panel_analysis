
rm(list=ls())

setwd('C:/Users/wkdgu/Desktop/학술제')
getwd()

library(plm); library(dplyr); library(stargazer); library(readxl)

# ----------------------------------------------------------------------------------------------
# 엑셀 파일 불러오기
data_1 <- read_excel("C:/Users/wkdgu/Desktop/학술제/학술제 독립변인.xlsx", range="A1:G65")
data_1

# pdata.frame()을 이용해 panel_date를 패널데이터로 선언
panel_data_1 <- pdata.frame(data_1, index=c('시군구별', '년도'))


#       종속변수 ~ 설명변수1 + 설명변수2 + 설명변수3 + 설명변수4
form_1 <-  육아휴직_사용률 ~ 인지도 + 근로자_평균연봉 + 여성_고용률 +  남성_참여율


# plm()은 패널데이터 함수
# plm은 세가지 model이 있음, 고정효과, 임의효과, OLS
fe_1 <- plm(form_1, data=panel_data_1, model='within') # 고정효과 모형
re_1 <- plm(form_1, data=panel_data_1, model = 'random') # 임의효과 모형

stargazer(fe_1, re_1, type='html', out='패널모형비교.doc')


# Hausman Test를 통해 RE와 FE 중에서 더 좋은 것을 고를 수 있음
# 귀무가설 채택 : RE선택, 대립가설 채택 : FE선택
# 변수를 쓰는 순서는 상관없음
phtest(fe_1, re_1) #  p-value = 0.5054, 귀무가설 채택



# ----------------------------------------------------------------------------------------------
# 엑셀 파일 불러오기
data_2 <- read_excel("C:/Users/wkdgu/Desktop/학술제/학술제 자료 지역별 합계출산율 및 모성보호제도.xlsx", sheet = 'Sheet2', range="A1:G65")

# pdata.frame()을 이용해 panel_date를 패널데이터로 선언
panel_data_2 <- pdata.frame(data_2, index=c('시군구별', '년도'))


######    종속변수 ~ 설명변수1 + 설명변수2 + 설명변수3 + 설명변수4
form_2 <-  합계출산율 ~ 출산전후_휴가급여 + 육아휴직_급여 + 육아기_근로시간_단축급여 +  배우자_출산휴가_급여


# plm()은 패널데이터 함수
# plm은 세가지 model이 있음, 고정효과, 임의효과, OLS
fe_2 <- plm(form_2, data=panel_data_2, model='within') # 고정효과 모형
re_2 <- plm(form_2, data=panel_data_2, model = 'random') # 임의효과 모형

stargazer(fe_2, re_2, column.labels = c('FE', 'RE'), type='html', out='패널모형비교2.doc')


# Hausman Test를 통해 RE와 FE 중에서 더 좋은 것을 고를 수 있음
# 귀무가설 채택 : RE선택, 대립가설 채택 : FE선택
# 변수를 쓰는 순서는 상관없음
phtest(fe_2, re_2) #   p-value = 0.999, 귀무가설 채택, re모형 채택


