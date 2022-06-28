## CDM 표준화 작업 수행하였던 R code를 공유하고자 함. 
## 다만, 데이터는 개인정보로 인해 공개할 수 없으며 표준화 방법에 대한 코드를 각 개인의 가지고 있는 데이터셋에 맞추어 적용해야 함. 
## 그렇기 때문에 데이터의 구조나 형식 등의 차이로 인해 코드가 제대로 작동하지 않을 가능성을 항상 염두에 두어야 하겠음. 
## 또한, 리눅스(CenTOS or Ubuntu)를 기반으로 하여 postgresql DB에 데이터를 적재하였으며 
## 리눅스 서버나 R, postgresql 등의 업데이트에 따라 기능이 작동하지 않을 수 있어 이에 대한 주의가 필요함. 
## 소스코드의 데이터 경로 및 데이터 이름은 임의로 설정함.
## 이 코드를 통해 정제되지 않은 데이터의 핸들링에 대한 아이디어를 제공하는 기초 자료가 되기를 희망함. 


# 필요한 Library import (install.packages를 통해 설치)
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(data.table)
library(haven)
library(readxl)
library(lubridate)
library(openxlsx)
library(foreach)
library(iterators)
library(parallel)
library(stringr) 
library(writexl)
library(DBI)
library(odbc)
library(RPostgreSQL)
library(doMC)
library(htmlTable)
library(Table1)
library(survival)
library(survminer)
library(MatchIt)
library(rms)
registerDoMC(31)

#폴더 내 연도별/월별 데이터를 리스트 형식을 통해 데이터를 한번에 import할 수 있음.
ldf <-list()
ldf<-dir(path="~pathway/rawdata/sujinfile", pattern="*.xlsx") # 
ldf
paste0('~pathway/rawdata/sujinfile/', ldf)
su0 <-lapply(ldf, function(ldf){read_excel(paste0('~pathway/rawdata/sujinfile/', ldf)) })

rm(list=ls())
library(dslabs)
library(tidyverse)
library(haven)
library(readxl)
library(lubridate)
library(data.table)
library(bit64)
library(knitr)
library(kableExtra)


ldf <-list()
ldf<-dir(path="~pathway/questionnaires", pattern="*.*") # data/basic 에 있는 모든 파일 보기
ldf 
sq1<-read_excel('~pathway/questionnaires/sp_q1.xlsx')

## 특검한 사람들을 구분 위해 s_que값을 1로 줌
sq1<-sq1 %>% mutate(s_que=1) %>% unique()

## 특검에서의 인적 정보 사항을 key로 만듦. 
## 검진일자, 이름, ori_person_id, 사업장명을 선택해서 검진년도, 검진월, 생년월일, sq=1를 추가
sqkey<-sq1 %>% mutate(examdate=as.character(검진일자), pidsq=ori_person_id) %>%
  select(examdate, 이름, pidsq, 사업장명, 검진일자) %>%
  mutate(year=substr(examdate, 1, 4), 
         sqmonth=substr(examdate, 6, 7), sq=1) %>%
  mutate(birthy=substr(pidsq, 1, 6))


# ori_person_id 차용 (sujin파일)-----------------------------------------------------------------

sujin_wide1 <- fread('~pathway/sujin_wide1.csv')

sujin_wide1

## sujin은 대상자명 sq는 이름, 통일해주기. 필요한 정보 추출 및 mutate로 생성
sujinkey <-sujin_wide1 %>% mutate(이름=대상자명) %>%
  select(n1차실시일,이름, ori_person_id, 사업장명) %>%
  mutate(year=substr(n1차실시일, 1, 4), 
         month=substr(n1차실시일, 6, 7), 
         ym=paste0(year, month)) %>%
  mutate(birthy=substr(ori_person_id, 1, 6))



# join --------------------------------------------------------------------

##full_join으로 하면 NA값도 우선 전부 다 포함해서 크게 붙는다 + sujin data에서의 ori_person_id NA 지움
mm<-sujinkey %>% full_join(sqkey, by=c('이름', '사업장명', 'year', 'birthy')) %>%
  filter(!is.na(ori_person_id))

## mm 중에서 특검한 사람들의 개인정보만 골라내기
sq_re <-mm %>% filter(sq==1)%>%
  select(이름, 사업장명, pidsq, ori_person_id , birthy) %>% unique()

## 특검데이터와 수진 데이터의 인적 정보를 합치기, 그중에서 ori_person_id가 NA인 경우는 그냥 버리기로 함. 
## ((sq_re를 left join 시켰을 때 sq1에 정보가 없어서 안 붙는 정보는 ori_person_id가 NA로 남을거기 때문에 
## sq1 데이터에 있는 사람의 ori_person_id를 수진 정보에서 매칭이 안된다는 것을 의미))

sq1.pid<-sq1 %>% mutate(pidsq=ori_person_id, year=substr(검진일자, 1, 4)) %>% select(-ori_person_id) %>%
  mutate(birthy=substr(pidsq, 1, 6)) %>%
  left_join(sq_re, by=c('pidsq', '이름', '사업장명', 'birthy')) %>% unique() %>%
  mutate(pid=ifelse(is.na(ori_person_id), pidsq, ori_person_id)) %>%
  filter(!is.na(ori_person_id))%>%
  select(-(contains('...')))


fwrite(sq1.pid, '~pathway/sq_id.csv')
## 결론적으로 특검 문진데이터에 sujin_wide1에서 가져온 ori_person_id를 생성한 것


# 야간 문진 데이터 불러오기  ---------------------------------------------------------

nq0 <- read_excel('~pathway/night.xlsx', sheet=1)
nqnames <- read_excel('~pathway/night_names.xlsx', sheet=1)
names(nqnames)


# nq0 이름 변경(coding book) --------------------------------------------------
##nqnames에 있는 이름을 colnames를 통해서 nq0에 붙여주기

ncol(nq0)==ncol(nqnames)
colnames(nq0) <- names(nqnames)
colnames(nq0)[1:15]


# 수진키랑 비교 option -----------------------------------------------------------------

names(sujinkey)[c(2, 3, 4, 5,6, 8)]

# 병합 준비 -------------------------------------------------------------------

## sq1.pid와 nq1의 변수명 맞춰주기, 특검, 야검 구별해주기 

sq1.pid<-sq1.pid %>% dplyr::mutate(특수검진일자=검진일자)

nq1 <-nq0 %>% dplyr::mutate(이름=환자성명, pid = ori_person_id, 야간검진일자=검진일자) %>% mutate(year=substr(야간검진일자, 1, 4))%>%
  select(-환자성명, -ori_person_id)

mmsqnq<-sq1.pid %>% full_join(nq1, by=c('이름', '사업장명', 'year', 'ori_person_id') )
mmsqnq2<-sq1.pid %>% full_join(nq1, by=c('이름', '사업장명', '검진일자', 'ori_person_id') )
nrow(sq1.pid)

mmsqnq2%>% filter(is.na(pid))

mmsqnq3 <- sq1.pid %>% full_join(nq1, by= c('이름','사업장명','year','검진일자','pid'))%>%
  select(-ori_person_id)


# year보다는 검진일자를 기준으로 해서 병합------------------------------------------------
## year와 검진일자를 같이 full_join variable로 넣어서 병합해야함.
## 야간검진의 경우 검진일자로 붙여줘야 한 횟수가 정확히 나오는데 검진일자로만 하면 full_join에서 양쪽에 year 변수가 다 있어서 year.x와 year.y로 나뉨
## 또한, 유해물질 검사의 경우, 년도별로 붙여야 하기 때문에 year, 검진일자를 전부 넣어서 병합 

## data 확인 과정 

## pid는 person ID(ori_person_id), pidsq는 뒷자리가 별표인 특검에서 나온 ori_person_id 

## 병합된 데이터 저장

fwrite(mmsqnq3, '~pathway/sqnq.csv')


# 생체지표 데이터  ---------------------------------------------------------------

ldf <-list()
ldf<-dir(path="~pathway/clinical", pattern="*.*") # data/clinical 에 있는 모든 파일 보기
ldf

#import data

bio0<-read_excel('~pathway/clinical/biomonitoring.xlsx', sheet=1)


##Cleaning

bio0 %>% group_by(결과) %>% dplyr::count() %>%nrow()
bio0 %>% group_by(결과) %>% dplyr::count() %>% tibble()%>% .[1750:1759,]

#나오는 negative 주관식 값들을 0으로 변환
bio1 <-bio0 %>% mutate(gg=ifelse(grepl('불검|붉|블|정상|not|<', 결과), 0, 결과)) 
bio1 %>% group_by(gg) %>% dplyr::count() %>% tibble() %>% .[1740:1759,]

#gg는 숫자로 변환. 검진년도 생성(year), 유해인자 삭제 
bio2 <-bio1 %>% mutate( gg=as.numeric(gg)) %>% mutate(year=substr(실시일, 1, 4)) %>% select(-유해인자)


#사업장 및 검사명으로 group by 해서 count. 

bio3<-bio2 %>%   group_by(사업장명, 대상자명, ori_person_id, year, 검사명) %>%
  dplyr:: mutate(flag=row_number(), count=n()) %>% dplyr::arrange(desc(count))

bio3

#검사명과 횟수 identify

bio4<-bio3 %>% ungroup () %>% mutate(keyvalue=paste0(검사명, "_", 'v',flag)) %>%
  select(-검사명, -flag)

#wide 변환
bio4.w <-bio4 %>% spread(key=keyvalue, value=gg) %>%  select(-결과, -실시일)


#fill up and down 

bio4.w %>%select(matches("v\\d")) %>%  names() -> fill_name_bio

#exdate.w %>% ungroup() %>% select(., contains("n_")) %>%  names() -> fill_name
fill_name_bio

#fill up and down 
bio4.w1 <-bio4.w %>% group_by(사업장명, 대상자명, ori_person_id, year) %>%
  fill(fill_name_bio)%>% fill(fill_name_bio, .direction = "up")%>%
  select(-count) %>% 
  group_by(사업장명, 대상자명, ori_person_id, year)%>%filter(row_number()==1)

#실시일자 wide form 

exdate <-bio3 %>% select(사업장명, 대상자명, ori_person_id, 실시일, year, flag, 검사명, count) %>%
  mutate(exam_n=case_when(flag ==1 ~'n1', 
                          flag ==2~ 'n2',
                          flag==3 ~ 'n3', 
                          flag==4 ~ 'n4')) %>% select(-flag) %>%
  mutate(nthvalue=paste(검사명, exam_n, sep="_"))

#실시일자 wide 변형

exdate.w <-exdate %>% mutate(iden=실시일)%>%
  spread(key=nthvalue, value=실시일) #%>% fill(n1, n2, n3, n4) %>% fill(n1:n4, .direction="up")


#fill up down name 찾기 

exdate.w %>% ungroup() %>% select(matches("n\\d")) %>%  names() -> fill_name
#exdate.w %>% ungroup() %>% select(., contains("n_")) %>%  names() -> fill_name
fill_name
exdate.w

#fill up down 실시 
exdate.w1 <-exdate.w %>% group_by(사업장명, 대상자명, ori_person_id, year)%>%
  fill(fill_name, .direction='down') %>% fill(fill_name, .direction="up")

#중복 제거
exdate.w2<-exdate.w1 %>% ungroup()%>%select(-iden, -exam_n,-검사명,-count) %>%
  group_by(사업장명, 대상자명, ori_person_id, year)%>%filter(row_number()==1)

#bio4.w1과 exdate.w2를 병합 

names(exdate.w2)[1:10]
names(bio4.w1)[1:10]
nrow(exdate.w2)
nrow(bio4.w1)

mmsb <-exdate.w2 %>% full_join(bio4.w1 , by=c("사업장명", "대상자명", "ori_person_id", "year" )) %>%
  dplyr::rename(pidsq=ori_person_id, 이름=대상자명) %>% mutate(sbio=1) %>%
  mutate(bym = substr(pidsq, 1, 4))


# biomonitoring ori_person_id 생성

# 수진에서 기초 정보랑 ori_person_id 가져 오기
pidref <- sujin1 %>%  select("사업장명", "이름"="대상자명", "ori_person_id",  "n1차실시일")
pidref1 <- pidref %>%
  mutate(year = substr(n1차실시일, 1, 4), 
         bym   = substr(ori_person_id, 1, 4)) %>%
  select(-n1차실시일) %>%
  unique()

pidref1

mmsb_pid <- mmsb %>%
  left_join(pidref1, by = c("사업장명", "이름", "year", "bym")) %>%
  filter(sbio==1)



#특검, 야간, biomonitoring 합치기 

mm_sp_exam <-mmsqnq3 %>% full_join(mmsb_pid, by=c("사업장명", "이름", "year", "pid"="ori_person_id"))


# 합친 데이터 저장. 추후 일반검진과 long으로 병합

fwrite(mm_sp_exam, '~pathway/mm_sp_exam_wide.csv')



# 새로운 Sheet ---------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(haven)
library(readxl)
library(lubridate)
library(data.table)
library(bit64)
library(knitr)
library(kableExtra)
library(openxlsx)
library(doMC)
registerDoMC(31)

#data import 

longbase1 <- fread('~pathway/longbase1.csv')
mm_sp_exam_wide <- fread('~pathway/mm_sp_exam_wide.csv')
sujin1 <- fread('~pathway/sujin_wide1.csv')

mm_sp_exam_wide <- mm_sp_exam_wide %>%select(-이름)

## 일반검진 long to wide 
st <-Sys.time()
wide1 <- longbase1 %>%
  spread(key = variable, value = value)
et <- Sys.time()
et-st

# wide merge --------------------------------------------------------------

mm <- sujin1 %>% mutate(검진일자 = as.character(n1차실시일)) %>%
  full_join(wide1 %>% mutate(검진일자 = as.character(검진일자)), by=c("ori_person_id",  "검진일자"))

mm1 <-mm %>%
  full_join(mm_sp_exam_wide %>% mutate(검진일자 = as.character(검진일자), 
                                           ori_person_id = pid)%>%select(-pid), by = c('검진일자', 'ori_person_id'))

# import master file ------------------------------------------------------

##left_join이나 full_join시에 중복변수는 뒤에 .x,.y같은 것들이 붙어 이런것들을 정리해주기
#대상자명x,y의 경우 data가 나뉘어 있어서 추후 뒤에서 따로 제거
testing <- mm1 %>%
  select(matches('y$'))
testing %>% names()

testing2 <- mm1 %>%
  select(matches('유해인자'))


mm2 <- mm1 %>%
  select(-matches('y$')) %>%
  select(-사업장명) %>%
  rename(사업장명 = 사업장명.x, 유해인자=유해인자.x, pidsq=pidsq.x)

names <-colnames(mm2) 
openxlsx::write.xlsx(names, 'names.xlsx')


# data cleaning -----------------------------------------------------------
# 
mstcode <- read_xlsx('~pathway/200625_master.xlsx')

# 현재 wide의 변수에 n=1을 부여하여 master와 병합
# n이 NA인 것과 mstcode상 mmnames가 NA인 것에 대해 확인하여 변수명 교정 예정
mstcode
name_dat <- tibble(origin = names, n =1)

name_comp <- name_dat %>%
  full_join(mstcode, by ='origin')

name_comp %>% 
  pull(n) %>%
  is.na() %>% table()

name_comp %>% 
  pull(mmnames) %>%
  is.na() %>% table()

name_check <- name_comp %>% filter(is.na(n) | is.na(mmnames))


# 변수 일원화 ------------------------------------------------------------

v3 <- mm1 %>%
  select(vo2, matches('체중')) # 전체데이터(mm1)에서 visiting_occurrence(수진번호)와 체중 관련 변수 가져오기

## wide to long으로 변환
v3long <- v3 %>%
  gather(-vo2, key = variable, value = values)

v3long1 <- v3long %>%
  arrange(vo2) %>%
  mutate(nn = '체중') %>% # 변수명 명명
  select(-variable) %>%
  rename(variable = nn) %>% 
  group_by(vo2) %>%
  fill(values, .direction = 'updown') %>% # 같은 그룹 내 위아래로 변수값(value)이 NA인 경우 다른 값을 차용
  unique() %>%
  ungroup() %>%
  mutate(values = as.numeric(values)) %>%
  na.omit() %>%
  unique() %>%
  filter(values <150) # 이상치 제거
  

#다시 long to wide하고나서 merge (반드시 다시 wide로 바뀌는지 확인, 중복값 없는지 확인하는 과정)
v3wide <- v3long1 %>%
  spread(key = variable, value = values)


mm2 <- mm1 %>%
  select(-matches('체중')) %>%   # v3 실시 사항 
  left_join(v3wide, by = 'vo2') # 일원화된 데이터셋을 visiting_occurrence 기준으로 join

#이와 같은 방식으로 모든 변수들에 대해 일원화를 시행하였음.

# 유해물질 text 분석 ------------------------------------------------------------

## stringr package 및 greple을 이용하여 텍스트 정제 작업

## 정규표현식을 통해 괄호 및 특수문자 등을 제거
test1<- exposure_assessment %>%  
  select(value) %>%
  mutate(nvalue = str_replace_all(value, "\\(", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\)", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\]", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\[|\\-", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, " ", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\d", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "Oil", "")) %>%
  select(nvalue) %>%
  group_by(nvalue) %>%
  count()


test2 <- exposure_assessment %>%
  select(value) %>%
  mutate(nvalue = str_replace_all(value, "[[:punct:]]|\\d", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, " ", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "함유제제|및미스트|미네랄오일미스트|Oilmist|납화합물|그무기화합물|연및", "")) %>% ## 유해물질 외 부속설명어 등을 제거
  mutate(nvalue = str_replace_all(nvalue, "구리분진|구리흄", "구리")) %>%
  mutate(nvalue = str_replace_all(nvalue, "무기화합물|가용성|불용성|DCM|그화합물|및|과|강목|연목|기타|중량비율이상|월평균회|그동족체", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "기타분진", "기타광물성분진")) %>%
  mutate(nvalue = str_replace_all(nvalue, "망간분진", "망간")) %>%
  mutate(nvalue = str_replace_all(nvalue, "메타", "메탄올")) %>%
  mutate(nvalue = ifelse(grepl('크롬', nvalue), '크롬',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('부톡시에탄올', nvalue), '부톡시에탄올', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='메틸시클로헥사놀', '	메틸시클로헥산', nvalue)) %>% # 같은 물질인 경우 이름 통일해주기
  mutate(nvalue = ifelse(nvalue =='디클로로플루오르메탄', '디클로로플루오로메탄', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='메틸에틸케톤MEK', '메틸에틸케톤', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='알코올', '알콜', nvalue)) %>%
  mutate(nvalue = case_when (nvalue == '불화수소불산' ~  '불산', 
                             nvalue == '산화아연흄'   ~  '산화아연', 
                             nvalue == '산화아연분진' ~  '산화아연', 
                             nvalue == '에틸렌글리콜모노부틸아세테이트' ~ '에틸렌글리콜',
                             nvalue == '트리클로로아세트산삼염화초산' ~ '트리클로로아세트',
                             TRUE ~ nvalue
  )) %>%
  group_by(nvalue) %>%
  count() 

expgroup <- 
  exposure_assessment %>%
  mutate(nvalue = str_replace_all(value, "\\(", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\)", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\]", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\[|\\-", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, " | ", "")) %>%
  mutate(nvalue = str_replace_all(value, "[[:punct:]]|\\d", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, " ", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "\\d|-", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "함유제제|및미스트|미네랄오일미스트|Oilmist|납화합물|그무기화합물|연및|삼염화초산|제품|취급|함유|물질", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "구리분진|구리흄", "구리")) %>%
  mutate(nvalue = str_replace_all(nvalue, "무기화합물|가용성|불용성|DCM|그화합물|및|과|강목|연목|기타|중량비율이상|월평균회", "")) %>%
  mutate(nvalue = str_replace_all(nvalue, "기타분진", "기타광물성분진")) %>%
  mutate(nvalue = str_replace_all(nvalue, "망간분진", "망간")) %>%
  mutate(nvalue = str_replace_all(nvalue, "메타", "메탄올")) %>%
  mutate(nvalue = ifelse(grepl('크롬', nvalue), '크롬',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('부톡시에탄올', nvalue), '부톡시에탄올', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='메틸시클로헥사놀', '	메틸시클로헥산', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='디클로로플루오르메탄', '디클로로플루오로메탄', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='메틸에틸케톤MEK', '메틸에틸케톤', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='알코올', '알콜', nvalue)) %>%
  mutate(nvalue = ifelse(nvalue =='트리클로로에틸렌총삼염화물', '트리클로로에틸렌', nvalue)) %>%
  
  mutate(nvalue = ifelse(grepl('수은', nvalue), '수은',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('알루미늄', nvalue), '알루미늄',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('주석', nvalue), '주석',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('텅스텐', nvalue), '텅스텐',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('니켈', nvalue), '니켈',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('안티몬', nvalue), '안티몬',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('아연', nvalue), '아연',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('이소시아네이트', nvalue), '톨루엔디이소시아네이트',  nvalue)) %>%
  mutate(nvalue = ifelse(grepl('철', nvalue), '산화철',  nvalue)) %>%
  mutate(nvalue = case_when (
    nvalue == '불화수소불산' ~  '불산', 
    nvalue %in% c('mineral', '기구사용작업','오일미스트식물성')      ~  '금속가공유', 
    nvalue == '에틸렌글리콜모노부틸아세테이트' ~ '에틸렌글리콜',
    nvalue == '트리클로로아세트산삼염화초산' ~ '트리클로로아세트',
    nvalue == '생애검진' ~ '일반검진',
    nvalue == 'n부틸알코올부탄올' ~ '부틸알콜',
    nvalue == '분진목분진'        ~ '목분진',
    nvalue == '파라디클로로벤젠'  ~ 'p디클로로벤젠',
    nvalue == '물리적인자진동'    ~ '진동',
    nvalue == '광물성'            ~ '광물성분진',
    nvalue == '아세트산에톡시에틸에톡시에틸아세테이트'    ~ '에틸아세테이트',
    nvalue == '초산메톡시에틸에틸렌글리콜모노메틸에테르아세테'    ~ '메톡시에탄올',
    nvalue == '부틸알코올'    ~ '부틸알콜',
    nvalue == '수은아릴화합물'    ~ '수은',
    nvalue == '물리적인자진동'    ~ '진동',
    nvalue == '진동그밖의진동기계'    ~ '진동',
    nvalue == '진동착암기사용작업'    ~ '진동',
    nvalue == '휘발성콜타르피치함유물질'  ~ '휘발성콜타르피치',
    nvalue == '황화니켈함유물질'  ~ '니켈',
    nvalue == '엑스선'            ~ '방사선',
    nvalue %in% c('활석석면불포함','활석석면포함','규산염활석')    ~ '활석',
    nvalue == '유리섬유분진'      ~ '유리섬유',
    nvalue == '아닐린아미노벤젠그동족체'      ~ '아닐린',
    nvalue == '에폭시프로판'      ~ '에폭시프로판올',
    nvalue == '히드로퀴논'      ~ '하이드로퀴논',
    nvalue == '연실납'      ~ '납',
    nvalue == '헥산n헥산'      ~ '노말헥산',
    nvalue == '크실렌오르토'      ~ '크실렌',
    nvalue == '아닐린아미노벤젠그동족체'      ~ '아닐린',
    nvalue == '산화철분진흄'      ~ '철',
    nvalue == '파라이성체'      ~ '크실렌',
    nvalue == '규산석영'      ~ '석영',
    nvalue == '에틸알콜'      ~ '에탄올',
    nvalue == 'IPA'      ~ '이소프로필알콜',
    nvalue == '초산메틸'      ~ '메틸아세테이트',
    nvalue == '초산에틸'      ~ '에틸아세테이트',
    nvalue == '디히도록시벤젠'      ~ '하이드로퀴논',
    nvalue == '염화수소염산'      ~ '염화수소',
    nvalue == '크레졸모든이성체'      ~ '크레졸',
    nvalue == '메틸알코올'      ~ '메탄올',
    nvalue %in% c('메틸클로라이드','디클로로메탄')      ~ '염화메틸렌',
    nvalue == '초산메톡시에틸'      ~ '메톡시에틸아세테이트',
    nvalue == '셀로솔'      ~ '에톡시에탄올',
    nvalue == '초산메톡시에틸에틸렌글리콜모노메틸에테르아세테이트'      ~ '메톡시에틸아세테이트',
    nvalue == '초산메톡시에틸셀로솔브아세테이트'   ~ '메톡시에틸아세테이트',
    nvalue == '콜타르'   ~ '휘발성콜타르피치',
    nvalue == 'DMF'   ~ '디메틸포름아미드',
    nvalue == '디에틸에테르에틸에테르'   ~ '디에틸에테르',
    nvalue == '디메틸아닐린아미노디메틸벤젠'   ~ '디메틸아닐린',
    nvalue == '무수초산무수아세틱엑시드'   ~ '무수초산',
    nvalue == 'DMAC'   ~ '디메틸아세트아미드',
    nvalue == '헵탄n헵탄'   ~ '헵탄',
    TRUE ~ nvalue
  ))

# 각 유해인자는 이후 group_by count를 통해 유해물질 CAS code와 수작업으로 매칭


# 전 데이터 long file로  -------------------------------------------------------

#long만들 때 pid 제거. 검진일자 character로 변경. 빈칸은 NA로 하고 omit

st<- Sys.time()
long1 <- wide2 %>% #wide2가 기본 정보 제외한 나머지 검사 결과 전부 담은 것
  gather(-vo2, key=variable, value=values)
et <- Sys.time()
et-st

st<- Sys.time()
long2 <- long1 %>% 
  mutate('values'= ifelse(`values`=="", NA, `values`))%>%
  filter(!is.na(values))
et <- Sys.time()
et-st

fwrite(long2, '~pathway/longdata.csv') # long file 데이터 저장

# 주관식 대체값 -----------------------------------------------------------------
# 주관식 대체값은 위의 전체 long file 데이터에서 변수별로 가능한 값을 group by count로 뽑은 뒤 수작업으로 매칭함

lookup <- read_xlsx('~pathway/replace_lkupfile.xlsx', sheet = 1)



long1 <- fread('~pathway/longwithdomain.csv')

textvar <- lookup %>% 
  filter(value_source=='textdata')%>% pull(variable)


# text data를 우선 빼주고 주관식 left_join -----------------------------------------

textdata <- long1 %>%
  filter(variable %in% textvar)


fwrite(textdata, '~pathway/textdata.csv')

#long2는 text data 제외한 것 
long2 <- setdiff(long1, textdata)
lookup2 <- lookup%>%select(variable, value_source,value_code)


# left join해보기 
long3 <- long2 %>%
  rename(value=values)%>%
  left_join(lookup2, by=c('variable', 'value' = 'value_source'))


long4 <- long3 %>% 
  mutate(rep_value = ifelse(is.na(value_code), value, value_code))%>%
  select(-value_code)

fwrite(long4, '~pathway/long_replace.csv')


# DB적재 예시--------------------------------------------------------------------

# # db connect ------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(haven)
library(readxl)
library(lubridate)
library(data.table)
library(bit64)
library(knitr)
library(kableExtra)
library(openxlsx)
library(foreach)
library(iterators)
library(parallel)
library(stringr)
library(writexl)
library(DBI)
library(odbc)
library(RPostgreSQL)
library(dplyr)
library(DatabaseConnector)
library(DatabaseConnectorJars)
library(doMC)
registerDoMC(31)
getwd()
con <-connect(dbms = "postgresql", 
              connectionString = "jdbc:postgresql://192.168.0.x:5432/DBname",
              user="user", 
              password="pwd") # DB를 con으로 불러오기 

# schema create ----
# dbSendStatement(con, "drop schema cdm cascade") # CDM schema 안의 table 전부 제거
# dbSendStatement(con, "create schema cdm") #schema 생성
# dbSendStatement(con, "drop table if exists cdm.sujin") #특정 table 제거

#person ad 생성
visit_occurrence <- fread('~pathway/visit_occurrence.csv')

person_ad <-  visit_occurrence%>%
  select(visit_occurrence = visit_occurrence_id , person_id, visit_start_date) %>%
  mutate(visit_start_date = as.Date(visit_start_date))


#마스터 파일 통해 schema를 다음과 같이 숫자로 부여함

mf1 <- mf %>%
  mutate(대분류_테이블 = ifelse(대분류_테이블 =='건강진단 접수_채용', '건강진단 접수', 대분류_테이블))%>%
  mutate(대분류_테이블 = ifelse(대분류_테이블 =='건강지단 접수', '건강진단 접수', 대분류_테이블))%>%
  mutate(schema = case_when (
    대분류_테이블 == '공통 문진'     ~ '1', # common question 
    대분류_테이블 == '기본정보'      ~ '2', # sujin file
    대분류_테이블 == '야간문진'     ~ '4',  # night question
    대분류_테이블 == '특수검진 문진' ~ '5', # special question
    TRUE ~ '3' # general checkup domain 
  ))

cq <- long_d %>%
  filter(schema == '1') %>% #schema 
  rename(var = variable, val = value) %>%
  left_join(person_ad, by =c('vo'='visit_occurrence')) %>% 
  select(-schema,-atlasdom)%>%
  rename(visit_occurrence = vo)  

st <- Sys.time()
dbWriteTable(con, c("cdm.common_question"), value = cq) # cq라는 data.frame을 cdm.common_question에 적재
et <- Sys.time()
et-st


# DB 데이터 추출 쿼리 예시 ---------------------------------------------------------

cq <- dbGetQuery(con, "SELECT * FROM cdm.COMMON_QUESTION 
                 WHERE var in ('f_hist_g4','hist_g3','hist_g4','hist_g5','hist_g10','hist_g11','hist_g12',
                 'smok_g1','smok_g2','smok_g3','smok_g4','smok_g5',
                 'exe_g4','exe_g5','exe_g6','exe_g7','exe_g8','hb_g1')") # 괄호 안의 변수값만 추출
