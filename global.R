library(shiny); library(shinydashboard); library(tidyverse);library(dplyr); library(ggplot2);  
library(plotly); library(DT); library(readxl); library(shinyWidgets); library(feather);
library(ggrepel)

## global, server, ui 3가지 파일을 통해 shinyapp 구동 가능
## prob_tab은 업종분류별 유해물질 취급 근로자의 수와 분율 등을 포함한 데이터임.
## domexp는 업종대분류에 따른 같은 형식의 테이블임. 


# 작업환경실태조사 global ---------------------------------------------------------
data_db <-feather::read_feather('~pathway/prob_tab.feather')
data_db1 <- data_db %>%
  select(`업종`   = subdm, 
         `유해물질명` = exposure, 
         `취급근로자(건)` = n, 
         `분류내 총 근로자(건, 비노출자 포함)` = pop, 
         `노출률(취급자/총근로자, 건)` = prob,
         `업종대분류`              = domain                 
  )

  
#tpop <- data_db %>% select(subdm, pop, n) %>%
#  mutate(tpop = sum(n)) 
# 398219 명 
data_db2 <- data_db  %>%
  group_by(exposure) %>%
  summarize(tpop = sum(n),
    tprob = round(sum(n)/398219 *100, 2)) %>%
  select(`유해물질명` = exposure,
         `유해물질노출수` = tpop, 
         `관리대상률(유해물질노출수/총검진수)` = tprob)

  
plot_db2 <- data_db1  %>%
  filter(`노출률(취급자/총근로자, 건)` >= 0.5)


plot_db3 <- feather::read_feather('~pathway/domexp.feather')%>%
  select(`업종대분류` = domain,
         `유해물질명` = exposure,
         `노출된 근로자(건)` = people,
         `분류 내 총근로자(건)` = total,
         `업종대분류별 유해인자노출률` = prob)




## testplot

# data_db1 %>% filter(`유해물질명` %in% c('소음', '벤젠')) %>%
#  ggplot(aes(y = log(`분류내 총 근로자수(비노출자 포함)`), 
#             x = log(`노출률(취급자/총근로자)`), 
#              color = `유해물질명`)) +
#   geom_point() +
#   theme_classic()+
#   geom_label_repel(aes(label = `업종`), 
#                    fill = NA, # 투명하게 해서 겹쳐도 보이게
#                   alpha =1, size = 3, # 작게
#                    box.padding = 0.4,  # 분별해서
#                    segment.size =0.1,  # 선 
#                   force = 2) +
# stat_ellipse(aes(fill=`유해물질명`),
#              geom="polygon", level=0.95, alpha=0.2)
# 

##testplot2 
# 
# data_db1 %>% filter(`유해물질명` %in% c('소음', '벤젠','톨루엔','기타광물성분진')) %>%
#   ggplot(aes(y = log(`분류내 총 근로자수(비노출자 포함)`), 
#              x = log(`노출률(취급자/총근로자)`), 
#              color = `업종대분류`)) +
#   geom_point() +
#   theme_classic()+
#   geom_label_repel(aes(label = `업종`), 
#                    fill = NA, # 투명하게 해서 겹쳐도 보이게
#                    alpha =1, size = 3, # 작게
#                    box.padding = 0.4,  # 분별해서
#                    segment.size =0.1,  # 선 
#                    force = 2) +
#   facet_wrap(. ~ `유해물질명`)

#class(data_db1$`노출률(취급자/총근로자)`)


