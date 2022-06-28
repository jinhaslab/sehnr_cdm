
# sever start -------------------------------------------------------------

server = function(input, output, session){
  
  # 업종별 연령표준화 입원률(발생) -------------------------------------------------------
  
  # reactive values ----
  values <- reactiveValues(
    basic = data.frame(),
    basic_table = data.frame(), 
    group_table  = data.frame()
  )
  
  
  # observed create ---------------------------------------------------------
  
  # observed create end -----
  
  # output values ----
  options(DT.options = list(pageLength = 25))
  # ouput basic table ----
  # 작업환경실태조사 ----------------------------------------------------------------
  output$db_table1 <- DT::renderDataTable(DT::datatable({
    
    
    data_db1_2 <- data_db1 %>% filter(업종 %in% input$업종,
                                      유해물질명 %in% input$유해물질명)%>%select(-`업종대분류`)
    
    data_db1_2 %>%
      arrange(desc(`노출률(취급자/총근로자, 건)`)) %>%
      arrange(desc(`업종`))
  }, options = list(
    pageLength = 30,
    lengthMenu = c(30, 50, 100))
  ))
  
  
  # pickerinput 공정명, 유해물질명 업데이트하기 -------------------------------------------
  
  observeEvent(c(input$업종), {
    updatePickerInput(session = session, inputId = "유해물질명",
                      choices = sort(unique(data_db1 %>% filter(업종%in% input$업종) %>% pull(유해물질명))),
                      selected = sort(unique(data_db1 %>% filter(업종%in% input$업종) %>% pull(유해물질명)))
    )
    
  }, ignoreInit = TRUE)
  

  output$db_table2 <- DT::renderDataTable(DT::datatable({
    
    data_db2_1 <- data_db2[data_db2$유해물질명 %in% input$`유해물질1`,]
    
    data_db2_1 %>%
      arrange(desc(`유해물질노출수`)) 
  }, options = list(
    pageLength = 30,
    lengthMenu = c(30, 50, 100))
  ))
  
  
  output$plot1 <- renderPlot({
    
    ##
    plot_db1 <- data_db1[data_db1$유해물질명 %in% input$`유해물질2`,]
    
    plot_db1 %>% 
      ggplot(aes(y =`분류내 총 근로자(건, 비노출자 포함)`, 
                 x = `노출률(취급자/총근로자, 건)`, 
                 color = 유해물질명)) +
      geom_point(size = 2.5) +
      theme_classic()+
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title  = element_text(size = 15),
            strip.text  = element_text(size = 15),
            legend.text = element_text(size = 15))+
      geom_label_repel(aes(label = `업종`), 
                       fill = NA, # 투명하게 해서 겹쳐도 보이게
                       alpha =1, size = 4, # 작게
                       box.padding = 0.4,  # 분별해서
                       segment.size =0.1,  # 선 
                       force = 2) +
     stat_ellipse(aes(fill=`유해물질명`),
                   geom="polygon", level=0.95, alpha=0.1)+
    scale_x_continuous(trans='log10')+
      scale_y_continuous(trans='log10')
    ##
  })
  
  output$plot2 <- renderPlot({
    
    ##
    
    plot_db2 %>% filter(`유해물질명` %in% input$`유해물질3`)%>%
      ggplot(aes(y =`분류내 총 근로자(건, 비노출자 포함)`, 
                 x = `노출률(취급자/총근로자, 건)`, 
                 color = `업종대분류`)) +
      geom_point(size=2.5) +
      theme_classic()+
      geom_label_repel(aes(label = `업종`), 
                       fill = NA, # 투명하게 해서 겹쳐도 보이게
                       alpha =1, size = 4, # 작게
                       box.padding = 0.4,  # 분별해서
                       segment.size =0.1,  # 선 
                       force = 2) +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title  = element_text(size = 15),
            strip.text  = element_text(size = 15),
            legend.text = element_text(size = 15))+
      facet_wrap( `유해물질명` ~ ., ncol =2)
    ##
  })
  
  output$plot3 <- renderPlot({
    
    ##
    
    plot_db3 %>% filter(`업종대분류` %in% input$`업종대분류`)%>%
      group_by(`업종대분류`)%>%
      top_n(10, `노출된 근로자(건)`)%>%
      ungroup()%>%
      filter(!`유해물질명` %in% c('생애검진','일반검진'))%>%
      mutate(`유해물질명` = fct_reorder(`유해물질명`, desc(`업종대분류별 유해인자노출률`)))%>%
      ggplot(aes(y = `업종대분류별 유해인자노출률`, 
                 x = `유해물질명`, 
                 fill = `유해물질명`)) +
      geom_bar(stat='identity') +
      theme_classic()+
      scale_y_continuous(trans='sqrt')+
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title  = element_text(size = 15),
            strip.text  = element_text(size = 15),
            legend.text = element_text(size = 15))+
      facet_wrap(. ~ `업종대분류`, ncol=2)
    
    ##
  })
  
}
  
  
  

