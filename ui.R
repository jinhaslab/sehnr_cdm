
# ui start ----------------------------------------------------------------
ui = dashboardPage(
  
  
  
  skin = "green",
  
  # header ------------------------------------------------------------------
  header = dashboardHeader(
    title = "업종별 유해물질 2021" 
  ),
  # sidebar -----------------------------------------------------------------
  sidebar = dashboardSidebar(
    sidebarMenu(
      # sidebar menu ----
      
      menuItem("특수검진 노출 CDM", icon = icon("building"),
               # mensubitem으로 세분화 -----
               menuSubItem("업종별 유해인자", icon = icon("bars"), tabName = "b1"), 
               menuSubItem("유해인자 총괄표", icon = icon("bars"),   tabName = "b2"), 
               menuSubItem("시각화1", icon = icon("bars"),   tabName = "b3"),
               menuSubItem("시각화2", icon = icon("bars"),   tabName = "b4"),
               menuSubItem("시각화3", icon = icon("bars"),   tabName = "b5")
               
      )
    )    
    
  ), 
  
  # body --------------------------------------------------------------------
  body = dashboardBody(
    tabItems(
      tabItem(tabName = 'b1',
              fluidRow(
                box(width = 12,
                    column(4,
                           pickerInput("업종",
                                       "업종: ",
                                       sort(unique(as.character(data_db1$`업종`))), 
                                       multiple = TRUE,
                                       selected = sort(unique(as.character(data_db1$`업종`))), 
                                       options = pickerOptions(actionsBox = TRUE,
                                                               liveSearch = TRUE,
                                                               selectAllText = '모두 선택',
                                                               deselectAllText = '모두 비선택',
                                                               noneSelectedText = '선택 필요',
                                                               multipleSeparator = " | "
                                       )
                           )
                    ), 
                    column(4,
                           pickerInput("유해물질명",
                                       "유해물질명: ",
                                       sort(unique(as.character(data_db2$`유해물질명`))), 
                                       multiple = TRUE,
                                       selected = sort(unique(as.character(data_db2$`유해물질명`))),
                                       options = pickerOptions(actionsBox = TRUE,
                                                               liveSearch = TRUE,
                                                               selectAllText = '모두 선택',
                                                               deselectAllText = '모두 비선택',
                                                               noneSelectedText = '선택 필요',
                                                               multipleSeparator = " | "
                                       )
                           )
                    )
              
              
      )),
    
      fluidRow(
        DT::dataTableOutput("db_table1")
        )
      ) ,
# tabitem 2 ----      
      tabItem(tabName = 'b2',
             fluidRow(
               box(width = 6,
                   column(12,
                          pickerInput("유해물질1",
                                      "유해물질명: ",
                                      sort(unique(as.character(data_db2$`유해물질명`))), 
                                      multiple = TRUE,
                                      selected = sort(unique(as.character(data_db2$`유해물질명`))),
                                      options = pickerOptions(actionsBox = TRUE,
                                                              liveSearch = TRUE,
                                                              selectAllText = '모두 선택',
                                                              deselectAllText = '모두 비선택',
                                                              noneSelectedText = '선택 필요',
                                                              multipleSeparator = " | "
                                      )
                          )
                   )
               )
               
             ) ,
             fluidRow(
               DT::dataTableOutput("db_table2")
             )
             
             
    )
#tab item 3 
,
tabItem(tabName = 'b3',
        fluidRow(
          box(width = 6,
              column(12,
                     pickerInput("유해물질2",
                                 "유해물질명: ",
                                 sort(unique(as.character(data_db1$`유해물질명`))), 
                                 multiple = TRUE,
                                 selected = NULL,
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         selectAllText = '모두 선택',
                                                         deselectAllText = '모두 비선택',
                                                         noneSelectedText = '선택 필요',
                                                         multipleSeparator = " | "
                                 )
                     )
              )
          )
          
        ) ,
        div(style = "height: 77vh;",
            plotOutput("plot1", height='100%'))
        
)
##### tabitem 4
,
tabItem(tabName = 'b4',
        fluidRow(
          box(width = 6,
              column(12,
                     pickerInput("유해물질3",
                                 "유해물질명: ",
                                 sort(unique(as.character(plot_db2$`유해물질명`))), 
                                 multiple = TRUE,
                                 #selected = NULL,
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         selectAllText = '모두 선택',
                                                         deselectAllText = '모두 비선택',
                                                         noneSelectedText = '선택 필요',
                                                         multipleSeparator = " | "
                                 )
                     )
              )
          )
          
        ) ,
        div(style = "height: 77vh;",
            plotOutput("plot2", height='100%'))
        
        
)
## tab Item 5

,
tabItem(tabName = 'b5',
        fluidRow(
          box(width = 6,
              column(12,
                     pickerInput("업종대분류",
                                 "업종대분류: ",
                                 sort(unique(as.character(plot_db3$`업종대분류`))), 
                                 multiple = TRUE,
                                 #selected = NULL,
                                 options = pickerOptions(actionsBox = TRUE,
                                                         liveSearch = TRUE,
                                                         selectAllText = '모두 선택',
                                                         deselectAllText = '모두 비선택',
                                                         noneSelectedText = '선택 필요',
                                                         multipleSeparator = " | "
                                 )
                     )
              )
          )
          
        ) ,
        div(style = "height: 77vh;",
          plotOutput("plot3", height='100%')
        )
        
        
)
###
    )
  )
)
  


