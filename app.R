library(shiny)
library(data.table, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rgdal, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(CARBayes,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(caret,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(coda, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)


# 0. load essential data needed

setwd("C:/Users/qtuej/Desktop/DMV_2021/newdir/")

## (1) 지도에 채워 넣을 수치 데이터 및 seoul shp
### a. 최종 데이터
data <- fread("final_data_agg.csv",
              data.table = T, drop = c(1,2))


dat_shiny <- data %>% 
  select(-c(city,gu)) %>%
  select(-contains("adstrd")) # shiny app 만들기위한 데이터

dat_col <- colnames(dat_shiny)[-c(1,2)] #행정동이름, id제외, 변수이름 선택지에 들어갈것



### b. seoul shp 와 병합. 사전가공하여 서울만 따로 떼놓은 shp 파일

map <- readOGR("seoul_shp")
map_df <- fortify(map, region = "id")
map_df$id <- as.numeric(map_df$id)
map_merged <- map_df %>% left_join(data,by = "id") 


cnames <- data.frame(long = coordinates(map)[,1],
                     lat = coordinates(map)[,2],
                     ad_dong = map$ADM_DR_NM) %>%
  left_join(data,by = "ad_dong") # plotting에 쓰일 행정동별 중심좌표와 수치


#.1 UI part

ui <- fluidPage(
  
  titlePanel(
    h1(strong("행정동별 만족도지수 및 데이터"), style = "font-size:30px;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      helpText(
        h5("행정동별 각종 수치와 베이지안 회귀로 추정된",
           br(),
           "만족도지수를 볼 수 있습니다.")),
      hr(),
      selectInput("hdong",
                  label = "행정동 선택",
                  choices = data$ad_dong,
                  selected = data$ad_dong[1]),
      
      selectInput("var",
                  label = "변수 선택",
                  choices = dat_col,
                  selected = dat_col[1]),
      
    ),
    mainPanel(plotOutput("map_disp"))
  ),
  
  hr(),
  h2("변수별 분포 히스토그램" %>% strong()),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "bins",
                  label = "히스토그램의 막대 개수",
                  choices = c(10, 20, 35, 50),
                  selected = 20),
      
      selectInput("var2",
                  label = "변수 선택",
                  choices = setdiff(dat_col,c("cluster","highsat")),
                  selected = setdiff(dat_col,c("cluster","highsat"))[1]),
      
      plotOutput(outputId = "main_plot", height = "300px")
                       
      ),
    mainPanel(plotOutput("hist")))
  )


# 2. Server

server <- function(input, output){
  
  hdongInput <- reactive({
    data[hdong==input$hdong]
  })
  
  
  output$map_disp <- renderPlot({
    ggplot() + 
      geom_polygon(data = map_merged,
                   aes(x = long,y = lat, group = id,fill = .data[[input$var]]),
                   color = "grey")+
      theme(panel.background = element_rect(fill = "white", color ="#a9cd07"),
            legend.position = "none",
            plot.title = element_text(size=12, face="bold.italic"))+
      scale_fill_continuous(low="#EAFFB6", high="#014700")+
      labs(title=input$var,
           x = "",
           y = "")+
      geom_polygon(data = subset(map_merged,ad_dong == input$hdong),
                   aes(x = long,y = lat, group = id,fill = input$var),
                   fill = "#FF9000",
                   alpha = 0.2,
                   color = "#FF9000") +
      geom_text(data = cnames %>% filter(ad_dong == input$hdong),
                aes(x = long, y = lat +0.025, label = ad_dong), color =  "black", size = 4.5)+
      geom_text(data = cnames %>% filter(ad_dong == input$hdong),
                aes(x = long, y = lat +0.01, label = .data[[input$var]] %>% round(3)),
                color =  "black", size = 5)
    
  })
  
  output$hist <- renderPlot({
    ggplot(data = dat_shiny %>% select(-c(ad_dong,id)),
           aes(x=.data[[input$var2]]))+
      geom_histogram(fill="#00AFBB",
                     colour="#00AFBB",
                     alpha = 0.25,
                     bins = as.numeric(input$bins))+
      theme_classic() +
      scale_y_continuous(breaks = NULL)+
      labs(title=paste0("Distribution of ",input$var2),
           x ="")+theme_classic()+
      theme(
        plot.title = element_text(size=5, face="bold.italic"))
    
  })
}


# 3. ShinyApp defining


shinyApp(ui,server)

