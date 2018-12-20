rm(list = ls())
library(shiny)
library(shinydashboard)
library(shinyjs)
library(googleVis)
library(flexdashboard)
library(DT)
library(dimple) #devtools::install_github("Bart6114/dimple")
library(dplyr)




ui1 <-tagList(
  div(id = "login",
      wellPanel(textInput("userName", "Username"),
                passwordInput("passwd", "Password"),
                br(),actionButton("Login", "Log in"))),
  tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
)

ui2<- dashboardPage(
  dashboardHeader(title="S-League X Shoot!"),
  dashboardSidebar(
    gaugeOutput("plt1",height='130px'),
    sidebarMenu(
      menuItem("Shoot 소개", tabName = "shoot_info", icon= icon("heart", lib= "glyphicon")),
      menuItem("점수순위 및 분석", tabName = "leaderboard", icon= icon("bar-chart-o")),
      menuItem("참가신청서", tabName = "signup", icon=icon("pencil", lib= "glyphicon"),
               badgeLabel = "관리자", badgeColor = "red")
    ),
    uiOutput("checkbox")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "shoot_info",
              fluidRow(
                tabBox(
                  id= "tabtab1", width = 12,
                  tabPanel("Shoot 소개",
                           fluidRow(div(img(src="1.jpg"), img(src="2.jpg"), img(src="3.jpg"))),
                           fluidRow(div(img(src="4.jpg"), img(src="5.jpg"), img(src="6.jpg")))
                  ),
                  tabPanel("소아암 소개 및 후원",
                           fluidRow(tags$a(img(src="66.jpg"),href="http://www.soaam.or.kr/donation/introduction.php?PHPSESSID=80f03a3e88d2ee7137d904c22e00a75b")),
                           fluidRow(div(img(src="11.jpg"))),
                           fluidRow(div(img(src="22.png"))),
                           fluidRow(div(img(src="33.png"))),
                           fluidRow(div(img(src="44.png"))),
                           fluidRow(div(img(src="55.png")))
                  ),
                  tabPanel("2016년도 Shoot 활동",
                           fluidRow(div(img(src="111.jpg"))),
                           fluidRow(div(img(src="222.jpg"))),
                           fluidRow(div(img(src="333.jpg"))),
                           fluidRow(div(img(src="444.jpg"))),
                           fluidRow(div(img(src="555.jpg"))),
                           fluidRow(div(img(src="666.jpg")))
                  )
                )
              )
      ),
      tabItem(tabName = "leaderboard",
              fluidRow(
                tabBox(
                  id= "tabtab2", width = 12,
                  tabPanel("선수별순위",
                           dataTableOutput("content"),
                           dimpleOutput("distPlot1"),
                           width=12),
                  tabPanel("팀별순위",
                           uiOutput("summa2"), 
                           dimpleOutput("distPlot2"),
                           width=6),
                  tabPanel("단과대별순위",
                           uiOutput("summa3"), 
                           dimpleOutput("distPlot3"),
                           width=6)
                )
              )
      ),
      tabItem(tabName = "signup",
              uiOutput("page") #This is the only difference between ui2 and ui3
      )
      
    ))
)
ui3<- box(fluidRow(
  tabBox(
    id= "tabset1", width = 12,
    tabPanel("참가신청서", textInput("name",  "이름"),
             radioButtons("gender", "성별", list("남자","여자")),
             selectInput("college", "대학",
                         choices = list("간호대학", "경영대학",
                                        "공과대학", "농업생명과학대학",
                                        "미술대학", "법과대학",
                                        "사범대학", "사회과학대학",
                                        "수의과대학", "생활과학대학",
                                        "약학대학", "음악대학",
                                        "인문대학", "의과대학",
                                        "자연과학대학", "기타"),
                         selected = 1),
             selectInput("team", "교내 소속축구팀",
                         choices = list("싸커21", "아르마다",
                                        "에코플러스", "아크로",
                                        "P.O.S", "공대",
                                        "자연대", "관악사",
                                        "농대축구부 휘모리", "지오싸카스",
                                        "새츠", "샥스",
                                        "FC SEES", "Cells United",
                                        "프리템포", "남풍",
                                        "없음")),
             textInput("score", "점수"),
             actionButton("click_counter","Submit"), width=12),
    tabPanel("참가자 삭제", textInput("delete_name", "삭제할 참가자 이름을 아래 박스에 기입한 뒤, 삭제 버튼을 눌러주세요."),
             actionButton("delete_button","삭제"),
             h4("주의사항: 동명이인이 있을시, 모두가 삭제되므로 삭제하지 않고자 하는 참가자의 정보를 다시 '참가신청서' tab에서 기입해줘야 함."),width=12)
  )
),
fluidRow(
  box(dataTableOutput("nText"), width=12)
)
)


server <- shinyServer(function(input, output, session) {
  
  Logged = FALSE;
  my_username <- "test"
  my_password <- "test"
  
  USER <- reactiveValues(Logged = Logged)
  
  
  
  observe({ 
    #print("Observe")
    if (USER$Logged == FALSE) {
      #print("1")
      if (!is.null(input$Login)) {
        #print("2")
        if (input$Login > 0) {
          #print("Username")
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          #print(sprintf("Username:%s Pass:%s",Username,Password))
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              #print("Success")
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  
  output$page <- renderUI({
    #print("output$page called")
    if (USER$Logged == FALSE){
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1)))
      })
    }
    if (USER$Logged == TRUE)
      ui3
  })
  
  output$page <- renderUI({
    if (USER$Logged==TRUE){
      return({  div(class="outer",do.call(bootstrapPage,c("",ui3)))  })
    } else {
      return({  div(class="outer",do.call(bootstrapPage,c("",ui1)))    })
    }
  })
  
  
  
  observe({
    values<- reactiveValues(df = NULL)
    money<- reactiveValues(a=0)
    
    
    observeEvent(input$click_counter, {
      
      # Button 클릭시, 1000원씩 적립
      money$a<- money$a+1000
      
      # create df
      name<- input$name
      gender<- input$gender
      college<- input$college
      team<- input$team
      score<- as.numeric(input$score)
      rank<- 0
      
      new_row<- data.frame(rank,name,college,gender,team,score, stringsAsFactors = FALSE)
      
      values$df<- rbind(values$df, new_row)
      values$df<- values$df[order(values$df$score, decreasing=TRUE),]
      values$df$rank<- 1:nrow(values$df)
    })
    
    observeEvent(input$delete_button, {
      values$df<- values$df[-c(values$df$name==input$delete_name),]
    })
    
    output$nText<- renderDataTable({
      values$df}, options = list(orderClasses = TRUE,
                                 lengthMenu = c(5, 10, 30), pageLength = 5), rownames=FALSE)
    
    output$content<- renderDataTable({
      values$df}, options = list(orderClasses = TRUE,
                                 lengthMenu = c(5, 10, 30), pageLength = 10), rownames=FALSE)
    
    
    output$summa1 <- renderPrint({summary(values$df$score)})
    
    output$distPlot1<- renderdimple({
      name <- group_by (values$df, score)
      dd<- as.data.frame(summarise(name, n()))
      colnames(dd)<- c("score","count")
      
      dimple(dd,
             xCategory="score",
             xOrderRule="score",
             yMeasure="count")
    })
    
    df2<- data.frame()
    output$summa2 <- renderGvis({
      df2<<- df2[0,]
      for (team_name in unique(values$df$team)){
        rank <- 0
        team <- team_name
        score <- format(mean(values$df[values$df$team==team_name,]$score), digits=3)
        
        new_row<- data.frame(rank, team, score, stringsAsFactors = FALSE)
        
        df2 <<- rbind(df2, new_row)
        df2 <<- df2[order(as.numeric(df2$score), decreasing=TRUE),]
        df2$rank <<- 1:nrow(df2)
      }
      return(gvisTable(df2[order(df2$score, decreasing=TRUE),]))
    })
    
    dff2<- data.frame()
    output$distPlot2 <- renderdimple({
      dff2<<- dff2[0,]
      for (team_name in unique(values$df$team)){
        rank <- 0
        team <- team_name
        score <- format(mean(values$df[values$df$team==team_name,]$score), digits=3)
        
        new_row<- data.frame(rank, team, score, stringsAsFactors = FALSE)
        
        dff2 <<- rbind(dff2, new_row)
        dff2 <<- dff2[order(as.numeric(dff2$score), decreasing=TRUE),]
        dff2$rank <<- 1:nrow(dff2)
      }
      dimple(dff2,
             yCategory="team",
             yOrderRule="score",
             xMeasure="score")
    })
    
    
    df3<- data.frame()
    output$summa3 <- renderGvis({
      df3<<- df3[0,]
      for (college_name in unique(values$df$college)){
        rank <- 0
        college <- college_name
        score <- format(mean(values$df[values$df$college==college_name,]$score), digits=3)
        
        new_row<- data.frame(rank, college, score, stringsAsFactors = FALSE)
        
        df3 <<- rbind(df3, new_row)
        df3 <<- df3[order(as.numeric(df3$score), decreasing=TRUE),]
        df3$rank <<- 1:nrow(df3)
      }
      return(gvisTable(df3[order(df3$score, decreasing=TRUE),]))
    })
    
    dff3<- data.frame()
    output$distPlot3 <- renderdimple({
      dff3<<- dff3[0,]
      for (team_name in unique(values$df$team)){
        rank <- 0
        team <- team_name
        score <- format(mean(values$df[values$df$team==team_name,]$score), digits=3)
        
        new_row<- data.frame(rank, team, score, stringsAsFactors = FALSE)
        
        dff3 <<- rbind(dff3, new_row)
        dff3 <<- dff3[order(as.numeric(dff3$score), decreasing=TRUE),]
        dff3$rank <<- 1:nrow(dff3)
      }
      dimple(dff3,
             yCategory="team",
             yOrderRule="score",
             xMeasure="score")
    })
    
    output$plt1 <- renderGauge({
      gauge(money$a, min = 0, max = 300000, symbol = '원', abbreviate=FALSE, label = paste("모금액"),gaugeSectors(
        success = c(0,300000)))
    })
    
  })
})

shinyApp(ui = ui2, server = server)