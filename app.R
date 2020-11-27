install.packages("shinythemes")
setwd("/Users/ryancrenny/Documents/GitHub/github-repos/senior-project-nbastatstuff-master")
library(shiny)
library("nbastatR")
library(tidyverse)
library(plotly)
library(shinythemes)
library(shinyjs)

playerTable <- read_csv("playerdata.csv")
teamTable <- read_csv("teamdata.csv")
playerBaseTable <- read_csv("playerbasedata.csv")
playerTeam<- read_csv("playerwithteam.csv")
hist3<- read_csv("historical3pTest.csv")


assign_nba_teams()

df_new <- left_join(teamTable, df_dict_nba_teams)
df_curr <- filter(df_new, yearSeason == 2020)

teamTable[teamTable == "LA Clippers"] <- "Los Angeles Clippers"
teamTable[teamTable == "New Jersey Nets"] <- "Brooklyn Nets"
teamTable[teamTable == "Charlotte Bobcats"] <- "Charlotte Hornets"
teamTable[teamTable == "Vancouver Grizzlies"] <- "Memphis Grizzlies"
teamTable[teamTable == "Washington Bullets"] <- "Washington Wizards"
teamTable[teamTable == "Seattle SuperSonics"] <- "Oklahoma City Thunder"
teamTable[teamTable == "New Orleans Hornets"] <- "New Orleans Pelicans"
teamTable[teamTable == "New Orleans/Oklahoma City Hornets"] <- "New Orleans Pelicans"

currTeams <- teamTable %>% 
  filter(yearSeason == 2020)


ui <- fluidPage(theme=shinytheme("superhero"),
                useShinyjs(),
  radioButtons(inputId = "stat",label = "Choose stat",choices = list("Offensive Rating" = 1,"Defensive Rating" = 2, "Net Rating"=3),
               selected = 1),
  tabsetPanel(
    tabPanel("Player",
      fluidRow(column(width = 6, 
        htmlOutput("picture"),
        textInput(inputId = "player", label = "Player One"),
        textOutput("boxScore"),
        actionButton(inputId = "enter",label = "Get Player 1 Chart")
      ),
      column(6,
        htmlOutput("picture2"),
        textInput(inputId = "player2", label = "Player Two"),
        textOutput("boxScore2"),
        actionButton(inputId = "enter2",label = "Get Player 2 Chart")
      ))
    ),
    
    tabPanel("Team",
      fluidRow(column(width = 6,
        htmlOutput("picteam1"),
        selectInput(inputId = "team", label = "Team One", choices = currTeams$nameTeam),
        actionButton(inputId = "enterTeam",label = "Get Team 1 Chart")
      ),
      column(6,
        htmlOutput("picteam2"),
        selectInput(inputId = "team2", label = "Team Two", choices = currTeams$nameTeam),
        actionButton(inputId = "enterTeam2",label = "Get Team 2 Chart")
      ))
    ),
    
    tabPanel("Historical Trends",
             actionButton(inputId = "histEnter",label = "Get Historical 3 Point Data"),
             fluidRow(plotlyOutput('plot1')),
             fluidRow(plotlyOutput('plot3'))
             
             
             
    ),
    tabPanel("Customizable Player Scatter Plots",
             selectInput(inputId = "histYear", label = "Select a Year", choices = c(1997:2020)),
             selectInput(inputId = "xInput", label = "Select x axis", choices = list("Net Rating"=1, "Field Goals Made"=2)),
             selectInput(inputId = "yInput", label = "Select y axis", choices = list("Net Rating"=1, "Field Goals Made"=2, "Assist:Turnover Ratio"=3)),
             sliderInput(inputId="minuteSlide", label="Players Shown by Minutes Played", min=0, max=300, value=150),
             actionButton(inputId = "compEnter",label = "Get Comparison Chart"),
             fluidRow(plotlyOutput('plot2')),
             textOutput("Error")
             
             )
    
  ),

  splitLayout(
    plotlyOutput(outputId = "chart"), plotlyOutput(outputId = "chart2")
  )
)


  

  
  

server <- function(input, output) {
  
  observeEvent(input$histEnter,{
    output$plot1 <- renderPlotly(
      ggplotly(ggplot(hist3) +
                 geom_point(aes(x=SeasonStart,y=total3pa)) +
                 ggtitle("Total 3 Pointers Attempted by Year") +
                 annotate(geom="text", x=1996, y=44470, label="1995-1997 3p Line Moved in to uniform 22ft",
                          color="blue", size=3)+
                 scale_x_continuous(breaks = c(1980:2017), limits = c(1980,2017)) +
                 labs(x="Year", y="3 Pointers Attempted"))
    )
    output$plot3 <- renderPlotly(
      ggplot(hist3)+geom_point(aes(x=SeasonStart, y=average3p))+
        ggtitle("3 Point Percentage by Year")+
        scale_x_continuous(breaks = c(1980:2017), limits = c(1980,2017)) +
        scale_y_continuous(breaks = c(.2:.4), limits = c(.2,.4))+
        
        labs(x="Year", y="3 Point Percentage")
      
        
       
  
        
    )
  })
  
  observeEvent(input$compEnter,{
    hide("chart")
    hide("chart2")
    if(input$xInput==1 && input$yInput==2){
      yearly_df <- playerTable %>% 
        filter(yearSeason == input$histYear & minutesRank < input$minuteSlide & netrtg != 0 & fgm != 0)
      
      tempplot1 <- ggplot(yearly_df, aes(x=netrtg,y=fgm)) +
        geom_point(aes(text = namePlayer)) +
        labs(x="Net Rating", y="Field Goals Made")
    }
    if(input$xInput==1 && input$yInput==3){
      yearly_df <- playerTable %>% 
        filter(yearSeason == input$histYear & minutesRank < input$minuteSlide & netrtg != 0 & ratioASTtoTO != 0)
      
      tempplot1 <- ggplot(yearly_df, aes(x=netrtg,y=ratioASTtoTO)) +
        geom_point(aes(text = namePlayer)) +
        labs(x="Net Rating", y="Assist to TO Ratio")
    }
    if(input$xInput==2 && input$yInput==3){
      yearly_df <- playerTable %>% 
        filter(yearSeason == input$histYear & minutesRank < input$minuteSlide & fgm != 0 & ratioASTtoTO != 0)
      
      tempplot1 <- ggplot(yearly_df, aes(y=ratioASTtoTO,x=fgm)) +
        geom_point(aes(text = namePlayer)) +
        labs(x="Field Goals Made", y="Assist to TO Ratio")
    }
   
    if(input$xInput==input$yInput){
      output$errror <- renderText("Stat types cannot be the same")
    }
    else{
    output$plot2 <- renderPlotly(
      ggplotly(tempplot1, dynamicTicks = T)
    )
    }
  })
  
  observeEvent(input$enter, {
    output$chart <- renderPlotly({
      specPlayer <- playerTable %>% 
        filter(namePlayer == input$player)
      specYear <- playerTable %>% 
        group_by(yearSeason) %>% 
        summarize(avgortg = mean(ortg), avgdrtg = mean(drtg), avgnet = mean(netrtg))
      total = full_join(specPlayer, specYear)
      
      yearHigh <- specPlayer$yearSeasonLast[1] + 1
      yearLow <- specPlayer$yearSeasonFirst[1]
      src <- specPlayer$urlPlayerThumbnail[1]
      
     if (input$stat == 1){
       ggplotly(ggplot(total) +
       geom_line(aes(x=yearSeason,y=ortg), color = "blue") +
       geom_line(aes(x=yearSeason,y=avgortg), color = "red") +
         ggtitle("League Average-Red Line")+
       scale_x_continuous(breaks = c(yearHigh:yearLow), limits = c(yearLow+1,yearHigh))+
       scale_y_continuous(limits=c(90,130))+
       labs(x="Year", y="Offensive Rating"))
     }
     else if (input$stat == 3){
       ggplotly(ggplot(total) +
         geom_line(aes(x=yearSeason,y=netrtg), color = "blue") +
         geom_line(aes(x=yearSeason,y=avgnet), color = "red") +
         ggtitle("League Average-Red Line")+
         scale_x_continuous(breaks = c(yearHigh:yearLow), limits = c(yearLow+1,yearHigh))+
         scale_y_continuous(limits=c(-20,20))+
         labs(x="Year", y="Net Rating"))
     }
     else{
       ggplotly(ggplot(total) +
         geom_line(aes(x=yearSeason,y=drtg), color = "blue") +
         geom_line(aes(x=yearSeason,y=avgdrtg), color = "red") +
         ggtitle("League Average-Red Line")+
         scale_x_continuous(breaks = c(yearHigh:yearLow), limits = c(yearLow+1,yearHigh))+
         scale_y_continuous(limits=c(90,130))+
         labs(x="Year", y="Defensive Rating"))
      }
    })
    
    p1 <- playerBaseTable %>% 
      filter(namePlayer == input$player)
    specPlayer <- playerTable %>% 
      filter(namePlayer == input$player)
    src <- specPlayer$urlPlayerThumbnail[1]
    
    box=capture.output(cat("PPG:", round(p1$ppg, 1),
                            "| RPG:", round(p1$rpg, 1),
                            "| APG:", round(p1$apg, 1),
                            "| FG%", round(p1$fg*100,1),
                            "| 3FG%", round(p1$fg3*100,1),
                            "| FT%", round(p1$ft*100,1)))
    
    
    output$boxScore <- renderText(box)
    output$picture<-renderText({c('<img src="',src[1],'">')})
  })
  
  
  
  observeEvent(input$enter2, {
    output$chart2 <- renderPlotly({
      
      specPlayer2 <- playerTable %>% 
        filter(namePlayer == input$player2)
      specYear2 <- playerTable %>% 
        group_by(yearSeason) %>% 
        summarize(avgortg2 = mean(ortg), avgdrtg2 = mean(drtg), avgnet2 = mean(netrtg))
      total2 = full_join(specPlayer2, specYear2)
      
      yearHigh2 <- specPlayer2$yearSeasonLast[1] + 1
      yearLow2 <- specPlayer2$yearSeasonFirst[1]
      src2 <- specPlayer2$urlPlayerThumbnail[1]
      
      if (input$stat == 1){
        ggplotly(ggplot(total2) +
          geom_line(aes(x=yearSeason,y=ortg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgortg2), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_x_continuous(breaks = c(yearHigh2:yearLow2), limits = c(yearLow2+1,yearHigh2))+
          scale_y_continuous(limits=c(90,130))+
          labs(x="Year", y="Offensive Rating"))
      }
      else if (input$stat == 3){
        ggplotly(ggplot(total2) +
          geom_line(aes(x=yearSeason,y=netrtg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgnet2), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_x_continuous(breaks = c(yearHigh2:yearLow2), limits = c(yearLow2+1,yearHigh2))+
          scale_y_continuous(limits=c(-20,20))+
          labs(x="Year", y="Net Rating"))
      }
      else{
        ggplotly(ggplot(total2) +
          geom_line(aes(x=yearSeason,y=drtg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgdrtg2), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_x_continuous(breaks = c(yearHigh2:yearLow2), limits = c(yearLow2+1,yearHigh2))+
          scale_y_continuous(limits=c(90,130))+
          labs(x="Year", y="Defensive Rating"))
      }
    })
    
    p2 <- playerBaseTable %>% 
      filter(namePlayer == input$player2)
    specPlayer2 <- playerTable %>% 
      filter(namePlayer == input$player2)
    src2 <- specPlayer2$urlPlayerThumbnail[1]
    
    
    box2=capture.output(cat("PPG:", round(p2$ppg, 1),
                            "| RPG:", round(p2$rpg, 1),
                            "| APG:", round(p2$apg, 1),
                            "| FG%", round(p2$fg*100,1),
                            "| 3FG%", round(p2$fg3*100,1),
                            "| FT%", round(p2$ft*100,1)))
    
   
    output$boxScore2 <- renderText(box2)
    output$picture2<-renderText({c('<img src="',src2[1],'">')})
  })

  observeEvent(input$enterTeam, {
    output$chart <- renderPlotly({
      
      team1 <- teamTable %>% 
        filter(nameTeam == input$team)
      leagueavg <- teamTable %>% 
        group_by(yearSeason) %>% 
        summarize(avgortg = mean(ortg), avgdrtg = mean(drtg), avgnet = mean(netrtg))
      full <- full_join(team1, leagueavg, by = "yearSeason")
      
      if (input$stat == 1){
        ggplotly(ggplot(full) +
          geom_line(aes(x=yearSeason,y=ortg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgortg), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_y_continuous(limits=c(90,130))+
          labs(x="Year", y="Offensive Rating"))
      }
      else if (input$stat == 3){
        ggplotly(ggplot(full) +
          geom_line(aes(x=yearSeason,y=netrtg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgnet), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_y_continuous(limits=c(-20,20))+
          labs(x="Year", y="Net Rating"))
      }
      else{
        ggplotly(ggplot(full) +
          geom_line(aes(x=yearSeason,y=drtg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgdrtg), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_y_continuous(limits=c(90,130))+
          labs(x="Year", y="Defensive Rating"))
      }
    })
    
    team1 <- df_curr %>% 
      filter(nameTeam == input$team)
    team1src <- team1$urlThumbnailTeam[1]
    
    output$picteam1<-renderText({c('<img src="',team1src[1],'">')})
    
  })
  
  observeEvent(input$enterTeam2, {
    output$chart2 <- renderPlotly({
      
      team2 <- teamTable %>% 
        filter(nameTeam == input$team2)
      leagueavg <- teamTable %>% 
        group_by(yearSeason) %>% 
        summarize(avgortg = mean(ortg), avgdrtg = mean(drtg), avgnet = mean(netrtg))
      full2 <- full_join(team2, leagueavg, by = "yearSeason")
      
      if (input$stat == 1){
        ggplotly(ggplot(full2) +
          geom_line(aes(x=yearSeason,y=ortg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgortg), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_y_continuous(limits=c(90,130))+
          labs(x="Year", y="Offensive Rating"))
      }
      else if (input$stat == 3){
        ggplotly(ggplot(full2) +
          geom_line(aes(x=yearSeason,y=netrtg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgnet), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_y_continuous(limits=c(-20,20))+
          labs(x="Year", y="Net Rating"))
      }
      else{
        ggplotly(ggplot(full2) +
          geom_line(aes(x=yearSeason,y=drtg), color = "blue") +
          geom_line(aes(x=yearSeason,y=avgdrtg), color = "red") +
          ggtitle("League Average-Red Line")+
          scale_y_continuous(limits=c(90,130))+
          labs(x="Year", y="Defensive Rating"))
      }
    })
    
    team2 <- df_curr %>% 
      filter(nameTeam == input$team2)
    team2src <- team2$urlThumbnailTeam[1]
    
    output$picteam2<-renderText({c('<img src="',team2src[1],'">')})
    
  })
  

}

shinyApp(ui = ui, server = server)


