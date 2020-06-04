
require(tidyverse)
library(readxl)
library(csv)
library(tidyr)
library(modelr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(ggplot2)

library(shinydashboard)
library(shiny)

options(na.action = na.warn)

setwd("c:/Users/chris/Documents/R_Studio/")

dat <-read_excel('data_stryk_test_loop.xlsx')


### EXTRA CRITERIAS ###

min_value = 7 #Min med positivt spelvÃ¤rde
#Minimun totalt spelvarde 
min_total_value = 15 #Min radens totala spelvarde

##Which row number to select
select_row =1


#Odds inom buckets
#bucket_1_min = 15
#bucket_1_max = 21
#bucket_2_max = 35
#bucket_3_max = 50
#bucket_4_max = 65

#Min antal inom bucket
#crit_min_buck_1 = 1
#crit_min_buck_2 = 8
#crit_min_buck_3 = 10
#crit_min_buck_4 = 11




#
#Criterias Garderingar
#
#Best?m antal 1
number_1 = 8

#max antal
number_0 = 5
number_2 = 8

number_gardering = 5
#
min_pos_gard_value = 1



#Calculate value
dat <- dat %>%
  mutate(value_1 = odds_1 - svfo_1)%>%
  mutate(value_0 = odds_0 - svfo_0)%>%
  mutate(value_2 = odds_2 - svfo_2)

##Combinations odds
data_odds <- dat%>%
  select(odds_1, odds_0, odds_2)

data_odds <- as.data.frame(t(data_odds))

#all_combinations_odds <- expand(data_odds, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13)
all_combinations_odds <- expand.grid(data_odds)

all_combinations_odds <- tibble::rownames_to_column(all_combinations_odds, "Row")


##Combinations value
data_value <- dat%>%
  select(value_1, value_0, value_2)

data_value <- as.data.frame(t(data_value))

#all_combinations_value <- expand(data_value, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13)
all_combinations_value <- expand.grid(data_value)
all_combinations_value <- tibble::rownames_to_column(all_combinations_value, "Row")

all_combinations_value$obs_value <- rowSums(all_combinations_value[,2:14] > 0)
all_combinations_value <- all_combinations_value%>%
  mutate(total_value = V1 + V2 + V3 +V4 +V5+V6+V7+V8+V9+V10+V11+V12+V13)




#all combinations 1x2
data_1x2 <- dat%>%
  select(res_1, res_0, res_2)

data_1x2 <- as.data.frame(t(data_1x2))
all_combinations_1x2 <- expand.grid(data_1x2)
all_combinations_1x2 <- tibble::rownames_to_column(all_combinations_1x2, "Row")
all_combinations_1x2$obs_1 <- rowSums(all_combinations_1x2[,2:14] == 1)
all_combinations_1x2$obs_0 <- rowSums(all_combinations_1x2[,2:14] == 0)
all_combinations_1x2$obs_2 <- rowSums(all_combinations_1x2[,2:14] == 2)



###Att ha kriteriet hÃ¤r gÃ¶r att man slipper ha det i funktionen. Borde gÃ¥ snabbare.
### Criteria: Calculate value  / spelvarde. Kommer alltid vilja ha en grundrad med positivt spelvÃ¤rde. 
#regel summa spelv?rde som minst -25
all_combinations_value <- all_combinations_value%>%
  filter(obs_value >= min_value)%>%
  filter(total_value > min_total_value)

#Decrease number of rows from new criteria
temp <- all_combinations_value %>%
  select(Row)

all_combinations_odds <- left_join(temp, all_combinations_odds, by = "Row")%>%
  mutate(total_odds = V1 + V2 + V3 +V4 +V5+V6+V7+V8+V9+V10+V11+V12+V13)

all_combinations_1x2 <- left_join(temp, all_combinations_1x2, by = "Row")




get_row <- function(min_1, max_1, min_x, max_x, min_2, max_2, level){
  
  ### 
  ##############Flytta upp denna
  ### 
  ### Criteriacount number of 1,x,2 per row
  
  all_combinations_1x2 <- all_combinations_1x2 %>%
    filter(obs_1 <= max_1 & obs_0 <= max_x & obs_2 <= max_2)%>%
    filter(obs_1 >= min_1 & obs_0 >= min_x & obs_2 >= min_2)
  
  #Decrease number of rows in all_combinations_odds
  temp <- all_combinations_1x2 %>%
    select(Row)
  
  all_combinations_odds <- left_join(temp, all_combinations_odds, by = "Row")
  all_combinations_value <- left_join(temp, all_combinations_value, by = "Row")
  
  ### 
  
  ### 
  
  ### Criteria: Calculate value  / spelvarde. Regel: minst antal med positivt spelvarde
  #regel summa spelv?rde som minst -25
  #all_combinations_value <- all_combinations_value%>%
  #filter(obs_value >= min_value)
  
  #Decrease number of rows from new criteria
  #temp <- all_combinations_value %>%
  #  select(Row)
  
  # all_combinations_odds <- left_join(temp, all_combinations_odds, by = "Row")%>%
  #   mutate(total_odds = V1 + V2 + V3 +V4 +V5+V6+V7+V8+V9+V10+V11+V12+V13)
  
  #all_combinations_1x2 <- left_join(temp, all_combinations_1x2, by = "Row")
  
  
  
  
  
  ###FrÃ¥ga: lÃ¤gg in total value fÃ¶r "all_combinations_odds"
  
  ##### Run least square method. Least deviation from model row.
  
  ####
  ###
  
  #get row to aim for
  data_model_row <- dat %>%
    select(level)
  
  data_model_row <- as.data.frame(t(data_model_row))
  
  
  # ta ut relevanta rader, sortera varje rad med apply
  all_rows_to_be_predicted <- all_combinations_odds %>%
    select(V1:V13)
  all_rows_to_be_predicted <- t(apply(all_rows_to_be_predicted, 1, sort))
  
  
  #ta ut rad from combinations, sen binda med de sorterade matchoddsen. Detta steg ar da det ej gick att kora mutate pa tabellen dar apply anvants
  
  help_table <- all_combinations_odds %>%
    select(Row)
  all_rows_to_be_predicted <- cbind(help_table, all_rows_to_be_predicted)
  
  colnames(all_rows_to_be_predicted) <- c("Row", "V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11", "V12","V13")
  
  #berakna diff mot indata (gor detta sen dynamisk sa man kan valja olika optimala rader)
  all_rows_to_be_predicted <- all_rows_to_be_predicted %>%
    mutate(
      diff_1 = (all_rows_to_be_predicted$V1 - data_model_row$V1)^2,
      diff_2 = (all_rows_to_be_predicted$V2 - data_model_row$V2)^2,
      diff_3 = (all_rows_to_be_predicted$V3 - data_model_row$V3)^2,
      diff_4 = (all_rows_to_be_predicted$V4 - data_model_row$V4)^2,
      diff_5 = (all_rows_to_be_predicted$V5 - data_model_row$V5)^2,
      diff_6 = (all_rows_to_be_predicted$V6 - data_model_row$V6)^2,
      diff_7 = (all_rows_to_be_predicted$V7 - data_model_row$V7)^2,
      diff_8 = (all_rows_to_be_predicted$V8 - data_model_row$V8)^2,
      diff_9 = (all_rows_to_be_predicted$V9 - data_model_row$V9)^2,
      diff_10 = (all_rows_to_be_predicted$V10 - data_model_row$V10)^2,
      diff_11 = (all_rows_to_be_predicted$V11 - data_model_row$V11)^2,
      diff_12 = (all_rows_to_be_predicted$V12 - data_model_row$V12)^2,
      diff_13 = (all_rows_to_be_predicted$V13 - data_model_row$V13)^2,
    )%>%
    select(Row, diff_1:diff_13)
  
  all_rows_to_be_predicted <- all_rows_to_be_predicted %>%
    mutate(square_mean = sqrt(rowMeans(all_rows_to_be_predicted[2:14])))
  
  #binda ihop med ursprungligt dataset
  
  all_rows_to_be_predicted <- all_rows_to_be_predicted%>%
    select(Row, square_mean)
  
  all_combinations_odds <- left_join(all_combinations_odds, all_rows_to_be_predicted, by = "Row", all.x = TRUE)
  all_combinations_1x2 <- left_join(all_combinations_1x2, all_rows_to_be_predicted, by = "Row", all.x = TRUE)
  
  all_combinations_odds <- arrange(all_combinations_odds, (square_mean))
  all_combinations_1x2 <- arrange(all_combinations_1x2, (square_mean))
  
  
  
  #vald rad  
  best_row_1x2 <- all_combinations_1x2 %>%
    slice(which(row_number() == select_row))
  #filter(total_odds == max(total_odds))
  #filter(Row == 123455)
  
  #ta ut number of 1,x,2
  n_1_in_row = best_row_1x2$obs_1
  n_x_in_row = best_row_1x2$obs_0
  n_2_in_row = best_row_1x2$obs_2
  
  best_row_1x2 <- best_row_1x2 %>%
    select(V1:V13)
  best_row_1x2 <- as.data.frame(t(best_row_1x2))%>%
    rename(tecken = V1)
  best_row_1x2 <- tibble::rownames_to_column(best_row_1x2, "Row")
  
  
  best_row_odds <- all_combinations_odds %>%
    slice(which(row_number() == select_row))%>%
    #filter(total_odds == max(total_odds))%>%
    select(V1:V13)
  
  best_row_odds <- as.data.frame(t(best_row_odds))%>%
    rename(Odds = V1)
  best_row_odds <- tibble::rownames_to_column(best_row_odds, "Row")
  
  
  #test
  #test <- all_combinations_1x2%>%
  
  
  
  
  best_row_1x2 <- full_join(best_row_1x2, best_row_odds, by = "Row", all = TRUE)
  
  best_row_1x2 <- best_row_1x2 %>%
    mutate(tecken = as.character(tecken))
  
  return(best_row_1x2)
  
}




#https://shiny.rstudio.com/gallery/widgets.html
#https://www.listendata.com/2018/02/shiny-tutorial-r.html

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Stryktipset",tags$li(class = "dropdown", actionButton(inputId='show', label="Learn More", icon = icon("th")))),
                    
                    #dashboardHeader(title = "Stryktipset"),
                    
                    dashboardSidebar(
                      
                      sidebarMenu(id = "sidebarmenu",
                                  menuItem("Dashboard", tabName = "dashboard",  icon = icon("group", lib="font-awesome")),
                                  conditionalPanel("input.sidebarmenu === 'dashboard'"),
                                  
                                  menuItem("Mean-Square Model", tabName = "mean_square_model", icon = icon("check-circle", lib = "font-awesome")),
                                  conditionalPanel("input.sidebarmenu === 'mean_square_model'",
                                                   
                                                   
                                                   #   radioButtons("model", "Choose a model",
                                                   #               c("Mean-Square Model" = "m_mean_square",
                                                   #                 "Segment Model" = "m_segment")),
                                                   
                                                   
                                                   helpText("Choose criterias for optimal Row"),
                                                   
                                                   
                                                   radioButtons("level", "Choose a level",
                                                                c("Level 1" = "level_1",
                                                                  "Level 2" = "level_2",
                                                                  "Level 3" = "level_3",
                                                                  "Level 4" = "level_4",
                                                                  "Level 5" = "level_5")),
                                                   
                                                   
                                                   
                                                   sliderInput("number_1", "Select range for 1",
                                                               min = 1, max = 13,
                                                               value = c(2,8)),
                                                   
                                                   sliderInput("number_x", "Select range for x",
                                                               min = 1, max = 13,
                                                               value = c(2,8)),
                                                   
                                                   sliderInput("number_2", "Select range for 2",
                                                               min = 1, max = 13,
                                                               value = c(2,8)),
                                                   
                                                   
                                                   actionButton("update", "Get Optimal Row"),
                                                   actionButton("reset", "Reset"),
                                                   
                                                   helpText("Choose number of half-hedges"),
                                                   
                                                   
                                                   numericInput("halvgarderingar", "Number of half-hedges",
                                                                5, min = 0, max = 13),
                                                   
                                                   actionButton("update_gardering", "Get half-hedgess") 
                                                   
                                  )),
                      
                      menuItem("Segment Model", tabName = "segment_model", icon = icon("check-circle", lib = "font-awesome")),
                      conditionalPanel("input.sidebarmenu === 'segment_model'",
                                       
                                       
                                       radioButtons("test", "Choose a model",
                                                    c("test" = "m_mean_square",
                                                      "test" = "m_segment")))),
                    
                    
                    
                    
                    dashboardBody(
                      tabItems(
                        
                        tabItem(
                          tabName = "dashboard", class='active',
                          fluidRow(
                            
                            box(title="Summary of ingoing data",
                                
                                status="primary", solidHeader = TRUE,width=12)
                            
                            
                          #  box(title="Regional Dashboard - Plot latest available regional data",
                              #  status="primary", solidHeader = TRUE,width=12,
                                
                               # box(
                                #  plotlyOutput(outputId = "scatter_all"), height=500, width=12))
                            
                            
                          )),
                        
                        
                        
                        tabItem(
                          tabName = "mean_square_model",
                          fluidRow(
                            
                            box(title="Optimal Row",
                                status="primary", solidHeader = TRUE,width=12,
                                
                                box(#collapsible = TRUE,
                                  tableOutput("summary"), width=6))))
                        
                        
                        
                      )
                      
                    ))



server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  
  
  mylist <- reactiveValues() # we will store the inputs in a reactive list
  
 # eventReactive(input$update,{
  observeEvent(input$update,{
#    input$update
    
    
    min_1 = input$number_1[1]
    max_1 = input$number_1[2]
    min_x = input$number_x[1]
    max_x = input$number_x[2]
    min_2 = input$number_2[1]
    max_2 = input$number_2[2]
    level = input$level
    #      medium = input$Medium,
    #     campaign = input$Camp))
    
    
    mylist$hej <- isolate({
      
      get_row(
        min_1,
        max_1,
        min_x,
        max_x,
        min_2,
        max_2,
        level
      )
      
    })
    
    
  })     
  
  
  #  my_table <- eventReactive(input$update, { # "runScript" is an action button
  #      source("run_meansquare.R", local = list2env(mylist()))
  #   })
  
  #   df <- eventReactive(input$update, function() {
  #      best_row_1x2
  #   })
  
  
  observeEvent(input$reset, {
    mylist$hej <- NULL
    input$update == 0
  })  
    

  
  
  output$summary <- renderTable({
    if(input$update == 0) ""
    else
      mylist$hej
  })
  
  #  output$mtcars <- renderTable(head(mtcars))
  
  
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "More information",
      
      HTML("Data based on release date of regional data. There may be a later print on aggregate level. <br><br>
        <b>Initial Jobless Claims:</b> People filing to receive unenpmoyment insurance benefits for the first time. Reliable number. Moves close to general economy, good indicator. <br><br>
        <b>Continuing Jobless Claims:</b> People who are continuing filing to receive unenpmoyment insurance. <br><br>
        <b>Unemployment Rate:</b> The unemployment rate measured as the number of persons unemployed divided by the civilian labor force.<br><br>
        <b>Participation Rate:</b> The participation rate refers to the total number of people or individuals who are currently employed or in search of a job.<br><br>
        <b>Employment-to-population rate:</b> The employment-to-population ratio is equal to the number of persons employed divided by the working-age population.<br><br>
        <b>Civilian Population:</b> In the United States, the civilian noninstitutional population refers to people 16 years of age and older residing in the 50 States and the District of Columbia who are not inmates of institutions (penal, mental facilities, homes for the aged), and who are not on active duty in the Armed Forces.<br><br>
             
         "
           
      ),
      
      easyClose = TRUE
    ))
  })
  
}
shinyApp(ui, server)
