library(shinydashboard)
library(shiny)

#https://shiny.rstudio.com/gallery/widgets.html
#https://www.listendata.com/2018/02/shiny-tutorial-r.html

ui <- dashboardPage(
  dashboardHeader(title = "Quick Example"),
  
  dashboardSidebar(
    
                  radioButtons("model", "Choose a model",
                             c("Mean-Square Model" = "m_mean_square",
                               "Segment Model" = "m_segment")),
        
                  radioButtons("level", "Choose a level",
                              c("Level 1" = "level1",
                                "Level 2" = "level2",
                                "Level 3" = "level3",
                                "Level 4" = "level4",
                                "Level 5" = "level5")),
                                
                  

                  sliderInput("number_1", "Select range for 1",
                              min = 1, max = 13,
                              value = c(5,8)),
                  
                  sliderInput("number_x", "Select range for x",
                              min = 1, max = 13,
                              value = c(5,8)),
                  
                  sliderInput("number_2", "Select range for 2",
                              min = 1, max = 13,
                              value = c(5,8)),
                  
                  helpText("Choose number of garderingar."),
                  
                  
                  numericInput("halvgarderingar", "Number of half-hedges",
                               5, min = 0, max = 13),
                  
                  actionButton("update", "Get Optimal Row") 
                  
                   ),
 


  
   dashboardBody(
    valueBox(100, "Basic example"),
    tableOutput("mtcars")
  )
)



server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
#  output$mtcars <- renderTable(head(mtcars))
}
shinyApp(ui, server)
