#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("dataset", "Dataset", names(mtcars), selected = names(mtcars)[1]),
    uiOutput("dataset2"),

    conditionalPanel( condition = "output.nrows",
                      selectInput("dataset2", "Dataset2", "dataset2")),
    
 dataTableOutput("dynamic")
)

server <- function(input, output, session) {
    # datasetInput <- reactive({
    #     mtcars %>%
    #         select(input$dataset)
    #     
    # })
    
    row_names <- reactive({ mtcars %>%
                    select(input$dataset) %>% 
                    unique() %>% 
                    as_vector()})
    
    output$dynamic <- renderDataTable(mtcars %>% select(input$dataset), options = list(pageLength = 5))
    
    output$dataset2 <- renderUI(
       # value_choices <- reactive({mtcars %>% select(input$dataset) %>% as_vector() %>% unique() })
        #selectInput("dataset", "Dataset",  mtcars %>% select(input$dataset) %>% unique() %>% as.vector(), selected = mtcars %>% select(input$dataset) %>% .[1])
        checkboxGroupInput("dataset", "Dataset", choices = mtcars %>% select(input$dataset) %>% as_vector() %>% unique(),
                           mtcars %>% select(input$dataset) %>% as_vector() %>% unique()) # mtcars %>% select(input$dataset) %>% unique() %>% as.vector()
       
        
        )

    
   outputOptions(output, "dataset2", suspendWhenHidden = FALSE)  
}

# Run the application 
shinyApp(ui = ui, server = server)
