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

#### this can be done using heirarchical drill-downs, as shown in Mastering Shiny
# https://mastering-shiny.org/action-dynamic.html
# 10.1.2 Hierarchical select boxes
#### See live at https://hadley.shinyapps.io/ms-update-nested.

# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
    selectInput("customername", "Customer", choices = NULL),
    selectInput("ordernumber", "Order number", choices = NULL),
    tableOutput("data")
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
