library(DT)
library(shiny)


ui <- shinyUI(
    fluidRow(
        DT::dataTableOutput("myDatatable"),
        DT::dataTableOutput("selectedCells"),
        verbatimTextOutput("selectedCells_print"),
        plotOutput("plot")
    )
)

server <- shinyServer(function(input, output, session) {

    output$myDatatable <- DT::renderDataTable(mtcars[c(1:3), c(1:3)],
                                              selection= list(mode="multiple", target="cell"),
                                              server = FALSE,
                                              rownames=FALSE)

    output$selectedCells <- DT::renderDataTable({
        req(input$myDatatable_cells_selected)

        #input$myDatatable_cells_selected
        mt_edit <-  mtcars[c(1:3), c(1:3)] 
        for (row in c(1:nrow(input$myDatatable_cells_selected))) {
            print(row)
            print(input$myDatatable_cells_selected[row,])
            mt_edit[input$myDatatable_cells_selected[row,1],  unique(input$myDatatable_cells_selected[row,2]+1 )] <- NA
            
        }
        mt_edit
       })

    output$selectedCells_print <- renderPrint(
        input$myDatatable_cells_selected
     
    )
})

shinyApp(ui, server)

# some other relevant vignettes which were looked at along the way
#  https://yihui.shinyapps.io/DT-rows/ # just uses rows
# https://community.rstudio.com/t/infinite-loop-when-updating-value-on-select-cell-in-shiny-dt-datatable/24007 # theres a lot going on here

