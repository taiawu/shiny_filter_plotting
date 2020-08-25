source("filter_on_select_utils.R")
library(shinyBS)
library(DT)
library(shinyalert)
library(shiny)

## https://mastering-shiny.org/scaling-modules.html#case-studies

ui <- fluidPage(useShinyalert(), 
                sidebarLayout(
                    sidebarPanel(
                        bsCollapse(id = "file_parse_types", open = "Panel 1",
                                   bsCollapsePanel("Select data to plot",
                                                   selectVariablesTableUI(id = "selection_table1"),
                                                   DT::dataTableOutput("selectedCells") 
                                                   ),
                                   bsCollapsePanel("Select data to fit",
                                                   selectVariablesTableUI(id = "selection_table2",
                                                                          drop_button_name = "Do not fit selected conditions", 
                                                                          keep_button_name = "Fit only selected conditions"),
                                                   DT::dataTableOutput("selectedCells2") 
                                   ))
                        

                    ),
                    mainPanel()
                ))

server <- function(input, output, session) {
    layout_raw <- reactive(make_layout("sample_daughter_layout.csv") )
    
    new_layout <- selectVariablesTableServer(id = "selection_table1", 
                                             layout_raw = layout_raw) # why is unresolved (no () ) reactive required for layout_raw, which is reactive?
    
    new_layout2 <- selectVariablesTableServer(id = "selection_table2", 
                                              layout_raw = layout_raw) # why is unresolved (no () ) reactive required for layout_raw, which is reactive?
    
    output$selectedCells <- DT::renderDataTable(
        new_layout(),
        options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr')
        )
    
    output$selectedCells2 <- DT::renderDataTable(
        new_layout2(),
        options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr')
    )
    
}

shinyApp(ui, server)

# some other relevant vignettes which were looked at along the way
#  https://yihui.shinyapps.io/DT-rows/ # just uses rows
# https://community.rstudio.com/t/infinite-loop-when-updating-value-on-select-cell-in-shiny-dt-datatable/24007 # theres a lot going on here

