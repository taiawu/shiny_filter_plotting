library(DT)
library(shiny)

# new daughter layout function
df_to_layout <- function(df, layout_type) {
    df_m <-   set_names( df ,  c("type","row",as.numeric( df [1,-c(1,2)]))) %>%
        . [ -1 , -1] %>%
        reshape2::melt( . ,id.vars = "row") %>%
        mutate( . , well = as_vector(map2( . $row,  . $variable, paste0)) ) %>%
        set_names( . , c("row", "column", layout_type, "well"))
    df_m
}

make_layout <- function( filename ) { # from path to raw layout to a final fomatted layout file
    # read the layout file, and split each layout into an individual
    layout_list <- data.table::fread( filename, header = TRUE) %>%
        as_tibble() %>%
        split( . ,  . $Type)
    
    # put into a merge-able form
    layout <- df_to_layout(layout_list[[1]], names(layout_list)[[1]])[c(1,2,4)] # initialize the list
    for (i in c(1:length(layout_list))) {
        layout <- layout %>%
            mutate("var" =  as_vector(df_to_layout(layout_list[[i]], layout_type = names(layout_list)[[i]])[3] )) %>% # append the column of interest
            set_names(c(names(layout), names(layout_list)[[i]])) # rename based on the column of interest
    }
    layout <- layout %>%
        unite("condition", c(4:ncol(.)), remove = FALSE) %>% # create a unique column, used to define groups after averaging
        mutate_if(is.factor, as.character)
    
    layout
}

convert_numerics <- function( vec ) {
    
    if(all(varhandle::check.numeric(vec))){
        # convert the vector to numeric
        vec <- as.numeric(vec)
    }
    vec
} 

make_layout_table <- function( layout_df ) {
    
    col_names <- names(layout_df)
    
    all_vars <- lapply(col_names, pull, .data = layout_df) %>% 
        lapply(unique) %>%
        lapply(sort) %>%
        lapply(convert_numerics)
    
    
    max_length <- sapply( all_vars, length, simplify = "vector" ) %>% max()
    
    #var_list_len <- lapply( all_vars, set_length, len = max_length)
    var_list_len <- lapply( all_vars,  function( x ) { length(x) <- max_length 
    x })
    
    names(var_list_len) <- col_names
    
    var_list_len %>% as_tibble()
    
}

ui <- shinyUI(

    fluidRow(    
        tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
        DT::dataTableOutput("myDatatable"),
        DT::dataTableOutput("selectedCells"),
        verbatimTextOutput("selectedCells_print"),
        plotOutput("plot")
    )
)

server <- shinyServer(function(input, output, session) {
    layout_in <- make_layout("sample_daughter_layout.csv") %>%
        select(-c(row, column, well, condition)) %>%
        make_layout_table()
            

    output$myDatatable <- DT::renderDataTable( layout_in,
                                              selection= list(mode="multiple", target="cell"),
                                              server = FALSE,
                                              rownames=FALSE)

    output$selectedCells <- DT::renderDataTable({
        req(input$myDatatable_cells_selected)

        #input$myDatatable_cells_selected
        mt_edit <-  layout_in 
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

