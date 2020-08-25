library(varhandle)
library(tidyverse)

## generic functions:

# make_layout # ultimately won't be needed, as a layout will be passed to this module
# df_to_layout # helper for make_layout
# convert_numerics # part of the make_layout suite, which is applied again once filtering is complete. Will remain a part of this module always, in case all non-numeric variables are removed. 

## specific functions:
# make_layout_table: converts a layout into a non-redundant variable table to display for user interaction
# filter mask: filters a layout-inlusive data frame based on selections from a data table


filter_mask <- function( layout_in, original_mask, mask_table, keep_or_drop = "drop") {
  ### function to make a unique layout table, as created by make_layout_table, and modified by use selection, to filter the original long-form data
  
  if (keep_or_drop == "drop") { # to drop the user-selected values, which have been overwritten with NA
    vars <- names(layout_in)[names(layout_in) %in% names(mask_table)]  # look only in the variables present in both user-modified and original data
   
     filt_layout <- layout_in # initiate the fitered dataframe 
    
    for (x in c(1:length(vars)) ) { # for each variable to filer on
      filt_layout <-  filt_layout %>% 
                      filter(.data[[vars[[x]]]] %in% mask_table[[vars[[x]]]]) } # filter out the user-selected values
    
  } else if (keep_or_drop == "keep") { # to keep only the selected values
    
    original_mask[!is.na(mask_table)] <- NA # invert the selection table
    keep_mask <- original_mask %>%  discard(~all(is.na(.))) # drop the variables unmodified by the user, which presumably, they don't want to filter on 
    
    vars <- names(layout_in)[names(layout_in) %in% names(keep_mask)] 
    filt_layout <- layout_in 
    
    for (x in c(1:length(vars)) ) {
      filt_layout <-  filt_layout %>% 
        filter(.data[[vars[[x]]]] %in% keep_mask[[vars[[x]]]]) 
    }
  }

  filt_layout # return the filtered layout
}


make_layout_table <- function( layout_df ) { 
  ### function to convert a long-form data to a table which lists all of its unique components
  
  col_names <- names(layout_df)
  
  all_vars <- lapply(col_names, pull, .data = layout_df) %>% # pull each of the named columns out of the tibble as individual vectors
                lapply(unique) %>% # remove duplicate entries in these vectors 
                lapply(sort) # sort them so these entries appear in an intuitive order
  # %>%   lapply(convert_numerics) # this could cause issues in re-joining with the final data, so this should be done after re-joining
  
  
  max_length <- sapply( all_vars, length, simplify = "vector" ) %>% max() # determine length of the longest vector
  
  var_list_len <- lapply( all_vars,  function( x ) { length(x) <- max_length 
  x }) # set the lengths of all vectors to be equal to the longest length
  
  names(var_list_len) <- col_names # name each vector
  
  var_list_len %>% as_tibble() # bind back into the final, all-unique entries tibble
}


###### shiny module elements #######
# this module is designed to take reactive layouts as an input, and return a filtered reactive layout as an output
# whie it technically could handle layotus merged to data, returning just the layout, and joining it with the appropriate data is likely the best practice, to reduce duplicate calls to the server function if the same filtering should be applied to multiple data forms 

selectVariablesTableUI <- function(id, # a shiny module UI, which returns action buttons and a table for a user to select on
                                   drop_button_name = "Remove selected conditions", 
                                   keep_button_name = "Keep only selected conditions",
                                   table_HTML_style = 'table.dataTable tr.selected td, table.dataTable td.selected {background-color: #addd8e !important;}'
) {
  
  tagList( # return all of the elements in a tagLigs
    tags$style(HTML(table_HTML_style)), # the styling of the table
    
    actionButton( NS(id, "drop_vars"), drop_button_name), # clicking this passes "drop" to keep_or_drop argument
    actionButton( NS(id, "keep_these_vars"), keep_button_name), # clicking this passes "keep" to keep_or_drop argument
    
    DT::dataTableOutput( NS(id, "myDatatable") ) # the unique variable table which the user can select with
  )
  
}



selectVariablesTableServer <- function(id, # namespace to connect ui and server modules
                                       layout_raw, # this is reactive, and supplied by the user, unresolved (e.g. layout_raw, not layout_raw() ). 
                                       drop_layout_cols = c("row", "column", "well", "condition"), # mask out columns which users probably don't want to select on
                                       table_display_options = list(scrollX = TRUE, scrollY = 200, scrollCollapse = TRUE, paging = FALSE, dom = 'tr'), 
                                       empty_message_main = "These selection criteria will remove all data!",
                                       empty_message_sub = "Please update your selections and try again. Meanwhile, these criteria have not been applied to the data.") {
  
  print("is layout raw reactive?")
  print(is.reactive(layout_raw))
  stopifnot(is.reactive(layout_raw)) # layout_raw should be reactive
  print("entering server")
  
  moduleServer(id, function(input, output,session) {
    
    layout_in <-reactive( layout_raw() %>% # reactive object input to server
                            select(- all_of(drop_layout_cols)) %>%
                            make_layout_table( . ) )
    
    output$myDatatable <- DT::renderDataTable( layout_in(),
                                               selection= list(mode="multiple", target="cell"),
                                               server = FALSE,
                                               rownames = FALSE,
                                               options = table_display_options)
    
    
    drop_var <- 0
    keep_var <- 0
    
    # eventReactive which updates if either UI button is clicked. It uses counters to determine which button was clicked to trigger the event.
    chosen_vars <- eventReactive( {input$keep_these_vars |  input$drop_vars}, {
      if(is.null(input$myDatatable_cells_selected)) {return(layout_raw())} # if the buttons get clicked with no selections
      
      tryCatch({ # this tryCatch is here to handle cases when uses click the buttons without supplying any selections
        
        # try assuming user has made selections
        mt_edit <-  layout_in() # why isn't this reactive?
        for (row in c(1:nrow(input$myDatatable_cells_selected))) {
          mt_edit[input$myDatatable_cells_selected[row,1],  unique(input$myDatatable_cells_selected[row,2]+1 )] <- NA }
        
        if (drop_var != input$drop_vars %>% as.numeric()) {
          out <- filter_mask( layout_raw(), original_mask = layout_in(),  mt_edit, keep_or_drop = "drop") 
          
        } else if (keep_var != input$keep_these_vars %>% as.numeric()) {
          out <- filter_mask( layout_raw(), original_mask = layout_in(),  mt_edit, keep_or_drop = "keep") 
        }
        
        # update the button counters
        drop_var <<- input$drop_vars %>% as.numeric()
        keep_var <<- input$keep_these_vars %>% as.numeric()
        
        if ( nrow(out) < 1 ) { # if the user has selected conditions with mask out all of their data
          shinyalert(empty_message_main, empty_message_sub)
          out <- layout_raw()
        }
        
        out
        
      }, error = function(e) {
        shinyalert("No cells have been selected!", "Nothing to isolate or remove. Data is unchanged.")
        # if user hasn't made any selections, return the original, unfiltered layout
        layout_raw()
      })
      
    }) # end eventReactive
    
    reactive( chosen_vars() ) # this reactive value gets returned
    
  }) # end moduleServer
  
} # end server module function




# # new daughter layout function
# df_to_layout <- function(df, layout_type) {
#   df_m <-   set_names( df ,  c("type","row",as.numeric( df [1,-c(1,2)]))) %>%
#     . [ -1 , -1] %>%
#     reshape2::melt( . ,id.vars = "row") %>%
#     mutate( . , well = as_vector(map2( . $row,  . $variable, paste0)) ) %>%
#     set_names( . , c("row", "column", layout_type, "well"))
#   df_m
# }
# 
# make_layout <- function( filename ) { # from path to raw layout to a final fomatted layout file
#   # read the layout file, and split each layout into an individual
#   layout_list <- data.table::fread( filename, header = TRUE) %>%
#     as_tibble() %>%
#     split( . ,  . $Type)
#   
#   # put into a merge-able form
#   layout <- df_to_layout(layout_list[[1]], names(layout_list)[[1]])[c(1,2,4)] # initialize the list
#   for (i in c(1:length(layout_list))) {
#     layout <- layout %>%
#       mutate("var" =  as_vector(df_to_layout(layout_list[[i]], layout_type = names(layout_list)[[i]])[3] )) %>% # append the column of interest
#       set_names(c(names(layout), names(layout_list)[[i]])) # rename based on the column of interest
#   }
#   layout <- layout %>%
#     unite("condition", c(4:ncol(.)), remove = FALSE) %>% # create a unique column, used to define groups after averaging
#     mutate_if(is.factor, as.character)
#   
#   layout
# }
# 
# convert_numerics <- function( vec ) {
#   
#   if(all(varhandle::check.numeric(vec))){
#     # convert the vector to numeric
#     vec <- as.numeric(vec)
#   }
#   vec
# } 




# https://itsalocke.com/blog/shiny-module-design-patterns-pass-module-inputs-to-other-modules/

### 
