#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import DT
#' @import dplyr
#' @import readr
#' @import RPostgres
#' @import DBI
#' @import shinyjs
#' @noRd


temp_dir <- tempdir()# getwd()
# if(!dir.exists(temp_dir)){
#   dir.create(temp_dir)
# }

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    navbarPage('Bohemia entomology data management tool',
               tabPanel('A4',
                        fluidPage(
                          fluidRow(
                            column(6,
                                   h4('Select a specimen from the table below'),
                                   # selectInput('country', 'Country',
                                   #             choices = c('Mozambique', 'Tanzania')),
                                   DT::dataTableOutput('main_table')),
                            column(6,
                                   h1('A4'),
                                   h4('Enter data about the selected specimen'),
                                   textOutput('selected_text'),
                                   selectInput('parity', 
                                               label = 'Enter parity status',
                                               choices = c('P', 'NP')),
                                   sliderInput('wing_length', 
                                               label = 'Enter the wing length (micrometers)',
                                               min = 0, 
                                               max = 4000, 
                                               value = 2000), 
                                   actionButton('submit_entry',
                                                label = 'Submit entry'))
                          ),
                          fluidRow(
                            h2('Already-entered data'),
                            helpText('(read only)'),
                            DT::dataTableOutput('read_table')
                          )
                        )))
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ento'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}



##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
app_server <- function(input, output, session) {

  message('Server code running')
  message('Working directory is: ', getwd())
  
  # Load in the data from s3
  retrieve_ento_data(s3creds_path = 'credentials/bohemiacensuss3credentials.csv',
                     country = 'Mozambique')
  # There is now an object in memory named "entoa3"
  # There is now an object in memory named "entoa4" (if some data has already been saved there)
  if(exists('entoa3')){
    message('---the entoa3 object has ', length(entoa3), ' elements')
  } else {
    message('---there is no object named entoa3!')
  }
  if(exists('entoa4')){
    message('---the entoa4 object has ', nrow(entoa4), ' rows')
  }  else {
    message('---there is no object named entoa4!')
  }
  # Define a function for whittling down the data from entoa3 to only that which matters
  trim_down_a3 <- function(entoa3,
                           entoa4 = NULL,
                           remove_a4s = TRUE){
    # get only the dissected data sets
    ds <- entoa3[grepl('dissected', names(entoa3))]
    rl <- list()
    for(i in 1:length(ds)){
      tmp <- ds[[i]]
      names(tmp)[1:2]<- c('qr', 'manual')
      tmp$species <- unlist(lapply(strsplit(names(tmp)[length(tmp)], '_'), function(x) x[length(x)]))
        
      # get qr codes from qr or manual
      tmp <-tmp %>% dplyr::mutate(qr = ifelse(is.na(qr), manual, qr))%>% select(qr,species, PARENT_KEY)
      # stor in liest
      rl[[i]] <- tmp
    }
    df <- do.call('rbind', rl)
    # df$number <- 1:nrow(df)
    
    # join with data 
    df <- inner_join(df, entoa3$entoa3, by = c('PARENT_KEY' = 'KEY')) %>%
      select(qr, Date = todays_date, `Household ID` = hh_id, Species = species) %>%
      arrange(qr)
    
    # Keep only those rows with a qr which is NOT in entoa4
    if(remove_a4s){
      if(!is.null(entoa4)){
        if(nrow(entoa4) > 0){
          remove_qrs <- entoa4$qr
          message('Going to remove ', length(remove_qrs), ' since they are already in a4')
          df <- df %>%
            filter(!qr %in% remove_qrs)
        }
      }
    }
    return(df)
  }
  
  # function to clean up the table used for selection (reducing number of rows
  # when a new a4 entry is submitted)
  clean_up_left <- function(dl3,
                            dl4){
    dl3 <- dl3 %>% filter(!qr %in% dl4$qr)
    return(dl3)
  }
  
  
  data_list <- reactiveValues(main = trim_down_a3(entoa3), 
                              sub = data.frame(),
                              entoa3 = trim_down_a3(entoa3, remove_a4s = FALSE),
                              entoa4 = data.frame())
  
  if(exists('entoa4')){
    data_list$entoa4 <- entoa4
    new_main <- trim_down_a3(entoa3, entoa4, remove_a4s = TRUE)
    data_list$main <- new_main
  } 

  output$main_table <- DT::renderDataTable({
    pd <- data_list$main
    DT::datatable(pd,
                  selection = list(mode = 'single', selected = c(1)),
                  options = list(
                    paging =TRUE,
                    pageLength =  nrow(pd) 
                  ))
  })  
  
  observeEvent(input$main_table_rows_selected, {
    # capture the selected row
    rs <- input$main_table_rows_selected
    message('---The selected row is: ', rs)
    # Subset the data
    ok <- FALSE
    if(!is.null(rs)){
      if(length(rs) == 1){
        ok <- TRUE
      }
    }
    if(ok){
      pd <- data_list$main
      sub_data <- pd[rs,]
      data_list$sub <- sub_data
    } else {
      data_list$sub <- data.frame()
    }
  })

  output$selected_text <- renderText({
    # Get the selected row
    dls <- data_list$sub
    if(nrow(dls) == 1){
      out <- paste0('You are entering data about QR :', 
                    dls$qr, 
                    ' (species: ', 
                    dls$Species, 
                    ')' )
    } else {
      out <- 'Select an entry by clicking on the table.'
    }
  })
  
  # Observe the submission and save the data
  observeEvent(input$submit_entry, {
    # Get the a3 data
    this_a3_selection <- data_list$sub
    # Get the a4 data
    this_a4_submission <- tibble(
      qr = this_a3_selection$qr,
      parity = input$parity,
      wing_length = input$wing_length)
    # Get the previous WHOLE a4 dataset and add rows to it
    old_a4 <- data_list$entoa4
    
    # save
    save(this_a3_selection,
         this_a4_submission,
         old_a4,
         file = '/tmp/joe.RData')
    
    if(nrow(old_a4) > 0){
      new_a4 <-
        bind_rows(old_a4,
                  this_a4_submission)
    } else {
      new_a4 <- this_a4_submission
    }
    # Overwrite the in-memory a4 object
    data_list$entoa4 <- new_a4
    rm(entoa4)
    entoa4 <- new_a4
    message('entoa4 has ', nrow(entoa4), ' rows')
    # Write to s3 too
    write_ento_data(full_a4 = entoa4,
                    # directory = temp_dir,
                    s3creds_path = 'credentials/bohemiacensuss3credentials.csv',
                    country = 'Mozambique')
    message('Just wrote the following data to s3')
    print(entoa4)
    
    # Clean up the selection table
    new_left <- clean_up_left(data_list$main,
                              entoa4)
    message('updating data_list$main')
    data_list$main <- new_left
  })
  
  # Read only table of previously entered data
  output$read_table <- DT::renderDataTable({
    a4 <- data_list$entoa4
    a3 <- data_list$entoa3
    ok <- FALSE
    if(!is.null(a4)){
      if(!is.null(a3)){
        if(nrow(a4) > 0){
          if(nrow(a3) > 0){
            ok <- TRUE
          }
        }
      }
    }
    if(ok){
      joined <- left_join(a3 %>% mutate(qr = as.character(qr)), 
                          a4 %>% mutate(qr = as.character(qr))) %>%
        filter(!is.na(wing_length))
      DT::datatable(joined,
                    # selection = list(mode = 'single', selected = c(1)),
                    options = list(
                      paging =TRUE,
                      pageLength =  nrow(joined) 
                    ))
    }
  })
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}
