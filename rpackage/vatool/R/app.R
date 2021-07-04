
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import gsheet
#' @import DT
#' @import shinyMobile
#' @import DBI
#' @import RPostgres
#' @import dplyr
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "Bohemia VA tool", uiOutput('top_button')),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="Main",
            tabName="main"),
          menuItem(
            text="History",
            tabName="history"),
          menuItem(
            text="Adjudicate",
            tabName="adjudicate"),
          menuItem(
            text="Summary",
            tabName="summary"),
          menuItem(
            text = 'About',
            tabName = 'about')
        )),
      dashboardBody(
        tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #ffe9e9 !important;}')),
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
          tabItem(
            tabName="main",
            fluidPage(
              fluidRow(
                column(12, 
                       # uiOutput('top_button'),
                       # br(),
                       div(class = 'tableCard',
                           uiOutput('ui_main', inline = TRUE)
                       )
                )
              ),
              br(),
              fluidRow(
                column(8,
                       div(class = 'tableCard',
                           h1('VA form'),
                           DT::dataTableOutput('va_table')
                       )
                ),
                column(4,
                       div(class = 'tableCard',
                           h3('Physician inputs'),
                           uiOutput('ui_select_va'),
                           uiOutput('ui_assign_cod'),
                           uiOutput('ui_submission')
                       )
                )
              )
            )
          ),
          tabItem(
            tabName = 'history',
            fluidRow(
              column(12,
                     h2('Adjudicator info'),
                     div(class = 'tableCard',
                         DT::dataTableOutput('user_table')
                     ),
                     
                     br(),
                     
                     h2('Submission history'),
                     div(class = 'tableCard',
                         DT::dataTableOutput('history_table')
                     )
              )
            )
          ),
          tabItem(
            tabName = 'adjudicate',
            fluidRow(
              uiOutput('ui_adjudicate')
            )
          ),
          tabItem(
            tabName = 'summary',
            fluidRow(
              div(class = "tableCard",
                  DT::dataTableOutput('summary_1')),
              br(), br(),
              div(class = "tableCard",
                  DT::dataTableOutput('summary_2'))
            )
          ),
          tabItem(
            tabName = 'about',
            make_about()
          )
        )
      )
    )
  )
}



##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
#' @import dplyr
app_server <- function(input, output, session) {
  
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  is_local <- ifelse(is_aws, FALSE, TRUE)
  # is_local <- FALSE
  
  logged_in <- reactiveVal(value = FALSE)
  submission_success <- reactiveVal(value = NULL)
  adj_submission_success <- reactiveVal(value = NULL)
  
  log_in_fail <- reactiveVal(value=FALSE)
  # Connect to database
  message('Connecting to database : ', ifelse(is_local, ' local', 'remote'))
  con <- get_db_connection(local = is_local)
  # Get list of authorized users, session, and cod tables
  users <- dbReadTable(conn = con, 'vatool_users')
  cods <- dbReadTable(conn=con, 'vatool_cods')
  data <- reactiveValues(va = data.frame(), session = data.frame(), cod = data.frame())
  # save(users, cods, file = 'va_data.rda')
  print(users)
  
  # Upon log in, read in data
  observeEvent(input$log_in,{
    message('in observeEvent(input$log_in')
    liu <- input$log_in_user
    lip <- input$log_in_password
    # See if the user exists and the password is correct
    ok <- FALSE
    if(!is.null(users)){
      message('at point 6')
      if(tolower(liu) %in% users$username & tolower(lip) %in% users$password){
        ok <-TRUE
      }
    }
    
    if(ok){
      logged_in(TRUE)
      removeModal()
      
      # load data
      data$va <- load_va_data(is_local = is_local)
      # data$va <- readRDS('~/Desktop/va_data.rda')
      
      # print(head(data$va))
      # create table with same columns as session table in database (to append upon logout)
      print(users)
      message('at point 2')
      # save(users, file = '/tmp/users.RData')
      user_id <- users %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
      start_time <- Sys.time()
      end_time <- NA
      data$session <- dplyr::tibble(user_id = user_id, start_time=start_time, end_time=end_time)
      
      # create cod table
      data$cod <- dplyr::tibble(user_id = user_id, death_id = NA, cod_code_1 = NA,cod_1 =NA,cod_code_2 = NA,cod_2 =NA,cod_code_3 = NA,cod_3 =NA, time_stamp = NA)
      
    } else {
      logged_in(FALSE)
      log_in_fail(TRUE)
      removeModal()
    }
    
  })
  
  # # Summary table 1
  output$summary_1 <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      # join users and cods
      out <- left_join(users, cods) %>%
        group_by(`Last name` = last_name) %>%
        summarise(`Number of VAs coded` = length(which(!is.na(time_stamp))))
      
    }
    if(!is.null(out)){
      if(is.data.frame(out)){
        out
      }
    }
  })
  
  # # Summary table 2
  output$summary_2 <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      pd <- data$va
      pd <- pd %>% group_by(country = server) %>%
        summarise(`Total VAs` = n()) %>%
        mutate(country = ifelse(grepl('ihi', country), 'Tanzania', 'Mozambique'))
      
      out <- left_join(users, cods) %>%
        group_by(country) %>%
        summarise(`Number of VAs coded` = length(which(!is.na(time_stamp)))) %>% 
        inner_join(pd)
      
    }
    if(!is.null(out)){
      if(is.data.frame(out)){
        out
      }
    }
  })
  
  # Selection input for VA ID
  output$ui_adjudicate <- renderUI({
    li <- logged_in()
    uu <- !is.null(users)
    li <- li & uu
    if(li){
      cn <- input$country
      liu <- input$log_in_user
      message('at point 3')
      # user_role <- users %>% dplyr::filter(username == tolower(liu)) %>% .$role
      
      
      # get ids that have more than one diagnosis
      user <- users %>% dplyr::filter(username == tolower(liu))
      userid <- user %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
      message('at point 3.5')
      
      # make sure user cant adjudicate a VA he already adjudicatd 
      cods <- cods %>% filter(user_id != userid)
      cod <- cods %>% 
        group_by(death_id, cod_3) %>% 
        summarise(counts = n())
      death_id_choices <- unique(cod$death_id[duplicated(cod$death_id)])
      
      
      cods_choices <- cod_choices(country = cn)
      
      fluidPage(
        fluidRow(
          column(12,
                 h2('Causes of deaths from other physicians')
          ),
          column(9,
                 
                 div(class = "tableCard",
                     DT::dataTableOutput('adj_table_2')
                 ),
                 h2('VA form'),
                 div(class = "tableCard",
                     DT::dataTableOutput('adj_table_1')
                 )
          ),
          column(3,
                 # h2(' '),
                 
                 div(class = "tableCard",
                     selectInput('adj_death_id', 'Select the VA ID', choices = sort(unique(death_id_choices))),
                     selectInput('adj_cods', 'Select underlying cause of death',  choices = c('', sort(unique(names(cods_choices))))),
                     textInput(inputId = 'adj_phy_notes', label = 'Enter additional notes on cause of death', value = NULL),
                     actionButton('adj_submit_cod', 'Submit cause of death and notes'),
                     uiOutput('ui_submission_adj')
                 )
          )
        )
      )
      
    } else {
      NULL
    }
  })
  
  # table showing 
  output$adj_table_1 <- DT::renderDataTable({
    li <- logged_in()
    cn <- input$country
    out <- data_frame(' ' = 'No VAs to adjudicate')
    if(li){
      idi <- input$adj_death_id
      if(!is.null(idi) & idi != ''){
        pd <- data$va 
        if(cn == 'Mozambique'){
          pd <- pd %>% filter(grepl('manhica', server))
        } else {
          pd <- pd %>% filter(grepl('ihi', server))
        }
        person <- pd %>% filter(death_id == idi)
        person <- get_va_names(person)
        # remove columns with NA
        person <- person[ , apply(person, 2, function(x) !any(is.na(x)))] 
        # remove other columns
        remove_these <- "write your 3 digit|Id10007|server|first or given|the surname|name of VA|1	Manually write your 3 digit worker ID here|tz001|this_usernameTake a picture of the painted Household ID|isadult1|isadult2|isneonatal|isneonatal2|ischild1|ischild2|instancename|instance_id|device_id|end_time|start_time|todays_date|wid|Do you have a QR code with your worker ID?|wid|ageindays|ageindaysneonate|ageinmonths|ageinmonthsbyyear|ageinmonthsremain|ageinyears2|ageinyearsremain|The GPS coordinates represents|Collect the GPS coordinates of this location|Does the house you're at have a painted ID number on it?|hh_id|Write the 6 digit household ID here|Id10007|Id10008|Id10009|Id10010|Id10010a| Id10010b|Id10011|Id10013|Id10017|Id10018|Id10018d|Id10020|Id10022|Id10023|Id10052|Id10053|Id10057|Id10061|Id10062|Id10069"
        
        person <- person[, !grepl(remove_these, names(person))]
        person <- person[,apply(person, 2, function(x) x != 'no')]
        
        out <- as.data.frame(t(person))
        out$Question <- rownames(out)
        names(out) <- c('Answer', 'Question')
        rownames(out) <- NULL
        out <- out[, c('Question', 'Answer')]
        
      }
    } 
    out
  })
  
  # table showing 
  output$adj_table_2 <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      idi <- input$adj_death_id
      
      if(!is.null(idi)){
        out <- cods %>% filter(death_id == idi)
      }
    } 
    names(out) <- c('User ID', 'Death ID', 'Immediate COD code', 'Immediate COD', 'Intermediary COD code', 'Intermediary COD', 'Underlying COD code', 'Underlying COD', 'Time stamp')
    out
  })
  
  # Define the button
  output$top_button <- renderUI({
    li <- logged_in()
    if(li){
      actionButton("log_out", "Log out")
    } else {
      actionButton("show", "Log in")
    }
  })
  
  # Define the placeholder main ui
  output$ui_main <- renderUI({
    li <- logged_in()
    lif <- log_in_fail()
    if(li){
      liu <- input$log_in_user
      li_text <- paste0('Welcome ', liu)
      p(li_text)
      
      fluidRow(
        selectInput(inputId = 'country',
                    label = 'Choose country',
                    choices = c('Mozambique', 'Tanzania'),
                    selected = 'Mozambique')
      )
    } else if (lif){
      p('Username or password incorrect')
    } else {
      p('Not logged in')
    }
    
    
  })
  
  # Selection input for VA ID
  output$ui_select_va <- renderUI({
    message('in ui_select_va')
    li <- logged_in()
    cn <- input$country
    ok <- FALSE
    message('cn is ', cn)
    message('li is ', li)
    if(li){
      if(!is.null(cn)){
        if(nchar(cn) > 0){
          ok <- TRUE
        }
      }
    }
    if(ok){
      pd <- data$va
      if(cn == 'Mozambique'){
        pd <- pd %>% filter(grepl('manhica', server))
      } else {
        pd <- pd %>% filter(grepl('ihi', server))
      }
      
      liu <- input$log_in_user
      user <- users %>% dplyr::filter(username == tolower(liu))
      userid <- user %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
      # save(pd, liu, user, userid,cods,  file = 'temp_choice.rda')
      out <- cods %>% dplyr::filter(user_id == userid)
      choices <- pd$death_id
      # Removing already used VA ID from the list of the user
      choices <- setdiff(choices, out$death_id)
      # Removing VA IDs with more than one frequency  
      frequency<-as.data.frame(table(cods$death_id))
      emit<-frequency %>% dplyr::filter(Freq > 1)
      choices <- setdiff(choices, emit$Var1)
      choices <- choices[!is.na(choices)]
      selectInput('death_id', 'Select the VA ID', choices = sort(unique(choices)), selected = sort(unique(choices))[1]) 
    } else {
      NULL
    }
    message('done with ui_select_va')
    
  })
  
  output$ui_assign_cod <- renderUI({
    li <- logged_in()
    cn <- input$country
    ok <- FALSE
    if(li){
      if(!is.null(cn)){
        if(nchar(cn) > 0){
          ok <- TRUE
        }
      }
    }
    if(ok){
      message('about to run cod_choices')
      choices <- cod_choices(country = cn)
      fluidPage(
        fluidRow(
          selectInput('cod_1', 'Select immediate cause of death', choices =c('', sort(unique(names(choices)))) , selected = ''),
          selectInput('cod_2', 'Select intermediary cause of death', choices =c('', sort(unique(names(choices)))), selected = ''),
          selectInput('cod_3', 'Select underlying cause of death', choices =c('', sort(unique(names(choices)))), selected = '')
        ),
        textInput(inputId = 'phy_notes', label = 'Enter additional notes on cause of death', value = NULL),
        fluidRow(
          actionButton('submit_cod', 'Submit causes of death and notes')
        )
      )
    } else {
      NULL
    }
  })
  
  
  output$va_table <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    cn <- input$country
    idi <- input$death_id
    ok <- FALSE
    message('va_table cn is ', cn)
    message('va_table li is ', li)
    if(li){
      if(!is.null(cn)){
        if(nchar(cn) > 0){
          if(!is.null(idi)){
            ok <- TRUE
          }
        }
      }
    }
    if(ok){
      pd <- data$va
      if(cn == 'Mozambique'){
        pd <- pd %>% filter(grepl('manhica', server))
      } else {
        pd <- pd %>% filter(grepl('ihi', server))
      }
      person <- pd %>% filter(death_id == idi)
      person <- get_va_names(person)
      # remove columns with NA
      person <- person[ , apply(person, 2, function(x) !any(is.na(x)))]
      
      
      # remove other columns 
      remove_these <- "write your 3 digit|Id10007|server|first or given|the surname|name of VA|1	Manually write your 3 digit worker ID here|tz001|this_usernameTake a picture of the painted Household ID|isadult1|isadult2|isneonatal|isneonatal2|ischild1|ischild2|instancename|instance_id|device_id|end_time|start_time|todays_date|wid|Do you have a QR code with your worker ID?|wid|ageindays|ageindaysneonate|ageinmonths|ageinmonthsbyyear|ageinmonthsremain|ageinyears2|ageinyearsremain|The GPS coordinates represents|Collect the GPS coordinates of this location|Does the house you're at have a painted ID number on it?|hh_id|Write the 6 digit household ID here|Id10007|Id10008|Id10009|Id10010|Id10010a| Id10010b|Id10011|Id10013|Id10017|Id10018|Id10018d|Id10020|Id10022|Id10023|Id10052|Id10053|Id10057|Id10061|Id10062|Id10069"
      
      person <- person[, !grepl(remove_these, names(person))]
      person <- person[,apply(person, 2, function(x) x != 'no')]
      out <- as.data.frame(t(person))
      out$Question <- rownames(out)
      # message(nrow(out))
      
      names(out) <- c('Answer', 'Question')
      rownames(out) <- NULL
      out <- out[, c('Question', 'Answer')]
    } else {
      out <- NULL
    }
    if(!is.null(out)){
      if(is.data.frame(out)){
        databrew::prettify(out, nrows = nrow(out))
      }
    }
    
  })
  
  observeEvent(input$show, {
    # logged_in(TRUE)
    showModal(modalDialog(
      title = "Log in",
      # fluidPage(
      # fluidRow(
      div(
        textInput('log_in_user', 'User')),
      div(
        passwordInput('log_in_password', 'Password')),
      # )
      # ,
      # fluidRow(
      #   actionButton('log_in', 'Log in')
      # )
      # ),
      
      footer =  tagList(
        modalButton("Dismiss"),
        actionButton('log_in', 'Log in')
      )
    ))
  })
  observeEvent(input$log_out, {
    logged_in(FALSE)
    session_data <- data$session
    session_data$end_time <- Sys.time()
    dbAppendTable(conn = con, name = 'vatool_sessions', value = session_data)
  })
  
  # Observe submission of cause of death and save
  observeEvent(input$submit_cod, {
    cn <- input$country
    cod_names <- cod_data(country = cn)
    cod_data <- data$cod
    cod_1 = input$cod_1
    cod_2 = input$cod_2
    cod_3 = input$cod_3
    
    # condition if underlying cause of death is not fiilled out, wont submit
    if(cod_1==''){
      submission_success(FALSE)
    } else {
      death_id = input$death_id
      cod_data$cod_1 = cod_1
      cod_data$cod_2 = cod_2
      cod_data$cod_3 = cod_3
      cod_data$death_id = death_id
      cod_data$time_stamp <- Sys.time()
      
      # ISSUE HERE IS THAT SOME (LIKE DIARRHOEA) ARE ASSOCIATED WITH TWO CODES AND VICE VERSA
      cod_data$cod_code_1<- cod_names$cod_code[cod_names$cod_names==cod_data$cod_1][1]
      cod_data$cod_code_2 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_2][1]
      cod_data$cod_code_3 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_3][1]
      dbAppendTable(conn = con, name = 'vatool_cods', value = cod_data)
      submission_success(TRUE)
    }
    
  })
  
  # Observe changes in inputs
  observeEvent(c(input$cod,input$death_id), {
    submission_success(NULL)
  })
  
  output$ui_submission <- renderUI({
    ss <- submission_success()
    if(is.null(ss)){
      NULL
    } else if(ss){
      h3('Submission successful')
    } else {
      h3('Submission unsuccessful')
    }
  })
  
  # history table 
  output$user_table <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      if(!is.null(users)){
        message('at point 4')
        liu <- input$log_in_user
        out <- users %>% dplyr::filter(username == tolower(liu))
        names(out) <- c('User ID', 'Username', 'Password', 'First name', 'Last name')
        out
      }
    } 
    out
  })
  
  # history table 
  output$history_table <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      if(!is.null(users)){
        message('at point 5')
        liu <- input$log_in_user
        user <- users %>% dplyr::filter(username == tolower(liu))
        userid <- user %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
        out <- cods %>% dplyr::filter(user_id == userid)
        names(out) <-  c('User ID', 'Death ID', 'Immediate COD code', 'Immediate COD', 'Intermediary COD code', 'Intermediary COD', 'Underlying COD code', 'Underlying COD', 'Time stamp')
      }
    } 
    DT::datatable(out, options = list(scrollX = TRUE)
    )
  })
  
  # Adjudicator submissions
  # Observe submission of cause of death and save
  observeEvent(input$adj_submit_cod, {
    cn <- input$country
    cod_names <- cod_data(country = cn)
    cod_data <- data$cod
    cod_1 = input$adj_cods
    # condition if underlying cause of death is not fiilled out, wont submit
    if(cod_1==''){
      adj_submission_success(FALSE)
    } else {
      death_id = input$adj_death_id
      cod_data$cod_1 = input$adj_cods
      cod_data$death_id = death_id
      cod_data$time_stamp <- Sys.time()
      
      cod_data$cod_code_1 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_1][1]
      dbAppendTable(conn = con, name = 'vatool_cods', value = cod_data)
      adj_submission_success(TRUE)
    }
    
  })
  
  # Observe changes in inputs
  observeEvent(c(input$adj_cod,input$adj_death_id), {
    adj_submission_success(NULL)
  })
  
  output$ui_submission_adj <- renderUI({
    ss <- adj_submission_success()
    if(is.null(ss)){
      NULL
    } else if(ss){
      h3('Submission successful')
    } else {
      h3('Submission unsuccessful')
    }
  })
  session$onSessionEnded(function(){
    cat(paste0('Session ended.'))
    if(exists('con')){
      dbDisconnect(con)
      cat(paste0('Disconnected from database.'))
    }
  })
  
  
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'vatool')
  )
  
  
  share <- list(
    title = "Bohemia VA Tool",
    url = "https://bohemia.team/va/",
    image = "https://www.databrew.cc/images/logo_clear.png",
    description = "Bohemia app",
    twitter_user = "data_brew"
  )
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}
